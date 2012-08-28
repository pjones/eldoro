;;; eldoro.el -- A pomodoro timer that works with org-mode

;;; Code
(eval-when-compile
  (require 'org))

(defgroup eldoro nil
  "A pomodoro timer that works with org-mode."
  :version "0.1.0"
  :prefix "eldoro-"
  :group 'applications)

(defcustom eldoro-work-time 25
  "The number of minutes that a pomodoro working block takes."
  :type 'integer
  :group 'eldoro)

(defcustom eldoro-short-break 5
  "The number of minutes that a short break lasts."
  :type 'integer
  :group 'eldoro)

(defcustom eldoro-long-break 20
  "The number of minutes that a long break lasts."
  :type 'integer
  :group 'eldoro)

(defcustom eldoro-long-break-after 4
  "The number of work blocks after which a long break is taken."
  :type 'integer
  :group 'eldoro)

(defcustom eldoro-current-task-prompt " > "
  "The string to place in front of the active task."
  :type 'string
  :group 'eldoro)

(defcustom eldoro-date-format "%A, %B %d, %Y"
  "The date format used for pomodoro statistics."
  :type 'string
  :group 'eldoro)

(defvar eldoro-buffer-name "*Eldoro*"
  "The name of the buffer used to show pomodoros.")

(defvar eldoro-start-time nil)
(defvar eldoro-source-marker nil)
(defvar eldoro-active-marker nil)
(defvar eldoro-timer nil)
(defvar eldoro-clock 0)
(defvar eldoro-clock-type nil)
(defvar eldoro-clock-start nil)
(defvar eldoro-pomodori 0)
(defvar eldoro-breaks 0)
(defvar eldoro-interrupts 0)
(defvar eldoro-leave-point 0)

(defvar eldoro-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q")   'eldoro-quit)
    (define-key map (kbd "g")   'eldoro-update)
    (define-key map (kbd "RET") 'eldoro-next-clock-state)
    map)
  "Keymap used in Eldoro mode.")

;;;###Autoload
(defun eldoro ()
  "Start Eldoro on the current org-mode heading.  If Eldoro is
already running bring its buffer forward."
  (interactive)
  (unless (string= major-mode "org-mode")
    (error "Eldoro mode should be started from an org-mode heading"))
  (org-back-to-heading)
  (setq eldoro-source-marker (point-marker))
  (unless (> (eldoro-children-count) 0)
    (error "This heading doesn't have any children"))
  (switch-to-buffer (get-buffer-create eldoro-buffer-name))
  (eldoro-reset)
  (eldoro-mode))

(defun eldoro-update ()
  "Update the Eldoro buffer."
  (interactive)
  (let ((buffer (get-buffer eldoro-buffer-name)))
    (if (not buffer) (eldoro-stop-timer)
      (with-current-buffer buffer
        (let ((buffer-read-only nil))
          (eldoro-draw-buffer))))))

(defun eldoro-quit ()
  "Stop the current timer and kill the Eldoro buffer."
  (interactive)
  (when (y-or-n-p "Really quit Eldoro? ")
    (eldoro-stop-timer)
    (eldoro-stop-clock)
    (kill-buffer eldoro-buffer-name)))

(defun eldoro-next-clock-state ()
  "Start the appropriate clock (pomodoro or break) if the clock
isn't running, otherwise stop the clock."
  (interactive)
  (if (not eldoro-clock-start) (eldoro-start-clock)
    (eldoro-stop-clock)))

(defun eldoro-start-clock ()
  "Start a pomodoro or break clock for the task at point."
  (interactive)
  (if eldoro-clock-start (eldoro-stop-clock))
  (with-current-buffer eldoro-buffer-name
    (let ((marker (get-text-property (point) 'eldoro-src)))
      (if (not marker) (error "Please move point to a task first"))
      (setq eldoro-active-marker marker
            eldoro-clock 0
            eldoro-clock-start (current-time)
            eldoro-clock-type (eldoro-next-clock-type))))
  (eldoro-update))

(defun eldoro-stop-clock ()
  "Stop the current pomodoro/break clock."
  (interactive)
  (eldoro-reset))

;;;-------------------------------------------------------------------------
;;; Internal Functions.
;;;-------------------------------------------------------------------------

(defun eldoro-map-tree (eldoro-fun)
  "Call `eldoro-fun' for each child in the org source tree."
  (let ((start-level))
    (with-current-buffer (marker-buffer eldoro-source-marker)
      (save-excursion
        (goto-char (marker-position eldoro-source-marker))
        (setq start-level (funcall outline-level)) ; Why funcall?
        (org-map-tree
         (lambda () (if (/= start-level (funcall outline-level))
                        (funcall eldoro-fun))))))))

(defun eldoro-children-count ()
  "Returns the number of child headings in the org doc."
  (let ((children 0))
    (eldoro-map-tree (lambda () (setq children (1+ children))))
    children))

(defun eldoro-minutes-as-string (minutes)
  (if (= (abs minutes) 1) "minute" "minutes"))

(defun eldoro-clock-as-string ()
  (let ((ajd (if (>= eldoro-clock 0) " remaining" " too long"))
        (clock (number-to-string (abs eldoro-clock))))
    (concat clock " " (eldoro-minutes-as-string eldoro-clock) ajd)))

(defun eldoro-stop-timer ()
  "Stop the internal Emacs timer."
  (if eldoro-timer (setq eldoro-timer (cancel-timer eldoro-timer))))

(defun eldoro-reset ()
  "Reset all counters."
  (setq eldoro-start-time (current-time)
        eldoro-clock 0
        eldoro-clock-type nil
        eldoro-clock-start nil
        ;; eldoro-pomodori 0
        ;; eldoro-breaks 0
        ;; eldoro-interrupts 0
        eldoro-leave-point 0
        eldoro-active-marker nil))

(defun eldoro-next-clock-type ()
  (cond
   ((eq eldoro-clock-type 'work)  'break)
   ((eq eldoro-clock-type 'break) 'work)
   (t 'work)))

(defun eldoro-clock-length ()
  "Returns the number of minutes the clock should run for."
  (cond
   ((eq eldoro-clock-type 'work)
    eldoro-work-time)
   ((and (eq eldoro-clock-type 'break)
         (/= eldoro-pomodori 0)
         (= (% eldoro-pomodori eldoro-long-break-after) 0))
    eldoro-long-break)
   (t eldoro-short-break)))

(defun eldoro-update-clock ()
  (when (and eldoro-clock-start eldoro-clock-type)
    (let ((now (float-time))
          (then (float-time eldoro-clock-start)))
      (setq eldoro-clock
            (round (- (eldoro-clock-length)
                      (/ (- now then) 60)))))))

(defun eldoro-draw-buffer ()
  "Write the contents of the Eldoro buffer."
  (delete-region (point-min) (point-max))
  (setq eldoro-leave-point 0)
  (eldoro-update-clock)
  (eldoro-draw-stats)
  (insert "Tasks:\n\n")
  (eldoro-map-tree 'eldoro-draw-heading)
  (goto-char eldoro-leave-point))

(defun eldoro-draw-stats ()
  (let ((indent (make-string (length eldoro-current-task-prompt) ? ))
        (clock (eldoro-clock-as-string))
        (pomodori (number-to-string eldoro-pomodori))
        (breaks (number-to-string eldoro-breaks))
        (interrupts (number-to-string eldoro-interrupts)))
    (insert (concat "Pomodoro statistics for "
                    (format-time-string eldoro-date-format) ":\n\n"))
    (cond
     ((eq eldoro-clock-type 'work)
      (insert (concat indent " Pomodoro Timer: " clock "\n")))
     ((eq eldoro-clock-type 'break)
      (insert (concat indent "    Break Timer: " clock "\n"))))
    (insert (concat indent "       Pomodori: " pomodori "\n"))
    (insert (concat indent "         Breaks: " breaks "\n"))
    (insert (concat indent "  Interruptions: " interrupts "\n"))
    (insert "\n")))

(defun eldoro-draw-heading ()
  (let ((heading (substring-no-properties (org-get-heading t t)))
        (mark (point-marker))
        (prompt (make-string (length eldoro-current-task-prompt) ? ))
        task active)
    (if (equal mark eldoro-active-marker) (setq active t))
    (if active (setq prompt eldoro-current-task-prompt))
    (setq task (concat prompt heading "\n"))
    (put-text-property 0 (length task) 'eldoro-src mark task)
    (with-current-buffer eldoro-buffer-name
      (if (or (= 0 eldoro-leave-point) active)
          (setq eldoro-leave-point (point)))
      (insert task))))

(define-derived-mode eldoro-mode fundamental-mode "Eldoro"
  "A major mode for showing pomodoro timers."
  :group 'eldoro
  (setq buffer-read-only t)
  (toggle-truncate-lines 1)
  (eldoro-stop-timer)
  (setq eldoro-timer (run-at-time nil 10 'eldoro-update)))

(provide 'eldoro)
