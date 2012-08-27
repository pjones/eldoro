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

(defvar eldoro-source-marker nil)
(defvar eldoro-active-marker nil)
(defvar eldoro-timer nil)
(defvar eldoro-clock 0)
(defvar eldoro-clock-type nil)
(defvar eldoro-pomodori 0)
(defvar eldoro-breaks 0)
(defvar eldoro-interrupts 0)

;;;###autoload
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
  (eldoro-mode))

(defun eldoro-map-tree (eldoro-fun)
  "Call `fun' for each child in the org source tree."
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

(defun eldoro-update ()
  "Update the Eldoro buffer."
  (interactive)
  (let ((buffer (get-buffer eldoro-buffer-name)))
    (if (not buffer)
        (when eldoro-timer
          (setq eldoro-timer (cancel-timer eldoro-timer)))
      (with-current-buffer buffer
        (let ((buffer-read-only nil))
          (eldoro-draw-buffer))))))

(defun eldoro-draw-buffer ()
  "Write the contents of the Eldoro buffer."
  (delete-region (point-min) (point-max))
  (eldoro-draw-stats)
  (insert "Tasks:\n\n")
  (eldoro-map-tree 'eldoro-draw-heading))

(defun eldoro-draw-stats ()
  (let ((indent (make-string (length eldoro-current-task-prompt) ? ))
        (clock (number-to-string eldoro-clock))
        (pomodori (number-to-string eldoro-pomodori))
        (breaks (number-to-string eldoro-breaks))
        (interrupts (number-to-string eldoro-interrupts)))
    (insert (concat "Pomodoro statistics for "
                    (format-time-string eldoro-date-format) ":\n\n"))
    (cond
     ((eq eldoro-clock-type 'work)
      (insert (concat indent "    Break Timer: " clock "\n")))
     ((eq eldoro-clock-type 'break)
      (insert (concat indent " Pomodoro Timer: " clock "\n"))))
    (insert (concat indent "       Pomodori: " pomodori "\n"))
    (insert (concat indent "         Breaks: " breaks "\n"))
    (insert (concat indent "  Interruptions: " interrupts "\n"))
    (insert "\n\n")))

(defun eldoro-draw-heading ()
  (let ((heading (substring-no-properties (org-get-heading t t)))
        (mark (point-marker))
        (prompt (make-string (length eldoro-current-task-prompt) ? ))
        task active)
    (if (null eldoro-active-marker) (setq eldoro-active-marker mark))
    (if (equal mark eldoro-active-marker) (setq active t))
    (if active (setq prompt eldoro-current-task-prompt))
    (setq task (concat prompt heading "\n"))
    (put-text-property 0 (length task) 'eldoro-src mark task)
    (with-current-buffer eldoro-buffer-name (insert task))))

(define-derived-mode eldoro-mode fundamental-mode "Eldoro"
  "A major mode for showing pomodoro timers."
  :group 'eldoro
  (setq buffer-read-only t)
  (toggle-truncate-lines 1)
  (when eldoro-timer (cancel-timer eldoro-timer))
  (setq eldoro-timer (run-at-time nil 30 'eldoro-update)))

(provide 'eldoro)
