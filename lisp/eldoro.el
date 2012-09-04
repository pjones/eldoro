;;; eldoro.el -- A pomodoro timer that works with org-mode

;;; Code
(eval-when-compile
  (require 'org)
  (require 'org-clock))

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

(defcustom eldoro-notify-function 'org-notify
  "A function to call to notify the user that a pomodoro or break
  has expired.  The function should take a single argument, a
  string to display to the user."
  :type 'function
  :group 'eldoro)

(defcustom eldoro-pomodoro-end-msg
  "The current pomodoro for \"%s\" has ended.  Time for a break!"
  "A notification message shown when a pomodoro has ended.  The
string is run through `format' with one string argument, the
title of the current task."
  :type 'string
  :group 'eldoro)

(defcustom eldoro-break-end-msg
  "The current break has ended.  Get back to work!"
  "A notification message shown when a break has ended.  The
string is run through `format' with one string argument, the
title of the current task."
  :type 'string
  :group 'eldoro)

(defcustom eldoro-record-in-properties t
  "If non-nil, record the number of pomodori and interruptions
into the source org buffer using properties."
  :type 'boolean
  :group 'eldoro)

(defcustom eldoro-pomodoro-property "ELDORO_POMODORI"
  "The name of the org-mode property in which to store pomodoro
counts."
  :type 'string
  :group 'eldoro)

(defcustom eldoro-interruption-property "ELDORO_INTERRUPTIONS"
  "The name of the org-mode property in which to store
  interruption counts."
  :type 'string
  :group 'eldoro)

(defgroup eldoro-faces nil
  "Customize the appearance of Eldoro."
  :prefix "eldoro-"
  :group 'faces
  :group 'eldoro)

(defface eldoro-header
  '((t :inherit header-line))
  "Face for the header lines in the Eldoro buffer."
  :group 'eldoro-faces)

(defface eldoro-active-task
  '((t :inherit highlight))
  "Face for the active task in Eldoro."
  :group 'eldoro-faces)

(defvar eldoro-buffer-name "*Eldoro*"
  "The name of the buffer used to show pomodoros.")

(defvar eldoro--start-time nil)
(defvar eldoro--source-marker nil)
(defvar eldoro--active-marker nil)
(defvar eldoro--countdown-type nil)
(defvar eldoro--countdown-start nil)
(defvar eldoro--pomodori 0)
(defvar eldoro--breaks 0)
(defvar eldoro--interrupts 0)
(defvar eldoro--leave-point 0)
(defvar eldoro--timer nil)
(defvar eldoro--sent-notification nil)

(defvar eldoro-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "b")   'bury-buffer)
    (define-key map (kbd "g")   'eldoro-update)
    (define-key map (kbd "h")   'describe-mode)
    (define-key map (kbd "i")   'eldoro-interruption)
    (define-key map (kbd "q")   'eldoro-quit)
    (define-key map (kbd "r")   'eldoro-reset-counters)
    (define-key map (kbd "s")   'eldoro-clock-stop)
    (define-key map (kbd "RET") 'eldoro-clock-next)
    map)
  "Keymap used in Eldoro mode.")

;;;###autoload
(defun eldoro (&optional force-reset)
  "Start Eldoro on the current org-mode heading.  If Eldoro is
already running bring its buffer forward.

If Eldoro has already been started and this function is called
from an org-mode buffer, prompt for permission to reset the
Eldoro tasks.  With a prefix argument force a reset without
prompting."
  (interactive)
  (cond
   ;; We're in an org buffer and we're allowed to reset.
   ((and (string= major-mode "org-mode")
         (or (not eldoro--source-marker) force-reset
             (y-or-n-p "Reset Eldoro from this org buffer? ")))
    (if eldoro--countdown-start (eldoro-clock-stop))
    (eldoro-reset-vars)
    (save-excursion
      (org-back-to-heading)
      (setq eldoro--source-marker (point-marker)))
    (unless (> (eldoro-children-count) 0)
      (error "This heading doesn't have any children"))
    (switch-to-buffer (get-buffer-create eldoro-buffer-name))
    (eldoro-mode))
   ;; There's an Eldoro buffer already.
   ((get-buffer eldoro-buffer-name)
    (switch-to-buffer eldoro-buffer-name))
   ;; No Eldoro buffer, and we're not in an org-mode buffer.
   (t
    (error "Eldoro mode should be started from an org-mode heading"))))

(define-derived-mode eldoro-mode fundamental-mode "Eldoro"
  "A major mode for showing pomodoro timers."
  :group 'eldoro
  (setq buffer-read-only t
        truncate-lines t)
  (eldoro-timer-stop)
  (setq eldoro--timer (run-at-time nil 10 'eldoro-update)))

(defun eldoro-update ()
  "Update the Eldoro buffer."
  (interactive)
  (when (not (string= (format-time-string "%Y%m%d" eldoro--start-time)
                      (format-time-string "%Y%m%d")))
    (setq eldoro--start-time (current-time))
    (eldoro-really-reset-counters))
  (let ((buffer (get-buffer eldoro-buffer-name)))
    (if (not buffer) (progn (eldoro-timer-stop) (eldoro-reset-vars))
      (with-current-buffer buffer
        (let ((buffer-read-only nil)) (eldoro-draw-buffer)))))
  (if (and eldoro--countdown-start (<= (eldoro-remaining) 0))
      (eldoro-send-notification)))

(defun eldoro-quit ()
  "Stop the current timer and kill the Eldoro buffer."
  (interactive)
  (when (y-or-n-p "Really quit Eldoro? ")
    (if eldoro--countdown-start (eldoro-clock-stop))
    (eldoro-timer-stop)
    (eldoro-reset-vars)
    (kill-buffer eldoro-buffer-name)))

(defun eldoro-clock-next ()
  "Start the next appropriate clock (pomodoro or break)."
  (interactive)
  (if (not (eldoro-task-p)) (error "Please move point to a task first"))
  (let ((old eldoro--countdown-type))
    (if eldoro--countdown-start (eldoro-clock-stop))
    (setq eldoro--countdown-type old))
  (eldoro-clock-start))

(defun eldoro-clock-start ()
  "Start a pomodoro or break clock for the task at point."
  (interactive)
  (if eldoro--countdown-start (error "A task is still in progress"))
  (let ((marker (eldoro-task-p)))
    (if (not marker) (error "Please move point to a task first"))
    (setq eldoro--active-marker marker
          eldoro--countdown-start (float-time)
          eldoro--countdown-type (eldoro-next-clock-type)
          eldoro--sent-notification nil))
  (eldoro-update))

(defun eldoro-clock-stop (&optional interruption)
  "Stop the current pomodoro/break clock.  With a prefix argument
abort the current pomodoro due to an interruption."
  (interactive "P")
  (if (not eldoro--countdown-start) (error "No task is in progress"))
  (let ((restarting nil))
    (cond
     ;; Stop working (not an interruption).
     ((and (eq eldoro--countdown-type 'work) (not interruption))
      (eldoro-record-pomodoro)
      (setq eldoro--pomodori (1+ eldoro--pomodori)))
     ;; Restart work timer due to an interruption.
     ((eq eldoro--countdown-type 'work)
      (eldoro-record-interruption)
      (setq eldoro--interrupts (1+ eldoro--interrupts)))
     ;; Stop during a break (not an interruption).
     ((and (eq eldoro--countdown-type 'break) (not interruption))
      (setq eldoro--breaks (1+ eldoro--breaks)))
     ;; Restart a break due to an interruption.
     ((eq eldoro--countdown-type 'break)
      (setq eldoro--countdown-start (float-time)
            eldoro--sent-notification nil
            restarting t)))
    (unless restarting
      (setq eldoro--countdown-start nil
            eldoro--countdown-type nil
            eldoro--active-marker nil))
    (eldoro-update)))

(defun eldoro-interruption ()
  "Abort the current pomodoro due to an interruption and start a
new pomodoro."
  (interactive)
  (if eldoro--countdown-start (eldoro-clock-stop t)))

(defun eldoro-reset-counters (&optional force)
  "Reset the counters used to track pomodori, breaks, and
interruptions.  With a prefix argument don't prompt for
confirmation."
  (interactive "P")
  (when (or force (y-or-n-p "Really reset Eldoro counters? "))
    (eldoro-really-reset-counters)
    (eldoro-update)))

;;;-------------------------------------------------------------------------
;;; Internal Functions.
;;;-------------------------------------------------------------------------

(defun eldoro-map-tree (eldoro-fun)
  "Call ELDORO-FUN for each child in the org source tree."
  (let ((start-level))
    (with-current-buffer (marker-buffer eldoro--source-marker)
      (save-excursion
        (goto-char (marker-position eldoro--source-marker))
        (setq start-level (funcall outline-level)) ; Why funcall?
        (org-map-tree
         (lambda () (if (/= start-level (funcall outline-level))
                        (funcall eldoro-fun))))))))

(defun eldoro-children-count ()
  "Returns the number of child headings in the org doc."
  (let ((children 0))
    (eldoro-map-tree (lambda () (setq children (1+ children))))
    children))

(defun eldoro-task-p ()
  "Returns nil if point isn't on a Eldoro task, otherwise returns
the marker associated with the task at point."
  (with-current-buffer eldoro-buffer-name
    (get-text-property (point) 'eldoro-src)))

(defun eldoro-minutes-as-string (minutes)
  (if (= (abs minutes) 1) "minute" "minutes"))

(defun eldoro-remaining-string (&optional countdown)
  (let* ((time (or countdown eldoro--countdown-start))
         (min (eldoro-remaining time))
         (ajd (if (>= min 0) " remaining" " too long"))
         (clock (number-to-string (abs min))))
    (concat clock " " (eldoro-minutes-as-string min) ajd)))

(defun eldoro-remaining (&optional countdown)
  (setq countdown (or countdown eldoro--countdown-start))
  (round (- (eldoro-duration)
            (/ (- (float-time) countdown) 60))))

(defun eldoro-duration ()
  "Returns the number of minutes the clock should run for."
  (cond
   ((eq eldoro--countdown-type 'work)
    eldoro-work-time)
   ((and (eq eldoro--countdown-type 'break)
         (/= eldoro--pomodori 0)
         (= (% eldoro--pomodori eldoro-long-break-after) 0))
    eldoro-long-break)
   (t eldoro-short-break)))

(defun eldoro-timer-stop ()
  "Stop the internal Emacs timer."
  (if eldoro--timer (setq eldoro--timer (cancel-timer eldoro--timer))))

(defun eldoro-reset-vars ()
  "Reset all internal variables tied to a given org file."
  (if eldoro--countdown-start (eldoro-clock-stop))
  (if (not eldoro--start-time) (setq eldoro--start-time (current-time)))
  (setq eldoro--countdown-type nil
        eldoro--countdown-start nil
        eldoro--sent-notification nil
        eldoro--leave-point 0
        eldoro--source-marker nil
        eldoro--active-marker nil))

(defun eldoro-really-reset-counters ()
  (setq eldoro--pomodori 0
        eldoro--breaks 0
        eldoro--interrupts 0))

(defun eldoro-next-clock-type ()
  (cond
   ((eq eldoro--countdown-type 'work)  'break)
   ((eq eldoro--countdown-type 'break) 'work)
   (t 'work)))

(defun eldoro-draw-buffer ()
  "Write the contents of the Eldoro buffer."
  (setq eldoro--leave-point (point))
  (erase-buffer)
  (eldoro-draw-stats)
  (insert (propertize (concat (eldoro-parent-task-heading) ":")
                      'face 'eldoro-header))
  (insert "\n\n")
  (eldoro-map-tree 'eldoro-draw-heading)
  (set-buffer-modified-p nil)
  (goto-char eldoro--leave-point))

(defun eldoro-draw-stats ()
  (let ((indent (make-string (length eldoro-current-task-prompt) ? ))
        (clock (and eldoro--countdown-start (eldoro-remaining-string)))
        (pomodori (number-to-string eldoro--pomodori))
        (breaks (number-to-string eldoro--breaks))
        (interrupts (number-to-string eldoro--interrupts)))
    (insert (propertize (concat "Pomodoro statistics for "
                                (format-time-string eldoro-date-format) ":")
                        'face 'eldoro-header))
    (insert "\n\n")
    (if eldoro--countdown-start
        (cond
         ((eq eldoro--countdown-type 'work)
          (insert (concat indent " Pomodoro Timer: " clock "\n")))
         ((eq eldoro--countdown-type 'break)
          (insert (concat indent "    Break Timer: " clock "\n")))))
    (insert (concat indent "       Pomodori: " pomodori "\n"))
    (insert (concat indent "         Breaks: " breaks "\n"))
    (insert (concat indent "  Interruptions: " interrupts "\n"))
    (insert "\n")))

(defun eldoro-draw-heading ()
  (let ((heading (substring-no-properties (org-get-heading t t)))
        (mark (point-marker))
        (prompt (make-string (length eldoro-current-task-prompt) ? ))
        task active)
    (if (equal mark eldoro--active-marker)
        (setq prompt eldoro-current-task-prompt active t))
    (setq task (concat prompt heading))
    (put-text-property 0 (length task) 'eldoro-src mark task)
    (with-current-buffer eldoro-buffer-name
      (if (and active (= 0 eldoro--leave-point))
          (setq eldoro--leave-point (point)))
      (if active (insert (propertize task 'face 'eldoro-active-task))
        (insert task))
      (insert "\n"))))

(defun eldoro-at-marker (marker fun)
  "Move to MARKER and apply FUN."
  (setq marker (or marker eldoro--active-marker))
  (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char (marker-position marker))
        (funcall fun))))

(defun eldoro-get-task-heading (&optional marker)
  "Returns the heading text for the heading at MARKER or at the
active marker if MARKER is nil."
  (eldoro-at-marker
   marker
   (lambda () (substring-no-properties (org-get-heading t t)))))

(defun eldoro-parent-task-heading ()
  "Returns the heading text for the task Eldoro was started on."
  (eldoro-get-task-heading eldoro--source-marker))

(defun eldoro-active-task-heading ()
  "Returns the heading text for the active task."
  (eldoro-get-task-heading eldoro--active-marker))

(defun eldoro-record-pomodoro ()
  "Increment the number of pomodori for the active task."
  (if eldoro-record-in-properties
      (eldoro-inc-org-prop eldoro-pomodoro-property)))

(defun eldoro-record-interruption ()
  "Increment the number of interruptions for the active task."
  (if eldoro-record-in-properties
      (eldoro-inc-org-prop eldoro-interruption-property)))

(defun eldoro-get-org-prop (name &optional missing marker)
  "Return the value for the given property.  If the property is
missing return the value of MISSING.  By default the property is
looked up on the active org heading unless MARKER is given."
  (eldoro-at-marker
   marker (lambda () (or (org-entry-get (point) name) missing))))

(defun eldoro-set-org-prop (name value &optional marker)
  "Set the property NAME to VALUE for MARKER or the active
heading."
  (eldoro-at-marker
   marker (lambda () (org-entry-put (point) name value))))

(defun eldoro-inc-org-prop (name &optional marker)
  (let* ((s (eldoro-get-org-prop name "0" marker))
         (n (1+ (string-to-number s))))
    (eldoro-set-org-prop name (number-to-string n) marker)))

(defun eldoro-send-notification ()
  "Send a notification that a pomodoro or break ended."
  (when (and (not eldoro--sent-notification) eldoro-notify-function)
    (setq eldoro--sent-notification t)
    (let ((msg (if (eq eldoro--countdown-type 'work)
                   eldoro-pomodoro-end-msg
                 eldoro-break-end-msg)))
      (funcall eldoro-notify-function
               (format msg (eldoro-active-task-heading))))))

(provide 'eldoro)
