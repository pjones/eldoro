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

(defcustom eldoro-current-task-prompt ">"
  "The string to place in front of the active task."
  :type 'string
  :group 'eldoro)

(defvar eldoro-buffer-name "*Eldoro*"
  "The name of the buffer used to show pomodoros.")

(defvar eldoro-source-marker nil)
(defvar eldoro-active-marker nil)
(defvar eldoro-timer nil)

;;;###autoload
(defun eldoro ()
  "Start Eldoro on the current org-mode heading.  If Eldoro is
already running bring its buffer forward."
  (interactive)
  (unless (string= major-mode "org-mode")
    (error "Eldoro mode should be started from an org-mode heading"))
  (org-back-to-heading)
  (setq eldoro-source-marker (make-marker))
  (set-marker eldoro-source-marker (point))
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
  (eldoro-map-tree 'eldoro-draw-heading))

(defun eldoro-draw-heading ()
  (let ((task (substring-no-properties (org-get-heading t t))))
    (with-current-buffer eldoro-buffer-name
      (insert (concat task "\n")))))

(define-derived-mode eldoro-mode fundamental-mode "Eldoro"
  "A major mode for showing pomodoro timers."
  :group 'eldoro
  (setq buffer-read-only t)
  (toggle-truncate-lines 1)
  (when eldoro-timer (cancel-timer eldoro-timer))
  (setq eldoro-timer (run-at-time nil 30 'eldoro-update)))

(provide 'eldoro)
