(defface evil-mc/cursor-face
  '((t (:inverse-video t)))
  "The face used for fake cursors"
  :group 'multiple-cursors)

(defface evil-mc/region-face
  '((t :inherit region))
  "The face used for fake regions"
  :group 'multiple-cursors)


;;; ======================================================================
;;; manipulate fake cursor

(defun evil-mc/create-fake-cursor-at-point ()
  "Add a fake cursor and possibly a fake active region overlay based on point and mark
saves the current state in the overlay to be restored later"
  (let ((overlay (evil-mc/make-cursor-overlay-at-point)))
    (evil-mc/store-current-state-in-overlay overlay)

    ;; TODO: create a reagion overlay to simulate visual selection

    (overlay-put overlay 'type 'fake-cursor)
    overlay))

(defun evil-mc/remove-fake-cursor (o)
  "Delete overlay with state, including dependent overlays and markers."
  (set-marker (overlay-get o 'point) nil)
  (set-marker (overlay-get o 'mark) nil)
  (delete-overlay o))

;;; ======================================================================
;;; manipulate overlay information

(defun evil-mc/make-cursor-overlay-at-eol (pos)
  "Create overlay to look like cursor at end of line."
  (let ((overlay (make-overlay pos pos nil nil nil)))
    (overlay-put overlay 'after-string (propertize " " 'face 'evil-mc/cursor-face))
    overlay))

(defun evil-mc/make-cursor-overlay-inline (pos)
  "Create overlay to look like cursor inside text."
  (let ((overlay (make-overlay pos (1+ pos) nil nil nil)))
    (overlay-put overlay 'face 'evil-mc/cursor-face)
    overlay))

(defun evil-mc/make-cursor-overlay-at-point ()
  "Create overlay to look like cursor.
Special case for end of line, because overlay over a newline
highlights the entire width of the window."
  (if (eolp)
      (evil-mc/make-cursor-overlay-at-eol (point))
    (evil-mc/make-cursor-overlay-inline (point))))

;;; ======================================================================
;;; manipulate per-cursor local states

(defvar evil-mc/cursor-specific-vars '(transient-mark-mode
				       kill-ring
				       kill-ring-yank-pointer
				       mark-ring
				       mark-active
				       yank-undo-function
				       autopair-action
				       autopair-wrap-action
				       er/history
					; evil related
				       evil-state
				       evil-visual-direction
				       evil-visual-selection
				       evil-visual-mark
				       evil-visual-point
				       evil-visual-beginning
				       evil-visual-end
				       evil-this-register
				       )
  "A list of vars that need to be tracked on a per-cursor basis.")

(defun evil-mc/store-current-state-in-overlay (o)
  "Store relevant info about point and mark in the given overlay."
  (overlay-put o 'point (set-marker (make-marker) (point)))
  (overlay-put o 'mark (set-marker (make-marker) (mark)))
  (dolist (var evil-mc/cursor-specific-vars)
    (when (boundp var) (overlay-put o var (symbol-value var))))
  o)

(defun evil-mc/restore-state-from-overlay (o)
  "Restore point and mark from stored info in the given overlay."
  (goto-char (overlay-get o 'point))
  (set-marker (mark-marker) (overlay-get o 'mark))
  (dolist (var evil-mc/cursor-specific-vars)
    (when (boundp var) (set var (overlay-get o var)))))

(defun evil-mc/pop-state-from-overlay (o)
  "Restore the state stored in given overlay and then remove the overlay."
  (evil-mc/restore-state-from-overlay o)
  (evil-mc/remove-fake-cursor o))

;;; ======================================================================
;;; Cursor utilities for executing commands
(defun evil-mc/fake-cursor-p (o)
  "Predicate to check if an overlay is a fake cursor"
  (eq (overlay-get o 'type) 'fake-cursor))

(defun evil-mc/all-fake-cursors (&optional start end)
  (remove-if-not 'evil-mc/fake-cursor-p
                 (overlays-in (or start (point-min))
                              (or end   (point-max)))))

(defmacro evil-mc/for-each-fake-cursor (&rest forms)
  "Runs the body for each fake cursor, bound to the name cursor"
  `(mapc #'(lambda (cursor) ,@forms)
         (evil-mc/all-fake-cursors)))

(defmacro evil-mc/save-excursion (&rest forms)
  "Saves and restores all the state that multiple-cursors cares about."
  (let ((cs (make-symbol "current-state")))
    `(let ((,cs (evil-mc/store-current-state-in-overlay
                 (make-overlay (point) (point) nil nil t))))
       (overlay-put ,cs 'type 'original-cursor)
       (save-excursion ,@forms)
       (evil-mc/pop-state-from-overlay ,cs))))

(defmacro evil-mc/save-window-scroll (&rest forms)
  "Saves and restores the window scroll position"
  (let ((p (make-symbol "p"))
        (s (make-symbol "start"))
        (h (make-symbol "hscroll")))
    `(let ((,p (set-marker (make-marker) (point)))
           (,s (set-marker (make-marker) (window-start)))
           (,h (window-hscroll)))
       ,@forms
       (goto-char ,p)
       (set-window-start nil ,s t)
       (set-window-hscroll nil ,h)
       (set-marker ,p nil)
       (set-marker ,s nil))))

(defun evil-mc/execute-command-for-fake-cursor (cmd cursor)
  (let ((evil-mc--executing-command-for-fake-cursor t)
        (annoying-arrows-mode nil)
        (smooth-scroll-margin 0))
    (evil-mc/pop-state-from-overlay cursor)
    (ignore-errors
      (evil-mc/execute-command cmd)
      (evil-mc/create-fake-cursor-at-point id))))

(defun evil-mc/execute-command (cmd)
  "Run command, simulating the parts of the command loop that makes sense for fake cursors."
  (setq this-command cmd)
  (run-hooks 'pre-command-hook)
  (unless (eq this-command 'ignore)
    (call-interactively cmd))
  (run-hooks 'post-command-hook)
  (when deactivate-mark (deactivate-mark)))

(defun evil-mc/execute-command-for-all-fake-cursors (cmd)
  "Calls CMD interactively for each cursor.
It works by moving point to the fake cursor, setting
up the proper environment, and then removing the cursor.
After executing the command, it sets up a new fake
cursor with updated info."
  (evil-mc/save-excursion
   (evil-mc/save-window-scroll
    (evil-mc/for-each-fake-cursor
     (save-excursion
       (evil-mc/execute-command-for-fake-cursor cmd cursor))))))

(defun evil-mc/execute-repeat-for-fake-cursor (repeat-info cursor)
  (let ((evil-mc--executing-command-for-fake-cursor t)
        (annoying-arrows-mode nil)
        (smooth-scroll-margin 0))
    (evil-mc/pop-state-from-overlay cursor)
    (ignore-errors
      (evil-execute-repeat-info repeat-info)
      (evil-mc/create-fake-cursor-at-point id))))

(defun evil-mc/execute-repeat-for-all-fake-cursors (repeat-info)
  "execute the last opeartion by evil for all fake cursors"
  (evil-mc/for-each-fake-cursor
   (evil-mc/execute-repeat-for-fake-cursor repeat-info cursor)))

;;; ======================================================================
;;; Evil part -- record every key sequence for every command.

(defvar evil-last-keystrokes nil
  "the keystroke sequece of executing an evil command")

(defun evil-mc/record-keys (info)
  "Add INFO to the end of `evil-last-keystrokes`"
  (setq evil-last-keystrokes (nconc evil-last-keystrokes (list info))))
 

(defun evil-mc/clear-command-keys ()
  (clear-this-command-keys t))

(defun evil-mc/start-record-keystrokes ()
  (evil-mc/record-keys (list "before" (this-command-keys))))

(defun evil-mc/stop-record-keystrokes ()
  (evil-mc/record-keys (list "after" (this-command-keys))))

(defun evil-mc/start-command-hook ()
  (message (concat "start command: " (symbol-name this-command)))
  (print evil-last-keystrokes)
  (evil-mc/start-record-keystrokes))

(defun evil-mc/end-command-hook ()
  (message (concat "stop command: " (symbol-name this-command)))
  (evil-mc/stop-record-keystrokes)
  (print evil-last-keystrokes)
  (setq evil-last-keystrokes nil))

;(add-hook 'pre-command-hook #'evil-mc/start-command-hook)
;(add-hook 'post-command-hook #'evil-mc/end-command-hook)

;; (defadvice read-key-sequence (before evil-mc activate)
;;   "Record `this-command-keys' before it is reset."
;;   (message "read sequence")
;;   (evil-mc/stop-record-keystrokes))

;; (defadvice evil-operator-range (before evil-mc activate)
;;   "Record `this-command-keys' before it is reset."
;;   (message "operator-range"))

;; (add-hook 'evil-repeat-post-hook-hook
;; 	  '(lambda ()
;; 	     (message (format "%s" (ring-ref evil-repeat-info 0)))))


(defun evil-mc-post-repeat ()
  (message "evil-repeat-post-hook: %s." (ring-ref evil-repeat-ring 0))
  (evil-mc/execute-repeat-for-all-fake-cursors (ring-ref evil-repeat-ring 0)))

;(add-function :after 'evil-repeat-post-hook #'evil-mc-post-repeat)
(advice-add 'evil-repeat-post-hook :after #'evil-mc-post-repeat)
(advice-remove 'evil-repeat-post-hook #'evil-mc-post-repeat)



