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
  (evil-mc/start-record-keystrokes))

(defun evil-mc/end-command-hook ()
  (evil-mc/stop-record-keystrokes)
  (print evil-last-keystrokes)
  (setq evil-last-keystrokes nil))

(add-hook 'pre-command-hook #'evil-mc/start-command-hook)
(add-hook 'post-command-hook #'evil-mc/end-command-hook)

(defadvice read-key-sequence (before evil-mc activate)
  "Record `this-command-keys' before it is reset."
  (evil-mc/stop-record-keystrokes))





