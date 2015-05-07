;;; avy-jump.el --- jump to things tree-style

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Oleh Krehel
;; URL: https://github.com/abo-abo/avy-jump
;; Version: 0.1.0
;; Keywords: point, location

;; This file is part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package offers various commands for navigating to things using `avy'.
;; They are in the "Commands" outline.

;;; Code:
;;* Requires
(require 'avy)

;;* Customization
(defgroup avy-jump nil
  "Jump to things tree-style."
  :group 'convenience
  :prefix "avy-")

(defcustom avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
  "Keys for jumping.")

(defcustom avy-background nil
  "When non-nil, a gray background will be added during the selection."
  :type 'boolean)

(defcustom avy-word-punc-regexp "[!-/:-@[-`{-~]"
  "Regexp of punctuation characters that should be matched when calling
`avy-goto-word-1' command. When nil, punctuation chars will not be matched.

\"[!-/:-@[-`{-~]\" will match all printable punctuation chars.")

(defface avy-lead-face
  '((t (:foreground "white" :background "#e52b50")))
  "Face used for the leading chars.")

(defface avy-background-face
  '((t (:foreground "gray40")))
  "Face for whole window background during selection.")

;;* Internals
(defun avy--goto (x)
  "Goto X.
X is (POS . WND)
POS is either a position or (BEG . END)."
  (if (null x)
      (message "zero candidates")
    (select-window (cdr x))
    (let ((pt (car x)))
      (when (consp pt)
        (setq pt (car pt)))
      (unless (= pt (point)) (push-mark))
      (goto-char pt))))

(defun avy--process (candidates overlay-fn)
  "Select one of CANDIDATES using `avy-read'."
  (unwind-protect
       (cl-case (length candidates)
         (0
          nil)
         (1
          (car candidates))
         (t
          (avy--make-backgrounds (list (selected-window)))
          (avy-read (avy-tree candidates avy-keys)
                    overlay-fn
                    #'avy--remove-leading-chars)))
    (avy--done)))

(defvar avy--overlays-back nil
  "Hold overlays for when `avy-background' is t.")

(defun avy--make-backgrounds (wnd-list)
  "Create a dim background overlay for each window on WND-LIST."
  (when avy-background
    (setq avy--overlays-back
          (mapcar (lambda (w)
                    (let ((ol (make-overlay
                               (window-start w)
                               (window-end w)
                               (window-buffer w))))
                      (overlay-put ol 'face 'avy-background-face)
                      ol))
                  wnd-list))))

(defun avy--done ()
  "Clean up overlays."
  (mapc #'delete-overlay avy--overlays-back)
  (setq avy--overlays-back nil)
  (avy--remove-leading-chars))

(defcustom avy-all-windows t
  "When non-nil, loop though all windows for candidates."
  :type 'boolean)

(defun avy--regex-candidates (regex &optional wnd beg end pred)
  "Return all elements that match REGEX in WND.
Each element of the list is ((BEG . END) . WND)
When PRED is non-nil, it's a filter for matching point positions."
  (let (candidates)
    (dolist (wnd (if avy-all-windows
                     (window-list)
                   (list (selected-window))))
      (with-selected-window wnd
        (unless (memq major-mode '(image-mode doc-view-mode))
          (let ((we (or end (window-end (selected-window) t))))
            (save-excursion
              (goto-char (or beg (window-start)))
              (while (re-search-forward regex we t)
                (unless (get-char-property (point) 'invisible)
                  (when (or (null pred)
                            (funcall pred))
                    (push (cons (cons (match-beginning 0)
                                      (match-end 0))
                                wnd) candidates)))))))))
    (nreverse candidates)))

(defvar avy--overlay-offset 0
  "The offset to apply in `avy--overlay'.")

(defvar avy--overlays-lead nil
  "Hold overlays for leading chars.")

(defun avy--remove-leading-chars ()
  "Remove leading char overlays."
  (mapc #'delete-overlay avy--overlays-lead)
  (setq avy--overlays-lead nil))

(defun avy--overlay (str pt wnd)
  "Create an overlay with STR at PT in WND."
  (let* ((pt (+ pt avy--overlay-offset))
         (ol (make-overlay pt (1+ pt) (window-buffer wnd)))
         (old-str (with-selected-window wnd
                    (buffer-substring pt (1+ pt)))))
    (when avy-background
      (setq old-str (propertize
                     old-str 'face 'avy-background-face)))
    (overlay-put ol 'window wnd)
    (overlay-put ol 'display (concat str old-str))
    (push ol avy--overlays-lead)))

(defun avy--overlay-pre (path leaf)
  "Create an overlay with STR at LEAF.
PATH is a list of keys from tree root to LEAF.
LEAF is ((BEG . END) . WND)."
  (avy--overlay
   (propertize (apply #'string (reverse path))
               'face 'avy-lead-face)
   (cond ((numberp leaf)
          leaf)
         ((consp (car leaf))
          (caar leaf))
         (t
          (car leaf)))
   (if (consp leaf)
       (cdr leaf)
     (selected-window))))

(defun avy--overlay-at (path leaf)
  "Create an overlay with STR at LEAF.
PATH is a list of keys from tree root to LEAF.
LEAF is ((BEG . END) . WND)."
  (let ((str (propertize
              (string (car (last path)))
              'face 'avy-lead-face))
        (pt (if (consp (car leaf))
                (caar leaf)
              (car leaf)))
        (wnd (cdr leaf)))
    (let ((ol (make-overlay pt (1+ pt)
                            (window-buffer wnd)))
          (old-str (with-selected-window wnd
                     (buffer-substring pt (1+ pt)))))
      (when avy-background
        (setq old-str (propertize
                       old-str 'face 'avy-background-face)))
      (overlay-put ol 'window wnd)
      (overlay-put ol 'display str)
      (push ol avy--overlays-lead))))

(defun avy--overlay-post (path leaf)
  "Create an overlay with STR at LEAF.
PATH is a list of keys from tree root to LEAF.
LEAF is ((BEG . END) . WND)."
  (avy--overlay
   (propertize (apply #'string (reverse path))
               'face 'avy-lead-face)
   (cond ((numberp leaf)
          leaf)
         ((consp (car leaf))
          (cdar leaf))
         (t
          (car leaf)))
   (if (consp leaf)
       (cdr leaf)
     (selected-window))))

(defun avy--style-fn (style)
  "Transform STYLE symbol to a style function."
  (cl-case style
    (pre #'avy--overlay-pre)
    (at #'avy--overlay-at)
    (post #'avy--overlay-post)
    (t (error "Unexpected style %S" style))))

(defun avy--generic-jump (regex window-flip style)
  "Jump to REGEX.
When WINDOW-FLIP is non-nil, do the opposite of `avy-all-windows'.
STYLE determines the leading char overlay style."
  (let ((avy-all-windows
         (if window-flip
             (not avy-all-windows)
           avy-all-windows)))
    (avy--goto
     (avy--process
      (avy--regex-candidates
       regex)
      (avy--style-fn style)))))

(defcustom avy-goto-char-style 'pre
  "Method of displaying the overlays for `avy-goto-char' and `avy-goto-char-2'."
  :type '(choice
          (const :tag "Pre" pre)
          (const :tag "At" at)
          (const :tag "Post" post)))

(defcustom avy-goto-word-style 'pre
  "Method of displaying the overlays for `avy-goto-word-0' and `avy-goto-word-0'."
  :type '(choice
          (const :tag "Pre" pre)
          (const :tag "At" at)
          (const :tag "Post" post)))

;;* Commands
;;;###autoload
(defun avy-goto-char (&optional arg)
  "Read one char and jump to it.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive "P")
  (avy--generic-jump
   (regexp-quote (string (read-char "char: ")))
   arg
   avy-goto-char-style))

;;;###autoload
(defun avy-goto-char-2 (&optional arg)
  "Read two consecutive chars and jump to the first one.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive "P")
  (avy--generic-jump
   (regexp-quote (string
		  (read-char "char 1: ")
		  (read-char "char 2: ")))
   arg
   avy-goto-char-style))

;;;###autoload
(defun avy-isearch ()
  "Jump to one of the current isearch candidates."
  (interactive)
  (let* ((candidates
          (avy--regex-candidates isearch-string))
         (avy-background nil)
         (candidate
          (avy--process candidates #'avy--overlay-post)))
    (isearch-done)
    (avy--goto candidate)))

;;;###autoload
(defun avy-goto-word-0 (arg)
  "Jump to a word start.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive "P")
  (let ((avy-keys (number-sequence ?a ?z)))
    (avy--generic-jump "\\b\\sw" arg avy-goto-word-style)))

;;;###autoload
(defun avy-goto-word-1 (&optional arg)
  "Read one char at word start and jump there.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive "P")
  (let* ((str (string (read-char "char: ")))
         (regex (if (and avy-word-punc-regexp
                         (string-match avy-word-punc-regexp str))
                    str
                  (concat
                   "\\b"
                   str))))
    (avy--generic-jump regex arg avy-goto-word-style)))

;;;###autoload
(defun avy-goto-subword-0 (&optional arg)
  "Jump to a word or subword start.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive "P")
  (let* ((avy-all-windows
          (if arg
              (not avy-all-windows)
            avy-all-windows))
         (avy-keys (number-sequence ?a ?z))
         (case-fold-search nil)
         (candidates (avy--regex-candidates
                      "\\(\\b\\sw\\)\\|\\(?:[^A-Z]\\([A-Z]\\)\\)")))
    (dolist (x candidates)
      (when (> (- (cdar x) (caar x)) 1)
        (cl-incf (caar x))))
    (avy--goto
     (avy--process candidates (avy--style-fn avy-goto-word-style)))))

(defun avy--line (&optional arg)
  "Select line in current window."
  (let ((avy-background nil)
        (avy-all-windows
         (if arg
             (not avy-all-windows)
           avy-all-windows))
        candidates)
    (dolist (wnd (if avy-all-windows
                     (window-list)
                   (list (selected-window))))
      (with-selected-window wnd
        (let ((ws (window-start)))
          (save-excursion
            (save-restriction
              (narrow-to-region ws (window-end (selected-window) t))
              (goto-char (point-min))
              (while (< (point) (point-max))
                (unless (get-char-property
                         (max (1- (point)) ws) 'invisible)
                  (push (cons (point) (selected-window))
                        candidates))
                (forward-line 1)))))))
    (avy--process (nreverse candidates) #'avy--overlay-pre)))

;;;###autoload
(defun avy-goto-line (&optional arg)
  "Jump to a line start in current buffer."
  (interactive "P")
  (avy--goto (avy--line arg)))

;;;###autoload
(defun avy-copy-line (arg)
  "Copy a selected line above the current line.
ARG lines can be used."
  (interactive "p")
  (let ((start (car (avy--line))))
    (move-beginning-of-line nil)
    (save-excursion
      (insert
       (buffer-substring-no-properties
        start
        (save-excursion
          (goto-char start)
          (move-end-of-line arg)
          (point)))
       "\n"))))

;;;###autoload
(defun avy-move-line (arg)
  "Move a selected line above the current line.
ARG lines can be used."
  (interactive "p")
  (let ((start (car (avy--line))))
    (move-beginning-of-line nil)
    (save-excursion
      (save-excursion
        (goto-char start)
        (move-end-of-line arg)
        (kill-region start (point)))
      (insert
       (current-kill 0)
       "\n"))))

;;;###autoload
(defun avy-copy-region ()
  "Select two lines and copy the text between them here."
  (interactive)
  (let ((beg (car (avy--line)))
        (end (car (avy--line)))
        (pad (if (bolp) "" "\n")))
    (move-beginning-of-line nil)
    (save-excursion
      (insert
       (buffer-substring-no-properties
        beg
        (save-excursion
          (goto-char end)
          (line-end-position)))
       pad))))

(define-obsolete-variable-alias 'avi-keys 'avy-keys "0.1.0")
(define-obsolete-variable-alias 'avi-background 'avy-background "0.1.0")
(define-obsolete-variable-alias 'avi-word-punc-regexp 'avy-word-punc-regexp "0.1.0")
(define-obsolete-face-alias 'avi-lead-face 'avy-lead-face "0.1.0")
(define-obsolete-function-alias 'avi--goto 'avy--goto "0.1.0")
(define-obsolete-function-alias 'avi--process 'avy--process "0.1.0")
(define-obsolete-variable-alias 'avi-all-windows 'avy-all-windows "0.1.0")
(define-obsolete-function-alias 'avi--overlay-pre 'avy--overlay-pre "0.1.0")
(define-obsolete-function-alias 'avi--overlay-at 'avy--overlay-at "0.1.0")
(define-obsolete-function-alias 'avi--overlay-post 'avy--overlay-post "0.1.0")
(define-obsolete-function-alias 'avi-goto-char 'avy-goto-char "0.1.0")
(define-obsolete-function-alias 'avi-goto-char-2 'avy-goto-char-2 "0.1.0")
(define-obsolete-function-alias 'avi-isearch 'avy-isearch "0.1.0")
(define-obsolete-function-alias 'avi-goto-word-0 'avy-goto-word-0 "0.1.0")
(define-obsolete-function-alias 'avi-goto-subword-0 'avy-goto-subword-0 "0.1.0")
(define-obsolete-function-alias 'avi-goto-word-1 'avy-goto-word-1 "0.1.0")
(define-obsolete-function-alias 'avi-goto-line 'avy-goto-line "0.1.0")
(define-obsolete-function-alias 'avi-copy-line 'avy-copy-line "0.1.0")
(define-obsolete-function-alias 'avi-move-line 'avy-move-line "0.1.0")
(define-obsolete-function-alias 'avi-copy-region 'avy-copy-region "0.1.0")
(define-obsolete-function-alias 'avi--regex-candidates 'avy--regex-candidates "0.1.0")

(provide 'avy-jump)

;;; avy-jump.el ends here
