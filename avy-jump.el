;;; avy-jump.el --- jump to things tree-style. -*- lexical-binding: t -*-

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Oleh Krehel

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
(require 'cl-lib)
(require 'avy)

;;* Customization
(defgroup avy-jump nil
  "Jump to things tree-style."
  :group 'convenience
  :prefix "avy-")

(defcustom avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
  "Default keys for jumping."
  :type '(repeat :tag "Keys" character))

(defcustom avy-keys-alist nil
  "Alist of avy-jump commands to `avy-keys' overriding the default `avy-keys'."
  :type '(alist
          :key-type (choice :tag "Command"
                     (const avy-goto-char)
                     (const avy-goto-char-2)
                     (const avy-isearch)
                     (const avy-goto-line)
                     (const avy-goto-subword-0)
                     (const avy-goto-subword-1)
                     (const avy-goto-word-0)
                     (const avy-goto-word-1)
                     (const avy-copy-line)
                     (const avy-copy-region)
                     (const avy-move-line))
          :value-type (repeat :tag "Keys" character)))

(defcustom avy-style 'pre
  "The default method of displaying the overlays.
Use `avy-styles-alist' to customize this per-command."
  :type '(choice
          (const :tag "Pre" pre)
          (const :tag "At" at)
          (const :tag "At Full" at-full)
          (const :tag "Post" post)))

(defcustom avy-styles-alist nil
  "Alist of avy-jump commands to the style for each command.
If the commands isn't on the list, `avy-style' is used."
  :type '(alist
          :key-type (choice :tag "Command"
                     (const avy-goto-char)
                     (const avy-goto-char-2)
                     (const avy-isearch)
                     (const avy-goto-line)
                     (const avy-goto-subword-0)
                     (const avy-goto-subword-1)
                     (const avy-goto-word-0)
                     (const avy-goto-word-1)
                     (const avy-copy-line)
                     (const avy-copy-region)
                     (const avy-move-line))
          :value-type (choice
                       (const :tag "Pre" pre)
                       (const :tag "At" at)
                       (const :tag "At Full" at-full)
                       (const :tag "Post" post))))

(defmacro avy--with-avy-keys (command &rest body)
  "Set `avy-keys' according to COMMAND and execute BODY."
  (declare (indent 1))
  `(let ((avy-keys (or (cdr (assq ',command avy-keys-alist))
                       avy-keys))
         (avy-style (or (cdr (assq ',command avy-styles-alist))
                        avy-style)))
     ,@body))

(defcustom avy-background nil
  "When non-nil, a gray background will be added during the selection."
  :type 'boolean)

(defcustom avy-word-punc-regexp "[!-/:-@[-`{-~]"
  "Regexp of punctuation chars that count as word starts for `avy-goto-word-1.
When nil, punctuation chars will not be matched.

\"[!-/:-@[-`{-~]\" will match all printable punctuation chars."
  :type 'regexp)

(defface avy-lead-face
  '((t (:foreground "white" :background "#e52b50")))
  "Face used for the leading chars.")

(defface avy-background-face
  '((t (:foreground "gray40")))
  "Face for whole window background during selection.")

;;* Internals
(defcustom avy-all-windows t
  "When non-nil, loop though all windows for candidates."
  :type 'boolean)

(defmacro avy-dowindows (flip &rest body)
  "Depending on FLIP and `avy-all-windows' run BODY in each or selected window."
  (declare (indent 1))
  `(let ((avy-all-windows (if ,flip
                              (not avy-all-windows)
                            avy-all-windows)))
     (dolist (wnd (if avy-all-windows
                      (window-list)
                    (list (selected-window))))
       (with-selected-window wnd
         (unless (memq major-mode '(image-mode doc-view-mode))
           ,@body)))))

(defun avy--goto (x)
  "Goto X.
X is (POS . WND)
POS is either a position or (BEG . END)."
  (cond ((null x)
         (message "zero candidates"))

        ;; ignore exit from `avy-handler-function'
        ((eq x 'exit))

        (t
         (select-window (cdr x))
         (let ((pt (car x)))
           (when (consp pt)
             (setq pt (car pt)))
           (unless (= pt (point)) (push-mark))
           (goto-char pt)))))

(defun avy--process (candidates overlay-fn)
  "Select one of CANDIDATES using `avy-read'.
Use OVERLAY-FN to visualize the decision overlay."
  (unwind-protect
       (cl-case (length candidates)
         (0
          nil)
         (1
          (car candidates))
         (t
          (avy--make-backgrounds
           (if avy-all-windows
               (window-list)
             (list (selected-window))))
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
                      (overlay-put ol 'window w)
                      ol))
                  wnd-list))))

(defun avy--done ()
  "Clean up overlays."
  (mapc #'delete-overlay avy--overlays-back)
  (setq avy--overlays-back nil)
  (avy--remove-leading-chars))

(defun avy--regex-candidates (regex &optional beg end pred)
  "Return all elements that match REGEX.
Each element of the list is ((BEG . END) . WND)
When PRED is non-nil, it's a filter for matching point positions."
  (let (candidates)
    (avy-dowindows nil
      (let ((we (or end (window-end (selected-window) t))))
        (save-excursion
          (goto-char (or beg (window-start)))
          (while (re-search-forward regex we t)
            (unless (get-char-property (point) 'invisible)
              (when (or (null pred)
                        (funcall pred))
                (push (cons (cons (match-beginning 0)
                                  (match-end 0))
                            wnd) candidates)))))))
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
  (when (<= (1+ pt) (with-selected-window wnd (point-max)))
    (let* ((pt (+ pt avy--overlay-offset))
           (ol (make-overlay pt (1+ pt) (window-buffer wnd)))
           (old-str (with-selected-window wnd
                      (buffer-substring pt (1+ pt)))))
      (when avy-background
        (setq old-str (propertize
                       old-str 'face 'avy-background-face)))
      (overlay-put ol 'window wnd)
      (overlay-put ol 'display (concat str old-str))
      (push ol avy--overlays-lead))))

(defun avy--overlay-pre (path leaf)
  "Create an overlay with PATH at LEAF.
PATH is a list of keys from tree root to LEAF.
LEAF is normally ((BEG . END) . WND)."
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
  "Create an overlay with PATH at LEAF.
PATH is a list of keys from tree root to LEAF.
LEAF is normally ((BEG . END) . WND)."
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
      (overlay-put ol 'display (if (string= old-str "\n")
                                   (concat str "\n")
                                 str))
      (push ol avy--overlays-lead))))

(defun avy--overlay-at-full (path leaf)
  "Create an overlay with PATH at LEAF.
PATH is a list of keys from tree root to LEAF.
LEAF is normally ((BEG . END) . WND)."
  (let* ((str (propertize
               (apply #'string (reverse path))
               'face 'avy-lead-face))
         (len (length path))
         (beg (if (consp (car leaf))
                  (caar leaf)
                (car leaf)))
         (wnd (cdr leaf)))
    (with-selected-window wnd
      (save-excursion
        (goto-char beg)
        (let* ((end (if (= beg (line-end-position))
                        (1+ beg)
                      (min (+ beg len) (line-end-position))))
               (ol (make-overlay
                    beg end
                    (current-buffer)))
               (old-str (buffer-substring beg (1+ beg))))
          (when avy-background
            (setq old-str (propertize
                           old-str 'face 'avy-background-face)))
          (overlay-put ol 'window wnd)
          (overlay-put ol 'display (if (string= old-str "\n")
                                       (concat str "\n")
                                     str))
          (push ol avy--overlays-lead))))))

(defun avy--overlay-post (path leaf)
  "Create an overlay with PATH at LEAF.
PATH is a list of keys from tree root to LEAF.
LEAF is normally ((BEG . END) . WND)."
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
    (at-full 'avy--overlay-at-full)
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
      (avy--regex-candidates regex)
      (avy--style-fn style)))))

;;* Commands
;;;###autoload
(defun avy-goto-char (&optional arg)
  "Read one char and jump to it.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive "P")
  (avy--with-avy-keys avy-goto-char
    (avy--generic-jump
     (let ((c (read-char "char: ")))
       (if (= 13 c)
           "\n"
         (regexp-quote (string c))))
     arg
     avy-style)))

;;;###autoload
(defun avy-goto-char-2 (&optional arg)
  "Read two consecutive chars and jump to the first one.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive "P")
  (avy--with-avy-keys avy-goto-char-2
    (avy--generic-jump
     (regexp-quote (string
                    (read-char "char 1: ")
                    (read-char "char 2: ")))
     arg
     avy-style)))

;;;###autoload
(defun avy-isearch ()
  "Jump to one of the current isearch candidates."
  (interactive)
  (avy--with-avy-keys avy-isearch
    (let* ((candidates
            (avy--regex-candidates isearch-string))
           (avy-background nil)
           (candidate
            (avy--process candidates #'avy--overlay-post)))
      (isearch-done)
      (avy--goto candidate))))

;;;###autoload
(defun avy-goto-word-0 (arg)
  "Jump to a word start.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive "P")
  (avy--with-avy-keys avy-goto-word-0
    (avy--generic-jump "\\b\\sw" arg avy-style)))

;;;###autoload
(defun avy-goto-word-1 (&optional arg)
  "Read one char at word start and jump there.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive "P")
  (avy--with-avy-keys avy-goto-word-1
    (let* ((str (string (read-char "char: ")))
           (regex (cond ((string= str ".")
                         "\\.")
                        ((and avy-word-punc-regexp
                              (string-match avy-word-punc-regexp str))
                         str)
                        (t
                         (concat
                          "\\b"
                          str)))))
      (avy--generic-jump regex arg avy-style))))

(declare-function subword-backward "subword")

;;;###autoload
(defun avy-goto-subword-0 (&optional arg predicate)
  "Jump to a word or subword start.

The window scope is determined by `avy-all-windows' (ARG negates it).

When PREDICATE is non-nil it's a function of zero parameters that
should return true."
  (interactive "P")
  (require 'subword)
  (avy--with-avy-keys avy-goto-subword-0
    (let ((case-fold-search nil)
          candidates)
      (avy-dowindows arg
        (let ((ws (window-start))
              window-cands)
          (save-excursion
            (goto-char (window-end (selected-window) t))
            (subword-backward)
            (while (> (point) ws)
              (when (or (null predicate)
                        (and predicate (funcall predicate)))
                (push (cons (point) (selected-window)) window-cands))
              (subword-backward)))
          (setq candidates (nconc candidates window-cands))))
      (avy--goto
       (avy--process candidates (avy--style-fn avy-style))))))

;;;###autoload
(defun avy-goto-subword-1 (&optional arg)
  "Prompt for a subword start char and jump there.
The window scope is determined by `avy-all-windows' (ARG negates it).
The case is ignored."
  (interactive "P")
  (avy--with-avy-keys avy-goto-subword-1
    (let ((char (downcase (read-char "char: "))))
      (avy-goto-subword-0
       arg (lambda () (eq (downcase (char-after)) char))))))

(defun avy--line (&optional arg)
  "Select a line.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (let ((avy-background nil)
        candidates)
    (avy-dowindows arg
      (let ((ws (window-start)))
        (save-excursion
          (save-restriction
            (narrow-to-region ws (window-end (selected-window) t))
            (goto-char (point-min))
            (while (< (point) (point-max))
              (unless (get-char-property
                       (max (1- (point)) ws) 'invisible)
                (push (cons (point) (selected-window)) candidates))
              (forward-line 1))))))
    (avy--process (nreverse candidates) #'avy--overlay-pre)))

;;;###autoload
(defun avy-goto-line (&optional arg)
  "Jump to a line start in current buffer.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive "P")
  (avy--with-avy-keys avy-goto-line
    (let ((avy-handler-function
           (lambda (char)
             (if (or (< char ?0)
                     (> char ?9))
                 (avy-handler-default char)
               (let ((line (read-from-minibuffer
                            "Goto line: " (string char))))
                 (when line
                   (goto-char (point-min))
                   (forward-line (1- (string-to-number line)))
                   (throw 'done 'exit)))))))
      (avy--goto (avy--line arg)))))

;;;###autoload
(defun avy-copy-line (arg)
  "Copy a selected line above the current line.
ARG lines can be used."
  (interactive "p")
  (avy--with-avy-keys avy-copy-line
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
         "\n")))))

;;;###autoload
(defun avy-move-line (arg)
  "Move a selected line above the current line.
ARG lines can be used."
  (interactive "p")
  (avy--with-avy-keys avy-move-line
    (let ((start (car (avy--line))))
      (move-beginning-of-line nil)
      (save-excursion
        (save-excursion
          (goto-char start)
          (move-end-of-line arg)
          (kill-region start (point)))
        (insert
         (current-kill 0)
         "\n")))))

;;;###autoload
(defun avy-copy-region ()
  "Select two lines and copy the text between them here."
  (interactive)
  (avy--with-avy-keys avy-copy-region
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
         pad)))))

;;;###autoload
(defun avy-setup-default ()
  "Setup the default shortcuts."
  (eval-after-load "isearch"
    '(define-key isearch-mode-map (kbd "C-'") 'avy-isearch)))

(defcustom avy-timeout-seconds 0.5
  "How many seconds to wait for the second char.")

;;;###autoload
(defun avy-goto-char-timer (&optional arg)
  "Read one or two consecutive chars and jump to the first one.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive "P")
  (let ((c1 (read-char "char 1: "))
        (c2 (read-char "char 2: " nil avy-timeout-seconds)))
    (avy--generic-jump
     (regexp-quote
      (if c2
          (string c1 c2)
        (string c1)))
     arg
     avy-style)))

(define-obsolete-variable-alias
    'avy-goto-char-style 'avy-style "0.1.0"
    "Use `avy-style' and `avy-styles-alist' instead.")
(define-obsolete-variable-alias
    'avy-goto-word-style 'avy-style "0.1.0"
    "Use `avy-style' and `avy-styles-alist' instead.")
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
