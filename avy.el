;;; avy.el --- set-based completion -*- lexical-binding: t -*-

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/avy
;; Version: 0.3.0
;; Package-Requires: ((emacs "24.1") (cl-lib "0.5"))
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
;; This package provides a generic completion method based on building
;; a balanced decision tree with each candidate being a leaf.  To
;; traverse the tree from the root to a desired leaf, typically a
;; sequence of `read-key' can be used.
;;
;; In order for `read-key' to make sense, the tree needs to be
;; visualized appropriately, with a character at each branch node.  So
;; this completion method works only for things that you can see on
;; your screen, all at once:
;;
;; * character positions
;; * word or subword start positions
;; * line beginning positions
;; * link positions
;; * window positions
;;
;; If you're familiar with the popular `ace-jump-mode' package, this
;; package does all that and more, without the implementation
;; headache.

;;; Code:
(require 'cl-lib)
(require 'ring)

;;* Customization
(defgroup avy nil
  "Jump to things tree-style."
  :group 'convenience
  :prefix "avy-")

(defcustom avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
  "Default keys for jumping.
Any key is either a character representing a self-inserting
key (letters, digits, punctuation, etc.) or a symbol denoting a
non-printing key like an arrow key (left, right, up, down).  For
non-printing keys, a corresponding entry in
`avy-key-to-char-alist' must exist in order to visualize the key
in the avy overlays."
  :type '(repeat :tag "Keys" (choice (character :tag "char")
                              (symbol :tag "non-printing key"))))

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

(defcustom avy-style 'at-full
  "The default method of displaying the overlays.
Use `avy-styles-alist' to customize this per-command."
  :type '(choice
          (const :tag "Pre" pre)
          (const :tag "At" at)
          (const :tag "At Full" at-full)
          (const :tag "Post" post)
          (const :tag "De Bruijn" de-bruijn)))

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
                       (const :tag "Post" post)
                       (const :tag "De Bruijn" de-bruijn))))

(defcustom avy-dispatch-alist
  '((?x . avy-action-kill)
    (?m . avy-action-mark)
    (?n . avy-action-copy))
  "List of actions for `avy-handler-default'.

Each item is (KEY . ACTION).  When KEY not on `avy-keys' is
pressed during the dispatch, ACTION is set to replace the default
`avy-action-goto' once a candidate is finally selected."
  :type
  '(alist
    :key-type (choice (character :tag "Char"))
    :value-type (choice
                 (const :tag "Mark" avy-action-mark)
                 (const :tag "Copy" avy-action-copy)
                 (const :tag "Kill" avy-action-kill))))

(defcustom avy-background nil
  "When non-nil, a gray background will be added during the selection."
  :type 'boolean)

(defcustom avy-all-windows t
  "Determine the list of windows to consider in search of candidates."
  :type
  '(choice
    (const :tag "All Frames" all-frames)
    (const :tag "This Frame" t)
    (const :tag "This Window" nil)))

(defcustom avy-case-fold-search t
  "Non-nil if searches should ignore case."
  :type 'boolean)

(defcustom avy-word-punc-regexp "[!-/:-@[-`{-~]"
  "Regexp of punctuation chars that count as word starts for `avy-goto-word-1.
When nil, punctuation chars will not be matched.

\"[!-/:-@[-`{-~]\" will match all printable punctuation chars."
  :type 'regexp)

(defcustom avy-ignored-modes '(image-mode doc-view-mode pdf-view-mode)
  "List of modes to ignore when searching for candidates.
Typically, these modes don't use the text representation.")

(defvar avy-translate-char-function #'identity
  "Function to translate user input key into another key.
For example, to make SPC do the same as ?a, use
\(lambda (c) (if (= c 32) ?a c)).")

(defface avy-lead-face-0
  '((t (:foreground "white" :background "#4f57f9")))
  "Face used for first non-terminating leading chars.")

(defface avy-lead-face-1
  '((t (:foreground "white" :background "gray")))
  "Face used for matched leading chars.")

(defface avy-lead-face-2
  '((t (:foreground "white" :background "#f86bf3")))
  "Face used for leading chars.")

(defface avy-lead-face
  '((t (:foreground "white" :background "#e52b50")))
  "Face used for the leading chars.")

(defface avy-background-face
  '((t (:foreground "gray40")))
  "Face for whole window background during selection.")

(defface avy-goto-char-timer-face
  '((t (:inherit highlight)))
  "Face for matches during reading chars using `avy-goto-char-timer'.")

(defconst avy-lead-faces '(avy-lead-face
                           avy-lead-face-0
                           avy-lead-face-2
                           avy-lead-face
                           avy-lead-face-0
                           avy-lead-face-2)
  "Face sequence for `avy--overlay-at-full'.")

(defvar avy-key-to-char-alist '((left . ?◀)
                                (right . ?▶)
                                (up . ?▲)
                                (down . ?▼)
                                (prior . ?△)
                                (next . ?▽))
  "An alist from non-character keys to printable chars used in avy overlays.
This alist must contain all keys used in `avy-keys' which are not
self-inserting keys and thus aren't read as characters.")

;;* Internals
;;** Tree
(defmacro avy-multipop (lst n)
  "Remove LST's first N elements and return them."
  `(if (<= (length ,lst) ,n)
       (prog1 ,lst
         (setq ,lst nil))
     (prog1 ,lst
       (setcdr
        (nthcdr (1- ,n) (prog1 ,lst (setq ,lst (nthcdr ,n ,lst))))
        nil))))

(defun avy--de-bruijn (keys n)
  "De Bruijn sequence for alphabet KEYS and subsequences of length N."
  (let* ((k (length keys))
         (a (make-list (* n k) 0))
         sequence)
    (cl-labels ((db (T p)
                    (if (> T n)
                        (if (eq (% n p) 0)
                            (setq sequence
                                  (append sequence
                                          (cl-subseq a 1 (1+ p)))))
                      (setf (nth T a) (nth (- T p) a))
                      (db (1+ T) p)
                      (cl-loop for j from (1+ (nth (- T p) a)) to (1- k) do
                               (setf (nth T a) j)
                               (db (1+ T) T)))))
      (db 1 1)
      (mapcar (lambda (n)
                (nth n keys))
              sequence))))

(defun avy--path-alist-1 (lst seq-len keys)
  "Build a De Bruin sequence from LST.
SEQ-LEN is how many elements of KEYS it takes to identify a match."
  (let ((db-seq (avy--de-bruijn keys seq-len))
        prev-pos prev-seq prev-win path-alist)
    ;; The De Bruijn seq is cyclic, so append the seq-len - 1 first chars to
    ;; the end.
    (setq db-seq (nconc db-seq (cl-subseq db-seq 0 (1- seq-len))))
    (cl-labels ((subseq-and-pop ()
                  (when (nth (1- seq-len) db-seq)
                    (prog1 (cl-subseq db-seq 0 seq-len)
                      (pop db-seq)))))
      (while lst
        (let* ((cur (car lst))
               (pos (cond
                      ;; ace-window has matches of the form (pos . wnd)
                      ((integerp (car cur)) (car cur))
                      ;; avy-jump have form ((start . end) . wnd)
                      ((consp (car cur)) (caar cur))
                      (t (error "Unexpected match representation: %s" cur))))
               (win (cdr cur))
               (path (if prev-pos
                         (let ((diff (if (eq win prev-win)
                                         (- pos prev-pos)
                                       0)))
                           (when (and (> diff 0) (< diff seq-len))
                             (while (and (nth (1- seq-len) db-seq)
                                         (not
                                          (eq 0 (cl-search
                                                 (cl-subseq prev-seq diff)
                                                 (cl-subseq db-seq 0 seq-len)))))
                               (pop db-seq)))
                           (subseq-and-pop))
                       (subseq-and-pop))))
          (if (not path)
              (setq lst nil
                    path-alist nil)
            (push (cons path (car lst)) path-alist)
            (setq prev-pos pos
                  prev-seq path
                  prev-win win
                  lst (cdr lst))))))
    (nreverse path-alist)))

(defun avy-tree (lst keys)
  "Coerce LST into a balanced tree.
The degree of the tree is the length of KEYS.
KEYS are placed appropriately on internal nodes."
  (let ((len (length keys)))
    (cl-labels
        ((rd (ls)
           (let ((ln (length ls)))
             (if (< ln len)
                 (cl-pairlis keys
                             (mapcar (lambda (x) (cons 'leaf x)) ls))
               (let ((ks (copy-sequence keys))
                     res)
                 (dolist (s (avy-subdiv ln len))
                   (push (cons (pop ks)
                               (if (eq s 1)
                                   (cons 'leaf (pop ls))
                                 (rd (avy-multipop ls s))))
                         res))
                 (nreverse res))))))
      (rd lst))))

(defun avy-subdiv (n b)
  "Distribute N in B terms in a balanced way."
  (let* ((p (1- (floor (+ (log n b) 1e-6))))
         (x1 (expt b p))
         (x2 (* b x1))
         (delta (- n x2))
         (n2 (/ delta (- x2 x1)))
         (n1 (- b n2 1)))
    (append
     (make-list n1 x1)
     (list
      (- n (* n1 x1) (* n2 x2)))
     (make-list n2 x2))))

(defun avy-traverse (tree walker &optional recur-key)
  "Traverse TREE generated by `avy-tree'.
WALKER is a function that takes KEYS and LEAF.

RECUR-KEY is used in recursion.

LEAF is a member of LST argument of `avy-tree'.

KEYS is the path from the root of `avy-tree' to LEAF."
  (dolist (br tree)
    (let ((key (cons (car br) recur-key)))
      (if (eq (cadr br) 'leaf)
          (funcall walker key (cddr br))
        (avy-traverse (cdr br) walker key)))))

(defvar avy-action nil
  "Function to call at the end of select.")

(defun avy-handler-default (char)
  "The default handler for a bad CHAR."
  (let (dispatch)
    (if (setq dispatch (assoc char avy-dispatch-alist))
        (progn
          (setq avy-action (cdr dispatch))
          (throw 'done 'restart))
      (signal 'user-error (list "No such candidate" char))
      (throw 'done nil))))

(defvar avy-handler-function 'avy-handler-default
  "A function to call for a bad `read-key' in `avy-read'.")

(defvar avy-current-path ""
  "Store the current incomplete path during `avy-read'.")

(defun avy-read (tree display-fn cleanup-fn)
  "Select a leaf from TREE using consecutive `read-char'.

DISPLAY-FN should take CHAR and LEAF and signify that LEAFs
associated with CHAR will be selected if CHAR is pressed.  This is
commonly done by adding a CHAR overlay at LEAF position.

CLEANUP-FN should take no arguments and remove the effects of
multiple DISPLAY-FN invokations."
  (catch 'done
    (setq avy-current-path "")
    (while tree
      (let ((avy--leafs nil))
        (avy-traverse tree
                      (lambda (path leaf)
                        (push (cons path leaf) avy--leafs)))
        (dolist (x avy--leafs)
          (funcall display-fn (car x) (cdr x))))
      (let ((char (funcall avy-translate-char-function (read-key)))
            branch)
        (funcall cleanup-fn)
        (if (setq branch (assoc char tree))
            (if (eq (car (setq tree (cdr branch))) 'leaf)
                (throw 'done (cdr tree))
              (setq avy-current-path
                    (concat avy-current-path (string (avy--key-to-char char)))))
          (funcall avy-handler-function char))))))

(defun avy-read-de-bruijn (lst keys)
  "Select from LST dispatching on KEYS."
  ;; In theory, the De Bruijn sequence B(k,n) has k^n subsequences of length n
  ;; (the path length) usable as paths, thus that's the lower bound.  Due to
  ;; partially overlapping matches, not all subsequences may be usable, so it's
  ;; possible that the path-len must be incremented, e.g., if we're matching
  ;; for x and a buffer contains xaxbxcx only every second subsequence is
  ;; usable for the four matches.
  (catch 'done
    (let* ((path-len (ceiling (log (length lst) (length keys))))
           (alist (avy--path-alist-1 lst path-len keys)))
      (while (not alist)
        (cl-incf path-len)
        (setq alist (avy--path-alist-1 lst path-len keys)))
      (let* ((len (length (caar alist)))
             (i 0))
        (setq avy-current-path "")
        (while (< i len)
          (dolist (x (reverse alist))
            (avy--overlay-at-full (reverse (car x)) (cdr x)))
          (let ((char (funcall avy-translate-char-function (read-key))))
            (avy--remove-leading-chars)
            (setq alist
                  (delq nil
                        (mapcar (lambda (x)
                                  (when (eq (caar x) char)
                                    (cons (cdr (car x)) (cdr x))))
                                alist)))
            (setq avy-current-path
                  (concat avy-current-path (string (avy--key-to-char char))))
            (cl-incf i)
            (unless alist
              (funcall avy-handler-function char))))
        (cdar alist)))))

;;** Rest
(defun avy-window-list ()
  "Return a list of windows depending on `avy-all-windows'."
  (cond ((eq avy-all-windows 'all-frames)
         (cl-mapcan #'window-list (frame-list)))

        ((eq avy-all-windows t)
         (window-list))

        ((null avy-all-windows)
         (list (selected-window)))

        (t
         (error "Unrecognized option: %S" avy-all-windows))))

(defcustom avy-all-windows-alt t
  "The alternative `avy-all-windows' for use with \\[universal-argument]."
  :type '(choice
          (const :tag "All windows on the current frame" t)
          (const :tag "All windows on all frames" all-frames)))

(defmacro avy-dowindows (flip &rest body)
  "Depending on FLIP and `avy-all-windows' run BODY in each or selected window."
  (declare (indent 1)
           (debug (form body)))
  `(let ((avy-all-windows (if ,flip
                              avy-all-windows-alt
                            avy-all-windows)))
     (dolist (wnd (avy-window-list))
       (with-selected-window wnd
         (unless (memq major-mode avy-ignored-modes)
           ,@body)))))

(defmacro avy-with (command &rest body)
  "Set `avy-keys' according to COMMAND and execute BODY.
Set `avy-style' according to COMMMAND as well."
  (declare (indent 1)
           (debug (form body)))
  `(let ((avy-keys (or (cdr (assq ',command avy-keys-alist))
                       avy-keys))
         (avy-style (or (cdr (assq ',command avy-styles-alist))
                        avy-style)))
     (setq avy-action nil)
     ,@body))

(defun avy-action-goto (pt)
  "Goto PT."
  (goto-char pt))

(defun avy-action-mark (pt)
  "Mark sexp at PT."
  (goto-char pt)
  (set-mark (point))
  (forward-sexp))

(defun avy-action-copy (pt)
  "Copy sexp starting on PT."
  (save-excursion
    (let (str)
      (goto-char pt)
      (forward-sexp)
      (setq str (buffer-substring pt (point)))
      (kill-new str)
      (message "Copied: %s" str))))

(defun avy-action-kill (pt)
  "Kill sexp at PT."
  (goto-char pt)
  (forward-sexp)
  (kill-region pt (point))
  (message "Killed: %s" (current-kill 0)))

(defun avy--process (candidates overlay-fn)
  "Select one of CANDIDATES using `avy-read'.
Use OVERLAY-FN to visualize the decision overlay."
  (unless (and (consp (car candidates))
               (windowp (cdar candidates)))
    (setq candidates
          (mapcar (lambda (x) (cons x (selected-window)))
                  candidates)))
  (let ((len (length candidates))
        (cands (copy-sequence candidates))
        res)
    (if (= len 0)
        (message "zero candidates")
      (if (= len 1)
          (setq res (car candidates))
        (unwind-protect
             (progn
               (avy--make-backgrounds
                (avy-window-list))
               (setq res (if (eq avy-style 'de-bruijn)
                             (avy-read-de-bruijn
                              candidates avy-keys)
                           (avy-read (avy-tree candidates avy-keys)
                                     overlay-fn
                                     #'avy--remove-leading-chars))))
          (avy--done)))
      (cond ((eq res 'restart)
             (avy--process cands overlay-fn))
            ;; ignore exit from `avy-handler-function'
            ((eq res 'exit))
            (t
             (avy-push-mark)
             (when (and (consp res)
                        (windowp (cdr res)))
               (let* ((window (cdr res))
                      (frame (window-frame window)))
                 (unless (equal frame (selected-frame))
                   (select-frame-set-input-focus frame))
                 (select-window window))
               (setq res (car res)))

             (funcall (or avy-action 'avy-action-goto)
                      (if (consp res)
                          (car res)
                        res)))))))

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

(defun avy--next-visible-point ()
  "Return the next closest point without 'invisible property."
  (let ((s (point)))
    (while (and (not (= (point-max) (setq s (next-overlay-change s))))
                (get-char-property s 'invisible)))
    s))

(defun avy--next-invisible-point ()
  "Return the next closest point with 'invisible property."
  (let ((s (point)))
    (while (and (not (= (point-max) (setq s (next-overlay-change s))))
                (not (get-char-property s 'invisible))))
    s))

(defun avy--find-visible-regions (rbeg rend)
  "Return a list of all visible regions between RBEG and REND."
  (setq rbeg (max rbeg (point-min)))
  (setq rend (min rend (point-max)))
  (when (< rbeg rend)
    (let (visibles beg)
      (save-excursion
        (save-restriction
          (narrow-to-region rbeg rend)
          (setq beg (goto-char (point-min)))
          (while (not (= (point) (point-max)))
            (goto-char (avy--next-invisible-point))
            (push (cons beg (point)) visibles)
            (setq beg (goto-char (avy--next-visible-point))))
          (nreverse visibles))))))

(defun avy--regex-candidates (regex &optional beg end pred group)
  "Return all elements that match REGEX.
Each element of the list is ((BEG . END) . WND)
When PRED is non-nil, it's a filter for matching point positions.
When GROUP is non-nil, (BEG . END) should delimit that regex group."
  (setq group (or group 0))
  (let ((case-fold-search (or avy-case-fold-search
                              (not (string= regex (upcase regex)))))
        candidates)
    (avy-dowindows current-prefix-arg
      (dolist (pair (avy--find-visible-regions
                     (or beg (window-start))
                     (or end (window-end (selected-window) t))))
        (save-excursion
          (goto-char (car pair))
          (while (re-search-forward regex (cdr pair) t)
            (unless (get-char-property (1- (point)) 'invisible)
              (when (or (null pred)
                        (funcall pred))
                (push (cons (cons (match-beginning group)
                                  (match-end group))
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

(defun avy--old-str (pt wnd)
  "Return a one-char string at PT in WND."
  (let ((old-str (with-selected-window wnd
                   (buffer-substring pt (1+ pt)))))
    (if avy-background
        (propertize old-str 'face 'avy-background-face)
      old-str)))

(defun avy--overlay (str beg end wnd &optional compose-fn)
  "Create an overlay with STR from BEG to END in WND.
COMPOSE-FN is a lambda that concatenates the old string at BEG with STR."
  (when (<= (1+ beg) (with-selected-window wnd (point-max)))
    (let* ((beg (+ beg avy--overlay-offset))
           (ol (make-overlay beg (or end (1+ beg)) (window-buffer wnd)))
           (old-str (avy--old-str beg wnd))
           (os-line-prefix (get-text-property 0 'line-prefix old-str))
           (os-wrap-prefix (get-text-property 0 'wrap-prefix old-str)))
      (when os-line-prefix
        (add-text-properties 0 1 `(line-prefix ,os-line-prefix) str))
      (when os-wrap-prefix
        (add-text-properties 0 1 `(wrap-prefix ,os-wrap-prefix) str))
      (overlay-put ol 'window wnd)
      (overlay-put ol 'category 'avy)
      (overlay-put ol 'display (funcall
                                (or compose-fn #'concat)
                                str old-str))
      (push ol avy--overlays-lead))))

(defcustom avy-highlight-first nil
  "When non-nil highlight the first decision char with `avy-lead-face-0'.
Do this even when the char is terminating."
  :type 'boolean)

(defun avy--key-to-char (c)
  "If C is no character, translate it using `avy-key-to-char-alist'."
  (if (characterp c)
      c
    (or (cdr (assoc c avy-key-to-char-alist))
        (error "Unknown key %s" c))))

(defun avy-candidate-beg (leaf)
  "Return the start position for LEAF."
  (cond ((numberp leaf)
         leaf)
        ((consp (car leaf))
         (caar leaf))
        (t
         (car leaf))))

(defun avy-candidate-end (leaf)
  "Return the end position for LEAF."
  (cond ((numberp leaf)
         leaf)
        ((consp (car leaf))
         (cdar leaf))
        (t
         (car leaf))))

(defun avy-candidate-wnd (leaf)
  "Return the window for LEAF."
  (if (consp leaf)
      (cdr leaf)
    (selected-window)))

(defun avy--overlay-pre (path leaf)
  "Create an overlay with PATH at LEAF.
PATH is a list of keys from tree root to LEAF.
LEAF is normally ((BEG . END) . WND)."
  (let* ((path (mapcar #'avy--key-to-char path))
         (str (propertize (apply #'string (reverse path))
                          'face 'avy-lead-face)))
    (when (or avy-highlight-first (> (length str) 1))
      (set-text-properties 0 1 '(face avy-lead-face-0) str))
    (setq str (concat
               (propertize avy-current-path
                           'face 'avy-lead-face-1)
               str))
    (avy--overlay
     str
     (avy-candidate-beg leaf) nil
     (avy-candidate-wnd leaf))))

(defun avy--overlay-at (path leaf)
  "Create an overlay with PATH at LEAF.
PATH is a list of keys from tree root to LEAF.
LEAF is normally ((BEG . END) . WND)."
  (let* ((path (mapcar #'avy--key-to-char path))
         (str (propertize
               (string (car (last path)))
               'face 'avy-lead-face)))
    (avy--overlay
     str
     (avy-candidate-beg leaf) nil
     (avy-candidate-wnd leaf)
     (lambda (str old-str)
       (cond ((string= old-str "\n")
              (concat str "\n"))
             ;; add padding for wide-width character
             ((eq (string-width old-str) 2)
              (concat str " "))
             (t
              str))))))

(defun avy--overlay-at-full (path leaf)
  "Create an overlay with PATH at LEAF.
PATH is a list of keys from tree root to LEAF.
LEAF is normally ((BEG . END) . WND)."
  (let* ((path (mapcar #'avy--key-to-char path))
         (str (propertize
               (apply #'string (reverse path))
               'face 'avy-lead-face))
         (len (length path))
         (beg (avy-candidate-beg leaf))
         (wnd (cdr leaf))
         end)
    (dotimes (i len)
      (set-text-properties (- len i 1) (- len i)
                           `(face ,(nth i avy-lead-faces))
                           str))
    (when (eq avy-style 'de-bruijn)
      (setq str (concat
                 (propertize avy-current-path
                             'face 'avy-lead-face-1)
                 str))
      (setq len (length str)))
    (with-selected-window wnd
      (save-excursion
        (goto-char beg)
        (let* ((lep (if (bound-and-true-p visual-line-mode)
                        (save-excursion
                          (end-of-visual-line)
                          (point))
                      (line-end-position)))
               (len-and-str (avy--update-offset-and-str len str lep)))
          (setq len (car len-and-str))
          (setq str (cdr len-and-str))
          (setq end (if (= beg lep)
                        (1+ beg)
                      (min (+ beg
                              (if (eq (char-after) ?\t)
                                  1
                                len))
                           lep)))
          (when (and (bound-and-true-p visual-line-mode)
                     (> len (- end beg)))
            (setq len (- end beg))
            (let ((old-str (apply #'string (reverse path))))
              (setq str
                    (substring
                     (propertize
                      old-str
                      'face
                      (if (= (length old-str) 1)
                          'avy-lead-face
                        'avy-lead-face-0))
                     0 len)))))))
    (avy--overlay
     str beg end wnd
     (lambda (str old-str)
       (cond ((string= old-str "\n")
              (concat str "\n"))
             ((string= old-str "\t")
              (concat str (make-string (max (- tab-width len) 0) ?\ )))
             (t
              ;; add padding for wide-width character
              (if (eq (string-width old-str) 2)
                  (concat str " ")
                str)))))))

(defun avy--overlay-post (path leaf)
  "Create an overlay with PATH at LEAF.
PATH is a list of keys from tree root to LEAF.
LEAF is normally ((BEG . END) . WND)."
  (let* ((path (mapcar #'avy--key-to-char path))
         (str (propertize (apply #'string (reverse path))
                          'face 'avy-lead-face)))
    (when (or avy-highlight-first (> (length str) 1))
      (set-text-properties 0 1 '(face avy-lead-face-0) str))
    (setq str (concat
               (propertize avy-current-path
                           'face 'avy-lead-face-1)
               str))
    (avy--overlay
     str
     (avy-candidate-end leaf) nil
     (avy-candidate-wnd leaf))))

(defun avy--update-offset-and-str (offset str lep)
  "Recalculate the length of the new overlay at point.

OFFSET is the previous overlay length.
STR is the overlay string that we wish to add.
LEP is the line end position.

We want to add an overlay between point and END=point+OFFSET.
When other overlays already exist between point and END, set
OFFSET to be the difference between the start of the first
overlay and point.  This is equivalent to truncating our new
overlay, so that it doesn't intersect with overlays that already
exist."
  (let* ((wnd (selected-window))
         (beg (point))
         (oov (delq nil
                    (mapcar
                     (lambda (o)
                       (and (eq (overlay-get o 'category) 'avy)
                            (eq (overlay-get o 'window) wnd)
                            (overlay-start o)))
                     (overlays-in beg (min (+ beg offset) lep))))))
    (when oov
      (setq offset (- (apply #'min oov) beg))
      (setq str (substring str 0 offset)))
    (let ((other-ov (cl-find-if
                     (lambda (o)
                       (and (eq (overlay-get o 'category) 'avy)
                            (eq (overlay-start o) beg)
                            (not (eq (overlay-get o 'window) wnd))))
                     (overlays-in (point) (min (+ (point) offset) lep)))))
      (when (and other-ov
                 (> (overlay-end other-ov)
                    (+ beg offset)))
        (setq str (concat str (buffer-substring
                               (+ beg offset)
                               (overlay-end other-ov))))
        (setq offset (- (overlay-end other-ov)
                        beg))))
    (cons offset str)))

(defun avy--style-fn (style)
  "Transform STYLE symbol to a style function."
  (cl-case style
    (pre #'avy--overlay-pre)
    (at #'avy--overlay-at)
    (at-full 'avy--overlay-at-full)
    (post #'avy--overlay-post)
    (de-bruijn #'avy--overlay-at-full)
    (t (error "Unexpected style %S" style))))

(defun avy--generic-jump (regex window-flip style &optional beg end)
  "Jump to REGEX.
When WINDOW-FLIP is non-nil, do the opposite of `avy-all-windows'.
STYLE determines the leading char overlay style.
BEG and END delimit the area where candidates are searched."
  (let ((avy-all-windows
         (if window-flip
             (not avy-all-windows)
           avy-all-windows)))
    (avy--process
     (avy--regex-candidates regex beg end)
     (avy--style-fn style))))

;;* Commands
;;;###autoload
(defun avy-goto-char (char &optional arg)
  "Jump to the currently visible CHAR.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-with avy-goto-char
    (avy--generic-jump
     (if (= 13 char)
         "\n"
       (regexp-quote (string char)))
     arg
     avy-style)))

;;;###autoload
(defun avy-goto-char-in-line (char)
  "Jump to the currently visible CHAR in the current line."
  (interactive (list (read-char "char: " t)))
  (avy-with avy-goto-char
    (avy--generic-jump
     (regexp-quote (string char))
     avy-all-windows
     avy-style
     (line-beginning-position)
     (line-end-position))))

;;;###autoload
(defun avy-goto-char-2 (char1 char2 &optional arg)
  "Jump to the currently visible CHAR1 followed by CHAR2.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive (list (read-char "char 1: " t)
                     (read-char "char 2: " t)
                     current-prefix-arg))
  (avy-with avy-goto-char-2
    (avy--generic-jump
     (regexp-quote (string char1 char2))
     arg
     avy-style)))

;;;###autoload
(defun avy-isearch ()
  "Jump to one of the current isearch candidates."
  (interactive)
  (avy-with avy-isearch
    (let ((avy-background nil))
      (avy--process
       (avy--regex-candidates isearch-string)
       (avy--style-fn avy-style))
      (isearch-done))))

;;;###autoload
(defun avy-goto-word-0 (arg)
  "Jump to a word start.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive "P")
  (avy-with avy-goto-word-0
    (avy--generic-jump "\\b\\sw" arg avy-style)))

;;;###autoload
(defun avy-goto-word-1 (char &optional arg)
  "Jump to the currently visible CHAR at a word start.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-with avy-goto-word-1
    (let* ((str (string char))
           (regex (cond ((string= str ".")
                         "\\.")
                        ((and avy-word-punc-regexp
                              (string-match avy-word-punc-regexp str))
                         (regexp-quote str))
                        (t
                         (concat
                          "\\b"
                          str)))))
      (avy--generic-jump regex arg avy-style))))

(declare-function subword-backward "subword")
(defvar subword-backward-regexp)

(defcustom avy-subword-extra-word-chars '(?{ ?= ?} ?* ?: ?> ?<)
  "A list of characters that should temporarily match \"\\w\".
This variable is used by `avy-goto-subword-0' and `avy-goto-subword-1'."
  :type '(repeat character))

;;;###autoload
(defun avy-goto-subword-0 (&optional arg predicate)
  "Jump to a word or subword start.

The window scope is determined by `avy-all-windows' (ARG negates it).

When PREDICATE is non-nil it's a function of zero parameters that
should return true."
  (interactive "P")
  (require 'subword)
  (avy-with avy-goto-subword-0
    (let ((case-fold-search nil)
          (subword-backward-regexp
           "\\(\\(\\W\\|[[:lower:][:digit:]]\\)\\([!-/:@`~[:upper:]]+\\W*\\)\\|\\W\\w+\\)")
          candidates)
      (avy-dowindows arg
        (let ((syn-tbl (copy-syntax-table)))
          (dolist (char avy-subword-extra-word-chars)
            (modify-syntax-entry char "w" syn-tbl))
          (with-syntax-table syn-tbl
            (let ((ws (window-start))
                  window-cands)
              (save-excursion
                (goto-char (window-end (selected-window) t))
                (subword-backward)
                (while (> (point) ws)
                  (when (or (null predicate)
                            (and predicate (funcall predicate)))
                    (unless (get-char-property (point) 'invisible)
                      (push (cons (point) (selected-window)) window-cands)))
                  (subword-backward)))
              (setq candidates (nconc candidates window-cands))))))
      (avy--process candidates (avy--style-fn avy-style)))))

;;;###autoload
(defun avy-goto-subword-1 (char &optional arg)
  "Jump to the currently visible CHAR at a subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).
The case of CHAR is ignored."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-with avy-goto-subword-1
    (let ((char (downcase char)))
      (avy-goto-subword-0
       arg (lambda () (eq (downcase (char-after)) char))))))

;;;###autoload
(defun avy-goto-word-or-subword-1 ()
  "Forward to `avy-goto-subword-1' or `avy-goto-word-1'.
Which one depends on variable `subword-mode'."
  (interactive)
  (if (bound-and-true-p subword-mode)
      (call-interactively #'avy-goto-subword-1)
    (call-interactively #'avy-goto-word-1)))

(defvar visual-line-mode)

(defun avy--line (&optional arg beg end)
  "Select a line.
The window scope is determined by `avy-all-windows' (ARG negates it).
Narrow the scope to BEG END."
  (let (candidates)
    (avy-dowindows arg
      (let ((ws (or beg (window-start))))
        (save-excursion
          (save-restriction
            (narrow-to-region ws (or end (window-end (selected-window) t)))
            (goto-char (point-min))
            (while (< (point) (point-max))
              (unless (get-char-property
                       (max (1- (point)) ws) 'invisible)
                (push (cons
                       (if (eq avy-style 'post)
                           (line-end-position)
                         (point))
                       (selected-window)) candidates))
              (if visual-line-mode
                  (progn
                    (setq temporary-goal-column 0)
                    (line-move-visual 1 t))
                (forward-line 1)))))))
    (setq avy-action #'identity)
    (avy--process (nreverse candidates) (avy--style-fn avy-style))))

;;;###autoload
(defun avy-goto-line (&optional arg)
  "Jump to a line start in current buffer.

When ARG is 1, jump to lines currently visible, with the option
to cancel to `goto-line' by entering a number.

When ARG is 4, negate the window scope determined by
`avy-all-windows'.

Otherwise, forward to `goto-line' with ARG."
  (interactive "p")
  (if (not (memq arg '(1 4)))
      (progn
        (goto-char (point-min))
        (forward-line (1- arg)))
    (avy-with avy-goto-line
      (let* ((avy-handler-function
              (lambda (char)
                (if (or (< char ?0)
                        (> char ?9))
                    (avy-handler-default char)
                  (let ((line (read-from-minibuffer
                               "Goto line: " (string char))))
                    (when line
                      (avy-push-mark)
                      (save-restriction
                        (widen)
                        (goto-char (point-min))
                        (forward-line (1- (string-to-number line))))
                      (throw 'done 'exit))))))
             (r (avy--line (eq arg 4))))
        (unless (eq r t)
          (avy-action-goto r))))))

;;;###autoload
(defun avy-goto-line-above ()
  "Goto visible line above the cursor."
  (interactive)
  (let* ((avy-all-windows nil)
         (r (avy--line nil (window-start)
                       (line-beginning-position))))
    (unless (eq r t)
      (avy-action-goto r))))

;;;###autoload
(defun avy-goto-line-below ()
  "Goto visible line below the cursor."
  (interactive)
  (let* ((avy-all-windows nil)
         (r (avy--line
             nil (line-beginning-position 2)
             (window-end (selected-window) t))))
    (unless (eq r t)
      (avy-action-goto r))))

(defcustom avy-line-insert-style 'above
  "How to insert the newly copied/cut line."
  :type '(choice
          (const :tag "Above" above)
          (const :tag "Below" below)))

;;;###autoload
(defun avy-copy-line (arg)
  "Copy a selected line above the current line.
ARG lines can be used."
  (interactive "p")
  (avy-with avy-copy-line
    (let* ((start (avy--line))
           (str (buffer-substring-no-properties
                 start
                 (save-excursion
                   (goto-char start)
                   (move-end-of-line arg)
                   (point)))))
      (cond ((eq avy-line-insert-style 'above)
             (beginning-of-line)
             (save-excursion
               (insert str "\n")))
            ((eq avy-line-insert-style 'below)
             (end-of-line)
             (insert "\n" str)
             (beginning-of-line))
            (t
             (user-error "Unexpected `avy-line-insert-style'"))))))

;;;###autoload
(defun avy-move-line (arg)
  "Move a selected line above the current line.
ARG lines can be used."
  (interactive "p")
  (avy-with avy-move-line
    (let ((start (avy--line)))
      (save-excursion
        (goto-char start)
        (kill-whole-line arg))
      (cond ((eq avy-line-insert-style 'above)
             (beginning-of-line)
             (save-excursion
               (insert
                (current-kill 0))))
            ((eq avy-line-insert-style 'below)
             (end-of-line)
             (newline)
             (save-excursion
               (insert (substring (current-kill 0) 0 -1))))
            (t
             (user-error "Unexpected `avy-line-insert-style'"))))))

;;;###autoload
(defun avy-copy-region (arg)
  "Select two lines and copy the text between them to point.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil."
  (interactive "P")
  (let ((initial-window (selected-window)))
    (avy-with avy-copy-region
      (let* ((beg (avy--line arg))
             (end (avy--line arg))
             (str (buffer-substring-no-properties
                   beg
                   (save-excursion
                     (goto-char end)
                     (line-end-position)))))
        (select-window initial-window)
        (cond ((eq avy-line-insert-style 'above)
               (beginning-of-line)
               (save-excursion
                 (insert str "\n")))
              ((eq avy-line-insert-style 'below)
               (end-of-line)
               (newline)
               (save-excursion
                 (insert str)))
              (t
               (user-error "Unexpected `avy-line-insert-style'")))))))

;;;###autoload
(defun avy-setup-default ()
  "Setup the default shortcuts."
  (eval-after-load "isearch"
    '(define-key isearch-mode-map (kbd "C-'") 'avy-isearch)))

(defcustom avy-timeout-seconds 0.5
  "How many seconds to wait for the second char.")

(defun avy--read-candidates ()
  "Read as many chars as possible and return their occurences.
At least one char must be read, and then repeatedly one next char
may be read if it is entered before `avy-timeout-seconds'.  `DEL'
deletes the last char entered, and `RET' exits with the currently
read string immediately instead of waiting for another char for
`avy-timeout-seconds'.
The format of the result is the same as that of `avy--regex-candidates'.
This function obeys `avy-all-windows' setting."
  (let ((str "") char break overlays regex)
    (unwind-protect
         (progn
           (while (and (not break)
                       (setq char
                             (read-char (format "char%s: "
                                                (if (string= str "")
                                                    str
                                                  (format " (%s)" str)))
                                        t
                                        (and (not (string= str ""))
                                             avy-timeout-seconds))))
             ;; Unhighlight
             (dolist (ov overlays)
               (delete-overlay ov))
             (setq overlays nil)
             (cond
               ;; Handle RET
               ((= char 13)
                (setq break t))
               ;; Handle DEL
               ((= char 127)
                (let ((l (length str)))
                  (when (>= l 1)
                    (setq str (substring str 0 (1- l))))))
               (t
                (setq str (concat str (list char)))))
             ;; Highlight
             (when (>= (length str) 1)
               (let (found)
                 (avy-dowindows current-prefix-arg
                   (dolist (pair (avy--find-visible-regions
                                  (window-start)
                                  (window-end (selected-window) t)))
                     (save-excursion
                       (goto-char (car pair))
                       (setq regex (regexp-quote str))
                       (while (re-search-forward regex (cdr pair) t)
                         (unless (get-char-property (1- (point)) 'invisible)
                           (let ((ov (make-overlay
                                      (match-beginning 0)
                                      (match-end 0))))
                             (setq found t)
                             (push ov overlays)
                             (overlay-put ov 'window (selected-window))
                             (overlay-put ov 'face 'avy-goto-char-timer-face)))))))
                 ;; No matches at all, so there's surely a typo in the input.
                 (unless found (beep)))))
           (nreverse (mapcar (lambda (ov)
                               (cons (cons (overlay-start ov)
                                           (overlay-end ov))
                                     (overlay-get ov 'window)))
                             overlays)))
      (dolist (ov overlays)
        (delete-overlay ov)))))

;;;###autoload
(defun avy-goto-char-timer (&optional arg)
  "Read one or many consecutive chars and jump to the first one.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive "P")
  (let ((avy-all-windows (if arg
                             (not avy-all-windows)
                           avy-all-windows)))
    (avy-with avy-goto-char-timer
      (avy--process
       (avy--read-candidates)
       (avy--style-fn avy-style)))))

(defvar avy-ring (make-ring 20)
  "Hold the window and point history.")

(defun avy-push-mark ()
  "Store the current point and window."
  (ring-insert avy-ring
               (cons (point) (selected-window)))
  (unless (region-active-p)
    (push-mark)))

(defun avy-pop-mark ()
  "Jump back to the last location of `avy-push-mark'."
  (interactive)
  (let (res)
    (condition-case nil
        (progn
          (while (not (window-live-p
                       (cdr (setq res (ring-remove avy-ring 0))))))
          (let* ((window (cdr res))
                 (frame (window-frame window)))
            (when (and (frame-live-p frame)
                       (not (eq frame (selected-frame))))
              (select-frame-set-input-focus frame))
            (select-window window)
            (goto-char (car res))))
      (error
       (set-mark-command 4)))))

(define-obsolete-function-alias
    'avy--goto 'identity "0.3.0"
    "Don't use this function any more.
`avy--process' will do the jump all by itself.")

(define-obsolete-function-alias 'avy--with-avy-keys 'avy-with "0.3.0")

(provide 'avy)

;;; avy.el ends here
