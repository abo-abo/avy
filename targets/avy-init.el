;;; avy-init.el --- bare avy init

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Oleh Krehel

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(add-to-list 'load-path default-directory)
(mapc #'byte-compile-file '("avy.el"))
(require 'avy)
(if (fboundp 'checkdoc-file)
    (checkdoc-file "avy.el")
  (require 'checkdoc)
  (with-current-buffer (find-file "avy.el")
    (checkdoc-current-buffer t)))

(global-set-key (kbd "C-c j") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
