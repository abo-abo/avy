## Introduction

`avy-jump` is a GNU Emacs package for jumping to visible text using a char-based decision tree.  See also [ace-jump-mode](https://github.com/winterTTr/ace-jump-mode) and [vim-easymotion](https://github.com/Lokaltog/vim-easymotion) - `avy-jump` uses the same idea.

![logo](https://raw.githubusercontent.com/wiki/abo-abo/avy-jump/images/avy-avatar-1.png)

## Command overview

You can bind some of these useful commands in your config:

- `avy-goto-char`: input one char, jump to it with a tree.
- `avy-goto-char-2`: input two consecutive chars, jump to the first one with a tree.
- `avy-goto-word-0`: input zero chars, jump to word start with a tree.
- `avy-goto-word-1`: input one char at word start, jump to word start with a tree.
- `avy-goto-line`: input zero chars, jump to line start with a tree.

There are some more commands which you can explore yourself by looking at the code.
