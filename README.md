## Introduction

`avy-jump` is a GNU Emacs package for jumping to visible text using a char-based decision tree.  See also [ace-jump-mode](https://github.com/winterTTr/ace-jump-mode) and [vim-easymotion](https://github.com/Lokaltog/vim-easymotion) - `avy-jump` uses the same idea.

![logo](https://raw.githubusercontent.com/wiki/abo-abo/avy-jump/images/avy-avatar-1.png)

## Command overview

You can bind some of these useful commands in your config.

### `avy-goto-char`

> Input one char, jump to it with a tree.

```elisp
(global-set-key (kbd "π") 'avy-goto-char)
```

After <kbd>πb</kbd>:

![avy-goto-char](http://oremacs.com/download/avi-goto-char.png)

### `avy-goto-char-2`

> Input two consecutive chars, jump to the first one with a tree.

The advantage over the previous one is less candidates for the tree search. And it's not too inconvenient to enter two consecutive chars instead of one.

```elisp
(global-set-key (kbd "C-'") 'avy-goto-char-2)
```

After <kbd>C-' bu</kbd>:

![avy-goto-char-2](http://oremacs.com/download/avi-goto-char-2.png)

### `avy-goto-line`

> Input zero chars, jump to a line start with a tree.

```elisp
(global-set-key (kbd "M-g f") 'avy-goto-line)
```

After <kbd>M-g f</kbd>:

![avy-goto-line](http://oremacs.com/download/avi-goto-line.png)

You can actually replace the <kbd>M-g g</kbd> binding of `goto-line`, since if you enter a digit for `avy-goto-line`, it will switch to `goto-line` with that digit already entered.

### `avy-goto-word-1`

> Input one char at word start, jump to a word start with a tree.

```elisp
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
```

After <kbd>M-g wb</kbd>:

![avy-goto-word-1](http://oremacs.com/download/avi-goto-word-1.png)

### `avy-goto-word-0`

> Input zero chars, jump to a word start with a tree.

Compared to `avy-goto-word-1`, there are a lot more candidates. But at a least there's not need to input the initial char.

```elisp
(global-set-key (kbd "M-g e") 'avy-goto-word-0)
```

After <kbd>M-g e</kbd>:

![avy-goto-word-0](http://oremacs.com/download/avi-goto-word-0.png)


### Other commands

There are some more commands which you can explore yourself by looking at the code.

### Bindings

You add this to your config to bind some stuff:

```elisp
(avy-setup-default)
```

It will bind, for example, `avy-isearch` to <kbd>C-'</kbd> in `isearch-mode-map`, so that you can select one of the currently visible `isearch` candidates using `avy`.

### Style customization

There are four styles of overlay functions to choose from. You can choose to use one style for all functions, or you can select a different style for each function.  Customize `avy-style` and `avy-styles-alist` respectively for this.
The styles to choose from:

- pre: the full path will be displayed before target, leaving all original text
- at: the single character path will be displayed on target, obscuring the target
- at-full: the full path will be displayed on target, obscuring the target and the text behind it
- post: the full path will be displayed after target, leaving all original text

At first it seems that `pre` and `post` are advantageous over `at` and `at-full`, since you can still see the original text with them.  However, they make the text shift a bit. If you don't like that, use either `at` or `at-full`.
