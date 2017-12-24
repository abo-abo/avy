## Introduction

`avy` is a GNU Emacs package for jumping to visible text using a char-based decision tree.  See also [ace-jump-mode](https://github.com/winterTTr/ace-jump-mode) and [vim-easymotion](https://github.com/Lokaltog/vim-easymotion) - `avy` uses the same idea.

![logo](https://raw.githubusercontent.com/wiki/abo-abo/avy/images/avy-avatar-1.png)

## Command overview

You can bind some of these useful commands in your config.

### `avy-goto-char`

> Input one char, jump to it with a tree.

```elisp
(global-set-key (kbd "C-:") 'avy-goto-char)
```

After <kbd>C-: b</kbd>:

![avy-goto-char](https://raw.githubusercontent.com/wiki/nloyola/avy/images/avy-goto-char.png)

### `avy-goto-char-2`

> Input two consecutive chars, jump to the first one with a tree.

The advantage over the previous one is less candidates for the tree search. And it's not too inconvenient to enter two consecutive chars instead of one.

```elisp
(global-set-key (kbd "C-'") 'avy-goto-char-2)
```

After <kbd>C-' bu</kbd>:

![avy-goto-char-2](http://oremacs.com/download/avi-goto-char-2.png)

### `avy-goto-char-timer`

> Input an arbitrary amount of consecutive chars, jump to the first one with a tree.

This is a more flexible version of `avy-goto-char-2`. First part works similarly to `isearch`: you type a query and it's highlighted dynamically on the screen.  When you stop typing for `avy-timeout-seconds` (0.5s by default), you'll be able to select one of the candidates with `avy`.

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

### Org-mode commands

  * `avy-org-goto-heading-timer`: Type part of an Org heading.  When you stop typing, if only one heading on the screen matches, it will be jumped to; if more than one matches, you can jump to a heading with Avy.  This is like `avy-goto-char-timer` but for Org headings.
  * `avy-org-refile-as-child`: With point in an entry you want to refile, run this command, select a heading with Avy, and the entry will be refiled as its first child heading.  This makes it quick and easy to refile to headings that are visible on-screen, even to other windows or buffers.

### Other commands

There are some more commands which you can explore yourself by looking at the code.

### Bindings

You add this to your config to bind some stuff:

```elisp
(avy-setup-default)
(global-set-key (kbd "C-c C-j") 'avy-resume)
```

It will bind, for example, `avy-isearch` to <kbd>C-'</kbd> in `isearch-mode-map`, so that you can select one of the currently visible `isearch` candidates using `avy`.

### Customization

See the comprehensive custom variable list on [the defcustom wiki page](https://github.com/abo-abo/avy/wiki/defcustom).

See how to write your own avy commands on [the custom-commands wiki page](https://github.com/abo-abo/avy/wiki/custom-commands).

## Contributing

### Copyright Assignment

Avy is subject to the same [copyright assignment](http://www.gnu.org/prep/maintain/html_node/Copyright-Papers.html) policy as Emacs itself, org-mode, CEDET and other packages in [GNU ELPA](http://elpa.gnu.org/packages/). Any [legally significant](http://www.gnu.org/prep/maintain/html_node/Legally-Significant.html#Legally-Significant) contributions can only be accepted after the author has completed their paperwork. Please see [the request form](http://git.savannah.gnu.org/cgit/gnulib.git/tree/doc/Copyright/request-assign.future) if you want to proceed.

The copyright assignment isn't a big deal, it just says that the copyright for your submitted changes to Emacs belongs to the FSF. This assignment works for all projects related to Emacs. To obtain it, you need to send one email, then send one letter (if you live in the US, it's digital), and wait for some time (in my case, I had to wait for one month).

### Style

The basic code style guide is to use `(setq indent-tabs-mode nil)`. It is provided for you in [.dir-locals.el](https://github.com/abo-abo/avy/blob/master/.dir-locals.el), please obey it.

Before submitting the change, run `make compile` and `make test` to make sure that it doesn't introduce new compile warnings or test failures. Also run `make checkdoc` to see that your changes obey the documentation guidelines.

Use your own judgment for the commit messages, I recommend a verbose style using `magit-commit-add-log`.
