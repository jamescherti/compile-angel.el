# compile-angel.el
![Build Status](https://github.com/jamescherti/compile-angel.el/actions/workflows/ci.yml/badge.svg)
![License](https://img.shields.io/github/license/jamescherti/compile-angel.el)
![](https://raw.githubusercontent.com/jamescherti/compile-angel.el/main/.images/made-for-gnu-emacs.svg)

Compile Emacs Lisp libraries automatically. 

This is a lightweight alternative to `auto-compile`. Here are the main differences with `auto-compile`:
- compile-angel does not only compile Emacs Lisp files loaded with `load` and `require`, but also handles files that `auto-compile` misses, such as those that are deferred (e.g., with `defer t` and `use-package`) or `autoload`.
- `compile-angel` is lightweight (200 lines vs 885 lines for `auto-compile`)

## Installation

### Install using straight

To install `compile-angel` using `straight.el`:

1. It if hasn't already been done, [add the straight.el bootstrap code](https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started) to your init file.
2. Add the following code to the Emacs init file:
```emacs-lisp
(use-package compile-angel
  :ensure t
  :straight (compile-angel
             :type git
             :host github
             :repo "jamescherti/compile-angel.el"))
```

## Author and License

The `compile-angel` Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2024 James Cherti

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.

## Links

- [compile-angel.el @GitHub](https://github.com/jamescherti/compile-angel.el)
