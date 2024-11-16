# compile-angel.el - Byte-compile and native-compile Emacs Lisp libraries Automatically
![Build Status](https://github.com/jamescherti/compile-angel.el/actions/workflows/ci.yml/badge.svg)
![License](https://img.shields.io/github/license/jamescherti/compile-angel.el)
![](https://raw.githubusercontent.com/jamescherti/compile-angel.el/main/.images/made-for-gnu-emacs.svg)

The **compile-angel** package automatically byte-compiles and native-compiles Emacs Lisp libraries. It offers:
- `(compile-angel-on-load-mode)`: Global mode that compiles .el files before they are loaded.
- `(compile-angel-on-save-local-mode)`: Local mode that compiles .el files whenever the user saves them.

These modes **speed up Emacs by ensuring all libraries are byte-compiled and native-compiled**. Byte-compilation reduces the overhead of loading Emacs Lisp code at runtime, while native compilation optimizes performance by generating machine code specific to your system.

## Before installing

It is recommended to set the following variables in your init file:

``` emacs-lisp
;; Set `load-prefer-newer` to `t` to ensure that Emacs loads the most recent
;; version of byte-compiled or source files.
(setq load-prefer-newer t)

;; Show buffer when there is a warning
(setq warning-minimum-level :warning)
(setq native-comp-async-report-warnings-errors t)

;; Ensure that quitting only occurs once Emacs finishes native compiling,
;; preventing incomplete or leftover compilation files in `/tmp`.
(setq native-comp-async-query-on-exit t)
(setq confirm-kill-processes t)

;; Make sure jit compilation is enabled
(setq native-comp-jit-compilation t)  ;; Let compile-angel handle this
(setq native-comp-deferred-compilation t) ;; Obsolete in Emacs > 29.1
```

Additionally, ensure that native compilation is enabled; this should return t: `(native-comp-available-p)`.

## Installation

### Install compile-angel with straight (Emacs version < 30)

To install *compile-angel* with `straight.el`:

1. It if hasn't already been done, [add the straight.el bootstrap code](https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started) to your init file.
2. Add the following code **at the very beginning of your Emacs init file, before all other packages**:
```emacs-lisp
(use-package compile-angel
  :ensure t
  :demand t
  :straight (compile-angel
             :type git
             :host github
             :repo "jamescherti/compile-angel.el")
  :custom
  (compile-angel-verbose nil)
  :config
  (compile-angel-on-load-mode)
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))
```

## Customizations

``` emacs-lisp
;; Enable/Disable byte compilation and native compilation
(setq compile-angel-enable-byte-compile t)
(setq compile-angel-enable-native-compile t)

;; Enable displaying messages (e.g., when files are compiled)
(setq compile-angel-verbose t)

;; Ignore certain files, for example, for users of the `dir-config` package:
(setq compile-angel-excluded-files-regexps '("/\\.dir-config\\.el$"))

;; Function that determines if an .el file should be compiled. It takes one
;; argument (an EL file) and returns t if the file should be compiled,
;; (By default, `compile-angel-predicate-function` is set to nil, which
;; means that the predicate function is not called.)
(setq compile-angel-predicate-function
   #'(lambda(el-file)
       ;; Show a message
       (message "PREDICATE: %s" el-file)
       ;; Return t to compile the file
       t))
```

## Frequently Asked Questions

## Why did the author develop compile-angel?

The author used to be an auto-compile user, but several of his .el files were not being compiled by auto-compile, which caused Emacs to become slow due to the lack of native compilation. The author experimented for an extended period to understand why auto-compile wasn't compiling many of the .el files in the configuration. The result of those hours of research and testing became a package called compile-angel.

During his investigation, the author discovered that auto-compile was not using autoload and eval-after-load to compile .el files. Even though autoload and eval-after-load don't directly load libraries, they provide a good indication of what will be loaded in the future. In the case of compile-angel, this triggers compilation if the file has not yet been compiled (The compile-angel package checks whether the .elc and/or .eln files are outdated before compiling them; it does not simply compile them without checking.). Special thanks to Jonas Bernoulli, the creator of the auto-compile package, whose work inspired the development of compile-angel.

The compile-angel package was created to offer an alternative to auto-compile that guarantees all .el files are both byte-compiled and native-compiled, which significantly speeds up Emacs.

## What is the difference between auto-compile and compile-angel?

Here are the main differences:
- **Compile-angel ensures more .el files are compiled**: The compile-angel package, in addition to compiling the elisp files that are loaded using `load` and `require`, **also handles files that auto-compile misses**, such as packages that are deferred (e.g., `:defer t` in `use-package`) and the `use-package` dependencies using, for example,`:after package-name`.
- Compile-angel can exclude files from compilation using regular expressions in `compile-angel-excluded-files-regexps`.
- Compile-angel provides options to allow enabling and disabling specific functions that should be advised (load, require, etc.).
- Compile-angel allows enabling debug mode, which allows knowing exactly what compile-angel does. Additionally, compiled files and features are stored in variables that help identify what was compiled.
- compile-angel-on-save-mode supports compiling indirect buffers (clones).
- compile-angel-on-load-mode compiles features that have already been loaded to make sure that they are compiled.
- Compile-Angel uses caching to enhance performance when locating the .el file corresponding to a given feature.

## How to make compile-angel behave like auto-compile?

Execute the following setq **before** activating `compile-angel-on-load-mode`:

```emacs-lisp
(setq compile-angel-on-load-advise-load t)
(setq compile-angel-on-load-advise-require t)
(setq compile-angel-on-load-advise-eval-after-load nil)
(setq compile-angel-on-load-advise-autoload nil)
(setq compile-angel-on-load-compile-features nil)
```

This will make *compile-angel* behave similarly to *auto-compile*. It will only compile required and loaded packages. However, if you disable the 'compile features' mode, autoload, or eval-after-load features, certain packages may be missed, especially for heavy users of delaying package loading (e.g., via `:defer t`) and package dependencies (e.g., via `:after`), as well as those whose configuration frequently evaluate ELisp code after load.

## Author and License

The *compile-angel* Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2024 James Cherti

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.

## Links

- [compile-angel.el @GitHub](https://github.com/jamescherti/compile-angel.el)
- For users who prefer compiling .el files from the command line: [elispcomp](https://github.com/jamescherti/elispcomp)

Other Emacs packages by the same author:
- [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d): This repository hosts a minimal Emacs configuration designed to serve as a foundation for your vanilla Emacs setup and provide a solid base for an enhanced Emacs experience.
- [outline-indent.el](https://github.com/jamescherti/outline-indent.el): An Emacs package that provides a minor mode that enables code folding and outlining based on indentation levels for various indentation-based text files, such as YAML, Python, and other indented text files.
- [vim-tab-bar.el](https://github.com/jamescherti/vim-tab-bar.el): Make the Emacs tab-bar Look Like Vim’s Tab Bar.
- [easysession.el](https://github.com/jamescherti/easysession.el): Easysession is lightweight Emacs session manager that can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, the tab-bar, and the Emacs frames (with or without the Emacs frames size, width, and height).
- [elispcomp](https://github.com/jamescherti/elispcomp): A command line tool that allows compiling Elisp code directly from the terminal or from a shell script. It facilitates the generation of optimized .elc (byte-compiled) and .eln (native-compiled) files.
- [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el): The Tomorrow Night Deepblue Emacs theme is a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes. It features a deep blue background color that creates a calming atmosphere. The theme is also a great choice for those who miss the blue themes that were trendy a few years ago.
- [Ultyas](https://github.com/jamescherti/ultyas/): A command-line tool designed to simplify the process of converting code snippets from UltiSnips to YASnippet format.
- [dir-config.el](https://github.com/jamescherti/dir-config.el): Automatically find and evaluate .dir-config.el Elisp files to configure directory-specific settings.
- [flymake-bashate.el](https://github.com/jamescherti/flymake-bashate.el): A package that provides a Flymake backend for the bashate Bash script style checker.
- [flymake-ansible-lint.el](https://github.com/jamescherti/flymake-ansible-lint.el): An Emacs package that offers a Flymake backend for ansible-lint.
- [inhibit-mouse.el](https://github.com/jamescherti/inhibit-mouse.el): A package that disables mouse input in Emacs, offering a simpler and faster alternative to the disable-mouse package.
- [quick-sdcv.el](https://github.com/jamescherti/quick-sdcv.el): This package enables Emacs to function as an offline dictionary by using the sdcv command-line tool directly within Emacs.
