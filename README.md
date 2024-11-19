# compile-angel.el - Byte-compile and native-compile Emacs Lisp libraries Automatically
[![MELPA](https://melpa.org/packages/compile-angel-badge.svg)](https://melpa.org/#/compile-angel)
![Build Status](https://github.com/jamescherti/compile-angel.el/actions/workflows/ci.yml/badge.svg)
![License](https://img.shields.io/github/license/jamescherti/compile-angel.el)
![](https://raw.githubusercontent.com/jamescherti/compile-angel.el/main/.images/made-for-gnu-emacs.svg)

The **compile-angel** package automatically byte-compiles and native-compiles Emacs Lisp libraries. It offers:
- `(compile-angel-on-load-mode)`: Global mode that compiles .el files before they are loaded.
- `(compile-angel-on-save-local-mode)`: Local mode that compiles .el files whenever the user saves them.

These modes **speed up Emacs by ensuring all libraries are byte-compiled and native-compiled**. Byte-compilation reduces the overhead of loading Emacs Lisp code at runtime, while native compilation optimizes performance by generating machine code specific to your system.

The *compile-angel* author used to be an *auto-compile* user, but several of his .el files were not being compiled by *auto-compile*, which caused Emacs to become slow due to the lack of native compilation. The author experimented for an extended and the result of those hours of research and testing became a package called *compile-angel*.

**The compile-angel package guarantees that all .el files are both byte-compiled and native-compiled, which significantly speeds up Emacs.**

## Before installing

It is highly recommended to set the following variables in your init file:

``` emacs-lisp
;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)

;; Ensure JIT compilation is enabled for improved performance by
;; native-compiling loaded .elc files asynchronously
(setq native-comp-jit-compilation t)
(setq native-comp-deferred-compilation t) ; Deprecated in Emacs > 29.1
```

Additionally, ensure that native compilation is enabled:

This should return t: `(native-comp-available-p)`

## Installation

To install `compile-angel` from MELPA:

1. If you haven't already done so, [add MELPA repository to your Emacs configuration](https://melpa.org/#/getting-started).

2. Add the following code **at the very beginning of your Emacs init file, before all other packages**:
```emacs-lisp
(use-package compile-angel
  :ensure t
  :demand t
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

## What are some interesting Emacs customizations to consider alongside compile-angel?

Below are a few interesting options:

```elisp
;; Ensure that quitting only occurs once Emacs finishes native compiling,
;; preventing incomplete or leftover compilation files in `/tmp`.
(setq native-comp-async-query-on-exit t)
(setq confirm-kill-processes t)

;; Show buffer when there is a warning.
;; (NOT RECOMMENDED, except during development).
(setq warning-minimum-level :warning)
(setq byte-compile-verbose t)
(setq byte-compile-warnings t)
(setq native-comp-async-report-warnings-errors t)
(setq native-comp-warning-on-missing-source t)

;; Non-nil means to natively compile packages as part of their installation.
(setq package-native-compile t)
```

## Why not just use the package-recompile-all function?

The *package-recompile-all* function is effective for recompiling files within packages, but it misses other files that are not part of a package.

In the *compile-angel* author's configuration, for example, *package-recompile-all* skipped most of the local packages loaded using *use-package* with *:ensure nil* or *require*. Additionally, *package-recompile-all* does not compile transparently; the user must manually run it and wait for it to complete.

The *compile-angel* package, on the other hand, transparently compiles all packages without any user intervention. The user simply needs to enable *(compile-angel-on-load-mode)*.

## What is the difference between auto-compile and compile-angel?

Jonas Bernouli, the author of auto-compile, has made some design decisions that prevent it from guaranteeing that all .el packages are byte-compiled and native-compiled. For example, if the user deletes all the .elc files or if the .el files have never been compiled before, auto-compile won't recompile them. Here is a quote from u/tarsius_ (Jonas Bernouli), the auto-compile author ([from this discussion](https://www.reddit.com/r/emacs/comments/1gmmnhn/comment/lwhtte2/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button)):
> Both [autocompile] modes only ever re-compile a source file when the
> respective byte code file already exists but is outdated. Otherwise
> they do not compile the source file. By "otherwise" I mean if:
>   - The *.elc exists but is newer than the corresponding *.el, OR
>   - The *.elc does not exist.
> In both cases the source file is not compiled, by design.

Here are the main differences between compile-angel and auto-compile:
- Compile-angel ensures that even when when the .elc file doesn't exist, the .el source file is compiled. Auto-compile, on the other hand, requires (by design, as explained above) an existing .elc file in order to compile.
- **Compile-angel ensures more .el files are compiled**: The compile-angel package, in addition to compiling the .el files that are loaded using *load* and *require*, also handles files that auto-compile does not support, such as autoload and eval-after-load. They can be useful for compiling packages in advance that will be loaded in the future.
- Compile-angel can exclude files from compilation using regular expressions in *compile-angel-excluded-files-regexps*.
- Compile-angel provides options to allow enabling and disabling specific functions that should be advised (load, require, etc.).
- Compile-angel allows enabling debug mode, which allows knowing exactly what compile-angel does. Additionally, compiled files and features are stored in variables that help identify what was compiled.
- *compile-angel-on-save-mode* supports compiling indirect buffers (clones).
- *compile-angel-on-load-mode* compiles features that have already been loaded to make sure that they are compiled.
- Compile-Angel can use caching to enhance performance when locating the .el file corresponding to a given feature.
- *compile-angel-on-load-mode* only performs native compilation when JIT compilation is disabled. **Explanation**: *compile-angel-on-load-mode* performs native compilation only when JIT compilation is disabled. When JIT compilation is enabled, loading an *.elc* file causes Emacs to trigger native compilation automatically anyway. (This also loads makes Emacs load the natively-compiled files, replacing the auto-compiled functions with their native versions automatically and asynchronously.) In contrast, auto-compilation disables native compilation by default, which leads to native compilation being ignored in save mode as well. When native compilation is enabled in auto-compile, it native compiles files before load anyway, even though Emacs will native compile later on after loading the .elc file.

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
