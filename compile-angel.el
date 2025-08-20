;;; compile-angel.el --- Automatically Compile Elisp files (auto-compile alternative) -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.1.2
;; URL: https://github.com/jamescherti/compile-angel.el
;; Keywords: convenience
;; Package-Requires: ((emacs "26.3"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; The compile-angel package automatically byte-compiles and native-compiles
;; Emacs Lisp libraries. It offers:
;; - (compile-angel-on-load-mode): A global mode that compiles .el files before
;;   they are loaded.
;; - (compile-angel-on-save-local-mode): A local mode that compiles .el files
;;   whenever the user saves them.
;;
;; The compile-angel modes speed up Emacs by ensuring all libraries are
;; byte-compiled and native-compiled. Byte-compilation reduces the overhead of
;; loading Emacs Lisp code at runtime, while native compilation optimizes
;; performance by generating machine code specific to your system.
;;
;; The author of compile-angel was previously a user of auto-compile but
;; encountered an issue where several .el files were not being compiled by
;; auto-compile, resulting in Emacs performance degradation due to the lack of
;; native compilation. After extensive experimentation and research, the author
;; developed compile-angel to address this problem. The compile-angel package
;; guarantees that all .el files are both byte-compiled and native-compiled,
;; which significantly speeds up Emacs.
;;
;; The compile-angel package was created to offer an alternative to auto-compile
;; that guarantees all .el files are both byte-compiled and native-compiled,
;; which significantly speeds up Emacs.
;;
;; Before installing:
;; ------------------
;; It is highly recommended to set the following variables in your init file:
;;   (setq load-prefer-newer t)
;;   (setq native-comp-jit-compilation t)
;;   (setq native-comp-deferred-compilation t) ; Deprecated in Emacs > 29.1
;;
;; Additionally, ensure that native compilation is enabled; this should
;; return t: `(native-comp-available-p)`.
;;
;; Installation from MELPA:
;; ------------------------
;; (use-package compile-angel
;;   :ensure t
;;   :demand t
;;   :custom
;;   (compile-angel-verbose t)
;;
;;   :config
;;   ;; Set `compile-angel-verbose' to nil to disable compile-angel messages.
;;   ;; (When nil, compile-angel won't show which file is being compiled.)
;;   (setq compile-angel-verbose t)
;;
;;   ;; Uncomment the line below to auto compile when an .el file is saved
;;   ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)
;;
;;   ;; A global mode that compiles .el files before they are loaded
;;   ;; using `load' or `require'.
;;   (compile-angel-on-load-mode))
;;
;; Links:
;; ------
;; - More information about compile-angel (Frequently asked questions, usage...):
;;   https://github.com/jamescherti/compile-angel.el

;;; Code:

(require 'bytecomp)
(require 'cl-lib)
(eval-when-compile (require 'subr-x))

;;; Variables

(defgroup compile-angel nil
  "Compile Emacs Lisp libraries automatically."
  :group 'compile-angel
  :prefix "compile-angel-")

(defcustom compile-angel-enable-byte-compile t
  "Non-nil to enable byte compilation of Emacs Lisp (.el) files."
  :type 'boolean
  :group 'compile-angel)

(defcustom compile-angel-enable-native-compile t
  "Non-nil to enable native-compilation of Emacs Lisp (.el) files."
  :type 'boolean
  :group 'compile-angel)

(defcustom compile-angel-excluded-files '("loaddefs.el"
                                          "autoloads.el"
                                          "/lisp/org/org-version.el"
                                          "/lisp/cus-load.el"
                                          "/lisp/finder-inf.el"
                                          ;; /lisp and /site-lisp: subdirs.el
                                          "lisp/subdirs.el"
                                          ;; Built-in no-byte-compile packages
                                          "/lisp/leim/leim-list.el")
  "A list of path suffixes used to exclude specific .el files from compilation.

Example: \\='(\"suffix.el\" \"/filename.el\") This excludes any path that ends
with \"suffix.el\" (or its variations, such as \"/path/ANYTHINGsuffix.el.gz\")
and exactly matches paths that end with \"/filename.el\" (including their
variations, like \"/filename.el.gz\" or \"ANYTHING/filename.el.gz\").

If a path suffix in `compile-angel-excluded-files' ends with .el, compile-angel
will automatically exclude the .el.gz variant of that file as well (e.g.,
\"suffix.el\" will also exclude \"ANYTHINGsuffix.el.gz\").

The variable `load-file-rep-suffixes' is used by compile-angel to detect and
include all extensions associated with .el files."
  :type '(repeat string)
  :group 'compile-angel)

(defcustom compile-angel-excluded-files-regexps
  (delq nil (list "/lisp/international/.*\\.el"
                  (when (bound-and-true-p doom-user-dir)
                    "/doom-snippets/.*")))
  "A list of regular expressions to exclude certain .el files from compilation.

It is advisable to use `compile-angel-excluded-files' instead of
`compile-angel-excluded-files-regexps', as it simplifies matching file names.
Regular expressions may become unnecessarily complex in this context,
particularly since .el files might also end with the extension .el.gz on certain
configurations. Furthermore, Emacs regular expressions differ from PCRE, adding
another layer of potential complexity.

These regular expression apply to all modes:
- `compile-angel-on-load-mode'
- `compile-angel-on-save-local-mode'"
  :type '(repeat string)
  :group 'compile-angel)

(defcustom compile-angel-verbose nil
  "Enable displaying messages (e.g., when files are compiled).
When set to non-nil, this option will cause messages to be shown during the
compilation process, providing feedback on the compilation status."
  :type 'boolean
  :group 'compile-angel)

(defcustom compile-angel-debug nil
  "Non-nil to display debug messages in the *compile-angel:debug* buffer.
This displays a lot of messages."
  :type 'boolean
  :group 'compile-angel)

(defcustom compile-angel-byte-compile-report-issues nil
  "Controls whether byte-compilation warnings are reported.

When set to nil, `byte-compile-file' will disregard the value of
`byte-compile-warnings', preventing byte-compilation warnings from being shown
to the user.

When set to a non-nil value, this variable ensures that the value of
`byte-compile-warnings' is respected during byte-compilation and warnings are
displayed accordingly."
  :type 'boolean
  :group 'compile-angel)

(defcustom compile-angel-predicate-function nil
  "Function that determines if an .el file should be compiled.
It takes one argument (an .el file) and returns t if the file should be
compiled, or nil if the file should not be compiled.

This function applies to all modes:
- `compile-angel-on-load-mode'
- `compile-angel-on-save-local-mode'"
  :group 'compile-angel
  :type '(choice (const nil)
                 (function)))

(defcustom compile-angel-on-load-mode-compile-once t
  "If non-nil, enable single compilation for `compile-angel-on-load-mode'.
This setting ensures that `compile-angel-on-load-mode' performs byte and native
compilation of .el files only once during the initial loading. When set to nil,
the mode will verify if the .el file needs recompilation each time it is
loaded."
  :type 'boolean
  :group 'compile-angel)

;; Enable/Disable features
(defvar compile-angel-on-load-advise-load t
  "When non-nil, automatically compile .el files loaded using `load'.")

(defvar compile-angel-on-load-advise-require t
  "When non-nil, automatically compile .el files loaded using `require'.")

(defvar compile-angel-on-load-hook-after-load-functions t
  "Non-nil to compile missed .el during `after-load-functions'.")

(defvar compile-angel-on-load-compile-features t
  "Non-nil to compile features listed in the `features' variable.
When `compile-angel-on-load-mode' is activated, this ensures that all features
listed in the `features' variable are compiled.")

(defvar compile-angel-on-load-compile-load-history t
  "Non-nil to compile all uncompiled files in the load history.
This ensures that all files loaded before `compile-angel-on-load-mode' is
activated are compiled when this mode is activated.")

(defcustom compile-angel-on-save-check-parens t
  "Non-nil to check for unbalanced parentheses before compiling a file.
This applies only to `compile-angel-on-save-local-mode' and
`compile-angel-on-save-mode'. If unbalanced parentheses are detected, the file
is not compiled, as the compilation would fail anyway."
  :group 'compile-angel
  :type 'boolean)

(defcustom compile-angel-optimize-file-name-handlers 'safe
  "Control optimization of `file-name-handler-alist' during compilation.

The value determines how file name handlers are temporarily adjusted:

- \\='all   Disable all handlers (NOT RECOMMENDED).
- \\='safe  Disable all handlers except the compression handler
  (`.el.gz' files).
- \\='nil   Do not modify `file-name-handler-alist'.

This setting is intended to improve performance by reducing the overhead
associated with file-related operations during compilation."
  :type '(choice (const :tag "Disable all (NOT RECOMMENDED)" all)
                 (const :tag "Disable all except compression handler" safe)
                 (const :tag "No optimization" nil))
  :group 'compile-angel)

;;; Experimental features

(defvar compile-angel-guess-el-file-use-load-history nil)
(defvar compile-angel-use-file-index nil
  "EXPERIMENTAL: Enable a faster feature-to-file lookup.
When non-nil, construct a hash table mapping feature names to their file paths
by scanning all directories in `load-path' to improve lookup performance.")

;;; Internal variables

(defconst compile-angel--builtin-features
  '(tty-child-frames xwidget-internal move-toolbar dbusbind native-compile
                     font-render-setting system-font-setting
                     android inotify x xinput2 x-toolkit motif gtk cairo
                     gfilenotify haiku multi-tty make-network-process threads
                     w32notify pgtk w32 lcms2 kqueue emacs mps
                     hashtable-print-readable code-pages base64 md5 sha1 overlay
                     text-properties lisp-float-type dynamic-modules jansson
                     harfbuzz byte-compile :system noutline multi-isearch
                     dotassoc)
  "Features provided by Emacs core without associated Elisp files.
This includes features provided directly by C code as well as
features provided by core Elisp that don't have their own .el files.
These features are excluded from compilation attempts since they
have no source files to compile.")

(defvar compile-angel--builtin-features-table
  (let ((table (make-hash-table :test 'eq :size
                                (length compile-angel--builtin-features))))
    (dolist (feature compile-angel--builtin-features)
      (puthash feature t table))
    table)
  "Hash table of built-in features for fast lookups.
Contains features provided by Emacs core (both C and Elisp) that
don't have associated .el files and therefore don't need compilation.")

(defvar compile-angel--init-completed nil)
(defvar compile-angel--native-comp-available nil)

(defvar compile-angel--quiet-byte-compile t)

(defvar compile-angel--track-no-byte-compile-files t)
(defvar compile-angel--no-byte-compile-files-list (make-hash-table :test 'equal))

(defvar compile-angel--list-jit-native-compiled-files (make-hash-table :test 'equal))
(defvar compile-angel--native-compile-when-jit-enabled nil)

(defvar compile-angel--list-compiled-files (make-hash-table :test 'equal))
(defvar compile-angel--list-compiled-features (make-hash-table :test 'eq))
(defvar compile-angel--currently-compiling nil)
(defvar compile-angel--el-file-regexp nil)
(defvar compile-angel--el-file-extensions nil)
(defvar compile-angel--excluded-path-suffixes-regexps nil)
(defvar compile-angel--advice-before-require-ignore-using-featurep t)
(defvar compile-angel--doom-user-dir
  (when (bound-and-true-p doom-user-dir)
    (concat (directory-file-name (file-truename doom-user-dir)) "/")))
(defvar compile-angel--doom-emacs-lisp-dir
  (when (bound-and-true-p doom-emacs-dir)
    (concat (directory-file-name (file-truename
                                  (expand-file-name "lisp" doom-emacs-dir)))
            "/")))
(defvar compile-angel--doom-modules-dir
  (when (bound-and-true-p doom-modules-dir)
    (concat (directory-file-name (file-truename doom-modules-dir)) "/")))

;; EXPERIMENTAL (Disabled by default):
;; Speed up file lookups when `compile-angel-use-file-index' is non-nil.
(defvar compile-angel--file-index (make-hash-table :test 'eq))
(defvar compile-angel--file-index-hits 0)
(defvar compile-angel--file-index-misses 0)
(defvar compile-angel-track-file-index-stats nil
  "Non-nil to track statistics about file index cache hits and misses.
When enabled, compile-angel will count how many times the file index cache
was hit or missed. This information can be displayed using the
`compile-angel-display-file-index-stats' function.")

;;; Internal functions

(defvar compile-angel--file-name-handler-alist nil)

(defmacro compile-angel--with-fast-file-ops (&rest body)
  "Execute BODY with optimized file operations.
This disables file handlers temporarily for faster file operations."
  (declare (indent 0) (debug t))
  `(let ((file-name-handler-alist
          (cond
           ;; Disable all handlers (NOT RECOMMENDED).
           ((eq compile-angel-optimize-file-name-handlers 'all)
            nil)

           ;; Do not modify `file-name-handler-alist'
           ((eq compile-angel-optimize-file-name-handlers nil)
            file-name-handler-alist)

           ;; Safe: Disable all handlers except the compression handler
           ;; (`.el.gz' files).
           (t
            compile-angel--file-name-handler-alist)))
         (case-fold-search nil))
     ,@body))

(defun compile-angel--insert-message (buffer-name msg &rest args)
  "Insert formatted MSG with ARGS into BUFFER-NAME buffer."
  (let ((inhibit-read-only t))
    (with-current-buffer (get-buffer-create buffer-name)
      (setq-local buffer-read-only t)
      (save-excursion
        (goto-char (point-max))
        (insert (apply 'format msg args) "\n")))))

(defmacro compile-angel--debug-message (&rest args)
  "Display a debug message with the same ARGS arguments as `message'.
The messages are displayed in the *compile-angel* buffer."
  `(when compile-angel-debug
     (compile-angel--insert-message "*compile-angel:debug*"
                                    ,(car args) ,@(cdr args))))

(defmacro compile-angel--verbose-message (&rest args)
  "Display a verbose message with the same ARGS arguments as `message'."
  `(progn
     (when compile-angel-debug
       (compile-angel--debug-message ,(car args) ,@(cdr args)))
     (when compile-angel-verbose
       (message (concat "[compile-angel] " ,(car args)) ,@(cdr args)))))

(defvar enable-local-variables)
(defvar local-enable-local-variables)
(defvar enable-dir-local-variables)
(defvar enable-remote-dir-locals)
(defvar enable-local-eval)
(defvar permanently-enabled-local-variables)
(defvar ignored-local-variable-values)
(defun compile-angel--no-byte-compile-p (file-path)
  "Return the value of the `no-byte-compile' local variable in FILE-PATH.
This function inspects FILE-PATH for a file-local variable declaration
of the form `no-byte-compile: t` and returns its value, or nil if the
declaration is absent or not trusted under safe-local-variable rules."
  (with-temp-buffer
    (insert-file-contents file-path)
    (let (;; Tell Emacs to parse and apply only *safe* file-local variables.
          ;; This avoids running arbitrary code.
          (enable-local-variables :safe)

          ;; Explicitly allow file-local variables to be processed in this
          ;; buffer.
          (local-enable-local-variables t)

          ;; Disable any influence from .dir-locals.el files in the directory.
          ;; Ensures only variables from FILE-PATH are considered.
          (enable-dir-local-variables nil)

          ;; Disable remote directory-local variables via TRAMP or similar.
          ;; Prevents remote configuration from influencing results.
          (enable-remote-dir-locals nil)

          ;; Explicitly disallow evaluation of eval: forms in local variables.
          ;; Prevents execution of arbitrary code when inspecting the file.
          (enable-local-eval nil)

          ;; Ensure `no-byte-compile' is recognized even under restricted
          ;; conditions.
          (permanently-enabled-local-variables '(no-byte-compile))

          ;; Prevent Emacs from silently skipping unsafe or blacklisted values.
          ;; This ensures accurate reading of declared variables.
          (ignored-local-variable-values nil))
      ;; Read and apply local variables under the restricted settings above.
      (hack-local-variables)

      ;; Return the parsed value of `no-byte-compile', or nil if not present.
      (alist-get 'no-byte-compile file-local-variables-alist))))

(defun compile-angel--el-file-excluded-p (el-file)
  "Check if EL-FILE matches any regex in `compile-angel-excluded-files-regexps'.
Return non-nil if the file should be ignored, nil otherwise."
  (when (or (and compile-angel--excluded-path-suffixes-regexps
                 (cl-some (lambda (regex)
                            (string-match-p regex el-file))
                          compile-angel--excluded-path-suffixes-regexps))
            (and compile-angel-excluded-files-regexps
                 (cl-some (lambda (regex)
                            (string-match-p regex el-file))
                          compile-angel-excluded-files-regexps)))
    (compile-angel--debug-message
     "SKIP (.el file excluded with a regex): %s" el-file)
    t))

(defun compile-angel--elisp-native-compiled-p (el-file)
  "Return non-nil if EL-FILE is native-compiled and up to date.
Return nil if it is not native-compiled or if its .eln file is out of date."
  (let ((eln-file (and (fboundp 'comp-el-to-eln-filename)
                       (funcall 'comp-el-to-eln-filename el-file))))
    (when (and eln-file
               (file-newer-than-file-p eln-file el-file))
      t)))

(defun compile-angel--native-compile (el-file)
  "Native-compile EL-FILE."
  (compile-angel--with-fast-file-ops
    (cond
     ((not compile-angel--native-comp-available)
      (compile-angel--debug-message
       "Native-compilation ignored (native-comp unavailable): %s" el-file))

     ((compile-angel--elisp-native-compiled-p el-file)
      (compile-angel--debug-message
       "Native-compilation ignored (up-to-date): %s" el-file))

     (t
      (let ((el-file-abbreviated (abbreviate-file-name el-file)))
        (compile-angel--verbose-message
         "Async native-compilation: %s" el-file-abbreviated))
      (let ((inhibit-message (not (or (not compile-angel-verbose)
                                      (not compile-angel-debug)))))
        (when (fboundp 'native-compile-async)
          (funcall 'native-compile-async el-file)))))))

(defun compile-angel--byte-compile (el-file elc-file)
  "Byte-compile EL-FILE into ELC-FILE.
Return non-nil to allow native compilation."
  (cond
   ((not (file-newer-than-file-p el-file elc-file))
    (compile-angel--debug-message
     "Byte-compilation ignored (up-to-date): %s" el-file)
    t)

   (t
    (let* ((byte-compile-warnings
            (when compile-angel-byte-compile-report-issues
              byte-compile-warnings))
           (after-change-major-mode-hook
            (and (fboundp 'global-font-lock-mode-enable-in-buffer)
                 (list 'global-font-lock-mode-enable-in-buffer)))
           (inhibit-message (not (or (not compile-angel-verbose)
                                     (not compile-angel-debug))))
           (el-file-abbreviated (abbreviate-file-name el-file))
           (prog-mode-hook nil)
           (emacs-lisp-mode-hook nil)
           (byte-compile-result
            (let ((original-message (symbol-function 'message)))
              (cl-letf (((symbol-function #'message)
                         #'(lambda (format-string &rest messages-args)
                             (let ((combined-args (cons format-string
                                                        messages-args)))
                               (when (or (not compile-angel--quiet-byte-compile)
                                         (and compile-angel--quiet-byte-compile
                                              (and (not (string-prefix-p
                                                         "Wrote"
                                                         format-string))
                                                   (not (string-prefix-p
                                                         "Compiling "
                                                         format-string)))))
                                 ;; Show the message
                                 (apply original-message combined-args))))))
                (condition-case err
                    (let ((noninteractive t))
                      (byte-compile-file el-file))
                  (permission-denied
                   (progn
                     (compile-angel--debug-message
                      "IGNORED: Permission denied: %s"
                      (error-message-string err))
                     ;; Try to native compile
                     'compile-angel-ignore)))))))
      (cond
       ((eq byte-compile-result 'compile-angel-ignore)
        nil)

       ((eq byte-compile-result 'no-byte-compile)
        (puthash el-file t compile-angel--no-byte-compile-files-list)
        (compile-angel--debug-message
         "Byte-compilation Ignore (no-byte-compile): %s" el-file-abbreviated)
        nil)

       ((not byte-compile-result)
        (compile-angel--verbose-message "Byte-compilation error: %s"
                                        el-file-abbreviated)
        nil)

       (byte-compile-result
        (compile-angel--verbose-message
         "Byte-compilation successful: %s" el-file-abbreviated)
        t))))))

(defun compile-angel--need-compilation-p (el-file feature)
  "Return non-nil if EL-FILE or FEATURE need compilation.
EL-FILE is a String representing the path to the Elisp source file.
FEATURE is a symbol representing the feature being loaded."
  (cond
   ((not el-file)
    (compile-angel--debug-message
     "SKIP (el-file is nil): %s | %s" el-file feature)
    nil)

   ((not (compile-angel--is-el-file el-file))
    (compile-angel--debug-message
     "SKIP (Does not end with the .el): %s | %s" el-file feature)
    nil)

   (t
    (let ((decision (if compile-angel-predicate-function
                        (funcall compile-angel-predicate-function el-file)
                      t)))
      (when (and (not (eq decision t))  ; t = :continue
                 (not (eq decision nil))  ; nil = :ignore

                 ;; `:compile' have precedence over excluded files
                 ;; (`compile-angel-excluded-files' and
                 ;; `compile-angel-excluded-files-regexps')
                 (not (eq decision :compile))

                 ;; `:continue' is the same as nil
                 (not (eq decision :continue))

                 ;; Do not use `:force-compile'. Use `:compile' instead.
                 (not (eq decision :force-compile))

                 ;; Do not compile the file
                 (not (eq decision :ignore)))
        (message (format
                  (concat "[compile-angel] WARNING: The predicate function "
                          "`compile-angel-predicate-function' returned an "
                          "invalid value: %s")
                  decision))
        (setq decision t))

      (cond
       ((eq decision :force-compile)
        t)

       ;; This is specific to Doom Emacs: It ensures that the Doom Emacs user
       ;; directory, Emacs directory, and modules directory are not compiled by
       ;; compile-angel. This is important because `.el` files in these
       ;; directories should never be compiled, or Doom may fail to load some of
       ;; them correctly.
       ((and compile-angel--doom-user-dir
             (let ((el-file-truename (file-truename el-file)))
               (or (and compile-angel--doom-user-dir
                        (string-prefix-p compile-angel--doom-user-dir
                                         el-file-truename))
                   (and compile-angel--doom-emacs-lisp-dir
                        (string-prefix-p compile-angel--doom-emacs-lisp-dir
                                         el-file-truename))
                   (and compile-angel--doom-modules-dir
                        (string-prefix-p compile-angel--doom-modules-dir
                                         el-file-truename)))))
        (compile-angel--debug-message
         "SKIP (Doom Emacs modules/emacs/user directory): %s | %s"
         el-file feature)
        nil)

       ((eq decision :compile)
        t)

       ((eq decision :ignore)
        (compile-angel--debug-message
         "SKIP (Predicate function returned :ignore): %s | %s"
         el-file feature)
        nil)

       ((not decision)  ; nil = :ignore
        (compile-angel--debug-message
         "SKIP (Predicate function returned nil): %s | %s" el-file feature)
        nil)

       ;; :continue starts here:
       ((compile-angel--el-file-excluded-p el-file)
        nil)

       (t t))))))

(defun compile-angel--compile-elisp (el-file &optional noerror)
  "Byte-compile and Native-compile the .el file EL-FILE.
When NOERROR is non-nil, suppress warnings if the file is absent."
  (compile-angel--with-fast-file-ops
    (let* ((elc-file (byte-compile-dest-file el-file))
           (do-native-compile nil)
           (compile-angel--native-compile-when-jit-enabled
            compile-angel--native-compile-when-jit-enabled))
      (cond
       ((not elc-file)
        (unless noerror
          (message "[compile-angel] Warning: The file is not an .el file: %s"
                   el-file)))

       ((not (file-exists-p el-file))
        (unless noerror
          (message "[compile-angel] Warning: The file does not exist: %s" el-file)))

       (t
        (cond
         ;; Byte-compile Disabled
         ((not compile-angel-enable-byte-compile)
          (when compile-angel-enable-native-compile
            ;; Disable native compilation when no-byte-compile is set to t
            (if (compile-angel--no-byte-compile-p el-file)
                (progn
                  (setq do-native-compile nil)
                  (puthash el-file t compile-angel--no-byte-compile-files-list)
                  (compile-angel--debug-message
                   "Native-compilation ignored (no-byte-compile): %s"
                   el-file))
              ;; Native compile
              (setq do-native-compile t)

              ;; Ensure the files are native compiled, as JIT compilation depends
              ;; on the presence of .elc files.
              (setq compile-angel--native-compile-when-jit-enabled t))))

         ;; Byte-compile Enabled
         ((file-writable-p elc-file)
          ;; Byte-compile
          (setq do-native-compile (compile-angel--byte-compile
                                   el-file elc-file)))

         ;; .elc not writable
         (t
          ;; Do not byte-compile
          (compile-angel--debug-message
           "Byte-compilation ignored (not writable)%s: %s"
           (if compile-angel-enable-native-compile
               ". Native-compilation only"
             "")
           elc-file)
          (setq do-native-compile t)))

        (let ((jit-enabled (or (bound-and-true-p native-comp-jit-compilation)
                               (bound-and-true-p native-comp-deferred-compilation))))
          (when (and jit-enabled
                     (not compile-angel--native-compile-when-jit-enabled))
            ;; Do not native-compile. Let the JIT compiler do it.
            (setq do-native-compile nil)
            ;; The `compile-angel--list-jit-native-compiled-files' hash
            ;; table serves as a safeguard to verify that the JIT compiler
            ;; has not overlooked any files.
            (puthash el-file t compile-angel--list-jit-native-compiled-files)
            (compile-angel--debug-message
             "Native-compilation ignored (JIT compilation will do it): %s"
             el-file)))

        (when (and compile-angel-enable-native-compile
                   do-native-compile)
          (compile-angel--native-compile el-file)))))))

(defun compile-angel--check-parens ()
  "Check for unbalanced parentheses in the current buffer.

This function scans the entire buffer for balanced parentheses. If an imbalance
is found, it raises a user error with details about the position of the
unmatched parenthesis or quote, including the line number, column number, and
the file name.

If the parentheses are balanced, it returns t. If unbalanced parentheses are
detected, it raises an error and returns nil."
  (condition-case data
      (progn
        (scan-sexps (point-min) (point-max))
        ;; Return t
        t)
    (scan-error
     (let ((char (nth 2 data)))
       (compile-angel--debug-message
        "SKIP compile-angel--compile-on-save (Unmatched parenthesis): %s"
        (buffer-file-name (buffer-base-buffer)))
       (user-error
        (concat "[compile-angel] Compilation aborted: Unmatched bracket or "
                "quote in line %s, column %s in %s")
        (line-number-at-pos char)
        (let ((column (save-excursion (goto-char char) (current-column))))
          (when (integerp column)
            (1+ column)))
        (let ((file-name (buffer-file-name (buffer-base-buffer))))
          (when file-name
            (abbreviate-file-name file-name))))
       ;; Return nil
       nil))))

(defun compile-angel--compile-on-save ()
  "Compile the current buffer."
  (when (derived-mode-p 'emacs-lisp-mode)
    (let ((el-file (buffer-file-name (buffer-base-buffer)))
          ;; Ensure that compile-angel performs the native compilation itself,
          ;; rather than waiting for Emacs to do it.
          (compile-angel--native-compile-when-jit-enabled t)
          (compile-angel-on-load-mode-compile-once nil))
      (when (or (not compile-angel-on-save-check-parens)
                (compile-angel--check-parens))
        (compile-angel--entry-point el-file)))))

(defun compile-angel--normalize-feature (feature)
  "Normalize FEATURE to a symbol when possible.
If FEATURE is a string, try to intern it. If it's already a symbol, return it.
For other types, return nil and log a debug message."
  (cond
   ((symbolp feature)
    feature)

   ((stringp feature)
    (intern feature))

   (t
    (compile-angel--debug-message
     "ISSUE: UNSUPPORTED Feature: Not a symbol or string: %s (type: %s)"
     feature (type-of feature))
    nil)))

(defun compile-angel--build-file-index ()
  "Build an index of all Elisp files in `load-path'.
This creates a mapping from feature symbols to their file paths."
  (compile-angel--debug-message "Building Elisp file index from load-path...")
  (compile-angel--with-fast-file-ops
    (clrhash compile-angel--file-index)
    (setq compile-angel--file-index-hits 0
          compile-angel--file-index-misses 0)

    ;; Pre-compute the combined pattern once
    (let* ((combined-pattern (concat "\\`[^.].*\\("
                                     (mapconcat #'regexp-quote
                                                compile-angel--el-file-extensions
                                                "\\|")
                                     "\\)\\'"))
           ;; Filter load-path once
           (filtered-load-path (cl-remove-if-not #'file-directory-p load-path)))

      ;; Process each directory in load-path
      (dolist (dir filtered-load-path)
        (dolist (file (directory-files dir t combined-pattern t))
          (when (file-regular-p file)
            ;; Extract the feature symbol from the filename - intern directly
            (let ((feature-symbol (intern (file-name-base file))))
              ;; Store in the index, with the first occurrence taking precedence
              (unless (gethash feature-symbol compile-angel--file-index)
                (puthash feature-symbol file compile-angel--file-index)))))))

    ;; `archive-mode' is a special case.
    (puthash 'archive-mode (gethash 'arc-mode compile-angel--file-index)
             compile-angel--file-index)

    ;; Special handling for evil-collection package
    (let ((evil-collection-file (gethash 'evil-collection
                                         compile-angel--file-index)))
      (when evil-collection-file
        (let ((evil-collection-modes-dir
               (expand-file-name "modes" (file-name-directory
                                          evil-collection-file))))
          (when (file-directory-p evil-collection-modes-dir)
            ;; Process all mode directories in a single pass
            (dolist (file (directory-files
                           evil-collection-modes-dir t
                           directory-files-no-dot-files-regexp t))
              (when (file-directory-p file)
                (let* ((mode-name (file-name-nondirectory file))
                       ;; Create symbol directly without intermediate string
                       (feature-symbol (intern (format "evil-collection-%s"
                                                       mode-name)))
                       (expected-file (format "%s/evil-collection-%s.el"
                                              file mode-name)))
                  (puthash feature-symbol expected-file
                           compile-angel--file-index)))))))))

  (compile-angel--debug-message
   "Elisp file index built with %d entries"
   (hash-table-count compile-angel--file-index)))

(defun compile-angel-display-file-index-stats ()
  "Display statistics about the file index cache hits and misses.
This shows how effective the file index optimization has been."
  (let* ((total (+ compile-angel--file-index-hits
                   compile-angel--file-index-misses))
         (hit-percentage (if (> total 0)
                             (* 100.0 (/ (float compile-angel--file-index-hits)
                                         total))
                           0.0))
         (miss-percentage (if (> total 0)
                              (* 100.0
                                 (/ (float compile-angel--file-index-misses)
                                    total))
                            0.0)))
    (message
     "File index stats: %d hits (%.2f%%), %d misses (%.2f%%), %d total lookups"
     compile-angel--file-index-hits hit-percentage
     compile-angel--file-index-misses miss-percentage
     total)))

(defun compile-angel--feature-el-file-from-load-history (feature-name)
  "Return the source file for FEATURE-NAME if it is loaded.
Uses `load-history' to determine the file where the feature was loaded from.
Returns nil for features provided directly by C code."
  (when feature-name
    (let* ((history-regexp (load-history-regexp feature-name))
           (history-file (and (stringp history-regexp)
                              (load-history-filename-element history-regexp))))
      (and (listp history-file)
           (compile-angel--normalize-el-file (car history-file))))))

(defun compile-angel--locate-feature-file (feature-or-file nosuffix)
  "Locate a file for FEATURE-OR-FILE using `locate-file'.
If NOSUFFIX is non-nil, use `load-file-rep-suffixes' instead of
`compile-angel--el-file-extensions'."
  (when feature-or-file
    (compile-angel--with-fast-file-ops
      (locate-file feature-or-file
                   load-path
                   (if nosuffix
                       load-file-rep-suffixes
                     compile-angel--el-file-extensions)))))

(defun compile-angel--guess-el-file-using-file-index (feature-symbol nosuffix)
  "Locate the .el file using the file index.
FEATURE-SYMBOL is the feature (string) and NOSUFFIX is the same NOSUFFIX
argument as `load'."
  (let* ((feature-name (symbol-name feature-symbol))
         (cached-result (and feature-symbol
                             (gethash feature-symbol
                                      compile-angel--file-index))))
    (cond
     (cached-result
      ;; Cache hit
      (when compile-angel-track-file-index-stats
        (cl-incf compile-angel--file-index-hits)
        (compile-angel--debug-message
         "File index cache HIT for feature: %s" feature-symbol))
      cached-result)

     ;; Try `load-history' if feature is loaded
     ((and feature-symbol (featurep feature-symbol))
      (when compile-angel-track-file-index-stats
        (cl-incf compile-angel--file-index-misses)
        (compile-angel--debug-message
         "File index cache MISS for feature: %s" feature-symbol))

      ;; Store feature-symbol once to avoid repeated symbol-name calls
      (let* ((history-regexp (load-history-regexp feature-name))
             (history-file (and (listp history-regexp)
                                (car (load-history-filename-element
                                      history-regexp)))))
        (when (stringp history-file)
          (compile-angel--normalize-el-file history-file))))

     ;; Cache miss and feature not loaded
     (t
      (when compile-angel-track-file-index-stats
        (cl-incf compile-angel--file-index-misses)
        (compile-angel--debug-message
         "File index cache MISS for feature: %s" feature-symbol))

      ;; Avoid unnecessary symbol->string conversion
      (compile-angel--locate-feature-file feature-name nosuffix)))))

(defun compile-angel--guess-el-file (el-file
                                     &optional feature-symbol nosuffix)
  "Guess the path of the EL-FILE or FEATURE.

EL-FILE must be an absolute path. If EL-FILE is not provided,
FEATURE-SYMBOL is used to search. FEATURE-SYMBOL has to be a symbol.

NOSUFFIX behaves similarly to `load', controlling whether file suffixes are
considered. Checks caches before performing any computation. Returns the
resolved file path or nil if not found."
  (let ((feature-name (when feature-symbol
                        (symbol-name feature-symbol))))
    ;; Fast path: if we have an absolute path that's an el file, just return it
    ;; El File
    (cond
     ((and el-file
           (stringp el-file)
           (file-name-absolute-p el-file)
           (compile-angel--is-el-file el-file))
      (compile-angel--debug-message
       "compile-angel--guess-el-file: File: %s" el-file)
      el-file)

     ;; Skip built-in features
     ((and feature-symbol
           (gethash feature-symbol compile-angel--builtin-features-table nil))
      (compile-angel--debug-message
       "compile-angel--guess-el-file: SKIP (Built-in feature): %s"
       feature-symbol)
      nil)

     ;; Experimental feature
     ((when-let* ((file-index-result
                   (and compile-angel-use-file-index
                        feature-symbol
                        (compile-angel--guess-el-file-using-file-index
                         feature-symbol
                         nosuffix))))
        (compile-angel--debug-message
         "compile-angel--guess-el-file: CACHE: %s" file-index-result)
        file-index-result))

     ;; Experimental feature
     ;; Try load-history if feature is loaded
     ((when-let* ((el-file-from-history
                   (and compile-angel-guess-el-file-use-load-history
                        feature-name
                        (compile-angel--feature-el-file-from-load-history
                         feature-name))))
        (compile-angel--debug-message
         "compile-angel--guess-el-file: HIST: %s" el-file-from-history)
        el-file-from-history))

     ;; Locate feature or file
     ((when-let* ((feature-file (compile-angel--locate-feature-file
                                 (or el-file feature-name) nosuffix)))

        (compile-angel--debug-message
         "compile-angel--guess-el-file: %s: %s"
         (if el-file "FILE" "FEATURE") (or el-file feature-name))
        feature-file)))))

(defun compile-angel--entry-point (el-file &optional feature nosuffix noerror)
  "This function is called by all the :before advices.
EL-FILE, FEATURE, NOERROR, and NOSUFFIX are the same arguments as
`load' and `require'."
  (compile-angel--with-fast-file-ops
    (when (or compile-angel-enable-byte-compile
              compile-angel-enable-native-compile)
      (let* ((feature-symbol (compile-angel--normalize-feature feature))
             (el-file (compile-angel--guess-el-file
                       el-file feature-symbol nosuffix)))
        (cond
         ((not el-file)
          (compile-angel--debug-message
           "SKIP (Returned a nil .el file): %s | %s" el-file feature))

         ((member el-file compile-angel--currently-compiling)
          (compile-angel--debug-message
           "SKIP (To prevent recursive compilation): %s | %s" el-file feature))

         ((and compile-angel-on-load-mode-compile-once
               (or (gethash el-file compile-angel--list-compiled-files)
                   (when feature-symbol
                     (gethash feature-symbol
                              compile-angel--list-compiled-features))))
          (compile-angel--debug-message
           "SKIP (In the skip hash list): %s | %s" el-file feature)
          nil)

         ((not (compile-angel--need-compilation-p el-file feature-symbol))
          (compile-angel--debug-message
           "SKIP (Does not need compilation): %s | %s" el-file feature))

         ((and compile-angel--track-no-byte-compile-files
               (gethash el-file compile-angel--no-byte-compile-files-list))
          (compile-angel--debug-message
           "Compilation ignored (in the no-byte-compile list): %s" el-file)
          t)

         (t
          (compile-angel--debug-message "COMPILATION ARGS: %s | %s"
                                        el-file feature-symbol)
          (puthash el-file t compile-angel--list-compiled-files)
          (when feature-symbol
            (puthash feature-symbol t compile-angel--list-compiled-features))
          (let ((compile-angel--currently-compiling
                 (cons el-file compile-angel--currently-compiling)))
            (compile-angel--compile-elisp el-file noerror))))))))

(defun compile-angel--advice-before-require (feature
                                             &optional filename noerror)
  "Recompile the library before `require'.
FEATURE, FILENAME, and NOERROR are the same arguments as the `require'
function."
  ;; Avoid repeated calls to featurep by storing the result
  (compile-angel--debug-message "\n[TASK] REQUIRE: %s (%s) | %s (%s)"
                                filename (type-of filename)
                                feature (type-of feature))
  ;; TODO: All other advices should do this
  ;; (compile-angel--entry-point filename feature nil noerror)
  (setq feature (compile-angel--normalize-feature feature))
  (if (and feature
           (or
            ;; The features were not compiled when `compile-angel' started
            (not compile-angel-on-load-compile-features)
            ;; Compile more than once
            (not compile-angel-on-load-mode-compile-once)
            ;; Ignore featurep
            (not compile-angel--advice-before-require-ignore-using-featurep)
            ;; The feature hasn't already been loaded
            (not (featurep feature))))
      ;; TODO err noerror
      (compile-angel--entry-point filename feature nil noerror)
    (compile-angel--debug-message "SKIP (Feature already loaded): %s | %s"
                                  feature filename)))

(defun compile-angel--advice-before-load (el-file &optional noerror _nomessage
                                                  nosuffix _must-suffix)
  "Recompile before `load'.
EL-FILE, NOERROR, and NOSUFFIX are the same args as `load'."
  (compile-angel--debug-message "\n[TASK] LOAD: %s (%s)" el-file
                                (type-of el-file))
  (if (stringp el-file)
      ;; Unset the special init-file status to prevent recursive loads
      (let ((user-init-file (if (eq user-init-file t)
                                nil
                              user-init-file)))
        (compile-angel--entry-point (when el-file
                                      (expand-file-name
                                       (substitute-in-file-name el-file)))
                                    nil  ; Feature
                                    nosuffix
                                    noerror))
    (compile-angel--debug-message
     (concat "ISSUE: Wrong type passed to "
             "compile-angel--advice-before-require %s (%s)")
     el-file (type-of el-file))))

(defun compile-angel--compile-load-history ()
  "Compile `load-history', which tracks all previously loaded files."
  (compile-angel--with-fast-file-ops
    (dolist (entry load-history)
      (let ((fname (car entry)))
        (when (compile-angel--is-el-file fname)
          (progn
            (compile-angel--debug-message
             "\n[TASK] compile-angel--compile-load-history: %s" fname)
            (compile-angel--entry-point fname)))))))

(defun compile-angel--compile-loaded-features ()
  "Compile all loaded features that are in the `features' variable."
  (compile-angel--with-fast-file-ops
    (dolist (feature features)
      (compile-angel--debug-message
       "\n[TASK] compile-angel--compile-loaded-features: %s" feature)
      (let ((compile-angel--advice-before-require-ignore-using-featurep nil))
        (compile-angel--entry-point nil feature)))))

(defun compile-angel--normalize-el-file (file)
  "Find the .el file corresponding to FILE.

If FILE is already a .el file, return it. If FILE is a .elc file, check for the
corresponding .el file by removing the .elc extension and verifying its
existence.

The function iterates through the extensions in `load-file-rep-suffixes` to
construct possible .el file paths. If a matching file exists, return its path;
otherwise, return nil."
  (compile-angel--with-fast-file-ops
    (cond
     ((not file)
      (compile-angel--debug-message
       "compile-angel--normalize-el-file: nil file")
      nil)

     ((compile-angel--is-el-file file)
      file)

     ((string-suffix-p ".elc" file)
      (compile-angel--debug-message
       "compile-angel--normalize-el-file: elc file: %s" file)
      (let* ((base (file-name-sans-extension file)))
        (cl-some (lambda (suffix)
                   (let ((candidate (concat base ".el" suffix)))
                     (and (file-exists-p candidate) candidate)))
                 load-file-rep-suffixes)))

     (t
      (compile-angel--debug-message
       "IGNORED: compile-angel--normalize-el-file: Not an .el or .elc: %s"
       file)))))

(defun compile-angel--hook-after-load-functions (file)
  "Compile FILE after load."
  (compile-angel--with-fast-file-ops
    (compile-angel--debug-message
     "\n[TASK] compile-angel--hook-after-load-functions: %s"
     file)
    (let ((file (compile-angel--normalize-el-file file))
          (compile-angel-on-load-mode-compile-once t))
      (when file
        (compile-angel--entry-point file)))))

(defun compile-angel--update-el-file-regexp (symbol new-value
                                                    _operation _where)
  "Update the `compile-angel--el-file-regexp' variable.
SYMBOL is the symbol.
NEW-VALUE is the value of the variable."
  (when (eq symbol 'load-file-rep-suffixes)
    (compile-angel--debug-message
     "WATCHER: Update compile-angel--el-file-regexp: %s" new-value)
    (setq compile-angel--el-file-extensions
          (mapcar (lambda (ext) (concat ".el" ext)) load-file-rep-suffixes))
    (setq compile-angel--el-file-regexp
          (format "\\.el%s\\'" (regexp-opt new-value))))

  (when (eq symbol 'compile-angel-excluded-files)
    (compile-angel--debug-message
     "WATCHER: Update compile-angel-excluded-files: %s"
     compile-angel-excluded-files)
    (let ((path-suffixes-regexp nil))
      ;; Process `compile-angel-excluded-files' to generate regular
      ;; expressions.
      ;; For each suffix:
      ;; - If it ends with `.el`, remove the `.el` and concatenate it with
      ;;   `compile-angel--el-file-regexp`, then add \\' at the end.
      ;; - Otherwise, convert it into a regular expression and add \\' at the
      ;;   end.
      (dolist (suffix new-value)
        (when (and suffix (not (string= suffix "")))
          (let* ((el-suffix-p (string-suffix-p ".el" suffix))
                 (suffix-without-el (if el-suffix-p
                                        (string-remove-suffix ".el" suffix)
                                      suffix))
                 (el-file-regexp (if (and el-suffix-p
                                          compile-angel--el-file-regexp)
                                     compile-angel--el-file-regexp)))
            (push (concat (regexp-quote suffix-without-el)
                          el-file-regexp
                          (unless (string-prefix-p "\\'" el-file-regexp)
                            "\\'" ))
                  path-suffixes-regexp))))
      (setq compile-angel--excluded-path-suffixes-regexps
            (nreverse path-suffixes-regexp)))))

(defun compile-angel--update-file-name-handler-alist (&rest _)
  "Update the `file-name-handler-alist' variable."
  (setq compile-angel--file-name-handler-alist
        (list (rassq 'jka-compr-handler
                     file-name-handler-alist))))

(defun compile-angel--init ()
  "Initialize internal variables."
  (unless compile-angel--init-completed
    (setq compile-angel--native-comp-available
          (and (featurep 'native-compile)
               (fboundp 'native-comp-available-p)
               (fboundp 'native-compile-async)
               (native-comp-available-p)))

    ;; load-file-rep-suffixes
    (compile-angel--update-el-file-regexp 'load-file-rep-suffixes
                                          load-file-rep-suffixes
                                          nil nil)
    (add-variable-watcher 'load-file-rep-suffixes
                          #'compile-angel--update-el-file-regexp)

    ;; compile-angel--update-el-file-regexp
    (compile-angel--update-el-file-regexp 'compile-angel-excluded-files
                                          compile-angel-excluded-files
                                          nil nil)
    (add-variable-watcher 'compile-angel-excluded-files
                          #'compile-angel--update-el-file-regexp)

    ;; Build the file index if enabled
    (when compile-angel-use-file-index
      (compile-angel--build-file-index))

    ;; Minimal `file-name-handler-alist'
    (compile-angel--update-file-name-handler-alist)

    (setq compile-angel--init-completed t)))

(defun compile-angel--is-el-file (file)
  "Return non-nil if FILE is an el-file."
  (and file
       compile-angel--el-file-regexp
       ;; A variable watcher (the `compile-angel--update-el-file-regexp`
       ;; function) dynamically updates `compile-angel--el-file-regexp` whenever
       ;; `load-file-rep-suffixes` is modified.
       (string-match-p compile-angel--el-file-regexp file)))

(defun compile-angel--ensure-jit-compile ()
  "When JIT is enabled, ensure that Emacs native-compiles the loaded .elc files.
Occasionally, Emacs fails to `native-compile' certain `.elc` files that should
be JIT compiled."
  (when (and compile-angel-enable-native-compile
             (> (hash-table-count compile-angel--list-jit-native-compiled-files)
                0))
    (unwind-protect
        (maphash (lambda (el-file _value)
                   (compile-angel--debug-message
                    "Checking if Emacs really JIT Native-Compiled: %s" el-file)
                   (compile-angel--native-compile el-file))
                 compile-angel--list-jit-native-compiled-files)
      (clrhash compile-angel--list-jit-native-compiled-files))))

(defvar compile-angel--report-native-compiled-features
  (make-hash-table :test 'equal))

(defun compile-angel--get-list-non-native-compiled ()
  "Return a list of the non natively-compiled features symbols."
  (let ((result nil))
    (dolist (feature features)
      (when feature
        (let* ((feature-symbol (compile-angel--normalize-feature feature)))
          (when (and feature-symbol
                     (not (gethash feature-symbol
                                   compile-angel--report-native-compiled-features)))
            (let* ((feature-el-file (when feature-symbol
                                      (locate-library (symbol-name feature-symbol))))
                   (el-file (when feature-el-file
                              (compile-angel--normalize-el-file
                               feature-el-file))))
              (when (and el-file
                         (not (gethash el-file
                                       compile-angel--no-byte-compile-files-list))
                         (not (compile-angel--el-file-excluded-p el-file)))
                (if (and (and (fboundp 'comp-el-to-eln-filename)
                              (funcall 'comp-el-to-eln-filename el-file))
                         (not (compile-angel--elisp-native-compiled-p el-file)))
                    (push feature result)
                  (puthash feature t compile-angel--report-native-compiled-features)
                  (compile-angel--debug-message
                   (concat "compile-angel--get-list-non-native-compiled: "
                           "UP TO DATE: %s (%s)")
                   feature (type-of feature)))))))))
    result))

;;; Functions

(defun compile-angel-report ()
  "Create a buffer listing all features that are not native compiled."
  (let ((buffer (get-buffer-create "*Non-Native-Compiled*"))
        (inhibit-read-only t))
    (when (buffer-live-p buffer)
      (with-current-buffer (get-buffer-create "*compile-angel:report*")
        (read-only-mode 1)
        (erase-buffer)
        (insert "Non-natively compiled features:\n")
        (insert "-------------------------------\n\n")
        (goto-char (point-min))

        (pop-to-buffer (current-buffer))

        (save-excursion
          (goto-char (point-max))
          (let ((count 0))
            (dolist (feature (compile-angel--get-list-non-native-compiled))
              (setq count (1+ count))
              (insert (format "- %s\n" (symbol-name feature))))

            (if (= count 0)
                (insert "(All features are natively-compiled.)")
              (insert (format "\n(%s feature%s %s NOT natively compiled)"
                              count
                              (if (< count 2) "" "s")
                              (if (< count 2) "is" "are"))))))))))

;;;###autoload
(define-minor-mode compile-angel-on-load-mode
  "Toggle `compile-angel-mode' then compiles .el files before they are loaded."
  :global t
  :lighter " CAngelOL"
  :group 'compile-angel
  (if compile-angel-on-load-mode
      (progn
        ;; Init
        (compile-angel--init)
        ;; Compile load-history
        (when compile-angel-on-load-compile-load-history
          (compile-angel--compile-load-history))
        ;; Compile features
        (when compile-angel-on-load-compile-features
          (compile-angel--compile-loaded-features))
        ;; After load hook
        (when compile-angel-on-load-hook-after-load-functions
          (add-hook 'after-load-functions
                    #'compile-angel--hook-after-load-functions))
        (when compile-angel-enable-native-compile
          (add-hook 'native-comp-async-all-done-hook
                    #'compile-angel--ensure-jit-compile))
        ;; Advices
        (when compile-angel-on-load-advise-require
          (advice-add 'require :before #'compile-angel--advice-before-require))
        (when compile-angel-on-load-advise-load
          (advice-add 'load :before #'compile-angel--advice-before-load)))
    ;; Hooks
    (remove-hook 'after-load-functions
                 #'compile-angel--hook-after-load-functions)
    (remove-hook 'native-comp-async-all-done-hook
                 #'compile-angel--ensure-jit-compile)
    ;; Advices
    (advice-remove 'require #'compile-angel--advice-before-require)
    (advice-remove 'load #'compile-angel--advice-before-load)))

;;;###autoload
(define-minor-mode compile-angel-on-save-mode
  "Toggle `compile-angel-mode'that compiles .el file when saved."
  :global t
  :lighter " CAngelSG"
  :group 'compile-angel
  (if compile-angel-on-save-mode
      (progn
        (compile-angel--init)
        (add-hook 'after-save-hook #'compile-angel--compile-on-save 99))
    (remove-hook 'after-save-hook #'compile-angel--compile-on-save)))

;;;###autoload
(define-minor-mode compile-angel-on-save-local-mode
  "Toggle `compile-angel-mode'that compiles .el file when saved."
  :global nil
  :lighter " CAngelSL"
  :group 'compile-angel
  (if compile-angel-on-save-local-mode
      (progn
        (compile-angel--init)
        (add-hook 'after-save-hook #'compile-angel--compile-on-save 99 t))
    (remove-hook 'after-save-hook #'compile-angel--compile-on-save t)))

;;; Provide 'compile-angel
(provide 'compile-angel)
;;; compile-angel.el ends here
