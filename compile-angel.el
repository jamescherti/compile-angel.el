;;; compile-angel.el --- Automatically Compile Elisp files (auto-compile alternative) -*- lexical-binding: t; -*-

;; Copyright (C) 2024 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.4
;; URL: https://github.com/jamescherti/compile-angel.el
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1"))
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
;;   (compile-angel-verbose nil)
;;   :config
;;   (compile-angel-on-load-mode)
;;   (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))
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

(defcustom compile-angel-on-load-mode-compile-once t
  "If non-nil, enable single compilation for `compile-angel-on-load-mode'.
This setting causes the `compile-angel-on-load-mode' to perform byte and native
compilation of .el files only once during initial loading. When disabled (nil),
the mode will recompile on each load."
  :type 'boolean
  :group 'compile-angel)

(defcustom compile-angel-excluded-files-regexps nil
  "A list of regular expressions to exclude certain .el files from compilation.
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

;;; Internal variables

(defvar compile-angel--list-compiled-files (make-hash-table :test 'equal))
(defvar compile-angel--list-jit-native-compiled-files (make-hash-table :test 'equal))
(defvar compile-angel--currently-compiling nil)
(defvar compile-angel--force-compilation nil)
(defvar compile-angel--native-compile-when-jit-enabled nil)
(defvar compile-angel--el-file-regexp nil)
(defvar compile-angel--loaddefs-regexps nil)

;;; Functions

(defun compile-angel--insert-message (buffer-name msg &rest args)
  "Insert formatted MSG with ARGS into BUFFER-NAME buffer."
  (with-current-buffer (get-buffer-create buffer-name)
    (unwind-protect
        (progn
          (read-only-mode -1)
          (goto-char (point-max))
          (insert (apply 'format msg args) "\n"))
      (read-only-mode 1))))

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

(defun compile-angel--el-file-excluded-p (el-file)
  "Check if EL-FILE matches any regex in `compile-angel-excluded-files-regexps'.
Return non-nil if the file should be ignored, nil otherwise."
  (when (and compile-angel-excluded-files-regexps
             (cl-some (lambda (regex)
                        (string-match-p regex el-file))
                      compile-angel-excluded-files-regexps))
    (compile-angel--debug-message
     "SKIP (.el file excluded with a regex): %s" el-file)
    t))

(defun compile-angel--elisp-native-compiled-p (el-file)
  "Return non-nil if EL-FILE is native-compiled and up to date.
Return nil if it is not native-compiled or if its .eln file is out of date."
  (let ((eln-file (comp-el-to-eln-filename el-file)))
    (when (and eln-file
               (file-exists-p eln-file)
               (file-newer-than-file-p eln-file el-file))
      t)))

(defun compile-angel--native-compile (el-file)
  "Native-compile EL-FILE."
  (cond ((and (not compile-angel--native-compile-when-jit-enabled)
              (or
               (bound-and-true-p native-comp-jit-compilation)
               (bound-and-true-p native-comp-deferred-compilation)))
         (puthash el-file t compile-angel--list-jit-native-compiled-files)
         (compile-angel--debug-message
          "Native-compilation ignored (Reason: JIT compilation will do it): %s"
          el-file))

        ((and (not compile-angel--force-compilation)
              (and (boundp 'comp-files-queue)
                   (assoc el-file comp-files-queue)))
         (compile-angel--debug-message
          "Native-compilation ignored (Already in the async compile queue): %s"
          el-file))

        (t
         (cond
          ((not (and (featurep 'native-compile)
                     (fboundp 'native-comp-available-p)
                     (fboundp 'native-compile-async)
                     (native-comp-available-p)))
           (compile-angel--debug-message
            "Native-compilation ignored (native-comp unavailable): %s" el-file))

          ((compile-angel--elisp-native-compiled-p el-file)
           (compile-angel--debug-message
            "Native-compilation ignored (up-to-date): %s" el-file))

          (t
           (compile-angel--debug-message "Native-compilation: %s" el-file)
           (let ((inhibit-message (not (or compile-angel-verbose
                                           compile-angel-debug))))
             (native-compile-async el-file)))))))

(defun compile-angel--byte-compile (el-file elc-file)
  "Byte-compile EL-FILE into ELC-FILE.
Return non-nil to allow native compilation."
  (compile-angel--debug-message "Start: Byte-compilation: %s -> %s"
                                el-file elc-file)
  (cond
   ((and (file-exists-p elc-file)
         (not (file-newer-than-file-p el-file elc-file)))
    (compile-angel--debug-message
     "Byte-compilation Ignored (up-to-date): %s" el-file)
    t)

   ((not (file-writable-p elc-file))
    (compile-angel--debug-message
     "Byte-compilation ignored (not writable): %s" elc-file)
    t)

   (t
    (let* ((after-change-major-mode-hook
            (and (fboundp 'global-font-lock-mode-enable-in-buffer)
                 (list 'global-font-lock-mode-enable-in-buffer)))
           (inhibit-message (not (or compile-angel-verbose
                                     compile-angel-debug)))
           (prog-mode-hook nil)
           (emacs-lisp-mode-hook nil)
           (byte-compile-result
            (byte-compile-file el-file)))
      (cond
       ((eq byte-compile-result 'no-byte-compile)
        (compile-angel--debug-message
         "Byte-compilation Ignore (no-byte-compile): %s" el-file)
        nil)

       ((not byte-compile-result)
        (compile-angel--debug-message "Byte-compilation error: %s" el-file)
        nil)

       (byte-compile-result
        (compile-angel--debug-message
         "Byte-compilation successful: %s" el-file)
        t))))))

(defun compile-angel--need-compilation-p (el-file feature-name)
  "Return non-nil if EL-FILE or FEATURE-NAME need compilation.
EL-FILE is a String representing the path to the Elisp source file.
FEATURE-NAME is a string representing the feature name being loaded."
  (cond
   ((not el-file)
    (compile-angel--debug-message
     "SKIP (el-file is nil): %s | %s" el-file feature-name)
    nil)

   ((not (if compile-angel-predicate-function
             (funcall compile-angel-predicate-function el-file)
           t))
    (compile-angel--debug-message
     "SKIP (Predicate function returned nil): %s | %s" el-file feature-name)
    nil)

   ((not (compile-angel--is-el-file el-file))
    (compile-angel--debug-message
     "SKIP (Does not end with the .el): %s | %s" el-file feature-name)
    nil)

   ((and (not compile-angel--force-compilation)
         (or (not compile-angel-on-load-mode-compile-once)
             (gethash el-file compile-angel--list-compiled-files)))
    (compile-angel--debug-message
     "SKIP (In the skip hash list): %s | %s" el-file feature-name)
    nil)

   ((compile-angel--el-file-excluded-p el-file)
    nil)

   (t t)))

(defun compile-angel--compile-elisp (el-file)
  "Byte-compile and Native-compile the .el file EL-FILE."
  (let* ((elc-file (byte-compile-dest-file el-file)))
    (cond
     ((not (file-exists-p el-file))
      (message "[compile-angel] Warning: The file does not exist: %s" el-file))

     ((not elc-file)
      (message "[compile-angel] Warning: The file is not an .el file: %s"
               el-file))

     (t
      (if compile-angel-enable-byte-compile
          (progn
            (compile-angel--verbose-message
             "Byte and Native compilation: %s" el-file)
            (when (compile-angel--byte-compile el-file elc-file)
              (when compile-angel-enable-native-compile
                (compile-angel--native-compile el-file))))
        (when compile-angel-enable-native-compile
          (compile-angel--verbose-message "Native-compilation only: %s" el-file)
          (compile-angel--native-compile el-file)))))))

(defun compile-angel--compile-current-buffer ()
  "Compile the current buffer."
  (let ((compile-angel--force-compilation t)
        (compile-angel--native-compile-when-jit-enabled t)
        (el-file (buffer-file-name (buffer-base-buffer))))
    (when (and (derived-mode-p 'emacs-lisp-mode)
               (if compile-angel-predicate-function
                   (funcall compile-angel-predicate-function el-file)
                 t)
               (not (compile-angel--el-file-excluded-p el-file)))
      (compile-angel--compile-elisp el-file))))

(defun compile-angel--feature-to-feature-name (feature)
  "Convert a FEATURE symbol into a feature name and return it."
  (cond
   ((stringp feature)
    feature)
   ((symbolp feature)
    (symbol-name feature))
   (t
    (compile-angel--debug-message
     "ISSUE: UNSUPPORTED Feature: Not a symbol: %s (type: %s)"
     feature (type-of feature))
    nil)))

(defun compile-angel--guess-el-file (el-file
                                     &optional feature-name nosuffix)
  "Guess the EL-FILE or FEATURE-NAME path. NOSUFFIX is similar to `load'.
Checks caches before performing computation."
  (let* ((el-file (when (stringp el-file) el-file))
         (result nil))
    ;; Return result
    (if result
        result
      ;; Find result and return it
      (setq result (if (and el-file
                            (compile-angel--is-el-file el-file))
                       el-file
                     (locate-file (or el-file feature-name)
                                  load-path
                                  (if nosuffix
                                      load-file-rep-suffixes
                                    (mapcar (lambda (s) (concat ".el" s))
                                            load-file-rep-suffixes)))))
      result)))

(defun compile-angel--entry-point (el-file &optional feature nosuffix)
  "This function is called by all the :before advices.
EL-FILE, FEATURE, and NOSUFFIX are the same arguments as `load' and `require'."
  (when (or compile-angel-enable-byte-compile
            compile-angel-enable-native-compile)
    (let* ((feature-name (compile-angel--feature-to-feature-name feature))
           (el-file (compile-angel--guess-el-file
                     el-file feature-name nosuffix)))
      (compile-angel--debug-message "COMPILATION ARGS: %s | %s"
                                    el-file feature-name)
      (cond
       ((not el-file)
        (compile-angel--debug-message
         "SKIP (Returned a nil .el file): %s | %s" el-file feature))

       ((member el-file compile-angel--currently-compiling)
        (compile-angel--debug-message
         "SKIP (To prevent recursive compilation): %s | %s" el-file feature))

       ((not (compile-angel--need-compilation-p el-file feature-name))
        (compile-angel--debug-message
         "SKIP (Does not need compilation): %s | %s" el-file feature))

       (t
        (puthash el-file t compile-angel--list-compiled-files)
        (let ((compile-angel--currently-compiling
               (cons el-file compile-angel--currently-compiling)))
          (compile-angel--compile-elisp el-file)))))))

(defun compile-angel--advice-before-require (feature
                                             &optional filename _noerror)
  "Recompile the library before `require'.
FEATURE and FILENAME are the same arguments as the `require' function."
  (compile-angel--debug-message
   "REQUIRE: %s (%s) | %s (%s)"
   filename (type-of filename) feature (type-of feature))
  (compile-angel--entry-point filename feature))

(defun compile-angel--advice-before-load (el-file &optional _noerror _nomessage
                                                  nosuffix _must-suffix)
  "Recompile before `load'. EL-FILE and NOSUFFIX are the same args as `load'."
  (compile-angel--debug-message "LOAD: %s (%s)" el-file (type-of el-file))
  (if (stringp el-file)
      ;; Unset the special init-file status to prevent recursive loads
      (let ((user-init-file (if (eq user-init-file t)
                                nil
                              user-init-file)))
        (compile-angel--entry-point (when el-file
                                      (expand-file-name
                                       (substitute-in-file-name el-file)))
                                    nil nosuffix))
    (compile-angel--debug-message
     (concat "ISSUE: Wrong type passed to "
             "compile-angel--advice-before-require %s (%s)")
     el-file (type-of el-file))))

(defun compile-angel-compile-features ()
  "Compile all loaded features that are in the `features' variable."
  (let ((compile-angel--native-compile-when-jit-enabled t))
    (dolist (feature features)
      (compile-angel--debug-message
       "compile-angel-compile-features: %s" feature)
      (compile-angel--entry-point nil feature))))

(defun compile-angel--find-el-file (file)
  "Find the .el file corresponding to FILE.

If FILE is already a .el file, return it. If FILE is a .elc file, check for the
corresponding .el file by removing the .elc extension and verifying its
existence.

The function iterates through the extensions in `load-file-rep-suffixes` to
construct possible .el file paths. If a matching file exists, return its path;
otherwise, return nil."
  (cond
   ((not file)
    (compile-angel--debug-message
     "compile-angel--find-el-file: nil file")
    nil)

   ((compile-angel--is-el-file file)
    file)

   ((string-equal (file-name-extension file) "elc")
    (let ((base (file-name-sans-extension file))
          (suffixes load-file-rep-suffixes)
          result)
      (while (and suffixes (not result))
        (let ((candidate (concat base ".el")))
          (when (file-exists-p candidate)
            (setq result candidate)))
        (setq suffixes (cdr suffixes)))
      result))

   (t
    (compile-angel--debug-message
     "compile-angel--find-el-file: NO .el FILE CORRESPONDS TO: %s" file))))

(defun compile-angel--hook-after-load-functions (file)
  "Compile FILE after load."
  (let ((file (compile-angel--find-el-file file)))
    (when file
      (compile-angel--debug-message
       "compile-angel--hook-after-load-functions: %s" file)
      (if (not (compile-angel--is-el-file file))
          (compile-angel--debug-message
           "compile-angel--hook-after-load-functions: IGNORE: %s" file)
        (progn
          (compile-angel--debug-message
           "compile-angel--hook-after-load-functions: COMPILE: %s"
           file)
          (let ((compile-angel--native-compile-when-jit-enabled t)
                (compile-angel--force-compilation t))
            (compile-angel--entry-point file)))))))

(defun compile-angel--update-el-file-regexp (_symbol new-value
                                                     _operation _where)
  "Update the `compile-angel--el-file-regexp' variable.
NEW-VALUE is the value of the variable."
  (compile-angel--debug-message "WATCHER: Update: %s" new-value)
  (setq compile-angel--el-file-regexp
        (format "\\.el%s\\'" (regexp-opt new-value)))
  (setq compile-angel--loaddefs-regexps
        (list (concat "/cus-load" compile-angel--el-file-regexp)
              (concat "/charprop" compile-angel--el-file-regexp)
              (concat "-loaddefs" compile-angel--el-file-regexp)
              (concat "/loaddefs" compile-angel--el-file-regexp))))

(defun compile-angel--init ()
  "Initialize internal variables."
  (unless compile-angel--el-file-regexp
    (compile-angel--update-el-file-regexp nil load-file-rep-suffixes nil nil)
    (add-variable-watcher 'load-file-rep-suffixes
                          #'compile-angel--update-el-file-regexp)))

(defun compile-angel--is-el-file (file)
  "Return non-nil if FILE is an el-file."
  (when compile-angel--el-file-regexp
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
                   (let ((compile-angel--native-compile-when-jit-enabled t))
                     (compile-angel--native-compile el-file)))
                 compile-angel--list-jit-native-compiled-files)
      (clrhash compile-angel--list-jit-native-compiled-files))))

;;;###autoload
(define-minor-mode compile-angel-on-load-mode
  "Toggle `compile-angel-mode' then compiles .el files before they are loaded."
  :global t
  :lighter " CAngelL"
  :group 'compile-angel
  (if compile-angel-on-load-mode
      (progn
        ;; Init
        (compile-angel--init)
        ;; Hooks
        (when compile-angel-on-load-hook-after-load-functions
          (add-hook 'after-load-functions #'compile-angel--hook-after-load-functions))
        (when compile-angel-enable-native-compile
          (add-hook 'native-comp-async-all-done-hook #'compile-angel--ensure-jit-compile))
        ;; Compile features
        (when compile-angel-on-load-compile-features
          (compile-angel-compile-features))
        ;; Advices
        (when compile-angel-on-load-advise-require
          (advice-add 'require :before #'compile-angel--advice-before-require))
        (when compile-angel-on-load-advise-load
          (advice-add 'load :before #'compile-angel--advice-before-load)))
    ;; Hooks
    (remove-hook 'after-load-functions #'compile-angel--hook-after-load-functions)
    (remove-hook 'native-comp-async-all-done-hook #'compile-angel--ensure-jit-compile)
    ;; Advices
    (advice-remove 'require #'compile-angel--advice-before-require)
    (advice-remove 'load #'compile-angel--advice-before-load)))

;;;###autoload
(define-minor-mode compile-angel-on-save-mode
  "Toggle `compile-angel-mode'that compiles .el file when saved."
  :global t
  :lighter " CAngelSg"
  :group 'compile-angel
  (if compile-angel-on-save-mode
      (progn
        (compile-angel--init)
        (add-hook 'after-save-hook #'compile-angel--compile-current-buffer))
    (remove-hook 'after-save-hook #'compile-angel--compile-current-buffer)))

;;;###autoload
(define-minor-mode compile-angel-on-save-local-mode
  "Toggle `compile-angel-mode'that compiles .el file when saved."
  :global nil
  :lighter " CAngelSl"
  :group 'compile-angel
  (if compile-angel-on-save-local-mode
      (progn
        (compile-angel--init)
        (add-hook 'after-save-hook #'compile-angel--compile-current-buffer nil t))
    (remove-hook 'after-save-hook #'compile-angel--compile-current-buffer t)))

(provide 'compile-angel)
;;; compile-angel.el ends here
