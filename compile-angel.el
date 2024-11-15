;;; compile-angel.el --- Compile Emacs Lisp libraries automatically -*- lexical-binding: t; -*-

;; Copyright (C) 2024 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 0.9.9
;; URL: https://github.com/jamescherti/compile-angel.el
;; Keywords: convenience
;; Package-Requires: ((emacs "24.4"))
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
;; Compile Emacs Lisp libraries automatically.

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
  "A list of regular expressions to exclude certain .el files from compilation."
  :type '(repeat string)
  :group 'compile-angel)

(defcustom compile-angel-verbose nil
  "Non-nil to display more messages."
  :type 'boolean
  :group 'compile-angel)

(defcustom compile-angel-debug nil
  "Non-nil to display debug messages. This displays a lot of messages."
  :type 'boolean
  :group 'compile-angel)

(defcustom compile-angel-predicate-function nil
  "Function that determines if an .el file should be compiled.
It takes one argument (an EL file) and returns t if the file should be compiled,
or nil if the file should not be compiled."
  :group 'compile-angel
  :type '(choice (const nil)
                 (function)))

(defvar compile-angel-on-load-advise-load t
  "When non-nil, automatically compile .el files loaded using `load'.")

(defvar compile-angel-on-load-advise-require t
  "When non-nil, automatically compile .el files loaded using `require'.")

(defvar compile-angel-on-load-advise-autoload t
  "When non-nil, automatically compile .el files loaded using `autoload'.")

(defvar compile-angel-on-load-advise-eval-after-load t
  "When non-nil, compile .el files before `eval-after-load'.")

(defvar compile-angel-on-load-compile-features t
  "Non-nil to compile features listed in the `features' variable.
When `compile-angel-on-load-mode' is activated, this ensures that all features
listed in the `features' variable are compiled.")

;;; Internal variables

(defvar compile-angel--list-compiled-features (make-hash-table :test 'equal))
(defvar compile-angel--list-compiled-files (make-hash-table :test 'equal))
(defvar compile-angel--currently-compiling (make-hash-table :test 'equal))

;;; Functions

(defmacro compile-angel--verbose-message (&rest args)
  "Display a verbose message with [compile-angel] prepended.
The message is formatted with the provided arguments ARGS."
  `(when (or compile-angel-verbose compile-angel-debug)
     (message (concat "[compile-angel] " ,(car args)) ,@(cdr args))))

(defmacro compile-angel--debug-message (&rest args)
  "Display a debug message with [compile-angel] prepended.
The message is formatted with the provided arguments ARGS."
  `(when compile-angel-debug
     (message (concat "[compile-angel] DEBUG: " ,(car args)) ,@(cdr args))))

(defun compile-angel--el-file-excluded-p (el-file)
  "Check if EL-FILE matches any regex in `compile-angel-excluded-files-regexps'.
Return t if the file should be ignored, nil otherwise."
  (when (and compile-angel-excluded-files-regexps
             (cl-some (lambda (regex)
                        (string-match-p regex el-file))
                      compile-angel-excluded-files-regexps))
    (compile-angel--verbose-message "File excluded: %s" el-file)
    t))

(defun compile-angel--elisp-native-compiled-p (el-file)
  "Return t if EL-FILE is native-compiled and up to date.
The return value is non-nil only when the corresponding .eln file is newer than
its source."
  (let ((eln-file (comp-el-to-eln-filename el-file)))
    (when (and eln-file (file-newer-than-file-p el-file eln-file))
      eln-file)))

(defun compile-angel--native-compile (el-file)
  "Native-compile EL-FILE."
  (when compile-angel-enable-native-compile
    (if (and (not (compile-angel--elisp-native-compiled-p el-file)))
        (if (and (featurep 'native-compile)
                 (fboundp 'native-comp-available-p)
                 (fboundp 'native-compile-async)
                 (native-comp-available-p))
            (progn (compile-angel--verbose-message "Native-compilation: %s"
                                                   el-file)
                   (native-compile-async el-file))
          (compile-angel--debug-message
           "Native-compilation ignored (native-comp unavailable): %s" el-file))
      (compile-angel--debug-message "Native-compilation ignored (up-to-date): %s"
                                    el-file))))

(defun compile-angel--byte-compile (el-file elc-file)
  "Byte-compile EL-FILE into ELC-FILE."
  (let* ((elc-file-exists (file-exists-p elc-file)))
    (when (or (not elc-file-exists)
              (file-newer-than-file-p el-file elc-file))
      (if (not (file-writable-p elc-file))
          (compile-angel--debug-message
           "Byte-compilation ignored (not writable): %s" elc-file)
        (let* ((after-change-major-mode-hook
                (and (fboundp 'global-font-lock-mode-enable-in-buffer)
                     (list 'global-font-lock-mode-enable-in-buffer)))
               (inhibit-message (not compile-angel-verbose))
               (prog-mode-hook nil)
               (emacs-lisp-mode-hook nil)
               (byte-compile-result
                (byte-compile-file el-file)))
          (cond
           ;; Ignore (no-byte-compile)
           ((eq byte-compile-result 'no-byte-compile)
            (compile-angel--debug-message
             "Byte-compilation Ignore (no-byte-compile): %s" el-file)
            t) ; Return t: We can native-compile

           ;; Ignore: Byte-compilation error
           ((not byte-compile-result)
            (compile-angel--verbose-message "Byte-compilation error: %s" el-file)
            nil) ; Return nil (No native-compile)

           ;; Success
           (byte-compile-result
            (compile-angel--verbose-message "Byte-compilation: %s" el-file)
            t))))))) ; Return t: We can native-compile

(defun compile-angel--need-compilation-p (el-file feature-name)
  "Return non-nil if EL-FILE or FEATURE-NAME need compilation.
EL-FILE is a String representing the path to the Elisp source file.
FEATURE-NAME is a string representing the feature name being loaded."
  (and el-file
       ;; Predicate function
       (if compile-angel-predicate-function
           (funcall compile-angel-predicate-function el-file)
         t)
       ;; El files
       (string-match-p
        (format "\\.el%s\\'" (regexp-opt load-file-rep-suffixes)) el-file)
       ;; Compile when compile-once mode is off or file/feature not compiled yet
       (or (not compile-angel-on-load-mode-compile-once)
           (and
            (not (gethash el-file compile-angel--list-compiled-files))
            (or (not feature-name)
                (not (gethash feature-name
                              compile-angel--list-compiled-features)))))
       (not (compile-angel--el-file-excluded-p el-file))))

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
          (when (compile-angel--byte-compile el-file elc-file)
            (compile-angel--native-compile el-file))
        (compile-angel--native-compile el-file))))))

(defun compile-angel--compile-current-buffer ()
  "Compile the current buffer."
  (when (derived-mode-p 'emacs-lisp-mode)
    (compile-angel--compile-elisp (buffer-file-name (buffer-base-buffer)))))

(defun compile-angel--guess-el-file (el-file &optional feature nosuffix)
  "Guess the EL-FILE or FEATURE path. NOSUFFIX is similar to `load'."
  (let* ((library (or el-file (cond ((stringp feature)
                                     feature)
                                    ((symbolp feature)
                                     (symbol-name feature))
                                    (t nil))))
         (result (when library
                   (locate-file (substitute-in-file-name library)
                                load-path
                                (if nosuffix
                                    load-file-rep-suffixes
                                  (mapcar (lambda (s) (concat ".el" s))
                                          load-file-rep-suffixes))))))
    (if result
        (expand-file-name result)
      result)))

(defun compile-angel--feature-to-feature-name (feature)
  "Convert a FEATURE symbol into a feature name and return it."
  (cond
   ((and (not (stringp feature))
         (not (symbolp feature)))
    (setq feature nil)
    (compile-angel--debug-message
     "ISSUE: UNSUPPORTED Feature: Not a symbol: %s (type: %s)"
     feature (type-of feature)))
   (t
    (setq feature (cond ((stringp feature)
                         feature)
                        ((symbolp feature)
                         (symbol-name feature)))))))

(defun compile-angel--compile-before-loading (el-file
                                              &optional feature nosuffix)
  "This function is called by all the :before advices.
EL-FILE, FEATURE, and NOSUFFIX are the same arguments as `load' and `require'."
  (when (or compile-angel-enable-byte-compile
            compile-angel-enable-native-compile)
    (when compile-angel-debug
      (when (and el-file (not (stringp el-file)))
        (compile-angel--debug-message
         (concat "ISSUE: Wrong el-file type passed to "
                 "compile-angel--compile-before-loading: %s (%s)")
         el-file (type-of el-file)))

      (when (and feature (not (stringp feature)) (not (symbolp feature)))
        (compile-angel--debug-message
         (concat "ISSUE: Wrong feature type passed to "
                 "compile-angel--compile-before-loading: %s (%s)")
         feature (type-of feature)))

      (when (gethash el-file compile-angel--currently-compiling)
        (compile-angel--debug-message
         (concat "Already compiling while trying to compile: "
                 "compile-angel--compile-before-loading: %s | %s")
         feature el-file)))

    (let* ((el-file (compile-angel--guess-el-file el-file feature nosuffix))
           (feature-name (when feature
                           (compile-angel--feature-to-feature-name feature))))
      (compile-angel--debug-message "COMPILATION ARGS: %s | %s"
                                    el-file feature-name)
      (if (and (not (gethash el-file
                             compile-angel--currently-compiling))
               (compile-angel--need-compilation-p el-file feature-name))
          (progn
            (when feature-name
              (puthash feature-name t compile-angel--list-compiled-features))
            (puthash el-file t compile-angel--list-compiled-files)
            (unwind-protect
                (progn
                  (puthash el-file t compile-angel--currently-compiling)
                  (compile-angel--compile-elisp el-file))
              (remhash el-file compile-angel--currently-compiling)))
        (compile-angel--debug-message
         "IGNORE compilation: %s | %s" el-file feature)))))

(defun compile-angel--advice-before-require (feature
                                             &optional filename _noerror)
  "Recompile the library before `require'.
FEATURE and FILENAME are the same arguments as the `require' function."
  (compile-angel--debug-message
   "REQUIRE: %s (%s) | %s (%s)"
   filename (type-of filename) feature (type-of feature))
  (compile-angel--compile-before-loading filename feature))

(defun compile-angel--advice-before-load (el-file &optional _noerror _nomessage
                                                  nosuffix _must-suffix)
  "Recompile before `load'. EL-FILE and NOSUFFIX are the same args as `load'."
  (compile-angel--debug-message "LOAD: %s (%s)" el-file (type-of el-file))
  (if (stringp el-file)
      ;; Unset the special init-file status to prevent recursive loads
      (let ((user-init-file (if (eq user-init-file t)
                                nil
                              user-init-file)))
        (compile-angel--compile-before-loading el-file nil nosuffix))
    (compile-angel--debug-message
     (concat "ISSUE: Wrong type passed to "
             "compile-angel--advice-before-require %s (%s)")
     el-file (type-of el-file))))

(defun compile-angel--advice-before-autoload (_function
                                              feature
                                              &optional _docstring _interactive
                                              _type)
  "Recompile before `autoload'. FEATURE is the file or the feature."
  (when compile-angel-debug (compile-angel--debug-message
                             "AUTOLOAD: %s (%s)" feature (type-of feature)))
  (compile-angel--compile-before-loading nil feature))

(defun compile-angel--advice-eval-after-load (feature-or-file _form)
  "Advice to track what FEATURE-OR-FILE (symbol) is passed to `eval-after-load'."
  (compile-angel--debug-message "EVAL-AFTER-LOAD: %s (%s)"
                                feature-or-file (type-of feature-or-file))
  (cond
   ((symbolp feature-or-file)
    (compile-angel--compile-before-loading nil feature-or-file))
   ((stringp feature-or-file)
    (compile-angel--compile-before-loading feature-or-file nil))
   (t
    (compile-angel--debug-message
     (concat "ISSUE: Wrong type passed to "
             "compile-angel--advice-eval-after-load: %s (%s)")
     feature-or-file (type-of feature-or-file)))))

(defun compile-angel--compile-features ()
  "Compile all loaded features that are in the `features' variable."
  (dolist (feature features)
    (compile-angel--debug-message
     "compile-angel--compile-features: %s" feature)
    (compile-angel--compile-before-loading nil feature)))

;;;###autoload
(define-minor-mode compile-angel-on-load-mode
  "Toggle `compile-angel-mode' then compiles .el files before they are loaded."
  :global t
  :lighter " CompAngelL"
  :group 'compile-angel
  (if compile-angel-on-load-mode
      (progn
        (when compile-angel-on-load-compile-features
          (compile-angel--compile-features))
        (when compile-angel-on-load-advise-autoload
          (advice-add 'autoload :before #'compile-angel--advice-before-autoload))
        (when compile-angel-on-load-advise-require
          (advice-add 'require :before #'compile-angel--advice-before-require))
        (when compile-angel-on-load-advise-load
          (advice-add 'load :before #'compile-angel--advice-before-load))
        (when compile-angel-on-load-advise-eval-after-load
          (advice-add 'eval-after-load
                      :before #'compile-angel--advice-eval-after-load)))
    (advice-remove 'autoload #'compile-angel--advice-before-autoload)
    (advice-remove 'require #'compile-angel--advice-before-require)
    (advice-remove 'load #'compile-angel--advice-before-load)
    (advice-remove 'eval-after-load #'compile-angel--advice-eval-after-load)))

;;;###autoload
(define-minor-mode compile-angel-on-save-mode
  "Toggle `compile-angel-mode'that compiles .el file when saved."
  :global t
  :lighter " CompAngelS"
  :group 'compile-angel
  (if compile-angel-on-save-mode
      (add-hook 'after-save-hook #'compile-angel--compile-current-buffer)
    (remove-hook 'after-save-hook #'compile-angel--compile-current-buffer)))

(provide 'compile-angel)
;;; compile-angel.el ends here
