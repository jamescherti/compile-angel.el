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

(defcustom compile-angel-display-buffer nil
  "Non-nil to automatically display the *Compile-Log* buffer."
  :type 'boolean
  :group 'compile-angel)

(defcustom compile-angel-verbose nil
  "Non-nil to display more messages."
  :type 'boolean
  :group 'compile-angel)

(defcustom compile-angel-enable-byte-compile t
  "Non-nil to enable byte compilation of Emacs Lisp (.el) files."
  :type 'boolean
  :group 'compile-angel)

(defcustom compile-angel-enable-native-compile t
  "Non-nil to enable native compilation of Emacs Lisp (.el) files."
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

(defcustom compile-angel-predicate-function nil
  "Function that determines if an .el file should be compiled.
It takes one argument (an EL file) and returns t if the file should be compiled,
or nil if the file should not be compiled."
  :group 'compile-angel
  :type '(choice (const nil)
                 (function)))

(defvar compile-angel-on-load-mode-advise-load t
  "When non-nil, automatically compile .el files loaded using `load'.")

(defvar compile-angel-on-load-mode-advise-require t
  "When non-nil, automatically compile .el files loaded using `require'.")

(defvar compile-angel-on-load-mode-advise-autoload t
  "When non-nil, automatically compile .el files loaded using `autoload'.")

(defvar compile-angel-on-load-mode-advise-eval-after-load t
  "When non-nil, compile .el files before `eval-after-load'.")

;;; Internal variables

(defvar compile-angel--list-compiled-files (make-hash-table :test 'equal))
(defvar compile-angel--native-comp-available nil)
(defvar warning-minimum-level)
(defvar compile-angel--currently-compiling nil)

;;; Functions

(defun compile-angel--el-file-excluded-p (el-file)
  "Check if EL-FILE matches any regex in `compile-angel-excluded-files-regexps'.
Return t if the file should be ignored, nil otherwise."
  (when (and compile-angel-excluded-files-regexps
             (cl-some (lambda (regex)
                        (string-match-p regex el-file))
                      compile-angel-excluded-files-regexps))
    (when compile-angel-verbose
      (message "[compile-angel] File excluded: %s" el-file))
    t))

(defun compile-angel--elisp-native-compiled-p (el-file)
  "Return t if EL-FILE is native compiled and up to date.
The return value is non-nil only when the corresponding .eln file is newer than
its source."
  (let ((eln-file (comp-el-to-eln-filename el-file)))
    (when (and eln-file (file-newer-than-file-p el-file eln-file))
      eln-file)))

(defun compile-angel--native-compile (el-file)
  "Native compile EL-FILE."
  (when compile-angel--native-comp-available
    (unless (compile-angel--elisp-native-compiled-p el-file)
      (let ((warning-minimum-level :error))
        (when compile-angel-verbose
          (message "[compile-angel] Native compile: %s" el-file))
        (native-compile-async el-file)))))

(defun compile-angel--byte-compile (el-file elc-file)
  "Byte-compile EL-FILE into ELC-FILE."
  (let* ((elc-file-exists (file-exists-p elc-file)))
    (when (or (not elc-file-exists)
              (file-newer-than-file-p el-file elc-file))
      (if (not (file-writable-p elc-file))
          (when compile-angel-verbose
            (message
             "[compile-angel] Byte-compilation ignored (not writable): %s"
             elc-file)
            t) ; Return t: We can native compile
        (let* ((byte-compile-verbose compile-angel-verbose)
               (warning-minimum-level (if compile-angel-display-buffer
                                          :warning
                                        :error))
               (save-silently t)
               (byte-compile-result
                (let ((after-change-major-mode-hook
                       (and (fboundp 'global-font-lock-mode-enable-in-buffer)
                            (list 'global-font-lock-mode-enable-in-buffer)))
                      (inhibit-message (not compile-angel-verbose))
                      (prog-mode-hook nil)
                      (emacs-lisp-mode-hook nil))

                  (byte-compile-file el-file))))
          (cond
           ;; Ignore (no-byte-compile)
           ((eq byte-compile-result 'no-byte-compile)
            (when compile-angel-verbose
              (message
               "[compile-angel] Byte-compilation Ignore (no-byte-compile): %s"
               el-file))
            t) ; Return t: We can native compile

           ;; Ignore: Byte-compilation error
           ((not byte-compile-result)
            (when compile-angel-verbose
              (message "[compile-angel] Byte-compilation error: %s" el-file))
            nil) ; Return nil (No native compile)

           ;; Success
           (byte-compile-result
            (when compile-angel-verbose
              (message "[compile-angel] Byte-compilation: %s" el-file))
            t))))))) ; Return t: We can native compile

(defun compile-angel--compile-elisp (el-file)
  "Byte-compile and Native-compile the .el file EL-FILE."
  (when (and el-file
             (not compile-angel--currently-compiling)
             (or compile-angel-enable-byte-compile
                 compile-angel-enable-native-compile)
             (or (not compile-angel-on-load-mode-compile-once)
                 (not (gethash el-file compile-angel--list-compiled-files)))
             (not (compile-angel--el-file-excluded-p el-file))
             (if compile-angel-predicate-function
                 (funcall compile-angel-predicate-function el-file)
               t)
             (string-match-p
              (format "\\.el%s\\'" (regexp-opt load-file-rep-suffixes)) el-file))
    (puthash el-file t compile-angel--list-compiled-files)
    (setq compile-angel--currently-compiling t)
    (unwind-protect
        (let* ((elc-file (byte-compile-dest-file el-file)))
          (cond
           ((not (file-exists-p el-file))
            (message "[compile-angel] Warning: The file does not exist: %s"
                     el-file))

           ((not elc-file)
            (message "[compile-angel] Warning: The file is not an .el file: %s"
                     el-file))

           (t
            (if compile-angel-enable-byte-compile
                (when (compile-angel--byte-compile el-file elc-file)
                  (when compile-angel-enable-native-compile
                    (compile-angel--native-compile el-file)))
              (when compile-angel-enable-native-compile
                (compile-angel--native-compile el-file))))))
      (setq compile-angel--currently-compiling nil))))

(defun compile-angel--compile-current-buffer ()
  "Compile the current buffer."
  (when (derived-mode-p 'emacs-lisp-mode)
    (let ((el-file (buffer-file-name (buffer-base-buffer))))
      (compile-angel--compile-elisp el-file))))

(defun compile-angel--locate-library (library nosuffix)
  "Return the path to the LIBRARY el file.
Use `load-file-rep-suffixes' when NOSUFFIX is non-nil."
  (locate-file (substitute-in-file-name library)
               load-path
               (if nosuffix
                   load-file-rep-suffixes
                 (mapcar (lambda (s) (concat ".el" s))
                         load-file-rep-suffixes))))

(defun compile-angel--guess-el-file (el-file &optional feature nosuffix)
  "Guess the EL-FILE or FEATURE path. NOSUFFIX is similar to `load'."
  (compile-angel--locate-library (or el-file (cond ((stringp feature)
                                                    feature)
                                                   ((symbolp feature)
                                                    (symbol-name feature))
                                                   (t nil)))
                                 nosuffix))

(defun compile-angel--compile-before-loading (el-file
                                              &optional feature nosuffix)
  "This function is called by the :before advices.
EL-FILE, FEATURE, and NOSUFFIX are the same arguments as `load' and `require'."
  (when (or el-file feature)
    (let ((el-file (compile-angel--guess-el-file el-file feature nosuffix)))
      (compile-angel--compile-elisp el-file))))

(defun compile-angel--advice-before-require (feature
                                             &optional filename _noerror)
  "Recompile the library before `require'.
FEATURE and FILENAME are the same arguments as the `require' function."
  (compile-angel--compile-before-loading filename feature))

(defun compile-angel--advice-before-load (el-file &optional _noerror _nomessage
                                                  nosuffix _must-suffix)
  "Recompile before `load'. EL-FILE and NOSUFFIX are the same args as `load'."
  (if (eq user-init-file t)
      (let ((user-init-file nil))
        ;; Temporarily unset the special init-file status to prevent recursive
        ;; loads from being treated as init-file loads.
        (compile-angel--compile-before-loading el-file nil nosuffix))
    (compile-angel--compile-before-loading el-file nil nosuffix)))

(defun compile-angel--advice-before-autoload (_function
                                              file-or-feature
                                              &optional _docstring _interactive
                                              _type)
  "Recompile before `autoload'. FILE-OR-FEATURE is the file or the feature."
  (compile-angel--compile-before-loading file-or-feature))

(defun compile-angel--check-native-comp-available ()
  "Determine if native compilation is available and set a flag accordingly."
  (unless compile-angel--native-comp-available
    (when (and (featurep 'native-compile)
               (fboundp 'native-comp-available-p)
               (fboundp 'native-compile-async)
               (native-comp-available-p))
      (setq compile-angel--native-comp-available t))))

(defun compile-angel--advice-eval-after-load (el-file _form)
  "Advice to track what EL-FILE is passed to `eval-after-load'."
  (cond ((and el-file (stringp el-file))
         (compile-angel--compile-before-loading el-file nil))
        ((symbolp el-file)
         (let ((feature (symbol-name el-file)))
           (compile-angel--compile-before-loading nil feature)))))

;;;###autoload
(define-minor-mode compile-angel-on-load-mode
  "Toggle `compile-angel-mode' then compiles .el files before they are loaded."
  :global t
  :lighter " CompAngelL"
  :group 'compile-angel
  (compile-angel--check-native-comp-available)

  (if compile-angel-on-load-mode
      (progn
        (when compile-angel-on-load-mode-advise-autoload
          (advice-add 'autoload :before #'compile-angel--advice-before-autoload))
        (when compile-angel-on-load-mode-advise-require
          (advice-add 'require :before #'compile-angel--advice-before-require))
        (when compile-angel-on-load-mode-advise-load
          (advice-add 'load :before #'compile-angel--advice-before-load))
        (when compile-angel-on-load-mode-advise-eval-after-load
          (advice-add 'eval-after-load :before #'compile-angel--advice-eval-after-load)))
    (advice-remove 'autoload #'compile-angel--advice-before-autoload)
    (advice-remove 'require 'compile-angel--advice-before-require)
    (advice-remove 'load 'compile-angel--advice-before-load)
    (advice-remove 'eval-after-load #'compile-angel--advice-eval-after-load)))

;;;###autoload
(define-minor-mode compile-angel-on-save-mode
  "Toggle `compile-angel-mode'that compiles .el file when saved."
  :global t
  :lighter " CompAngelS"
  :group 'compile-angel
  (compile-angel--check-native-comp-available)
  (if compile-angel-on-save-mode
      (add-hook 'after-save-hook #'compile-angel--compile-current-buffer)
    (remove-hook 'after-save-hook #'compile-angel--compile-current-buffer)))

(provide 'compile-angel)
;;; compile-angel.el ends here
