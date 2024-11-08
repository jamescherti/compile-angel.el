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

;;; Internal variables

(defvar compile-angel--list-compiled-files (make-hash-table :test 'equal))
(defvar compile-angel--native-comp-available nil)
(defvar warning-minimum-level)

;;; Functions

(defun compile-angel--file-ends-with-load-file-suffix (filename base-extension)
  "Return FILENAME without suffix if it matches one of `load-file-rep-suffixes'.
BASE-EXTENSION is the extension before one of the `load-file-rep-suffixes'.
For example, .el in the case of .el and .el.gz files."
  (catch 'found
    (dolist (suffix load-file-rep-suffixes)
      (let ((full-suffix (concat base-extension suffix)))
        (when (string-suffix-p full-suffix filename)
          (let ((stripped-filename (substring filename 0 (- (length filename)
                                                            (length suffix)))))
            (when (string-suffix-p base-extension stripped-filename)
              (throw 'found stripped-filename))))))
    nil))

(defun compile-angel--native-compile (el-file)
  "Native compile EL-FILE."
  (when compile-angel--native-comp-available
    (let ((warning-minimum-level (if compile-angel-display-buffer
                                     :warning
                                   :error))
          (eln-file (comp-el-to-eln-filename el-file)))
      (when (and eln-file (file-newer-than-file-p el-file eln-file))
        (when compile-angel-verbose
          (message "[compile-angel] Native compile: %s" el-file))
        (native-compile-async el-file)))))

(defun compile-angel--byte-compile (el-file elc-file)
  "Byte compile EL-FILE into ELC-FILE."
  (let* ((elc-file-exists (file-exists-p elc-file)))
    (when (or (not elc-file-exists)
              (file-newer-than-file-p el-file elc-file))
      (if (not (file-writable-p elc-file))
          (when compile-angel-verbose
            (message "[compile-angel] Byte compile ignored (not writable): %s"
                     elc-file)
            nil)
        ;; Byte-compile
        (let ((byte-compile-verbose compile-angel-verbose)
              (warning-minimum-level (if compile-angel-display-buffer
                                         :warning
                                       :error))
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
              (message "[compile-angel] Ignore (no-byte-compile): %s"
                       el-file))
            ;; Return nil
            nil)

           ;; Ignore: Byte compilation error
           ((not byte-compile-result)
            (when compile-angel-verbose
              (message "[compile-angel] Compilation error: %s" el-file))
            ;; Return nil
            nil)

           ;; Success
           (byte-compile-result
            (when compile-angel-verbose
              (message "[compile-angel] Compile: %s" el-file))
            ;; Return t
            t)))))))

(defun compile-angel-compile-elisp (el-file)
  "Byte compile and Native compile the .el file EL-FILE."
  (when (or compile-angel-enable-byte-compile
            compile-angel-enable-native-compile)
    (let* ((el-file-sans-suffix (compile-angel--file-ends-with-load-file-suffix
                                 el-file ".el")))
      (cond
       ((not (file-exists-p el-file))
        (message "[compile-angel] Warning: The file does not exist: %s"
                 el-file))

       ((not el-file-sans-suffix)
        (message "[compile-angel] Warning: The file is not an .el file: %s"
                 el-file))

       (t
        ;; 1. Byte compile
        (let* ((elc-file (concat el-file-sans-suffix "c")))
          (when compile-angel-enable-byte-compile
            (compile-angel--byte-compile el-file elc-file)))

        ;; 2. Native compile
        (when compile-angel-enable-native-compile
          (compile-angel--native-compile el-file)))))))

(defun compile-angel--compile-current-buffer ()
  "Compile the current buffer."
  (when (derived-mode-p 'emacs-lisp-mode)
    (let ((el-file (buffer-file-name (buffer-base-buffer))))
      (compile-angel-compile-elisp el-file))))

(defun compile-angel--locate-library (library nosuffix)
  "Return the path to the LIBRARY el file.
Use `load-file-rep-suffixes' when NOSUFFIX is non-nil."
  (locate-file (substitute-in-file-name library)
               load-path
               (if nosuffix
                   load-file-rep-suffixes
                 (mapcar (lambda (s) (concat ".el" s))
                         load-file-rep-suffixes))))

(defun compile-angel--advice-before-require (feature
                                             &optional filename _noerror)
  "Recompile the library before `require'.
FEATURE and FILENAME are the same arguments as the `require' function."
  (when (or feature filename)
    (let* ((el-file (compile-angel--locate-library
                     (or filename (symbol-name feature))
                     nil)))
      (when (and el-file
                 (and compile-angel-on-load-mode-compile-once
                      (not (gethash el-file compile-angel--list-compiled-files))))
        (puthash el-file t compile-angel--list-compiled-files)
        (compile-angel-compile-elisp el-file)))))

(defun compile-angel--advice-before-load (el-file &optional _noerror
                                                  _nomessage nosuffix
                                                  _must-suffix)
  "Recompile before `load'.
EL-FILE and NOSUFFIX are the same arguments as `load'."
  (when el-file
    (let ((el-file (compile-angel--locate-library el-file
                                                  nosuffix)))
      (when (and el-file
                 (and compile-angel-on-load-mode-compile-once
                      (not (gethash el-file compile-angel--list-compiled-files))))
        (puthash el-file t compile-angel--list-compiled-files)
        (compile-angel-compile-elisp el-file)))))

(defun compile-angel--advice-before-autoload (_function
                                              file-or-feature
                                              &optional
                                              _docstring _interactive _type)
  "Recompile before `autoload'.
FILE-OR-FEATURE is the file or the feature."
  (when file-or-feature
    (let ((el-file (compile-angel--locate-library file-or-feature nil)))
      (when (and el-file
                 (and compile-angel-on-load-mode-compile-once
                      (not (gethash el-file compile-angel--list-compiled-files))))
        (puthash el-file t compile-angel--list-compiled-files)
        (compile-angel-compile-elisp el-file)))))

(defun compile-angel--check-native-comp-available ()
  "Determine if native compilation is available and set a flag accordingly.
It sets the flag: `compile-angel--native-comp-available'."
  (unless compile-angel--native-comp-available
    (when (and (featurep 'native-compile)
               (fboundp 'native-comp-available-p)
               (native-comp-available-p))
      (setq compile-angel--native-comp-available t))))

;;;###autoload
(define-minor-mode compile-angel-on-load-mode
  "Toggle `compile-angel-mode'."
  :global t
  :lighter " CompAngelLd"
  :group 'compile-angel
  (compile-angel--check-native-comp-available)
  (if compile-angel-on-load-mode
      (progn
        (advice-add 'autoload :before #'compile-angel--advice-before-autoload)
        (advice-add 'require :before #'compile-angel--advice-before-require)
        (advice-add 'load :before #'compile-angel--advice-before-load))
    (advice-remove 'autoload #'compile-angel--advice-before-autoload)
    (advice-remove 'require 'compile-angel--advice-before-require)
    (advice-remove 'load 'compile-angel--advice-before-load)))

;;;###autoload
(define-minor-mode compile-angel-on-save-mode
  "Toggle `compile-angel-mode'."
  :global t
  :lighter " CompAngelSv"
  :group 'compile-angel
  (compile-angel--check-native-comp-available)
  (if compile-angel-on-save-mode
      (add-hook 'after-save-hook #'compile-angel--compile-current-buffer)
    (remove-hook 'after-save-hook #'compile-angel--compile-current-buffer)))

(provide 'compile-angel)
;;; compile-angel.el ends here
