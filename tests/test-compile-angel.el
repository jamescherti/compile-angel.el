;;; test-compile-angel.el --- Test compile-angel -*- lexical-binding: t; -*-

;; Copyright (C) 2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.6
;; URL: https://github.com/jamescherti/compile-angel.el
;; Keywords: abbrev
;; Package-Requires: ((emacs "24.1"))
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
;; Test compile-angel.

;;; Code:

(require 'ert)
(require 'compile-angel)

;; (with-no-warnings
;;   (when (require 'undercover nil t)
;;     (undercover "compile-angel.el"
;;                 (:report-file ".coverage")
;;                 (:report-format 'text)
;;                 (:send-report nil))))

;;; Variables

(defvar test-compile-angel--test-dir (expand-file-name "~/.test-compile-angel"))
(defvar test-compile-angel--el-file
  (expand-file-name "simple-el-file.el" test-compile-angel--test-dir))
(defvar test-compile-angel--el-file-symbol 'simple-el-file)
(defvar test-compile-angel--elc-file
  (expand-file-name "simple-el-file.elc" test-compile-angel--test-dir))
(defvar test-compile-angel--load-path (copy-sequence load-path))

;;; Helper functions

(defun test-compile-angel--init-default-vars ()
  (setq compile-angel-verbose t)
  (clrhash compile-angel--no-byte-compile-files-list)
  (clrhash compile-angel--list-jit-native-compiled-files)
  (clrhash compile-angel--list-compiled-files)
  (clrhash compile-angel--list-compiled-features)
  (setq compile-angel-excluded-files '("/random-thing.el"))
  (setq compile-angel-excluded-files-regexps '("random-thing.el"))
  (setq compile-angel-on-load-mode-compile-once t)
  (setq compile-angel-enable-byte-compile t)
  (setq compile-angel-enable-native-compile t)
  (setq compile-angel-predicate-function nil)

  ;; TODO test the following
  (setq compile-angel-on-load-advise-load t)
  (setq compile-angel-on-load-advise-require t)
  (setq compile-angel-on-load-hook-after-load-functions t)
  (setq compile-angel-on-load-compile-features t)
  (setq compile-angel-on-load-compile-load-history t))

(defun test-compile-angel--init ()
  (test-compile-angel--init-default-vars)
  (compile-angel-on-load-mode -1)
  (compile-angel-on-save-mode -1)
  (compile-angel-on-save-local-mode -1)

  (when (featurep 'simple-el-file)
    (unload-feature 'simple-el-file))
  (delete-file test-compile-angel--elc-file)

  (setq load-path (copy-sequence test-compile-angel--load-path))
  (add-to-list 'load-path test-compile-angel--test-dir)

  (make-directory test-compile-angel--test-dir t)
  (with-temp-buffer
    (insert (concat
             ";;; simple-el-file.el --- Test -*- lexical-binding: t; -*-\n"
             "\n"
             "(defun simple-el-file-func () (message \"Hello world\"))\n\n"
             "(provide 'simple-el-file)\n"
             ";;; test-compile-angel.el ends here"))
    (let ((coding-system-for-write 'utf-8)
          (write-region-annotate-functions nil)
          (write-region-post-annotation-function nil))
      (write-region (point-min) (point-max)
                    test-compile-angel--el-file nil 'silent))))

(defmacro test-compile-angel--test-all-macro (init-func &rest body)
  "Macro to test compile-angel behavior on file loading."
  `(progn
     ;; Test load
     (funcall ,init-func)
     (compile-angel-on-load-mode 1)
     ;; (should-not (file-exists-p test-compile-angel--elc-file))
     (require test-compile-angel--el-file-symbol)
     (should (fboundp 'simple-el-file-func))
     ,@body

     ;; Test require
     (funcall ,init-func)
     (compile-angel-on-load-mode 1)
     ;; (should-not (file-exists-p test-compile-angel--elc-file))
     (load test-compile-angel--el-file)
     (should (fboundp 'simple-el-file-func))
     ,@body))

;;; Unit-tests

(ert-deftest test-compile-angel--test-compile-once ()
  ;; Test 1
  (test-compile-angel--test-all-macro
   #'test-compile-angel--init
   (should (file-exists-p test-compile-angel--elc-file)))

  ;; Test 2: load: It shouldn't compile
  (delete-file test-compile-angel--elc-file)
  (load test-compile-angel--el-file)
  (should-not (file-exists-p test-compile-angel--elc-file))

  ;; Test 3: require: It shouldn't compile
  (delete-file test-compile-angel--elc-file)
  (require test-compile-angel--el-file-symbol)
  (should-not (file-exists-p test-compile-angel--elc-file)))

(ert-deftest test-compile-angel--test-compile-twice ()
  ;; Test 1
  (test-compile-angel--test-all-macro
   #'test-compile-angel--init
   (should (file-exists-p test-compile-angel--elc-file)))

  (setq compile-angel-on-load-mode-compile-once nil)

  ;; Test 2: load: It shouldn't compile
  (delete-file test-compile-angel--elc-file)
  (load test-compile-angel--el-file)
  (should (file-exists-p test-compile-angel--elc-file))

  ;; Test 3: require: It shouldn't compile
  (delete-file test-compile-angel--elc-file)
  (require test-compile-angel--el-file-symbol)
  (should (file-exists-p test-compile-angel--elc-file)))

(ert-deftest test-compile-angel--disable-byte-compilation ()
  (test-compile-angel--test-all-macro
   #'(lambda()
       (test-compile-angel--init)
       (setq compile-angel-enable-byte-compile nil))
   (should-not (file-exists-p test-compile-angel--elc-file))))

(ert-deftest test-compile-angel--excluded-files ()
  (test-compile-angel--test-all-macro
   #'(lambda()
       (test-compile-angel--init)
       (setq compile-angel-excluded-files (list "/random-thing.el")))
   (should (file-exists-p test-compile-angel--elc-file)))

  (test-compile-angel--test-all-macro
   #'(lambda()
       (test-compile-angel--init)
       (setq compile-angel-excluded-files
             (list (concat "/" (file-name-nondirectory
                                test-compile-angel--el-file)))))
   (should-not (file-exists-p test-compile-angel--elc-file))))

(ert-deftest test-compile-angel--excluded-regexps ()
  (test-compile-angel--test-all-macro
   #'(lambda()
       (test-compile-angel--init)
       (setq compile-angel-excluded-files-regexps (list "random-thing.el")))
   (should (file-exists-p test-compile-angel--elc-file)))

  (test-compile-angel--test-all-macro
   #'(lambda()
       (test-compile-angel--init)
       (setq compile-angel-excluded-files-regexps
             (list (file-name-nondirectory test-compile-angel--el-file))))
   (should-not (file-exists-p test-compile-angel--elc-file))))

(ert-deftest test-compile-angel--predicate ()
  (test-compile-angel--test-all-macro
   #'(lambda()
       (test-compile-angel--init)
       (setq compile-angel-predicate-function #'(lambda(&rest _) t)))
   (should (file-exists-p test-compile-angel--elc-file)))

  (test-compile-angel--test-all-macro
   #'(lambda()
       (test-compile-angel--init)
       (setq compile-angel-predicate-function #'(lambda(&rest _) nil)))
   (should-not (file-exists-p test-compile-angel--elc-file))))

(provide 'test-compile-angel)
;;; test-compile-angel.el ends here
