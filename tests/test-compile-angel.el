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

(with-no-warnings
  (when (require 'undercover nil t)
    (undercover "compile-angel.el"
                (:report-file ".coverage")
                (:report-format 'text)
                (:send-report nil))))

(ert-deftest test-compile-angel ()
  "Test compile-angel."
  (interactive)
  (should t))

(provide 'test-compile-angel)
;;; test-compile-angel.el ends here
