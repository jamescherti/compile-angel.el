;;; compile-angel.el --- Compile Emacs Lisp libraries automatically.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 0.9.9
;; URL: https://github.com/jamescherti/compile-angel.el
;; Keywords: convenience
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
;; Compile Emacs Lisp libraries automatically.

;;; Code:

(defgroup compile-angel nil
  "Non-nil if compile-angel mode mode is enabled."
  :group 'compile-angel
  :prefix "compile-angel-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/compile-angel.el"))

(defun compile-angel--message (&rest args)
  "Display a message with '[compile-angel]' prepended.
The message is formatted with the provided arguments ARGS."
  (apply #'message (concat "[compile-angel] " (car args)) (cdr args)))

(defun compile-angel--warning (&rest args)
  "Display a warning message with '[compile-angel] Warning: ' prepended.
The message is formatted with the provided arguments ARGS."
  (apply #'message (concat "[compile-angel] Warning: " (car args)) (cdr args)))

;;;###autoload
(define-minor-mode compile-angel-mode
  "Toggle `compile-angel-mode'."
  :global t
  :lighter " compile-angel"
  :group 'compile-angel
  (if compile-angel-mode
      t
    t))

(provide 'compile-angel)
;;; compile-angel.el ends here
