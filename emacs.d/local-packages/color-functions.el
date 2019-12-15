;;; color-functions.el --- functions to manipulate colors -*- lexical-binding: t -*-

;; Author: Gabriele Lana
;; Maintainer: Gabriele Lana
;; Version: 0.0.1
;; Package-Requires:
;; Homepage: http://github.com/gabrielelana
;; Keywords: color

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; functions to manipulate colors

;;; Code:

(defun color-to-rgb (color)
  (or (color-hex-to-rgb color)
      (color-name-to-rgb color)
      color))

(defun color-hex-to-rgb (color-hex)
  (when (string-match "#\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)" color-hex)
    (list
     (string-to-number (match-string 1 color-hex))
     (string-to-number (match-string 2 color-hex))
     (string-to-number (match-string 3 color-hex)))))

(defun color-name-to-rgb (color-name)
  (color-values color-name))

(defun color-rgb-to-hex (color-rgb)
  (format "#%02x%02x%02x"
          (nth 0 color-rgb)
          (nth 1 color-rgb)
          (nth 2 color-rgb)))

(color-rgb-to-hex '(20 20 20))

(defun color-rgb-lighter (color-rgb percent)
  (mapcar (lambda (c) (/ c (/ (- 100 percent) 100.0))) color-rgb))

(defun color-rgb-darker (color-rgb percent)
  (mapcar (lambda (c) (/ c (/ percent 100.0))) color-rgb))

(defun color-lighter (color percent)
  (color-rgb-to-hex (color-rgb-lighter (color-to-rgb color) percent)))

(defun color-darker (color percent)
  (color-rgb-to-hex (color-rgb-darker (color-to-rgb color) percent)))

(provide 'color-functions)

;;; color-functions.el ends here
