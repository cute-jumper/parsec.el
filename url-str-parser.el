;;; url-str-parser.el --- URL-encoded query string parser using parsec.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Ref: http://book.realworldhaskell.org/read/using-parsec.html

;;; Code:


(defun url-str-query ()
  (pl-sepby (url-str-pair) (pl-ch ?&)))

(defun url-str-pair ()
  (cons
   (pl-many1-as-string (url-str-char))
   (pl-make-maybe (pl-and (pl-ch ?=) (pl-many-as-string (url-str-char))))))

(defun url-str-char ()
  (pl-or (pl-re "[a-zA-z0-9$_.!*'(),-]")
         (pl-and (pl-ch ?+) " ")
         (url-str-hex)))

(defun url-str-hex ()
  (pl-and
    (pl-ch ?%)
    (format "%c"
            (string-to-number (format "%s%s"
                                      (pl-re "[0-9a-zA-z]")
                                      (pl-re "[0-9a-zA-z]"))
                              16))))

(defun url-str-parse (input)
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (url-str-query)))

(url-str-parse "foo=bar&a%21=b+c")
(url-str-parse "foo=&a%21=b+c")
(url-str-parse "foo&a%21=b+c")

(provide 'url-str-parser)
;;; url-str-parser.el ends here
