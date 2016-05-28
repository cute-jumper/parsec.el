;;; csv-parser.el --- Sample csv parser using parsec.el  -*- lexical-binding: t; -*-

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

;;

;;; Code:


(defun csv-file ()
  (pl-many (csv-line)))

(defun csv-line ()
  (prog1 (csv-cells)
    (csv-eol)))

(defun csv-eol ()
  (pl-or (pl-str "\n\r")
         (pl-str "\r\n")
         (pl-str "\n")
         (pl-str "\r")
         (pl-eob)))

(defun csv-cells ()
  (cons (csv-cell-content) (csv-remaining-cells)))

(defun csv-cell-content ()
  (pl-many-as-string (pl-re "[^,\n]")))

(defun csv-remaining-cells ()
  (pl-or (pl-and (pl-ch ?,) (csv-cells))
         nil))


(defun csv-file1 ()
  (pl-endby (csv-line1) (csv-eol)))

(defun csv-line1 ()
  (pl-sepby (csv-cell2) (pl-ch ?,)))

(defun csv-cell1 ()
  (pl-many-as-string (pl-re "[^,\r\n]")))

(defun csv-cell2 ()
  (pl-or (csv-quoted-cell) (pl-many (pl-re "[^,\n\r]"))))

(defun csv-quoted-cell ()
  (pl-ch ?\")
  (prog1 (pl-many (csv-quoted-char))
    (pl-failed (pl-ch ?\") "quote at end of cell")))

(defun csv-quoted-char ()
  (pl-or (pl-re "[^\"]")
         (pl-and (pl-str "\"\"")
                 "\"")))

(defun parse-csv1 (input)
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (csv-file1)))
(parse-csv1 "\"a,1,s,b,\r\nd,e,f")

(defun parse-csv (input)
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (csv-file)))

(parse-csv "a1s,b,\n\nd,e,f")
(with-temp-buffer
  (insert "a,b,")
  (goto-char (point-min))
  (csv-line))

(provide 'csv-parser)
;;; csv-parser.el ends here
