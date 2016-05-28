;;; parsec.el --- Emacs Lisp fork of Haskell's Parsec library  -*- lexical-binding: t; -*-

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

(require 'cl-lib)

(defgroup pl nil
  "Combinator parsing library for Emacs, similar to Haskell's Parsec"
  :group 'development)

(defun pl-ch (ch &rest args)
  (if (and (not (eobp))
           (char-equal (char-after) ch))
      (prog1
          (cond
           ((memq :nil args) nil)
           ((memq :beg args)
            (point))
           ((memq :end args)
            (1+ (point)))
           (t
            (char-to-string ch)))
        (forward-char 1))
    (throw 'failed nil)))

(defun pl-eob ()
  (unless (eobp)
    (throw 'failed nil)))

(defun pl-re (regexp &rest args)
  (if (looking-at regexp)
      (prog1
          (cond
           ((memq :nil args) nil)
           ((memq :beg args)
            (match-beginning 0))
           ((memq :end args)
            (match-end 0))
           ((memq :group args)
            (let ((group
                   (loop named outer for arg on args
                         when (eq (car arg) :group) do
                         (return-from outer (cadr arg)))))
              (if group
                  (match-string group)
                (error "Unexpected regexp :group %s" group))))
           (t
            (match-string 0)))
        (goto-char (match-end 0)))
    (throw 'failed nil)))

(defsubst pl-str (str &rest args)
  (pl-re (regexp-quote str)))

(defsubst pl-num (num &rest args)
  (pl-re (regexp-quote (number-to-string num))))

(defmacro pl-or (&rest parsers)
  (let ((outer-sym (make-symbol "outer"))
        (parser-sym (make-symbol "parser"))
        (error-sym (make-symbol "error-message")))
    `(loop named ,outer-sym for ,parser-sym in ',parsers
           finally (throw 'failed nil) do
           (when (setq ,error-sym
                       (catch 'failed
                         (return-from ,outer-sym (eval ,parser-sym))))
             (error ,error-sym)))))

(defalias 'pl-and 'progn)

(defmacro pl-failed (parser msg)
  `(pl-or ,parser
          (throw 'failed ,msg)))

(defmacro pl-try (&rest forms)
  `(catch 'failed ,@forms))

(defalias 'pl-parse 'pl-try)

(defmacro pl-until (parser &optional &key skip)
  (let ((error-sym (make-symbol "error-message")))
    `(let (,error-sym)
       (catch 'done
         (while (not (eobp))
           (when (setq ,error-sym
                       (catch 'failed
                         (throw 'done ,parser)))
             (error ,error-sym))
           ,(if skip
                `(,skip 1)
              `(forward-char 1)))))))

(defmacro pl-many (parser)
  (let ((res (make-symbol "results"))
        (msg (make-symbol "error-message")))
    `(let (,res ,msg)
       (when (setq ,msg (pl-try
                         (while (not (eobp))
                           (push ,parser ,res))))
         (error ,msg))
       (nreverse ,res))))

(defun pl-list-to-string (l)
  (mapconcat #'identity l ""))

(defmacro pl-many-as-string (parser)
  `(mapconcat #'identity (pl-many ,parser) ""))

(defmacro pl-endby (parser end)
  `(pl-many (prog1 ,parser
              ,end)))

(defmacro pl-sepby (parser separator)
  `(pl-or
    (cons ,parser (pl-many (pl-and ,separator ,parser)))
    nil))

(provide 'parsec)
;;; parsec.el ends here
