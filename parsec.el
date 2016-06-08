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

(defgroup parsec nil
  "Combinator parsing library for Emacs, similar to Haskell's Parsec"
  :group 'development)

(defun parsec-eob-or-char-as-string ()
  (let ((c (char-after)))
    (if c
        (char-to-string c)
      "`eob'")))

(defun parsec-msg (msg)
  (cons 'parsec-msg msg))

(defun parsec-msg-p (msg)
  (and (consp msg)
       (eq (car msg) 'parsec-msg)))

(defalias 'parsec-msg-get 'cdr)

(defun parsec-stop (&rest args)
  (throw 'failed
         (let ((msg (plist-get args :message))
               (expected (plist-get args :expected))
               (found (plist-get args :found)))
           (when (or (stringp msg)
                     (and (stringp expected)
                          (stringp found)))
             (parsec-msg (if (stringp msg)
                             msg
                           (format "Found \"%s\" -> Expected \"%s\""
                                   found expected)))))))

(defun parsec-ch (ch &rest args)
  (let ((next-char (char-after)))
    (if (and (not (eobp))
             (char-equal next-char ch))
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
      (parsec-stop :expected (char-to-string ch)
                   :found (parsec-eob-or-char-as-string)))))

(defun parsec-eob ()
  (unless (eobp)
    (parsec-stop :expected "`eob'"
                 :found (parsec-eob-or-char-as-string))))

(defun parsec-re (regexp &rest args)
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
                   (cl-loop named outer for arg on args
                            when (eq (car arg) :group) do
                            (return-from outer (cadr arg)))))
              (if group
                  (match-string group)
                (error "Unexpected regexp :group %s" group))))
           (t
            (match-string 0)))
        (goto-char (match-end 0)))
    (parsec-stop :expected regexp
                 :found (parsec-eob-or-char-as-string))))

(defsubst parsec-str (str &rest args)
  (parsec-re (regexp-quote str)))

(defsubst parsec-num (num &rest args)
  (parsec-re (regexp-quote (number-to-string num))))

(defmacro parsec-or (&rest parsers)
  (let ((outer-sym (make-symbol "outer"))
        (parser-sym (make-symbol "parser")))
    `(cl-loop named ,outer-sym for ,parser-sym in ',parsers
              finally (parsec-stop :message "None of the parsers succeeds") do
              (parsec-try
               (return-from ,outer-sym (eval ,parser-sym))))))

(defalias 'parsec-and 'progn)

(defalias 'parsec-return 'prog1)

(defmacro parsec-try (&rest forms)
  `(catch 'failed ,@forms))

(defmacro parsec-try-with-message (msg &rest forms)
  (declare (indent 1))
  (let ((res-sym (make-symbol "result")))
    `(let ((,res-sym (parsec-try ,@forms)))
       ,(if msg
            `(if (parsec-msg-p ,res-sym)
                 (parsec-msg ,msg)
               ,res-sym)
          `,res-sym))))

(defmacro parsec-ensure-with-message (msg &rest forms)
  (declare (indent 1))
  (let* ((error-sym (make-symbol "err")))
    `(let (,error-sym)
       (if (parsec-msg-p (setq ,error-sym
                               (parsec-try-with-message ,msg ,@forms)))
           (error (parsec-msg-get ,error-sym))
         ,error-sym))))

(defmacro parsec-ensure (&rest forms)
  `(parsec-ensure-with-message nil ,@forms))

(defalias 'parsec-parse 'parsec-try)

(defmacro parsec-until (parser &optional &key skip)
  `(catch 'done
     (while (not (eobp))
       (parsec-try
        (throw 'done ,parser))
       ,(if skip
            `(,skip 1)
          `(forward-char 1)))))

(defmacro parsec-many (parser)
  (let ((res (make-symbol "results")))
    `(let (,res)
       (parsec-try
        (while (not (eobp))
          (push ,parser ,res)))
       (nreverse ,res))))

(defmacro parsec-many1 (parser)
  `(cons ,parser (parsec-many ,parser)))

(defun parsec-list-to-string (l)
  (mapconcat #'identity l ""))

(defmacro parsec-many-as-string (parser)
  `(mapconcat #'identity (parsec-many ,parser) ""))

(defmacro parsec-many1-as-string (parser)
  `(mapconcat #'identity (parsec-many1 ,parser) ""))

(defmacro parsec-endby (parser end)
  `(parsec-many (parsec-return ,parser
                  ,end)))

(defmacro parsec-sepby (parser separator)
  `(parsec-or
    (cons ,parser (parsec-many (parsec-and ,separator ,parser)))
    nil))

(defun parsec-just (x) (cons 'Just x))

(defvar parsec-nothing 'Nothing)

(defun parsec-maybe-p (x)
  (or (eq x parsec-nothing)
      (and
       (consp x)
       (eq (car x) 'Just))))

(defmacro parsec-make-maybe (&rest body)
  (let ((res (make-symbol "result")))
    `(let ((,res (parsec-try
                  ,@body)))
       (if (parsec-msg-p ,res)
           parsec-nothing
         (parsec-just ,res)))))

(provide 'parsec)
;;; parsec.el ends here
