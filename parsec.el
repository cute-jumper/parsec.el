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

(defun pl-eob-or-char-as-string ()
  (let ((c (char-after)))
    (if c
        (char-to-string c)
      "`eob'")))

(defun pl-msg (msg)
  (cons 'pl-msg msg))

(defun pl-msg-p (msg)
  (and (consp msg)
       (eq (car msg) 'pl-msg)))

(defalias 'pl-msg-get 'cdr)

(defun pl-stop (&rest args)
  (throw 'failed
         (let ((msg (plist-get args :message))
               (expected (plist-get args :expected))
               (found (plist-get args :found)))
           (when (or (stringp msg)
                     (and (stringp expected)
                          (stringp found)))
             (pl-msg (if (stringp msg)
                         msg
                       (format "Found \"%s\" -> Expected \"%s\""
                               found expected)))))))

(defun pl-ch (ch &rest args)
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
      (pl-stop :expected (char-to-string ch)
               :found (pl-eob-or-char-as-string)))))

(defun pl-eob ()
  (unless (eobp)
    (pl-stop :expected "`eob'"
             :found (pl-eob-or-char-as-string))))

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
    (pl-stop :expected regexp
             :found (pl-eob-or-char-as-string))))

(defsubst pl-str (str &rest args)
  (pl-re (regexp-quote str)))

(defsubst pl-num (num &rest args)
  (pl-re (regexp-quote (number-to-string num))))

(defmacro pl-or (&rest parsers)
  (let ((outer-sym (make-symbol "outer"))
        (parser-sym (make-symbol "parser")))
    `(loop named ,outer-sym for ,parser-sym in ',parsers
           finally (pl-stop :message "None of the parsers succeeds") do
           (pl-try
            (return-from ,outer-sym (eval ,parser-sym))))))

(defalias 'pl-and 'progn)

(defalias 'pl-return 'prog1)

(defmacro pl-try (&rest forms)
  `(catch 'failed ,@forms))

(defmacro pl-try-with-message (msg &rest forms)
  (declare (indent 1))
  (let ((res-sym (make-symbol "result")))
    `(let ((,res-sym (pl-try ,@forms)))
       ,(if msg
            `(if (pl-msg-p ,res-sym)
                 (pl-msg ,msg)
               ,res-sym)
          `,res-sym))))

(defmacro pl-ensure-with-message (msg &rest forms)
  (declare (indent 1))
  (let* ((error-sym (make-symbol "err")))
    `(let (,error-sym)
       (if (pl-msg-p (setq ,error-sym
                           (pl-try-with-message ,msg ,@forms)))
           (error (pl-msg-get ,error-sym))
         ,error-sym))))

(defmacro pl-ensure (&rest forms)
  `(pl-ensure-with-message nil ,@forms))

(defalias 'pl-parse 'pl-try)

(defmacro pl-until (parser &optional &key skip)
  `(catch 'done
     (while (not (eobp))
       (pl-try
        (throw 'done ,parser))
       ,(if skip
            `(,skip 1)
          `(forward-char 1)))))

(defmacro pl-many (parser)
  (let ((res (make-symbol "results")))
    `(let (,res)
       (pl-try
           (while (not (eobp))
             (push ,parser ,res)))
       (nreverse ,res))))

(defmacro pl-many1 (parser)
  `(cons ,parser (pl-many ,parser)))

(defun pl-list-to-string (l)
  (mapconcat #'identity l ""))

(defmacro pl-many-as-string (parser)
  `(mapconcat #'identity (pl-many ,parser) ""))

(defmacro pl-many1-as-string (parser)
  `(mapconcat #'identity (pl-many1 ,parser) ""))

(defmacro pl-endby (parser end)
  `(pl-many (pl-return ,parser
              ,end)))

(defmacro pl-sepby (parser separator)
  `(pl-or
    (cons ,parser (pl-many (pl-and ,separator ,parser)))
    nil))


(defun pl-just (x) (cons 'Just x))

(defvar pl-nothing 'Nothing)

(defun pl-maybe-p (x)
  (or (eq x pl-nothing)
      (and
       (consp x)
       (eq (car x) 'Just))))

(defmacro pl-make-maybe (&rest body)
  (let ((res (make-symbol "result")))
    `(let ((,res (pl-try
                     ,@body)))
       (if (pl-msg-p ,res)
           pl-nothing
         (pl-just ,res)))))


(provide 'parsec)
;;; parsec.el ends here
