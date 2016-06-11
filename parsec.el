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

(defvar parsec-last-error-message nil)

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

(defsubst parsec-throw (msg)
  (throw 'parsec-failed msg))

(defun parsec-stop (&rest args)
  (parsec-throw
   (setq parsec-last-error-message
         (let ((msg (plist-get args :message))
               (expected (plist-get args :expected))
               (found (plist-get args :found)))
           (when (or (stringp msg)
                     (and (stringp expected)
                          (stringp found)))
             (parsec-msg (if (stringp msg)
                             msg
                           (format "Found \"%s\" -> Expected \"%s\""
                                   found expected))))))))

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

(defun parsec-satisfy (pred)
  (let ((next-char (char-after)))
    (if (and (not (eobp))
             (funcall pred next-char))
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
      (parsec-stop :expected (format "%s" pred)
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

(defsubst parsec-letter ()
  (parsec-re "[a-zA-Z]"))

(defsubst parsec-digit ()
  (parsec-re "[0-9]"))

(defmacro parsec-or (&rest parsers)
  (let ((outer-sym (make-symbol "outer"))
        (parser-sym (make-symbol "parser"))
        (msg-sym (make-symbol "msg"))
        (error-list-sym (make-symbol "err-list")))
    `(let (,error-list-sym)
       (cl-loop named ,outer-sym for ,parser-sym in ',parsers
                finally (parsec-stop
                         :message
                         (replace-regexp-in-string
                          "\n" "\n\t"
                          (concat "None of the parsers succeeds:\n"
                                  (mapconcat #'identity ,error-list-sym "\n"))))
                do
                (parsec--if-catch-and-forward 'parsec-failed-at-half
                  (parsec-start
                   (cl-return-from ,outer-sym
                     (parsec--if-handle-and-forward ,msg-sym
                         (parsec-as-single (eval ,parser-sym))
                       (push (parsec-msg-get ,msg-sym) ,error-list-sym)))))))))

(defalias 'parsec-and 'progn)

(defalias 'parsec-return 'prog1)

(defmacro parsec-start (&rest forms)
  `(catch 'parsec-failed ,@forms))

(defalias 'parsec-parse 'parsec-start)

(defmacro parsec-try (&rest forms)
  (let ((orig-pt-sym (make-symbol "orig-pt"))
        (msg-sym (make-symbol "msg")))
    `(let ((,orig-pt-sym (point)))
       (parsec--if-handle-and-forward ,msg-sym
           (parsec-and ,@forms)
         (goto-char ,orig-pt-sym)))))

(defmacro parsec--if-catch (tag body &rest forms)
  (declare (indent 2))
  `(catch 'parsec-success
     (catch ,tag
       (throw 'parsec-success ,body))
     ,@forms))

(defmacro parsec--if-catch-and-forward (tag parser)
  (declare (indent 1))
  (let ((error-sym (make-symbol "err")))
    `(catch 'parsec-success
       (parsec-throw (catch ,tag
                       (throw 'parsec-success ,parser))))))

(defmacro parsec--if-handle-and-forward (error-sym parser &rest handler)
  (declare (indent 2))
  `(catch 'parsec-success
     (let ((,error-sym (parsec-start
                        (throw 'parsec-success ,parser))))
       ,@handler
       (parsec-throw ,error-sym))))

(defmacro parsec-with-message (msg &rest forms)
  (declare (indent 1))
  `(parsec--if-catch 'parsec-failed
       (parsec-and ,@forms)
     (parsec-throw (parsec-msg msg))))

(defmacro parsec-ensure (&rest forms)
  `(parsec--if-handle-and-forward msg
       (parsec-and ,@forms)
     (error "%s" (parsec-msg-get msg))))

(defmacro parsec-ensure-with-message (msg &rest forms)
  (declare (indent 1))
  `(parsec-ensure
    (parsec-with-message msg
      (parsec-and ,@forms))))

(cl-defmacro parsec-until (parser &optional &key skip)
  `(catch 'done
     (while (not (eobp))
       (parsec-start
        (throw 'done ,parser))
       ,(if skip
            `(,skip 1)
          `(forward-char 1)))))

(defmacro parsec-as-single (parser)
  (let ((orig-pt-sym (make-symbol "orig-pt"))
        (error-sym (make-symbol "err")))
    `(let ((,orig-pt-sym (point)))
       (parsec--if-handle-and-forward ,error-sym
           ,parser
         (unless (= (point) ,orig-pt-sym)
           (throw 'parsec-failed-at-half ,error-sym))))))

(defmacro parsec-many (parser)
  (let ((res (make-symbol "results"))
        (error-sym (make-symbol "err")))
    `(let (,res)
       (parsec--if-catch-and-forward 'parsec-failed-at-half
         (parsec-start
          (while (not (eobp))
            (push (parsec-as-single ,parser) ,res))))
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

(defmacro parsec-between (open close parser)
  `(parsec-and
     ,open
     (parsec-return ,parser
       ,close)))

(defun parsec-just (x) (cons 'Just x))

(defvar parsec-nothing 'Nothing)

(defun parsec-maybe-p (x)
  (or (eq x parsec-nothing)
      (and
       (consp x)
       (eq (car x) 'Just))))

(defmacro parsec-make-maybe (&rest body)
  (let ((res (make-symbol "result")))
    `(let ((,res (parsec-start
                  ,@body)))
       (if (parsec-msg-p ,res)
           parsec-nothing
         (parsec-just ,res)))))

(defmacro parsec-do-parse (input &rest parsers)
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,input)
     (goto-char (point-min))
     (parsec-start
      ,@parsers)))

(provide 'parsec)
;;; parsec.el ends here
