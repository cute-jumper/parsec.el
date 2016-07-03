;;; parsec.el --- Parser combinator library  -*- lexical-binding: t; -*-

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
  "Parser combinators for Emacs Lisp"
  :group 'development)

(defvar parsec-last-error-message nil)

(defun parsec-eof-or-char-as-string ()
  (let ((c (char-after)))
    (if c
        (char-to-string c)
      "`EOF'")))

(defun parsec-error-new (msg)
  (cons 'parsec-error msg))

(defun parsec-error-p (obj)
  (and (consp obj)
       (eq (car obj) 'parsec-error)))

(defalias 'parsec-error-str 'cdr)

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
             (parsec-error-new (if (stringp msg)
                                   msg
                                 (format "Found \"%s\" -> Expected \"%s\""
                                         found expected))))))))

(defun parsec-ch (ch)
  (let ((next-char (char-after)))
    (if (and (not (eobp))
             (char-equal next-char ch))
        (progn (forward-char 1)
               (char-to-string ch))
      (parsec-stop :expected (char-to-string ch)
                   :found (parsec-eof-or-char-as-string)))))

(defun parsec-any-ch ()
  (if (not (eobp))
      (prog1 (char-to-string (char-after))
        (forward-char))
    (parsec-stop :expected "any char"
                 :found (parsec-eof-or-char-as-string))))

(defun parsec-satisfy (pred)
  (let ((next-char (char-after)))
    (if (and (not (eobp))
             (funcall pred next-char))
        (progn (forward-char 1)
               (char-to-string ch))
      (parsec-stop :expected (format "%s" pred)
                   :found (parsec-eof-or-char-as-string)))))

(defun parsec-newline ()
  (parsec-ch ?\n))

(defun parsec-crlf ()
  (parsec-and (parsec-ch ?\r) (parsec-ch ?\n)))

(defun parsec-eol ()
  (parsec-or (parsec-newline) (parsec-crlf)))

(defun parsec-eob ()
  (unless (eobp)
    (parsec-stop :expected "`EOF'"
                 :found (parsec-eof-or-char-as-string))))

(defalias 'parsec-eof 'parsec-eob)

(defun parsec-re (regexp)
  (if (looking-at regexp)
      (progn (goto-char (match-end 0))
             (match-string 0))
    (parsec-stop :expected regexp
                 :found (parsec-eof-or-char-as-string))))

(defsubst parsec-str (str)
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
        (error-sym (make-symbol "err"))
        (error-str-list-sym (make-symbol "err-list")))
    `(let (,error-str-list-sym)
       (cl-loop named ,outer-sym for ,parser-sym in ',parsers
                finally (parsec-stop
                         :message
                         (replace-regexp-in-string
                          "\n" "\n\t"
                          (concat "None of the parsers succeeds:\n"
                                  (mapconcat #'identity ,error-str-list-sym "\n"))))
                do
                (parsec-protect-atom parsec-or
                  (parsec-start
                   (cl-return-from ,outer-sym
                     (parsec-eavesdrop-error ,error-sym
                         (parsec-make-atom parsec-or (eval ,parser-sym))
                       (push (parsec-error-str ,error-sym) ,error-str-list-sym)))))))))

(defalias 'parsec-and 'progn)

(defalias 'parsec-return 'prog1)

(defalias 'parsec-collect 'list)

(defun parsec-collect* (&rest args)
  (delq nil (apply #'parsec-collect args)))

(defmacro parsec-collect-as-string (&rest forms)
  `(parsec-list-to-string (parsec-collect ,@forms)))

(defmacro parsec-start (&rest forms)
  `(catch 'parsec-failed ,@forms))

(defalias 'parsec-parse 'parsec-start)

(defmacro parsec-try (&rest forms)
  (let ((orig-pt-sym (make-symbol "orig-pt"))
        (error-sym (make-symbol "err")))
    `(let ((,orig-pt-sym (point)))
       (parsec-eavesdrop-error ,error-sym
           (parsec-and ,@forms)
         (goto-char ,orig-pt-sym)))))

(defsubst parsec--atom-tag (name)
  (intern (format "parsec-failed-at-half-%s" name)))

(defmacro parsec-protect-atom (name parser)
  "This must be used together with `parsec-make-atom'."
  (declare (indent 1))
  (let ((tag (parsec--atom-tag name)))
    `(catch 'parsec-success
       (parsec-throw (catch ',tag
                       (throw 'parsec-success ,parser))))))

(defmacro parsec-make-atom (name parser)
  (let ((orig-pt-sym (make-symbol "orig-pt"))
        (error-sym (make-symbol "err"))
        (tag (parsec--atom-tag name)))
    `(let ((,orig-pt-sym (point)))
       (parsec-eavesdrop-error ,error-sym
           ,parser
         (message "equal=%s" (= (point) ,orig-pt-sym))
         (unless (= (point) ,orig-pt-sym)
           (throw ',tag ,error-sym))))))

(defmacro parsec-eavesdrop-error (error-sym parser &rest handler)
  (declare (indent 2))
  `(catch 'parsec-success
     (let ((,error-sym (parsec-start
                        (throw 'parsec-success ,parser))))
       ,@handler
       (parsec-throw ,error-sym))))

(defmacro parsec-with-error-message (msg &rest forms)
  (declare (indent 1))
  `(parsec-eavesdrop-error _
       (parsec-and ,@forms)
     (parsec-throw (parsec-error-new ,msg))))

(defmacro parsec-ensure (&rest forms)
  (let ((error-sym (make-symbol "err")))
    `(parsec-eavesdrop-error ,error-sym
         (parsec-and ,@forms)
       (error "%s" (parsec-error-str ,error-sym)))))

(defmacro parsec-ensure-with-error-message (msg &rest forms)
  (declare (indent 1))
  `(parsec-ensure
    (parsec-with-error-message ,msg
      (parsec-and ,@forms))))

(defmacro parsec-many (parser)
  (let ((res-sym (make-symbol "results"))
        (error-sym (make-symbol "err")))
    `(let (,res-sym)
       (parsec-protect-atom parsec-many
         (parsec-start
          (while (not (eobp))
            (push (parsec-make-atom parsec-many ,parser) ,res-sym))))
       (nreverse ,res-sym))))

(defmacro parsec-many1 (parser)
  `(cons ,parser (parsec-many ,parser)))

(defsubst parsec-list-to-string (l)
  (mapconcat #'identity l ""))

(defmacro parsec-many-as-string (parser)
  `(mapconcat #'identity (parsec-many ,parser) ""))

(defmacro parsec-many1-as-string (parser)
  `(mapconcat #'identity (parsec-many1 ,parser) ""))

(defmacro parsec-many-till (parser end &optional type)
  (let ((res-sym (make-symbol "results"))
        (end-res-sym (make-symbol "end-result")))
    `(let* (,res-sym
            (,end-res-sym (catch 'parsec-immediate-stop
                            (while t
                              (parsec-or (throw 'parsec-immediate-stop ,end)
                                         (push ,parser ,res-sym))))))
       (setq ,res-sym (nreverse ,res-sym))
       ,(cond
         ((eq type :both) `(cons ,res-sym ,end-res-sym))
         ((eq type :end) end-res-sym)
         (t res-sym)))))

(defmacro parsec-many-till-as-string (parser end &optional type)
  (let ((res-sym (make-symbol "results")))
    (cond
     ((eq type :both)
      `(let ((,res-sym (parsec-many-till ,parser ,end ,type)))
         (cons (parsec-list-to-string (car ,res-sym)) (cdr ,res-sym))))
     (t
      `(parsec-list-to-string (parsec-many-till ,parser ,end ,type))))))

(defmacro parsec-until (parser &optional type)
  `(parsec-many-till (parsec-any-ch) ,parser ,type))

(defmacro parsec-until-as-string (parser &optional type)
  `(parsec-many-till-as-string (parsec-any-ch) ,parser ,type))

(defmacro parsec-not-followed-by (parser)
  (let ((res-sym (make-symbol "results")))
    `(catch 'parsec-not-followed-by
       (let ((,res-sym
              (catch 'parsec-immediate-stop
                (throw 'parsec-not-followed-by
                       (parsec-or (throw 'parsec-immediate-stop (parsec-try ,parser))
                                  nil)))))
         (parsec-stop :message (format "Unexpected followed by: %s" ,res-sym))))))

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

(defmacro parsec-count (n parser)
  (let ((res-sym (make-symbol "results")))
    `(let (,res-sym)
       (dotimes (_ ,n ,res-sym)
         (push ,parser ,res-sym)))))

(defmacro parsec-count-as-string (n parser)
  `(parsec-list-to-string (parsec-count ,n ,parser)))

(defmacro parsec-option (opt &rest forms)
  `(parsec-or (parsec-and ,@forms) ,opt))

(defmacro parsec-optional (&rest forms)
  `(parsec-or (parsec-and ,@forms) nil))

(defmacro parsec-optional* (&rest forms)
  `(parsec-and (parsec-optional ,@forms) nil))

(defmacro parsec-query (parser &rest args)
  (let ((orig-pt-sym (make-symbol "orig-pt"))
        (res-sym (make-symbol "results")))
    `(let ((,orig-pt-sym (point))
           (,res-sym ,parser))
       ,(cond
         ((memq :beg args) orig-pt-sym)
         ((memq :end args) '(point))
         ((memq :nil args) nil)
         ((and (memq :group args)
               (consp parser)
               (eq (car parser) 'parsec-re))
          (let ((group
                 (cl-loop named outer for arg on args
                          when (eq (car arg) :group) do
                          (cl-return-from outer (cadr arg)))))
            (if (and group (integerp group))
                `(match-string ,group)
              (error "Invalid query :group %s" group))))
         (t res-sym)))))

(defsubst parsec-just (x) (cons 'Just x))

(defconst parsec-nothing 'Nothing)

(defun parsec-maybe-p (x)
  (or (eq x parsec-nothing)
      (and
       (consp x)
       (eq (car x) 'Just))))

(defun parsec-from-just (x)
  (and (consp x)
       (eq (car x) 'Just)
       (cdr x)))

(defmacro parsec-optional-maybe (&rest forms)
  (let ((res-sym (make-symbol "result")))
    `(let ((,res-sym (parsec-optional ,@forms)))
       (if ,res-sym
           (parsec-just ,res-sym)
         parsec-nothing))))

(defmacro parsec-with-input (input &rest parsers)
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,input)
     (goto-char (point-min))
     (parsec-start
      ,@parsers)))

(provide 'parsec)
;;; parsec.el ends here
