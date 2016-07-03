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

(defun parsec-error-new-2 (expected found)
  (parsec-error-new (format "Found \"%s\" -> Expected \"%s\""
                            found expected)))

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
             (if (stringp msg)
                 (parsec-error-new msg)
               (parsec-error-new-2 expected found)))))))

(defun parsec-ch (ch)
  "Parse a character CH."
  (let ((next-char (char-after)))
    (if (and (not (eobp))
             (char-equal next-char ch))
        (progn (forward-char 1)
               (char-to-string ch))
      (parsec-stop :expected (char-to-string ch)
                   :found (parsec-eof-or-char-as-string)))))

(defun parsec-any-ch ()
  "Parse any character."
  (if (not (eobp))
      (prog1 (char-to-string (char-after))
        (forward-char))
    (parsec-stop :expected "any char"
                 :found (parsec-eof-or-char-as-string))))

(defun parsec-satisfy (pred)
  "Parse any character that satisfies the predicate PRED."
  (let ((next-char (char-after)))
    (if (and (not (eobp))
             (funcall pred next-char))
        (progn (forward-char 1)
               (char-to-string next-char))
      (parsec-stop :expected (format "%s" pred)
                   :found (parsec-eof-or-char-as-string)))))

(defun parsec-newline ()
  "Parse a newline character \"\\n\"."
  (parsec-ch ?\n))

(defun parsec-crlf ()
  "Parse a carriage return (\'\\r\') followed by a newline \"\\n\"."
  (parsec-and (parsec-ch ?\r) (parsec-ch ?\n)))

(defun parsec-eol ()
  "Parse a newline or a CRLF and return \"\\n\"."
  (parsec-or (parsec-newline) (parsec-crlf)))

(defun parsec-eob ()
  "Indicate the end of file (buffer)."
  (unless (eobp)
    (parsec-stop :expected "`EOF'"
                 :found (parsec-eof-or-char-as-string))))

(defalias 'parsec-eof 'parsec-eob)

(defun parsec-eol-or-eof ()
  "Indicate either eol or eof."
  (parsec-or (parsec-eol) (parsec-eof)))

(defun parsec-re (regexp)
  "Parse the input matching the regular expression REGEXP."
  (if (looking-at regexp)
      (progn (goto-char (match-end 0))
             (match-string 0))
    (parsec-stop :expected regexp
                 :found (parsec-eof-or-char-as-string))))

(defun parsec-make-alternatives (chars)
  (let ((regex-head "")
        (regex-str "")
        (regex-end "")
        contains-caret-p)
    (dolist (c chars)
      (cond
       ((char-equal c ?\]) (setq regex-head "]"))
       ((char-equal c ?-) (setq regex-end "-"))
       ((char-equal c ?^) (setq contains-caret-p t))
       (t (setq regex-str (concat regex-str (char-to-string c))))))
    (when contains-caret-p
      (if (and
           (string-equal regex-end "-")
           (string-equal regex-head "")
           (string-equal regex-str ""))
          (setq regex-end "-^")
        (setq regex-str (concat regex-str "^"))))
    (concat regex-head regex-str regex-end)))

(defun parsec-one-of (&rest chars)
  "Succeed if the current character is in the supplied list of CHARS.
Return the parsed character.

>  (parsec-one-of ?a ?e ?i ?o ?u)

Note this function is just a wrapper of `parsec-re'.  For complicated use cases,
consider using `parsec-re' instead."
  (parsec-re (format "[%s]" (parsec-make-alternatives chars))))

(defun parsec-none-of (&rest chars)
  "Succeed if the current character not in the supplied list of CHARS.
Return the parsed character.

>  (parsec-none-of ?a ?e ?i ?o ?u)

Note this function is just a wrapper of `parsec-re'.  For complicated use cases,
consider using `parsec-re' instead."
  (parsec-re (format "[^%s]" (parsec-make-alternatives chars))))

(defsubst parsec-str (str)
  "Parse STR and only consume the input for an exact match.
Return the parsed string.

Note this function's behavior is different from the `string'
function of Haskll's Parsec.  Use `parsec-string' if you want the
same behavior as in Haskell."
  (parsec-re (regexp-quote str)))

(defsubst parsec-string (str)
  "Parse STR and consume the input even for a partial match.
Return the parsed string.

It is equivalent to calling `parsec-ch' multiples times so the
input will be consumed if the parser fails in the middle of the
STR.  This function has the same behavior as the `string' function
of Haskell's Parsec.  See also `parsec-str'."
  (mapc (lambda (c) (parsec-ch c)) str))

(defsubst parsec-num (num)
  "Parse the number NUM and return the parsed number as a string."
  (parsec-re (regexp-quote (number-to-string num))))

(defsubst parsec-letter ()
  "Parse any English letter."
  (parsec-re "[a-zA-Z]"))

(defsubst parsec-digit ()
  "Parse any digit."
  (parsec-re "[0-9]"))

(defmacro parsec-or (&rest parsers)
  "Try the PARSERS one by one.
If the current parser succeeds, return its results.  If the
current parser fails without consuming any input, try the next
parser if available.  This combinator fails if the current parser
fails after consuming some input or there is no more parsers."
  (let ((outer-sym (make-symbol "outer"))
        (parser-sym (make-symbol "parser"))
        (error-sym (make-symbol "err"))
        (error-str-list-sym (make-symbol "err-list")))
    `(let (,error-str-list-sym ,parser-sym ,error-sym)
       (catch 'parsec-parsec-or
         ,@(mapcar
            (lambda (parser)
              `(parsec-protect-atom parsec-or
                 (parsec-start
                  (throw 'parsec-parsec-or
                         (parsec-eavesdrop-error ,error-sym
                             (parsec-make-atom parsec-or ,parser)
                           (push (parsec-error-str ,error-sym) ,error-str-list-sym))))))
            parsers)
         (parsec-stop
          :message
          (replace-regexp-in-string
           "\n" "\n\t"
           (concat "None of the parsers succeeds:\n"
                   (mapconcat #'identity ,error-str-list-sym "\n"))))))))

(defalias 'parsec-and 'progn
  "Eval BODY sequentially and return the result of the last parser.
This combinator fails if one of the parsers fails.")

(defalias 'parsec-return 'prog1
  "Eval FIRST and BODY sequentially and return the results of the first parser.
This combinator fails if one of the parsers fails.")

(defalias 'parsec-collect 'list
  "Collect the results of all the parsers OBJECTS into a list.")

(defun parsec-collect* (&rest args)
  "Collect the non-nil results of all the parsers ARGS into a list."
  (delq nil (apply #'parsec-collect args)))

(defmacro parsec-collect-as-string (&rest forms)
  "Collect the results of all the parsers FORMS as a string."
  `(parsec-list-to-string (parsec-collect ,@forms)))

(defmacro parsec-start (&rest forms)
  "Eval the parsers FORMS and return the results or a `parsec-error'.
This combinator should be used at the top level as the entry
point of your parsing program."
  `(catch 'parsec-failed ,@forms))

(defalias 'parsec-parse 'parsec-start)

(defmacro parsec-try (parser)
  "Try PARSER, and pretend that no input is consumed when an error occurs."
  (let ((orig-pt-sym (make-symbol "orig-pt"))
        (error-sym (make-symbol "err")))
    `(let ((,orig-pt-sym (point)))
       (parsec-eavesdrop-error ,error-sym
           (parsec-and ,parser)
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
  "Use MSG as the error message if an error occurs when Evaling the FORMS."
  (declare (indent 1))
  `(parsec-eavesdrop-error _
       (parsec-and ,@forms)
     (parsec-throw (parsec-error-new ,msg))))

(defmacro parsec-ensure (&rest forms)
  "Exit the program immediately if FORMS fail."
  (let ((error-sym (make-symbol "err")))
    `(parsec-eavesdrop-error ,error-sym
         (parsec-and ,@forms)
       (error "%s" (parsec-error-str ,error-sym)))))

(defmacro parsec-ensure-with-error-message (msg &rest forms)
  "Exit the program immediately with MSG if FORMS fail."
  (declare (indent 1))
  `(parsec-ensure
    (parsec-with-error-message ,msg
      (parsec-and ,@forms))))

(defmacro parsec-many (parser)
  "Apply the PARSER zero or more times and return a list of the results."
  (let ((res-sym (make-symbol "results"))
        (error-sym (make-symbol "err")))
    `(let (,res-sym)
       (parsec-protect-atom parsec-many
         (parsec-start
          (while (not (eobp))
            (push (parsec-make-atom parsec-many ,parser) ,res-sym))))
       (nreverse ,res-sym))))

(defmacro parsec-many1 (parser)
  "Apply the PARSER one or more times and return a list of the results."
  `(cons ,parser (parsec-many ,parser)))

(defsubst parsec-list-to-string (l)
  (if (stringp l)
      l
    (mapconcat #'identity l "")))

(defmacro parsec-many-as-string (parser)
  "Apply the PARSER zero or more times and return the results as a string."
  `(mapconcat #'identity (parsec-many ,parser) ""))

(defmacro parsec-many1-as-string (parser)
  "Apply the PARSER one or more times and return the results as a string."
  `(mapconcat #'identity (parsec-many1 ,parser) ""))

(defmacro parsec-many-till (parser end &optional type)
  "Apply PARSER zero or more times until END succeeds.
The return value is determined by TYPE.  If TYPE is `:both', return
the cons `(many . end)'.  If TYPE is `:end', return the result of END.
In other cases, return the result of PARSER.

Used to scan comments:

> (parsec-and
>   (parsec-str \"<--\")
>   (parsec-many-till (parsec-any-ch) (parsec-str \"-->\")))"

  (let ((res-sym (make-symbol "results"))
        (end-res-sym (make-symbol "end-result")))
    `(let ((,res-sym nil) ,end-res-sym)
       (setq ,end-res-sym
             (catch 'parsec-immediate-stop
               (while t
                 (parsec-or (throw 'parsec-immediate-stop ,end)
                            (push ,parser ,res-sym)))))
       (setq ,res-sym (nreverse ,res-sym))
       ,(cond
         ((eq type :both) `(cons ,res-sym ,end-res-sym))
         ((eq type :end) end-res-sym)
         (t res-sym)))))

(defmacro parsec-many-till-as-string (parser end &optional type)
  "Apply PARSER zero or more times until END succeeds.
Return the result of PARSER or END as a string.  TYPE has the same
meaning as `parsec-many-till'."
  (let ((res-sym (make-symbol "results")))
    (cond
     ((eq type :both)
      `(let ((,res-sym (parsec-many-till ,parser ,end ,type)))
         (cons (parsec-list-to-string (car ,res-sym))
               (parsec-list-to-string (cdr ,res-sym)))))
     (t
      `(parsec-list-to-string (parsec-many-till ,parser ,end ,type))))))

(defmacro parsec-until (parser &optional type)
  "Parse any characters until PARSER succeeds.
TYPE has the same meaning as `parsec-many-till'."
  `(parsec-many-till (parsec-any-ch) ,parser ,type))

(defmacro parsec-until-as-string (parser &optional type)
  "Parse any characters until PARSER succeeds.
Return the result of either part as a string.  TYPE has the same
meaning as `parsec-many-till'."
  `(parsec-many-till-as-string (parsec-any-ch) ,parser ,type))

(defmacro parsec-not-followed-by (parser)
  "Succeed only when PARSER fails.  Consume no input."
  (let ((res-sym (make-symbol "results")))
    `(catch 'parsec-not-followed-by
       (let ((,res-sym
              (catch 'parsec-immediate-stop
                (throw 'parsec-not-followed-by
                       (parsec-or (throw 'parsec-immediate-stop (parsec-try ,parser))
                                  nil)))))
         (parsec-stop :message (format "Unexpected followed by: %s" ,res-sym))))))

(defmacro parsec-endby (parser end)
  "Parse zero or more occurrences of PARSER, separated and ended by END.
Return a list of values returned by PARSER."
  `(parsec-many (parsec-return ,parser
                  ,end)))

(defmacro parsec-sepby (parser separator)
  "Parse zero or more occurrences of PARSER, separated by SEPARATOR.
Return a list of values returned by PARSER."
  `(parsec-or
    (cons ,parser (parsec-many (parsec-and ,separator ,parser)))
    nil))

(defmacro parsec-between (open close parser)
  "Parse OPEN, followed by PARSER and CLOSE.
Return the value returned by PARSER."
  `(parsec-and
     ,open
     (parsec-return ,parser
       ,close)))

(defmacro parsec-count (n parser)
  "Parse N occurrences of PARSER.
Return a list of N values returned by PARSER."
  (let ((res-sym (make-symbol "results")))
    `(let (,res-sym)
       (dotimes (_ ,n ,res-sym)
         (push ,parser ,res-sym)))))

(defmacro parsec-count-as-string (n parser)
  "Parse N occurrences of PARSER.
Return the N values returned by PARSER as a string."
  `(parsec-list-to-string (parsec-count ,n ,parser)))

(defmacro parsec-option (opt parser)
  "Try to apply PARSER and return OPT if PARSER fails without comsuming input."
  `(parsec-or ,parser ,opt))

(defmacro parsec-optional (parser)
  "Apply PARSER zero or one time.  Fail if PARSER fails after consuming input.
Return the result of PARSER or nil.

Note this combinator doesn't discard the result of PARSER so it is
different from the `optional' function of Haskell's Parsec.  If
you want the Haskell's behavior, use `parsec-optional*'."
  `(parsec-or ,parser nil))

(defmacro parsec-optional* (parser)
  "Apply PARSER zero or one time and discard the result.
Fail if PARSER fails after consuming input.

This combinator has the same behavior as the `optional' function of
Haskell's Parsec."
  `(parsec-and ,parser nil))

(defmacro parsec-query (parser &rest args)
  "Get an alternative return value of the PARSER specified by the ARGS.

The args can be in the following forms:

    :beg      --> return the point before applying the PARSER
    :end      --> return the point after applying the PARSER
    :nil      --> return nil
    :groups N --> return Nth group for `parsec-re'."
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

(defun parsec-from-maybe (x)
  "Retrieve the value from Maybe monad X.
If X is `(Just . p)', return p. Otherwise return nil."
  (and (consp x)
       (eq (car x) 'Just)
       (cdr x)))

(defmacro parsec-optional-maybe (parser)
  "Apply PARSER zero or one time and return the value in a Maybe monad.
If PARSER fails without consuming any input, return `parsec-nothing'.
Otherwise, return `(Just . p)' where p is the result of PARSER."
  (let ((res-sym (make-symbol "result")))
    `(let ((,res-sym (parsec-optional ,parser)))
       (if ,res-sym
           (parsec-just ,res-sym)
         parsec-nothing))))

(defmacro parsec-with-input (input &rest parsers)
  "With INPUT, start parsing by applying PARSERS sequentially."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,input)
     (goto-char (point-min))
     (parsec-start
      ,@parsers)))

(provide 'parsec)
;;; parsec.el ends here
