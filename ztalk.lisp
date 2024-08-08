(defpackage ztalk
  (:use :cl)
  (:shadow read compile eval))

(in-package ztalk)

(defmacro fn (params . body)
  `(lambda ,params ,@body))

(defmacro rfn (name params &rest body)
  `(labels ((,name ,params ,@body)) #',name))

;(defmacro afn (params &rest body)
;  `(rfn self ,params ,@body))

(defmacro w/uniq (names &rest body)
  `(let ,(mapcar (fn (x) (list x '(gensym))) names) ,@body))

(defmacro while (c &rest body)
  (w/uniq (f)
    `(funcall (rfn ,f () (when ,c ,@body (,f))))))

(defmacro awhile (c &rest body)
  (w/uniq (f)
    `(funcall (rfn ,f () (let ((it ,c)) (when it ,@body (,f)))))))

(defmacro after (x &rest xs)
  (w/uniq (gx)
    `(let ((,gx ,x))
       ,@xs
       ,gx)))

; reader

(defun skip-space ()
  (peek-char t nil nil)
  nil)

(defun read-char-if (p)
  (let ((c (peek-char nil nil nil)))
    (if (and c (funcall p c))
      (read-char))))

(defun sym-char-p (c)
  (or (alphanumericp c) (char= c #\-)))

(defun read-sym-char ()
  (read-char-if #'sym-char-p))

(defun zt-read ()
  (let ((c (peek-char t nil nil)))
    (if (char= c #\()
      (read-list)
      (read-atom))))

(defun read-atom ()
  (with-output-to-string (s)
    (awhile (read-sym-char)
      (write-char it s))))

(defun read-cdr ()
  (let ((c (peek-char t)))
    (if (char= c #\))
      nil
      (cons (zt-read) (read-cdr)))))

(defun read-list ()
  (read-char)
  (after
    (read-cdr)
    (read-char)))

(defvar *free-vars*)

(declaim (ftype (function (t t) t) compile))

(defun car-is (x y) (and (consp x) (eq (car x) y)))

(defun compile-var (x env)
  (unless (member x env)
    (push x *free-vars*))
  x)

(defun compile-set (x env)
  `(setq ,(compile-var (car x) env) ,(compile (cadr x) env)))

(defun compile-if (x env)
  (unless (null x)
    (let ((cc (compile (car x) env)))
      (if (null (cdr x))
        cc
        `(if ,cc
           ,(compile (cadr x) env)
           ,(compile-if (cddr x) env))))))

(defun compile-many (xs env)
  (mapcar (lambda (x) (compile x env)) xs))

(defun compile-fn (exp env)
  (let ((params (car exp)))
    `(lambda ,params ,@(compile-many (cdr exp) (append params env)))))

;(defun compile-list (xs env)
;  (let ((x (car xs)))
;    (if (and (symbolp x) (free x env))

(defun compile-funcall (xs env)
  (let ((cxs (compile-many xs env)))
    `(funcall ,(car cxs) ,@(cdr cxs))))

(defun compile (x env)
  (cond ((or (null x) (eq x t)) x)
        ((symbolp x)     (compile-var x env))
        ((car-is x 'set) (compile-set (cdr x) env))
        ((car-is x 'if)  (compile-if (cdr x) env))
        ((car-is x 'fn)  (compile-fn (cdr x) env))
        ((consp x)       (compile-funcall x env))
        (t               x)))

(defstruct eof)
(defvar *eof-object* (make-eof))
(defun eof-object? (x) (eq x *eof-object*))

(defun read ()
  (if (peek-char t *standard-input* nil)
    (cl:read)
    *eof-object*))

(defun eval (x)
  (let ((*free-vars* nil))
    (let ((cx (compile x nil)))
      (format t "free vars:: ~S~%" *free-vars*)
      (format t "Compiled: ~S~%" cx)
      (funcall (cl:compile nil `(lambda () (declare (special ,@*free-vars*)) ,cx))))))

(defun prompt ()
  (princ "ztalk> ")
  (finish-output))

(defun repl ()
  (prompt)
  (let ((x (read)))
    (unless (eof-object? x)
      (format t "Read:  ~S~%"  x)
      (format t "Value: ~S~%" (eval x))
      (repl))))

(repl)
