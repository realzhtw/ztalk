(defpackage ztalk
  (:use :cl)
  (:shadow read compile eval))

(in-package ztalk)

(defmacro fn (params . body)
  `(lambda ,params ,@body))

(defmacro rfn (name params &rest body)
  `(labels ((,name ,params ,@body)) #',name))

;(defmacro aif (c then &rest else)
;  `(let ((it ,c))
;     (if it ,then ,@else)))

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

(defun gensyms (n)
  (unless (zerop n)
    (cons (gensym) (gensyms (- n 1)))))

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

(defun read-atom ()
  (with-output-to-string (s)
    (awhile (read-sym-char)
      (write-char it s))))

(defun zt-read ()
  (let ((c (peek-char t nil nil)))
    (if (char= c #\()
      (read-list)
      (read-atom))))

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

(declaim (ftype (function (t t t) t) compile))

(defun car-is (x y) (and (consp x) (eq (car x) y)))

(defun declare-var (x env)
  (unless (member x env)
    (push x *free-vars*))
  x)

(defun variable-p (x)
  (and (symbolp x)
       (not (null x))
       (not (eq x t))))

(defun compile-var (k x env)
  (declare-var x env)
    `(funcall ,k ,x))

(defun compile-value (k x) `(funcall ,k ,x))

(defun compile-set (k x env)
  (destructuring-bind (var exp) x
    (declare-var var env)
    (cond ((atom exp) (if (variable-p exp) (declare-var exp env))
                      `(funcall ,k (setq ,var ,exp)))
          (t          (w/uniq (val)
                        (compile `(lambda (,val) (funcall ,k (setq ,var ,val)))
                                 exp env))))))

(defun compile-if (k x env)
  (cond ((null x)          `(funcall ,k nil))
        ((null (cdr x))    (compile k (car x) env))
        ((not (symbolp k)) (w/uniq (k2) `(let ((,k2 ,k)) ,(compile-if k2 x env))))
        ((atom (car x))    `(if ,(car x)
                                ,(compile k (cadr x) env)
                                ,(compile-if k (cddr x) env)))
        (t                 (w/uniq (val)
                             (compile `(lambda (,val)
                                         (if ,val
                                             ,(compile k (cadr x) env)
                                             ,(compile-if k (cddr x) env)))
                                      (car x) env)))))
      
(defun compile-body (k x env)
  (cond ((null x)       `(funcall ,k nil))
        ((null (cdr x)) (compile k (car x) env))
        ((atom (car x)) `(progn ,(car x)
                                ,(compile-body k (cdr x) env)))
        (t              (w/uniq (val)
                          (compile `(lambda (,val) ,(compile-body k (cdr x) env))
                                   (car x) env)))))


(defun compile-fn (k exp env)
  (destructuring-bind (params &rest body) exp
    (w/uniq (k2)
      `(funcall ,k (lambda (,k2 ,@params)
                     ,(compile-body k2 body (append params env)))))))

(defun compile-many (k xs rs env)
  (cond ((null xs)       `(funcall ,k ,@(reverse rs)))
        ((atom (car xs)) (if (variable-p (car xs)) (declare-var (car xs) env))
                         (compile-many k (cdr xs) (cons (car xs) rs) env))
        (t               (w/uniq (k2 val)
                           `(let ((,k2 (lambda (,val)
                                         ,(compile-many k (cdr xs) (cons val rs) env))))
                              ,(compile k2 (car xs) env))))))

(defun compile-funcall (k x env)
  (let ((args (gensyms (length x))))
    (compile-many `(lambda ,args (funcall ,(car args) ,k ,@(cdr args))) x '() env)))

(defun compile (k x env)
  (cond ((or (null x)
             (eq x t))   (compile-value k x))
        ((symbolp x)     (compile-var k x env))
        ((atom x)        (compile-value k x))
        ((car-is x 'set) (compile-set k (cdr x) env))
        ((car-is x 'if)  (compile-if k (cdr x) env))
        ((car-is x 'fn)  (compile-fn k (cdr x) env))
        ((consp x)       (compile-funcall k x env))
        (t               (error (format t "Can't compile ~S" x)))))

(defstruct eof)
(defvar *eof-object* (make-eof))
(defun eof-object? (x) (eq x *eof-object*))

(defun read ()
  (if (peek-char t *standard-input* nil)
    (cl:read)
    *eof-object*))

(defun eval (x)
  (let ((*free-vars* nil))
    (w/uniq (k)
      (let ((cx (compile k x nil)))
        (format t "free vars:: ~S~%" *free-vars*)
        (format t "Compiled: ~S~%" cx)
        (funcall (cl:compile nil `(lambda (,k)
                                    (declare (special ,@*free-vars*))
                                    ,cx))
                 #'identity)))))

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
