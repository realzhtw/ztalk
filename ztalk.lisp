(defpackage ztalk
  (:use :cl)
  (:shadow read compile eval))

(in-package ztalk)

;(defmacro fn (params . body)
;  `(lambda ,params ,@body))

(defmacro rfn (name params &rest body)
  `(labels ((,name ,params ,@body)) #',name))

;(defmacro aif (c then &rest else)
;  `(let ((it ,c))
;     (if it ,then ,@else)))

;(defmacro afn (params &rest body)
;  `(rfn self ,params ,@body))

(defmacro w/uniq (names &rest body)
  `(let ,(mapcar (lambda (x) (list x '(gensym))) names) ,@body))

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

(declaim (ftype (function () t) zt-read))

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

(defun zt-read ()
  (let ((c (peek-char t nil nil)))
    (if (char= c #\()
      (read-list)
      (read-atom))))

(defvar *free-vars*)

(declaim (ftype (function (t t t) t) compile))

(defun car-is (x y) (and (consp x) (eq (car x) y)))

(defun declare-var (x env)
  (unless (member x env)
    (push x *free-vars*))
  x)

(defun litsymp (x)
   (and (symbolp x) (member x '(|zt.true| |zt.false| |zt.nil|))))

(defun literalp (x)
  (or (characterp x)
      (stringp x)
      (numberp x)
      (litsymp x)))

(defun variablep (x)
  (and (symbolp x)
       (not (litsymp x))))

(defun emit-funcall (fn &rest args)
  (if (symbolp fn)
    (append (list 'funcall fn) args)
    (cons fn args)))

(defun compile-var (x env k)
  (declare-var x env)
  (emit-funcall k x))

(defun zt-to-cl-literal (x)
  (cond ((eq x '|zt.true|)  t)
        ((eq x '|zt.false|) nil)
        ((eq x '|zt.nil|)   nil)
        (t             x)))

(defun compile-literal (x k)
  (emit-funcall k (zt-to-cl-literal x)))

(defun compile-set (x env k)
  (destructuring-bind (var exp) x
    (declare-var var env)
    (if (variablep exp)
      (declare-var exp env))
    (if (or (variablep exp)
            (literalp exp))
        (emit-funcall k `(setq ,var ,exp)))
        (w/uniq (r)
          (compile exp env
            `(lambda (,r) ,(emit-funcall k `(setq ,var ,r)))))))

(defun compile-if (x env k)
  (cond ((null x)          (emit-funcall k nil))
        ((null (cdr x))    (compile (car x) env k))
        ((not (symbolp k)) (w/uniq (k2) `(let ((,k2 ,k)) ,(compile-if x env k2))))
        ((atom (car x))    (destructuring-bind (cnd then &rest rest) x
                             (let ((zc (if (literalp cnd) (zt-to-cl-literal cnd) cnd)))
                               `(if ,zc
                                    ,(compile then env k)
                                    ,(compile-if rest env k)))))
        (t                 (w/uniq (r)
                             (compile (car x) env
                               `(lambda (,r)
                                  (if ,r
                                      ,(compile (cadr x) env k)
                                      ,(compile-if (cddr x) env k))))))))
      
(defun compile-body (k x env)
  (cond ((null x)       (emit-funcall k nil))
        ((null (cdr x)) (compile (car x) env k))
        ((atom (car x)) `(progn ,(car x)
                                ,(compile-body k (cdr x) env)))
        (t              (w/uniq (r)
                          (compile (car x) env
                            `(lambda (,r)
                               (declare (ignore ,r))
                               ,(compile-body k (cdr x) env)))))))


(defun compile-fn (exp env k)
  (destructuring-bind (params &rest body) exp
    (w/uniq (k2)
      `(funcall ,k (lambda (,k2 ,@params)
                     ,(compile-body k2 body (append params env)))))))

(defun compile-many (xs rs env k)
  (cond ((null xs)            (apply #'emit-funcall (cons k (reverse rs))))
        ((literalp (car xs))  (compile-many (cdr xs) (cons (zt-to-cl-literal (car xs)) rs) env k))
        ((variablep (car xs)) (declare-var (car xs) env)
                              (compile-many (cdr xs) (cons (car xs) rs) env k))
        (t                    (w/uniq (r)
                                (compile (car xs) env
                                  `(lambda (,r)
                                     ,(compile-many (cdr xs) (cons r rs) env k)))))))

(defun compile-call (k x env)
  (let ((args (gensyms (length x))))
    (compile-many x '() env
      `(lambda ,args (funcall ,(car args) ,k ,@(cdr args))))))

(defstruct tagged value tag)

(defun z-annotate (value tag) (make-tagged :value value :tag tag))

(defun z-type (x)
  (cond ((tagged-p x)   (tagged-tag x))
        ((consp x)      'cons)
        ((symbolp x)    'sym)
        ((functionp x)  'fn)
        ((characterp x) 'char)
        ((stringp x)    'string)
        ((integerp x)   'int)
        ((floatp x)     'float)
        (t              (error (format nil "Unknown type: ~S" x)))))

(defun z-rep (x)
  (if (tagged-p x)
      (tagged-value x)
      x))

(defun macro-p (fn)
  (and (symbolp fn)
       (boundp fn)
       (let ((v (symbol-value fn)))
         (and (tagged-p v)
              (eq (z-type v) 'mac)))))

(defun compile (x env k)
  (cond ((literalp x)            (compile-literal x k))
        ((symbolp x)             (compile-var x env k))
        ((car-is x '|zt.set|)    (compile-set (cdr x) env k))
        ((car-is x '|zt.if|)     (compile-if (cdr x) env k))
        ((car-is x '|zt.fn|)     (compile-fn (cdr x) env k))
        ((or (car-is x '|zt.quote|)
             (car-is x 'quote))  (emit-funcall k `(quote ,(cadr x))))
        ((consp x)               (compile-call k x env))
        (t                       (error (format t "Can't compile ~S" x)))))

(defstruct eof)
(defvar *eof-object* (make-eof))
(defun eof-object-p (x) (eq x *eof-object*))

(defvar *current-ztalk-package* "zt")
(defvar *case-sensitive-readtable*)

(defun prefix-ztalk-package (s)
  (intern (concatenate 'string *current-ztalk-package* "." (symbol-name s))))

(defun ztalk-symbol (s)
  (intern (concatenate 'string *current-ztalk-package* "." (string-downcase (symbol-name s)))))

(defun custom-read-delimited-list (s c)
  (declare (ignore c))
  (let ((l (read-delimited-list #\) s)))
    (mapcar (lambda (x)
              (if (symbolp x)
                (prefix-ztalk-package x)
                x))
            l)))

(defvar *case-sensitive-readtable*
  (let ((rt (copy-readtable)))
    (setf (readtable-case rt) :preserve)
    (set-macro-character #\( #'custom-read-delimited-list nil rt)
    rt))


(defun read ()
  (let ((*readtable* *case-sensitive-readtable*))
    (if (peek-char t *standard-input* nil)
      (let ((x (cl:read)))
        (if (symbolp x)
           (prefix-ztalk-package x)
           x))
      *eof-object*)))

(defun eval (x)
  (let ((*free-vars* nil))
    (w/uniq (k)
      (let ((cx (compile x nil k)))
        (format t "Free vars: ~S~%" *free-vars*)
        (format t "Compiled: ~S~%" cx)
        (funcall (cl:compile nil `(lambda (,k)
                                    (declare (special ,@*free-vars*))
                                    ,cx))
                 #'identity)))))

(defmacro zdef (name exp)
  (let ((zt-name (ztalk-symbol name)))
    `(progn
       (defvar ,zt-name)
       (setq ,zt-name ,exp))))

(defmacro zdefun (name params &rest body)
  (w/uniq (k)
    `(zdef ,name (lambda ,(cons k params)
                   (funcall ,k (progn ,@body))))))

(zdef f (lambda (k x) (funcall k x)))
(zdef call/cc (lambda (k f) (funcall f k (lambda (k2 k3) (funcall k k3)))))
(zdefun cons (a b) (cons a b))
(zdefun car (p) (car p))
(zdefun cdr (p) (cdr p))
(zdefun list (&rest xs) xs)
(zdefun = (a b) (= a b))
(zdefun < (a b) (< a b))
(zdefun > (a b) (> a b))

(defun prompt ()
  (princ "ztalk> ")
  (finish-output))

(defun repl ()
  (prompt)
  (let ((x (read)))
    (unless (eof-object-p x)
      (format t "Read:  ~S~%"  x)
      (format t "~S~%" (eval x))
      (repl))))

(repl)
