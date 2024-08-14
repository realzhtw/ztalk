(defpackage ztalk
  (:use :cl)
  (:shadow compile))

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

(defvar *ztalk-package* (make-package "zt"))

(defun skip-space ()
  (peek-char t nil nil)
  nil)

(defun read-char-if (p s)
  (let ((c (peek-char nil nil nil)))
    (and c
      (cond ((characterp p) (and (char= c p) (read-char s)))
            ((functionp p)  (and (funcall p c) (read-char s)))
            (t              (error "char or predicate expected"))))))

(defun sym-char-p (c)
  (or (alphanumericp c) (char= c #\-)))

(defun read-sym-char (&optional (s *standard-input*))
  (read-char-if #'sym-char-p s))

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
   (and (symbolp x) (member x '(|zt|::|true| |zt|::|false| |zt|::|nil|))))

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
  (case x (|zt|::|true|  t)
          (|zt|::|false| nil)
          (|zt|::|nil|   nil)
          (otherwise  x)))
          
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

(defun compile-call (x env k)
  (let ((args (gensyms (length x))))
    (compile-many x '() env
      `(lambda ,args (funcall ,(car args) ,k ,@(cdr args))))))

(declaim (ftype (function (t t) t) expand-qquoted-list))

(defun expand-qquote (x level)
  ;(format t "expand-qquote: ~S (level = ~S)~%" x level)
  (cond ((= level 0) x)
        ((atom x)                    `(|zt|::|quote| ,x))
        ((car-is x '|zt|::|unquote|) (if (= level 1)
                                         (cadr x)
                                         (expand-qquoted-list x (- level 1))))
        ((car-is x '|zt|::|qquote|)  `(|zt|::|qquote| ,(expand-qquote (cadr x) (+ level 1))))
        (t                           (expand-qquoted-list x level))))

; TODO:
;   `(,x) when x is not a list
;   `(1 2 . ,x)
(defun expand-qquoted-list (xs level)
  ;(format t "expand-qquoted-list: ~S (level = ~S)~%" xs level)
  (if (null xs)
    '|zt|::|nil|
    (let ((x (car xs)))
      (if (car-is x '|zt|::|unquote-splicing|)
        `(|zt|::|append| ,(expand-qquote (cadr x) (- level 1))
                         ,(expand-qquoted-list (cdr xs) level))
        `(|zt|::|cons| ,(expand-qquote x level)
                       ,(expand-qquoted-list (cdr xs) level))))))

(defun compile-qquote (x env k)
  (let ((ex-x (expand-qquote x 1)))
    ;(format t "After expansion: ~S~%" ex-x)
    (finish-output)
    (compile ex-x env k)))

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

(defun macrop (fn)
  (and (symbolp fn)
       (boundp fn)
       (let ((v (symbol-value fn)))
         (and (tagged-p v)
              (eq (z-type v) 'mac)))))

(defun compile (x env k)
  (cond ((literalp x)                (compile-literal x k))
        ((symbolp x)                 (compile-var x env k))
        ((car-is x '|zt|::|set|)     (compile-set (cdr x) env k))
        ((car-is x '|zt|::|if|)      (compile-if (cdr x) env k))
        ((car-is x '|zt|::|fn|)      (compile-fn (cdr x) env k))
        ((car-is x '|zt|::|quote|)   (emit-funcall k `(quote ,(cadr x))))
        ((car-is x '|zt|::|qquote|)  (compile-qquote (cadr x) env k))
        ;((car-is x '|zt|::|unquote|) (compile-unquote (cadr x) env 1 k))
        ((consp x)                   (compile-call x env k))
        (t                           (error (format t "Can't compile ~S" x)))))

; ztalk definitions

(defmacro zdef (name exp)
  (let ((zt-name (intern (string-downcase (symbol-name name)) *ztalk-package*)))
    `(progn
       (defvar ,zt-name)
       (setq ,zt-name ,exp))))

(defmacro zdefun (name params &rest body)
  (w/uniq (k)
    `(zdef ,name (lambda ,(cons k params)
                   (funcall ,k (progn ,@body))))))

(defmacro zexport (name params)
  (if (eq (car params) '&rest)
      `(zdefun ,name ,params (apply #',name ,(cadr params)))
      `(zdefun ,name ,params ,(cons name params))))

(zdef call/cc (lambda (k f)
  (funcall f k (lambda (k2 k3)
                 (declare (ignore k2))
                 (funcall k k3)))))

(zexport mod (a b))
(zexport cons (a b))
(zexport car (p))
(zexport cdr (p))
(zexport list (&rest xs))
(zexport = (a b))
(zexport < (a b))
(zexport > (a b))

(dolist (f '(+ - * / append))
  (eval `(zexport ,f (&rest xs))))

; ztalk-reader
(defvar *case-sensitive-readtable*)

(defun ztalk-read-quoted (s c)
  (cond ((eq c #\') (list '|zt|::|quote| (read s t nil t)))
        ((eq c #\`) (list '|zt|::|qquote| (read s t nil t)))
        ((eq c #\,) (if (read-char-if #\@ s)
                        (list '|zt|::|unquote-splicing| (read s t nil t))
                        (list '|zt|::|unquote| (read s t nil t))))))
   
(defvar *ztalk-readtable*
  (let ((rt (copy-readtable nil)))
    (setf (readtable-case rt) :preserve)
    (dolist (c '(#\' #\` #\,))
      (set-macro-character c #'ztalk-read-quoted nil rt))
    rt))

(defun ztalk-read ()
  (let ((*readtable* *ztalk-readtable*)
        (*package* *ztalk-package*))
    (read)))

(defun ztalk-eval (x)
  (let ((*free-vars* nil))
    (w/uniq (k)
      (let ((cx (compile x nil k)))
        ;(format t "Free vars: ~S~%" *free-vars*)
        ;(format t "Compiled: ~S~%" cx)
        (funcall (cl:compile nil `(lambda (,k)
                                    (declare (special ,@*free-vars*))
                                    ,cx))
                 #'identity)))))

(defun prompt ()
  (princ "ztalk> ")
  (finish-output))

(defun ztalk-repl ()
  (while (progn
           (prompt)
           (peek-char t nil nil))
    (let ((x (ztalk-read)))
      ;(format t "Read:  ~S~%"  x)
      (format t "~S~%" (ztalk-eval x)))))

(ztalk-repl)
