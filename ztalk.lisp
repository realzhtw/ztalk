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

(defvar *ztalk-package* (make-package "lk"))

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

(declaim (ftype (function () t) ztalk-read))
(declaim (ftype (function (t) t) ztalk-load))

(defvar *free-vars*)

(declaim (ftype (function (t t t) t) compile))

(defun car-is (x y) (and (consp x) (eq (car x) y)))

(defun declare-var (x env)
  (unless (member x env)
    (push x *free-vars*))
  x)

(defun litsymp (x)
  (and (symbolp x) (member x '(|lk|::|true| |lk|::|false| |lk|::|nil|))))

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
  (case x (|lk|::|true|  t)
          (|lk|::|false| nil)
          (|lk|::|nil|   nil)
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
      
(defun compile-body (x env k)
  (cond ((null x)       (emit-funcall k nil))
        ((null (cdr x)) (compile (car x) env k))
        ((atom (car x)) `(progn ,(car x)
                                ,(compile-body (cdr x) env k)))
        (t              (w/uniq (r)
                          (compile (car x) env
                            `(lambda (,r)
                               (declare (ignore ,r))
                               ,(compile-body (cdr x) env k)))))))


(defun compile-params (x)
  (cond ((null x)    nil)
        ((symbolp x) (list '&rest x))
        ((consp x)   (cons (car x) (compile-params (cdr x))))))

(defun param-names (x)
  (cond ((null x) nil)
        ((symbolp x) (list x))
        ((consp x)   (cons (car x) (param-names (cdr x))))))

(defun compile-fn (exp env k)
  (destructuring-bind (params &rest body) exp
    (w/uniq (k2)
      `(funcall ,k (lambda (,k2 ,@(compile-params params))
                     ,(compile-body body (append (param-names params) env) k2))))))

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
        ((atom x)                    `(|lk|::|quote| ,x))
        ((car-is x '|lk|::|unquote|) (if (= level 1)
                                         (cadr x)
                                         (expand-qquoted-list x (- level 1))))
        ((car-is x '|lk|::|qquote|)  `(|lk|::|qquote| ,(expand-qquote (cadr x) (+ level 1))))
        (t                           (expand-qquoted-list x level))))

; TODO:
;   `(,x) when x is not a list
;   `(1 2 . ,x)
(defun expand-qquoted-list (xs level)
  ;(format t "expand-qquoted-list: ~S (level = ~S)~%" xs level)
  (if (null xs)
    '|lk|::|nil|
    (let ((x (car xs)))
      (if (car-is x '|lk|::|unquote-splicing|)
        `(|lk|::|append| ,(expand-qquote (cadr x) (- level 1))
                         ,(expand-qquoted-list (cdr xs) level))
        `(|lk|::|cons| ,(expand-qquote x level)
                       ,(expand-qquoted-list (cdr xs) level))))))

(defun compile-qquote (x env k)
  (let ((ex-x (expand-qquote x 1)))
    ;(format t "After expansion: ~S~%" ex-x)
    (finish-output)
    (compile ex-x env k)))

(defstruct tagged tag value)

(defun annotate (tag x) (make-tagged :tag tag :value x))

(defun ztalk-type (x)
  (cond ((tagged-p x)   (tagged-tag x))
        ((consp x)      '|lk|::|pair|)
        ((symbolp x)    '|lk|::|symbol|)
        ((functionp x)  '|lk|::|fn|)
        ((characterp x) '|lk|::|char|)
        ((stringp x)    '|lk|::|string|)
        ((integerp x)   '|lk|::|int|)
        ((floatp x)     '|lk|::|float|)
        (t              (error (format nil "Unknown type: ~S" x)))))

(defun rep (x)
  (if (tagged-p x)
      (tagged-value x)
      x))

(defun macrop (x)
  ;(format t "macrop: ~S~%" x)
  (and (consp x)
       (symbolp (car x))
       (boundp (car x))
       (eq (ztalk-type (symbol-value (car x))) '|lk|::|macro|)))

(defun ztalk-macroexpand (x)
  (let ((m (rep (symbol-value (car x)))))
    ;(format t "macro: ~S~%" m)
    (apply m (cons #'identity (cdr x)))))

(defun compile (x env k)
  (cond ((literalp x)                (compile-literal x k))
        ((symbolp x)                 (compile-var x env k))
        ((car-is x '|lk|::|set|)     (compile-set (cdr x) env k))
        ((car-is x '|lk|::|if|)      (compile-if (cdr x) env k))
        ((car-is x '|lk|::|fn|)      (compile-fn (cdr x) env k))
        ((car-is x '|lk|::|quote|)   (emit-funcall k `(quote ,(cadr x))))
        ((car-is x '|lk|::|qquote|)  (compile-qquote (cadr x) env k))
        ((macrop x)                  (compile (ztalk-macroexpand x) env k))
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
    `(zdef ,name (lambda (,k ,@params)
                   (funcall ,k (progn ,@body))))))

(defmacro zexport (name params)
  (if (eq (car params) '&rest)
      `(zdefun ,name ,params (apply #',name ,(cadr params)))
      `(zdefun ,name ,params ,(cons name params))))

(zdef call/cc (lambda (k f)
  (funcall f k (lambda (k2 k3)
                 (declare (ignore k2))
                 (funcall k k3)))))

(zexport annotate (tag x))
(zdefun type (x) (ztalk-type x))
(zexport rep (x))
(zdefun symbol? (x) (and (symbolp x) (not (eq x nil)) (not (eq x t))))
(zdefun pair? (x) (consp x))
(zdefun is (x y) (eq x y))
(zexport gensym ())

(zdefun string? (x) (stringp x))
(defun ztalk-string-ref (x i) (declare (type string x)) (aref x i))
(zdefun string-ref (x i) (ztalk-string-ref x i))
(defun string-length (x) (declare (type string x)) (length x))
(zexport string-length (x))
(defun substring (x i j) (declare (type string x)) (subseq x i j))
(zexport substring (x i j))

(zdefun make-adjustable-string (&optional (n 0))
  (make-array n :element-type 'character
                :fill-pointer 0))
(zdefun string-push-back (x c) (vector-push-extend c x) x)

(zdefun make-vector (n &optional x) (make-array n :initial-element x))
(zdefun vector? (x) (vectorp x))
(zdefun vector-ref (x i) (aref x i))
(zdefun vector-set (x i v) (setf (aref x i) v))
(defun vector-size (x) (declare (type vector x)) (length x))
(zexport vector-size (x))

(zdefun make-adjustable-vector (&optional (n 0))
  (make-array n :fill-pointer 0))
(zdefun vector-push-back (x value) (vector-push-extend value x) x)
(zdefun vector-capacity (x) (array-total-size x))

(zdefun fn? (x) (functionp x))

(zdefun load (path) (ztalk-load path))

(zexport mod (a b))
(zexport cons (a b))
(zexport car (p))
(zexport cdr (p))
(zdefun set-car (p x) (setf (car p) x))
(zdefun set-cdr (p x) (setf (cdr p) x))
(zexport = (a b))
(zexport < (a b))
(zexport > (a b))

(dolist (f '(+ - * / append vector))
  (eval `(zexport ,f (&rest xs))))

;(zdef argv sb-ext:*posix-argv*)
;(zdef stdin *standard-input*)
;(zdef stdout *standard-output*)
;(zdef stderr *error-output*)

(zdefun peek-char () (peek-char nil nil nil))
(zdefun read-char () (read-char nil nil nil))
(zdefun write-char (c) (write-char c))
(zdefun print (&rest args) (dolist (x args) (princ x)))
(zdefun println (&rest args) (dolist (x args) (princ x)) (terpri) nil)

; ztalk-reader
(defvar *case-sensitive-readtable*)

(defun ztalk-read-quoted (s c)
  (cond ((eq c #\') (list '|lk|::|quote| (read s t nil t)))
        ((eq c #\`) (list '|lk|::|qquote| (read s t nil t)))
        ((eq c #\,) (if (read-char-if #\@ s)
                        (list '|lk|::|unquote-splicing| (read s t nil t))
                        (list '|lk|::|unquote| (read s t nil t))))))
   
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

(defun ztalk-load (path)
  (with-open-file (*standard-input* path)
    (while (peek-char t nil nil)
      (ztalk-eval (ztalk-read)))))

(defun prompt ()
  (princ "lk> ")
  (finish-output))

(defun ztalk-repl ()
  (while (progn
           (prompt)
           (peek-char t nil nil))
    (let ((x (ztalk-read)))
      ;(format t "Read:  ~S~%"  x)
      (format t "~S~%" (ztalk-eval x)))))

(ztalk-load "zta.lk")

(ztalk-repl)
