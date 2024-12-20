(defpackage ztalk
  (:use :cl)
  (:shadow compile))

;(declaim (optimize (speed 3) (safety 0)))

(defun die (msg)
  (format t msg)
  (sb-ext:quit :unix-status  1))

(defun split-sequence (d s)
  (let ((start 0)
        (r nil))
    (loop for end = (position d s :start start)
          while end do
            (push (subseq s start end) r)
            (setq start (1+ end)))
    (nreverse (cons (subseq s start) r))))

(defun parse-version (s)
  (mapcar #'parse-integer (split-sequence #\. s)))

(defun lexicographic-compare (x y)
  (cond ((null x) (if (null y) 0 -1))
        ((null y) (if (null x) 0  1))
        ((integerp (car x)) (cond ((= (car x) (car y))
                                   (lexicographic-compare (cdr x) (cdr y)))
                                  ((< (car x) (car y)) -1)
                                  (t                    1)))
       (t  (error "Can't compare versions."))))

(defun version<= (s1 s2)
  (let ((v1 (parse-version s1))
        (v2 (parse-version s2)))
    (<= (lexicographic-compare v1 v2) 0)))

(defun require-sbcl (version)
  (unless (and (string= (lisp-implementation-type) "SBCL")
               (version<= version (lisp-implementation-version)))
    (die (format nil "Z-talk requires SBCL ~A." version))))

(require-sbcl "2.4.4")

(in-package ztalk)

(defmacro rfn (name params &rest body)
  `(labels ((,name ,params ,@body)) #',name))

(defmacro w/uniq (names &rest body)
  `(let ,(mapcar (lambda (x) (list x '(gensym))) names) ,@body))

(defmacro while (c &rest body)
  (w/uniq (f)
    `(funcall (rfn ,f () (when ,c ,@body (,f))))))

(defun gensyms (n)
  (unless (zerop n)
    (cons (gensym) (gensyms (- n 1)))))

; reader

(defvar *ztalk-package* (make-package "lk"))
(defvar *ztalk-lex* (make-package "lx"))

(defun read-char-if (s c)
  (if (eq (peek-char nil s nil) c)
      (read-char s)))

(declaim (ftype (function (&optional t t t) t) ztalk-read))

(defvar *free-vars*)

(declaim (ftype (function (t t t) t) compile))

(defun car-is (x y) (and (consp x) (eq (car x) y)))

(defun litsymp (x)
  (and (symbolp x) (member x '(|lk|::|true| |lk|::|false| |lk|::|nil|))))

(defun literalp (x)
  (if (symbolp x)
      (litsymp x)
      (not (consp x))))

(defun variablep (x)
  (and (symbolp x)
       (not (litsymp x))))

(defun emit-funcall (fn &rest args)
  (if (symbolp fn)
    `(funcall ,fn ,@args)
    (cons fn args)))

(defun lexical (x)
  (intern (symbol-name x) *ztalk-lex*))

(defun isolate-lexical (x env)
  (if (member x env)
    (lexical x)
    (progn (push x *free-vars*) x)))

(defun compile-var (x env k)
  (emit-funcall k (isolate-lexical x env)))

(defun zt-to-cl-literal (x)
  (case x (|lk|::|true|  t)
          (|lk|::|false| nil)
          (|lk|::|nil|   nil)
          (otherwise  x)))
          
(defun compile-literal (x k)
  (emit-funcall k (zt-to-cl-literal x)))

(defun compile-assign (x env k)
  (let ((var (isolate-lexical (car x) env))
        (exp (cadr x)))
    (cond ((literalp  exp) (emit-funcall k `(setq ,var ,(zt-to-cl-literal exp))))
          ((variablep exp) (emit-funcall k `(setq ,var ,(isolate-lexical exp env))))
          (t               (w/uniq (r)
                             (compile exp env
                               `(lambda (,r) ,(emit-funcall k `(setq ,var ,r)))))))))

(defun compile-if (x env k)
  (cond ((null x)          (emit-funcall k '|lk|::|none|))
        ((null (cdr x))    (compile (car x) env k))
        ((not (symbolp k)) (w/uniq (k2) `(let ((,k2 ,k)) ,(compile-if x env k2))))
        ((atom (car x))    (destructuring-bind (cnd then &rest rest) x
                             (let ((zc (if (literalp cnd)
                                           (zt-to-cl-literal cnd)
                                           (isolate-lexical cnd env))))
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
  (cond ((null x)       (emit-funcall k '|lk|::|none|))
        ((null (cdr x)) (compile (car x) env k))
        ((atom (car x)) `(progn ,(car x)
                                ,(compile-body (cdr x) env k)))
        (t              (w/uniq (r)
                          (compile (car x) env
                            `(lambda (,r)
                               (declare (ignore ,r))
                               ,(compile-body (cdr x) env k)))))))

(defun compile-params (x env)
  (cond ((null x)                  nil)
        ((symbolp x)               (list '&rest (lexical x)))
        ((car-is x '|lk|::|&rest|) (cons '&rest (compile-params (cdr x) env)))
        ((car-is x '|lk|::|&opt|)  (cons '&optional (compile-params (cdr x) env)))
        ((consp x)                 (let ((param (car x)))
                                     (cons (if (consp param)
                                               (list (lexical (car param))
                                                     (cond ((literalp  (cadr param)) (zt-to-cl-literal (cadr param)))
                                                           ((variablep (cadr param)) (isolate-lexical (cadr param) env))
                                                           (t                        (error "no expressions in default param values allowed"))))
                                               (lexical param))
                                           (compile-params (cdr x) env))))))

(defun param-names (x)
  (cond ((null x)                  nil)
        ((symbolp x)               (list x))
        ((car-is x '|lk|::|&rest|) (param-names (cdr x)))
        ((car-is x '|lk|::|&opt|)  (param-names (cdr x)))
        ((consp (car x))           (cons (caar x) (param-names (cdr x))))
        ((consp x)                 (cons (car x) (param-names (cdr x))))))

(defun compile-fn (exp env k)
  (destructuring-bind (params &rest body) exp
    (w/uniq (k2)
      `(funcall ,k (lambda (,k2 ,@(compile-params params env))
                     ,(compile-body body (append (param-names params) env) k2))))))

(defun compile-many (xs rs env k)
  (cond ((null xs)            (apply #'emit-funcall k (reverse rs)))
        ((literalp  (car xs)) (compile-many (cdr xs) (cons (zt-to-cl-literal (car xs)) rs) env k))
        ((variablep (car xs)) (compile-many (cdr xs) (cons (isolate-lexical (car xs) env) rs) env k))
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
        ((atom x)                       `(|lk|::|quote| ,x))
        ((car-is x '|lk|::|unquote|)    (if (= level 1)
                                            (cadr x)
                                            (expand-qquoted-list x (- level 1))))
        ((car-is x '|lk|::|quasiquote|) `(|lk|::|quasiquote| ,(expand-qquote (cadr x) (+ level 1))))
        (t                              (expand-qquoted-list x level))))

; TODO:
;   `(,x) when x is not a list
;   `(1 2 . ,x)
(defun expand-qquoted-list (xs level)
  ;(format t "expand-qquoted-list: ~S (level = ~S)~%" xs level)
  (if (null xs)
    '|lk|::|nil|
    (let ((x (car xs)))
      (if (car-is x '|lk|::|unquote-splicing|)
        `(|lk|::|list-append| ,(expand-qquote (cadr x) (- level 1))
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

(defun filep (x) (and (streamp x) (subtypep (type-of x) 'file-stream)))

(defvar type-stats (make-hash-table))

(defun ztalk-type (x)
  (let ((r
    (cond ((tagged-p x)     (tagged-tag x))
          ((consp x)        '|lk|::|pair|)
          ((null x)         '|lk|::|null|)
          ((symbolp x)      '|lk|::|symbol|)
          ((functionp x)    '|lk|::|fn|)
          ((characterp x)   '|lk|::|char|)
          ((stringp x)      '|lk|::|wstring|)
          ((bytevectorp x)  '|lk|::|bytevector|)
          ((vectorp x)      '|lk|::|vector|)
          ((hash-table-p x) '|lk|::|dict|)
          ((integerp x)     '|lk|::|int|)
          ((streamp x)      '|lk|::|cl-stream|)
          ((floatp x)       '|lk|::|float|)
          (t                (error (format nil "Unknown type: ~S" x))))))
    (incf (gethash r type-stats 0))
    r))

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
  (cond ((literalp x)                   (compile-literal x k))
        ((symbolp x)                    (compile-var x env k))
        ((car-is x '|lk|::|assign|)     (compile-assign (cdr x) env k))
        ((car-is x '|lk|::|if|)         (compile-if (cdr x) env k))
        ((car-is x '|lk|::|fn|)         (compile-fn (cdr x) env k))
        ((car-is x '|lk|::|quote|)      (emit-funcall k `(quote ,(cadr x))))
        ((car-is x '|lk|::|quasiquote|) (compile-qquote (cadr x) env k))
        ((macrop x)                     (compile (ztalk-macroexpand x) env k))
        ((consp x)                      (compile-call x env k))
        (t                              (error (format t "Can't compile ~S" x)))))

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

(zdef type-stats type-stats)

(zdef none (annotate '|lk|::|none| nil))

(zdef call/cc
  (lambda (k f)
    (funcall f k (lambda (k2 k3)
                   (declare (ignore k2))
                   (funcall k k3)))))

(zdef apply (lambda (k f args)
  (apply f (cons k args))))

(zdef cons (lambda (k a b) (funcall k (cons a b))))

(zexport annotate (tag x))
(zdef type (lambda (k x) (declare (inline ztalk-type)) (funcall k (ztalk-type x))))
(zexport rep (x))
(zdefun symbol? (x) (and (symbolp x) (not (eq x nil)) (not (eq x t))))
(zexport symbol-name (x))
(zdefun bound? (x) (boundp x))
(zexport symbol-value (x))
(zdefun symbol (x) (intern x *ztalk-package*))
(zdefun pair? (x) (consp x))
(zdefun is (x y) (eq x y))
(zdefun eq (x y) (eql x y))
(zdefun equal (x y) (equal x y))
(zexport gensym ())

(require 'sb-introspect)
(zdefun fn-params (f) (cdr (sb-introspect:function-lambda-list f)))

(zdefun integer? (x) (integerp x))

(zdefun char? (x) (characterp x))
(zdefun wstring? (x) (stringp x))
(defun wstring-ref (x i) (declare (type string x)) (aref x i))
(zexport wstring-ref (x i))
(defun wstring-length (x) (declare (type string x)) (length x))
(zexport wstring-length (x))
(defun wsubstring (x i j) (declare (type string x)) (subseq x i j))
(zexport wsubstring (x i j))
(zdefun copy-wstring (s) (copy-seq s))

(zdefun make-adjustable-wstring (&optional (n 0))
  (make-array n :element-type 'character
                :fill-pointer 0))
(zdefun wstring-push-back (x c) (vector-push-extend c x) x)
(zdefun wstring-capacity (x) (array-total-size x))

(zdefun make-vector (n &optional x) (make-array n :initial-element x))
(zdefun vector? (x) (and (vectorp x) (eq (array-element-type x) t)))
(zdefun vector-ref (x i) (aref x i))
(zdefun vector-set (x i v) (setf (aref x i) v))
(defun vector-size (x) (declare (type vector x)) (length x))
(zexport vector-size (x))

(zdefun make-adjustable-vector (&optional (n 0))
  (make-array n :fill-pointer 0))
(zdefun vector-push-back (x value) (vector-push-extend value x) x)
(zdefun vector-capacity (x) (array-total-size x))

(defun make-bytevector (n)
  (make-array n :element-type '(unsigned-byte 8)))

(zdefun make-bytevector (&optional (n 0))
  (make-bytevector n))

(zdefun bytes (&rest xs)
  (make-array (length xs) :element-type '(unsigned-byte 8)
                          :initial-contents xs))

(defun bytevectorp (x)
  (and (arrayp x)
       (equal (array-element-type x) '(unsigned-byte 8))))

(zdefun bytevector? (x) (bytevectorp x))

(zdefun bytevector-ref (x i) (aref x i))
(zdefun bytevector-set (x i v) (setf (aref x i) v))
(zdefun bytevector-size (x) (length x))

(zdefun copy-bytevector! (dst src &optional (pos 0) (start 0) end)
  (replace dst src :start1 pos :start2 start :end2 end))

(zdefun make-adjustable-bytevector (&optional (n 0))
  (make-array n :element-type '(unsigned-byte 8)
                :fill-pointer 0))

(zdefun bytevector-push-back (x value) (vector-push-extend value x) x)
(zdefun bytevector-capacity (x) (array-total-size x))

(defun bytevector-reserve (x n)
  (if (> n (array-total-size x))
    (let ((new-size (max n (* (array-total-size x) 2))))
      (adjust-array x new-size))
    x))

(zexport bytevector-reserve (x n))

(zdefun bytevector-resize (x n)
  (bytevector-reserve x n)
  (setf (fill-pointer x) n)
  x)

(zdefun proc? (x) (functionp x))

(zdefun make-dict (&optional test)
  (cond ((null test)              (make-hash-table))
        ((eq test '|lk|::|equal|) (make-hash-table :test 'equal))
        (t                        (error "make-dict: wrong test type"))))

(zdefun dict? (x) (hash-table-p x))
(zdefun dict-ref (d k &optional v) (gethash k d v))
(zdefun dict-set (d k v) (setf (gethash k d) v))
(zdefun dict-size (d) (hash-table-count d))
(zdefun hash (x) (sxhash x))

(zdef dict-for-each
  (lambda (k d f)
    (funcall k (maphash (lambda (key value)
                          (funcall f #'identity key value)) d))))

(zdefun div (a b) (floor a b))
(zexport mod (a b))
(zexport car (p))
(zexport cdr (p))
(zdefun set-car (p x) (setf (car p) x))
(zdefun set-cdr (p x) (setf (cdr p) x))
(zexport = (a b))
(zexport < (a b))
(zexport <= (a b))
(zexport > (a b))
(zexport >= (a b))
(zdefun list-append (&rest xs) (apply #'append xs))
(zdefun wstring-append (&rest xs) (apply #'concatenate 'string (mapcar #'string xs)))
(zdefun vector-append (&rest xs) (apply #'concatenate 'vector xs))

(dolist (f '(+ - * / vector))
  (eval `(zexport ,f (&rest xs))))

(zexport char-code (c))
(zdefun char (x) (code-char x))

(zdef process-stdin *standard-input*)
(zdef process-stdout *standard-output*)
(zdef process-stderr *error-output*)

(zdefun open-input-file (path) (open path :direction :input))
(zdefun open-output-file (path) (open path :direction :output))
(zdefun cl-close (f) (close f))
(zdefun file? (x) (filep x))
(zdefun open-input-wstring (s) (make-string-input-stream s))
(zdefun open-output-wstring () (make-string-output-stream))
(zdefun get-output-wstring (s) (get-output-stream-string s))

(zdefun cl-sread (&optional s eof) (ztalk-read s nil eof))
(zdefun cl-load (path) (load path))

(zdefun cl-peek-char (&optional s) (peek-char nil s nil))
(zdefun cl-read-char (&optional s) (read-char s nil nil))

(zdefun cl-write-char (c s) (write-char c s))
(zdefun cl-write (x s) (write x :stream s))
(zdefun cl-print (x &optional s) (princ x s))
(zdefun cl-flush-output (&optional s) (finish-output s))

(zdefun write-symbol (x &optional s)
  (let ((*package* *ztalk-package*))
    (write x :stream s)))

(defun c-strlen (s)
  (let ((f (sb-alien:extern-alien "strlen" (function sb-alien:size-t (* sb-alien:char)))))
    (sb-alien:alien-funcall f s)))

(defun c-memmove (dst src n)
  (let ((f (sb-alien:extern-alien "memmove"
                                  (function (* sb-alien:char)
                                            (* sb-alien:char) (* sb-alien:char) sb-alien:size-t))))
    (sb-alien:alien-funcall f dst src n)))

(defun c-string-to-bytevector (s)
  (let* ((n (c-strlen s))
         (r (make-bytevector n)))
    (sb-sys:with-pinned-objects (r)
      (c-memmove (sb-sys:vector-sap r) s n))
    r))

(zdef argv
  (let ((c-argv (sb-alien:extern-alien "posix_argv" (* (* sb-alien:char)))))
    (loop for i from 0
      with result = (make-array 0 :fill-pointer 0)
      for arg = (sb-alien:alien-sap (sb-alien:deref c-argv i))
      while (not (= (sb-sys:sap-int arg) 0))
      do (vector-push-extend (annotate '|lk|::|string| (c-string-to-bytevector arg)) result)
      finally (return result))))

(zdef load-pathname *load-pathname*)
(zdef cl-posix-argv sb-ext:*posix-argv*)
(zdef cl-argv (apply #'vector (namestring *load-pathname*) (cdr sb-ext:*posix-argv*)))

(zdefun exit (&optional code) (sb-ext:quit :unix-status (or code 0)))

(zdefun file-exists (p) (probe-file p))

; ztalk-reader
(defvar *case-sensitive-readtable*)

(defun ztalk-read-quoted (s c)
  (cond ((eq c #\') (list '|lk|::|quote| (read s t nil t)))
        ((eq c #\`) (list '|lk|::|quasiquote| (read s t nil t)))
        ((eq c #\,) (if (read-char-if s #\@)
                        (list '|lk|::|unquote-splicing| (read s t nil t))
                        (list '|lk|::|unquote| (read s t nil t))))))
   
(defvar *ztalk-readtable*
  (let ((rt (copy-readtable nil)))
    (setf (readtable-case rt) :preserve)
    (dolist (c '(#\' #\` #\,))
      (set-macro-character c #'ztalk-read-quoted nil rt))
    rt))

(defun ztalk-read (&optional s (error-on-eof t) eof)
  (let ((*readtable* *ztalk-readtable*)
        (*package* *ztalk-package*))
    (read s error-on-eof eof)))

(defun ztalk-eval (x &optional (k #'identity))
  (let ((*free-vars* nil))
    (w/uniq (gk)
      (let ((cx (compile x nil gk)))
        ;(format t "Free vars: ~S~%" *free-vars*)
        ;(format t "Compiled: ~S~%" cx)
        (funcall (cl:compile nil `(lambda (,gk)
                                    (declare (special ,@*free-vars*))
                                    ,cx))
                 k)))))

(zdef eval
  (lambda (k e)
    (ztalk-eval e k)))

(defparameter ztalk-bootstrap
  (make-pathname :directory (pathname-directory *load-pathname*)
                 :name "zta.lk"))

(defun ztalk-load (path)
  (with-open-file (f path)
    (while (peek-char t f nil)
      (ztalk-eval (ztalk-read f)))))

(ztalk-load ztalk-bootstrap)
