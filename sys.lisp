(require 'sb-posix)

(zdefun set-signal-handler (signum handler)
  (sb-sys:enable-interrupt signum
                           (lambda (signum frame context)
                             (declare (ignore frame context))
                             (funcall handler #'identity signum))))

(zdef sigint  sb-unix:SIGINT)
(zdef sigterm sb-unix:SIGTERM)
(zdef sigstop sb-unix:SIGSTOP)
(zdef sigcont sb-unix:SIGCONT)

(zdefun unix-open (path mode perm)
  (let ((m (case mode
             (|lk|::|input|  sb-unix:O_RDONLY)
             (|lk|::|output| (logior sb-unix:O_WRONLY sb-unix:O_CREAT))
             (|lk|::|append| sb-unix:O_APPEND))))
    (sb-posix:open path m perm)))

(zdefun unix-read (fd buf start end)
  (sb-unix:unix-read fd (sb-sys:sap+ (sb-sys:vector-sap buf) start) (- end start)))

(zdefun unix-write (fd buf start end)
  (sb-unix:unix-write fd buf start end))

(zdefun unix-lseek (fd offset whence)
  (sb-unix-unix-lseek fd offset (case whence
                                  (set sb-posix:seek-set)
                                  (cur sb-posix:seek-cur)
                                  (end sb-posix:seek-end))))

(zdefun unix-close (fd)
  (sb-unix:unix-close fd))

(zdefun unix-fsync (fd)
  (sb-posix:fsync fd))

(zdefun unix-fstat (fd)
  (multiple-value-bind (ok dev inode mode nlinks uid gid rdev size
                        atime mtime ctime block-size nblocks)
      (sb-unix:unix-fstat fd)
    (if ok
        (vector dev inode mode nlinks uid gid rdev size
                atime mtime ctime block-size nblocks))))

(zdefun cpu-time () (/ (get-internal-run-time) (float internal-time-units-per-second)))
(zdefun real-time () (/ (get-internal-real-time) (float internal-time-units-per-second)))
(zexport sleep (x))
