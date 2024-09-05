(zdefun set-signal-handler (signum handler)
  (sb-sys:enable-interrupt signum
                           (lambda (signum frame context)
                             (declare (ignore frame context))
                             (funcall handler #'identity signum))))

(zdef sigint  sb-unix:SIGINT)
(zdef sigterm sb-unix:SIGTERM)
(zdef sigstop sb-unix:SIGSTOP)
(zdef sigcont sb-unix:SIGCONT)

(zdefun read-bytes (fd buf start end)
  (sb-unix:unix-read fd (sb-sys:sap+ (sb-sys:vector-sap buf) start) (- end start)))

(zdefun write-bytes (fd buf start end)
  (sb-unix:unix-write fd (sb-sys:sap+ (sb-sys:vector-sap buf) start) (- end start)))

(zdefun cpu-time () (/ (get-internal-run-time) (float internal-time-units-per-second)))
(zdefun real-time () (/ (get-internal-real-time) (float internal-time-units-per-second)))
(zexport sleep (x))
