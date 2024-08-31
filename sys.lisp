(zdefun set-signal-handler (signum handler)
  (sb-sys:enable-interrupt signum
                           (lambda (signum frame context)
                             (declare (ignore frame context))
                             (funcall handler #'identity signum))))

(zdef sigint  sb-unix:SIGINT)
(zdef sigterm sb-unix:SIGTERM)
(zdef sigstop sb-unix:SIGSTOP)
(zdef sigcont sb-unix:SIGCONT)
