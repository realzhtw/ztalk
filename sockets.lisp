(require 'sb-bsd-sockets)

(defun make-udp4-socket (address-string port)
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp)))
    (sb-bsd-sockets:socket-bind s (sb-bsd-sockets:make-inet-address address-string) port)
    s))

(defun make-udp6-socket (address-string port)
  (let ((s (make-instance 'sb-bsd-sockets:inet6-socket :type :datagram :protocol :udp)))
    (sb-bsd-sockets:socket-bind s (sb-bsd-sockets:make-inet6-address address-string) port)
    s))

(zdefun make-udp-socket (address port)
  (if (find #\: address)
      (make-udp6-socket address port)
      (make-udp4-socket address port)))

