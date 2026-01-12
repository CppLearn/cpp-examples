
(format t "~% Enter file name to read: ")
(defvar fname (read-line))
(format t "~% Thank you. I will read: ~A" fname)

(handler-case
  (with-open-file (f fname
                    :direction :input
                    :if-does-not-exist :error)
    (loop for line = (read-line f nil nil)
      while line do
      (format t "~% ~a :: ~a " fname line)))
  (file-error (e)
    (format t "~% [ERROR] with file [~a] ~A~%"
      fname e)))

