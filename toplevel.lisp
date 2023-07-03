(in-package #:org.shirakumo.cari3s)

(defun load-from-file (file)
  (with-open-file (i file)
    (let ((*package* #.*package*)
          (initargs ())
          (generators ()))
      (loop with eof = (make-symbol "EOF")
            for item = (read i NIL eof)
            until (eql item eof)
            do (etypecase item
                 (symbol
                  (push (read i) initargs)
                  (push item initargs))
                 (cons
                  (push (apply #'make-instance item) generators))))
      (apply #'make-instance 'status-bar :generators (nreverse generators) initargs))))

(defun run-bar-from-file (&optional (file #p"~/.config/i3/cari3s.conf"))
  (run-bar (load-from-file file)))

(defun toplevel ()
  (let ((args (uiop:command-line-arguments)))
    (handler-case
        (apply #'run-bar-from-file args)
      #+sbcl
      (sb-sys:interactive-interrupt (e)
        (declare (ignore e)))
      (error (e)
        (eformat "Unhandled condition: ~a" e)))))
