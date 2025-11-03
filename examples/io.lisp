(cl:in-package :cl-user)
(defpackage :do-control/examples/io
  (:use
   #:coalton
   #:coalton-prelude
   )
  (:export
   #:MonadIoTerm
   #:write
   #:write-line
   #:read-line
   #:derive-monad-io-term

   #:IO
   #:run!
   )
  (:local-nicknames
   (#:lp #:do-control/loops-adv)
   (#:free #:coalton-library/monad/free)
   (:stt #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment)))

(in-package :do-control/examples/io)

;;; This package implements a simple IO monad that can perform
;;; basic terminal IO, using the Free monad.

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-class (Monad :m => MonadIoTerm :m)
    (write (Into :a String => :a -> :m Unit))
    (write-line (Into :a String => :a -> :m Unit))
    (read-line (:m String))
    ))

(cl:defmacro derive-monad-io-term (monadT-form)
  "Automatically derive an instance of MonadIoTerm for a monad transformer.

Example:
  (derive-monad-io-term (st:StateT :s :m))"
  `(define-instance (MonadIoTerm :m => MonadIoTerm ,monadT-form)
     (define write (compose lift write))
     (define write-line (compose lift write-line))
     (define read-line (lift read-line))
     ))

;;
;; Std. Library & LoopT Transformer Instances
;;

(coalton-toplevel
  (derive-monad-io-term (stt:StateT :s :m))
  (derive-monad-io-term (env:EnvT :e :m))
  (derive-monad-io-term (lp:LoopT :m)))

;;
;; IO Implementation
;;

(coalton-toplevel
  (define-type (IOF :t)
    (WriteF String :t)
    (WriteLineF String :t)
    (ReadLineF (String -> :t))
    )

  (define-instance (Functor IOF)
    (define (map f io)
      (match io
        ((WriteF s next) (WriteF s (f next)))
        ((WriteLineF s next) (WriteLineF s (f next)))
        ((ReadLineF cont) (ReadLineF (map f cont)))
        )))

  (define-type-alias IO (free:Free IOF))

  (define-instance (MonadIoTerm IO)
    (inline)
    (define (write s)
      (free:liftf (WriteF (into s) Unit)))
    (inline)
    (define (write-line s)
      (free:liftf (WriteLineF (into s) Unit)))
    (inline)
    (define read-line
      (free:liftf (ReadLineF id)))
    ))

(coalton-toplevel
  (declare run! (IO :t -> :t))
  (define run!
    (free:run-free
     (fn (iof)
       (match iof
         ((WriteF s next)
          (lisp :a (s)
            (cl:format cl:t "~a" s))
          next)
         ((WriteLineF s next)
          (lisp :a (s)
            (cl:format cl:t "~a~%" s))
          next)
         ((ReadLineF cont)
          (let input = (lisp String ()
                         (cl:read-line)))
          (cont input)))))))

(coalton-toplevel
  (define (run-example)
    (run!
     (do
      (write-line "Please enter your name")
      (name <- read-line)
      (write-line (<> "Hello " name))))))
