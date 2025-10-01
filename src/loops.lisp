(cl:in-package :cl-user)
(defpackage :do-control/loops
  (:use
   #:coalton
   #:coalton-prelude
   )
  (:local-nicknames
   (:l #:coalton-library/list)
   )
  (:import-from #:do-control/core
   #:Terminator
   #:ended?
   #:Yielder
   #:yield)
  (:export
   ;;
   ;; Looping Control Flow
   ;;
   #:loop-while
   #:collect-val
   #:foreach

   #:do-loop-while
   #:do-collect-val
   #:do-foreach
   )
  )

(in-package :do-control/loops)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare loop-while ((Monad :m) (Terminator :t) => :m :t -> :m Unit))
  (define (loop-while m-operation)
    (do
     (res <- m-operation)
     (if (ended? res)
         (pure Unit)
         (loop-while m-operation))))

  (inline)
  (declare collect-val ((Monad :m) (Yielder :y) => :m (:y :a) -> :m (List :a)))
  (define (collect-val m-operation)
    (rec % ((result mempty))
      (do
       (val? <- m-operation)
       (match (yield val?)
         ((Some x)
          (% (Cons x result)))
         ((None)
          (pure (reverse result)))))))

  (inline)
  (declare foreach (Monad :m => List :a -> (:a -> :m :z) -> :m Unit))
  (define (foreach lst fa->m)
    (match lst
      ((Nil) (pure Unit))
      ((Cons h rem)
       (do
        (fold
         (fn (mz a)
           (>>= mz (const (fa->m a))))
         (fa->m h)
         rem)
        (pure Unit)))))
  )

;;
;; Loops
;;

(cl:defmacro do-loop-while (cl:&body body)
  `(loop-while
    (do
     ,@body)))

(cl:defmacro do-collect-val (cl:&body body)
  `(collect-val
    (do
     ,@body)))

(cl:defmacro do-foreach ((sym lst) cl:&body body)
  `(foreach ,lst
    (fn (,sym)
      (do
       ,@body))))

