(cl:in-package :cl-user)
(defpackage :do-control/loops
  (:use
   #:coalton
   #:coalton-prelude
   )
  (:local-nicknames
   (:l #:coalton-library/list)
   (:it #:coalton-library/iterator)
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
    "Repeat M-OPERATION until it returns a terminated value. Retuturns Unit."
    (do
     (res <- m-operation)
     (if (ended? res)
         (pure Unit)
         (loop-while m-operation))))

  (inline)
  (declare collect-val ((Monad :m) (Yielder :y) => :m (:y :a) -> :m (List :a)))
  (define (collect-val m-operation)
    "Repeatedly run M-OPERATION, collecting each yielded value into a list until
no value is yielded."
    (rec % ((result mempty))
      (do
       (val? <- m-operation)
       (match (yield val?)
         ((Some x)
          (% (Cons x result)))
         ((None)
          (pure (reverse result)))))))

  (declare foreach ((Monad :m) (it:IntoIterator :i :a) => :i -> (:a -> :m :z) -> :m Unit))
  (define (foreach into-itr fa->m)
    "Apply FA->M to each element produced by INTO-ITR and run the resulting monadic action.
Discards the return values and returns Unit."
    (rec % ((itr (it:into-iter into-itr)))
      (match (it:next! itr)
        ((None) (pure Unit))
        ((Some a)
         (do
          (fa->m a)
          (% itr))))))
  )

;;
;; Loops
;;

(cl:defmacro do-loop-while (cl:&body body)
  "Run BODY repeatedly (in a 'do' block) until it returns a terminator that has ended."
  `(loop-while
    (do
     ,@body)))

(cl:defmacro do-collect-val (cl:&body body)
  "Run BODY repeatedly (in a 'do' block) collecting each yielded value into a list."
  `(collect-val
    (do
     ,@body)))

(cl:defmacro do-foreach ((sym into-itr) cl:&body body)
  "For each element of INTO-ITR, bind it to SYM and run BODY in a 'do' block.
Returns Unit."
  `(foreach ,into-itr
    (fn (,sym)
      (do
       ,@body))))
