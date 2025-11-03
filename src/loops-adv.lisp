(cl:in-package :cl-user)
(defpackage :do-control/loops-adv
  (:use
   #:coalton
   #:coalton-prelude
   )
  (:local-nicknames
   (:ct #:do-control/core)
   (:i #:simple-io/io)
   (:it #:simple-io/term)
   )
  (:import-from #:coalton-library/monad/environment
   #:MonadEnvironment
   #:ask
   #:local
   #:asks)
  (:import-from #:coalton-library/monad/statet
   #:MonadState
   #:get
   #:put
   #:modify)
  (:export
   ;;
   ;; LoopT
   ;;
   #:LoopT

   ;;
   ;; Loop Commands
   ;;
   #:unwrap-loop
   #:break-loop
   #:continue-loop

   ;;
   ;; Looping Control Flow
   ;;
   #:loop_
   #:do-loop

   #:loop-while
   #:do-loop-while

   #:loop-do-while
   #:do-loop-do-while

   #:collect
   #:do-collect

   #:collect-val
   #:do-collect-val

   #:foreach
   #:do-foreach

   #:once
   #:do-once
   )
  )

(in-package :do-control/loops-adv)

(named-readtables:in-readtable coalton:coalton)
;;;
;;; LoopT Monad Transformer
;;;

(coalton-toplevel

  (define-type (Step :a)
    Continue%
    Break%
    (Value% :a))

  (repr :transparent)
  (define-type (LoopT :m :a)
    (LoopT (:m (Step :a))))

  (inline)
  (declare unwrap-loop (LoopT :m :a -> :m (Step :a)))
  (define (unwrap-loop (LoopT m-stp))
    m-stp)

  (inline)
  (declare break-loop (Monad :m => LoopT :m :a))
  (define break-loop (LoopT (pure Break%)))

  (inline)
  (declare continue-loop (Monad :m => LoopT :m :a))
  (define continue-loop (LoopT (pure Continue%)))

  (define-instance (Functor :m => Functor (LoopT :m))
    (inline)
    (define (map fa->b (LoopT m-stp-a))
      (LoopT
       (map
        (fn (stp)
          (match stp
            ((Continue%) Continue%)
            ((Break%) Break%)
            ((Value% a) (Value% (fa->b a)))))
        m-stp-a))))

  (define-instance (Monad :m => Applicative (LoopT :m))
    (inline)
    (define (pure a)
      (LoopT (pure (Value% a))))
    (inline)
    (define (liftA2 fa->b->c (LoopT m-stp-a) (LoopT m-stp-b))
      (LoopT
       (do
        (stp-a <- m-stp-a)
        (match stp-a
          ((Continue%) (pure Continue%))
          ((Break%) (pure Break%))
          ((Value% a)
           (do
            (stp-b <- m-stp-b)
            (match stp-b
              ((Continue%) (pure Continue%))
              ((Break%) (pure Break%))
              ((Value% b)
               (pure (Value% (fa->b->c a b))))))))))))

  (define-instance (Monad :m => Monad (LoopT :m))
    (inline)
    (define (>>= (LoopT m-stp-a) fa->lpt-m-stp-a)
      (LoopT
       (do
        (stp-a <- m-stp-a)
        (match stp-a
          ((Break%) (pure Break%))
          ((Continue%) (pure Continue%))
          ((Value% a)
           (unwrap-loop (fa->lpt-m-stp-a a))))))))

  (inline)
  (declare map-loopT ((:m (Step :a) -> :n (Step :b)) -> LoopT :m :a -> LoopT :n :b))
  (define (map-loopT fm-stp-a->n-stp-b (LoopT m-stp-a))
    (LoopT
     (fm-stp-a->n-stp-b m-stp-a)))

  (inline)
  (declare lift-loopT (Functor :m => :m :a -> LoopT :m :a))
  (define (lift-loopT ma)
    (LoopT (map Value% ma)))

  (define-instance (MonadTransformer LoopT)
    (define lift lift-loopT))

  ;;
  ;; MTL Instances
  ;;

  (define-instance (MonadEnvironment :e :m => MonadEnvironment :e (LoopT :m))
    (define ask (lift ask))
    (define asks (compose lift asks))
    (define local (compose map-loopT local)))

  (define-instance (MonadState :s :m => MonadState :s (LoopT :m))
    (define get (lift get))
    (define put (compose lift put))
    (define modify (compose lift modify)))

  (define-instance (it:MonadIoTerm :m => it:MonadIoTerm (LoopT :m))
    (define it:write (compose lift it:write))
    (define it:write-line (compose lift it:write-line))
    (define it:read-line (lift it:read-line)))
  )

;;;
;;; Loop Controls
;;;

(coalton-toplevel
  (declare loop_ (Monad :m => LoopT :m :a -> :m Unit))
  (define (loop_ body)
    (do
     (r <- (unwrap-loop body))
     (match r
       ((Break%) (pure Unit))
       (_ (loop_ body)))))

  (declare loop-while ((Monad :m) (ct::Terminator :t) => LoopT :m :t -> :m Unit))
  (define (loop-while body)
    (do
     (r <- (unwrap-loop body))
     (match r
       ((Break%) (pure Unit))
       ((Continue%) (loop-while body))
       ((Value% t)
        (if (ct::ended? t)
            (pure Unit)
            (loop-while body))))))

  (declare loop-do-while ((Monad :m) (ct::Terminator :t) => :m :t -> LoopT :m :a -> :m Unit))
  (define (loop-do-while m-term? body)
    (do
     (term? <- m-term?)
     (if (ct::ended? term?)
         (pure Unit)
         (do
          (r <- (unwrap-loop body))
          (match r
            ((Break%) (pure Unit))
            (_ (loop-do-while m-term? body)))))))

  (declare collect (Monad :m => LoopT :m :a -> :m (List :a)))
  (define (collect body)
    (rec % ((result mempty))
      (do
       (r <- (unwrap-loop body))
       (match r
         ((Break%) (pure (reverse result)))
         ((Continue%) (% result))
         ((Value% val)
          (% (Cons val result)))))))

  (declare collect-val ((Monad :m) (ct::Yielder :y) => LoopT :m (:y :a) -> :m (List :a)))
  (define (collect-val body)
    (rec % ((result mempty))
      (do
       (r <- (unwrap-loop body))
       (match r
         ((Break%) (pure (reverse result)))
         ((Continue%) (% result))
         ((Value% val?)
          (match (ct::yield val?)
            ((Some x)
             (% (Cons x result)))
            ((None)
             (pure (reverse result)))))))))

  (declare foreach (Monad :m => List :a -> (:a -> LoopT :m :z) -> :m Unit))
  (define (foreach lst fa->lpt-m)
    (rec % ((rem lst))
      (match rem
        ((Nil) (pure Unit))
        ((Cons a rst)
         (do
          (r <- (unwrap-loop (fa->lpt-m a)))
          (match r
            ((Break%) (pure Unit))
            (_ (% rst))))))))

  (inline)
  (declare once (Monad :m => LoopT :m :a -> :m Unit))
  (define (once lp-m)
    (do
     (unwrap-loop lp-m)
     (pure Unit))))

(cl:defmacro do-loop (cl:&body body)
  `(loop_
    (do
     ,@body)))

(cl:defmacro do-loop-while (cl:&body body)
  `(loop-while
    (do
     ,@body)))

(cl:defmacro do-loop-do-while (test cl:&body body)
  `(loop-do-while ,test
    (do
     ,@body)))

(cl:defmacro do-collect (cl:&body body)
  `(collect
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

(cl:defmacro do-once (cl:&body body)
  `(once
    (do
     ,@body)))
