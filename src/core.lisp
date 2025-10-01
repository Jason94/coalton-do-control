(cl:in-package :cl-user)
(defpackage :do-control/core
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/statet
   )
  (:local-nicknames
   (:ty #:coalton-library/types)
   (:l #:coalton-library/list)
   (:opt #:coalton-library/optional)
   (:rst #:coalton-library/result)
   )
  (:export
   ;;
   ;; Single Execution Control Flow
   ;;
   #:when_
   #:whenM
   #:when-val
   #:when-valM
   #:if%
   #:if-val
   #:if-val_
   #:if-valM
   #:map-success
   #:map-successM
   #:flatmap-success
   #:flatmap-successM

   #:do-when
   #:do-whenM
   #:do-when-val
   #:do-when-valM
   #:do-if
   #:do-if-val
   #:do-if-val_
   #:do-if-valM
   #:do-map-success
   #:do-map-successM
   #:do-flatmap-success
   #:do-flatmap-successM

   #:matchM
   #:do-when-match
   #:do-if-match
   )
  )

(in-package :do-control/core)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-class (Terminator :a)
    "Represents a value that terminates a control flow."
    (ended? (:a -> Boolean)))

  (define-instance (Terminator Boolean)
    (inline)
    (define ended? id))

  (define-instance (Terminator (Optional :a))
    (inline)
    (define ended? opt:none?))

  (define-instance (Terminator (Result :e :a))
    (inline)
    (define ended? rst:err?))

  (define-instance (Terminator (List :a))
    (inline)
    (define ended? l:null?))

  (define-class (Traversable :y => Yielder :y)
    "A data type that can terminate or yield a value into control flow."
    (yield (:y :a -> Optional :a))
    (concat-mapA (Applicative :m => :y :a -> (:a -> :m (:y :b)) -> :m (:y :b)))
    (wrap-success (:a -> :y :a)))

  (define-instance (Yielder Optional)
    (define yield id)
    (inline)
    (define (concat-mapA opt fa->mopt-b)
      (match opt
        ((None) (pure None))
        ((Some a) (fa->mopt-b a))))
    (define wrap-success Some))

  (define-instance (Yielder (Result :e))
    (inline)
    (define (yield res)
      (match res
        ((Ok a) (Some a))
        ((Err _) None)))
    (inline)
    (define (concat-mapA res fa->mres-b)
      (match res
        ((Err e) (pure (Err e)))
        ((Ok a) (fa->mres-b a))))
    (define wrap-success Ok))

  (define-instance (Yielder List)
    (define yield l:head)
    (inline)
    (define (concat-mapA lst fa->mlst-b)
      (map (fn (x) (>>= x id)) (traverse fa->mlst-b lst)))
    (inline)
    (define (wrap-success x)
      (make-list x)))
  )

(coalton-toplevel

  ;;
  ;; Single Execution
  ;;

  (inline)
  (declare when_ ((Monad :m) (Terminator :t) => :t -> :m :z -> :m Unit))
  (define (when_ term? m)
    (if (ended? term?)
        (do m (pure Unit))
        (pure Unit)))

  (inline)
  (declare whenM ((Monad :m) (Terminator :t) => :m :t -> :m :z -> :m Unit))
  (define (whenM mterm? mop)
    (do
     (term? <- mterm?)
     (when_ term? mop)))

  (inline)
  (declare when-val ((Monad :m) (Yielder :y) => :y :a -> (:a -> :m :z) -> :m Unit))
  (define (when-val val? f->m)
    (match (yield val?)
      ((None)
       (pure Unit))
      ((Some x)
       (do
        (f->m x)
        (pure Unit)))))

  (inline)
  (declare when-valM ((Monad :m) (Yielder :y) => :m (:y :a) -> (:a -> :m :z) -> :m Unit))
  (define (when-valM mval? f->m)
    (do
     (val? <- mval?)
     (when-val val? f->m)))

  (inline)
  (declare if% ((Monad :m) (Terminator :t) => :t -> :m :b -> :m :b -> :m :b))
  (define (if% val? m-true m-false)
    (if (ended? val?)
        m-true
        m-false))

  (inline)
  (declare if-val ((Monad :m) (Yielder :y) => :y :a -> (:a -> :m :b) -> :m :b -> :m :b))
  (define (if-val val? f-mval m-none)
    (match (yield val?)
      ((None)
       m-none)
      ((Some x)
       (f-mval x))))

  (inline)
  (declare if-val_ ((Monad :m) (Yielder :y) => :y :a -> (:a -> :m :b) -> :m :c -> :m Unit))
  (define (if-val_ val? f-mval m-none)
    (match (yield val?)
      ((None)
       (do m-none (pure Unit)))
      ((Some x)
       (do (f-mval x) (pure Unit)))))

  (inline)
  (declare if-valM ((Monad :m) (Yielder :y) => :m (:y :a) -> (:a -> :m :b) -> :m :b -> :m :b))
  (define (if-valM mval? f-mval m-none)
    (do
     (val? <- mval?)
     (if-val val? f-mval m-none)))

  (inline)
  (declare map-success ((Monad :m) (Yielder :y) => :y :a -> (:a -> :m :b) -> :m (:y :b)))
  (define (map-success val? f->mb)
    (traverse f->mb val?))

  (inline)
  (declare map-successM ((Monad :m) (Yielder :y) => :m (:y :a) -> (:a -> :m :b) -> :m (:y :b)))
  (define (map-successM mval? f->mb)
    (do
     (val? <- mval?)
     (map-success val? f->mb)))

  (inline)
  (declare flatmap-success ((Monad :m) (Yielder :y) => :y :a -> (:a -> :m (:y :b)) -> :m (:y :b)))
  (define flatmap-success concat-mapA)

  (inline)
  (declare flatmap-successM ((Monad :m) (Yielder :y) => :m (:y :a) -> (:a -> :m (:y :b)) -> :m (:y :b)))
  (define (flatmap-successM mval? f->mval?b)
    (do
     (val? <- mval?)
     (flatmap-success val? f->mval?b)))

  ;;
  ;; Loops
  ;;

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

(cl:defmacro do-when (b cl:&body body)
  `(when_ ,b
    (do
     ,@body)))

(cl:defmacro do-whenM (m-term cl:&body body)
  `(whenM ,m-term
    (do
     ,@body)))

(cl:defmacro do-when-val ((sym opt) cl:&body body)
  `(when-val ,opt
    (fn (,sym)
      (do
       ,@body))))

(cl:defmacro do-when-valM ((sym opt) cl:&body body)
  `(when-valM ,opt
    (fn (,sym)
      (do
       ,@body))))

(cl:defmacro do-if (term true-body cl:&body none-body)
  `(if% ,term
    ,true-body
    (do
     ,@none-body)))

(cl:defmacro do-if-val ((sym opt) some-body cl:&body none-body)
  `(if-val ,opt
    (fn (,sym)
      ,some-body)
    (do
     ,@none-body)))

(cl:defmacro do-if-val_ ((sym opt) some-body cl:&body none-body)
  `(if-val_ ,opt
    (fn (,sym)
      ,some-body)
    (do
     ,@none-body)))

(cl:defmacro do-if-valM ((sym opt) some-body cl:&body none-body)
  `(if-valM ,opt
    (fn (,sym)
      ,some-body)
    (do
     ,@none-body)))

(cl:defmacro do-map-success ((sym val?) cl:&body body)
  `(map-success
     ,val?
     (fn (,sym)
       (do
        ,@body))))

(cl:defmacro do-map-successM ((sym val?) cl:&body body)
  `(map-successM
     ,val?
     (fn (,sym)
       (do
        ,@body))))

(cl:defmacro do-flatmap-success ((sym val?) cl:&body body)
  `(flatmap-success
    ,val?
    (fn (,sym)
      (do
       ,@body))))

(cl:defmacro do-flatmap-successM ((sym val?) cl:&body body)
  `(flatmap-successM
    ,val?
    (fn (,sym)
      (do
       ,@body))))

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

(cl:defmacro do-when-match (scrut match-form cl:&body match-body)
  `(match ,scrut
     (,match-form
      (do
       ,@match-body
       (pure Unit)))
     (_ (pure Unit))))

(cl:defmacro do-if-match (scrut match-form match-body cl:&body rest-body)
  `(match ,scrut
     (,match-form ,match-body)
     (_ ,@rest-body)))

(cl:defmacro matchM (m cl:&body body)
  (cl:let ((sym (cl:gensym)))
    `(do
      (,sym <- ,m)
      (match ,sym
        ,@body))))
