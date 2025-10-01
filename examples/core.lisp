(cl:in-package :cl-user)
(defpackage :do-control/examples/core
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/identity
   #:coalton-library/monad/statet
   #:coalton-library/monad/environment
   #:coalton-library/list
   #:do-control/core))

(in-package :do-control/examples/core)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-type-alias (ST :s) (StateT :s Identity))

  (declare demo-when (Boolean -> ST (List Integer) Unit))
  (define (demo-when cond?)
    "Push 0 & 1 onto the state if a condition is true."
    (when_ cond?
           (do
            (modify (Cons 0))
            (modify (Cons 1)))))

  (declare demo-do-when (Boolean -> ST (List Integer) Unit))
  (define (demo-do-when cond?)
    "Push 0 & 1 onto the state if a condition is true."
    (do-when cond?
      (modify (Cons 0))
      (modify (Cons 1))))

  (declare state-empty? (ST (List Integer) Boolean))
  (define state-empty?
    (map mempty? get))

  (declare demo-whenM (Integer -> ST (List Integer) Unit))
  (define (demo-whenM def)
    "Push a default int twice onto the state if it is empty."
    (whenM state-empty?
           (do
            (modify (Cons def))
            (modify (Cons def)))))

  (declare demo-do-whenM (Integer -> ST (List Integer) Unit))
  (define (demo-do-whenM def)
    "Push a default int twice onto the state if it is empty."
    (do-whenM state-empty?
      (modify (Cons def))
      (modify (Cons def))))

  (declare demo-when-val (ST (List Integer) Unit))
  (define demo-when-val
    "Push the head of the list, if non-empty, back onto the list twice."
    (do
     (lst <- get)
     (when-val (head lst)
      (fn (x)
        (do
         (modify (Cons x))
         (modify (Cons x)))))))

  (declare demo-do-when-val (ST (List Integer) Unit))
  (define demo-do-when-val
    "Push the head of the list, if non-empty, back onto the list twice."
    (do
     (lst <- get)
     (do-when-val (x (head lst))
       (modify (Cons x))
       (modify (Cons x)))))

  (declare state-head (ST (List Integer) (Optional Integer)))
  (define state-head
    (map head get))

  (declare demo-when-valM (ST (List Integer) Unit))
  (define demo-when-valM
    "Push the head of the list, if non-empty, back onto the list twice."
    (when-valM state-head
               (fn (x)
                 (do
                  (modify (Cons x))
                  (modify (Cons x))))))

  (declare demo-do-when-valM (ST (List Integer) Unit))
  (define demo-do-when-valM
    "Push the head of the list, if non-empty, back onto the list twice."
    (do-when-valM (x state-head)
      (modify (Cons x))
      (modify (Cons x))))

  )
