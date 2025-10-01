(cl:in-package :cl-user)
(defpackage :do-control/examples/hangman
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/identity
   #:coalton-library/monad/statet
   #:coalton-library/monad/environment
   #:do-control/core
   #:do-control/loops)
  (:local-nicknames
   (:lp #:do-control/loops-adv)
   (:i #:simple-io/io)
   (:it #:simple-io/term)
   (:itr #:coalton-library/iterator)
   (:l #:coalton-library/list)
   (:tp #:coalton-library/tuple)
   (:s #:coalton-library/string)))

(in-package :do-control/examples/hangman)

(named-readtables:in-readtable coalton:coalton)

;;
;; Helper Functions
;;

(coalton-toplevel
  (declare contains? (Eq :a => :a -> List :a -> Boolean))
  (define (contains? elt lst)
    (match lst
      ((Nil) False)
      ((Cons x rst)
       (if (== x elt)
           True
           (contains? elt rst)))))

  (declare str-contains? (Char -> String -> Boolean))
  (define (str-contains? c s)
    (match (itr:find! (== c) (s:chars s))
      ((Some _) True)
      ((None) False)))

  (declare chars-list (String -> List Char))
  (define (chars-list str)
    (itr:collect! (s:chars str)))
  )

;;
;; Hangman Program
;;
(coalton-toplevel
  (declare first-char (String -> Optional Char))
  (define (first-char str)
    (itr:next! (s:chars str)))

  (define-struct HangmanConf
    (num-guesses UFix)
    ;; We don't want to have to leave the dictionary in memory during
    ;; the whole program, so we ask for an IO operation that can get
    ;; a random word (possibly from a file) and return it.
    (get-random-word (i:IO String)))

  ;; Workaround for:
  ;; https://github.com/coalton-lang/coalton/issues/1656
  (declare get-random-word_ (HangmanConf -> HangmanM String))
  (define (get-random-word_ conf)
    (lift (lift (.get-random-word conf))))

  ;; Workaround for:
  ;; https://github.com/coalton-lang/coalton/issues/1656
  (declare num-guesses_ (HangmanConf -> UFix))
  (define (num-guesses_ conf)
    (.num-guesses conf))

  (define-struct HangmanState
    (guessed-chars (List Char))
    (num-wrong-guesses UFix))

  ;; Workaround for:
  ;; https://github.com/coalton-lang/coalton/issues/1656
  (declare guessed-chars_ (HangmanState -> List Char))
  (define (guessed-chars_ st)
    (.guessed-chars st))

  ;; Workaround for:
  ;; https://github.com/coalton-lang/coalton/issues/1656
  (declare num-wrong-guesses_ (HangmanState -> UFix))
  (define (num-wrong-guesses_ st)
    (.num-wrong-guesses st))

  (define-type-alias HangmanM (EnvT HangmanConf (StateT HangmanState i:IO)))

  (declare run-hangman (HangmanConf -> HangmanM :a -> :a))
  (define (run-hangman conf m)
    (i:run!
     (map tp:snd
          (run-stateT
           (run-envT m conf)
           (HangmanState
            Nil
            0)))))

  (declare store-bad-guess (Char -> HangmanM Unit))
  (define (store-bad-guess c)
    (do
     (st <- get)
     (put (HangmanState
           (Cons c (guessed-chars_ st))
           (+ 1 (num-wrong-guesses_ st))))))

  (define-type Guess
    (WordGuess String)
    (LetterGuess Char)
    (InputError String))

  ;; TODO: Post issue for this. Should be able to use HangmanState alias here.
  (declare parse-guess (MonadState HangmanState :m => String -> :m Guess))
  (define (parse-guess input)
    (if (> (s:length input) 1)
        (pure (WordGuess input))
        (match (first-char input)
          ((None) (pure (InputError "Must enter a guess!")))
          ((Some c)
           (do
            (already-guessed <- (map guessed-chars_ get))
            (if (contains? c already-guessed)
                (pure (InputError (<> (<> "Already guessed " (into c)) "!")))
                (pure (LetterGuess c))))))))

  (declare do-letter-guess (String -> Char -> HangmanM Unit))
  (define (do-letter-guess secret-word c)
    (do
     (st <- get)
     (put (HangmanState
           (Cons c (guessed-chars_ st))
           (if (str-contains? c secret-word)
               (num-wrong-guesses_ st)
               (+ 1 (num-wrong-guesses_ st)))))))

  (declare write-status (String -> HangmanM Unit))
  (define (write-status secret-word)
    (do
     (it:write-line "Secret Word:")
     (i:do-foreach-io (_c (chars-list secret-word))
       (pure Unit))))

  (declare hangman (HangmanM Unit))
  (define hangman
    (do
     (get-random-word <- (asks get-random-word_))
     (secret-word <- get-random-word)
     (it:write-line "Please enter a full word to make a guess at the answer.")
     (it:write-line "Otherwise, enter a single letter to make a letter guess.")
     (lp:do-loop
       (input <- it:read-line)
       (matchM (parse-guess input)
         ((InputError msg)
          (do
           (it:write-line (<> "Invalid guess: " msg))
           lp:continue-loop))
         ((LetterGuess c)
          (lift (do-letter-guess secret-word c)))
         ((WordGuess w)
          (do-if (== w secret-word)
              (do
               (it:write-line (<> "The word was " secret-word))
               (it:write-line "You won!")
               lp:break-loop)
            (do
             (st <- get)
             (put (HangmanState
                   (guessed-chars_ st)
                   (+ 1 (num-wrong-guesses_ st))))))))
       (guessed <- get)
       (it:write-line "You have now guessed:")
       (it:write-line (guessed-chars_ guessed))))))

(cl:defun play ()
  (coalton (run-hangman (HangmanConf 4 (i:wrap-io "pizza")) hangman)))
