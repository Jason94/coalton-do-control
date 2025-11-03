# Do-Control

```lisp
  (declare hangman (HangmanM Unit))
  (define hangman
    (do
     (get-random-word <- (asks get-random-word_))
     (secret-word <- get-random-word)
     (i:write-line "Please enter a full word to make a guess at the answer.")
     (i:write-line "Otherwise, enter a single letter to make a letter guess.")
     (lp:do-loop
       (input <- i:read-line)
       (matchM (parse-guess input)
         ((InputError msg)
          (i:write-line (<> "Invalid guess: " msg)))
         ((LetterGuess c)
          (enter-letter-guess secret-word c))
         ((WordGuess w)
          (do-if (/= w secret-word)
              (modify inc-wrong-guesses)
            (i:write-line (<> "The word was " secret-word))
            (i:write-line "You won!")
            lp:break-loop)))
       (do-whenM over-and-failed?
         (i:write-line failure-msg)
         lp:break-loop)
       (write-status secret-word))
      ))
```
