;;; do-control.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Jason Walker

(asdf:defsystem #:coalton-do-control
  :description ""
  :author "Jason Walker"
  :maintainer "Jason Walker"
  :mailto "Jason0@pm.me"
  :license     "MIT"
  :version     "0.1.0"
  :depends-on ("coalton" "named-readtables" "coalton-simple-io")
  :components ((:module "src"
                :serial t
                :components
                ((:file "core")
                 (:file "loops")
                 (:file "loops-adv"))))
  :description ""
  :long-description ""
  :in-order-to ((test-op (test-op "coalton-do-control/tests"))))

(defsystem "coalton-do-control/tests"
  :author "Jason Walker"
  :license "MIT"
  :depends-on ("coalton-do-control"
               "coalton/testing"
               "fiasco")
  :components ((:module "tests"
                :components
                ((:file "main")
                 (:file "package"))))
  :description "Test system for coalton-do-control"
  :perform (test-op (op c) (symbol-call '#:coalton-do-control/tests '#:run-tests)))

(asdf:defsystem #:coalton-do-control/examples
  :description ""
  :author "Jason Walker"
  :maintainer "Jason Walker"
  :mailto "Jason0@pm.me"
  :license     "MIT"
  :version     "0.1.0"
  :depends-on ("coalton-do-control")
  :components ((:module "examples"
                :components
                ((:file "core")
                 (:file "hangman"))))
  :description ""
  :long-description ""
  :in-order-to ((test-op (test-op "coalton-do-control/tests"))))
