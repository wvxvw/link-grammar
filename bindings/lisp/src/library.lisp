;; -*- mode: lisp; package: link-grammar -*-

(in-package :link-grammar)

(define-foreign-library link-grammar
  (:unix (:or #p"/usr/local/lib/liblink-grammar.so"
              #p"/usr/local/lib/liblink-grammar.so.5"
              #p"/usr/local/lib/liblink-grammar.so.5.3.16"))
  (t (:default "liblink-grammar")))

(pushnew #P"/" *foreign-library-directories*
         :test #'equal)

(use-foreign-library link-grammar)
