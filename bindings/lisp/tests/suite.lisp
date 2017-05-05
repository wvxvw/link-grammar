(in-package :link-grammar.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (get-test :link-grammar)
    (def-suite :link-grammar)))

(def-suite :link-grammar.test :in :link-grammar)
