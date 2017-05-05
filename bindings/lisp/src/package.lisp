(defpackage :link-grammar
  (:nicknames :lg)
  (:use :cl :iterate :cffi)
  ;; (:shadow :length)
  ;; macros
  (:export :with-dictionary
           :with-sentence
           :with-linkage
           :with-options
           ;; methods
           :timer-expired-p
           :memory-exhausted-p
           :resource-exhausted-p
           :parse
           :split
           :sentence-length
           :null-count
           :linkages
           :num-violations
           :print-diagram
           :linkage-print-length
           :print-postscript
           :left-word
           :right-word
           :link-length
           :link-label
           :num-domains
           :domain-names
           :word
           :print-links-and-domains
           :print-senses
           :print-constituent-tree
           ;; slot accessors
           :link-cost
           :words
           :num-words
           :num-links
           :unused-word-cost
           :disjunct-cost
           :corpus-cost
           :violation-name
           :verbosity
           :linkage-limit
           :min-null-count
           :max-null-count
           :islandsp
           :short-length
           :max-memory
           :max-parse-time
           :cost-model-type
           :display-morphology-p
           :spell-guess-p
           :all-short-connectors-p
           ;; classes
           :dictionary
           :sentence
           :linkage
           :parse-options
           ;; special variables
           :*link-index*
           :*sentence-index*
           :*options*
           :*sentence*
           :*linkage*
           ;; functions
           :data-dir)
  (:documentation
   "@a[http://www.abisource.com/projects/link-grammar/]{link-grammar}
    Provides Common Lisp bindings for link-grammar library.

    These bindings provide thin wrapper and a simple CLOS abstraction
    layer on top of it.

    @begin[Macors]{section}
    The library provides these macros

    @aboutfun{with-sentence}
    @aboutfun{with-linkage}
    @aboutfun{with-options}
    @aboutfun{with-sentence}

    To make management of C++ objects easier.
    @end{section}

    @begin[Classes]{section}

    @aboutclass{dictionary}
    @aboutclass{sentence}
    @aboutclass{parse-options}
    @aboutclass{linkage}

    These classes map to their C++ prototypes. These classes also
    use special meta-class that allows them to have virtual slots.
    Virtual slots behave in all respects as regular slots, except
    that the data isn't stored in Lisp memory, instead, whenever it
    is stored or accessed, a special function is invoked to store
    or access it in C++ environment.
    @end{section}

    @begin[Example]{section}

    @begin{pre}
\(in-package :link-grammar)

\(defun test-hello-world ()
  (with-dictionary (en)
    (with-options (opts)
      (loop :for saying :in
         (list \"Grammar is useless because there is nothing to say -- Gertrude Stein.\"
               \"Computers are useless; they can only give you answers -- Pablo Picasso.\")
         :do (with-sentence (sent saying)
               (split sent opts)
               (with-linkage (link)
                 (format t \"~&diagram:~&~s\" (print-diagram link))))))))
    @end{pre}

    The example above will produce the following output:

    @begin{pre}
diagram:
\"
    +-----------------------------------------------Xp-----------------------------------------------+
    +------------------------------------Xx-----------------------------------+                      |
    |                         +-------------------B*d-------------------+     |                      |
    |                         +-----------------TOt----------------+    |     |                      |
    +------->WV------->+      |        +------CV----->+            |    |     +--------Js-------+    |
    +----Wd----+---Ss--+--Pa--+---MVs--+---Cs--+-SFst-+-Ost-+      +--I-+     |       +----G----+    |
    |          |       |      |        |       |      |     |      |    |     |       |         |    |
LEFT-WALL Grammar[!] is.v useless.a because there.r is.v nothing to.r say.v --.r Gertrude.f Stein[!] . 

\"
diagram:
\"
    +----------------Xx---------------+-------->WV-------->+           +----------MXp----------+      
    +-------->WV------->+             |        +-----I-----+----Opn----+       +-------Xd------+      
    +----Wd----+---Spx--+---Pa--+     +-Wd+-Sp-+     +--E--+-Ox-+      |       |     +----G----+--Xc-+
    |          |        |       |     |   |    |     |     |    |      |       |     |         |     |
LEFT-WALL computers.n are.v useless.a ; they can.v only give.v you answers.n --.r Pablo.m Picasso[!] . 

\"
    @end{pre}
    @end{section}"))
