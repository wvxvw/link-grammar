;; -*- mode: lisp; package: link-grammar.test -*-

(in-package :link-grammar.test)

(in-suite :link-grammar.test)

(def-suite test-suite
    :description "Minimal testing suite for link-grammar bindings.")

(defparameter *diagrams*
  (list "
    +------------------------------------Xx------------------------------------+                       
    |                          +-------------------B*w-------------------+     |                       
    |                          +-----------------TOt----------------+    |     +----------Xp----------+
    |                          |        +------CV----->+            |    |     +--------Wa-------+    |
    +----Wd----+---Ss---+--Pa--+---MVs--+---Cs--+-SFst-+-Ost-+      +-I*t+     |       +----G----+    |
    |          |        |      |        |       |      |     |      |    |     |       |         |    |
LEFT-WALL grammar.n-u is.v useless.a because there.r is.v nothing to.r say.v --.r Gertrude.f Stein[!] . 

"
        "
                                      +-------------------Xx-------------------+                      
    +----------------Xx---------------+-------->WV-------->+                   +----------Xp---------+
    +-------->WV------->+             |        +-----I-----+----Opn----+       +-------Wa------+     |
    +----Wd----+---Spx--+---Pa--+     +-Wd+-Sp-+     +--E--+-Ox-+      |       |     +----G----+     |
    |          |        |       |     |   |    |     |     |    |      |       |     |         |     |
LEFT-WALL computers.n are.v useless.a ; they can.v only give.v you answers.n --.r Pablo.m Picasso[!] . 

"))

(defvar *sentences*
  (list "Grammar is useless because there is nothing to say -- Gertrude Stein."
        "Computers are useless; they can only give you answers -- Pablo Picasso."))

(test test-hello-world
  "Prints two diagrams and makes sure that the results are ex expected"
  (is
   (every 'identity
          (lg:with-dictionary (en)
            (lg:with-options ()
              (iter
                (for saying :in *sentences*)
                (for dia :in *diagrams*)
                (collect
                    (lg:with-sentence (saying)
                      (lg:with-linkage ()
                        (string= dia (lg:print-diagram lg:*linkage*)))))))))))
