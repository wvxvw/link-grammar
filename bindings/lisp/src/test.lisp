;; -*- mode: lisp; package: link-grammar; fill-column: 80 -*-

(in-package :link-grammar)

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

(defun test-roundtrip ()
  (with-dictionary (en)
    (with-options ()
      (iter
        (for saying :in *sentences*)
        (for dia :in *diagrams*)
        (collect
            (with-sentence (saying)
              (with-linkage ()
                (format t "~%diagram: ~s" (print-diagram *linkage*))
                (string= dia (print-diagram *linkage*)))))))))
