;; -*- mode: lisp; fill-column: 80 -*-

;;; Commentary:

;; ------------------------------------------------------------------------
;; This code is a collection of ANSI Common Lisp bindings for the link-grammar
;; library which can be found here:
;; <http://www.abisource.com/projects/link-grammar/>
;; These bindings were partially generated using SWIG:
;; <http://www.swig.org/index.php> with more editing added by:
;; Copyright (C) 2014 Oleg Sivokon
;; Copyright (C) 2014 Zach Kost-Smith

;; This library is free software; you can redistribute it and/or modify it under
;; the terms of the GNU Lesser General Public License as published by the Free
;; Software Foundation; either version 2.1 of the License, or (at your option)
;; any later version.

;; This library is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
;; details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;; ------------------------------------------------------------------------

;; These bindings have been compiled against library version 5.0.8

;;; Versions:
;;
;;    0.0.0 - Not yet released.

;;; Usage:
;;
;; Load the system using Quicklisp:
;; (ql:quickload :link-grammar)
;;
;; This "hello world" example should get you started:
;; (in-package :link-grammar)
;; 
;; (defun test-hello-world ()
;;   (with-dictionary (en)
;;     (with-options (opts)
;;       (loop :for saying :in
;;          (list "Grammar is useless because there is nothing to say -- Gertrude Stein."
;;                "Computers are useless; they can only give you answers -- Pablo Picasso.")
;;          :do (with-sentence (sent saying)
;;                (split sent opts)
;;                (with-linkage (link)
;;                  (format t "~&diagram:~&~s" (print-diagram link))))))))
;;
;; Generate documentation:
;; (asdf:oos 'link-grammar-asd::doc-op :link-grammar)
;;
;; Run tests:
;; (asdf:oos 'asdf:test-op :link-grammar)


;;; Code:

(in-package :cl)
(defpackage link-grammar-asd
  (:use :cl :asdf)
  (:documentation "ASDF System package for link-grammar bindings."))
(in-package :link-grammar-asd)

(defclass doc-op (asdf:operation) ())

(defmethod asdf:perform ((op doc-op) (c asdf:component))
  (funcall (intern (string '#:generate-htdoc) :link-grammar.doc)
           (asdf:component-pathname c)))

(defsystem link-grammar
  :version "0.1"
  :author ("Zach Kost-Smith <zachkostsmith@gmail.com>"
           "Oleg Sivokon <olegsivokon@gmail.com>")
  :license "LGPL2"
  :depends-on (:alexandria :iterate :cffi :closer-mop :trivial-garbage)
  :serial t
  :components ((:module "src"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "library")
                         (:file "wrapper")
                         (:file "objects"))))
  :description "Bindings for link-grammar parser:
                http://www.abisource.com/projects/link-grammar/"
  :long-description
  #.(with-open-file
        (stream (merge-pathnames
                 #p"README.org" (or *load-pathname* *compile-file-pathname*))
                :if-does-not-exist nil :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream)) seq)))
  :in-order-to ((test-op (load-op :link-grammar-test))
                (doc-op (load-op :link-grammar-doc)))
  :perform (test-op :after (op c)
                    (funcall (intern (string '#:run!) :link-grammar.test)
                             :link-grammar.test)))

(defsystem :link-grammar-test
  :author ("Zach Kost-Smith <zachkostsmith@gmail.com>"
           "Oleg Sivokon <olegsivokon@gmail.com>")
  :description "Minimal test suite for testing link-grammar bindings"
  :license "LGPL2"
  :depends-on (:link-grammar :fiveam :iterate)
  :components ((:module "tests"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "suite" :depends-on ("package"))
                         (:file "test-lg" :depends-on ("suite"))))))

(defsystem :link-grammar-doc
  :author ("Zach Kost-Smith <zachkostsmith@gmail.com>"
           "Oleg Sivokon <olegsivokon@gmail.com>")
  :description "Documentation for link-grammar bindings"
  :license "LGPL2"
  :depends-on (:link-grammar :atdoc :cl-fad)
  :components ((:module "doc"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "gendoc" :depends-on ("package"))))))

;;;;@include "src/package.lisp"

;;;;@include "src/library.lisp"

;;;;@include "src/wrapper.lisp"

;;;;@include "src/ojbects.lisp"
