;; -*- mode: lisp; fill-column: 80 -*-

(in-package :link-grammar.doc)

(defun generate-htdoc (path)
  (let ((dest (cl-fad:merge-pathnames-as-file path #p"./doc/html/"))) 
    (ensure-directories-exist dest)
    (atdoc:generate-html-documentation
     (list :link-grammar)
     dest
     :index-title "link-grammar bindings API reference"
     :heading "link-grammar bindings")))
