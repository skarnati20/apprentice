;;;; package.lisp


(defpackage #:apprentice
  (:use #:cl)
  (:export #:chat
	   #:clear
	   #:add-allowed-dir
	   #:clear-allowed-dirs
	   #:resolve-loop))
