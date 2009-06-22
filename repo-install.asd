(in-package :asdf)

(defpackage repo-install
  (:nicknames ri)
  (:use :cl)
  (:export #:install #:update-all-packages #:repo-status)
)

(asdf:defsystem repo-install
  :depends-on (trivial-http)
  :components ((:file "utilities")
	       (:file "vars")
	       (:file "installer"))
  :serial t)