;;;; td.asd

(asdf:defsystem #:td
  :description "Simple todo list program"
  :author "semi <semilin@disroot.org>"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (:local-time :defclass-std :defstar :iterate :binding-arrows :ubiquitous :cl-ppcre)
  :components ((:module "src"
		:components
		((:file "package")
		 (:file "td")
		 (:file "filters")))))
