;;;; td.asd

(asdf:defsystem #:td
  :description "Describe td here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:local-time :defclass-std :defstar :iterate :binding-arrows :ubiquitous)
  :components ((:file "package")
               (:file "td")))
