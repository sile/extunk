(in-package :asdf)

(defsystem extunk
  :name "extunk"
  :version "0.0.1"
  :author "Takeru Ohta" 
  :description "A unknown word extraction module"

  :depends-on (:common-utils :igo)
  :serial t
  :components ((:file "package")
               (:file "corpus")))
