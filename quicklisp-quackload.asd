;;;; quicklisp-quackload.asd

(asdf:defsystem #:quicklisp-quackload
  :description "A system that adds duck typing to the Quicklisp client"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "Unlicense"
  :version "0.0.1-quack"
  :serial t
  :depends-on (#:trivial-gray-streams)
  :components ((:file "quicklisp-quackload")))
