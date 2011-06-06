;;;; qt-tutorial-app.asd

(asdf:defsystem #:qt-tutorial-app
  :serial t
  :depends-on (#:qt)
  :components ((:file "application")))
