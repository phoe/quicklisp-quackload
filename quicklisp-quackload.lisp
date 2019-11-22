;;;; quicklisp-quackload.lisp

(defpackage #:quicklisp-quackload
  (:use #:cl #:trivial-gray-streams))

(in-package #:quicklisp-quackload)

(defclass quack-stream (fundamental-character-output-stream)
  ((state :initform :empty :accessor state)
   (output :initform *standard-output* :accessor output)))

(defmethod stream-line-column ((stream quack-stream)))

(defmethod stream-start-line-p ((stream quack-stream)))

(defun write-quack (state output)
  (let ((quacks (case state
                  (:empty '(""))
                  (:first-upcase '("Q"))
                  (:first-downcase '("q"))
                  (:capitalized '("Quack"))
                  (:all-upcase '("QUACK"))
                  (:all-downcase '("quack"))
                  (:mixed '("QuAcK" "qUaCk" "QuacK" "qUACk" "qUack" "quacK")))))
    (princ (elt quacks (random (length quacks))) output)))

(defmethod stream-write-char ((stream quack-stream) char)
  (with-slots (state output) stream
    (cond
      ((alpha-char-p char)
       (case state
         (:empty
          (cond ((upper-case-p char)
                 (setf state :first-upcase))
                ((lower-case-p char)
                 (setf state :first-downcase))))
         (:first-downcase
          (cond ((upper-case-p char)
                 (setf state :mixed))
                ((lower-case-p char)
                 (setf state :all-downcase))))
         (:first-upcase
          (cond ((upper-case-p char)
                 (setf state :all-upcase))
                ((lower-case-p char)
                 (setf state :capitalized))))
         (:capitalized
          (cond ((upper-case-p char)
                 (setf state :mixed))
                ((lower-case-p char))))
         (:all-upcase
          (cond ((upper-case-p char))
                ((lower-case-p char)
                 (setf state :mixed))))
         (:all-downcase
          (cond ((upper-case-p char)
                 (setf state :mixed))
                ((lower-case-p char))))
         (:mixed
          (cond ((upper-case-p char))
                ((lower-case-p char))))))
      (t
       (write-quack state output)
       (write-char char output)
       (setf state :empty)))))

(defun quackload (systems &rest rest &key verbose silent prompt explain &allow-other-keys)
  (declare (ignore verbose silent prompt explain))
  (let* ((quack-stream (make-instance 'quack-stream))
         (*standard-output* quack-stream)
         (*trace-output* quack-stream)
         (ql-package (find-package "QUICKLISP")))
    (when ql-package
      (let ((ql-quickload (find-symbol "QUICKLOAD" ql-package)))
        (apply ql-quickload systems rest)))))

(let* ((ql-package (find-package "QUICKLISP")))
  (when ql-package
    (let((ql-quackload (intern "QUACKLOAD" ql-package)))
      (export ql-quackload ql-package)
      (setf (fdefinition ql-quackload) #'quackload))))
