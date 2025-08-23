(in-package :closless)

(define-layered-class person ()
  (name))

(deflayer employment-layer)

(define-layered-class employer
  :in-layer employment-layer ()
  (name))
  
(define-layered-class person
  :in-layer employment-layer ()
  (employer))

(define-layered-function display-object (object))

(define-layered-method display-object
  :in-layer T ((object person))
  (format t "Person~%")
  (format t "Name: ~A~%" (slot-value object 'name)))

(define-layered-method display-object
  :in-layer employment-layer
  ((object employer))
  (format t "Employer~%")
  (format t "Name: ~A~%" (slot-value object 'name)))

(define-layered-method display-object
  :in-layer employment-layer :after
  ((object person))
  (display-object (slot-value object 'employer)))



(defvar *vub* (make-instance 'employer))
(with-active-layers (employment-layer)
  (setf (slot-value *vub* 'name) "Vrije Universiteit Brussel"))


(defvar *pascal* (make-instance 'person))
(with-active-layers (employment-layer)
  (setf (slot-value *pascal* 'name) "Pascal Costanza")
  (setf (slot-value *pascal* 'employer) *vub*))

(display-object *pascal*)
(format t "~%") ; newline

(with-active-layers (employment-layer)
  (display-object *pascal*))
(format t "~%") ; newline

(deflayer info-layer)

(define-layered-class info-mixin
  :in-layer info-layer ()
  (address))

(define-layered-method display-object
  :in-layer info-layer :after ((object info-mixin))
  (format t "Address: ~A~%" (slot-value object 'address)))

(define-layered-class person
  :in-layer info-layer (info-mixin)
  ())

(define-layered-class employer
  :in-layer info-layer (info-mixin)
  ())

(defvar *docomo* (make-instance 'employer))
(with-active-layers (employment-layer info-layer)
  (setf (slot-value *docomo* 'name) "DoCoMo Euro-Labs")
  (setf (slot-value *docomo* 'address) "Munich"))

(defvar *robert* (make-instance 'person))
(with-active-layers (employment-layer info-layer)
  (setf (slot-value *robert* 'name) "Robert Hirschfeld")
  (setf (slot-value *robert* 'employer) *docomo*)
  (setf (slot-value *robert* 'address) "Ilmenau"))


(with-active-layers (employment-layer)
  (display-object *robert*))
(format t "~%") ; newline

(with-active-layers (employment-layer info-layer)
  (display-object *robert*))
(format t "~%") ; newline
