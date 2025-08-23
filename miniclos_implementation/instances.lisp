(in-package :closless)

(defstruct object
  (class standard-object)
  (slots (make-hash-table)))

(defun instancep (object class)
  (if (not (object-p object))
    (eq T class) ; if it is not a class (created using make-class) then false, exept when it is T
    (subclassp (object-class object) class)))
    
(defun slot-exists-p (object slot-name)
  (let ((class (object-class object)))
    (member slot-name (class-all-slots class))))

(defun slot-value (object slot-name)
  (if (slot-exists-p object slot-name)
    (gethash slot-name (object-slots object))
    (error "There exists no slot by that name for this object")))

(defun (setf slot-value) (value object slot-name) ; example: (setf (slot-value my-object 'name) "John")
  (if (slot-exists-p object slot-name)
    (setf (gethash slot-name (object-slots object)) value)
    (error "There exists no slot by that name for this object")))
