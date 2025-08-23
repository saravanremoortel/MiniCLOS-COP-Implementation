(in-package :closless)

; definition of generic functions and methods
(defstruct generic-function
  (methods nil)
  (arguments nil)
  (argument-precedence-order nil))

(defstruct method
  (specializers nil)
  (function (error "No method function provided."))
  (qualifier nil)
  (layer nil)) ; qualifier are either :before, :after or :around


; ------------- UTILITY FUNCTIONS FOR METHODS AND GENERIC FUNCTIONS -------------

(defun method-applicable-p (method arg-types)
  (let ((specializers (method-specializers method)))
    (and (= (length arg-types) (length specializers)) ; length arguments should match specializers
         (loop for arg-type in arg-types
               for spec in specializers
               always (subclassp arg-type spec))))) ; the argument type should be a subclass of the specializer type (meaning the argument is an instance of the specializer)


(defun find-method (gf specializers qualifier layer)
  (loop for method in (generic-function-methods gf)
        when (and (equal specializers (method-specializers method))
                  (eql qualifier (method-qualifier method))
                  (eql layer (method-layer method)))
        return method))

(defun remove-method (gf method)
  (setf (generic-function-methods gf)
        (remove method (generic-function-methods gf))))

(defun add-method (gf method)
  (let ((old-method (find-method gf (method-specializers method) (method-qualifier method) (method-layer method))))
    (when old-method
      (remove-method gf old-method))              ; remove old method if it exists
    (push method (generic-function-methods gf)))) ; add the new method