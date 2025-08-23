(in-package :closless)

; definition of next-method-data object which is used in method calling
(defstruct next-method-data
  (call-phase nil) ; call-phase = member of '(:around :standard :primary)
  (around-methods nil)
  (before-methods nil)
  (primary-methods nil)
  (after-methods nil)
  (generic-function nil)
  (parameters nil))

(defvar *next-method-data* (make-next-method-data)) 

; ---------------- START OF METHOD CALLING ----------------
(defun call-generic-function (gf &rest args)
  (let* ((applicable-methods (filter-methods-by-active-layers (compute-applicable-methods gf args)))
         (around-methods  (get-sorted-methods-by-qualifier gf applicable-methods args :around))
         (before-methods  (get-sorted-methods-by-qualifier gf applicable-methods args :before))
         (primary-methods (get-sorted-methods-by-qualifier gf applicable-methods args nil))
         (after-methods   (reverse (get-sorted-methods-by-qualifier gf applicable-methods args :after)))
         (*next-method-data* (make-next-method-data
                              :call-phase :around
                              :around-methods around-methods
                              :before-methods before-methods
                              :primary-methods primary-methods
                              :after-methods after-methods
                              :generic-function gf
                              :parameters args)))
    (call-next-method)))

; CALL-NEXT-METHOD 
; this function does the actual logic of method calling
; we check in what call-phase we are, and call methods accordingly
(defun call-next-method (&rest args)
  (let ((current-phase (next-method-data-call-phase *next-method-data*))
        (current-parameters (if (null args) (next-method-data-parameters *next-method-data*) args))) ; use previous parameters if no new ones are provided
    (setf (next-method-data-parameters *next-method-data*) current-parameters) ; updating parameters in next-method-data
    (check-arguments-length (next-method-data-generic-function *next-method-data*) current-parameters) ; checking arguments length
    (case current-phase
      ; call the around methods if there are any, 
      ; if no next around method is available change the call-phase to :standard and do call-next-method with the current-parameters
      (:around
        (let ((first-around-method (first (next-method-data-around-methods *next-method-data*)))
              (rest-around-methods (rest (next-method-data-around-methods *next-method-data*))))
          (if (not (null first-around-method))
            (progn 
              (setf (next-method-data-around-methods *next-method-data*) rest-around-methods)
              (apply (method-function first-around-method) current-parameters)) ; call around method with current parameters
            (progn
              (setf (next-method-data-call-phase *next-method-data*) :standard)
              (call-next-method)))))

      ; call before methods if there are any, then update call-phase and do call-next-method
      ; then call after methods if there are any and return the result of the call-next-method (which will be the primary methods)
      (:standard
        (let ((before-methods (next-method-data-before-methods *next-method-data*))
              (after-methods (next-method-data-after-methods *next-method-data*)))
          (setf (next-method-data-call-phase *next-method-data*) nil)      ; 1. set call-phase to nil to block call-next-method in in before methods
          (apply-listof-methods before-methods current-parameters)         ; 2. call before methods
          (setf (next-method-data-call-phase *next-method-data*) :primary) ; 3. set call-phase to :primary
          (let* ((result (call-next-method)))                              ; 4. save the result of calling the primary methods
            (setf (next-method-data-call-phase *next-method-data*) nil)    ; 5. set call-phase to nil to block call-next-method in after methods
            (apply-listof-methods after-methods current-parameters)        ; 6. call after methods
            result)))                                                      ; 7. return the result of the primary methods

      ; if there are primary methods call them.
      ; if no primary method is available then throw error
      (:primary
        (let ((first-primary-method (first (next-method-data-primary-methods *next-method-data*)))
              (rest-primary-methods (rest (next-method-data-primary-methods *next-method-data*))))

          (when (null first-primary-method)
            (no-next-method)) ; if there is no primary method, throw error
          
          (setf (next-method-data-primary-methods *next-method-data*) rest-primary-methods)
          (apply (method-function first-primary-method) current-parameters))) ; call primary method with current parameters
      
      ; if the call-phase is nil throw error
      (otherwise (no-defined-method)))))

; ---------------- UTILITY FUNCTIONS ----------------

; converts an argument to its class, if the argument is not a miniclos class we assume it is of type T
(defun get-argument-type (argument)
  (if (object-p argument)
    (object-class argument)
    T))

; computing applicable methods via method-applicable-p from classes.lisp
(defun compute-applicable-methods (gf args)
  (let ((arg-types (mapcar #'get-argument-type args)))
    (loop for method in (generic-function-methods gf)
          when (method-applicable-p method arg-types)
          collect method)))

; filter methods by active layers, only return methods that are in the current active layers *active-layers*
(defun filter-methods-by-active-layers (methods)
  (remove-if-not 
    (lambda (method)
      (member (method-layer method) *active-layers* :test #'eq))
    methods))

; sorting methods by most specific specializer
(defun sort-methods-by-most-specific (gf methods args)
  (sort (copy-list methods) ; copy list to avoid modifying the original list
        (lambda (m1 m2)
          (more-specific-method gf m1 m2 args))))


; check if a method is more specific than another method
(defun more-specific-method (gf method1 method2 call-args)
  (let* ((plist-specializers1 (make-plist (generic-function-arguments gf) (method-specializers method1)))
         (plist-specializers2 (make-plist (generic-function-arguments gf) (method-specializers method2)))
         (plist-arguments     (make-plist (generic-function-arguments gf) call-args))
         (argument-precedence-order (generic-function-argument-precedence-order gf)))
    (loop for arg-name in argument-precedence-order
          for arg-keyword = (intern (symbol-name arg-name) :keyword)
          for spec1       = (getf plist-specializers1 arg-keyword) ; get the next specializer based on the next argument out the argument-precedence-order via the property list 
          for spec2       = (getf plist-specializers2 arg-keyword)
          for call-arg    = (getf plist-arguments arg-keyword)
          do (when (not (eq spec1 spec2)) ; when equal keep looping, if not, use more-specific-specializer to determine which one is more specific
              (return (more-specific-specializer method1 method2 spec1 spec2 call-arg)))))); returns nil (false) if the loop doesn't return anything

; determines which specializer is more specific based on the class precedence list of the argument type
(defun more-specific-specializer (method1 method2 specializer1 specializer2 arg)
  (let* ((class-precedence-list (get-class-precedence-list (get-argument-type arg)))
         (specializer1-pos (position specializer1 class-precedence-list :test #'eq))
         (specializer2-pos (position specializer2 class-precedence-list :test #'eq))
         (layer1-pos (position (method-layer method1) *active-layers* :test #'eq))
         (layer2-pos (position (method-layer method2) *active-layers* :test #'eq)))

    (cond
      ((or (null specializer1-pos) (null specializer2-pos))
       (error "One of the specializers is not in the class-precedence-list."))
      ((or (null layer1-pos) (null layer2-pos))
       (error "One of the method layers is not in the active layers."))

      ((equal specializer1-pos specializer2-pos)
       (< layer1-pos layer2-pos)) ; if the specializers are equal, compare which layer was defined first in the active layers list
      
      (t (< specializer1-pos specializer2-pos)))))


; ----------------- HELPER FUNCTIONS -----------------

; helper for more-specific-method to convert a list of symbols and values into a plist
(defun make-plist (symbols values)
  (loop for sym in symbols
        for val in values
        append (list (intern (symbol-name sym) :keyword) val)))

(defun mq-eq (method qualifier) ; method-qualifier eq
  (eql (method-qualifier method) qualifier))

(defun get-sorted-methods-by-qualifier (gf methods args qualifier)
  (remove-if-not (lambda (m) (mq-eq m qualifier)) (sort-methods-by-most-specific gf methods args)))

(defun no-next-method ()
  (error "No next method available."))

(defun no-defined-method ()
  (error "call-next-method is not defined."))

(defun check-arguments-length (gf args)
  (when (not (equal (length (generic-function-arguments gf)) (length args)))
    (error "The number of arguments does not match the number of arguments of the generic function.")))

(defun apply-listof-methods (methods params)
  (loop for method in methods
        do (apply (method-function method) params)))