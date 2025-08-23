(in-package :closless)


; ------------------------------------------------------------------
; deflayer

(defmacro deflayer (name)
  `(defvar ,name
      (make-layer
        :name ',name)))

; ------------------------------------------------------------------
; define-layered-class/defclass

(defun parse-layered-class (rest)
  (let* ((layer-defined (eq (first rest) :in-layer))
         (layer (if layer-defined (second rest) T))
         (superclasses (if layer-defined (third rest) (first rest)))
         (direct-slots (if layer-defined (fourth rest) (second rest))))
    (values layer superclasses direct-slots)))

(defmacro define-layered-class (class &rest rest)
  (multiple-value-bind (layer superclasses direct-slots)
    (parse-layered-class rest)
    (if (not (boundp class))
      `(defvar ,class
        (make-class-wrapper 
            :name ',class ; as a symbol
            :layer ,layer
            :direct-superclasses (list ,@superclasses)
            :direct-slots ',direct-slots))
      `(add-layer-info
        :class ,class
        :layer ,layer
        :direct-superclasses (list ,@superclasses)
        :direct-slots ',direct-slots))))

; macro defclass, (defvar <name> (superclasses) (slots))
(defmacro defclass (name superclasses slots)
  `(define-layered-class ,name
     :in-layer T
     ,superclasses
     ,slots))

; ------------------------------------------------------------------
; define-layered-function/defgeneric

; define-layered-function checkers
(defun duplicates-exist (list) ; check for duplicates
  (let ((dupes (remove-duplicates list :test #'eql)))
    (/= (length dupes) (length list))))

(defun sets-are-different (list1 list2) ; check if lists are the different
  (not (and (null (set-difference list1 list2 :test #'eql))
            (null (set-difference list2 list1 :test #'eql)))))

; macro define-layered-function, (define-layered-function <name> arglist &optional argument-precedence-list)
(defmacro define-layered-function (name arglist &optional argument-precedence-list)
  (when (duplicates-exist arglist)
    (error "Argument list must not contain duplicates."))

  (when (and argument-precedence-list
             (or (not (eq :argument-precedence-order (first argument-precedence-list))) 
                 (duplicates-exist (rest argument-precedence-list))
                 (sets-are-different arglist (rest argument-precedence-list))))
    (error "Argument list and precedence list must not contain duplicates or be different sets."))

  (let ((precedence-list (if argument-precedence-list (rest argument-precedence-list) arglist))) ; you take the rest, since you don't need the keyword - if there is no p-list given, the p-list is equal to the arglist (default = arguments from left to right)
    `(progn 
      (defvar ,name
        (make-generic-function
          :arguments ',arglist
          :argument-precedence-order ',precedence-list))
        
      (defun ,name (&rest args) (apply #'call-generic-function ,name args)))))

(defmacro defgeneric (&rest rest)
  `(define-layered-function ,@rest))

; ------------------------------------------------------------------
; define-layered-method/defmethod

; helpers macro defmethod
(defun qualifier-p (qualifier)
  (member qualifier '(:before :after :around)))

(defun layer-declared-p (layer-keyword)
  (eq layer-keyword :in-layer))

(defun parse-specializers-vars-body (rest)
  (let* ((arglist (first rest))
         (method-body (cdr rest))
         (vars (mapcar (lambda (arg) (if (symbolp arg) arg (first arg))) arglist))
         (specializers (mapcar (lambda (arg) (if (symbolp arg) T (second arg))) arglist)))
      (values specializers vars method-body)))

(defun parse-qualifier-and-rest (rest)
  (let ((qualifier (if (qualifier-p (first rest)) (first rest) nil))
        (remaining (if (qualifier-p (first rest)) (cdr rest) rest)))
    (multiple-value-bind (specializers vars function-body)
      (parse-specializers-vars-body remaining)
      (values qualifier specializers vars function-body))))

(defun parse-layer-and-rest (rest)
  (let ((layer (if (layer-declared-p (first rest)) (second rest) T))
        (remaining (if (layer-declared-p (first rest)) (cddr rest) rest)))
    (multiple-value-bind (qualifier specializers vars function-body)
      (parse-qualifier-and-rest remaining)
      (values layer qualifier specializers vars function-body))))

(defmacro define-layered-method (name &rest rest)
  (multiple-value-bind (layer qualifier specializers vars function-body)
    (parse-layer-and-rest rest)
    `(add-method ,name
       (make-method
        :specializers (list ,@specializers)
        :function (lambda (,@vars) ,@function-body)
        :qualifier ,qualifier
        :layer ,layer))))

(defmacro defmethod (&rest rest)
  `(define-layered-method ,@rest))

; ------------------------------------------------------------------
; make-instance

; macro make-instance to make an instance of a self made class.. aka make an object
(defmacro make-instance (class-symbol)
  `(make-object :class (symbol-value ,class-symbol)))