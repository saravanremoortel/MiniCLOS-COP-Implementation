(in-package :closless)

(defstruct layer
  (name ""))

(defvar *active-layers* '(T))

(defmacro with-active-layers (layers &body body)
  `(let ((*active-layers* 
          (remove-duplicates
            (append (list ,@layers) *active-layers*)
            :test #'eq
            :from-end t)))
    ,@body))

(defmacro with-inactive-layers (layers &body body)
  (when (member T layers)
    (error "Cannot deactivate T layer"))
  `(let ((*active-layers*
           (remove-if (lambda (layer)
                        (member layer (list ,@layers)))
                      *active-layers*)))
     ,@body))
