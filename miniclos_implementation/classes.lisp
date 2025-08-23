(in-package :closless)

; ---------------------------------------------------
; --------------------- CLASSES ---------------------
; ---------------------------------------------------

; standard-object is the superclass of all user-defined classes (via defclass)
; T is the superclass of all classes, including standard-object
(defvar standard-object)

; defstruct automatically creates
; a constructor for structure: make-class
; getters for slots: class-direct-superclass, class-direct-slots
; and setters: (setf class-direct-superclass), (setf class-direct-slots)

(defstruct class
  (name nil)
  (layer-info nil)
  (layer-order nil))

(defun make-class-wrapper (&key name layer direct-superclasses direct-slots)
  (let* ((normalized-direct-superclasses (if (null direct-superclasses) (list standard-object) direct-superclasses))
         (layer-info (make-hash-table))
         (layer-order (list layer)))
    (setf (gethash layer layer-info)
          `(:direct-superclasses ,normalized-direct-superclasses
            :direct-slots ,direct-slots))
    (make-class 
      :name name
      :layer-info layer-info
      :layer-order layer-order)))

(defun add-layer-info (&key class layer direct-superclasses direct-slots)
  (unless (gethash layer (class-layer-info class))
    (setf (class-layer-order class)
          (append (class-layer-order class) (list layer))))
  (setf (gethash layer (class-layer-info class))
        `(:direct-superclasses ,direct-superclasses
          :direct-slots ,direct-slots)))


; class getter wrappers, first make them also work for the class T,
; next for direct-superclasses and direct-slots collect them for all active layers if they are defined in the class
; make sure that no duplicates are returned (via the member function)
(defun get-class-name (class)
  (if (eq class T) T (class-name class)))

; here we don't have to be that careful with the order/merging since we don't have any slot options, just a list of slot names
(defun get-class-direct-slots (class)
  (if (eq class T)
      '()
      (loop with collected-slots = '()
            for layer in *active-layers*
            for plist = (gethash layer (class-layer-info class))
            when plist
              do (loop for slot in (getf plist :direct-slots)
                       unless (member slot collected-slots)
                         do (push slot collected-slots))
            finally (return collected-slots))))

; here we collect and merge the direct-superclasses for each active layer,
; earlier defined layers come earlier, duplicate superclass remove earlier occurrences in the local precedence order
(defun get-class-direct-superclasses (class)
  (if (eq class T)
      '()
      (loop with collected-superclasses = '()
            ;; iterate in stored insertion order
            for layer in (class-layer-order class)
            ;; only include layers that are active
            when (member layer *active-layers*)
              do (let ((plist (gethash layer (class-layer-info class))))
                  (when plist
                    (loop for superclass in (getf plist :direct-superclasses)
                          do (progn
                              ;; remove earlier occurrence if present
                              (setf collected-superclasses (remove superclass collected-superclasses))
                              ;; push the current one
                              (push superclass collected-superclasses)))))
            finally (return (reverse collected-superclasses)))))

; get a list of lists for each partial class of the class where each list contains the direct superclasses of that partial class
(defun get-partial-class-direct-superclasses-list (class)
  (if (eq class T)
      '()
      (loop with collected-superclasses = '()
            for layer in (class-layer-order class)
            when (member layer *active-layers*)
              do (let ((plist (gethash layer (class-layer-info class))))
                  (when plist
                    (push (getf plist :direct-superclasses) collected-superclasses)))
            finally (return collected-superclasses))))

; ------------- UTILITY FUNCTIONS FOR CLASSES -------------

(defun class-all-superclasses (class)
  (let ((visited '()))
    (labels ((dfs (c)
               (unless (or (null c) (member c visited))
                 (push c visited)
                 (dolist (super (get-class-direct-superclasses c))
                   (dfs super)))))
      (dfs class)
      (reverse visited)))) ; return the visited list in reverse order to reflect DFS order

(defun class-all-slots (class)
  (remove-duplicates
    (loop for c in (class-all-superclasses class)
          append (get-class-direct-slots c))
    :test #'equal))

(defun subclassp (class1 class2)
  (member class2 (class-all-superclasses class1)))

; -------------- INITIALIZING THE STANDARD-OBJECT CLASS --------------

(unless (boundp 'standard-object)
  (let ((layer-info (make-hash-table)))
    (setf (gethash T layer-info) '(:direct-superclasses (T) :direct-slots ()))
    (setf standard-object (make-class :name 'standard-object :layer-info layer-info :layer-order (list T)))))

