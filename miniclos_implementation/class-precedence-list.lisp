(in-package :closless)

; ---------------- MAIN FUNCTIONS FOR CLASS PRECEDENCE LIST ----------------

(defun get-class-precedence-list (class)
  (let ((superclasses (class-all-superclasses class)))
    (topological-sort superclasses (union-make-pairs superclasses) '())))

(defun topological-sort (remaining-classes remaining-pairs sorted)
  (let ((candidate (find-candidate remaining-classes remaining-pairs sorted)))
    (cond
      ; precedence order found
      ((and (null candidate) (null remaining-classes))
       (reverse sorted))

      ; no candidate, but still classes left to process, the classes are inconsistent
      ((null candidate)
       (error "something went wrong during the construction of the class precedence list, classes are inconsistent."))

      ; candidate found, processing candidate
      (t
       (let* ((updated-classes (remove candidate remaining-classes :test #'eq))
              (updated-pairs (remove-precedence-pairs candidate remaining-pairs)))
         (topological-sort updated-classes updated-pairs (cons candidate sorted)))))))


; ----------- MAKING PRECEDENCE PAIRS -----------

; creates pairs of classes, where the first class must precede the second class in the precedence list 
(defun union-make-pairs (superclasses)
  (reduce (lambda (acc cls) ; folds superclass list from left to right (acc = acumulater, cls = class)
            (union (make-all-pairs cls) acc :test #'equal))
          superclasses
          :initial-value '()))

(defun make-all-pairs (class)
  (let ((partial-superclasses-list (get-partial-class-direct-superclasses-list class)))
    (loop for partial-superclasses in partial-superclasses-list
          collect (make-pairs class partial-superclasses) into pairs
          finally (return (apply #'append pairs)))))

(defun make-pairs (class supers)
  (let* ((pairs '())
         (all (cons class supers)))
    (loop for i from 0 below (- (length all) 1)
          for a = (nth i all)
          for b = (nth (+ i 1) all)
          do (push (cons a b) pairs))
    pairs))

; ----------- REMOVING PRECEDENCE PAIRS -----------

; removes all pairs where the first element is the given class
(defun remove-precedence-pairs (class pairs)
  (remove-if (lambda (pair)
               (eq (car pair) class))
             pairs))


; ----------- FINDING NEXT CANDIDATE FOR THE CLASS PRECEDENCE LIST -----------

(defun find-candidate (classes precedence-pairs precedence-list)
  (let ((candidates (find-candidates classes precedence-pairs)))
    (cond
      ((null candidates) nil) ; precedence-list is finished if there are no more candidates found
      
      ((= (length candidates) 1) (first candidates)) ; exactly one, just return it
      
      (t ; multiple candidates, find the most fitting one using direct-superclass of the last candidate/sorted class
        (let* ((last-class (first precedence-list))
              (direct-superclasses (get-class-direct-superclasses last-class)))
          (or (find-if (lambda (candidate)
                        (member candidate direct-superclasses :test #'eq))
                      candidates)
              (first candidates))))))) ; shouldn't occur, there should always be a candidate that is in the direct superclasses

(defun find-candidates (classes precedence-pairs)
  (remove-if (lambda (class)
               (has-predecessor-p class precedence-pairs))
             classes))

(defun has-predecessor-p (class precedence-pairs)
  (not (null
        (remove-if-not
         (lambda (pair) (eq (cdr pair) class))
         precedence-pairs))))