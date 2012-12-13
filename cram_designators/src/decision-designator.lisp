(in-package :desig)

(defclass decision-designator (designator designator-id-mixin equate-notification-mixin)
  ((solutions)))

(register-designator-class decision decision-designator)

(defmethod reference ((desig decision-designator) &optional (role *default-role*))
  (with-slots (solutions) desig
    (setf solutions (resolve-designator desig role))
    (or (setf (slot-value desig 'data) (lazy-car solutions))
        (error 'designator-error
               :format-control "Cannot resolve decision designator ~a."
               :format-arguments (list desig)
               :designator desig))))

(defmethod resolve-designator ((desig decision-designator) (role t))
  (lazy-mapcan (lambda (bdg)
                 (let ((dec-desig (var-value '?act bdg)))
                   (unless (is-var dec-desig)
                     (list dec-desig))))
               (prolog `(decision-desig ,desig ?act))))

(defmethod next-solution ((desig decision-designator))
  (reference desig)
  (when (lazy-cdr (slot-value desig 'solutions))
    (let ((new-desig (make-designator 'decision (description desig) desig)))
      (setf (slot-value new-desig 'solutions)
            (lazy-cdr (slot-value desig 'solutions)))
      new-desig)))
