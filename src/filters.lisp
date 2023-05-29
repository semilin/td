(in-package #:td)

(defun* filter ((tasks list) &rest (filters tasks-filter))
  (:returns list)
  (remove-if (lambda (task)
	       (remove-if (lambda (f)
			    (matches? f task))
			  filters))
	     tasks))

(defclass/std tasks-filter ()
  ())

(defclass/std filter-or ()
  ((filters)))

(defclass/std filter-not ()
  ((negated-filter)))

(defclass/std completed ()
  ())

(defclass/std exact-description (tasks-filter)
  ((description)))

(defclass/std description-regex (tasks-filter)
  ((match-string)))

(defclass/std has-tags (tasks-filter)
  ((tags)))

(defclass/std has-tags (tasks-filter)
  ((tags)))

(defclass/std overdue (tasks-filter)
  (()))

(defclass/std within-n-days (tasks-filter)
  ((days)))

(defgeneric* matches? ((self tasks-filter) (task task))
  (:returns boolean)
  (:documentation "Returns t if task matches the filter."))

(defmethod* matches? ((self filter-or) (task task))
  (:returns boolean)
  (if (remove-if-not (lambda (filter)
		       (matches? filter task))
		     (filters self))
      t
      nil))

(defmethod* matches? ((self filter-not) (task task))
  (:returns boolean)
  (not (matches? (negated-filter self) task)))

(defmethod* matches? ((self completed) (task task))
  (:returns boolean)
  (if (task-completed task)
      t
      nil))

(defmethod* matches? ((self exact-description) (task task))
  (string= (description self) (task-description task)))

(defmethod* matches? ((self description-regex) (task task))
  (:returns boolean)
  (if (re:scan (re:create-scanner (match-string self) :case-insensitive-mode t) (task-description task))
      t
      nil))

(defmethod* matches? ((self has-tags) (task task))
  (:returns boolean)
  (if (remove-if #'null
		 (mapcar (lambda (it)
			   (member it (task-tags task) :test #'equal))
			 (tags self)))
      t
      nil))

(defmethod* matches? ((self overdue) (task task))
  (:returns boolean)
  (lt:timestamp> (lt:now) (task-date task)))

(defmethod* matches? ((self completable) (task task))
  (:returns boolean)
  (not (uncompleted-prerequisites? task)))

(defmethod* matches? ((self within-n-days) (task task))
  (:returns boolean)
  (let ((diff (- (lt:day-of (task-date task))
		 (lt:day-of (lt:now)))))
    (<= 0 diff)
    (>= (days self) diff)))

(defun* filter-or (&rest (filters tasks-filter))
  (make-instance 'filter-or :filters filters))

(defgeneric* filter-not ((filter tasks-filter))
  (:method (filter)
    (make-instance 'filter-not :negated-filter filter)))

(defun* completed ()
  (make-instance 'completed))

(defun* within-n-days ((days integer))
  (make-instance 'within-n-days :days days))

(defun* exact-description ((description string))
  (make-instance 'exact-description :description description))

(defun* description-regex ((match-string string))
  (make-instance 'description-regex :match-string match-string))

(defun* has-tags (&rest (tags string))
  (make-instance 'has-tags :tags tags))

(defun* completable ()
  (make-instance 'completable))

(defun* overdue ()
  (make-instance 'overdue))

