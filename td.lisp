;;;; td.lisp

(in-package #:td)

(defvar *tasks* nil)

(defclass/std task ()
  ((description :ri :with-prefix)
   (date :ri :with-prefix)
   (completed :ri :with-prefix)
   (prerequisites :ri :with-prefix)
   (tags :ri :with-prefix)
   (schedule :ri :with-prefix)))

(defun* make-task (&key ((description string) nil)
			((date lt:timestamp) nil)
			((completed (or lt:timestamp null)) nil)
			((prerequisites list) nil)
			((tags list) nil)
			((schedule (or recur-schedule null)) nil))
  (:returns task)
  (make-instance 'task :description description
		       :date date
		       :completed completed
		       :prerequisites prerequisites
		       :tags tags
		       :schedule schedule))

(defmethod* print-object ((self task) (out stream))
  (:returns :void)
  (print-unreadable-object (self out :type t)
    (format out "~s" (task-description self))))

(defgeneric* complete ((self task) &optional (recur? boolean))
  (:returns :void)
  (:documentation "Marks a task as completed if possible and reschedules it."))

(define-condition locked-completion-attempt (error)
  ((task :initarg :task
	 :initform nil
	 :reader task))
  (:report (lambda (condition stream)
	     (format stream "~a cannot be completed because it has uncompleted prerequisites: ~a."
		     (task condition)
		     (task-prerequisites (task condition))))))

(defmethod* complete ((self task) &optional (recur? t))
  (:returns :void)
  (if (remove-if (lambda (prereq)
		   (task-completed (car (sort-tasks-time (filter *tasks* (exact-description prereq))))))
		 (task-prerequisites self))
      (error 'locked-completion-attempt :task self))
  (setf (slot-value self 'completed) (lt:now))
  (if (and recur? (task-schedule self))
      (push (make-task :description (task-description self)
		       :date (car (next-dates (task-schedule self) 1))
		       :completed ()
		       :prerequisites ()
		       :tags (task-tags self)
		       :schedule (task-schedule self))
	    *tasks*)))

(defclass/std recur-schedule ()
  ())

(defgeneric* next-dates ((self recur-schedule) (n integer))
  (:returns list)
  (:documentation "Return the n next dates for a schedule to recur from today"))

(defgeneric* recurs-on-day? ((self recur-schedule) (day lt:timestamp))
  (:returns boolean)
  (:documentation "Predicate returns t if schedule recurs on given day"))

(defclass/std recur-daily (recur-schedule)
  ())

(defun* recur-daily ()
  (make-instance 'recur-daily))

(defmethod* next-dates ((self recur-schedule) (n integer))
  (:returns list)
  (iter
   (for i from 1 to n)
   (collect (lt:timestamp+ (lt:now) n :day))))

(defmethod* recurs-on-day? ((self recur-schedule) (day lt:timestamp))
  (:returns boolean)
  t)

(defclass/std tasks-filter ()
  ())

(defgeneric* matches? ((self tasks-filter) (task task))
  (:returns boolean)
  (:documentation "Returns t if task matches the filter."))

(defclass/std exact-description (tasks-filter)
  ((description)))

(defmethod* matches? ((self exact-description) (task task))
  (string= (description self) (task-description task)))

(defun* exact-description ((description string))
  (:returns exact-description)
  (make-instance 'exact-description :description description))

(defclass/std description-regex (tasks-filter)
  ((match-string)))

(defmethod* matches? ((self description-regex) (task task))
  (:returns boolean)
  (if (re:scan (re:create-scanner (match-string self) :case-insensitive-mode t) (task-description task))
      t
      nil))

(defun* description-regex ((match-string string))
  (:returns description-regex)
  (make-instance 'description-regex :match-string match-string))

(defclass/std has-tags (tasks-filter)
  ((tags)))

(defmethod* matches? ((self has-tags) (task task))
  (:returns boolean)
  (if (remove-if #'null
		 (mapcar (lambda (it)
			   (member it (task-tags task) :test #'equal))
			 (tags self)))
      t
      nil))

(defun* has-tags (&rest (tags string))
  (:returns has-tags)
  (make-instance 'has-tags :tags tags))

(defun* filter ((tasks list) &rest (filters tasks-filter))
  (:returns list)
  (remove-if-not (lambda (task)
		   (remove-if-not (lambda (f)
				    (matches? f task))
				  filters))
		 tasks))

(defun* sort-tasks-time ((tasks list))
  (:returns list)
  (sort tasks (lambda (a b)
		(lt:timestamp> (task-date a) (task-date b)))))

(defun load-tasks ()
  (ubiquitous:restore 'td)
  (setf *tasks* (ubiquitous:value :tasks)))

(defun save-tasks ()
  (setf (ubiquitous:value :tasks) *tasks*))
