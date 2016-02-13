;;  colouring-new.lisp -  Colour scheme generation and application.
;;  Copyright (C) 2008, 2016 Rob Myers rob@robmyers.org
;;
;; This file is part of draw-something.
;; 
;; draw-something is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;; 
;; draw-something is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Acronyms:
;; lmh = low, medium, high
;; sv = saturation, value

;;(in-package "COLOUR-CELLS")
(in-package "DRAW-SOMETHING")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object symbols used in colouring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter object-symbol-choices
  '(leaf vein blade branch flower tendril))

(defparameter all-object-symbols
  (cons 'background object-symbol-choices))

(defun object-symbol (obj)
  (declare (ignore obj))
  (choose-one-of object-symbol-choices))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LMH - Low medium and high value ranges from 0.0..1.0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass lmh-values ()
  ((lows :type vector 
	 :initform (make-vector 7)
	 :initarg :lows
	 :accessor low-values)
   (mediums :type vector 
	    :initform (make-vector 7)
	    :initarg :mediums
	    :accessor medium-values)
   (highs :type vector 
	  :initform (make-vector 7)
	  :initarg :highs
	  :accessor high-values))
  (:documentation 
    "A range of values divided into low, medium and high values."))

(defun make-random-low-values (count medium-start)
  "Generate count random low values."
  (map-into (make-vector count)
	    (lambda () (random-range 0.0 medium-start))))

(defun make-random-medium-values (count medium-start high-start)
  "Generate count random medium values."
  (map-into (make-vector count)
	    (lambda () (random-range medium-start high-start))))

(defun make-random-high-values (count high-start)
  "Generate count random high values."
  (map-into (make-vector count)
	    (lambda () (random-range high-start 1.0))))

(defun make-lmh (count medium-start high-start)
  "Make an lmh."
  (let* ((low-count (random-range 1 (- count 2)))
	 (medium-count (random-range 1 (- count low-count 1)))
	 (high-count (- count medium-count low-count)))
    (make-instance 'lmh-values
		   :lows (make-random-low-values low-count
						 medium-start)
		   :mediums (make-random-medium-values medium-count
						       medium-start
						       high-start)
		   :highs (make-random-high-values high-count
						   high-start))))

(defun random-low-value (lmh)
  "Randomly choose a value from the list of low values of the lmh."
  (choose-one-of (low-values lmh)))

(defun random-medium-value (lmh)
  "Randomly choose a value from the list of medium values of the lmh."
  (choose-one-of (medium-values lmh)))

(defun random-high-value (lmh)
  "Randomly choose a value from the list of medium values of the lmh."
  (choose-one-of (high-values lmh)))

(defun random-lmh-value (lmh which)
  "Return a random low ('l), medium ('m) or high ('h) value based on which."
  (case which
	(l (random-low-value lmh))
	(m (random-medium-value lmh))
	(h (random-high-value lmh))))

(defun print-lmh (lmh)
  (advisory-message "low: ")
  (loop for l across (low-values lmh)
	do (advisory-message (format nil "~a " l)))
  (advisory-message  "~%medium: ")
  (loop for m across (medium-values lmh)
	do (advisory-message (format nil "~a " m)))
  (advisory-message  "~%high: ")
  (loop for h across (high-values lmh)
	do (advisory-message (format nil "~a " h)))
  (advisory-message  "~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colour Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass colour-scheme ()
  ((hues :type hash-table
	 :initarg :hues
	 :initform (make-hash-table)
	 :accessor colour-scheme-hues)
   (saturations :type lmh-values
		:initarg :saturations
		:initform (make-instance 'lmh-values)
		:accessor colour-scheme-saturations)
   (values :type lmh-values
	   :initarg :values
	   :initform (make-instance 'lmh-values)
	   :accessor colour-scheme-values))
  (:documentation "The values describing a colour scheme."))

(defun print-colour-scheme (scheme)
  (advisory-message  "Colour Scheme:~%")
  (advisory-message  "hues:~%")
  (maphash (lambda (key value)
	    (advisory-message (format nil "~a ~a " key value)))
	   (colour-scheme-hues scheme))
  (advisory-message  "~%saturations:~%")
  (print-lmh (colour-scheme-saturations scheme))
  (advisory-message  "values:~%")
  (print-lmh (colour-scheme-values scheme)))

(defun symbol-colour-scheme-hue (scheme hue-id)
  "Get the hue value from the scheme for a given id, eg :stem, :stalk."
  (gethash hue-id (colour-scheme-hues scheme)))

(defun random-colour-scheme-saturation (scheme spec)
  "Choose a saturation from the range specified by spec."
  (random-lmh-value (colour-scheme-saturations scheme)
		    spec))

(defun random-colour-scheme-value (scheme spec)
  "Choose a value from the range specified by spec."
  (random-lmh-value (colour-scheme-values scheme)
		    spec))

(defun sv-spec-components (spec)
  "Return each half of the symbol specifying how to choose saturation & value."
  (case spec
	(ll (values 'l 'l))
	(lm (values 'l 'm))
	(lh (values 'l 'h))
	(ml (values 'm 'l))
	(mm (values 'm 'm))
	(mh (values 'm 'h))
	(hl (values 'h 'l))
	(hm (values 'h 'm))
	(hh (values 'h 'h))))

(defun make-colour-by-sv-spec (scheme hue-id sv-spec)
  "Choose a colour for the hue id using the sv-spec eg 'lm, 'hh, 'ml."
  (multiple-value-bind 
	(saturationspec valuespec) (sv-spec-components sv-spec)
    (make-instance 'colour 
		   :hue (symbol-colour-scheme-hue scheme hue-id)
		   :saturation (random-colour-scheme-saturation scheme
								saturationspec)
		   :brightness (random-colour-scheme-value scheme
							   valuespec))))

;; Generate additive colour range

(defun make-hue-additive-series (hue-list)
  (let ((series (make-hash-table))
	(hue-value (random 1.0)))
    (dolist (hue-symbol hue-list)
      (setf hue-value (mod (+ hue-value (random 0.3))
			   1.0))
      (setf (gethash hue-symbol series)
	    hue-value))
    series))

(defun make-colour-scheme (hue-list saturations values medium high)
  "Make a colour scheme for the saturation symbol list."
  (make-instance 'colour-scheme
		 :hues (make-hue-additive-series hue-list)
		 :saturations (make-lmh saturations medium high)
		 :values (make-lmh values medium high)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Applying colour schemes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass colour-scheme-applier ()
  ((scheme :type colour-scheme
	   :accessor applier-scheme
	   :initarg :scheme)
   (count :type integer
	  :initform 1
	  :accessor applier-count
	  :documentation "How many objects this applier haas coloured.")
   (check :type integer
	  :initform 5
	  :initarg :check
	  :accessor applier-when-to-check
	  :documentation "How often to check deviation.")
   ;; This one is more part of the scheme but is more convenient here
   (sv-chooser :initarg :sv-chooser
	       :initform (lambda () 'll)
	       :accessor applier-sv-chooser
	       :documentation "The function to choose sv values.")
   (sv-probabilites :type hash-table
		    :initform (make-hash-table)
		    :initarg :sv-probabilities
		    :accessor applier-probabilities
		    :documentation "The probabilities of the chooser sv specs")
   (sv-occurrences :type hash-table
		   :initform (make-hash-table)
		   :accessor applier-occurences
		   :documentation "How often each sv spec has been chosen."))
  (:documentation
   "The data used in applying a colour scheme to an image."))

(defun set-applier-probabilities (applier spec-list)
  "Set the probabilites from a list of num/specs, and set occurences to zero"
  (let ((total-prob (float (prefs-range spec-list))))
    (loop for prob in spec-list by #'cddr 
	  for spec in (cdr spec-list) by #'cddr
	  do (setf (gethash (dequote spec)
			    (applier-probabilities applier))
		   (/ prob total-prob))
	  do (setf (gethash (dequote spec)
			    (applier-occurences applier)) 0))))

(defun set-applier-chooser (applier spec-list)
  "Make and set the chooser function for sv specs from the list."
  (setf (applier-sv-chooser applier)
	(prefs-list-lambda spec-list)))
  
(defun set-applier-sv-spec (applier spec-list)
  "Configure the applier from the sv spec eg '(1 'hh 4 'ml 3 'lm)"
  (set-applier-chooser applier spec-list)
  (set-applier-probabilities applier spec-list))

;; Change to being an :after initialize-instance method
(defun make-colour-scheme-applier (scheme spec-list)
  "Make a colour scheme applier."
  (let ((applier (make-instance 'colour-scheme-applier
				:scheme scheme)))
    (set-applier-chooser applier spec-list)
    (set-applier-probabilities applier spec-list)
    applier))
       
(defun spec-probability-difference (applier spec)
  "Get the difference between the intended and actual occurence of a spec."
  (let* ((generation-count (float (applier-count applier)))
	 (spec-count (gethash spec (applier-occurences applier)))
	 (target (float (gethash spec (applier-probabilities applier))))
	 (current (/ spec-count generation-count))
	 (difference (- target current)))
    (advisory-message (format nil "  ~a ~a ~,3F ~,3F ~,3F~%"
			      spec spec-count target current difference) )
    difference))

(defun most-deviant-spec (applier)
  "Find the spec that is being called the least compared to its probability."
  (let ((highest 0.0)
	(result nil))
    (maphash #'(lambda (key val)
		 (declare (ignore val))
		 (let ((difference (spec-probability-difference applier key)))
		   (when (> difference highest)
		     (setf highest difference)
		     (setf result key))))
	     (applier-probabilities applier))
    (advisory-message (format nil "~a~%" result))
    result))

(defun increase-spec-count (applier spec)
  "Update the number of times a spec has been used."
  (incf (gethash spec (applier-occurences applier))))

(defun increase-applier-count (applier)
  "Increase the applier's count of objects it has been called for by 1."
  (incf (applier-count applier)))

(defun update-applier-state (applier spec)
  "Call when you've applied colour to an object & are ready for the next one."
  (increase-spec-count applier spec)
  (increase-applier-count applier))

(defun applier-should-correct (applier)
  "Decide whether the applier should correct the spec probability error."
  (eq (mod (applier-count applier) 
	   (applier-when-to-check applier))
      0))

(defun applier-spec-choice (applier hue-id)
  "Choose the spec to be used."
  (declare (ignore hue-id))
  (if (applier-should-correct applier)
      (most-deviant-spec applier)
    (funcall (applier-sv-chooser applier))))

(defun choose-colour-for (applier hue-id)
  "Choose a colour from the scheme for the hue-id."
  (let* ((spec (applier-spec-choice applier hue-id))
	 (choice (make-colour-by-sv-spec (applier-scheme applier)
					 hue-id spec)))
    (update-applier-state applier spec)
    choice))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; How to make and apply a colour scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant minimum-spec-probability 3)
(defconstant maximum-spec-probability 9)
(defconstant minimum-spec-count 2)
(defconstant maximum-spec-count 3)
(defparameter sv-spec-options '('ll 'lm 'lh 'ml 'mm 'mh 'hl 'hm 'hh))

(defun chooser-spec ()
  "Construct a list describing a random spec pref, eg (6 'll 3 'hm 2 'lh)."
  (loop for spec in (choose-n-of (random-range-inclusive minimum-spec-count 
							 maximum-spec-count) 
				 sv-spec-options)
	collect (random-range-inclusive minimum-spec-probability 
					maximum-spec-probability)
	collect spec))

(defun colour-objects (drawing symbols)
  (let* ((scheme (make-colour-scheme symbols 7 7 .3 .6))
	 (sv-spec-list (chooser-spec))
	 (applier (make-colour-scheme-applier scheme sv-spec-list)))
    (advisory-message "Colouring forms.~%")
    (print-colour-scheme scheme)
    (advisory-message (format nil "sv-spec: ~a~%" sv-spec-list))
    (setf (ground drawing) 
	  (choose-colour-for applier
			     'background))
    (loop for plane across (planes drawing)
	  do (loop for figure across (figures plane)
		   do (loop for form across (forms figure)
			    do (setf (fill-colour form)
				     (choose-colour-for
				      applier
				      (object-symbol form))))))))
