;; figurine.lisp - Creates robust figures.
;; Copyright (C) 2023 Myers Studio, Ltd.
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

;; Make sure you have draw-something and sketch in local-projects,
;; e.g. in ~/quicklisp/local-projects/ or ~/.roswell/local-projects/ .

(ql:quickload '(:draw-something :sdl2 :sdl2kit :sketch))

(defpackage :figurine
  (:use :cl :sketch :draw-something))

(in-package :figurine)

;; 11in x 14in
(defparameter +page-width+ 960)
(defparameter +page-height+ 540)
(defparameter +drawing-width+ 900)
(defparameter +drawing-height+ 480)
(defparameter +drawing-x+ (/ (- +page-width+ +drawing-width+) 2.0))
(defparameter +drawing-y+ (/ (- +page-height+ +drawing-height+) 2.0))

(defparameter +point-rad+ 5.5)

(defparameter +substrate-bounds+
  (ds::make-rectangle :x 0
                      :y 0
                      :width +page-width+
                      :height +page-height+))

(defparameter +drawing-bounds+
  (ds::make-rectangle :x +drawing-x+
                      :y +drawing-y+
                      :width +drawing-width+
                      :height +drawing-height+))

(ds::random-init (get-universal-time))

(defparameter +poly+ nil)
(defparameter +top-left+ nil)

;; The next +poly+ line segment to draw along.
(defvar *next-index*)
;; The previous intersection point
;; (the last to and current from of the outline).
(defvar *previous-point*)
;; The drawn lines enclosing +poly+.
(defvar *hull*)

(defun initial-line-index (poly top-left)
  ;; A list of conses of the form (line . index)
  (let ((lines nil))
    ;; Walk the polyline.
    (ds::do-poly-lines (poly l i)
      ;; If a line begins or ends with top-left, save it and its index.
      (when (or (ds::point= (ds::from l) top-left)
                (ds::point= (ds::to l) top-left))
        (push (cons l i) lines)))
    (if (= (length lines) 1)
        ;; If there is only one, return the index.
        (cdr (first lines))
        ;; If there are two, determine which is the closest of them
        ;; clockwise from 12 o'clock up from top-left.
        (let* ((a (first lines))
               (b (second lines))
               (a-line (car a))
               (b-line (car b))
               (a-other (if (ds::point= (ds::from a-line) top-left)
                            (ds::to a-line)
                            (ds::from a-line)))
               (b-other (if (ds::point= (ds::from b-line) top-left)
                            (ds::to b-line)
                            (ds::from b-line))))
          ;; Return the index of the closest
          (if (ds::point= (ds::least-clockwise-point top-left
                                                     (list a-other
                                                           b-other))
                      a-other)
              (cdr a)
              (cdr b))))))

(defun init-poly ()
  (let ((pts (map 'list
                  #'identity
                  (ds::random-points-in-rectangle +drawing-bounds+ 10))))
    (setf +poly+ (ds::make-polyline :points pts)))
  (setf +top-left+ (ds::highest-leftmost-point +poly+))
  (setf *previous-point* +top-left+)
  (setf *next-index* (initial-line-index +poly+ *previous-point*))
  (setf *hull* (ds::make-polyline)))

;;(defun next-polyline-index (poly ))

(defun next-step ()
  ;; Get next end point, on left or right
  ;; Find nearest intersecting line
  ;; Truncate current line at intersection and 
  ;; If first or last point, flip l/r
  )

;;(defun draw-)

(defun draw-line (l)
  (line (ds::x (ds::from l)) (ds::y (ds::from l))
        (ds::x (ds::to l)) (ds::y (ds::to l))))

(defun draw-line-intersection (l1 l2)
  (let ((intersection-t (ds::intersects l1 l2)))
    (when intersection-t
      (let ((intersection (ds::line-at-t l1 intersection-t)))
      (with-pen (make-pen :fill +green+)
        (circle (ds::x intersection) (ds::y intersection) +point-rad+))))))

;; (defun draw-normals (l colour)
;;   (let ((left (ds::add-vector-to-point
;;                (ds::from +line+)
;;                (ds::scale-vector (ds::line-normal-left +line+) 100)))
;;         (right (ds::add-vector-to-point
;;                 (ds::from +line+)
;;                 (ds::scale-vector (ds::line-normal-right +line+) 100))))
;;     (with-pen (make-pen :stroke colour)
;;       (line (ds::x (ds::from l)) (ds::y (ds::from l))
;;             (ds::x left) (ds::y left))
;;       (line (ds::x (ds::from l)) (ds::y (ds::from l))
;;             (ds::x right) (ds::y right)))
;;     (with-pen (make-pen :fill colour)
;;       (circle (ds::x left) (ds::y left) +point-rad+)
;;       (circle (ds::x right) (ds::y right) +point-rad+))
;;     (text "left" (ds::x left) (ds::y left))
;;     (text "right" (ds::x right) (ds::y right))))

(defun draw-polyline-intersections (l1 poly)
  (let* ((intersections (ds::intersections l1 poly))
         (closest (car (ds::sort-points-by-distance (ds::from l1)
                                                    intersections)))
         (count 0))
    (with-pen (make-pen :fill +green+)
      (dolist (p intersections)
        (text (format nil "~d" (incf count)) (ds::x p) (ds::y p))
        (circle (ds::x p) (ds::y p) +point-rad+)))
    (text "closest" (- (ds::x closest) 60) (- (ds::y closest) 10))))

(init-poly)

(defsketch figurine ((title "figurine")
                    (width +page-width+)
                    (height +page-height+)
                    (y-axis :up))
  (rect +drawing-x+ +drawing-y+ +drawing-width+ +drawing-height+)
  (let* ((pts (ds::points +poly+))
         (from (aref pts 0))
         (to (aref pts (- (length pts) 1))))
    (with-pen (make-pen :fill +red+)
      (circle (ds::x from) (ds::y from) +point-rad+)
      (circle (ds::x to) (ds::y to) +point-rad+))
    (text "initial" (ds::x from) (ds::y from))
    (text "final" (ds::x to) (ds::y to)))
  (with-pen (make-pen :stroke +red+)
    (ds::do-poly-lines (+poly+ l)
      (draw-line l)))
  (with-pen (make-pen :fill +red+)
    (circle (ds::x +top-left+) (ds::y +top-left+) +point-rad+))
  (text "top-left" (ds::x +top-left+) (ds::y +top-left+))
  (with-pen (make-pen :stroke +black+)
    (draw-line (ds::poly-line-aref +poly+ *next-index*))))

(defmethod kit.sdl2:mousebutton-event ((window figurine) state ts b x y)
  (when (eq state :mousebuttondown)
    (init-poly)))

#-sbcl (make-instance 'figurine)
#+sbcl (sdl2:make-this-thread-main #'(lambda ()
                                       (make-instance 'figurine)))
