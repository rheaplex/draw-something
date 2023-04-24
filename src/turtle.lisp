;; turtle.lisp - A classic, L-system style computer graphics turtle.
;; Copyright (C) 2006, 2016 Rhea Myers
;; Copyright (C) 2023 Myers Studio Ltd.
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

(in-package #:draw-something)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A classic computer graphics turtle.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <turtle> ()
  ((location :accessor location
             :initform (make-point :x 0 :y 0)
             :initarg :location
             :documentation "The turtle's current location.")
   (direction :accessor direction
              :initform 0.0
              :initarg :direction
              :documentation "The heading, in radians anticlockwise."))
  (:documentation "A classic computer graphics turtle plus randomness ."))

(defun make-turtle (&key location direction)
  "Constructor function."
  (make-instance '<turtle> :location location :direction direction))

(defun turn (the-turtle amount)
  "Turn the turtle by the given amount in degrees,"
  (setf (direction the-turtle)
        (+ (direction the-turtle) amount)))

(defun left (the-turtle amount)
  "Turn the turtle left by the given amount in degrees,"
  (turn the-turtle (- amount)))

(defun right (the-turtle amount)
  "Turn the turtle left by the given amount in degrees,"
  (turn the-turtle amount))

(defun next-point-x (the-turtle amount)
  "The x co-ordinate of the next point the turtle would move forward to."
  (+ (x (location the-turtle))
     (* amount (cos (direction the-turtle)))))

(defun next-point-y (the-turtle amount)
  "The y co-ordinate of the next point the turtle would move forward to."
  (+ (y (location the-turtle))
     (* amount (sin (direction the-turtle)))))

(defun next-point (the-turtle amount)
  "The next point the turtle would move forward to."
  (make-point :x (next-point-x the-turtle amount)
              :y (next-point-y the-turtle amount)))

(defun forward (the-turtle amount)
  "Move the turtle forward the given distance at the turtle's current angle."
  (setf (location the-turtle)
        (next-point the-turtle amount)))

(defun backward (the-turtle amount)
  "Move the turtle backward the given distance at the turtle's current angle."
  (setf (location the-turtle)
        (next-point the-turtle (- amount))))
