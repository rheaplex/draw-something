;; package.lisp - The main package for draw-something
;; Copyright (C) 2006, 2010, 2016 Rhea Myers
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

(in-package :common-lisp-user)

(defpackage draw-something
  (:nicknames :ds)
  (:documentation
   "The draw-something package.")
  (:use :common-lisp)
  (:export
   #:generate-drawing
   #:draw-something))
