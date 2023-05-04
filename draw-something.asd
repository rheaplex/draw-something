;; draw-something.asd - The main package for draw-something
;; Copyright (C) 2006, 2010, 2016, 2021 Rhea Myers.

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

(in-package :asdf-user)

(defsystem "draw-something"
  :description "draw-something: a drawing generation system."
  :version "2023.1"
  :author "Rhea Myers"
  :licence "GNU General Public License v3.0 or later"
  :depends-on (#:mt19937)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "utilities")
   (:file "log")
   (:file "random")
   (:file "geometry")
   (:file "point")
   (:file "line")
   (:file "rectangle")
   (:file "polyline")
   (:file "colour")
   (:file "turtle")
   (:file "pen")
   (:file "form")
   (:file "figure")
   (:file "plane")
   (:file "drawing")
   (:file "composition")
   (:file "colouring")
   (:file "postscript")
   (:file "draw-something"))
  :in-order-to ((test-op (test-op draw-something/test))))

(defsystem "draw-something/gui-base"
  :description "Base package for a  GUI for developing draw-something."
  :version "2023.1"
  :author "Rhea Myers"
  :licence "GNU General Public License v3.0 or later"
  :depends-on (#:draw-something)
  :pathname "src"
  :serial t
  :components ((:file "gui-base")))

(defsystem "draw-something/gui-ltk"
  :description "An ltk GUI for developing draw-something."
  :version "2023.1"
  :author "Rhea Myers"
  :licence "GNU General Public License v3.0 or later"
  :depends-on (#:draw-something
               #:draw-something/gui-base
               #:ltk)
  :pathname "src"
  :serial t
  :components ((:file "gui-ltk")))

(defsystem "draw-something/gui-glut"
  :description "A GLUT GUI for developing draw-something."
  :version "2023.1"
  :author "Rhea Myers"
  :licence "GNU General Public License v3.0 or later"
  :depends-on (#:draw-something
               #:draw-something/gui-base
               #:cl-opengl
               #:cl-vectors
               #:trivial-main-thread)
  :pathname "src"
  :serial t
  :components ((:file "gui-glut")))
