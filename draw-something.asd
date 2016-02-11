;; draw-something.asd - The main package for draw-something
;; Copyright (C) 2006, 2010, 2016  Rhea Myers rhea@myers.studio
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
    :version "0.5.0"
    :author "Rhea Myers <rhea@myers.studio>"
    :licence "GNU GPL v3+"
    :pathname "src"
    :serial t
    :components
    ((:file "package")
     (:file "utilities")
     (:file "geometry")
     (:file "point")
     (:file "line")
     (:file "circle")
     (:file "arc")
     (:file "rectangle")
     (:file "polyline")
     (:file "colour")
     (:file "colouring-new")
     (:file "turtle")
     (:file "pen")
     (:file "form")
     (:file "figure")
     ;;(:file "codelet")
     ;;(:file "cell-matrix")
     ;;(:file "find-space")
     (:file "drawing")
     (:file "composition") 
     (:file "plane")
     (:file "postscript")
     (:file "svg")
     (:file "draw-something")))

(defsystem "draw-something/test"
    :description "draw-something/test: tests for draw-something."
    :version "0.5.0"
    :author "Rhea Myers <rhea@myers.studio>"
    :licence "GNU GPL v3+"
    :depends-on ("draw-something" "fiveam")
    :pathname "test"
    :perform (test-op (o s)
                      (uiop:symbol-call :fiveam '#:run!
                                        (uiop:find-symbol*
                                         '#:draw-something-test-suite
                                         :draw-something/test)))
    :components
    ((:file "package")
     (:file "test")))
