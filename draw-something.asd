;; draw-something.asd - The main package for draw-something
;; Copyright (C) 2006, 2010, 2016, 2021, 2023 Rhea Myers
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

(defsystem draw-something
  :description "draw-something: a drawing generation system."
  :version "2023a"
  :author "Rhea Myers"
  :licence "GNU GPL v3+"
  :pathname "src"
  :serial t
  :depends-on (:cl-pdf :log4cl)
  :components
  ((:file "mt19937")
   (:file "choosing")
   (:file "colour")
   (:file "geometry")
   (:file "drawing")
   (:file "pdf")
   (:file "draw-something")
   )
  :in-order-to ((test-op (test-op draw-something/test))))

(defsystem draw-something/test
  :description "draw-something/test: tests for draw-something."
  :version "0.5.0"
  :author "Rhea Myers"
  :licence "GNU GPL v3+"
  :depends-on (:draw-something
               :prove)
  :defsystem-depends-on (:prove-asdf)
  :pathname "test"
  :components
  ((:file "package")
   (:test-file "test"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
