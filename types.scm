;;
;; Data type definitions for NineML.
;;
;; Copyright 2010-2015 Ivan Raikov
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.
;;


(module 9ML-types

	(
         make-dynamics-node
         make-alsys-node
         make-connection-rule-node
         )

	(import scheme chicken)


(define-record-type dynamics-node (make-dynamics-node name formals body)
  dynamics-node? 
  (name dynamics-node-name)
  (formals dynamics-node-formals)
  (body dynamics-node-body)
  )


(define-record-type alsys-node (make-alsys-node name formals body)
  alsys-node? 
  (name alsys-node-name)
  (formals alsys-node-formals)
  (body alsys-node-body)
  )

(define-record-type connection-rule-node (make-connection-rule-node name formals stdlib)
  connection-rule-node? 
  (name connection-rule-node-name)
  (formals connection-rule-node-formals)
  (stdlib connection-rule-node-stdlib)
  )

        

)
