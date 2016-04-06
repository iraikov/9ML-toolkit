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
         make-dynamics-node dynamics-node? dynamics-node-env dynamics-node-formals dynamics-node-body
         make-alsys-node alsys-node?
         make-connection-rule-node connection-rule-node?
         make-random-dist-node random-dist-node?
         )

	(import scheme chicken)


(define-record-type dynamics-node (make-dynamics-node name formals env body)
  dynamics-node? 
  (name dynamics-node-name)
  (formals dynamics-node-formals)
  (env dynamics-node-env)
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


(define-record-type random-dist-node (make-random-dist-node name formals stdlib)
  random-dist-node? 
  (name random-dist-node-name)
  (formals random-dist-node-formals)
  (stdlib random-dist-node-stdlib)
  )

        

)
