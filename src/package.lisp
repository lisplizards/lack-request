;; Copyright (c) 2015 Eitaro Fukamachi
;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: MIT

(in-package #:cl-user)

(defpackage #:foo.lisp.lack/request
  (:use #:cl)
  (:export #:make-request
           #:request-cookies
           #:request-query-parameters
           #:request-body-parameters))

(defpackage #:foo.lisp.lack/request/content-negotiation
  (:use #:cl)
  (:export #:negotiate-media-type))
