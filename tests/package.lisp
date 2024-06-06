;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: MIT

(in-package #:cl-user)

(defpackage #:foo.lisp.lack/request/tests
  (:use #:cl #:rove))

(defpackage #:foo.lisp.lack/request/tests/content-negotiation
  (:use #:cl #:rove)
  (:import-from #:foo.lisp.lack/request/content-negotiation
                #:negotiate-media-type))
