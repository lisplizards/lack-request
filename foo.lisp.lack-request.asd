;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: MIT

(defsystem "foo.lisp.lack-request"
  :version "1.0.0"
  :author "John Newton"
  :maintainer "John Newton"
  :license "MIT"
  :homepage "https://github.com/lisplizards/lack-request"
  :bug-tracker "https://github.com/lisplizards/lack-request/issues"
  :source-control (:git "https://github.com/lisplizards/lack-request.git")
  :depends-on ("lack-request")
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("package"))
                 (:file "package"))))
  :description "Alternative constructor and parsing functions for LACK/REQUEST:REQUEST structs"
  :in-order-to ((test-op (test-op "foo.lisp.lack-request/tests"))))

(defsystem "foo.lisp.lack-request/tests"
  :author "John Newton"
  :license "MIT"
  :depends-on ("foo.lisp.lack-request"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main" :depends-on ("package"))
                 (:file "package"))))
  :description "Test system for foo.lisp.lack-request"
  :perform (test-op (op c) (symbol-call :rove :run c)))
