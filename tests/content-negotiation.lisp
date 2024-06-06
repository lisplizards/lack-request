;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: MIT

(in-package #:foo.lisp.lack/request/tests/content-negotiation)

(defparameter *accept-header*
  "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8")

(defparameter *accept-header-without-wildcards*
  "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp;q=0.8")

(deftest negotiate-media-type
    (testing "returns the first match against the request Accept header"
             (let ((request (lack/request:make-request
                             `(:request-method :GET
                               :request-uri "/"
                               :url-scheme "http"
                               :headers ,(alexandria:alist-hash-table
                                          `(("accept" . ,*accept-header*))
                                          :test #'equal)))))
               (ok (string= "text/html" (negotiate-media-type request '("text/html"))))))

  (testing "returns the first match against the request Accept header, when not first in the given list"
           (let ((request (lack/request:make-request
                           `(:request-method :GET
                             :request-uri "/"
                             :url-scheme "http"
                             :headers ,(alexandria:alist-hash-table
                                        `(("accept" . ,*accept-header*))
                                        :test #'equal)))))
             (ok (string= "application/xml" (negotiate-media-type request '("application/xml"))))))

  (testing "finds a match of the first value from the request Accept header media-types, not the given list"
           (let ((request (lack/request:make-request
                           `(:request-method :GET
                             :request-uri "/"
                             :url-scheme "http"
                             :headers ,(alexandria:alist-hash-table
                                        `(("accept" . ,*accept-header*))
                                        :test #'equal)))))
             (ok (string= "text/html" (negotiate-media-type request '("application/xml" "text/html"))))))

  (testing "returns the first in the list matching the wildcard when matched by a wildcard"
           (let ((request (lack/request:make-request
                           `(:request-method :GET
                             :request-uri "/"
                             :url-scheme "http"
                             :headers ,(alexandria:alist-hash-table
                                        `(("accept" . ,*accept-header*))
                                        :test #'equal)))))
             (ok (string= "application/json" (negotiate-media-type request '("application/json"
                                                                             "text/plain"))))))

  (testing "returns NIL when there is no match"
           (let ((request (lack/request:make-request
                           `(:request-method :GET
                             :request-uri "/"
                             :url-scheme "http"
                             :headers ,(alexandria:alist-hash-table
                                        `(("accept" . ,*accept-header-without-wildcards*))
                                        :test #'equal)))))
             (ok (null (negotiate-media-type request '("application/json")))))))
