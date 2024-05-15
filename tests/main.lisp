;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: MIT

(in-package :foo.lisp.lack-request/tests)

(deftest make-request
    (testing
     "returns a LACK/REQUEST:REQUEST struct without parsing cookies, query parameters, or body parameters"
     (let* ((env `(:request-method :POST
                   :request-uri "/foo"
                   :url-scheme "http"
                   :query-string "foo=bar&foo=baaz&bar=baar"
                   :headers ,(let ((headers (make-hash-table :test #'equal)))
                               (setf (gethash "content-type" headers)
                                     "application/json")
                               (setf (gethash "cookie" headers)
                                     "foo=bar;baaz=quux;bar=11")
                               headers)
                   :raw-body ,(flexi-streams:make-flexi-stream
                               (flexi-streams:make-in-memory-input-stream
                                #(123 34 104 101 108 108 111 34 58 34 119 111 114 108 100 34 125))
                               :external-format :utf-8)
                   :foo "bar"))
            (request (foo.lisp.lack/request:make-request env)))
       (ok request)
       (ok (eq (type-of request) 'lack/request:request))
       (ok (eq :POST (lack/request:request-method request)))
       (ok (equal "/foo" (lack/request:request-uri request)))
       (ok (equal "http" (lack/request:request-uri-scheme request)))
       (ok (hash-table-p (lack/request:request-headers request)))
       (ok (equal "application/json" (gethash "content-type" (lack/request:request-headers request))))
       (ok (equal "bar" (getf (lack/request:request-env request) :foo)))
       (ok (equal "foo=bar&foo=baaz&bar=baar" (lack/request:request-query-string request)))
       (ok (null (lack/request:request-query-parameters request)))
       (ok (null (getf env :query-parameters)))
       (ok (lack/request:request-raw-body request))
       (ok (null (lack/request:request-body-parameters request)))
       (ok (null (getf env :body-parameters)))
       (ok (equal "foo=bar;baaz=quux;bar=11" (gethash "cookie" (lack/request:request-headers request))))
       (ok (null (lack/request:request-cookies request)))
       (ok (null (getf env :cookies)))))

  (testing "maintains the original env list (not a copy) in the request env slot"
           (let* ((env `(:request-method :GET
                         :request-uri "/foo"
                         :url-scheme "http"
                         :headers ,(let ((headers (make-hash-table :test #'equal)))
                                     (setf (gethash "accept" headers)
                                           "text/html")
                                     headers)
                         :foo "bar"))
                  (request (foo.lisp.lack/request:make-request env)))
             (rplacd (last (lack/request:request-env request)) (list :baaz "quux"))
             (ok (equal "quux" (getf (lack/request:request-env request) :baaz)))
             (ok (equal "quux" (getf env :baaz))))))

(deftest request-cookies
    (testing "parses cookies on first access"
             (let* ((env `(:request-method :GET
                           :request-uri "/foo"
                           :url-scheme "http"
                           :headers ,(let ((headers (make-hash-table :test #'equal)))
                                       (setf (gethash "cookie" headers)
                                             "foo=bar;baaz=quux;bar=11")
                                       headers)))
                    (request (foo.lisp.lack/request:make-request env)))
               (ok (not (member :cookies (lack/request:request-env request))))
               (ok (null (lack/request:request-cookies request)))
               (ok (equal '(("foo" . "bar") ("baaz" . "quux") ("bar" . "11"))
                          (foo.lisp.lack/request:request-cookies request)))
               (ok (equal '(("foo" . "bar") ("baaz" . "quux") ("bar" . "11"))
                          (lack/request:request-cookies request)))
               (ok (equal '(("foo" . "bar") ("baaz" . "quux") ("bar" . "11"))
                          (getf env :cookies))))))

(deftest request-query-parameters
    (testing "parses query parameters on first access"
             (let* ((env `(:request-method :GET
                           :request-uri "/foo"
                           :url-scheme "http"
                           :query-string "foo=bar&foo=baaz&bar=baar"))
                    (request (foo.lisp.lack/request:make-request env)))
               (ok (equal "foo=bar&foo=baaz&bar=baar" (getf (lack/request:request-env request) :query-string)))
               (ok (not (member :query-parameters (lack/request:request-env request))))
               (let ((query-params (foo.lisp.lack/request:request-query-parameters request)))
                 (ok query-params)
                 (ok (member :query-parameters (lack/request:request-env request)))
                 (ok (equal (getf (lack/request:request-env request) :query-parameters)
                            '(("foo" . "bar") ("foo" . "baaz") ("bar" . "baar"))))
                 (ok (equal query-params '(("foo" . "bar") ("foo" . "baaz") ("bar" . "baar"))))
                 (ok (equal query-params (lack/request:request-query-parameters request)))))))

(deftest request-body-parameters
    (testing "parses the response body on first access"
             (let* ((env `(:request-method :POST
                           :request-uri "/foo"
                           :url-scheme "http"
                           :content-type "application/json"
                           :content-length 17
                           :raw-body ,(flexi-streams:make-flexi-stream
                                       (flexi-streams:make-in-memory-input-stream
                                        #(123 34 104 101 108 108 111 34 58 34 119 111 114 108 100 34 125))
                                       :external-format :utf-8)))
                    (request (foo.lisp.lack/request:make-request env)))
               (ok (not (member :body-parameters (lack/request:request-env request))))
               (ok (not (member :body-parameters env)))
               (ok (equal '(("hello" . "world"))
                          (foo.lisp.lack/request:request-body-parameters request)))
               (ok (equal '(("hello" . "world"))
                          (lack/request:request-body-parameters request)))
               (ok (equal '(("hello" . "world"))
                          (getf env :body-parameters))))))
