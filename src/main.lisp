;; Copyright (c) 2015 Eitaro Fukamachi
;; Copyright (c) 2024 John Newton (extract and adapt)
;; SPDX-License-Identifier: MIT

(in-package #:foo.lisp.lack/request)

(declaim (ftype (function (list) lack/request:request) make-request))
(defun make-request (env)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type list env))
  (let ((request (apply #'lack/request::%make-request :env env :allow-other-keys t env)))
    (declare (type lack/request:request request))
    (with-slots (lack/request::method
                 lack/request::uri
                 lack/request::uri-scheme
                 lack/request::accept
                 lack/request::headers)
        request
      (declare (type (or null keyword) lack/request::method)
               (type (or null string) lack/request::uri)
               (type (or null string) lack/request::uri-scheme))
      (unless lack/request::method
        (setf lack/request::method (getf env :request-method)))
      (unless lack/request::uri
        (setf lack/request::uri (getf env :request-uri)))
      (unless lack/request::uri-scheme
        (setf lack/request::uri-scheme (getf env :url-scheme)))
      (when (hash-table-p lack/request::headers)
        (setf lack/request::accept
              (mapcar #'lack/media-type:make-media-type
                      (ppcre:split "\\s*[,]\\s*"
                                   (gethash "accept" lack/request::headers))))))
    request))

(declaim (ftype (function (lack/request:request) list) request-cookies))
(defun request-cookies (request)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type lack/request:request request))
  (when (lack/request:request-cookies request)
    (return-from request-cookies
      (lack/request:request-cookies request)))
  (with-slots (lack/request::headers lack/request::env)
      request
    (let ((cookie (and (hash-table-p lack/request::headers)
                       (gethash "cookie" lack/request::headers))))
      (when cookie
        (setf (lack/request:request-cookies request)
              (loop for kv in (ppcre:split "\\s*[,;]\\s*" cookie)
                    append (quri:url-decode-params kv :lenient t)))
        (rplacd (last lack/request::env)
                (list :cookies (lack/request:request-cookies request)))
        (lack/request:request-cookies request)))))

(declaim (ftype (function (lack/request:request) list) request-query-parameters))
(defun request-query-parameters (request)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type lack/request:request request))
  (with-slots (lack/request::query-parameters lack/request::query-string)
      request
    (declare (type (or null string) lack/request::query-string))
    (when lack/request::query-parameters
      (return-from request-query-parameters
        lack/request::query-parameters))
    (when lack/request::query-string
      (setf lack/request::query-parameters
            (quri:url-decode-params lack/request::query-string :lenient t))
      (rplacd (last (lack/request:request-env request))
              (list :query-parameters lack/request::query-parameters))
      lack/request::query-parameters)))

(declaim (ftype (function (lack/request:request) list) request-body-parameters))
(defun request-body-parameters (request)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type lack/request:request request))
  (let ((body-parameters (lack/request:request-body-parameters request)))
    (when body-parameters
      (return-from request-body-parameters
        body-parameters)))
  (with-slots (lack/request::body-parameters
               lack/request::raw-body
               lack/request::content-length
               lack/request::content-type
               lack/request::env)
      request
    (declare (type (or null stream) lack/request::raw-body)
             (type (or null integer) lack/request::content-length)
             (type (or null string) lack/request::content-type)
             (type list lack/request::env))
    (when lack/request::raw-body
      (unless (typep lack/request::raw-body 'circular-streams:circular-input-stream)
        (setf lack/request::raw-body
              (circular-streams:make-circular-input-stream lack/request::raw-body)))
      (when (and (null lack/request::body-parameters)
                 (lack/request::request-has-body-p request)
                 (stringp lack/request::content-type))
        (let ((parsed (http-body:parse lack/request::content-type
                                       lack/request::content-length
                                       lack/request::raw-body)))
          (when (and (consp parsed)
                     (every #'consp parsed))
            (setf lack/request::body-parameters parsed))
          (file-position lack/request::raw-body 0)
          (setf (getf lack/request::env :raw-body)
                lack/request::raw-body)
          (rplacd (last lack/request::env)
                  (list :body-parameters lack/request::body-parameters))
          lack/request::body-parameters)))))
