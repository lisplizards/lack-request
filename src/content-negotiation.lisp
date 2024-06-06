;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: MIT

(in-package #:foo.lisp.lack/request/content-negotiation)

(defun negotiate-media-type (request acceptable-media-type-strings)
  "Content-type negotiation function based on the request Accept header.

Iterates over the parsed MEDIA-TYPE instances in the ACCEPT slot of
REQUEST, matching against parsed media-types constructed from parameter
ACCEPTABLE-MEDIA-TYPE-STRINGS.

Returns NIL when there is no match.

Returns two values when there is a match:
1. the matched media-type as a string
2. the matched media-type as a LACK/MEDIA-TYPE:MEDIA-TYPE instance"
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type lack/request:request request)
           (type list acceptable-media-type-strings))
  (let ((accept (lack/request:request-accept request))
        (acceptable-media-types (mapcar #'lack/media-type:make-media-type acceptable-media-type-strings)))
    (declare (type list accept acceptable-media-types))
    (dolist (media-type accept)
      (declare (type lack/media-type:media-type media-type))
      (loop for acceptable-media-type of-type lack/media-type:media-type in acceptable-media-types
            for acceptable-media-type-string of-type string in acceptable-media-type-strings
            when (lack/media-type:match-media-type media-type acceptable-media-type)
              do (return-from negotiate-media-type
                   (values
                    acceptable-media-type-string
                    acceptable-media-type))))))
