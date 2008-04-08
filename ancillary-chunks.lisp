(in-package :png-read)

(defgeneric parse-ancillary-chunk (chunk-type chunk-data)
  (:method (chunk-type chunk-data)
    (warn "Unknown ancillary chunk ~a." chunk-type)))
