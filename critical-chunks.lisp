(in-package :png-read)

(defgeneric parse-critical-chunk (chunk-type chunk-data)
  (:method (chunk-type chunk-data)
    (error "Unknown critical chunk.")))
