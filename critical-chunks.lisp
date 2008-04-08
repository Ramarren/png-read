(in-package :png-read)

(defgeneric parse-critical-chunk (chunk-type chunk-data)
  (:method (chunk-type chunk-data)
    (warn "Unknown critical chunk ~a." chunk-type)))

(defmethod parse-critical-chunk ((chunk-type (eql |IHDR|)) chunk-data)
  (let ((width (big-endian-vector-to-integer (subseq chunk-data 0 4)))
	(height (big-endian-vector-to-integer (subseq chunk-data 4 8)))
	(colour-type (aref chunk-data 8))
	(compression (aref chunk-data 9))
	(filter-method (aref chunk-data 10))
	(interlace-method (aref chunk-data 11)))
    (setf (width *png-state*) width
	  (height *png-state*) height
	  (colour-type *png-state*) colour-type
	  (compression *png-state*) compression
	  (filter-methods *png-state*) filter-method
	  (interlace-method *png-state*) interlace-method)))