(in-package :png-read)

(defgeneric parse-critical-chunk (chunk-type chunk-data)
  (:method (chunk-type chunk-data)
    (warn "Unknown critical chunk ~a." chunk-type)))

(defmethod parse-critical-chunk ((chunk-type (eql '|IHDR|)) chunk-data)
  (let ((width (big-endian-vector-to-integer (subseq chunk-data 0 4)))
	(height (big-endian-vector-to-integer (subseq chunk-data 4 8)))
	(bit-depth (aref chunk-data 8))
	(colour-type (aref chunk-data 9))
	(compression (aref chunk-data 10))
	(filter-method (aref chunk-data 11))
	(interlace-method (aref chunk-data 12)))
    (setf (width *png-state*) width
	  (height *png-state*) height
	  (bit-depth *png-state*) bit-depth
	  (colour-type *png-state*) (ecase colour-type
				      (0 :greyscale)
				      (2 :truecolor)
				      (3 :indexed-color)
				      (4 :greyscale-alpha)
				      (6 :truecolor-alpha))
	  (compression *png-state*) (ecase compression
				      (0 :zlib))
	  (filter-method *png-state*) (ecase filter-method
					(0 :standard-filter))
	  (interlace-method *png-state*) (ecase interlace-method
					   (0 :no-interlace)
					   (1 :adam7-interlace)))))

(defmethod parse-critical-chunk ((chunk-type (eql '|PLTE|)) chunk-data)
  (unless (zerop (mod (length chunk-data) 3))
    (error "Corrupted pallete data."))
  (let ((data-length (/ (length chunk-data) 3)))
   (let ((pallete-array (make-array (list data-length 3) :element-type '(unsigned-byte 8))))
     (iter (for i from 0 below data-length)
	   (setf (aref pallete-array i 0) (aref chunk-data (* i 3))
		 (aref pallete-array i 1) (aref chunk-data (+ 1 (* i 3)))
		 (aref pallete-array i 2) (aref chunk-data (+ 2 (* i 3)))))
     (setf (pallete *png-state*) pallete-array))))

(defmethod parse-critical-chunk ((chunk-type (eql '|IDAT|)) chunk-data)
  (setf (datastream *png-state*) (concatenate '(vector (unsigned-byte 8)) (datastream *png-state*) chunk-data)))

(defmethod parse-critical-chunk ((chunk-type (eql '|IEND|)) chunk-data)
  (finish-decoding *png-state*))
