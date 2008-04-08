(in-package :png-read)

(defun read-png-file (file)
  (with-open-file (png-stream file :direction :input :element-type '(unsigned-byte 8))
    (read-png-datastream png-stream)))

(defvar *png-header* #(137 80 78 71 13 10 26 10))

(defun read-png-datastream (png-stream)
  (let ((header (make-array (length *png-header*) :element-type '(unsigned-byte 8))))
    (read-sequence header png-stream)
    (cond
      ((every #'eql *png-header* header)
       (read-png-chunks png-stream))
      (t (error "Not PNG file.")))))

(defun big-endian-vector-to-integer (byte-vector)
  (check-type byte-vector (vector (unsigned-byte 8)))
  (iter (for i from (1- (length byte-vector)) downto 0)
	(for j from 0)
	(summing (ash (aref byte-vector j) (* 8 i)))))


(defun read-png-chunks (png-stream)
  (let ((length-field (make-array 4 :element-type '(unsigned-byte 8)))
	(type-field (make-array 4 :element-type '(unsigned-byte 8)))
	(crc-field (make-array 4 :element-type '(unsigned-byte 8)))
	(*png-state* (make-instance 'png-state)))
    (let ((crc-ok
	   (iter
	     (for read-status next (read-sequence length-field png-stream))
	     (for type-status next (read-sequence type-field png-stream))
	     (until (zerop read-status))
	     (assert (eql read-status 4))
	     (assert (eql type-status 4))
	     (let ((chunk-length (big-endian-vector-to-integer length-field))
		   (type-string (map 'string #'code-char type-field)))
	       (let ((chunk-data (make-array chunk-length :element-type '(unsigned-byte 8))))
		 (let ((data-status (read-sequence chunk-data png-stream)))
		   (assert (eql data-status chunk-length))
		   (let ((crc-status (read-sequence crc-field png-stream)))
		     (assert (eql crc-status 4))
		     (let ((read-crc (big-endian-vector-to-integer crc-field))
			   (computed-crc (finish-crc (updated-crc (start-crc type-field) chunk-data))))
		       (collect (eql read-crc computed-crc))))))))))
      (values *png-state* (every #'identity crc-ok)))))

(defun parse-chunk (chunk-type chunk-data)
  (let ((criticalp (char= (char chunk-type 0) (char (string-upcase chunk-type :end 1) 0))))
    (if criticalp
	(parse-critical-chunk (intern chunk-type) chunk-data)
	(parse-ancillary-chunk (intern chunk-type) chunk-data))))
