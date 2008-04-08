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