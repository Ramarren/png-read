(in-package :png-read)

(defclass png-state ()
  ((width :accessor width)
   (height :accessor height)
   (bit-depth :accessor bit-depth)
   (colour-type :accessor colour-type)
   (compression :accessor compression)
   (filter-method :accessor filter-method)
   (interlace-method :accessor interlace-method)
   (pallete :accessor pallete)
   (datastream :accessor datastream :initform nil)))

(defvar *png-state* nil)