(in-package :png-read)

(defgeneric decode-data (colour-type data png-state))

(defmethod decode-data ((colour-type (eql :greyscale)) data png-state)
  (let ((h (height png-state))
        (w (width png-state))
        (bd (bit-depth png-state)))
    (let ((bda (max 1 (/ bd 8))))
      (setf (image-data png-state)
            (make-array (list w h) :element-type `(unsigned-byte ,bd)))
      (let ((filtered-scanline-length (1+ (ceiling (* bd w) 8))))
        (let ((scanlines (get-scanlines data h filtered-scanline-length)))
          (unfilter-scanlines scanlines bda)
          (let ((image-data (image-data png-state)))
            (iter (for scanline in-vector scanlines with-index k)
                  (iter (for x in-vector scanline from 1 with-index xi by bda)
                        (ecase bd
                          (1 (iter (for l from 0 below 8)
                                   (for ix next (+ (* (1- xi) 8) l))
                                   (until (>= ix w))
                                   (setf (aref image-data ix k)
                                         (ldb (byte 1 l) x))))
                          (2 (iter (for l from 0 below 4)
                                   (for ix next (+ (* (1- xi) 4) l))
                                   (until (>= ix w))
                                   (setf (aref image-data ix k)
                                         (ldb (byte 2 (* 2 l)) x))))
                          (4 (iter (for l from 0 below 2)
                                   (for ix next (+ (* (1- xi) 2) l))
                                   (until (>= ix w))
                                   (setf (aref image-data ix k)
                                         (ldb (byte 4 (* 4 l)) x))))
                          (8 (setf (aref image-data (1- xi) k)
                                   x))
                          (16 (setf (aref image-data (floor (1- xi) 2) k)
                                    (big-endian-vector-to-integer (subseq scanline xi (+ xi 2))))))))))
        png-state))))

(defmethod decode-data ((colour-type (eql :truecolor)) data png-state)
  (let ((h (height png-state))
        (w (width png-state))
        (bd (bit-depth png-state)))
    (let ((bda (/ bd 8)))
      (setf (image-data png-state)
            (make-array (list w h 3) :element-type `(unsigned-byte ,bd)))
      (let ((scanlines (get-scanlines data h (1+ (* bda w 3)))))
        (unfilter-scanlines scanlines (* bda 3))
        (let ((image-data (image-data png-state)))
          (iter (for scanline in-vector scanlines with-index k)
                (iter (for x in-vector scanline from 1 with-index xi by bda)
                      (for y from 0)
                      (setf (aref image-data (floor (1- xi) (* bda 3)) k (mod y 3))
                            (big-endian-vector-to-integer (subseq scanline xi (+ xi bda)))))))
        png-state))))

(defun set-image-slice-to-index (x y idx pallete image-data)
  (iter (for k from 0 below 3)
        (setf (aref image-data x y k)
              (aref pallete idx k))))

(defmethod decode-data ((colour-type (eql :indexed-colour)) data png-state)
  (let ((h (height png-state))
        (w (width png-state))
        (pal (pallete png-state))
        (bd (bit-depth png-state)))
    (let ((bda (max 1 (/ bd 8))))
     (setf (image-data png-state)
           (make-array (list w h 3) :element-type '(unsigned-byte 8)))
     (setf (index-data png-state)
           (make-array (list w h) :element-type `(unsignned-byte ,bd)))
     (let ((scanlines (get-scanlines data h (1+ (ceiling (* bd w) 8)))))
       (unfilter-scanlines scanlines (* bda 4))
       (let ((image-data (image-data png-state))
             (index-data (index-data png-state)))
         (iter (for scanline in-vector scanlines with-index k)
               (iter (for x in-vector scanline from 1 with-index xi by bda)
                     (ecase bd
                          (1 (iter (for l from 0 below 8)
                                   (for xii next (+ (* (1- xi) 8) l))
                                   (until (>= xii w))
                                   (let ((idx (ldb (byte 1 l) x)))
                                     (setf (aref index-data xii k) idx)
                                     (set-image-slice-to-index xii k idx pal image-data))))
                          (2 (iter (for l from 0 below 4)
                                   (for xii next (+ (* (1- xi) 4) l))
                                   (until (>= xii w))
                                   (let ((idx (ldb (byte 2 (* 2 l)) x)))
                                     (setf (aref index-data xii k) idx)
                                     (set-image-slice-to-index xii k idx pal image-data))))
                          (4 (iter (for l from 0 below 2)
                                   (for xii next (+ (* (1- xi) 2) l))
                                   (until (>= xii w))
                                   (let ((idx (ldb (byte 4 (* 4 l)) x)))
                                     (setf (aref index-data xii k) idx)
                                     (set-image-slice-to-index xii k idx pal image-data))))
                          (8 (setf (aref index-data (1- xi) k) x)
                             (set-image-slice-to-index (1- xi) k x pal image-data))))))
       png-state))))

(defmethod decode-data ((colour-type (eql :greyscale-alpha)) data png-state)
  (let ((h (height png-state))
        (w (width png-state))
        (bd (bit-depth png-state)))
    (let ((bda (/ bd 8)))
      (setf (image-data png-state)
            (make-array (list w h 2) :element-type `(unsigned-byte ,bd)))
      (let ((filtered-scanline-length (1+ (ceiling (* 2 bd w) 8))))
        (let ((scanlines (get-scanlines data h filtered-scanline-length)))
          (unfilter-scanlines scanlines (* 2 bda))
          (let ((image-data (image-data png-state)))
            (iter (for scanline in-vector scanlines with-index k)
                  (iter (for x in-vector scanline from 1 with-index xi by bda)
                        (for y from 0)
                        (ecase bd
                          (8 (setf (aref image-data (floor (1- xi) 2) k (mod y 2))
                                   x))
                          (16 (setf (aref image-data (floor (1- xi) 4) k (mod y 2))
                                    (big-endian-vector-to-integer (subseq scanline xi (+ xi 2))))))))))
        png-state))))

(defmethod decode-data ((colour-type (eql :truecolor-alpha)) data png-state)
  (let ((h (height png-state))
        (w (width png-state))
        (bd (bit-depth png-state)))
    (let ((bda (/ bd 8)))
     (setf (image-data png-state)
           (make-array (list w h 4) :element-type `(unsigned-byte ,bd)))
     (let ((scanlines (get-scanlines data h (1+ (* bda w 4)))))
       (unfilter-scanlines scanlines (* bda 4))
       (let ((image-data (image-data png-state)))
         (iter (for scanline in-vector scanlines with-index k)
               (iter (for x in-vector scanline from 1 with-index xi by bda)
                     (for y from 0)
                     (setf (aref image-data (floor (1- xi) (* bda 4)) k (mod y 4))
                           (big-endian-vector-to-integer (subseq scanline xi (+ xi bda)))))))
       png-state))))

(defun get-scanlines (data h filtered-scanline-length)
  (let ((scanlines (make-array h)))
   (iter (for i from 0 below h)
         (until (>= (* i filtered-scanline-length) (length data)))
         (setf (aref scanlines i) (subseq data
                                          (* i filtered-scanline-length)
                                          (min (length data) (* (1+ i) filtered-scanline-length)))))
   scanlines))

(defun sub-byte (xi scanline pixel-length)
  (if (> xi pixel-length)
      (aref scanline (- xi pixel-length))
      0))

(defun up-byte (xi k scanlines)
  (if (zerop k)
      0
      (aref (aref scanlines (1- k)) xi)))

(defun subup-byte (xi k scanlines pixel-length)
  (if (or (zerop k)
          (<= xi pixel-length))
      0
      (aref (aref scanlines (1- k)) (- xi pixel-length))))

(defun paeth-predictor (xi k scanlines pixel-length)
  (let ((scanline (aref scanlines k)))
    (let ((a (sub-byte xi scanline pixel-length))
          (b (up-byte xi k scanlines))
          (c (subup-byte xi k scanlines pixel-length)))
      (let ((p (- (+ a
                     b)
                  c)))
        (let ((pa (abs (- p a)))
              (pb (abs (- p b)))
              (pc (abs (- p c))))
          (cond ((and (<= pa pb)
                      (<= pa pc))
                 a)
                ((<= pb pc)
                 b)
                (t c)))))))

(defun unfilter-scanlines (scanlines pixel-length)
  (iter (for scanline in-vector scanlines with-index k)
        (unless (zerop (length scanline))
         (ecase (aref scanline 0)
           (0 nil)
           (1 (iter (for x in-vector scanline from 1 with-index xi)
                    (setf (aref scanline xi)
                          (mod (+ x (sub-byte xi scanline pixel-length)) 256))))
           (2 (iter (for x in-vector scanline from 1 with-index xi)
                    (setf (aref scanline xi)
                          (mod (+ x (up-byte xi k scanlines)) 256))))
           (3 (iter (for x in-vector scanline from 1 with-index xi)
                    (setf (aref scanline xi)
                          (mod (+ x (floor (+ (sub-byte xi scanline pixel-length)
                                              (up-byte xi k scanlines))
                                           2))
                               256))))
           (4 (iter (for x in-vector scanline from 1 with-index xi)
                    (setf (aref scanline xi)
                          (mod (+ x (paeth-predictor xi k scanlines pixel-length))
                               256)))))))
  scanlines)

(defun finish-decoding (png-state)
  (unless (datastream png-state)
    (if (png-file png-state)
        (error "No data in image file ~a." (png-file png-state))
        (error "No data in stream.")))
  (let ((decompressed-data (decompress nil :zlib (datastream png-state))))
    (ecase (interlace-method png-state)
      (:no-interlace (decode-data (colour-type png-state) decompressed-data png-state))
      (:adam7-interlace (decode-interlaced decompressed-data png-state)))
    (values (image-data png-state)
            png-state)))