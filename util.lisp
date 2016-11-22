(in-package :png-read)

(defun big-endian-vector-to-integer (byte-vector)
  (iter (for i from (1- (length byte-vector)) downto 0)
        (for j from 0)
        (summing (ash (aref byte-vector j) (* 8 i)))))

(define-compiler-macro big-endian-vector-to-integer (&whole form byte-vector-form)
  (if (and (listp byte-vector-form)
           (eql (car byte-vector-form) 'subseq))
      (destructuring-bind (subseq seq start &optional (end nil)) byte-vector-form
        (declare (ignore subseq))
        (let ((seq-gensym (gensym "SEQ-"))
              (start-gensym (gensym "START-"))
              (end-gensym (gensym "END-")))
         `(let ((,seq-gensym ,seq)
                (,start-gensym ,start))
            (let ((,end-gensym
                   ,(if end
                        end
                        `(length ,seq-gensym))))
              (iter (for i from (- ,end-gensym ,start-gensym 1) downto 0)
                    (for j from ,start-gensym)
                    (summing (ash (aref ,seq-gensym j) (* 8 i))))))))
      form))

