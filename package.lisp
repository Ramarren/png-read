(defpackage #:png-read
  (:use #:cl #:iterate #:chipz #:babel)
  (:export #:read-png-file #:read-png-datastream
	   #:png-state
	   #:image-data #:width #:height #:bit-depth
	   #:colour-type #:palette
	   ;ancillaries
	   #:transparency #:gamma #:significant-bits #:rendering-intent #:textual-data
	   #:preferred-background #:image-histogram #:physical-dimensions #:last-modification))
