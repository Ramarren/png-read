(asdf:defsystem png-read
  :version "0"
  :description "A library for reading PNG files."
  :licence "BSD-style"
  :components ((:file "package")
		(:file "crc" :depends-on ("package"))
		(:file "critical-chunks" :depends-on ("package"))
		(:file "ancillary-chunks" :depends-on ("package"))
		(:file "basic-chunks" :depends-on ("package" "crc" "critical-chunks" "ancillary-chunks")))
  :depends-on (:iterate :chipz))
