(asdf:defsystem png-read
  :version "0"
  :description "A library for reading PNG files."
  :licence "BSD-style"
  :componenets ((:file "package")
		(:file "crc" :depends-on ("package"))
		(:file "chnuks" :depends-on ("package" "crc")))
  :depends-on (:iterate :chipz))
