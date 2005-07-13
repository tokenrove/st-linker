;;; Code to support (simple) linking of object files into GEMDOS PRG
;;; files.  These are basically A.OUT format.

(in-package :st-linker)

(defparameter *aout-obj-header-length* 32) ; bytes

;; Object magic is STO^Z
(defun check-magic (stream)
  (unless (= (read-big-endian-data stream 32) #x53544F26)
    (error "Bad magic.")))


(defstruct module
  (name)
  (text-base 0)
  (text-size)
  (data-base 0)
  (data-size)
  (bss-base 0)
  (bss-size)
  (symbol-table-size)
  (entry-point)
  (text-relocations-size)
  (data-relocations-size)
  (symbols)
  (relocs))


(defstruct relocation
  (address)
  (symbol)
  (pc-relative-p)
  (length)
  (extern-p))

(defstruct linker-symbol
  (name)
  (value)
  (type)
  ;; global-p, debug-info
  (module))


(defun read-relocations (n-relocations stream)
  (let ((table (make-array (list n-relocations)
			   :element-type 'linker-symbol)))
    (dotimes (i n-relocations)
      (let ((address (read-big-endian-data stream 32))
	    (rest (read-big-endian-data stream 32)))
	(setf (aref table i)
	      (make-relocation :address address
			       :symbol (ldb (byte 24 8) rest)
			       :pc-relative-p (= 1 (ldb (byte 1 7) rest))
			       :length (ldb (byte 2 5) rest)
			       :extern-p (= 1 (ldb (byte 1 4) rest))))))
    table))

(defun read-symbol-table (n-symbols stream module)
  (let ((table (make-array (list n-symbols) :element-type 'relocation)))
    (dotimes (i n-symbols)
      (let ((name-idx (read-big-endian-data stream 32))
	    (type (read-big-endian-data stream 32))
	    (value (read-big-endian-data stream 32)))
	(setf (aref table i)
	      (make-linker-symbol :name name-idx
				  :type (ldb (byte 7 25) type)
				  :value value
				  :module module))))
    table))

(defun read-string-table (stream symbols)
  (dotimes (i (length symbols))
    (setf (linker-symbol-name (aref symbols i))
	  (read-nul-terminated-string stream))))


(defun link-prg (objects &key (out-name "aout.prg"))
  (let ((text-segment-size 0)
	(data-segment-size 0)
	(bss-size 0)
	(modules nil))
    (dolist (object objects)
      ;; load each object file, figure out sizes of segments
      (with-open-file (stream object :element-type 'unsigned-byte)
	(check-magic stream)
	(let ((text (read-big-endian-data stream 32))
	      (data (read-big-endian-data stream 32))
	      (bss (read-big-endian-data stream 32))
	      (symbol-count (read-big-endian-data stream 32))
	      (entry (read-big-endian-data stream 32))
	      (text-reloc (read-big-endian-data stream 32))
	      (data-reloc (read-big-endian-data stream 32)))
	  (file-position stream (+ *aout-obj-header-length*
				   text data))
	  (let ((relocs (read-relocations text-reloc stream))
		(symbols (read-symbol-table symbol-count stream object)))
	    (read-string-table stream symbols)
	    (push (make-module :name object :text-size text :data-size data
			       :bss-size bss :symbol-table-size symbol-count
			       :entry-point entry
			       :text-relocations-size text-reloc
			       :data-relocations-size data-reloc
			       :relocs relocs :symbols symbols)
		  modules))
	  (incf text-segment-size text)
	  (incf data-segment-size data)
	  (incf bss-size bss))))

    (allocate-module-bases modules text-segment-size data-segment-size)

    (with-open-file (prg-stream out-name :direction :output
				:element-type 'unsigned-byte
				:if-exists :new-version 
				:if-does-not-exist :create)
      ;; start writing header
      (write-big-endian-data prg-stream #x601a 16)
      (write-big-endian-data prg-stream text-segment-size 32)
      (write-big-endian-data prg-stream data-segment-size 32)
      (write-big-endian-data prg-stream bss-size 32)
      (write-big-endian-data prg-stream 0 32) ; no symbol table
      (write-big-endian-data prg-stream 0 (* 2 32)) ; reserved, flags
      (write-big-endian-data prg-stream 0 16) ; absflag

      (dolist (module modules)
	;; over each object, read text segments
	(with-open-file (obj-stream (module-name module)
				    :element-type 'unsigned-byte)
	  (file-position obj-stream *aout-obj-header-length*) ;start of text segment
	  (copy-from-stream obj-stream prg-stream 
			    (module-text-size module))))
      (dolist (module modules)
	;; over each object, read data segments
	(with-open-file (obj-stream (module-name module)
				    :element-type 'unsigned-byte)
	  (file-position obj-stream (+ *aout-obj-header-length*
				       (module-text-size module)))
	  (copy-from-stream obj-stream prg-stream
			    (module-data-size module))))

      (let ((relocs (process-link-time-relocations prg-stream modules)))
	(output-load-time-relocations prg-stream relocs)))))


(defun allocate-module-bases (modules text-segment-size data-segment-size)
  (let ((cur-text-base 0)
	(cur-data-base text-segment-size)
	(cur-bss-base (+ text-segment-size data-segment-size)))
    (dolist (module modules)
      (setf (module-text-base module) cur-text-base
	    (module-data-base module) cur-data-base
	    (module-bss-base module) cur-bss-base)
      (format t "~&relocating ~A at ~A, ~A, ~A" (module-name module)
	      (module-text-base module) (module-data-base module) 
	      (module-bss-base module))
      (incf cur-text-base (module-text-size module))
      (incf cur-data-base (module-data-size module))
      (incf cur-bss-base (module-bss-size module)))))

(defun process-link-time-relocations (stream modules)
  (let ((relocs nil))
    (dolist (module modules)
      ;; for each relocation, try and process.
      ;; if it needs to be relocated at load time, adjust for long
      ;; relocs, and push it onto the reloc stack.
      )))

(defun output-load-time-relocations (stream relocs)
  (write-big-endian-data stream 0 32)	;fixup offset
  ;; output relocations in PC order... make sure relocation is on word
  ;; boundry, and fix smaller relocs so that they're long (eg, word
  ;; fixups need to start two bytes earlier).  if delta from current
  ;; position to next fixup > 254, output byte 1 until < 254.  then
  ;; output offset of fixup.
  (write-byte 0 stream))		; no more relocs.


;;;; UTILITIES

(defun copy-from-stream (source destination length
			 &key (element-type 'unsigned-byte))
  "Copy LENGTH bytes of data from open stream SOURCE to open stream
DESTINATION."
  (let ((buffer (make-array '(4096) :element-type element-type)))
    (do ((bytes #1=(read-sequence buffer source) #1#)
	 (length length (- length bytes)))
	((or (= bytes 0) (<= length 0)))
      (write-sequence buffer destination :end (if (> bytes length)
						  length
						  bytes)))))

(defun read-big-endian-data (stream length)
  "Read LENGTH bits of data encoded big-endian from STREAM, returning
an integer.  LENGTH must be a multiple of 8."
  (assert (zerop (logand length 7)))
  (do ((pos (- length 8) (- pos 8))
       (value (read-byte stream) (logior (read-byte stream)
					 (ash value 8))))
      ((<= pos 0) value)))

(defun write-big-endian-data (stream data length)
  "Write LENGTH bits of the integer DATA to STREAM, in big-endian
order.  LENGTH must be a multiple of 8."
  (assert (zerop (logand length 7)))
  (do ((pos (- length 8) (- pos 8)))
      ((< pos 0))
    (write-byte (ldb (byte 8 pos) data) stream)))


(defun read-nul-terminated-string (stream)
  (do ((char (read-byte stream) (read-byte stream))
       (string (make-array '(0) :element-type 'character :adjustable t
			   :fill-pointer 0)))
      ((eql char 0) string)
    (vector-push-extend (code-char char) string)))