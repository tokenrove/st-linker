;;; Code to support (simple) linking of object files into GEMDOS PRG
;;; files.  These are basically A.OUT format.

(in-package :st-linker)

(defparameter *aout-header-length* 32)	; bytes

;; Object magic is STO^Z
(defun check-magic (stream)
  (unless (= (read-big-endian-data stream 32) #x53544F26)
    (error "Bad magic.")))


(defstruct module
  (name)
  (text-size)
  (data-size)
  (bss-size)
  (symbol-table-size)
  (entry-point)
  (text-relocations-size)
  (data-relocations-size)
  (symbols)
  (relocs))


(defstruct symbol
  (name)
  (value)
  (module))


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
	      (sym (read-big-endian-data stream 32))
	      (entry (read-big-endian-data stream 32))
	      (text-reloc (read-big-endian-data stream 32))
	      (data-reloc (read-big-endian-data stream 32)))
	  (incf text-segment-size text)
	  (incf data-segment-size data)
	  (incf bss-size bss)
	  (file-position stream (+ *aout-header-length*
				   text data))
	  (read-relocations)
	  (read-symbol-table)
	  (read-string-table)
	  (push (make-module :text-size text :data-size data
			     :bss-size bss :symbol-table-size sym
			     :entry-point entry
			     :text-relocations-size text-reloc
			     :data-relocations-size data-reloc)
		modules))))

  (with-open-file (prg-stream out-name :direction :output
			      :element-type 'unsigned-byte
			      :if-exists :new-version 
			      :if-does-not-exist :create)
    ;; start writing header
    (write-big-endian-data prg-stream #x601a 16)
    (write-big-endian-data prg-stream text-segment-size 32)
    (write-big-endian-data prg-stream data-segment-size 32)
    (write-big-endian-data prg-stream bss-size 32)
    (write-big-endian-data prg-stream 0 32)	; no symbol table
    (write-big-endian-data prg-stream 0 (* 3 32)) ; reserved, flags, abs
    (dolist (module modules)
      ;; over each object, read text segments
      (with-open-file (obj-stream (module-name module)
				  :element-type 'unsigned-byte)
	(file-position obj-stream #x20)	;start of text segment
	(copy-from-stream obj-stream prg-stream 
			  (module-text-size module))))
    (dolist (module modules)
      ;; over each object, read data segments
      (with-open-file (obj-stream (module-name module)
				  :element-type 'unsigned-byte)
	(file-position obj-stream (+ #x20
				     (module-text-size module)))
	(copy-from-stream obj-stream prg-stream
			  (module-data-size module))))

    (process-link-time-relocations prg-stream modules)
    (output-load-time-relocations prg-stream))))

(defun process-link-time-relocations (stream modules))



(defun copy-from-stream (source destination length
			 &key (element-type 'unsigned-byte))
  "Copy all data from open stream SOURCE to open stream DESTINATION.
SOURCE is positioned at its beginning, and read until it reaches the
end of file."
  (file-position source 0)
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
      ((< pos 0))))

(defun write-big-endian-data (stream data length)
  "Write LENGTH bits of the integer DATA to STREAM, in big-endian
order.  LENGTH must be a multiple of 8."
  (assert (zerop (logand length 7)))
  (do ((pos (- length 8) (- pos 8)))
      ((< pos 0))
    (write-byte (ldb (byte 8 pos) data) stream)))

