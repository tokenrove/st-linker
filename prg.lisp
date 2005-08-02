;;; Code to support (simple) linking of object files into GEMDOS PRG
;;; files.  These are basically A.OUT format.

(in-package :st-linker)

(defparameter *aout-prg-header-length* 28) ; bytes

(defun link-prg (modules segment-sizes out-name)
  (with-open-file (prg-stream out-name :direction :io
			      :element-type 'unsigned-byte
			      :if-exists :new-version 
			      :if-does-not-exist :create)
    ;; start writing header
    (write-big-endian-data prg-stream #x601a 16)
    (mapcar (lambda (x)
	      (write-big-endian-data prg-stream
				     (cdr (assoc x segment-sizes))
				     32))
	    '(text data bss))
    (write-big-endian-data prg-stream 0 32) ; no symbol table
    (write-big-endian-data prg-stream 0 (* 2 32)) ; reserved, flags
    (write-big-endian-data prg-stream 0 16) ; absflag

    (dolist (module modules)
      ;; over each object, read text segments
      (with-open-file (obj-stream (module-name module)
				  :element-type 'unsigned-byte)
	(file-position obj-stream *aout-obj-header-length*) ;start of text segment
	(copy-from-stream obj-stream prg-stream 
			  (module-segment-size module 'text))))
    (dolist (module modules)
      ;; over each object, read data segments
      (with-open-file (obj-stream (module-name module)
				  :element-type 'unsigned-byte)
	(file-position obj-stream (+ *aout-obj-header-length*
				     (module-segment-size module 'text)))
	(copy-from-stream obj-stream prg-stream
			  (module-segment-size module 'data))))

    (let* ((*current-header-length* *aout-prg-header-length*)
	   (fixups (process-link-time-relocations prg-stream modules)))
      (output-load-time-relocations prg-stream fixups))))


(defun output-load-time-relocations (stream fixups)
  (let ((delta (or (and fixups (first fixups)) 0)))
    (write-big-endian-data stream delta 32) ;fixup offset
    (format t "~&outputting delta of ~A" delta)
    ;; output relocations in PC order... make sure relocation is on word
    ;; boundry, and fix smaller relocs so that they're long (eg, word
    ;; fixups need to start two bytes earlier).  if delta from current
    ;; position to next fixup > 254, output byte 1 until < 254.  then
    ;; output offset of fixup.
    (dolist (addr (cdr fixups))
      (assert (= 0 (mod addr 2)))
      (do () ((< (- addr delta) 254))
	(write-byte 1 stream)
	(incf delta 254))
      (write-byte (- addr delta) stream)
      (setf delta addr))
    (write-byte 0 stream)))		; no more relocs.
