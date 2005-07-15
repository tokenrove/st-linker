;;; Code to support (simple) linking of object files into GEMDOS PRG
;;; files.  These are basically A.OUT format.

(in-package :st-linker)

(defparameter *aout-obj-header-length* 32) ; bytes
(defparameter *aout-prg-header-length* 28) ; bytes

;; Object magic is STO^Z
(defun check-magic (stream)
  (unless (= (read-big-endian-data stream 32) #x53544F26)
    (error "Bad magic.")))


(defstruct module
  (name)
  (segment-bases)
  (segment-sizes)
  (symbol-table-size)
  (entry-point)
  (symbols)
  (relocations))

(defun module-segment-base (module segment)
  (cdr (assoc segment (module-segment-bases module))))
(defun module-segment-size (module segment)
  (cdr (assoc segment (module-segment-sizes module))))
(defun module-segment-relocations (module segment)
  (cdr (assoc segment (module-relocations module))))


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
			       :length (ash 1 (ldb (byte 2 5) rest))
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

(defun read-object-header (stream object)
  (let ((segment-sizes (mapcar (lambda (x)
				 (cons x (read-big-endian-data stream 32)))
			       '(text data bss)))
	(symbol-count (read-big-endian-data stream 32))
	(entry (read-big-endian-data stream 32))
	(reloc-sizes (mapcar (lambda (x)
			       (cons x (read-big-endian-data stream 32)))
			     '(text data))))
    (make-module :name object
		 :segment-sizes segment-sizes
		 :symbol-table-size symbol-count
		 :entry-point entry
		 :relocations reloc-sizes)))


(defun read-module (filename)
  (with-open-file (stream filename :element-type 'unsigned-byte)
    (check-magic stream)
    (let ((module (read-object-header stream filename)))
      (file-position stream (+ *aout-obj-header-length*
			       (module-segment-size module 'text)
			       (module-segment-size module 'data)))
      (let ((relocs (mapcar
		     (lambda (x)
		       (cons x 
			     (read-relocations
			      (module-segment-relocations module x)
			      stream)))
		     '(text data)))
	    (symbols (read-symbol-table (module-symbol-table-size module)
					stream filename)))
	(read-string-table stream symbols)
	(setf (module-relocations module) relocs
	      (module-symbols module) symbols))
      module)))


(defun link-prg (objects &key (out-name "aout.prg"))
  (let ((segment-sizes (list (cons 'text 0)
			     (cons 'data 0)
			     (cons 'bss 0)))
	(modules nil))
    (dolist (object objects)
      (push (read-module object) modules)
      ;; XXX merge symbols into global symbol table
      (dolist (x segment-sizes)
	(incf (cdr x) (module-segment-size (first modules) (car x)))))

    (allocate-module-bases modules segment-sizes)

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

      (let ((fixups (process-link-time-relocations prg-stream modules)))
	(output-load-time-relocations prg-stream fixups)))))


(defun allocate-module-bases (modules segment-sizes)
  (let ((bases (list (cons 'text 0)
		     (cons 'data #1=(cdr (assoc 'text segment-sizes)))
		     (cons 'bss (+ (cdr (assoc 'data segment-sizes))
				   #1#)))))
    (dolist (module modules)
      (setf (module-segment-bases module) (copy-tree bases))
      (format t "~&relocating ~A at ~A" (module-name module)
	      (module-segment-bases module))
      (dolist (x bases)
	(incf (cdr x) (module-segment-size module (car x)))))))

(defun process-link-time-relocations (stream modules)
  (let ((fixups nil)
	(position (file-position stream)))
    (dolist (module modules)
      (mapcar
       (lambda (segment)
	 (dovector (reloc (module-segment-relocations module segment))
	   (format t "~&doing reloc ~A" reloc)
	   (awhen (if (relocation-pc-relative-p reloc)
		      (if (relocation-extern-p reloc)
			  (relocate-pcrel-extern stream module segment reloc)
			  (relocate-pc-relative stream module segment reloc))
		      (if (relocation-extern-p reloc)
			  (relocate-absolute-extern stream module segment reloc)
			  (relocate-absolute stream module segment reloc)))
	     (push it fixups))))
       '(text data)))
    (file-position stream position)
    (sort fixups #'<=)))

(defun file-offset-of-address (address)
  (+ address *aout-prg-header-length*))

(defun relocate-absolute (stream module segment reloc)
  (setf (relocation-symbol reloc) (nth (relocation-symbol reloc)
				       '(text data bss)))
  (let ((base (module-segment-base module (relocation-symbol reloc))))
    ;; add base to value
    (incf (relocation-address reloc) (module-segment-base module segment))
    (file-position stream
		   (file-offset-of-address (relocation-address reloc)))
    (let* ((length (* (relocation-length reloc) 8))
	   (value (read-big-endian-data stream length)))
      (file-position stream
		     (file-offset-of-address (relocation-address reloc)))
      (write-big-endian-data stream (+ value base) length)
      ;; adjust address according to length
      (when (/= length 32)
	(decf (relocation-address reloc) (ceiling (- 32 length) 8))))
    ;; return fixup
    (relocation-address reloc)))

(defun relocate-absolute-extern (stream module segment reloc)
  nil)

(defun relocate-pcrel-extern (stream module segment reloc)
  nil)
(defun relocate-pc-relative (stream module segment reloc)
  (format t "~&wish i could say this was being handled: ~A ~A ~A"
	  #1=(module-segment-base module segment)
	  #2=(module-segment-base module (relocation-symbol reloc))
	  (- #1# #2#))
  nil)

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
      (format t "~&outputting fixup ~A, delta ~A" addr delta)
      (do () ((< (- addr delta) 254))
	(write-byte 1 stream)
	(incf delta 254))
      (write-byte (- addr delta) stream)
      (setf delta addr))
    (write-byte 0 stream)))		; no more relocs.


;;;; UTILITIES

(defmacro dovector ((var vector) &body body)
  "Iterate VAR across VECTOR."
  `(loop for ,var across ,vector
         do (progn ,@body)))

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