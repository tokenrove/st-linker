
(in-package :st-linker)

(defparameter *aout-obj-header-length* 32) ; bytes

;; Object magic is STO^Z
(defun check-magic (stream)
  (unless (= (read-big-endian-data stream 32) #x53544F26)
    (error "Bad magic.")))



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
			       :extern-p (= 1 (ldb (byte 1 6) rest))
			       :length (ldb (byte 6 0) rest)))))
    table))

(defun bits->linker-symbol-type (bits)
  (nth bits '(text data bss absolute extern)))

(defun read-symbol-table (n-symbols stream module)
  (let ((table (make-array (list n-symbols) :element-type 'relocation)))
    (dotimes (i n-symbols)
      (let ((name-idx (read-big-endian-data stream 32))
	    (type (read-big-endian-data stream 32))
	    (value (read-big-endian-data stream 32)))
	(setf (aref table i)
	      (make-linker-symbol :name name-idx
				  :type (bits->linker-symbol-type
					 (ldb (byte 7 25) type))
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
