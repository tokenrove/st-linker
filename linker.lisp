;;;
;;; Main linker body.
;;;
;;; Julian Squires / 2005
;;;

(in-package :st-linker)

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

(defvar *global-symbols* nil)
(defvar *current-header-length* nil)



(defun link (objects &key (out-name "a.out")
	     (format :gemdos-prg)
	     (entry-point 0))
  "Link the objects specified by the filenames contained in the list
OBJECTS into a binary called OUT-NAME.  The objects are linked
serially by list order."
  (setf *global-symbols* (make-hash-table :test #'equal))
  (let* ((segment-sizes (list (cons 'text 0)
			     (cons 'data 0)
			     (cons 'bss 0)))
	 (modules (mapcar
		   (lambda (object)
		     "Read a module with filename OBJECT, merge symbols
into global symbol table, and update segment sizes."
		     (let ((module (read-module object)))
		       ;; merge symbols into global symbol table
		       (dovector (sym (module-symbols module))
			 (setf (linker-symbol-module sym) module)
			 (sif (gethash (linker-symbol-name sym) *global-symbols*)
			      (push sym it)
			      (setf it (list sym))))
		       (dolist (x segment-sizes)
			 (incf (cdr x) (module-segment-size module (car x))))
		       module))
		   objects)))

    (allocate-module-bases modules segment-sizes)

    (ecase format
      (:gemdos-prg (link-prg modules segment-sizes out-name))
      (:binary (link-raw-bin modules segment-sizes out-name entry-point)))))

(defun allocate-module-bases (modules segment-sizes)
  (let ((bases (list (cons 'text 0)
		     (cons 'data #1=(cdr (assoc 'text segment-sizes)))
		     (cons 'bss (+ (cdr (assoc 'data segment-sizes))
				   #1#)))))
    (dolist (module modules)
      (setf (module-segment-bases module) (copy-list bases))
      (format t "~&relocating ~A at ~A" (module-name module)
	      (module-segment-bases module))
      (dolist (x bases)
	(incf (cdr x) (module-segment-size module (car x)))))))


(defun fix-relocation-symbol (reloc module)
  (if (relocation-extern-p reloc)
      (setf (relocation-symbol reloc) (aref (module-symbols module)
					    (relocation-symbol reloc)))
      (setf (relocation-symbol reloc) (nth (relocation-symbol reloc)
					   '(text data bss)))))

(defun fix-relocation-address (reloc module segment)
  (incf (relocation-address reloc)
	(module-segment-base module segment)))


(defun process-link-time-relocations (stream modules)
  "Goes through the relocations for each module, applies whichever
relocations can be done at link time, and then pushes the remainder
onto the fixup list, which it returns, in ascending order of address."
  (let ((fixups nil)
	(position (file-position stream)))
    (dolist (module modules)
      (mapcar
       (lambda (segment)
	 (dovector (reloc (module-segment-relocations module segment))
	   (fix-relocation-symbol reloc module)
	   (fix-relocation-address reloc module segment)
	   (awhen
	       (if (relocation-pc-relative-p reloc)
		   (if (relocation-extern-p reloc)
		       (relocate-pcrel-extern stream module segment reloc)
		       (relocate-pc-relative stream module segment reloc))
		   (if (relocation-extern-p reloc)
		       (relocate-abs-extern stream module segment reloc)
		       (relocate-absolute stream module segment reloc)))
	     (push it fixups))))
       '(text data)))
    (file-position stream position)
    (sort fixups #'<=)))

(defun file-offset-of-address (address)
  (+ address *current-header-length*))


;;; XXX should use more gensyms in case other things want to be called
;;; exactly once.
(defmacro patch-stream ((var length
			 stream &optional (position (file-position stream)))
			&body body)
  (let ((pos-holder (gensym)))
    `(let ((,pos-holder ,position))
      (file-position ,stream ,pos-holder)
      (let ((,var (read-big-endian-data ,stream ,length)))
	,@body
	(file-position ,stream ,pos-holder)
	(write-big-endian-data ,stream ,var ,length)))))


(defun relocate-absolute (stream module segment reloc)
  (declare (ignore segment))
  (let ((base (module-segment-base module (relocation-symbol reloc)))
	(length (relocation-length reloc)))
    (patch-stream (value length stream (file-offset-of-address
					(relocation-address reloc)))
      (incf value base))
    ;; adjust address according to length
    (when (/= length 32)
      (format t "~&short fixup ~A ~A" length (relocation-address reloc))
      (decf (relocation-address reloc) (ceiling (- 32 length) 8))))
  ;; return fixup
  (when (oddp (relocation-address reloc))
    (format t "~&probably emitting a bad fixup: ~A ~A ~A"
	    (relocation-symbol reloc)
	    (module-name module)
	    (relocation-address reloc))
    (incf (relocation-address reloc)))
  (relocation-address reloc))

(defun relocate-abs-extern (stream module segment reloc)
  (declare (ignore segment module))
  (let ((symbol (find-first-non-extern-instance (relocation-symbol reloc)))
	(length (relocation-length reloc)))
    (unless symbol
      (error "~A is an undefined symbol referenced in ~A."
	     (linker-symbol-name (relocation-symbol reloc))
	     (module-name (linker-symbol-module (relocation-symbol reloc)))))
    (patch-stream (value length stream (file-offset-of-address
					(relocation-address reloc)))
      (setf value (+ (linker-symbol-value symbol)
		     (module-segment-base (linker-symbol-module symbol)
					  (linker-symbol-type symbol)))))
    ;; adjust address according to length
    (when (/= length 32)
      (decf (relocation-address reloc) (ceiling (- 32 length) 8))))
  (relocation-address reloc))


(defun find-first-non-extern-instance (symbol)
  "Returns the first non-external instance of SYMBOL in the global
symbol table, or NIL."
  (let ((sym-list (gethash (linker-symbol-name symbol)
			   *global-symbols*)))
    (find-if (lambda (x) (not (eq (linker-symbol-type x) 'extern)))
	     sym-list)))

(defun relocate-pcrel-extern (stream module segment reloc)
  (declare (ignore segment module))
  (let ((symbol (find-first-non-extern-instance (relocation-symbol reloc)))
	(length (relocation-length reloc)))
    (patch-stream (value length stream (file-offset-of-address
					(relocation-address reloc)))
      (setf value (- (+ (linker-symbol-value symbol)
			(module-segment-base (linker-symbol-module symbol)
					     (linker-symbol-type symbol)))
		     (relocation-address reloc)))
      (when (oddp value)
	(error "This should have been fixed.")
	(decf value))))			; fix 24-bit pc-rel problem.
  nil)					; no fixup.

(defun relocate-pc-relative (stream module segment reloc)
  (declare (ignore segment module stream reloc))
  (error "~&pcrel: wish i could say this was being handled.")
  nil)
