;;;
;;; Raw binary output.
;;;

(in-package :st-linker)

(defun link-raw-bin (modules segment-sizes out-name entry-point)
  (with-open-file (prg-stream out-name :direction :io
			      :element-type 'unsigned-byte
			      :if-exists :new-version 
			      :if-does-not-exist :create)
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

    (let* ((*current-header-length* 0)
	   (fixups (process-link-time-relocations prg-stream modules)))
      (process-own-fixups prg-stream fixups entry-point))))


(defun process-own-fixups (stream fixups entry)
  (dolist (addr fixups)
    (patch-stream (value 32 stream (file-offset-of-address addr))
      (format t "~&fixup ~A => ~A -> ~A" addr value (+ value entry))
      (incf value entry))))
