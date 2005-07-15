
(in-package :st-linker)

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
