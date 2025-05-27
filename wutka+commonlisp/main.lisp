(ql:quickload :pngload)

(defun process-byte (ctx byte)
  ; pluck out the 3 items we are using
  (destructuring-bind (out-bytes curr-byte bit-num) ctx
    ; add another bit to the current byte
    (let ((new-byte (+ (* curr-byte 2) (logand byte 1))))
      (cond
	; if we just did bit 7, add it to the output if it isn't 0
	((= bit-num 7)
	 (progn (when (> new-byte 0) (vector-push-extend
				      (code-char new-byte) out-bytes))
		; And reset the curr-byte and bit-num
		(list out-bytes 0 0)))
	; Otherwise, return the current out-bytes, new-byte and
	; increment the bit num
	(t (list out-bytes new-byte (+ bit-num 1)))))))

(defun decode-file (filename)
  ; load the file
  (let* ((pixels (pngload:load-file filename :FLATTEN t))
	 ; create the initial context with an empty vector
	 (initial-ctx (list (make-array 0 :fill-pointer 0 :adjustable t) 0 0))
	 ; decode the image
	 (decoded (car (reduce #'process-byte (pngload:data pixels)
			       :initial-value initial-ctx))))
    ; Convert the image to a string
    (map 'string #'identity decoded)))


(print (decode-file #p"../hamlet_encoded.png"))
(quit)
