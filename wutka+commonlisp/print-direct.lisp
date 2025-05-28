(ql:quickload :pngload)

(defun process-byte (ctx byte)
  (let
      ((curr-byte (car ctx))
       (bit-num (cdr ctx)))
      ; add another bit to the current byte
      (let ((new-byte (+ (ash curr-byte 1) (logand byte 1))))
	(cond
         ; if we just did bit 7, add it to the output if it isn't 0
	 ((= bit-num 7)
	  (when (> new-byte 0) (write-char
				(code-char new-byte)))
	  ; And reset the curr-byte and bit-num
	  (cons 0 0))
	; Otherwise, return the current out-bytes, new-byte and
	; increment the bit num
	 (t (cons new-byte (+ bit-num 1)))))))

(defun decode-file (filename)
  ; load the file
  (let* ((pixels (pngload:load-file filename :FLATTEN t))
	 ; create the initial context with an empty vector
	 (initial-ctx (cons 0 0)))
    (reduce #'process-byte (pngload:data pixels)
	    :initial-value initial-ctx)))

(decode-file #p"../hamlet_encoded.png")
(quit)

