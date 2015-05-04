;;;; Convert Scheme Data Structure to ABC file

#|
The converter doesn't take into account into triplets and etc
|#

(define (scm->abc piece filename)
  (call-with-output-file filename
    (lambda (port)
      (write-abc-headers piece port)
      (write-abc-song piece port))))

(define (pitch->abc pitch)
  (string-append
   (let accs ((steps (pitch-accidentals pitch))
	      (str ""))
     (cond ((< steps 0)
	    (accs (+ steps 1) (string-append str "_")))
	   ((> steps 0)
	    (accs (- steps 1) (string-append str "^")))
	   (else
	    str)))
   (let ps ((octaves (pitch-octaves pitch))
	    (str (string-capitalize (symbol->string (pitch-basenote pitch)))))
     (cond ((= octaves 1)
	    (ps (- octaves 1) (string-downcase str)))
	   ((< octaves 0)
	    (ps (+ octaves 1) (string-append str ",")))
	   ((> octaves 1)
	    (ps (- octaves 1) (string-append str "'")))
	   (else
	    str)))))

(define (note->abc note)
  (let ((pitch (note-pitch note))
	(length (note-length note)))
    (string-append
     (if pitch
	 (pitch->abc pitch)
	 "z")
     (if (and (= 1 (numerator length))
	      (= 1 (denominator length)))
	 ""
	 (string-append (number->string (numerator length))
			"/"
			(number->string (denominator length)))))))

(define (chord->abc chord)
  (let ((notes (chord-notes chord)))
    (if (= 1 (length notes))
	(note->abc (car notes))
	(string-append "["
		       (reduce-left string-append ""
				    (map note->abc notes))
		       "]"))))

(define (write-abc-song piece port)
  (define (write obj)
    (display obj port))
  (let ((chords (song-chords (piece-song piece))))
    (for-each (lambda (chord)
		(write (chord->abc chord)))
	      chords)))
    
(define (write-abc-headers piece port)
  (define (write obj)
    (display obj port))
  (let ((metadata (piece-metadata piece)))
    (let ((keys (get-piece-metadata-fields metadata)))
      (write "X: ")
      (write (get-piece-metadata-field metadata 'X))
      (write "\n")
      (write "T: ")
      (write (get-piece-metadata-field metadata 'T))
      (write "\n")
      (for-each (lambda (key)
		  (cond ((not (memq key '(X T K)))
			 (write (string-capitalize (symbol->string key)))
			 (write ": ")
			 (write (get-piece-metadata-field metadata key))
			 (write "\n"))))
		keys)
      (write "K: ")
      (write (pitch->abc (get-piece-metadata-field metadata 'K)))
      (write "\n"))))
