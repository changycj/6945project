;;;; Different Profile data structure

(define (chord-difference pitch-delta beat-delta)
  (list 'chord-difference pitch-delta beat-delta))

(define (difference-profile chord-differences)
  (cons 'difference-profile chord-differences))

(define (difference-profile-chord-differences difference-profile)
  (cdr difference-profile))

(define (basenote->int basenote)
  (cond ((eq? basenote 'c) 0)
	((eq? basenote 'd) 2)
	((eq? basenote 'e) 4)
	((eq? basenote 'f) 5)
	((eq? basenote 'g) 7)
	((eq? basenote 'a) 9)
	((eq? basenote 'b) 11)))

(define (pitch->int pitch)
  (if (not pitch)
      1000
      (+ (basenote->int (pitch-basenote pitch))
	 (* 12 (pitch-octaves pitch))
	 (pitch-accidentals pitch))))

(define (note-from-chord chord)
  (let ((notes (chord-notes chord)))
    (if (= 1 (length notes))
	(car notes)
	(let ((pitch-ints (map pitch->int
			    (map note-pitch notes))))
	  (let ((max-pitch-int (apply max pitch-ints)))
	    (car (filter (lambda (n)
			   (= (pitch->int (note-pitch n))
			      max-pitch-int))
			 notes)))))))

(define (compute-beat-delta beat1 beat2)
  (- beat2 beat1))

(define (compute-pitch-delta pitch1 pitch2)
  (- (pitch->int pitch2)
     (pitch->int pitch1)))

(define (compute-chord-difference chord1 chord2)
  (let ((note1 (note-from-chord chord1))
	(note2 (note-from-chord chord2)))
    (chord-difference
     (compute-pitch-delta (note-pitch note1)
			  (note-pitch note2))
     (compute-beat-delta (note-length note1)
			 (note-length note2)))))

(define (scm->difference-profile piece)
  (let ((chords (song-chords (piece-song piece))))
    (let iter ((ind 0)
           (vec (make-vector (- (length chords) 1))))
      (cond ((= ind (vector-length vec))
         (difference-profile (vector->list vec)))
        (else
         (let ((chord1 (list-ref chords ind))
           (chord2 (list-ref chords (+ ind 1))))
           (vector-set! vec ind
                (compute-chord-difference chord1 chord2))
           (iter (+ ind 1) vec)))))))
		 
