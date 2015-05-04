;;;; Matcher

(define (find-pattern piece)
  (let ((difference-profile (scm->difference-profile piece)))
    (let ((ints (difference-profile->ints difference-profile)))
      (pp ints)
      ;; Dummy matcher, takes first 10 chords as pattern
      ;; starting at chord index 0, continue for length 10
      (list 0 10))))

(define (pattern->scm pattern peace)
  (let ((start-ind (car pattern))
	(pattern-length (cadr pattern)))
    (piece (piece-metadata peace)
	   (cons 'song
		 (sublist (song-chords (piece-song peace))
			  start-ind
			  (+ start-ind pattern-length))))))
