;;;; Map each chord difference to an integer that can bet digested by PS4 matchers

(define make-chord-difference-table make-equal-hash-table)

(define (chord-difference->int table chord-difference scheme)
  (let ((val (get-chord-difference->int table chord-difference scheme)))
    (if val
	val
	(add-chord-difference->int table chord-difference scheme))))

(define (add-chord-difference->int table chord-difference scheme)
  (cond ((eq? scheme 'exact-matching)
	 (exact-matching table chord-difference))
	((eq? scheme 'exact-pitch-matching)
	 (exact-pitch-matching table chord-difference))))

(define (get-chord-difference->int table chord-difference scheme)
  (cond ((eq? scheme 'exact-matching)
	 (hash-table/get table chord-difference #f))
	((eq? scheme 'exact-pitch-matching)
	 (let ((delta (chord-difference-pitch-delta chord-difference)))
	   (if (> (abs delta) 500)
	       (hash-table/get table 1000 #f)
	       (hash-table/get table delta #f))))))

;; Exact Matching
(define (exact-matching table chord-difference)
  (let ((l (hash-table/count table)))
    (hash-table/put! table chord-difference l)
    l))

;; Exact Pitch Matching (different hash for different pitch delta only)
(define (exact-pitch-matching table chord-difference)
  (let ((l (hash-table/count table))
	(delta (chord-difference-pitch-delta chord-difference)))
    (if (> (abs delta) 500)
	(hash-table/put! table 1000 l)
	(hash-table/put! table delta l))
    l))

(define (chord-differences->ints chord-differences scheme)
  (let ((vec (make-vector (length chord-differences)))
	(table (make-chord-difference-table)))
    (let iter ((ind 0))
      (if (= ind (length chord-differences))
	  (vector->list vec)
	  (let ((chord-difference (list-ref chord-differences ind)))
	    (let ((val (chord-difference->int table chord-difference scheme)))
	      (vector-set! vec ind val)
	      (iter (+ ind 1))))))))

(define (difference-profile->ints difference-profile matching-scheme)
  (chord-differences->ints
   (difference-profile-chord-differences difference-profile)
   ;; SPECIFY MATCHING SCHEME HERE: 'exact-pitch-matching or 'exact-matching
   matching-scheme))
				     
