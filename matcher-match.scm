;;;; Matcher

;; returns tuple of (index, value) for the max element in L
(define (argmax-with-max L)
  (letrec* ((with-index (map (lambda (i) (list i (list-ref L i))) (iota (length L))))
            (sorted (sort with-index (lambda (x y) (> (cadr x) (cadr y))))))
      (car sorted)))

(define (find-most-common-substring-of-length L sublist-length)
;; (display "length")(display (iota (- (length L) sublist-length) 1))(display L)
  (letrec* ((frequencies (make-equal-hash-table))
            (valid-offsets (iota (+ 1 (- (length L) sublist-length)) 0))
            (sublists (map 
              (lambda (i) (sublist L i (+ i sublist-length))) valid-offsets)))
    ;(display "sublists (sublist-length=")(display sublist-length)(display sublists) (newline)
    (for-each (lambda (sublist) (hash-table/put! frequencies sublist 
                            (+ 1 (hash-table/get frequencies sublist 0)))) sublists)
    (letrec* ((results (hash-table->alist frequencies))
          (substring-keys (map car results))
          (substring-freqs (map cdr results))
          (argmax (argmax-with-max substring-freqs))
          (best-i (car argmax))
          (best-freq (cadr argmax)))
      ;(display results)(newline)
      ;(display argmax) (newline)
      (list (list-ref substring-keys best-i) best-freq)
  )))


;; returns tuple of (sublist, index)
(define (find-longest-most-common-substring L min-length max-length)
  ;; best-substrings is list of (substring, frequency)
  (letrec* ((best-substrings (map (lambda (i) (find-most-common-substring-of-length L i)) 
                                          (iota (- (+ 1 (min (length L) max-length)) min-length) min-length)))
            (best-overall (sort best-substrings
                ;; sort by frequency and then by substring length
                (lambda (x y) (if (= (cadr x) (cadr y))
                                (> (length (car x)) (length (car y)))
                                (> (cadr x) (cadr y)))))))
    ;(display "best overall")(display (car best-overall))(newline)
    (car best-overall)))


(define (find-pattern piece)
  (pp "Find patterm")
  (let ((difference-profile (scm->difference-profile piece)))
    (pp "difference-profile")
    (pp difference-profile)
    (let ((ints (difference-profile->ints difference-profile)))
      (pp ints)
      ;; find the most commonly occuring pattern in the music
      ;; with a minimum length 1 and a maximum length N (length of music)
      (let ( (sublist-and-freq (find-longest-most-common-substring ints 3 (length ints))) )
        (list 
          (let ( (sublist-occurences (find-sublist-indices ints (car sublist-and-freq))) )
            (if (= 0 (length sublist-occurences))
                (error "sublist not found in list")
                (- (car sublist-occurences) 1))) ;; index of the first occurence of the sublist
          (length (car sublist-and-freq)))))))

;; finds all the indices of a sublist if it is contained in a list L
;; returns an empty list if the sublist is not found in list L
(define (find-sublist-indices L sub-list)
  (let ((len (+ (length L) 1) ))
    (let loop ((L (member (car sub-list) L))
      (sub-list2 sub-list)
      (ans '()))
      (cond ((not L) (reverse ans))
        ((null? sub-list2)
          (loop (member (car sub-list) L)
            sub-list
            (cons (- len (length L) (length sub-list)) ans)))
        ((null? L) (reverse ans))
        ((= (car L) (car sub-list2))
          (loop (cdr L)
            (cdr sub-list2)
            ans))
        (else
          (loop (member (car sub-list) L)
            sub-list
            ans))))))

(define (pattern->scm pattern peace)
  (let ((start-ind (car pattern))
	(pattern-length (cadr pattern)))
    (piece (piece-metadata peace)
	   (cons 'song
		 (sublist (song-chords (piece-song peace))
			  start-ind
			  (+ start-ind pattern-length))))))
