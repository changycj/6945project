;;;; Convert Piece No. 2 abc file into Scheme data structure

(load "music.scm")

(define t (make-piece-metadata-table))

;;; Add metadata

(add-piece-metadata-field! t 'X 1)
(add-piece-metadata-field! t 'T "Piece No. 2")
(add-piece-metadata-field! t 'M "4/4")
(add-piece-metadata-field! t 'L "1/4")
(add-piece-metadata-field! t 'Q "1/4=200")
(add-piece-metadata-field! t 'K (pitch 'c 0 0))

(define s
  (song
   ;; [^F/2e/2]
   (chord (note (pitch 'f 0 1) (/ 1 2))
	  (note (pitch 'e 1 0) (/ 1 2)))
   ;; [F/2e/2]
   (chord (note (pitch 'f 0 1) (/ 1 2))
	  (note (pitch 'e 1 0) (/ 1 2)))
   ;; z/2
   (chord (note #f (/ 1 2)))
   (chord (note (pitch 'f 0 1) (/ 1 2))
	  (note (pitch 'e 1 0) (/ 1 2)))
   (chord (note #f (/ 1 2)))
   (chord (note (pitch 'f 0 1) (/ 1 2))
	  (note (pitch 'c 1 0) (/ 1 2)))
   (chord (note (pitch 'f 0 1) 1)
	  (note (pitch 'e 1 0) 1))
   (chord (note (pitch 'g 0 0) 1)
	  (note (pitch 'b 0 0) 1)
	  (note (pitch 'g 1 0) 1))
   (chord (note #f 1))
   (chord (note (pitch 'g 0 0) 1))
   (chord (note #f 1))
   (chord (note (pitch 'c 1 0) (/ 3 2)))
   (chord (note (pitch 'g 0 0) (/ 1 2)))
   (chord (note #f 1))
   (chord (note (pitch 'e 0 0) 1))
   (chord (note (pitch 'e 0 0) (/ 1 2)))
   (chord (note (pitch 'a 0 0) 1))
   (chord (note (pitch 'b 0 0) 1))
   (chord (note (pitch 'b 0 -1) (/ 1 2)))
   (chord (note (pitch 'a 0 0) 1))
   (chord (note (pitch 'g 0 0) (/ 1 3))
	  (note (pitch 'e 1 0) (/ 1 3))
	  (note (pitch 'g 1 0) (/ 1 3)))
   (chord (note (pitch 'a 1 0) 1))
   (chord (note (pitch 'f 1 0) (/ 1 2)))
   (chord (note (pitch 'g 1 0) (/ 1 2)))
   (chord (note #f (/ 1 2)))
   (chord (note (pitch 'e 1 0) 1))
   (chord (note (pitch 'c 1 0) (/ 1 2)))
   (chord (note (pitch 'd 1 0) (/ 1 2)))
   (chord (note (pitch 'b 0 0) (/ 3 2)))))

(define piece2 (piece t s))