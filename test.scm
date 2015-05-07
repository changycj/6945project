;;;; Testing

;; this is THE function
;; arguments:
;;  in-file-name: the name of an ABC file
;;  out-file-name: the name of a file to write the chorus to
;;  k: minimum number of chords to look for in chorus
(define (find-chorus in-file-name out-file-name k)
  (letrec* ( (piece-scm (abc-file->scheme in-file-name))
             (piece-pattern (find-pattern piece-scm k))
             (piece-chorus (pattern->scm piece-pattern piece-scm)) )
    (newline)
    (pp "converting abc file to scheme music...")
    (pp piece-scm)(newline)

    (pp "calculating difference profile...")
    (pp "finding most repetitive pattern in difference profile...")
    (pp piece-pattern)(newline)

    (pp "converting pattern into scheme music....")
    (pp piece-chorus)(newline)

    (pp "translating scheme music back into ABC...")
  	(scm->abc piece-chorus out-file-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; piece2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; occurs 2 times
(find-chorus "tests/piece2/piece2.abc" "tests/piece2/piece2-out.abc" 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Barbie Girl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; finds 9 chords instead of 5 because the string of 9 chords are repeated just as often as 5
;; occurs 10 times
(find-chorus "tests/barbie/barbie.abc" "tests/barbie/barbie5-out.abc" 5)

(find-chorus "tests/barbie/barbie.abc" "tests/barbie/barbie9-out.abc" 9)

;; finds 14 chords instead of 10 because the string of 10 chords are repeated just as often as 10
;; occurs 8 times
(find-chorus "tests/barbie/barbie.abc" "tests/barbie/barbie10-out.abc" 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;