;;;; Testing

(define piece-file "piece2.abc")
(display piece-file)

;; abc-file->scheme only returns a song--not a piece
(define piece-scm (abc-file->scheme piece-file))
;(display piece-scm)

(define piece-pattern (find-pattern piece-scm))
(display piece-pattern)

(define piece-phrase (pattern->scm piece-pattern piece-scm))

(pp piece-phrase)


;(define piece-name "piece2.abc")
;(define scm-piece (abc-file->scheme piece-name))
;(define piece2 (piece t s))
;(define piece2-pattern (find-pattern piece2))
;(define piece2-phrase (pattern->scm piece2-pattern piece2))
;(scm->abc piece2-phrase "piece2-phrase")