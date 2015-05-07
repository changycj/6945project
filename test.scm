;;;; Testing

(define piece-file "tests/barbie/barbie.abc")

(define piece-scm (abc-file->scheme piece-file))
(pp piece-scm)

(define piece-pattern (find-pattern piece-scm))

(define piece-phrase (pattern->scm piece-pattern piece-scm))

(scm->abc piece-phrase "tests/barbie/barbie-out")
