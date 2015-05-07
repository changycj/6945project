;;;; Testing

(define piece-file "tests/piece2/piece2.abc")

(define piece-scm (abc-file->scheme piece-file))

(define piece-pattern (find-pattern piece-scm))

(define piece-phrase (pattern->scm piece-pattern piece-scm))

(scm->abc piece-phrase "tests/piece2/piece2-phrase.abc")
