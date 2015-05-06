;;;; Testing

(define piece-file "piece2.abc")

(define piece-scm (abc-file->scheme piece-file))

(define piece-pattern (find-pattern piece-scm))

(define piece-phrase (pattern->scm piece-pattern piece-scm))

(pp piece-phrase)
