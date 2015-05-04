;;;; Matcher


(define (find-pattern difference-profile)
  (let ((ints (difference-profile->ints difference-profile)))
    (pp ints)
    ;; Dummy matcher, takes first 10 chord differences as "pattern"
    (list-head difference-profile 10)))
