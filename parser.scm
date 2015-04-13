;; this file contains the logic for converting .abc files to our scheme music language

(load-option 'regular-expression)
(define header-regex "\\(C\\|K\\|L\\|M\\|Q\\|T\\|X\\|V\\):.*")
;(define SHARP "\\^")
;;(define DOUBLESHARP "\\^\\^")
;;(define FLAT "\\_")
;;(define DOUBLEFLAT "\\_\\_")
;;(define NATURAL "\\=")
;;(define DIGIT "\\[0\\-9\\]")
;;(define DURATION "DIGIT\\*\\/\\?DIGIT\\*")
;;(define REST "zDURATION")
(define accidental-regex "\\(\\^\\|\\_\\|\\=\\|\\^\\^\\|\\_\\_\\)")
;;(define HIGHBASENOTE "\\[a-g]
;;(define LOWBASENOTE "\\[A-G]
(define hilonote-regex "\\([a-g]\\|[A-G]\\|z\\)")
;(define hilonote-regex "\\(A\\|B\\|C\\|D\\|E\\|F\\|G\\|a\\|b\\|c\\|d\\|e\\|f\\|g\\)")

(define hilooctave-regex "\\(\\,*\\|\\'*\\)")
(define duration-regex "[0-9]*/?[0-9]*")
;;(define DOWNOCTAVE "\\,*
;;(define UPOCTAVE "\\'*
;(define NOTE "\\(ACCIDENTAL\\?\\(((HIGHBASENOTE)(UPOCTAVE))|((LOWBASENOTE )(DOWNOCTAVE))")
;;)DURATION)
;;(define CHORD "\\(\[(NOTE|REST)\])
;(define NOTEELEMENT "\\(NOTE|REST|CHORD)
;(define TUPLET2 "\\(\(2(NOTEELEMENT)(NOTEELEMENT))
;(define TUPLET3 "\\(\(3(NOTEELEMENT)(NOTEELEMENT)(NOTEELEMENT))
;(define TUPLET4 "\\(\(4(NOTEELEMENT)(NOTEELEMENT)(NOTEELEMENT)(NOTEELEMENT))
;TUPLET =(TUPLET2|TUPLET3|TUPLET4)
;(define REPEAT "\\(:\|)|(\|:)|(\[(1|2))
;(define ENDMEASURE "\\(\|\])|(\|{1,2})|(\[\|)
;(define MUSIC "\\(NOTEELEMENT)|(REPEAT)|(ENDMEASURE)|(TUPLET)


;; converts a .abc file to our scheme music language
(define (abc-file->scheme filename)
  (abc->scheme (read-file filename)))

;; read a text file line by line
(define (read-file filename)
  (letrec* ((port (open-input-file filename))
           (file-as-list (port->string-list port '())))
    (close-input-port port)
    file-as-list))

;; given an input file port, convert it to a list of lines
(define (port->string-list port accumulator)
  (let ((line (read-line port)))
    (if (eof-object? line)
       accumulator
       (port->string-list port (append accumulator (list line))))))

;; converts list of abc lines to our scheme representation
(define (abc->scheme file-as-list)
  (let ((metadata-table (make-piece-metadata-table))
        (header-lines (filter is-line-abc-header? file-as-list))
        (song-lines (remove is-line-abc-header? file-as-list)))
    (map (lambda (header-line) (abc-header-line->scheme metadata-table header-line)) header-lines)
    (display (map (lambda (song-line) (abc-song-line->scheme song-line '())) song-lines))))

;; from stack overflow
(define (flatten list)
      (cond ((null? list) '())
            ((list? (car list)) (append (flatten (car list)) (flatten (cdr list))))
            (else (cons (car list) (flatten (cdr list))))))

(define (abc-header-line->scheme metadata-table header-line)
  (letrec* ((header-break-loc (string-search-forward ": " header-line))
           (header-type (string-head header-line header-break-loc))
           (header-content (string-tail header-line (+ 2 header-break-loc))))
    (display header-type)
    (display ": ")
    (display header-content)
    (display "\n")
    (add-piece-metadata-field! metadata-table (string->symbol header-type) header-content)))

(define (abc-song-line->scheme song-line chord-list)
  (letrec* ((chord-break-loc (string-search-forward " " song-line)))
    (if (number? chord-break-loc)
      (let ((chord-string (string-head song-line chord-break-loc))
           (line-remainder (string-tail song-line (+ 1 chord-break-loc))))
        (abc-song-line->scheme line-remainder (append chord-list (list (abc-chord->scheme chord-string '())))))
      chord-list)))

;; converts an abc chord to a scheme chord
(define (abc-chord->scheme chord-string note-list)
  (string-replace! chord-string #\[ #\ )
  (string-replace! chord-string #\] #\ )
  (string-replace! chord-string #\| #\ )

  (letrec* ((trimmed (string-trim chord-string))
            (note-tuple (abc-notes->scheme trimmed))
            (chord-remainder (cdr note-tuple)))
  (newline)(display chord-remainder)(newline)
  (if (eq? 0 (string-length chord-remainder))
    note-list
    (abc-chord->scheme chord-remainder 
      (append note-list (list (car note-tuple)))))))

  ;(display "\n")
  ;(display (string-trim chord-string))
  ;(display "\n")))

(define (return-end-match-index regex string)
  (if (re-string-match regex string)
    (re-match-end-index 0 (re-string-match regex string))
    #f))

(define (split-string-at-index string index offset)
 (let ((head (string-head string index))
       (tail (string-tail string (+ offset index))))
       (cons head tail)
))

(define (parse-optional-param regex string)
  (let ((index (return-end-match-index regex string)))
  ;(display "index is:") (display index) (display string)
  (if (number? index)
    (split-string-at-index string index 0)
    (cons "" string))
))

(define (abc-parse-accidental string)
  (parse-optional-param accidental-regex string))

(define (abc-parse-hilonote string)
  ;(newline)(display string)
  (parse-optional-param hilonote-regex string))


(define (abc-parse-hilooctave string)
  ;(newline)(display string)
  (parse-optional-param hilooctave-regex string))

(define (abc-parse-duration string)
  (parse-optional-param duration-regex string))

;; returns list of (note, remainder); 
;; the remainder is the rest of the string for further parsing
(define (abc-notes->scheme string)
  (letrec* ((accidentals (abc-parse-accidental string))
            (octave (abc-parse-hilooctave (cdr accidentals)))
            (parsed-note (abc-parse-hilonote (cdr octave)))
            (duration (abc-parse-duration (cdr parsed-note)))
            (the-note (note (pitch 
                              (string->symbol (car parsed-note))
                              (car octave)  ;; TODO octave to number
                              (car accidentals)) ;; TODO accidental to number
                         (car duration)))) ;; convert to numeric
  (display "making note")
  (newline) (display "note:\t\t") (display (car parsed-note)) 
  (newline) (display "octave:\t\t") (display (car octave))
  (newline) (display "accidental:\t") (display (car accidentals))
  (newline) (display "duration:\t") (display (car duration))
  (cons the-note (cdr duration))
))



;; check if the line is part of the header
(define (is-line-abc-header? line)
  (re-string-match header-regex line))

(abc-file->scheme "piece2.abc")







