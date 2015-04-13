;; this file contains the logic for converting .abc files to our scheme music language

(load-option 'regular-expression)
(define header-regex "\\(C\\|K\\|L\\|M\\|Q\\|T\\|X\\|V\\):.*")

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
    (map (lambda (header-line) (parse-header-line metadata-table header-line)) header-lines)
    (display (flatten (map (lambda (song-line) (parse-song-line song-line '())) song-lines)))))

(define (flatten list)
      (cond ((null? list) '())
            ((list? (car list)) (append (flatten (car list)) (flatten (cdr list))))
            (else (cons (car list) (flatten (cdr list))))))

(define (parse-header-line metadata-table header-line)
  (letrec* ((header-break-loc (string-search-forward ": " header-line))
           (header-type (string-head header-line header-break-loc))
           (header-content (string-tail header-line (+ 2 header-break-loc))))
    (display header-type)
    (display " ")
    (display header-content)
    (display "\n")
    (add-piece-metadata-field! metadata-table (string->symbol header-type) header-content)))

(define (parse-song-line song-line chord-list)
  (letrec* ((chord-break-loc (string-search-forward " " song-line)))
    (if (number? chord-break-loc)
      (let ((chord-string (string-head song-line chord-break-loc))
           (line-remainder (string-tail song-line (+ 1 chord-break-loc))))
        (parse-song-line line-remainder (append chord-list (list chord-string))))
      chord-list)))

;; check if the line is part of the header
(define (is-line-abc-header? line)
  (re-string-match header-regex line))

(abc-file->scheme "piece2.abc")







