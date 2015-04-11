;;;; Data Structure to represent ABC files

;;; Pitch
(define (pitch basenote octaves accidentals)
  (list 'pitch basenote octaves accidentals))

(define (pitch? arg) (eq? (car arg) 'pitch))

(define (pitch-basenote pitch) (cadr pitch))

(define (pitch-octaves pitch) (caddr pitch))

(define (pitch-accidentals pitch) (cadddr pitch))

;;; Note
(define (note pitch length)
  (list 'note pitch length))

(define (note? arg) (eq? (car arg) 'note))

(define (note-pitch note) (cadr note))

(define (note-length note) (caddr note))

;;; Chord
(define (chord . notes)
  (cons 'chord notes))

(define (chord? arg) (eq? (car arg) 'chord))

(define (chord-notes chord) (cdr chord))

;;; Song
(define (song . chords)
  (cons 'song chords))

(define (song? arg) (eq? (car arg) 'song))

(define (song-chords song) (cdr song))

;;; Piece
(define (piece metadata song)
  (list 'piece metadata song))

(define (piece? arg) (eq? (car arg) 'piece))

(define (piece-metadata piece)
  (cadr piece))

(define (piece-song piece)
  (caddr piece))

;;; Piece Metadata
(define make-piece-metadata-table make-key-weak-eq-hash-table)

(define (add-piece-metadata-field! table field value)
  (hash-table/put! table field value))

(define (get-piece-metadata-field table field)
  (hash-table/get table field #f))
