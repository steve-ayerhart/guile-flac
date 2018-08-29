(define-module (flac stream)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports))

(define-class <stream> ()
  (info #:class <protected-slot> #:getter info #:init-value '())
  (metadata #:getter metadata #:init-value '())
  (frames #:getter frames #:init-value '()))

(define-class <file-stream> (<stream>)
  (port #:class <protected-slot>))

(define FLAC-MARKER-LENGTH 4)
(define FLAC-MARKER #vu8(102 76 97 67)) ; fLaC

(define (read-stream port)
  (let ((magic? (get-bytevector-n port 4)))
    (let read-metadata-block ((metadata '()))
      (receive (last? type length)
          (read-header port)
        (if last?
            metadata
            (read-metadata-block
             (cons (parse-metadata-block type (get-bytevector-n port length))
                   metadata)))))))

(define (read-header port)
  (parse-header (get-bytevector-n port 4)))

(define (parse-metadata-block type data)
  (eval `(,(string->symbol (string-append "parse-" (symbol->string type))) ,data) (current-module)))

(define (parse-header bv)
  (let ((header (bytevector-u32-ref bv 0 'big)))
    (values
     (= 1 (bit-extract header 31 32))
     (match (bit-extract header 24 31)
       (0 'stream-info)
       (1 'padding)
       (2 'application)
       (3 'seek-table)
       (4 'vorbis-comment)
       (5 'cue-sheet)
       (6 'picture)
       (else 'invalid))
     (bit-extract header 0 24))))

(define (parse-invalid bv)
  'invalid)

(define (parse-padding bv)
  'padding)

(define (parse-application bv)
  ; we dont' do anything with application data yet
  (make <flac-application>
    #:id (utf8->string (bytevector-uint-ref bv 0 'big 4))))

(define (parse-seek-table bv)
  'seek-table)

(define (parse-vorbis-comment bv)
  'vorbis-comment)

(define (parse-cue-sheet bv)
  'cue-sheet)

(define (parse-picture bv)
  'picture)

(define (parse-stream-info bv)
  (let ((sample/channel-data (bytevector-uint-ref bv 10 'big 8)))
    (make <flac-stream-info>
      #:min-block (bytevector-uint-ref bv 0 'big 2)
      #:max-block (bytevector-uint-ref bv 2 'big 2)
      #:min-frame (bytevector-uint-ref bv 4 'big 3)
      #:max-frame (bytevector-uint-ref bv 7 'big 3)
      #:samples (bit-extract sample/channel-data 0 36)
      #:bps (+ 1 (bit-extract sample/channel-data 36 41))
      #:channels (+ 1 (bit-extract sample/channel-data 41 45))
      #:sample-rate (bit-extract sample/channel-data 45 65)
      #:md5 (number->string (bytevector-uint-ref bv 18 'big 16) 16))))

(define-class <flac-seek-table> ()
  ; TODO: make array?
  (seek-points #:accessor seek-points #:init-keyword #:seek-points))

(define-class <flac-seek-point> ()
  (sample-number #:accessor sample-number #:init-keyword #:sample-number)
  (stream-offset #:accessor stream-offset #:init-keyword #:stream-offset)
  (frame-samples #:accessor frame-samples #:init-keyword #:frame-samples))

(define-method (write (fst <flac-seek-table>) port)
  (format port "#<<flac-seek-table> ~a>" (length (seek-points fst))))

(define-method (display (fst <flac-seek-table>) port)
  (format port "#<<flac-seek-table> ~a>" (length (seek-points fst))))

(define (parse-seek-table bv)
  (let ((seek-table-length (bytevector-length bv)))
    (let parse-seek-point ((seek-points '())
                           (bytes-read 0))
      (if (= seek-table-length bytes-read)
          (make <flac-seek-table> #:seek-points seek-points)
          (parse-seek-point
           (append! seek-points
                    (list (make <flac-seek-point>
                            #:sample-number (bytevector-uint-ref bv bytes-read 'big 8)
                            #:stream-offset (bytevector-uint-ref bv (+ bytes-read 8) 'big 8)
                            #:frame-samples (bytevector-uint-ref bv (+ bytes-read 16) 'big 2))))
           (+ 18 bytes-read))))))

(define-class <flac-application> ()
  (id #:accessor id #:init-keyword #:id)
  (data #:accessor data #:init-keyword #:data))

(define-class <flac-stream-info> ()
  (min-block #:class <int-slot> #:getter min-block #:init-keyword #:min-block)
  (max-block #:class <int-slot> #:getter max-block #:init-keyword #:max-block)
  (min-frame #:class <int-slot> #:getter min-frame #:init-keyword #:min-frame)
  (max-frame #:class <int-slot> #:getter max-frame #:init-keyword #:max-frame)
  (sample-rate #:class <int-slot> #:getter sample-rate #:init-keyword #:sample-rate)
  (channels #:class <int-slot> #:getter channels #:init-keyword #:channels)
  (total-samples #:class <int-slot> #:getter total-samples #:init-keyword #:samples)
  (bits-per-sample #:class <int-slot> #:getter bits-per-sample #:init-keyword #:bps)
  (md5sum #:getter md5 #:init-keyword #:md5))


(define-method (write (fsi <flac-stream-info>) port)
  (format port "#<<flac-stream-info>>"))

(define-method (display (fsi <flac-stream-info>) port)
  (format port "#<<flac-stream-info>>"))

(define-class <vorbis-comment> ()
  (vendor #:getter vendor #:init-keyword #:vendor)
  (comments #:accessor comments #:init-keyword #:comments))

(define (parse-vorbis-comment bv)
  (let* ((comments-length (bytevector-length bv))
         (vendor-string-length (bytevector-uint-ref bv 0 'little 4))
         (num-comments (bytevector-uint-ref bv (+ 4 vendor-string-length) 'little 4)))
    (let parse-comment ((bytes-read (+ 8 vendor-string-length))
                        (comments '()))
      (if (= comments-length bytes-read)
          (let ((vendor-bytestring (make-bytevector vendor-string-length)))
            (make <vorbis-comment>
              #:vendor (utf8->string vendor-bytestring)
              #:comments comments))
          (let* ((comment-length (bytevector-uint-ref bv bytes-read 'little 4))
                 (comment-bytestring (make-bytevector comment-length)))
            (bytevector-copy! bv (+ 4 bytes-read) comment-bytestring 0 comment-length)
            (parse-comment (+ 4 comment-length bytes-read)
                           (cons (utf8->string comment-bytestring) comments)))))))
