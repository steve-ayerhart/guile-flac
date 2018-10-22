(define-module (flac format)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)

  #:export (flac-max-metadata-type-code
            flac-min-block-size
            flac-max-block-size
            flac-subset-max-block-size
            flac-max-channels
            flac-min-bits-per-sample
            flac-max-bits-per-sample
            flac-reference-codec-max-bits-per-sample
            flac-max-sample-rate
            flac-max-lpc-order
            flac-subset-max-lpc-order-4800hz
            flac-min-qlp-coeff-precision
            flac-max-qlp-coeff-precision
            flac-max-fixed-order
            flac-max-rice-partition-order
            flac-subset-max-rice-partition-order

            flac-vendor-string
            flac-stream-sync-string
            flac-stream-sync-length

            <flac-application>
            make-flac-application
            flac-application?
            application-id
            application-data

            <flac-stream-info>
            make-flac-stream-info
            flac-stream-info?
            stream-min-block
            stream-max-block
            stream-min-frame
            stream-max-frame
            stream-sample-rate
            stream-channels
            stream-total-samples
            stream-bits-per-sample
            stream-md5sum

            <flac-vorbis-comment>
            make-flac-vorbis-comment
            flac-vorbis-comment?
            vorbis-comment-vendor
            set-vorbis-comment-vendor!
            vorbis-comment-comments
            set-vorbis-comment-comments!

            <flac-seek-table>
            make-flac-seek-table
            flac-seek-table?
            seek-table-seek-points

            <flac-seek-point>
            make-flac-seek-point
            flac-seek-point?
            seek-point-sample-number
            seek-point-stream-offset
            seek-point-frame-samples))

(define flac-max-metadata-type-code 126)
(define flac-min-block-size 16)
(define flac-max-block-size 65535)
(define flac-subset-max-block-size 4608)
(define flac-max-channels 8)
(define flac-min-bits-per-sample 4)
(define flac-max-bits-per-sample 32)
(define flac-reference-codec-max-bits-per-sample 24)
(define flac-max-sample-rate 655350)
(define flac-max-lpc-order 32)
(define flac-subset-max-lpc-order-4800hz 12)
(define flac-min-qlp-coeff-precision 5)
(define flac-max-qlp-coeff-precision 15)
(define flac-max-fixed-order 4)
(define flac-max-rice-partition-order 15)
(define flac-subset-max-rice-partition-order 8)

(define flac-vendor-string "Guile FLAC")
(define flac-stream-sync-string #vu8(102 76 97 67)) ; "fLaC"
(define flac-stream-sync-length 4)

(define-record-type <flac-application>
  (make-flac-application id data)
  flac-application?
  (id flac-application-id)
  (data flac-application-data))

(set-record-type-printer! <flac-application>
                          (λ (flac-application port)
                            (format port "#<<flac-application id: ~a>>"
                                    (flac-application-id flac-application))))

(define-record-type <flac-stream-info>
  (make-flac-stream-info
   min-block max-block min-frame max-frame sample-rate channels total-samples bits-per-sample md5sum)
  flac-stream-info?
  (min-block stream-min-block)
  (max-block stream-max-block)
  (min-frame stream-min-frame)
  (max-frame stream-max-frame)
  (sample-rate stream-sample-rate)
  (channels stream-channels)
  (total-samples stream-total-samples)
  (bits-per-sample stream-bits-per-sample)
  (md5sum stream-md5sum))

(set-record-type-printer! <flac-stream-info>
                          (λ (vorbis-comment port)
                            (format port "#<<flac-stream-info>>")))

(define-record-type <flac-vorbis-comment>
  (make-flac-vorbis-comment vendor comments)
  flac-vorbis-comment?
  (vendor vorbis-comment-vendor set-vorbis-comment-vendor!)
  (comments vorbis-comment-comments set-vorbis-comment-comments!))

(set-record-type-printer! <flac-vorbis-comment>
                          (λ (vorbis-comment port)
                            (format port "#<<flac-vorbis-comment> comments: ~a>"
                                    (length (vorbis-comment-comments vorbis-comment)))))


(define-record-type <flac-seek-table>
  (make-flac-seek-table seek-points)
  flac-seek-table?
  (seek-points seek-table-seek-points))

(set-record-type-printer! <flac-seek-table>
                          (λ (flac-seek-table port)
                            (format port "#<<flac-seek-table> seek-points: ~a>"
                                    (length (seek-table-seek-points flac-seek-table)))))

(define-record-type <flac-seek-point>
  (make-flac-seek-point sample-number stream-offset frame-samples)
  flac-seek-point?
  (sample-number seek-point-sample-number)
  (stream-offset seek-point-stream-offset)
  (frame-samples seek-point-frame-samples))
