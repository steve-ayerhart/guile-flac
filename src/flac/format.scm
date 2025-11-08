(define-module (flac format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 regex)
  #:use-module (rnrs enums)
  #:export (FLAC-MAGIC
            enum-lookup

            flac-frame-blocking-strategy-type
            flac-frame-number-type
            flac-channel-assignment-type
            flac-subframe-type
            flac-entropy-coding-method-type

            %make-frame-header
            frame-header-blocking-strategy set-frame-header-blocking-strategy!
            frame-header-blocksize set-frame-header-blocksize!
            frame-header-sample-rate set-frame-header-sample-rate!
            frame-header-channel-assignment set-frame-header-channel-assignment!
            frame-header-bits-per-sample set-frame-header-bits-per-sample!
            frame-header-frame/sample-number set-frame-header-frame/sample-number!
            frame-header-crc set-frame-header-crc!

            %make-subframe-header
            subframe-header-subframe-type set-subframe-header-subframe-type!
            subframe-header-predictor-order set-subframe-header-predictor-order!
            subframe-header-wasted-bits set-subframe-header-wasted-bits!

            %make-frame-footer
            frame-footer-crc
            set-frame-footer-crc!

            %make-metadata-padding metadata-padding?
            padding-length

            %make-metadata-cuesheet metadata-cuesheet?
            %make-metadata-cuesheet-track
            %make-metadata-cuesheet-index

            %make-metadata-application metadata-application?

            %make-metadata-stream-info metadata-stream-info?
            stream-info-min-block-size stream-info-max-block-size
            stream-info-min-frame-size stream-info-max-frame-size
            stream-info-sample-rate stream-info-channels
            stream-info-bits-per-sample stream-info-samples stream-info-md5

            %make-metadata-vorbis-comment
            metadata-vorbis-comment?
            vorbis-comment-vendor
            vorbis-comment-comments

            %make-metadata-picture
            metadata-picture?
            flac-picture-type

            %make-metadata-seek-table
            metadata-seek-table?
            %make-metadata-seek-point

            flac-metadata-type flac-metadata-type-index))

(define FLAC-MAGIC #x664c6143) ; fLaC

(define (enum-lookup enum int)
  (list-ref (enum-set->list enum) int))

(define flac-frame-blocking-strategy-type
  (make-enumeration '(fixed variable)))

(define flac-frame-number-type
  (make-enumeration '(frame sample)))

(define flac-channel-assignment-type
  (make-enumeration '(independent left right mid)))

(define flac-subframe-type
  (make-enumeration '(constant verbatim fix lpc)))

(define flac-entropy-coding-method-type
  (make-enumeration '(rice rice2)))

(define-record-type <subframe-header>
  (%make-subframe-header subframe-type predictor-order wasted-bits)
  subframe-header?
  (subframe-type subframe-header-subframe-type set-subframe-header-subframe-type!)
  (predictor-order subframe-header-predictor-order set-subframe-header-predictor-order!)
  (wasted-bits subframe-header-wasted-bits set-subframe-header-wasted-bits!))

(define-record-type <subframe>
  (%make-subframe header data)
  subframe?
  (header subframe-header)
  (data subframe-data))

(define-record-type <frame-header>
  (%make-frame-header blocking-strategy blocksize sample-rate channel-assignment bits-per-sample frame/sample-number crc)
  frame-header?
  (blocking-strategy frame-header-blocking-strategy set-frame-header-blocking-strategy!)
  (blocksize frame-header-blocksize set-frame-header-blocksize!)
  (sample-rate frame-header-sample-rate set-frame-header-sample-rate!)
  (channel-assignment frame-header-channel-assignment set-frame-header-channel-assignment!)
  (bits-per-sample frame-header-bits-per-sample set-frame-header-bits-per-sample!)
  (frame/sample-number frame-header-frame/sample-number set-frame-header-frame/sample-number!)
  (crc frame-header-crc set-frame-header-crc!))

(define-record-type <frame-footer>
  (%make-frame-footer crc)
  frame-footer?
  (crc frame-footer-crc set-frame-footer-crc!))

(define flac-metadata-type
  (make-enumeration '(stream-info
                      padding
                      application
                      seek-table
                      vorbis-comment
                      cuesheet
                      picture
                      invalid)))

(define-record-type <stream-info>
  (%make-metadata-stream-info min-block-size max-block-size min-frame-size max-frame-size sample-rate channels bits-per-sample samples md5)
  metadata-stream-info?
  (min-block-size stream-info-min-block-size)
  (max-block-size stream-info-max-block-size)
  (min-frame-size stream-info-min-frame-size)
  (max-frame-size stream-info-max-frame-size)
  (sample-rate stream-info-sample-rate)
  (channels stream-info-channels)
  (bits-per-sample stream-info-bits-per-sample)
  (samples stream-info-samples)
  (md5 stream-info-md5))

                                        ;(set-record-type-printer!
                                        ; <stream-info>
                                        ; (λ (record port)
                                        ;   (format port "#<stream-info>")))

(define-record-type <seek-table>
  (%make-metadata-seek-table seek-points)
  metadata-seek-table?
  (seek-points seek-table-seek-points))

(set-record-type-printer!
 <seek-table>
 (λ (record port)
   (format port "#<<seek-table> seek-points: ~a>" (length (seek-table-seek-points record)))))

(define-record-type <seek-point>
  (%make-metadata-seek-point sample-number offset total-samples)
  metadata-seek-point?
  (sample-number seek-point-sample-number)
  (offset seek-point-offset)
  (total-samples seek-point-total-samples))

(define-record-type <vorbis-comment>
  (%make-metadata-vorbis-comment vendor comments)
  metadata-vorbis-comment?
  (vendor vorbis-comment-vendor)
  (comments vorbis-comment-comments))

(set-record-type-printer!
 <vorbis-comment>
 (λ (record port)
   (format port "#<<vorbis-comment> vendor: ~a comments: ~a>" (vorbis-comment-vendor record) (length (vorbis-comment-comments record)))))

(define-record-type <padding>
  (%make-metadata-padding length)
  metadata-padding?
  (length padding-length))

(define-record-type <application>
  (%make-metadata-application id data)
  metadata-application?
  (id application-id)
  (data application-data))

(define flac-cuesheet-track-type (make-enumeration '(audio non-audio)))

(define-record-type <cuesheet-track>
  (%make-metadata-cuesheet-track offset number isrc type pre-emphasis indices)
  metadata-cuesheet-track?
  (offset cuesheet-track-offset)
  (number cuesheet-track-number)
  (isrc cuesheet-track-isrc)
  (type cuesheet-track-type)
  (pre-emphasis cuesheet-track-pre-emphasis)
  (indices cuesheet-track-indices))

(define-record-type <cuesheet-index>
  (%make-metadata-cuesheet-index offset number)
  metadata-cuesheet-index?
  (offset cuesheet-index-offset)
  (number cuesheet-index-number))

(define-record-type <cuesheet>
  (%make-metadata-cuesheet catalog-number lead-in cd? tracks)
  metadata-cuesheet?
  (catalog-number cuesheet-catalog-number)
  (lead-in cuesheet-lead-in)
  (cd? cuesheet-cd?)
  (tracks cuesheet-tracks))

(define flac-picture-type
  (make-enumeration
   '(other
     file-icon
     other-file-icon
     front-cover
     back-cover
     leaflet-page
     media
     lead-artist/performer/soloist
     artist/performer
     conductor
     band/orchestra
     composer
     lyricist/text-writer
     recording-location
     during-recording
     during-performance
     movie/video-screen-capture
     bright-coloured-fish
     illustration
     band/artist-logotype
     publisher/studio-logotype)))

(define-record-type <picture>
  (%make-metadata-picture type mime-type description width height depth colors data)
  metadata-picture?
  (type picture-type)
  (mime-type picture-mime-type)
  (description picture-description)
  (width picture-width)
  (height picture-height)
  (depth picture-depth)
  (colors picture-colors)
  (data picture-data))

(set-record-type-printer!
 <picture>
 (λ (record port)
   (format port "#<<picture> type: ~a mime-type: ~a>" (picture-type record) (picture-mime-type record))))
