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

            make-frame-header
            frame-header-strategy frame-header-blocksize frame-header-sample-rate
            frame-header-channel-assignment frame-header-bits-per-sample
            frame-header-frame/sample-number frame-header-crc

            %make-subframe
            subframe-header subframe-data

            %make-subframe-header
            subframe-header-subframe-type subframe-header-predictor-order subframe-header-wasted-bits

            %make-frame-footer
            frame-footer-crc

            %make-frame
            frame-header frame-subframes frame-footer

            %make-subframe-constant
            subframe-constant-value

            %make-subframe-verbatim
            subframe-verbatim-data

            %make-rice-partition
            partitioned-rice-order partitioned-rice-contents

            %make-entropy-coding-method
            entropy-coding-method-type entropy-coding-method-data

            %make-entropy-coding-method-partitioned-rice-contents
            entropy-coding-method-partitioned-rice-contents-parameters
            entropy-coding-method-partitioned-rice-contents-raw-bits
            entropy-coding-method-partitioned-rice-contents?
            entropy-codi

            %make-subframe-fixed
            subframe-fixed-entropy-coding-method
            subframe-fixed-predictor-order
            subframe-fixed-warmup
            subframe-fixed-residual

            make-metadata-block-header
            metadata-block-header-last?
            metadata-block-header-type
            metadata-block-header-length

            make-metadata-padding
make-metadata-stream-info metadata-stream-info?
            stream-info-min-block-size stream-info-max-block-size
            stream-info-min-frame-size stream-info-max-frame-size
            stream-info-sample-rate stream-info-channels
            stream-info-bits-per-sample stream-info-samples stream-info-md5

            make-metadata-vorbis-comment

            make-flac-metadata
            flac-metadata-stream-info set-flac-metadata-stream-info!
            flac-metadata-seek-table set-flac-metadata-seek-table!
            flac-metadata-vorbis-comment set-flac-metadata-vorbis-comment!
            flac-metadata-padding set-flac-metadata-padding!
            flac-metadata-pictures set-flac-metadata-pictures!
            flac-metadata-application set-flac-metadata-application!
            flac-metadata-cuesheet set-flac-metadata-cuesheet!


            make-metadata-picture
            flac-picture-type

            make-metadata-seek-point make-metadata-seek-table

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
  (subframe-type subframe-header-subframe-type)
  (predictor-order subframe-header-predictor-order)
  (wasted-bits subframe-header-wasted-bits))

(define-record-type <subframe>
  (%make-subframe header data)
  subframe?
  (header subframe-header)
  (data subframe-data))

(define-record-type <subframe-verbatim>
  (%make-subframe-verbatim value)
  subframe-verbatim?
  (value subframe-verbatim-value))

(define-record-type <subframe-constant>
  (%make-subframe-constant value)
  subframe-constant?
  (value subframe-constant-value))

(define-record-type <entropy-coding-method-partitioned-rice-contents>
  (%make-entropy-coding-method-partitioned-rice-contents parameters raw-bits capacity-by-order)
  entropy-coding-method-partitioned-rice-contents?
  (parameters entropy-coding-method-partitioned-rice-contents-parameters)
  (raw-bits entropy-coding-method-partitioned-rice-contents-raw-bits)
  (capacity-by-order entropy-coding-method-partitioned-rice-contents-capacity-by-order))

(define-record-type <rice-partition>
  (%make-rice-partition order contents)
  rice-partition?
  (order rice-partition-order)
  (contents rice-partition-contents))

(define-record-type <entropy-coding-method>
  (%make-entropy-coding-method type data)
  entropy-coding-method?
  (type entropy-coding-method-type)
  (data entropy-coding-method-data))

(define-record-type <subframe-fixed>
  (%make-subframe-fixed entropy-coding-method predictor-order warmup residual)
  subframe-fixed?
  (entropy-coding-method subframe-fixed-entropy-coding-method)
  (predictor-order subframe-fix-predictor-order)
  (warmup subframe-fixed-warmup)
  (residual subframe-fixed-residual))

(define-record-type <frame-header>
  (make-frame-header blocking-strategy blocksize sample-rate channel-assignment bits-per-sample frame/sample-number crc)
  frame-header?
  (blocking-strategy frame-header-blocking-strategy)
  (blocksize frame-header-blocksize)
  (sample-rate frame-header-sample-rate)
  (channel-assignment frame-header-channel-assignment)
  (bits-per-sample frame-header-bits-per-sample)
  (frame/sample-number frame-header-frame/sample-number)
  (crc frame-header-crc))

(define-record-type <frame-footer>
  (%make-frame-footer crc)
  frame-footer?
  (crc frame-footer-crc))

(define-record-type <frame>
  (%make-frame header subframes footer)
  frame?
  (header frame-header)
  (subframes frame-subframes)
  (footer frame-footer))

; metadata

(define flac-metadata-type
  (make-enumeration '(stream-info
                      padding
                      application
                      seek-table
                      vorbis-comment
                      cuesheet
                      picture
                      invalid)))

(define-record-type <metadata-block-header>
  (make-metadata-block-header last? type length)
  metadata-block-header?
  (last? metadata-block-header-last?)
  (type metadata-block-header-type)
  (length metadata-block-header-length))

(define-record-type <stream-info>
  (make-metadata-stream-info min-block-size max-block-size min-frame-size max-frame-size sample-rate channels bits-per-sample samples md5)
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

(set-record-type-printer!
 <stream-info>
 (λ (record port)
   (format port "#<stream-info>")))

(define-record-type <seek-table>
  (make-metadata-seek-table seek-points)
  metadata-seek-table?
  (seek-points seek-table-seek-points))

(define-record-type <seek-point>
  (make-metadata-seek-point sample-number offset total-samples)
  metadata-seek-point?
  (sample-number seek-point-sample-number)
  (offset seek-point-offset)
  (total-samples seek-point-total-samples))

(define-record-type <vorbis-comment>
  (make-metadata-vorbis-comment vendor comments)
  metadata-vorbis-comment?
  (vendor vorbis-comment-vendor)
  (comments vorbis-comment-comments))

(set-record-type-printer!
 <vorbis-comment>
 (λ (record port)
   (format port "#<<vorbis-comment> vendor: ~a comments: ~a>" (vorbis-comment-vendor record) (length (vorbis-comment-comments record)))))

(define-record-type <padding>
  (make-metadata-padding length)
  metadata-padding?
  (length padding-length))

(define-record-type <application>
  (make-metadata-application id data)
  metadata-application?
  (id application-id)
  (data application-data))

(define flac-cuesheet-track-type (make-enumeration '(audio non-audio)))

(define-record-type <cuesheet-track>
  (make-metadata-cuesheet-track offset number isrc type pre-emphasis indices)
  metadata-cuesheet-track?
  (offset cuesheet-track-offset)
  (number cuesheet-track-number)
  (isrc cuesheet-track-isrc)
  (type cuesheet-track-type)
  (pre-emphasis cuesheet-track-pre-emphasis)
  (indices cuesheet-track-indices))

(define-record-type <cuesheet-index>
  (make-metadata-cuesheet-index offset number)
  metadata-cuesheet-index?
  (offset cuesheet-index-offset)
  (number cuesheet-index-number))

(define-record-type <cuesheet>
  (make-metadata-cuesheet catalog-number lead-in cd? tracks)
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
  (make-metadata-picture type mime-type description width height depth colors data)
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

(define-record-type <flac-metadata>
  (make-flac-metadata stream-info padding application seek-table vorbis-comment cuesheet pictures)
  flac-metadata?
  (stream-info flac-metadata-stream-info set-flac-metadata-stream-info!)
  (padding flac-metadata-padding set-flac-metadata-padding!)
  (application flac-metadata-application set-flac-metadata-application!)
  (seek-table flac-metadata-seek-table set-flac-metadata-seek-table!)
  (vorbis-comment flac-metadata-vorbis-comment set-flac-metadata-vorbis-comment!)
  (cuesheet flac-metadata-cuesheet set-flac-metadata-cuesheet!)
  (pictures flac-metadata-pictures set-flac-metadata-pictures!))

(set-record-type-printer!
 <flac-metadata>
 (λ (record port)
   (format port "#<<flac-metadata>")
   (let ((getters '(flac-metadata-stream-info flac-metadata-vorbis-comment flac-metadata-application flac-metadata-cuesheet flac-metadata-pictures flac-metadata-seek-table flac-metadata-padding)))
     (for-each (λ (getter)
                 (when ((primitive-eval getter) record)
                   (regexp-substitute/global port "flac-metadata-" (symbol->string getter) 'pre " " 'post)))
               getters))
   (format port ">")))
