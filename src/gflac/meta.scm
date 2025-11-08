(define-module (gflac meta)
  #:use-module (flac format)
  #:use-module (flac metadata)
  #:use-module (flac version)
  #:use-module (ice-9 format)
  #:use-module (ice-9 getopt-long)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:export (meta-command))

(define meta-option-spec
  '((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))
    (show-md5sum (value #f))
    (show-min-blocksize (value #f))
    (show-max-blocksize (value #f))
    (show-min-framesize (value #f))
    (show-max-framesize (value #f))
    (show-sample-rate (value #f))
    (show-channels (value #f))
    (show-bps (value #f))
    (show-total-samples (value #f))
    (show-vendor-tag (value #f))
    (show-tag (value #t))
    (show-all-tags (value #f))
    (list (value #f))
    (with-filename (value #f))
    (no-filename (value #f))))

(define (show-help)
  (format #t "gflac meta - FLAC metadata operations

Usage:
  gflac meta [options] FLACfile [FLACfile ...]

Options:
  -h, --help                Show this help message
  -v, --version             Show version information

Shorthand operations:
  --show-md5sum             Show the MD5 signature from STREAMINFO
  --show-min-blocksize      Show the minimum block size from STREAMINFO
  --show-max-blocksize      Show the maximum block size from STREAMINFO
  --show-min-framesize      Show the minimum frame size from STREAMINFO
  --show-max-framesize      Show the maximum frame size from STREAMINFO
  --show-sample-rate        Show the sample rate from STREAMINFO
  --show-channels           Show the number of channels from STREAMINFO
  --show-bps                Show the bits per sample from STREAMINFO
  --show-total-samples      Show the total # of samples from STREAMINFO

  --show-vendor-tag         Show the vendor string from VORBIS_COMMENT
  --show-tag=NAME           Show all tags where field name matches 'NAME'
  --show-all-tags           Show all tags

Major operations:
  --list                    List the contents of metadata blocks
  --with-filename           Prefix each output line with the FLAC file name
  --no-filename             Do not prefix output lines with file name

Examples:
  gflac meta --list myfile.flac
  gflac meta --show-sample-rate --show-channels *.flac
  gflac meta --show-tag=ARTIST myfile.flac
"))

(define (format-md5 md5-bv)
  "Format a bytevector as a hex MD5 string"
  (string-join
   (map (lambda (byte) (format #f "~2,'0x" byte))
        (bytevector->u8-list md5-bv))
   ""))

(define (process-show-options options metadata filename)
  "Process all --show-* options for a single file"
  (let ((stream-info (find metadata-stream-info? metadata))
        (vorbis-comment (find metadata-vorbis-comment? metadata))
        (prefix (if (assoc-ref options 'with-filename)
                    (string-append filename ":")
                    "")))

    ;; STREAMINFO fields
    (when stream-info
      (when (assoc-ref options 'show-md5sum)
        (format #t "~a~a~%" prefix (format-md5 (stream-info-md5 stream-info))))
      (when (assoc-ref options 'show-min-blocksize)
        (format #t "~a~a~%" prefix (stream-info-min-block-size stream-info)))
      (when (assoc-ref options 'show-max-blocksize)
        (format #t "~a~a~%" prefix (stream-info-max-block-size stream-info)))
      (when (assoc-ref options 'show-min-framesize)
        (format #t "~a~a~%" prefix (stream-info-min-frame-size stream-info)))
      (when (assoc-ref options 'show-max-framesize)
        (format #t "~a~a~%" prefix (stream-info-max-frame-size stream-info)))
      (when (assoc-ref options 'show-sample-rate)
        (format #t "~a~a~%" prefix (stream-info-sample-rate stream-info)))
      (when (assoc-ref options 'show-channels)
        (format #t "~a~a~%" prefix (stream-info-channels stream-info)))
      (when (assoc-ref options 'show-bps)
        (format #t "~a~a~%" prefix (stream-info-bits-per-sample stream-info)))
      (when (assoc-ref options 'show-total-samples)
        (format #t "~a~a~%" prefix (stream-info-samples stream-info))))

    ;; VORBIS_COMMENT fields
    (when vorbis-comment
      (when (assoc-ref options 'show-vendor-tag)
        (format #t "~a~a~%" prefix (vorbis-comment-vendor vorbis-comment)))
      (when (assoc-ref options 'show-all-tags)
        (for-each (lambda (tag)
                    (format #t "~a~a~%" prefix (string-join tag "=")))
                  (vorbis-comment-comments vorbis-comment)))
      (let ((tag-name (assoc-ref options 'show-tag)))
        (when tag-name
          (for-each (lambda (tag)
                      (when (string-ci=? (car tag) tag-name)
                        (format #t "~a~a~%" prefix (string-join tag "="))))
                    (vorbis-comment-comments vorbis-comment)))))))

(define (list-metadata metadata filename options)
  "List all metadata blocks"
  (let ((prefix (if (assoc-ref options 'with-filename)
                    (string-append filename ":")
                    "")))
    (format #t "~aMETADATA blocks: ~a~%" prefix (length metadata))

    (let loop ((blocks metadata) (index 0))
      (unless (null? blocks)
        (let ((block (car blocks)))
          (cond
           ((metadata-stream-info? block)
            (format #t "~a  block ~a: STREAMINFO
~a    sample_rate: ~a Hz
~a    channels: ~a
~a    bits-per-sample: ~a
~a    total samples: ~a
~a    min blocksize: ~a
~a    max blocksize: ~a
~a    MD5 signature: ~a~%"
                    prefix index
                    prefix (stream-info-sample-rate block)
                    prefix (stream-info-channels block)
                    prefix (stream-info-bits-per-sample block)
                    prefix (stream-info-samples block)
                    prefix (stream-info-min-block-size block)
                    prefix (stream-info-max-block-size block)
                    prefix (format-md5 (stream-info-md5 block))))

           ((metadata-vorbis-comment? block)
            (format #t "~a  block ~a: VORBIS_COMMENT
~a    vendor: ~a
~a    comments: ~a~%"
                    prefix index
                    prefix (vorbis-comment-vendor block)
                    prefix (length (vorbis-comment-comments block))))

           ((metadata-padding? block)
            (format #t "~a  block ~a: PADDING
~a    length: ~a~%"
                    prefix index
                    prefix (padding-length block)))

           ((metadata-seek-table? block)
            (format #t "~a  block ~a: SEEKTABLE~%" prefix index))

           ((metadata-picture? block)
            (format #t "~a  block ~a: PICTURE
~a    type: ~a
~a    MIME type: ~a~%"
                    prefix index
                    prefix (picture-type block)
                    prefix (picture-mime-type block)))

           ((metadata-cuesheet? block)
            (format #t "~a  block ~a: CUESHEET~%" prefix index))

           ((metadata-application? block)
            (format #t "~a  block ~a: APPLICATION~%" prefix index)))

          (loop (cdr blocks) (+ index 1)))))))

(define (options->alist options)
  "Convert getopt-long options to alist"
  `((with-filename . ,(option-ref options 'with-filename #f))
    (show-md5sum . ,(option-ref options 'show-md5sum #f))
    (show-min-blocksize . ,(option-ref options 'show-min-blocksize #f))
    (show-max-blocksize . ,(option-ref options 'show-max-blocksize #f))
    (show-min-framesize . ,(option-ref options 'show-min-framesize #f))
    (show-max-framesize . ,(option-ref options 'show-max-framesize #f))
    (show-sample-rate . ,(option-ref options 'show-sample-rate #f))
    (show-channels . ,(option-ref options 'show-channels #f))
    (show-bps . ,(option-ref options 'show-bps #f))
    (show-total-samples . ,(option-ref options 'show-total-samples #f))
    (show-vendor-tag . ,(option-ref options 'show-vendor-tag #f))
    (show-tag . ,(option-ref options 'show-tag #f))
    (show-all-tags . ,(option-ref options 'show-all-tags #f))))

(define (process-file filename options)
  "Process a single FLAC file"
  (catch #t
    (lambda ()
      (let ((metadata (flac-file-metadata filename))
            (opts (options->alist options)))
        (when (option-ref options 'list #f)
          (list-metadata metadata filename opts))
        (process-show-options opts metadata filename)))
    (lambda (key . args)
      (format (current-error-port) "Error processing ~a: ~a ~a~%" filename key args)
      #f)))

(define (meta-command args)
  "Handle the meta subcommand"
  (let* ((options (getopt-long args meta-option-spec))
         (files (option-ref options '() '())))

    (cond
     ((option-ref options 'help #f)
      (show-help)
      (exit 0))

     ((option-ref options 'version #f)
      (format #t "gflac meta version ~a~%" (flac-version/string))
      (exit 0))

     ((null? files)
      (format (current-error-port) "Error: No FLAC files specified~%")
      (show-help)
      (exit 1))

     (else
      ;; Auto-enable --with-filename if multiple files
      (when (and (> (length files) 1)
                 (not (option-ref options 'no-filename #f)))
        (set! options (assoc-set! options 'with-filename #t)))

      ;; Process each file
      (for-each (lambda (file) (process-file file options)) files)))))
