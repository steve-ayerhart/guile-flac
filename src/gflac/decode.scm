;;; decode.scm -- FLAC decoder subcommand for gflac
;;;
;;; Copyright (C) 2025 Steve Ayerhart <steve@ayerh.art>
;;;
;;; This file is part of Guile-FLAC.
;;;
;;; Guile-FLAC is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.

(define-module (gflac decode)
  #:use-module (flac)
  #:use-module (flac decoder)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (decode-command))

(define decode-options
  '((help (single-char #\h) (value #f))
    (output-name (single-char #\o) (value #t))
    (force (single-char #\f) (value #f))
    (test (single-char #\t) (value #f))
    (silent (single-char #\s) (value #f))
    (totally-silent (value #f))
    (verify (single-char #\V) (value #f))
    (decode-through-errors (single-char #\F) (value #f))))

(define (show-decode-help)
  (format #t "Usage: gflac decode [OPTIONS] FLACFILE [...]

Decode FLAC files to WAV format.

Options:
  -h, --help                   Show this help
  -o, --output-name=FILENAME   Output filename (only works with single file)
  -f, --force                  Force overwrite of existing files
  -t, --test                   Test decoding (no output written, implies --verify)
  -s, --silent                 Silent mode (no progress messages)
      --totally-silent         Suppress all output including errors
  -V, --verify                 Verify MD5 checksums
  -F, --decode-through-errors  Continue decoding despite errors

Examples:
  gflac decode input.flac
  gflac decode -o output.wav input.flac
  gflac decode -f *.flac
  gflac decode -t --verify test.flac
"))

(define (output-filename input-file output-name)
  "Determine output filename from INPUT-FILE and OUTPUT-NAME option."
  (if output-name
      output-name
      (let ((base (if (string-suffix? ".flac" input-file)
                      (substring input-file 0 (- (string-length input-file) 5))
                      input-file)))
        (string-append base ".wav"))))

(define (file-exists-and-not-force? filename force?)
  "Check if FILENAME exists and FORCE? is false."
  (and (not force?) (file-exists? filename)))

(define* (decode-file input-file
                      #:key
                      (output-name #f)
                      (force? #f)
                      (test? #f)
                      (silent? #f)
                      (verify? #f)
                      (continue-on-error? #f))
  "Decode a single FLAC file."
  (let ((output-file (output-filename input-file output-name)))
    (when (and (not test?) (file-exists-and-not-force? output-file force?))
      (format (current-error-port) "Error: ~a already exists (use -f to overwrite)~%" output-file)
      (exit 1))

    (unless silent?
      (format #t "~a: ~a -> ~a~%"
              (if test? "Testing" "Decoding")
              input-file
              (if test? "(no output)" output-file)))

    (catch #t
      (lambda ()
        (if test?
            (with-flac-file-decoder input-file
                                    (lambda ()
                                      (let loop ((frame (read-flac-frame)))
                                        (unless (eof-object? frame)
                                          (loop (read-flac-frame))))))

            (decode-flac-file input-file output-file
                              #:verify-md5 verify?
                              #:md5-on-error (if continue-on-error? 'warn 'error)))

        (unless silent?
          (format #t "~a: done~%" input-file))
        #t)

      (lambda (key . args)
        (if continue-on-error?
            (begin
              (format (current-error-port) "Error decoding ~a: ~a ~s~%" input-file key args)
              #f)
            (begin
              (format (current-error-port) "Error decoding ~a: ~a ~s~%" input-file key args)
              (exit 1)))))))

(define (decode-command args)
  "Handle the decode subcommand."
  (let* ((options (getopt-long args decode-options))
         (help? (option-ref options 'help #f))
         (output-name (option-ref options 'output-name #f))
         (force? (option-ref options 'force #f))
         (test? (option-ref options 'test #f))
         (silent? (option-ref options 'silent #f))
         (totally-silent? (option-ref options 'totally-silent #f))
         (verify? (or (option-ref options 'verify #f) test?))  ; test implies verify
         (continue-on-error? (option-ref options 'decode-through-errors #f))
         (files (option-ref options '() '())))

    (when help?
      (show-decode-help)
      (exit 0))

    (when (null? files)
      (format (current-error-port) "Error: no input files specified~%")
      (show-decode-help)
      (exit 1))

    (when (and output-name (> (length files) 1))
      (format (current-error-port) "Error: -o/--output-name can only be used with a single input file~%")
      (exit 1))

    (when totally-silent?
      (set-current-output-port (open-output-file "/dev/null"))
      (set-current-error-port (open-output-file "/dev/null")))

    (let ((results (map (lambda (file)
                          (decode-file file
                                       #:output-name output-name
                                       #:force? force?
                                       #:test? test?
                                       #:silent? (or silent? totally-silent?)
                                       #:verify? verify?
                                       #:continue-on-error? continue-on-error?))
                        files)))

      (when (and (not continue-on-error?) (any not results))
        (exit 1)))))
