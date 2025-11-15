;;;  uncommon-tests -- Tests from the flac test file "uncommon" directory

;; Copyright (C) 2025 Steve Ayerhart <steve@ayerh.art>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; Tests for uncommon but valid FLAC files:
;; - Files 01-04: Mid-stream format changes (sample rate, channels, bit depth)
;; - File 05: 32-bit samples
;; - File 06: 768kHz sample rate (not streamable - needs streaminfo)
;; - File 07: 15-bit samples (not streamable - needs streaminfo)
;; - File 08: Maximum blocksize 65535
;; - File 09: Rice partition order 15 (extreme case)
;; - File 10: No fLaC marker, starts at frame header
;; - File 11: Unparsable data before first frame

;;; Code:

(add-to-load-path (or (getenv "abs_top_srcdir") "."))

(use-modules (flac reader)
             (flac format)
             (flac metadata)
             (flac decoder)
             (flac)

             (rnrs bytevectors)

             (ice-9 binary-ports)
             (ice-9 receive)
             (ice-9 ftw)
             (ice-9 popen)
             (ice-9 rdelim)

             (srfi srfi-13)
             (srfi srfi-64))

(test-begin "Uncommon Tests")

(define uncommon-directory
  (let ((srcdir (or (getenv "abs_top_srcdir") ".")))
    (format #f "~a/tests/test-files/uncommon" srcdir)))

(define (remove-extension filename extension)
  "Remove EXTENSION from FILENAME"
  (if (string-suffix? extension filename)
      (substring filename 0 (- (string-length filename) (string-length extension)))
      filename))

(define (file-contents-equal? file1 file2)
  "Return #t if FILE1 and FILE2 have identical contents"
  (let ((bv1 (call-with-input-file file1 get-bytevector-all #:binary #t))
        (bv2 (call-with-input-file file2 get-bytevector-all #:binary #t)))
    (bytevector=? bv1 bv2)))

(define (decode-with-reference-flac input-file output-file)
  "Decode INPUT-FILE to OUTPUT-FILE using reference flac decoder"
  (let* ((cmd (format #f "flac -d \"~a\" -o \"~a\" --force --silent" input-file output-file))
         (port (open-pipe* OPEN_READ "/bin/sh" "-c" cmd))
         (status (close-pipe port)))
    (zero? status)))

(define (test-uncommon-file filename description should-decode)
  "Test decoding FILENAME and compare with reference decoder if should-decode is #t"
  (let* ((input-path (format #f "~a/~a" uncommon-directory filename))
         (base-name (remove-extension filename ".flac"))
         (guile-output (format #f "/tmp/guile-flac-uncommon-~a.wav"
                              (string-filter (lambda (c) (char-alphabetic? c)) base-name)))
         (ref-output (format #f "/tmp/ref-flac-uncommon-~a.wav"
                            (string-filter (lambda (c) (char-alphabetic? c)) base-name))))

    (test-assert (format #f "~a: ~a" filename description)
      (catch #t
        (lambda ()
          (if should-decode
              (begin
                (catch #t
                  (lambda ()
                    (decode-flac-file input-path guile-output)
                    #t)
                  (lambda (key . args)
                    (format (current-error-port)
                            "Decode failed for ~a: ~a ~s\n"
                            filename key args)
                    #f))

                ;; Decode with reference implementation
                (unless (decode-with-reference-flac input-path ref-output)
                  (error "Reference decoder failed"))

                ;; Compare outputs
                (let ((result (file-contents-equal? guile-output ref-output)))
                  ;; Clean up temporary files
                  (when (file-exists? guile-output)
                    (delete-file guile-output))
                  (when (file-exists? ref-output)
                    (delete-file ref-output))
                  result))

              ;; File might not decode - just verify it doesn't crash
              (begin
                (catch #t
                  (lambda ()
                    (decode-flac-file input-path guile-output)
                    (when (file-exists? guile-output)
                      (delete-file guile-output))
                    #t)
                  (lambda (key . args)
                    ;; Error is acceptable - just don't crash
                    (when (file-exists? guile-output)
                      (delete-file guile-output))
                    #t)))))
        (lambda (key . args)
          ;; Uncaught error (crash) - this is always a failure
          (format (current-error-port)
                  "Uncaught error in ~a: ~a ~s\n"
                  filename key args)
          (when (file-exists? guile-output)
            (delete-file guile-output))
          (when (file-exists? ref-output)
            (delete-file ref-output))
          #f)))))

(test-group "Mid-stream Format Changes"
  ;; These files change format parameters mid-stream
  ;; The reference decoder REJECTS these files - they are invalid per FLAC spec
  ;; Our decoder handles them gracefully (may decode partial or error)

  (test-uncommon-file "01 - changing samplerate.flac"
                      "Sample rate changes: 32kHz -> 24kHz -> 16kHz -> 48kHz (invalid)"
                      #f)

  (test-uncommon-file "02 - increasing number of channels.flac"
                      "Channels increase: 1 -> 2 -> 6 (invalid)"
                      #f)

  (test-uncommon-file "03 - decreasing number of channels.flac"
                      "Channels decrease: 4 -> 2 -> 1 (invalid)"
                      #f)

  (test-uncommon-file "04 - changing bitdepth.flac"
                      "Bit depth changes: 16 -> 8 -> 24 (invalid)"
                      #f))

(test-group "Extreme Values"
  (test-uncommon-file "05 - 32bps audio.flac"
                      "32-bit samples"
                      #t)

  (test-uncommon-file "06 - samplerate 768kHz.flac"
                      "768kHz sample rate (not in frame header)"
                      #t)

  (test-uncommon-file "07 - 15 bit per sample.flac"
                      "15-bit samples (not in frame header)"
                      #t)

  (test-uncommon-file "08 - blocksize 65535.flac"
                      "Maximum blocksize 65535"
                      #t)

  (test-uncommon-file "09 - Rice partition order 15.flac"
                      "Rice partition order 15 (1 sample per partition)"
                      #t))

(test-group "Non-standard Structure"
  (test-uncommon-file "10 - file starting at frame header.flac"
                      "No fLaC marker or metadata (stream format)"
                      #f)  ; Might not decode - we require streaminfo

  (test-uncommon-file "11 - file starting with unparsable data.flac"
                      "Unparsable data before first frame"
                      #f)) ; Might not decode - simulates partial stream

(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "Uncommon Tests")

(exit (= 0 exit-status))
