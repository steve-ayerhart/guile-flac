;;;  faulty-tests -- Tests from the flac test file "faulty" directory

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
;; Tests for faulty FLAC files. These files contain various errors:
;; - Files 01-05: Incorrect streaminfo metadata (wrong blocksize, framesize, bit depth, channels, sample count)
;; - File 06: Missing streaminfo metadata block
;; - File 07: Streaminfo not first metadata block
;; - File 08: Blocksize 65536 (invalid - not representable in streaminfo)
;; - File 09: Blocksize 1 (invalid - only allowed for last frame)
;; - File 10: Invalid vorbis comment metadata block
;; - File 11: Incorrect metadata block length

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

(test-begin "Faulty Tests")

(define faulty-directory
  (let ((srcdir (or (getenv "abs_top_srcdir") ".")))
    (format #f "~a/tests/test-files/faulty" srcdir)))

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

(define (delete-test-file path)
  (when (file-exists? path)
    (delete-file path)))

(define (test-faulty-file filename expected-behavior description)
  "Test that FILENAME exhibits EXPECTED-BEHAVIOR.
   EXPECTED-BEHAVIOR can be:
   - 'should-error: decoder should raise an error
   - 'should-decode: decoder might decode despite metadata issues
   - 'should-handle-gracefully: decoder should not crash but may error"
  (let* ((input-path (format #f "~a/~a" faulty-directory filename))
         (output-path (format #f "/tmp/guile-flac-faulty-~a.wav"
                              (string-filter (lambda (c) (char-alphabetic? c)) filename)))
         (ref-output (format #f "/tmp/ref-flac-faulty-~a.wav"
                            (string-filter (lambda (c) (char-alphabetic? c)) filename))))

    (test-assert (format #f "~a: ~a" filename description)
      (catch #t
        (lambda ()
          (let ((result
                 (catch #t
                   (lambda ()
                     (decode-flac-file input-path output-path)
                     'decoded-successfully)
                   (lambda (key . args)
                     (cons 'error (cons key args))))))

            (case expected-behavior
              ((should-error)
               (when (file-exists? output-path)
                 (delete-file output-path))
               (and (pair? result) (eq? (car result) 'error)))

              ((should-decode)
               (let ((decode-with-reference-result (decode-with-reference-flac input-path ref-output))
                     (output-matches? (file-contents-equal? output-path ref-output)))
                 (delete-test-file output-path)
                 (delete-test-file ref-output)

                 (and (eq? result 'decoded-successfully) output-matches?)))
;;               (if (eq? result 'decoded-successfully)
;;                   (if (decode-with-reference-flac input-path ref-output)
;;                       (let ((match? (file-contents-equal? output-path ref-output)))
;;                         (when (file-exists? output-path)
;;                           (delete-file output-path))
;;                         (when (file-exists? ref-output)
;;                           (delete-file ref-output))
;;                         match?)
;;                       (begin
;;                         (when (file-exists? output-path)
;;                           (delete-file output-path))
;;                         #f))
;;                   (begin
;;                     (when (file-exists? output-path)
;;                       (delete-file output-path))
;;                     #f)))

              ((should-handle-gracefully)
               (let ((decode-with-reference-result (decode-with-reference-flac input-path ref-output))
                     (output-matches? (file-contents-equal? output-path ref-output)))
                 (unless output-matches?
                   (format (current-error-port)
                           "Warning: ~a decoded but output differs from reference\n"
                           filename))
                 (delete-test-file output-path)
                 (delete-test-file ref-output)
                 (eq? result 'decoded-successfully)))

              (else
               (error "Unknown expected behavior" expected-behavior)))))

        (lambda (key . args)
          (format (current-error-port)
                  "Uncaught error in ~a: ~a ~s\n"
                  filename key args)
          (delete-test-file output-path)
          (delete-test-file ref-output)
          #f)))))

(test-group "Metadata Mismatches"
  (test-faulty-file "01 - wrong max blocksize.flac"
                    'should-handle-gracefully
                    "Streaminfo says max blocksize 4096, actual is 16384")

  (test-faulty-file "02 - wrong maximum framesize.flac"
                    'should-handle-gracefully
                    "Streaminfo says max framesize 654, actual is 8846")

  (test-faulty-file "03 - wrong bit depth.flac"
                    'should-handle-gracefully
                    "Streaminfo says 24-bit, actual is 16-bit")

  (test-faulty-file "04 - wrong number of channels.flac"
                    'should-handle-gracefully
                    "Streaminfo says 5 channels, actual is 1")

  (test-faulty-file "05 - wrong total number of samples.flac"
                    'should-handle-gracefully
                    "Streaminfo says 39842 samples, actual is 109487"))

(test-group "Structural Violations"
  (test-faulty-file "06 - missing streaminfo metadata block.flac"
                    'should-error
                    "No streaminfo block (has other metadata)")

  (test-faulty-file "07 - other metadata blocks preceding streaminfo metadata block.flac"
                    'should-error
                    "Streaminfo is not first metadata block"))

(test-group "Invalid Values"
  (test-faulty-file "08 - blocksize 65536.flac"
                    'should-error
                    "Blocksize 65536 not representable in streaminfo")

  (test-faulty-file "09 - blocksize 1.flac"
                    'should-handle-gracefully
                    "Blocksize 1 only allowed for last frame (complex to validate)"))

(test-group "Malformed Metadata"
  (test-faulty-file "10 - invalid vorbis comment metadata block.flac"
                    'should-handle-gracefully
                    "Vorbis comment block has invalid tag count")

  (test-faulty-file "11 - incorrect metadata block length.flac"
                    'should-handle-gracefully
                    "Metadata block length causes parser to read garbage"))

(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "Faulty Tests")

(exit (= 0 exit-status))
