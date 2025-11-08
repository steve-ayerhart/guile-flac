;;;  subset-tests -- Tests from the flac test file "subset" directory

;; Copyright (C) 2022 Steve Ayerhart <steve@ayerh.art>
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

             (srfi srfi-13)  ; string-suffix?, etc.
             (srfi srfi-64))

(test-begin "Subset Tests")

(define subset-directory
  (let ((srcdir (or (getenv "abs_top_srcdir") ".")))
    (format #f "~a/tests/test-files/subset" srcdir)))

(define (remove-extension filename extension)
  "Remove EXTENSION from FILENAME"
  (if (string-suffix? extension filename)
      (substring filename 0 (- (string-length filename) (string-length extension)))
      filename))

(define (get-flac-files directory)
  "Return a sorted list of all .flac files in DIRECTORY"
  (let ((files (scandir directory
                        (lambda (file)
                          (string-suffix? ".flac" file)))))
    (if files
        (sort files string<?)
        '())))

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

(define (test-flac-file filename)
  "Test decoding FILENAME and compare with reference decoder"
  (let* ((input-path (format #f "~a/~a" subset-directory filename))
         (base-name (remove-extension filename ".flac"))
         (guile-output (format #f "/tmp/guile-flac-test-~a.wav" base-name))
         (ref-output (format #f "/tmp/ref-flac-test-~a.wav" base-name)))

    (test-assert (format #f "Decode ~a" filename)
      (catch #t
        (lambda ()
          ;; Decode with our implementation (with MD5 verification)
          (decode-flac-file input-path guile-output #:verify-md5 #t #:md5-on-error 'error)

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
        (lambda (key . args)
          ;; Clean up on error
          (when (file-exists? guile-output)
            (delete-file guile-output))
          (when (file-exists? ref-output)
            (delete-file ref-output))
          (format (current-error-port) "Error decoding ~a: ~a ~s\n" filename key args)
          #f)))))

(test-group "FLAC Subset Test Suite"
  (let ((flac-files (get-flac-files subset-directory)))
    (for-each test-flac-file flac-files)))

(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "Subset Tests")

(exit (= 0 exit-status))
