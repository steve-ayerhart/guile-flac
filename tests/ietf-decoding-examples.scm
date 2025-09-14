;;; ietf-decoding-examples.scm -- Tests from the examples on the ietf document

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
(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (flac reader)
             (flac format)
             (flac metadata)
             (flac decoder)

             (rnrs bytevectors)

             (ice-9 binary-ports)
             (ice-9 receive)
             (ice-9 format)

             (srfi srfi-64))

;;; https://datatracker.ietf.org/doc/html/draft-ietf-cellar-flac#name-decoding-example-1
(test-begin "IETF Examples")

(test-group "Example 1"
  (define example-1
    #vu8(#x66 #x4c #x61 #x43 #x80 #x00 #x00 #x22 #x10 #x00 #x10 #x00
              #x00 #x00 #x0f #x00 #x00 #x0f #x0a #xc4 #x42 #xf0 #x00 #x00
              #x00 #x01 #x3e #x84 #xb4 #x18 #x07 #xdc #x69 #x03 #x07 #x58
              #x6a #x3d #xad #x1a #x2e #x0f #xff #xf8 #x69 #x18 #x00 #x00
              #xbf #x03 #x58 #xfd #x03 #x12 #x8b #xaa #x9a))

  (define expected-stream-info
    (%make-metadata-stream-info
     4096 4096 15 15 44100 2 16 1
     #vu8(62 132 180 24 7 220 105 3 7 88 106 61 173 26 46 15)))

  (define expected-frame-header
    (%make-frame-header 'fixed 1 44100 'independent 16 0 191))

  (define expected-frame-footer
    (%make-frame-footer 43674))

  (define expected-frame-samples
    (array-cell-set! (array-cell-set! (make-array 0 2 4096) #(25588) 0) #(10416) 1))

  (with-flac-input-port
   (open-bytevector-input-port example-1)
   (λ ()
     (flac-read/assert-magic)
     (let ((metadata (read-flac-metadata)))
       (with-initialized-decoder
        (flac-metadata-stream-info metadata)
        (lambda ()

          (test-group "Metadata"
            (test-equal "stream info" expected-stream-info (flac-metadata-stream-info metadata)))

          (read-flac-frame)

          (test-group "Frame"
            (test-equal "Header" expected-frame-header (current-frame-header))
            (test-equal "Samples" expected-frame-samples (current-frame-samples)))))))))

;;; https://datatracker.ietf.org/doc/html/draft-ietf-cellar-flac#name-decoding-example-2
(test-group "Example 2"
  (define example-2
    #vu8(#x66 #x4c #x61 #x43 #x00 #x00 #x00 #x22 #x00 #x10 #x00 #x10
              #x00 #x00 #x17 #x00 #x00 #x44 #x0a #xc4 #x42 #xf0 #x00 #x00
              #x00 #x13 #xd5 #xb0 #x56 #x49 #x75 #xe9 #x8b #x8d #x8b #x93
              #x04 #x22 #x75 #x7b #x81 #x03 #x03 #x00 #x00 #x12 #x00 #x00
              #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
              #x00 #x00 #x00 #x10 #x04 #x00 #x00 #x3a #x20 #x00 #x00 #x00
              #x72 #x65 #x66 #x65 #x72 #x65 #x6e #x63 #x65 #x20 #x6c #x69
              #x62 #x46 #x4c #x41 #x43 #x20 #x31 #x2e #x33 #x2e #x33 #x20
              #x32 #x30 #x31 #x39 #x30 #x38 #x30 #x34 #x01 #x00 #x00 #x00
              #x0e #x00 #x00 #x00 #x54 #x49 #x54 #x4c #x45 #x3d #xd7 #xa9
              #xd7 #x9c #xd7 #x95 #xd7 #x9d #x81 #x00 #x00 #x06 #x00 #x00
              #x00 #x00 #x00 #x00 #xff #xf8 #x69 #x98 #x00 #x0f #x99 #x12
              #x08 #x67 #x01 #x62 #x3d #x14 #x42 #x99 #x8f #x5d #xf7 #x0d
              #x6f #xe0 #x0c #x17 #xca #xeb #x21 #x00 #x0e #xe7 #xa7 #x7a
              #x24 #xa1 #x59 #x0c #x12 #x17 #xb6 #x03 #x09 #x7b #x78 #x4f
              #xaa #x9a #x33 #xd2 #x85 #xe0 #x70 #xad #x5b #x1b #x48 #x51
              #xb4 #x01 #x0d #x99 #xd2 #xcd #x1a #x68 #xf1 #xe6 #xb8 #x10
              #xff #xf8 #x69 #x18 #x01 #x02 #xa4 #x02 #xc3 #x82 #xc4 #x0b
              #xc1 #x4a #x03 #xee #x48 #xdd #x03 #xb6 #x7c #x13 #x30))

  (define expected-stream-info
    (%make-metadata-stream-info
     16 16 23 68 44100 2 16 19
     #vu8(213 176 86 73 117 233 139 141 139 147 4 34 117 123 129 3)))

  (define expected-vorbis-comment
    (%make-metadata-vorbis-comment
     "reference libFLAC 1.3.3 20190804"
     (list '("TITLE" "שלום"))))

  (define expected-padding
    (%make-metadata-padding 6))

  (define expected-seek-table
    (%make-metadata-seek-table
     (list (%make-metadata-seek-point 0 0 16))))

  (define expected-first-frame-header
    (%make-frame-header 'fixed 16 44100 'right 16 0 153))

  (define expected-first-frame-samples
    #2((10372 18041 14942 17876 15627 17899 16242 18077 16824 18263 17295 -14418 -15201 -14508 -15195 -14818)
       (6070 10545 8743 10449 9143 10463 9502 10569 9840 10680 10113 -8428 -8895 -8476 -8896 -8653)))

  (define expected-first-frame-footer
    (%make-frame-footer 47120))

  (define expected-second-frame-header
    (%make-frame-header 'fixed 3 44100 'independent 16 1 164))

  (define expected-second-frame-samples
    #2((-15486 -15349 -16054 17876 15627 17899 16242 18077 16824 18263 17295 -14418 -15201 -14508 -15195 -14818)
       (-9072 -8958 -9410 10449 9143 10463 9502 10569 9840 10680 10113 -8428 -8895 -8476 -8896 -8653)))

  (define expected-second-frame-footer
    (%make-frame-footer 4912))

  (with-flac-input-port
   (open-bytevector-input-port example-2)
   (λ ()
     (flac-read/assert-magic)
     (let ((metadata (read-flac-metadata)))
       (with-initialized-decoder
        (flac-metadata-stream-info metadata)
        (lambda ()

          (test-group "Metadata"
            (test-equal "stream info"
              (flac-metadata-stream-info metadata) expected-stream-info)
            (test-equal "vorbis comment"
              (find-metadata metadata 'vorbis-comment) expected-vorbis-comment)
            (test-equal "padding"
              (find-metadata metadata 'padding) expected-padding)
            (test-equal "seek table"
              (find-metadata metadata 'seek-table) expected-seek-table))

          (test-group "Frames"

            (read-flac-frame)

            (test-group "Frame 1"
              (test-equal "Header" (current-frame-header) expected-first-frame-header)
              (test-equal "Samples" (current-frame-samples) expected-first-frame-samples))

            (read-flac-frame)

            (test-group "Frame 2"
              (test-equal "Header" (current-frame-header) expected-second-frame-header)
              (test-equal "Samples" (current-frame-samples) expected-second-frame-samples)))))))))

;;; https://datatracker.ietf.org/doc/html/draft-ietf-cellar-flac#name-decoding-example-3
(test-group "Example 3"
  (define example-3
    #vu8(#x66 #x4c #x61 #x43 #x80 #x00 #x00 #x22 #x10 #x00 #x10 #x00 #x00
              #x00 #x1f #x00 #x00 #x1f #x07 #xd0 #x00 #x70 #x00 #x00 #x00
              #x18 #xf8 #xf9 #xe3 #x96 #xf5 #xcb #xcf #xc6 #xdc #x80 #x7f
              #x99 #x77 #x90 #x6b #x32 #xff #xf8 #x68 #x02 #x00 #x17 #xe9
              #x44 #x00 #x4f #x6f #x31 #x3d #x10 #x47 #xd2 #x27 #xcb #x6d
              #x09 #x08 #x31 #x45 #x2b #xdc #x28 #x22 #x22 #x80 #x57 #xa3))

  (define expected-stream-info
    (%make-metadata-stream-info
     4096 4096 31 31 32000 1 8 24
     #vu8(248 249 227 150 245 203 207 198 220 128 127 153 119 144 107 50)))

  (define expected-first-frame-header
    (%make-frame-header 'fixed 24 32000 'independent 8 0 233))

  (define expected-first-frame-footer
    (%make-frame-footer 22435))

  (define expected-first-frame-samples
    (array-cell-set! (make-array 0 1 4096) #(0 79 111 78 8 -61 -90 -68 -13 42 67 53 13 -27 -46 -38 -12 14 24 19 6 -4 -5) 0))

  (with-flac-input-port
   (open-bytevector-input-port example-3)
   (λ ()
     (flac-read/assert-magic)
     (let ((metadata (read-flac-metadata)))
       (with-initialized-decoder
        (flac-metadata-stream-info metadata)
        (lambda ()

          (test-group "Metadata"
            (test-equal "stream info" expected-stream-info (flac-metadata-stream-info metadata)))

          (test-group "Frames"

            (read-flac-frame)

            (test-equal "Frame 1" expected-first-frame-samples (current-frame-samples)))))))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "IETF Examples")

(exit (= 0 exit-status))
