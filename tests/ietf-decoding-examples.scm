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

             (srfi srfi-64))

(test-begin "IETF Examples")

(test-group "Example 1"
  (define example-1
    #vu8(#x66 #x4c #x61 #x43 #x80 #x00 #x00 #x22 #x10 #x00 #x10 #x00
              #x00 #x00 #x0f #x00 #x00 #x0f #x0a #xc4 #x42 #xf0 #x00 #x00
              #x00 #x01 #x3e #x84 #xb4 #x18 #x07 #xdc #x69 #x03 #x07 #x58
              #x6a #x3d #xad #x1a #x2e #x0f #xff #xf8 #x69 #x18 #x00 #x00
              #xbf #x03 #x58 #xfd #x03 #x12 #x8b #xaa #x9a))

  (define expected-streaminfo
    (%make-metadata-stream-info
     4096 4096 15 15 44100 2 16 1
     #vu8(62 132 180 24 7 220 105 3 7 88 106 61 173 26 46 15)))

  (define expected-frame
    (%make-frame
     (%make-frame-header 'fixed 1 44100 'independent 16 0 191)
     43674 '((25588) (10416))))


  (receive (actual-metadata actual-frame)
      (with-flac-input-port (open-bytevector-input-port example-1)
        (λ ()
          (flac-read/assert-magic)
          (let* ((metadata (read-flac-metadata))
                 (stream-info (flac-metadata-stream-info metadata)))
            (values metadata
                    (read-flac-frame stream-info)))))
    (test-group "Metadata"
      (test-equal "stream info"
        (flac-metadata-stream-info actual-metadata) expected-streaminfo))
    (test-group "Frame"
      (test-equal "first frame" expected-frame expected-frame))))

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

  (define expected-first-frame
    (%make-frame
     (%make-frame-header 'fixed 16 44100 'right 16 0 153)
     47120
     '((4302 7496 6199 7427 6484 7436 6740 7508 6984 7583 7182 -5990 -6306 -6032 -6299 -6165)
       (6070 10545 8743 10449 9143 10463 9502 10569 9840 10680 10113 -8428 -8895 -8476 -8896 -8653))))

  (define expected-second-frame
    (%make-frame
     (%make-frame-header 'fixed 3 44100 'independent 16 1 164)
     4912
     '((-15486 -15349 -16054)
       (-9072 -8958 -9410))))

  (receive (actual-metadata actual-first-frame actual-second-frame)
      (with-flac-input-port (open-bytevector-input-port example-2)
        (λ ()
          (flac-read/assert-magic)
          (let* ((metadata (read-flac-metadata))
                 (stream-info (flac-metadata-stream-info metadata)))
            (values
             metadata (read-flac-frame stream-info) (read-flac-frame stream-info)))))
    (test-group "Metadata"
      (test-equal "stream info"
        (flac-metadata-stream-info actual-metadata)
        expected-stream-info)
      (test-equal "vorbis comment"
        (flac-metadata-vorbis-comment actual-metadata)
        expected-vorbis-comment)
      (test-equal "padding"
        (flac-metadata-padding actual-metadata)
        expected-padding)
      (test-equal "seek table"
        (flac-metadata-seek-table actual-metadata)
        expected-seek-table))
    (test-group "Frames"
      (test-equal "frame 1" actual-first-frame expected-first-frame)
      (test-equal "frame 2" actual-second-frame expected-second-frame))))

;;(define example-3
;;  #vu8(#x66 #x4c #x61 #x43 #x80 #x00 #x00 #x22 #x10 #x00 #x10 #x00 #x00
;;            #x00 #x1f #x00 #x00 #x1f #x07 #xd0 #x00 #x70 #x00 #x00 #x00
;;            #x18 #xf8 #xf9 #xe3 #x96 #xf5 #xcb #xcf #xc6 #xdc #x80 #x7f
;;            #x99 #x77 #x90 #x6b #x32 #xff #xf8 #x68 #x02 #x00 #x17 #xe9
;;            #x44 #x00 #x4f #x6f #x31 #x3d #x10 #x47 #xd2 #x27 #xcb #x6d
;;            #x09 #x08 #x31 #x45 #x2b #xdc #x28 #x22 #x22 #x80 #x57 #xa3))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "IETF Examples")

(exit (= 0 exit-status))
