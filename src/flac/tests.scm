(define-module (flac tests)
  #:use-module (flac tests utils)
  #:use-module (flac reader)
  #:use-module (flac format)
  #:use-module (flac metadata)

  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)

  #:use-module (srfi srfi-64))

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

(define example-1
  #vu8(#x66 #x4c #x61 #x43 #x80 #x00 #x00 #x22 #x10 #x00 #x10 #x00
            #x00 #x00 #x0f #x00 #x00 #x0f #x0a #xc4 #x42 #xf0 #x00 #x00
            #x00 #x01 #x3e #x84 #xb4 #x18 #x07 #xdc #x69 #x03 #x07 #x58
            #x6a #x3d #xad #x1a #x2e #x0f #xff #xf8 #x69 #x18 #x00 #x00
            #xbf #x03 #x58 #xfd #x03 #x12 #x8b #xaa #x9a))

; (test-begin "RFC Examples")
;
;


(with-tests
 "RFC Examples"
 (test-group "Example 1"
   (with-flac-input-port (open-bytevector-input-port example-1)
    (λ ()
      (define expected-stream-info
        (make-metadata-stream-info 4096 4096 15 15 44100 2 16 1 #vu8(62 132 180 24 7 220 105 3 7 88 106 61 173 26 46 15)))
      (define expected-metadata
        (make-flac-metadata expected-stream-info #f #f #f #f #f '()))
      (test-group "Metadata"
        (let ((actual-metadata ((@@ (flac metadata) read-flac-metadata))))
          (test-equal "stream info" (flac-metadata-stream-info actual-metadata) expected-stream-info)
          (test-equal "metadata" actual-metadata expected-metadata)))))))