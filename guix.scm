;;; guix.scm -- Guix package definition for development
;;;
;;; Copyright © 2025 Steve Ayerhart <steve@ayerh.art>
;;;
;;; This file is part of Guile-FLAC.
;;;
;;; Guile-FLAC is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.

(use-modules (guix packages)
             (guix build-system gnu)
             (guix licenses)
             (guix git-download)
             (gnu packages)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages pkg-config))

(package
  (name "guile-flac")
  (version "0.1.0-git")
  (source (local-file "." "guile-flac-checkout"
                      #:recursive? #t
                      #:select? (git-predicate (current-source-directory))))
  (build-system gnu-build-system)
  (arguments
   `(#:phases
     (modify-phases %standard-phases
       (add-after 'unpack 'bootstrap
         (lambda _
           (invoke "autoreconf" "-vif"))))))
  (native-inputs
   (list autoconf
         automake
         pkg-config
         guile-3.0))
  (inputs
   (list guile-3.0))
  (propagated-inputs
   (list guile-bytestructures))
  (synopsis "FLAC audio codec implementation in GNU Guile")
  (description
   "Guile-FLAC is a Free Lossless Audio Codec (FLAC) decoder and encoder
implemented in pure GNU Guile Scheme.  It provides a readable yet usable
implementation of the FLAC specification, supporting metadata operations,
decoding FLAC files to WAV, and frame-by-frame processing.

MD5 checksum verification is available when guile-gcrypt is installed.")
  (home-page "https://github.com/steve-ayerhart/guile-flac")
  (license gpl3+))
