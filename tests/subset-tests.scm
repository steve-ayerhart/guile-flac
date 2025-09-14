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

(add-to-load-path (getenv "abs_top_srcdir"))


(use-modules (flac reader)
             (flac format)
             (flac metadata)
             (flac decoder)

             (rnrs bytevectors)

             (ice-9 binary-ports)
             (ice-9 receive)

             (srfi srfi-64))

(test-begin "Subset Tests")

(define subset-directory
  (format #f "~a/tests/test-files/subset" (getenv  "abs_top_srcdir")))

(test-group "Various blocksizes"
  (define blocksizes '(4096 4608 16 192 254 512 725 1000 1937 2304))
  (define filenames
    (map
     (lambda (blocksize index)
       (format #f "~a/~2,'0d - blocksize ~a.flac" subset-directory index blocksize)) blocksizes (iota 10 1)))

  )

(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "Subset Tests")

(exit (= 0 exit-status))
