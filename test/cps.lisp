
(declaim (optimize (debug 3)))

(in-package :arc/test)

(deftest t-equal/cps 
  (chk (%equal/cps '(a b) '(a b)))
  (chk (%equal/cps '(#\a 1) '(#\a 1)))
  (chk (%equal/cps '(#:k) '(#:z)))
  (chk (not (%equal/cps '(#:i #:i) '(#:a #:b))))
  (chk (not (%equal/cps '(#:i #:j) '(#:a #:a))))
  (chk (%equal/cps '(fn (#:k1) a b c) '(fn (#:k2) a b c)))
  (chk (%equal/cps '(#:k2 #:k3 1 #\a "b") '(#:a2 #:a3 1 #\a "b")))
  (chk (not (%equal/cps '(#:k2 #:k3 1 #\a "b") '(#:a3 #:a3 1 #\a "b")))))

; (deftest c-simple
;  (chkcps '(fn (:k) 