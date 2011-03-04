;;
;; basic tests for mobile
;;

(use gauche.test)

(load "../build-site")
(define *sed-symbols*
  (map
    (lambda (s)
      (string->symbol #`"@@,|s|@@"))
    *config-fallback-keys*))

(add-load-path "..")

(test-start "mobile")
(load "../chaton-poster")
(provide "./chaton-poster-@@room-name@@")
(load "../chaton-mobile")
(define main #f)                        ;prevent execution of 'main'
(test-module 'chaton.mobile
             :allow-undefined `(@@room-name/escd@@
                                ,@*sed-symbols*))
(test-end)
