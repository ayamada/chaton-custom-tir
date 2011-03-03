;;
;; basic tests for entry
;;

(use gauche.test)

(load "../build-site")
(define *sed-symbols*
  (map
    (lambda (s)
      (string->symbol #`"@@,|s|@@"))
    *config-fallback-keys*))

(add-load-path "..")

(test-start "entry")
(load "../chaton-poster")
(provide "./chaton-poster-@@room-name@@")
(load "../chaton-mobile")
(provide "chaton/mobile")
(load "../chaton-entry")
(define main #f)                        ;prevent execution of 'main'
(test-module 'chaton.entry
             :allow-undefined `(@@room-description/escd@@
                                @@room-name/escd@@
                                @@room-html/escd@@
                                ,@*sed-symbols*))
(test-end)
