;;
;; basic tests for search
;;

(use gauche.test)

(load "../build-site")
(define *sed-symbols*
  (map
    (lambda (s)
      (string->symbol #`"@@,|s|@@"))
    *config-fallback-keys*))

(add-load-path "..")

(test-start "search")
(load "../chaton-search")
(define main #f)                        ;prevent execution of 'main'
(test-module 'chaton.search
             :allow-undefined `(@@room-name/escd@@
                                ,@*sed-symbols*))
(test-end)
