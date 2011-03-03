;;
;; basic tests for chaton.scm
;;

(use gauche.test)

(load "../build-site")
(define *sed-symbols*
  (map
    (lambda (s)
      (string->symbol #`"@@,|s|@@"))
    *config-fallback-keys*))

(test-start "chaton.scm")
(load "../chaton.scm")
(define main #f)                        ;prevent execution of 'main'
(test-module 'chaton
             :allow-undefined *sed-symbols*)
(test-end)
