;;
;; basic tests for browser
;;

(use gauche.test)

(load "../build-site")
(define *sed-symbols*
  (map
    (lambda (s)
      (string->symbol #`"@@,|s|@@"))
    *config-fallback-keys*))

(add-load-path "..")

(test-start "browser")
(load "../chaton-browser")
(define main #f)                        ;prevent execution of 'main'
(test-module 'chaton.browser
             :allow-undefined `(@@room-name/escd@@
                                ,@*sed-symbols*))
(test-end)
