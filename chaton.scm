;;;
;;; Some common routines for chaton scripts
;;;
(define-module chaton
  (use srfi-1)
  (use srfi-13)
  (use srfi-19)
  (use text.html-lite)
  (use text.tree)
  (use file.util)
  (use util.match)
  (use util.list)
  (use www.cgi)
  (use gauche.charconv)
  (use gauche.fcntl)
  (use gauche.sequence)
  (use gauche.experimental.lamb)
  (export chaton-render chaton-read-entries
          chaton-render-from-file
          chaton-render-html-1 chaton-render-rss-1
          chaton-with-shared-locking chaton-with-exclusive-locking

          chaton-alist->stree

          decompose-entry
          make-anchor-string
          make-permalink
          html-format-entry

          +room-url+ +archive-url+ +mobile-url+
          
          +datadir+ +current-file+ +sequence-file+
          +last-post-file+ +num-chatters-file+
          
          +logdir+

          +docdir+ +status.js+ +status.scm+ +index.rdf+

          +show-stack-trace+

          make-state
          state-chatter
          state-ip
          state-timestamp

          get-systime
          get-timestamp-omit-interval
          charconv-cgi-param
          make-navigation-html
          make-cookie-args
          
          with-output-to-file))
(select-module chaton)

;;; Some common constants
(define-constant +room-url+    "@@httpd-url@@@@url-path@@")
(define-constant +archive-url+ (build-path +room-url+ "a"))
;(define-constant +mobile-url+ (build-path +room-url+ "m"))
(define-constant +mobile-url+ +room-url+)

(define-constant +datadir+ (or (sys-getenv "CHATON_DATADIR")
                               "@@server-data-dir@@data"))
(define-constant +current-file+ (build-path +datadir+ "current.dat"))
(define-constant +sequence-file+ (build-path +datadir+ "sequence"))
(define-constant +last-post-file+ (build-path +datadir+ "last-post"))
(define-constant +num-chatters-file+ (build-path +datadir+ "num-chatters"))

(define-constant +logdir+  (or (sys-getenv "CHATON_LOGDIR")
                               "@@server-data-dir@@logs"))

(define-constant +docdir+ (or (sys-getenv "CHATON_DOCDIR")
                              "@@server-htdocs-dir@@"))
(define-constant +status.js+ (build-path +docdir+ "var/status.js"))
(define-constant +status.scm+ (build-path +docdir+ "var/status.scm"))
(define-constant +index.rdf+ (build-path +docdir+ "var/index.rdf"))

(define-constant +show-stack-trace+
  (read-from-string "@@show-stack-trace-on-error@@"))

;;;
;;;  Entries
;;;

;; es : ((<nick> (<sec> <nsec>) <text> <ipaddr>) ...)
;; last-state : (<chatter> <ipaddr> <timestamp>)
;; renderer : optional(if omitted, chaton-render-html-1 is used)
;; returns <text-tree> and new-state
(define (chaton-render es last-state . opts)
  (let-optionals* opts ((renderer chaton-render-html-1))
    (map-accum renderer (ensure-state last-state) es)))

;; Read data file from FILE, starting from POS.
;; Returns list of entries and new POS.
(define (chaton-read-entries file pos)
  (receive (lines pos) (read-diff file pos)
    (values (safe-lines->sexps lines) pos)))

;; A convenience routine combinig above two.
;; returns <text-tree>, new-state, and new POS.
(define (chaton-render-from-file file pos last-state
                                 :key (renderer chaton-render-html-1)
                                      (newest-first #f))
  (receive (es pos) (chaton-read-entries file pos)
    (let1 es2 (if newest-first (reverse es) es)
      (receive (tree new-state) (chaton-render es2 last-state renderer)
        (values tree new-state pos)))))

;; Utility; render alist into S-expr or Json.  Assuming keys are symbols.
(define (chaton-alist->stree alist sexp?)
  (if sexp?
    (write-to-string alist)
    (letrec ([obj (lambda (x)
                    (if (list? x)
                      (array x)
                      (write-to-string x)))]
             [array (lambda (xs)
                      `("[" ,(intersperse "," (map obj xs)) "]"))])
      `("{",(intersperse "," (map (^(p)`(,(write-to-string (x->string (car p)))
                                         ":",(obj (cdr p))))
                                  alist))"}"))))

;;;
;;;  rendering
;;;

(define (ensure-state last-state) ; bridge to support backward compat. 
  (match last-state
    [(c i t) last-state]
    [_       '(#f #f #f)]))

(define (make-state chatter ip timestamp) (list chatter ip timestamp))
(define (state-chatter last-state) (car last-state))
(define (state-ip last-state)      (cadr last-state))
(define (state-timestamp last-state) (caddr last-state))

(define (chaton-render-html-1 entry last-state)
  (receive (nick sec usec text ip) (decompose-entry entry)
    (let* ([anchor-string (make-anchor-string sec usec)]
           [permalink (make-permalink sec anchor-string)])
      (values `(,(if (and (equal? nick (state-chatter last-state))
                          (equal? ip (state-ip last-state))
                          (< (abs (- (state-timestamp last-state) sec)) 
                             (get-timestamp-omit-interval)))
                   '()
                   (html:div
                    :class "entry-header"
                    (html:span :class "timestamp"
                               (sys-strftime "%Y/%m/%d %T %Z" (get-systime sec)))
                    (html:span :class "chatter" (html-escape-string nick))))
                ,(html:a :class "permalink-anchor"
                         :id #`"anchor-,anchor-string"
                         :href permalink :name permalink :target "_parent"
                         "#")
                ,(html-format-entry text anchor-string))
              (make-state nick ip sec)))))

(define (chaton-render-rss-1 entry last-state)
  (receive (nick sec usec text ip) (decompose-entry entry)
    (let* ([text-with-nick #`",|nick|: ,|text|"]
           [anchor-string (make-anchor-string sec usec)]
           [permalink (make-permalink sec anchor-string)]
           [title (html-escape-string (if-let1 m (#/^[^\n]*/ text-with-nick)
                                        (m 0)
                                        text-with-nick))]
           [desc (html-format-entry text-with-nick anchor-string)])
      ;; NB: DESC can never have "]]>" in it, since the external text has
      ;; gone through safe-text and all >'s in it are replaced by &gt's.
      (values `("<item>\n"
                "<title>" ,title "</title>\n"
                "<link>" ,permalink "</link>\n"
                "<description><![CDATA[" ,desc "]]></description>\n"
                "<content:encoded><![CDATA[" ,desc "]]></content:encoded>\n"
                "<pubDate>" ,(time->rfc822-date-string sec) ,"</pubDate>\n"
                "<guid isPermaLink=\"true\">" ,permalink "</guid>\n"
                "</item>\n")
              (make-state nick ip sec)))))

(define (decompose-entry entry)
  (match-let1 (nick (sec usec) text . opt) entry
    (values nick sec usec text (if (pair? opt) (car opt) #f))))

(define (make-anchor-string sec usec) (format "entry-~x-~2,'0x" sec usec))

(define (make-permalink sec anchor)
  (build-path +archive-url+
              (format "~a#~a"
                      (sys-strftime "%Y/%m/%d" (get-systime sec))
                      anchor)))

(define (html-format-entry entry-text anchor-string :optional (dont-expand-url @@dont-expand-url@@))
  (if (#/\n/ entry-text)
    (html:pre :class "entry-multi" :id anchor-string
              (safe-text entry-text #t))
    (html:div :class "entry-single" :id anchor-string
              (html:span (safe-text entry-text dont-expand-url)))))  

(define *url-rx* #/https?:\/\/(\/\/[^\/?#\s]*)?([^?#\s\"]*(\?[^#\s\"]*)?(#[^\s\"]*)?)/)

(define (safe-text text :optional (dont-expand-url @@dont-expand-url@@))
  (let loop ([s text] [r '()])
    (cond
     [(string-null? s) (reverse r)]
     [(*url-rx* s)
      => (^(m)
           (loop (m'after)
                 `(,(render-url (m 0) dont-expand-url)
                   ,(html-escape-string (m'before))
                   ,@r)))]
     [else (reverse (cons (html-escape-string s) r))])))

(define (render-url url :optional (dont-expand-url @@dont-expand-url@@))
  (if dont-expand-url
    (render-url-default url)
    (rxmatch-case url
      [#/\.(?:jpg|gif|png)$/i () (render-url-image url)]
      [#/^http:\/\/(\w{2,3}\.youtube\.com)\/watch\?v=([\w-]{1,12})/ (_ host vid)
       (render-url-youtube url host vid)]
      [#/^http:\/\/www\.nicovideo\.jp\/watch\/(\w{1,13})/ (_ vid)
       (render-url-nicovideo url vid)]
      [else (render-url-default url)])))

(define (render-url-default url)
  (html:a :href url :rel "nofollow" :class "link-default"
          :onclick "window.open(this.href); return false;"
          (html-escape-string url)))

(define (render-url-image url)
  (html:a :href url :rel "nofollow" :class "link-image hide-while-loading"
          :onclick "window.open(this.href); return false;"
          (html:img :src url :alt url :onload "checkImageSize(this);")))

(define (render-url-youtube url host vid)
  (html:iframe :title "YouTube video player"
               :class "youtube-player"
               :type "text/html"
               :width "@@embed-youtube-width@@"
               :height "@@embed-youtube-height@@"
               :src #`"http://,|host|/embed/,|vid|"
               :frameborder "0"
               :allowFullScreen #t
               :onload "scrollToBottom();"
               (html:a :href url (html-escape-string url))))

(define (render-url-nicovideo url vid)
  (html:iframe :width "312" :height "176"
               :src #`"http://ext.nicovideo.jp/thumb/,|vid|"
               :scrolling "no" :class "nicovideo"
               :frameborder "0"
               :onload "scrollToBottom();"
               (html:a :href url (html-escape-string url))))

(define (time->rfc822-date-string seconds)
  (date->string (time-utc->date (make <time> :second seconds)) "~a, ~e ~b ~Y ~X ~z"))

;;;
;;;  Reading datafile
;;;

(define *lockfile* "@@server-data-dir@@lock")

(define (%with-chaton-lock locktype opener closer thunk)
  (ensure-lockfile)
  (let1 p #f
    (unwind-protect
        (begin (set! p (opener *lockfile*))
               (sys-fcntl p F_SETLK (make <sys-flock> :type locktype))
               (thunk))
      (when p
        (sys-fcntl p F_SETLK (make <sys-flock> :type F_UNLCK))
        (closer p)))))

(define (chaton-with-shared-locking thunk)
  (%with-chaton-lock F_RDLCK open-input-file close-input-port thunk))

(define (chaton-with-exclusive-locking thunk)
  (%with-chaton-lock F_WRLCK (cut open-output-file <> :if-exists :overwrite)
                     close-output-port thunk))

(define (ensure-lockfile)
  (unless (file-exists? *lockfile*)
    (with-output-to-file *lockfile* (cut display "lockfile\n")
                         :if-does-not-exist :create :if-exists #f)))

;; Read the source file from offset START, and returns a list of
;; lines and the updated offset that points the end of the source file.
(define (read-diff source start)
  (chaton-with-shared-locking
   (cut call-with-input-file source
        (^(in) (cond [in (port-seek in start)
                         (let1 tx (port->string-list in)
                           (values tx (port-tell in)))]
                     [else (values "" 0)]))
        :if-does-not-exist #f)))

(define (safe-read line)
  (guard (e [(<read-error> e) #f]) (read-from-string line)))

(define (safe-lines->sexps lines) (filter pair? (map safe-read lines)))

;;;
;;;  Misc. Utility
;;;

(define (get-systime sec)
  ((if @@use-localtime@@ sys-localtime sys-gmtime) sec))

(define (get-timestamp-omit-interval)
  (or
    @@timestamp-omit-interval@@
    240))

(define (charconv-cgi-param params)
  (let* ((_mb (or
                (cgi-get-parameter "_mb" params :default #f)
                (tree->string params)))
         (params-encoding (ces-guess-from-string _mb "*JP")))
    (define (_mb-convert str)
      (if (string? str)
        (ces-convert str params-encoding)
        str)) ; for (eq? str #t)
    (map
      (lambda (key+vals)
        (map
          _mb-convert
          key+vals))
      params)))

(define (make-navigation-html :key (mobile-mode? #f))
  (html:div
    :id "navigation"
    (intersperse
      " | "
      (filter
        identity
        (list*
          (html:a :href "a/today" "Read Archive")
          (html:a :href "@@httpd-url@@@@url-path@@var/index.rdf" "RSS")
          (cond
            ((not @@use-chaton-mobile@@) #f)
            (mobile-mode?
              (html:a :href "@@httpd-url@@@@url-path@@?c=m&mode=chaton" "Chaton mode"))
            (else
              (html:a :href "@@httpd-url@@@@url-path@@?c=m&mode=mobile" "Mobile mode")))
          (if @@use-internal-search@@
            (html:a :href "@@httpd-url@@@@url-path@@s" "Search")
            (html:a :href "@@httpd-url@@@@url-path@@search.html" "Search"))
          (if @@dont-use-badge@@
            #f
            (html:a :href "@@httpd-url@@@@url-path@@badge.html" "Badge"))
          (quote
            @@extra-menu-html-list/escd@@))))))

(define *max-age-epoch*
  (* 10 365 24 60 60))

(define (make-cookie-args :optional (max-age-epoch *max-age-epoch*))
  `(
    :path "@@cookie-path@@"
    :max-age ,max-age-epoch
    :expires ,(if (zero? max-age-epoch)
                0
                (+ (sys-time) max-age-epoch))
    ))

;; This feature should be built-in!

(define (with-output-to-file file thunk . args)
  (if-let1 atomic (get-keyword :atomic args #f)
    (let1 tmp #`",|file|.tmp"
      (guard (e [else (sys-unlink tmp) (raise e)])
        (apply (with-module gauche with-output-to-file)
               tmp thunk (delete-keyword :atomic args))
        (sys-rename tmp file)))
    (apply (with-module gauche with-output-to-file) file thunk args)))
