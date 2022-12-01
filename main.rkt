#lang racket/base

(require db
         net/mime-type
         net/url
         racket/bool
         racket/cmdline
         racket/date
         racket/exn
         racket/format
         racket/function
         racket/list
         racket/logging
         racket/match
         racket/math
         racket/port
         racket/string
         web-server/dispatch
         web-server/http
         web-server/safety-limits
         web-server/servlet-env
         web-server/templates
         "data.rkt"
         "helpers.rkt"
         "logger.rkt")

(define site-title "rpaste")

(define-values (dispatch rev-url)
  (dispatch-rules
   [("") #:method "get" list-pastes]
   [("") #:method "post" make-paste]
   [("form") #:method "get" paste-form]
   [((string-arg)) #:method "get" show-paste]))

(define ((headize start) req)
  (match (request-method req)
    [#"HEAD"
     (define get-response
       (start (struct-copy request req [method #"GET"])))
     (struct-copy response get-response
                  [output void])]
    [_ (start req)]))

(define ((log-request start) req)
  (define res (start req))
  (log-rpaste-info "~a ~a -> ~a"
                   (request-method req)
                   (url->string (request-uri req))
                   (response-code res))
  res)

;; XXX is there a better way to do this?  This feels not very user friendly.
(define (sql-timestamp->string sql-ts)
  (match-define (struct sql-timestamp (year month day hour minute second nanosecond tz))
    sql-ts)
  (define (f v)
    (~r v #:min-width 2 #:pad-string "0"))
  (format "~a-~a-~a ~a:~a" (f year) month day (f hour) (f minute)))

(define (epoch->rfc2822 epoch)
  (parameterize ([date-display-format 'rfc2822])
    (date->string (seconds->date epoch #f) #t)))

;;; TODO clean this up.  Note to self, always use headers-assq*, it is the
;;; case-insensitive version.  Header names are case insensitive.
(define (site-baseurl req)
  (define h (request-headers/raw req))
  (define s
    (cond
      [(headers-assq* #"Fly-Forwarded-Proto" h)
       =>
       (λ (p)
         (define proto (header-value p))
         (format "~a://~a~a/"
                 (bytes->string/utf-8 proto)
                 (bytes->string/utf-8 (header-value (headers-assq* #"Host" h)))
                 (match* (proto (header-value (headers-assq* #"Fly-Forwarded-Port" h)))
                   [(#"https" #"443") ""]
                   [(#"http" #"80") ""]
                   [(_ (app bytes->string/utf-8 port)) (string-append ":" port)])))]
      [(headers-assq* #"Forwarded" h) => (compose1 bytes->string/utf-8 header-value)]
      [(headers-assq* #"X-Forwarded-For" h) => (compose1 bytes->string/utf-8 header-value)]
      [(getenv "APP_URL") => identity]
      [(headers-assq* #"Host" h) => (compose1 bytes->string/utf-8 header-value)]
      [else (format "~a:~a" (request-host-ip req) (request-host-port req))]))
  (match (string-append (string-trim s "/" #:repeat? #t) "/") 
    [(regexp "https?://.*" (list m)) m]
    [no-scheme (string-append "http://" no-scheme)]))

(define (list-pastes req)
  (define rows (db:recent-pastes))
  (define address (site-baseurl req))
  (response/200
   #:mime-type TEXT/HTML-MIME-TYPE
   (include-template "templates/homepage.html")))

(define (paste-form req)
  (response/200
   #:mime-type TEXT/HTML-MIME-TYPE
   (include-template "templates/form.html")))

(define (show-paste req id)
  (match (db:get-paste id)
    [#f
     (response/404 (format "No paste with key [~a] :(\n" id))]
    [paste
     (response/200 paste)]))

(define (make-paste req)
  (let/ec escape
    (define raw (request-bindings/raw req))
    (define pf (implies raw (bindings-assq #"p" raw)))
    (unless (binding:form? pf)
      (escape
       (response/400 "Need payload p=...\n")))
    (define data (bytes->string/utf-8 (binding:form-value pf)))
    (unless (non-empty-string? data)
      (escape
       (response/400 "Payload p=... must have a non-zero length.\n")))
    (define hash (db:create-paste data))
    (if (and raw (bindings-assq #"redirect" raw))
        (redirect-to (format "/~a" hash))
        (response/200 (format "~a~a\n" (site-baseurl req) hash)))))

(define (not-found req)
  (response/404 "Not found\n"))

(define (fun)
  (define listen-port (make-parameter 8080))
  (define listen-ip (make-parameter #f))
  (command-line
   #:once-each
   [("-p" "--port") port-string "Port to listen on"
                    (listen-port (string->number port-string))]
   [("--ip") ip-string "IP to listen on"
             (listen-ip ip-string)])
  (db:setup-connection)
  (log-rpaste-info "Connected and schema created.  Visit http://~a:~a/" (or (listen-ip) "0.0.0.0") (listen-port))
  (define max-waiting 511)
  (define safety-limits
    (make-safety-limits #:max-waiting max-waiting
                        #:max-form-data-field-length (sqr 1024)))
  (serve/servlet (log-request (headize dispatch))
                 #:stateless? #t
                 #:listen-ip (listen-ip)
                 #:port (listen-port)
                 #:servlet-path "/"
                 #:servlet-regexp #rx""
                 #:command-line? #t
                 #:server-root-path "."
                 #:safety-limits safety-limits
                 #:file-not-found-responder (log-request (headize not-found))))

(module+ main
  (with-logging-to-port (current-error-port)
    fun
    #:logger rpaste-logger 'debug 'rpaste))
