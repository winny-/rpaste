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
         "logger.rkt")

(define-syntax-rule (send-output a ...)
  (λ (op)
    (parameterize ([current-output-port op])
      a ...)
    (void)))

(define site-title "rpaste")

(define-values (dispatch rev-url)
  (dispatch-rules
   [("") #:method "get" list-pastes]
   [("") #:method "post" make-paste]
   [("form") #:method "get" paste-form]
   [((string-arg)) #:method "get" show-paste]
   [("static") #:method "get" send-static]))

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
  res
  (log-rpaste-info "~a ~a -> ~a"
                   (request-method req)
                   (url->string (request-uri req))
                   (response-code res))
  res)

(define (send-static req)
  (define res #f)
  (with-handlers ([exn:fail:filesystem? (λ (ex) (set! res (mk-bad
                                                           (exn->string ex)
                                                           #:code 404
                                                           #:message #"Not Found")))])
    (define pth (apply build-path (map path/param-path (url-path (request-uri req)))))
    (define ip (open-input-file pth))
    (set! res (mk-gud (curry copy-port ip) #:mime (path-mime-type pth))))
  res)

(define (mk-bad txt #:code [code 404] #:message [msg #"Not found"] #:mime [mime #"text/plain"])
  (response/output (λ (op)
                     (write-string txt op)
                     (void))
                   #:code code
                   #:message msg
                   #:mime-type mime))

(define (mk-gud fn #:mime [mime #"text/plain; charset=utf-8"])
  (response/output fn #:mime-type mime))

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

(define (site-baseurl req)
  (define h (request-headers/raw req))
  (define s
    (cond
      [(headers-assq #"Forwarded" h) => (compose1 bytes->string/utf-8 header-value)]
      [(headers-assq #"X-Forwarded-For" h) => (compose1 bytes->string/utf-8 header-value)]
      [(getenv "APP_URL") => identity]
      [(headers-assq #"Host" h) => (compose1 bytes->string/utf-8 header-value)]
      [else (format "~a:~a" (request-host-ip req) (request-host-port req))]))
  (match (string-append (string-trim s "/" #:repeat? #t) "/") 
    [(regexp "https?://.*" (list m)) m]
    [no-scheme (string-append "http://" no-scheme)]))

(define (list-pastes req)
  (define rows (db:recent-pastes))
  (define address (site-baseurl req))
  (response/full
   200 #"Okay"
   (current-seconds) TEXT/HTML-MIME-TYPE
   empty
   (list (string->bytes/utf-8 (include-template "templates/homepage.html")))))

(define (paste-form req)
  (response/full
   200 #"Okay"
   (current-seconds) TEXT/HTML-MIME-TYPE
   empty
   (list (string->bytes/utf-8 (include-template "templates/form.html")))))

(define (show-paste req id)
      (match (db:get-paste id)
        [#f
         (mk-bad (format "No paste with key [~a] :(" id))]
        [paste
         (mk-gud (send-output (write-string paste)))]))

(define (make-paste req)
  (define raw (request-bindings/raw req))
  (define pf (implies raw (bindings-assq #"p" raw)))
  (if (binding:form? pf)
      (let* ([data (bytes->string/utf-8 (binding:form-value pf))]
             [hash (db:create-paste data)])
        (if (and raw (bindings-assq #"redirect" raw))
            (redirect-to (format "/~a" hash))
            (mk-gud (send-output (printf "~a~a\n" (site-baseurl req) hash)))))
      (response/output (send-output (write-string "Bad request. Need payload p=..."))
                       #:code 400
                       #:message #"Bad request")))

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
                 #:safety-limits safety-limits))

(module+ main
  (with-logging-to-port (current-error-port)
    fun
    #:logger rpaste-logger 'debug 'rpaste))
