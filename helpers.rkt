#lang racket/base

(require racket/function
         racket/port
         (for-syntax racket/base racket/format)
         web-server/http)

(define-syntax (define-http-helpers stx)
  (syntax-case stx ()
    [(_ code)
     (with-syntax ([binding (datum->syntax stx (string->symbol (format "response/~a" (syntax-e #'code))))])
       #'(begin
           (define (binding #:message [message #f]
                            #:mime-type [mime-type #"text/plain; charset=utf-8"]
                            body)
             (define proc
               (cond
                [(string? body)
                 (curry write-string body)]
                [(bytes? body)
                 (curry write-bytes body)]
                [(and (procedure? body)
                      (procedure-arity-includes? body 0))
                 (λ (op)
                   (parameterize ([current-output-port op])
                     (body)))]
                [(and (procedure? body)
                      (procedure-arity-includes? body 1))
                 body]
                [(input-port? body)
                 (curry copy-port body)]
                [else
                 (raise-type-error 'body "A string, bytes, or procedure with zero or one arguments")]))
             ;; Work around issue in Racket 8.3 where the contract for #:message expects bytes? but not #f.
             ;; https://github.com/racket/web-server/commit/9fa8e1b622b99d6ec7c223244c5b12255e5980eb
             (if message
                 (response/output proc
                                  #:code code
                                  #:message message
                                  #:mime-type mime-type)
                 (response/output proc
                                  #:code code
                                  #:mime-type mime-type)))
           (provide binding)))]
    [(_ code0 code* ...)
     #'(begin
         (define-http-helpers code0)
         (define-http-helpers code* ...))]))

(define-http-helpers
  200
  400
  404)

(module+ test
  (require rackunit (submod ".."))
  (test-case "Smoketest response/200"
    (define res (response/200 "yay"))
    (check-equal? (response-code res)
                  200)
    (check-equal? (call-with-output-string (response-output res))
                  "yay")))
