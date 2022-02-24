#lang racket/base

(require racket/function
         racket/port
         (for-syntax racket/base racket/format)
         web-server/http)

(define-syntax (define-http-helpers stx)
  (syntax-case stx ()
    [(_ [code default-message])
     (with-syntax ([binding (datum->syntax stx (string->symbol (format "response/~a" (syntax-e #'code))))]
                   [default-message (datum->syntax stx (datum->syntax stx (string->bytes/utf-8 (~a (syntax-e #'message)))))])
       #'(begin
           (define (binding #:message [message default-message]
                            #:mime-type [mime-type #"text/plain; charset=utf-8"]
                            body)
             (response/output #:code code
                              #:message message
                              #:mime-type mime-type
                              (cond
                                [(string? body)
                                 (curry write-string body)]
                                [(bytes? body)
                                 (curry write-bytes body)]
                                [(and (procedure? body) (procedure-arity-includes? body 0))
                                 (λ (op)
                                   (parameterize ([current-output-port op])
                                     (body)))]
                                [(and (procedure? body) (procedure-arity-includes? body 1))
                                 body]
                                [(input-port? body)
                                 (curry copy-port body)]
                                [else
                                 (raise-type-error 'body "A string, bytes, or procedure with zero or one arguments")])))
           (provide binding)))]
    [(_ [code0 message0] [code* message*] ...)
     #'(begin
         (define-http-helpers [code0 message0])
         (define-http-helpers [code* message*] ...))]))

(define-http-helpers
  [200 "Okay"]
  [400 "Bad Request"]
  [404 "Not Found"])
