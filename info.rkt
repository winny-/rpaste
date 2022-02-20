#lang info
(define collection "rpaste")
(define version "0.1.0")
(define deps
  '("base"
    "db-lib"
    "web-server-lib"
    "mime-type-lib"))
(define racket-launcher-names '("rpaste"))
(define racket-launcher-libraries '("main.rkt"))
