#lang info
(define collection "rpaste")
(define version "0.1.2")
(define deps
  '("base"
    "db-lib"
    "web-server-lib"
    "mime-type-lib"
    "rackunit-lib"
    "rackunit-typed"
    "typed-racket-lib"
    "binaryio-lib"))
(define racket-launcher-names '("rpaste"))
(define racket-launcher-libraries '("main.rkt"))
