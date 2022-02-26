#|

This is an vendored version of the base58 package.  It does not depend on
racket-doc.  It also removes the #:check? keyword argument, because the "sha"
package pulls in racket-doc as well.

https://github.com/marckn0x/base58

BSD-2-Clause license.

|#

#lang typed/racket


(require/typed
 binaryio
 [integer->bytes (-> Integer Nonnegative-Integer Boolean Boolean Bytes)]
 [bytes->integer (-> Bytes #f Boolean Nonnegative-Integer)]
 [integer-bytes-length (-> Nonnegative-Integer #f Nonnegative-Integer)])

(provide base58-encode
         base58-decode)

(define alphabet
  (for/hash : (Immutable-HashTable Char Nonnegative-Integer)
    ([c "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"]
     [i : Nonnegative-Integer (in-naturals)])
    (values c i)))

(define backward-alphabet
  (for/hash : (Immutable-HashTable Integer Char)
    ([(k v) alphabet])
    (values v k)))

(: base58-decode (-> String Bytes))
(define (base58-decode str)
  (define-values (bigval leading-zeroes)
    (for/fold ([n : Nonnegative-Integer 0]
               [l : Nonnegative-Integer 0])
              ([c : Char str])
      (define m (hash-ref alphabet c (thunk (error "Invalid character in base58 string"))))
      (if (and (= n 0) (= m 0))
          (values n (add1 l))
          (values (+ (* n 58) m) l))))

  (if (> bigval 0)
      (integer->bytes bigval (+ (integer-bytes-length bigval #f) leading-zeroes) #f #t)
      (make-bytes leading-zeroes 0)))

(: base58-encode (-> Bytes String))
(define (base58-encode bs)
  (define bs-maybe-check bs)

  (: encode-nonzero (-> Bytes (Listof Char)))
  (define (encode-nonzero bs)
    (reverse
     (let loop : (Listof Char)
       ([n : Nonnegative-Integer (bytes->integer bs #f #t)])
       (if (= n 0)
           empty
           (cons
            (hash-ref backward-alphabet (remainder n 58))
            (loop (quotient n 58)))))))

  (define-values (ones idx-first-nonzero)
    (for/fold ([ones : (Listof Char) empty]
               [idx-first-nonzero : Nonnegative-Integer 0])
              ([b bs-maybe-check])
      #:break (> b 0)
      (values (cons #\1 ones) (add1 idx-first-nonzero))))

  (list->string
   (append
    ones
    (if (< idx-first-nonzero (bytes-length bs-maybe-check))
        (encode-nonzero (subbytes bs-maybe-check idx-first-nonzero))
        empty))))

(module+ test
  (require typed/rackunit)

  (define tqbf #"The quick brown fox jumps over the lazy dog.")
  (define strs
    `(,tqbf
      #""
      #"\230F3x\316\323U\24\373[B\301\204\243#!\347\342Bl"
      #"\0\0\0\0\0"
      #"\0\0\0asdf"))
  (for* ([s strs])
    (check-equal? (base58-decode (base58-encode s)) s))

  (check-equal? (base58-decode "2NEpo7TZRRrLZSi2U") #"Hello World!")
  (check-equal? (base58-encode tqbf) "USm3fpXnKG5EUBx2ndxBDMPVciP5hGey2Jh4NDv6gmeo1LkMeiKrLJUUBk6Z")

  (define fixed #"\200\355\333\334\21h\361\332\352\333\323\344L\36?\217Z(L )\367\212\322j\371\205\203\244\231\336[\31")
  (define fixed-b58 "5Kd3NBUAdUnhyzenEwVLy9pBKxSwXvE9FMPyR4UKZvpe6E3AgLr")

  (check-equal? (base58-decode fixed-b58) fixed)
  (check-equal? (base58-encode fixed) fixed-b58))
