#lang racket/base

(require db
         openssl/sha1
         racket/function
         racket/match
         racket/contract
         "logger.rkt"
         "vendored/base58.rkt")

(provide (prefix-out db: (all-defined-out)))

(define/match (base58-conversion-exn? e)
  [((struct* exn ([message "Invalid character in base58 string"])))
   #t]
  [(_) #f])

(define (paste-id->sha1 id)
  (if (= 40 (string-length id))         ; Sha1, leave it be
      id
      (with-handlers ([base58-conversion-exn? (const #f)])
        (bytes->hex-string
         (base58-decode id)))))

(define (sha1->base58 id)
  (base58-encode (hex-string->bytes id)))

(define schema-version 2)
(define db-conn (make-parameter #f))

(define/contract (get-paste id)
  (string? . -> . (or/c #f string?))
  (define canonical-id (paste-id->sha1 id))
  (and canonical-id
       (query-maybe-value (db-conn)
                          #<<END
SELECT paste
FROM Pastes
WHERE key = $1
END
                          canonical-id)))

(define (create-paste paste)
  (define id (sha1 (open-input-string paste)))
  (query-exec (db-conn)
               #<<END
INSERT INTO Pastes (key, paste)
            VALUES ($1, $2)
       ON CONFLICT DO NOTHING
END
               id paste
               )
  (sha1->base58 id))

(define (recent-pastes)
  (define rows
    (query-rows (db-conn)
                          #<<END
SELECT key, timestamp
FROM Pastes
ORDER BY timestamp DESC
LIMIT 10
END
                          ))
  (for/list ([r rows])
    (match-define (vector key timestamp) r)
    (vector (sha1->base58 key) timestamp)))

(define (setup-connection)
  (db-conn
   (virtual-connection
    (connection-pool
     (thunk (postgresql-connect
             #:user (getenv "POSTGRES_USER")
             #:port (match (getenv "POSTGRES_PORT")
                      [#f 5432]
                      [s (string->number s)])
             #:database (getenv "POSTGRES_DB")
             #:server (getenv "POSTGRES_HOST")
             #:password (getenv "POSTGRES_PASSWORD")
             #:ssl 'optional)))))
  (log-rpaste-info "Creating schema...")
  (query-exec (db-conn) #<<END
CREATE TABLE IF NOT EXISTS Pastes
  ("id" SERIAL,
   "key" TEXT NOT NULL  UNIQUE,
   "paste" TEXT NOT NULL,
   "timestamp" TIMESTAMPTZ NOT NULL DEFAULT now()::timestamp)
END
              )
  (query-exec (db-conn) #<<END
CREATE TABLE IF NOT EXISTS Metadata
  ("key" varchar (20) PRIMARY KEY,
   "value" TEXT)
END
              )
  (query-exec (db-conn) #<<END
INSERT INTO Metadata (key, value)
              VALUES ('version', '001')
       ON CONFLICT DO NOTHING
END
              )
  )
