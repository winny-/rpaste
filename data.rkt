#lang racket/base

(require db
         openssl/sha1
         racket/function
         racket/match
         "logger.rkt")

(provide (prefix-out db: (all-defined-out)))

(define schema-version 2)
(define db-conn (make-parameter #f))

(define (get-paste id)
  (query-maybe-value (db-conn)
                   #<<END
SELECT paste
FROM Pastes
WHERE key = $1
END
                   id))

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
  id)

(define (recent-pastes)
  (query-rows (db-conn)
              #<<END
SELECT key, timestamp
FROM Pastes
ORDER BY timestamp DESC
LIMIT 10
END
              ))

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
