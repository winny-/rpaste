#!/bin/sh
# set -x
exec racket -l rpaste -- -p 8080 -d /db/db.sqlite3
