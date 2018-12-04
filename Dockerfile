FROM jackfirth/racket:7.0

WORKDIR /app

COPY . /app/

RUN raco pkg install --auto --link

CMD ["racket", "-l", "rpaste", "-m", "--", "-p", "8080", "-d", "/db/db.sqlite3"]
