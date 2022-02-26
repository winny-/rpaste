FROM racket/racket:8.3

WORKDIR /app

COPY info.rkt .
RUN raco pkg install --auto --no-docs --name rpaste

COPY . .

RUN raco setup --no-docs rpaste

ENTRYPOINT /app/entrypoint.sh
