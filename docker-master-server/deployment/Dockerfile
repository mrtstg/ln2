FROM haskell:9.4.7-buster AS builder
WORKDIR /usr/src/app
COPY docker-master-server.cabal package.yaml stack.yaml stack.yaml.lock ./
RUN stack setup --install-ghc
RUN stack install --only-dependencies
COPY . .
RUN stack install

FROM debian:trixie-slim AS runner
ENV TZ="Europe/Moscow"
RUN apt-get update && apt-get install ca-certificates locales libpq-dev -y
ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8
WORKDIR /usr/src/app
COPY --from=builder /root/.local/bin/docker-master-server-exe /usr/src/app/master-server
CMD /usr/src/app/master-server run -p 8000
