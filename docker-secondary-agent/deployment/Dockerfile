FROM haskell:9.4.7-buster AS builder
WORKDIR /usr/src/app
COPY docker-secondary-agent.cabal package.yaml stack.yaml stack.yaml.lock ./
RUN stack setup --install-ghc
RUN stack install --only-dependencies
COPY . .
RUN stack install

FROM debian:trixie-slim AS runner
ENV TZ="Europe/Moscow"
RUN apt-get update && apt-get install ca-certificates locales -y
ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8
WORKDIR /usr/src/app
COPY --from=builder /root/.local/bin/docker-secondary-agent-exe /usr/src/app/agent-server
CMD /usr/src/app/agent-server run
