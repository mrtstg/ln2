FROM haskell:9.4.7-buster AS builder
RUN apt-get update && apt-get install make -y
WORKDIR /ln2
COPY . .
RUN make install-deps
