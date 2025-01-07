FROM alpine/openssl AS builder
ARG domain=example.com
WORKDIR /ca
RUN openssl genrsa 2048 > ca-key.pem
RUN openssl req -new -x509 -nodes -days 365000 \
    -subj "/C=RU/O=mrtstg Inc." \
    -key ca-key.pem \
    -out ca-crt.pem
RUN openssl req -newkey rsa:2048 -nodes -days 365000 \
    -subj "/CN=$domain" \
    -keyout server-key.pem \
    -out server-req.pem
RUN openssl x509 -req -days 365000 -set_serial 01 \
    -in server-req.pem \
    -out server-crt.pem \
    -CA ca-crt.pem \
    -CAkey ca-key.pem

FROM scratch
COPY --from=builder /ca/ca-key.pem /ca/ca-crt.pem /ca/server-key.pem /ca/server-crt.pem /
ENTRYPOINT ["/"]
