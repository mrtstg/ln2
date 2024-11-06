FROM node:lts-slim AS builder
WORKDIR /project
RUN ["apt-get", "update"]
RUN ["apt-get", "install", "make", "-y"]
RUN ["mkdir", "-p", "static/js"]
RUN ["mkdir", "-p", "static/css"]
COPY . .
RUN ["make", "build-frontend"]

FROM nginx:stable-alpine
COPY deployment/nginx/nginx.conf /etc/nginx/nginx.conf
COPY deployment/nginx/prod.conf /etc/nginx/conf.d/site.conf
COPY static /static
COPY --from=builder /project/static/css/* /static/css/
COPY --from=builder /project/static/js/* /static/js/
