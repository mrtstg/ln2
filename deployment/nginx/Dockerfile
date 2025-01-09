FROM node:lts-slim AS builder
WORKDIR /project
RUN ["apt-get", "update"]
RUN ["apt-get", "install", "make", "wget", "-y"]
RUN ["mkdir", "-p", "static/js"]
RUN ["mkdir", "-p", "static/css"]
COPY . .
RUN ["make", "download-bulma"]
RUN ["make", "build-frontend"]

FROM nginx:stable-alpine
ARG config=deployment/nginx/prod.conf
COPY deployment/nginx/nginx.conf /etc/nginx/nginx.conf
COPY $config /etc/nginx/conf.d/site.conf
COPY static /static
COPY ca /etc/ssl/ca/
COPY --from=builder /project/static/css/* /static/css/
COPY --from=builder /project/static/js/* /static/js/
