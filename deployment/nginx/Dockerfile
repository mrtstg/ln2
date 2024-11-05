FROM nginx:stable-alpine
COPY deployment/nginx/nginx.conf /etc/nginx/nginx.conf
COPY deployment/nginx/prod.conf /etc/nginx/conf.d/site.conf
COPY static /static
