#!/bin/bash
cert_subject=""
read -p "Введите Subject сертификата: " cert_subject
docker build --no-cache --build-arg domain="$cert_subject" -f ./deployment/ssl/Dockerfile-crt --output=./ca .
