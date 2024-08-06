FROM haskell:9.4.7-buster AS builder
WORKDIR /proj1
COPY docker-master-server/*.cabal docker-master-server/*.yaml docker-master-server/stack.yaml.lock ./
RUN stack install --only-dependencies
WORKDIR /proj2
COPY auth-service/*.cabal auth-service/*.yaml auth-service/stack.yaml.lock ./
RUN stack install --only-dependencies
WORKDIR /proj3
COPY main-site/*.cabal main-site/*.yaml main-site/stack.yaml.lock ./
RUN stack install --only-dependencies
WORKDIR /proj4
COPY md-render-api/*.cabal md-render-api/*.yaml md-render-api/stack.yaml.lock ./
RUN stack install --only-dependencies
WORKDIR /proj5
COPY db-gen-api/*.cabal db-gen-api/*.yaml db-gen-api/stack.yaml.lock ./
RUN stack install --only-dependencies
WORKDIR /proj6
COPY proxmox-deploy-api/*.cabal proxmox-deploy-api/*.yaml proxmox-deploy-api/stack.yaml.lock ./
RUN stack install --only-dependencies
