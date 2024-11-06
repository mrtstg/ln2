FROM ln2-haskell AS builder
WORKDIR /usr/src/app
COPY proxmox-fs-agent.cabal package.yaml stack.yaml stack.yaml.lock ./
RUN stack install --only-dependencies
COPY . .
RUN stack install

FROM scratch
COPY --from=builder /root/.local/bin/proxmox-fs-agent-exe /proxmox-fs-agent
ENTRYPOINT ["/proxmox-fs-agent"]
