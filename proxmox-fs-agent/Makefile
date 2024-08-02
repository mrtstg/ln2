ifneq (,$(wildcard ../.env))
    include .env
    export
endif

all: run

run: ../.env configs/
	cp ../.env .
	stack run -- run -p 8006 -c configs/

build-bin: deployment/ deployment/proxmox-fs-agent.service.template
	envsubst < deployment/proxmox-fs-agent.service.template > deployment/proxmox-fs-agent.service
	stack install --local-bin-path deployment/ --copy-bins
	mv deployment/proxmox-fs-agent-exe deployment/proxmox-fs-agent

deployment/:
	mkdir deployment

configs/:
	mkdir configs
