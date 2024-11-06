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

build-bin-docker: deployment/ deployment/Dockerfile ../docker.env deployment/proxmox-fs-agent.service.template
	cp ../docker.env .env
	envsubst < deployment/proxmox-fs-agent.service.template > deployment/proxmox-fs-agent.service
	docker build -f deployment/Dockerfile --output=./deployment/ .

deployment/:
	mkdir deployment

configs/:
	mkdir configs
