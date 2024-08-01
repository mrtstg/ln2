all: run

run: ../.env configs/
	cp ../.env .
	stack run -- run -p 8006 -c configs/

build-bin: deployment/
	stack install --local-bin-path deployment/ --copy-bins
	mv deployment/proxmox-fs-agent-exe deployment/proxmox-fs-agent
	
deployment/:
	mkdir deployment

configs/:
	mkdir configs
