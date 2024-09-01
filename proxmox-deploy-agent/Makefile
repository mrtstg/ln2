all: run

run: ../.env
	cp ../.env .
	stack run -- run

build-image: deployment/Dockerfile
	docker build -t ln2-proxmox-deploy-agent -f deployment/Dockerfile .
