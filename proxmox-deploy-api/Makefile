all: run

run: ../.env
	cp ../.env .
	stack run -- run -p 8007

create-db: ../.env
	cp ../.env .
	stack run -- create-db

build-image: deployment/Dockerfile
	docker build -t ln2-proxmox-deploy-api -f deployment/Dockerfile .
