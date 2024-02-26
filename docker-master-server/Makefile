all: run

run: ../.env
	cp ../.env .
	stack run -- run -p 8001

create-db: ../.env
	cp ../.env .
	stack run -- create-db

build-image: deployment/Dockerfile
	docker build -t ln2-master -f deployment/Dockerfile .
