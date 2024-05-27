all: run

run: ../.env
	cp ../.env .
	stack run -- run -p 8005

build-image: deployment/Dockerfile
	docker build -t ln2-db-api -f deployment/Dockerfile .
