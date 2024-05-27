all: run

run: ../.env
	cp ../.env .
	stack run -- run -p 8004

build-image: deployment/Dockerfile
	docker build -t ln2-md-api -f deployment/Dockerfile .
