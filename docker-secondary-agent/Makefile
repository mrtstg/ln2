all: run

run: ../.env
	cp ../.env .
	cargo run

build-image: Dockerfile
	docker build -t ln2-agent .
