all: run

run: ../.env
	cp ../.env .
	stack run -- run -p 8003

create-db: ../.env
	cp ../.env .
	stack run -- create-db
