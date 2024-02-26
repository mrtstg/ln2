all: run

run: ../.env
	cp ../.env .
	stack run -- run -p 8002

create-db: ../.env
	cp ../.env .
	stack run -- create-db
