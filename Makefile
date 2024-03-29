COMPOSE_BIN=docker compose
BASE_COMPOSE_COMMAND=$(COMPOSE_BIN) --project-name ln2
COMPOSE_FILE=deployment/docker-compose.yml

procfile: Procfile
	procodile start --dev

deploy-dev: $(COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(COMPOSE_FILE) --profile dev up -d

stop-dev: $(COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(COMPOSE_FILE) --profile dev stop

start-dev: $(COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(COMPOSE_FILE) --profile dev start

destroy-dev: $(COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(COMPOSE_FILE) --profile dev down
