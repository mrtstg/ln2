COMPOSE_BIN=docker compose
ENV_FILE=.env
BASE_COMPOSE_COMMAND=$(COMPOSE_BIN) --env-file $(ENV_FILE) --project-name ln2
COMPOSE_FILE=deployment/docker-compose.yml

build-frontend: ./static/js
	make -C frontend/admin-course-form build
	make -C frontend/course-task-form build
	make -C frontend/course-create-form build
	make -C frontend/course-task-edit-form build
	make -C frontend/course-members-form build

./static/js:
	mkdir ./static/js -p

procfile: Procfile
	foreman start

deploy-dev: $(COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(COMPOSE_FILE) --profile dev up -d

stop-dev: $(COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(COMPOSE_FILE) --profile dev stop

start-dev: $(COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(COMPOSE_FILE) --profile dev start

destroy-dev: $(COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(COMPOSE_FILE) --profile dev down
