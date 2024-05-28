IMAGES_LIST = ln2-main-site ln2-master ln2-auth ln2-agent ln2-md-api ln2-db-api
COMPOSE_BIN=docker compose
ENV_FILE=.env
BASE_COMPOSE_COMMAND=$(COMPOSE_BIN) --env-file $(ENV_FILE) --project-name ln2
COMPOSE_FILE=deployment/docker-compose.yml

build-frontend: ./static/js ./static/css
	make -C frontend/admin-course-form build
	make -C frontend/course-task-form build
	make -C frontend/course-create-form build
	make -C frontend/course-task-edit-form build
	make -C frontend/course-members-form build

save-images: ./images
	@for n in $(IMAGES_LIST); do \
		docker save $$n -o ./images/$$n.tar; \
	done

restore-images: ./images
	@for n in $(IMAGES_LIST); do \
		docker load -i ./images/$$n.tar; \
	done

./images:
	mkdir ./images -p

./static/js:
	mkdir ./static/js -p

./static/css:
	mkdir ./static/css -p

procfile: Procfile
	foreman start

build-lib-image: deployment/Dockerfile
	docker build -t ln2-haskell -f deployment/Dockerfile .

deploy-prod: $(COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(COMPOSE_FILE) --profile prod up -d

destroy-prod: $(COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(COMPOSE_FILE) --profile prod down

deploy-dev: $(COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(COMPOSE_FILE) --profile dev up -d

stop-dev: $(COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(COMPOSE_FILE) --profile dev stop

start-dev: $(COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(COMPOSE_FILE) --profile dev start

destroy-dev: $(COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(COMPOSE_FILE) --profile dev down

download-bulma: ./static/css/
	wget https://cdn.jsdelivr.net/npm/bulma@1.0.0/css/bulma.min.css -O ./static/css/bulma.min.css
