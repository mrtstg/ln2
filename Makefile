HS_SERVICES=auth-service db-gen-api docker-master-server main-site md-render-api proxmox-fs-agent proxmox-deploy-api proxmox-deploy-agent
IMAGES_LIST=ln2-main-site ln2-master ln2-auth ln2-agent ln2-md-api ln2-db-api ln2-proxmox-deploy-api ln2-proxmox-deploy-agent
FRONTEND_PROJECTS=admin-course-form admin-users-form course-task-form users-solves-form course-create-form course-task-edit-form course-members-form novnc-connector deployments-page
COMPOSE_BIN=docker compose
ENV_FILE=.env
DOCKER_ENV_FILE=docker.env
BASE_COMPOSE_COMMAND=$(COMPOSE_BIN) --env-file $(DOCKER_ENV_FILE) --project-name ln2
COMPOSE_FILE=deployment/docker-compose.yml
PROCFILE_DIR=procfiles

build-websockify: ./deployment/websockify/Dockerfile
	docker build -t ln2-websockify -f ./deployment/websockify/Dockerfile .

build-frontend: ./static/js ./static/css
	make -C frontend build
	@for n in $(FRONTEND_PROJECTS); do \
		(make -C frontend/$$n build); \
	done

build-bin: $(HS_SERVICES)
	@for n in $(HS_SERVICES); do \
		(cd $$n && echo "Building $$n" && stack build); \
	done

install-deps: $(HS_SERVICES)
	@for n in $(HS_SERVICES); do \
		(cd $$n && stack install --only-dependencies); \
	done

build-images:
	make -C auth-service build-image
	make -C db-gen-api build-image
	make -C docker-master-server build-image
	make -C docker-secondary-agent build-image
	make -C main-site build-image
	make -C md-render-api build-image
	make -C proxmox-deploy-api build-image
	make -C proxmox-deploy-agent build-image

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

procfile: $(PROCFILE_DIR)/main
	foreman start -f $(PROCFILE_DIR)/main -d .

procfile-docker: $(PROCFILE_DIR)/docker
	foreman start -f $(PROCFILE_DIR)/docker -d .

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
