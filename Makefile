HS_SERVICES=auth-service db-gen-api docker-master-server main-site md-render-api proxmox-fs-agent proxmox-deploy-api proxmox-deploy-agent
IMAGES_LIST=ln2-main-site ln2-master ln2-auth ln2-agent ln2-md-api ln2-db-api ln2-proxmox-deploy-api ln2-proxmox-deploy-agent
FRONTEND_PROJECTS=admin-course-form admin-users-form course-task-form users-solves-form course-create-form course-task-edit-form course-members-form novnc-connector deployments-page
COMPOSE_BIN=docker compose
ENV_FILE=.env
DOCKER_ENV_FILE=docker.env
BASE_COMPOSE_COMMAND=$(COMPOSE_BIN) --env-file $(DOCKER_ENV_FILE) --project-name ln2
DEV_COMPOSE_FILE=deployment/docker-compose.yml
PROD_COMPOSE_FILE=deployment/prod.docker-compose.yml
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

procfile-agentless: $(PROCFILE_DIR)/agentless
	foreman start -f $(PROCFILE_DIR)/agentless -d .

build-lib-image: deployment/Dockerfile
	docker build -t ln2-haskell -f deployment/Dockerfile .

deploy-prod: $(PROD_COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(PROD_COMPOSE_FILE) up -d

destroy-prod: $(PROD_COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(PROD_COMPOSE_FILE) down

deploy-dev: $(DEV_COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(DEV_COMPOSE_FILE) up -d

stop-dev: $(DEV_COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(DEV_COMPOSE_FILE) stop

start-dev: $(DEV_COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(DEV_COMPOSE_FILE) start

destroy-dev: $(DEV_COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(DEV_COMPOSE_FILE) down

download-bulma: ./static/css/
	wget https://cdn.jsdelivr.net/npm/bulma@1.0.0/css/bulma.min.css -O ./static/css/bulma.min.css
