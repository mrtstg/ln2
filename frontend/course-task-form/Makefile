all: build

build: vite.config.ts ../../static/js/ ../../static/css/
	(cd .. && yarn)
	yarn
	yarn build
	mv dist/index.js ../../static/js/courseTaskForm.js
