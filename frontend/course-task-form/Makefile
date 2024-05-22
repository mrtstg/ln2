all: build

build: vite.config.ts ../../static/js/ ../../static/css/
	(cd .. && yarn)
	yarn
	yarn build
	mv dist/index.js ../../static/js/courseTaskForm.js
	mv dist/index.css ../../static/css/courseTaskForm.css
