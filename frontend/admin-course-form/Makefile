all: build

build: vite.config.ts ../../static/js/
	(cd .. && yarn)
	yarn
	yarn build
	mv dist/index.js ../../static/js/adminCourseForm.js
	mv dist/index.css ../../static/css/courseTaskCreateForm.css
