TARGET_DIR=../static/css/
TARGET_FILE=styles.css

build: tailwind.config.js styles/$(TARGET_FILE)
	npx tailwindcss -i styles/$(TARGET_FILE) -o $(TARGET_DIR)/$(TARGET_FILE)

watch: tailwind.config.js styles/$(TARGET_FILE)
	npx tailwindcss -i styles/$(TARGET_FILE) -o $(TARGET_DIR)/$(TARGET_FILE) --watch

../static/css:
	mkdir -p ../static/css
