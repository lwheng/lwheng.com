elmjs := elm.js
elmminjs := elm.min.js

secret_GCS_HOST := $(shell cat ~/.secret/secret_GCS_HOST)
secret_GCS_API_KEY := $(shell cat ~/.secret/secret_GCS_API_KEY)

# `make` will run the first target as default
all: create_dir format build uglify move_artefacts

debug: create_dir format build_debug uglify setup_index_local move_artefacts

# <target2>: <target1>
# <target2> depends on <target1>
# i.e. <target1> runs first
create_dir: clean
	@mkdir -p release/js
	@mkdir -p release/img

format:
	@elm-format --yes src

build:
	@elm make --optimize --output=${elmjs} src/Main.elm

build_debug:
	@elm make --output=${elmjs} src/Main.elm

uglify:
	@echo "Compressing ${elmjs} to ${elmminjs}..."
	@uglifyjs ${elmjs} --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output ${elmminjs}
	@echo "...Done!"

move_artefacts:
	@mv index.html release/index.html
	@cp templates/404.html.template release/404.html
	@mv ${elmjs} release/js/${elmjs}
	@mv ${elmminjs} release/js/${elmminjs}
	@cp src/img/* release/img/.

local: debug
	@cd release && python -m SimpleHTTPServer 8000

setup_index_local:
	@sed 's/SECRETS_GCS_HOST/${secret_GCS_HOST}/; s/SECRETS_GCS_API_KEY/${secret_GCS_API_KEY}/' templates/index.html.template > index.html

clean:
	@echo "Cleaning up build artefacts..."
	@rm -rf release/*
	@echo "...Done!"
