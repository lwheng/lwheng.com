elmjs=elm.js
elmminjs=elm.min.js

# `make` will run the first target as default
all: create_dir format build uglify move_artefacts

debug: create_dir format build_debug uglify move_artefacts

# <target2>: <target1>
# <target2> depends on <target1>
# i.e. <target1> runs first
create_dir: clean
	@mkdir -p release/js

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
	@cp index.html release/index.html
	@cp 404.html release/404.html
	@mv ${elmjs} release/js/${elmjs}
	@mv ${elmminjs} release/js/${elmminjs}

local: debug
	cd release && python -m SimpleHTTPServer 8000

clean:
	@echo "Cleaning up build artefacts..."
	@rm -rf release
	@echo "...Done!"
