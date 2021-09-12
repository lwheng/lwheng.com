js="js/elm.js"
min="js/elm.min.js"

# `make` will run the first target as default
all: create_dir format build uglify

debug: create_dir format build_debug uglify

# <target2>: <target1>
# <target2> depends on <target1>
# i.e. <target1> runs first
create_dir: clean
	@mkdir js

format:
	@elm-format --yes src

build:
	@elm make --optimize --output=${js} src/Main.elm

build_debug:
	@elm make --output=${js} src/Main.elm

uglify:
	@echo "Compressing ${js} to ${min}..."
	@uglifyjs ${js} --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output ${min}
	@echo "...Done!"

local: debug
	python -m SimpleHTTPServer 8000

clean:
	@echo "Cleaning up the 'js' folder..."
	@rm -rf js
	@echo "...Done!"
