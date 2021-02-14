all: src/Main.elm
	elm make --optimize src/Main.elm --output elm.js
	killall python3 || echo "first start"
	python3 -m http.server &
