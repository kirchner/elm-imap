all:
	elm-make --output=gh-pages/elm.js src/Main.elm --yes
	cp static/index.html gh-pages/index.html

clean:
	rm -rf elm-stuff/
