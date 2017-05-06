all:
	elm-make --output=elm.js src/Main.elm --yes

clean:
	rm -rf elm-stuff/
