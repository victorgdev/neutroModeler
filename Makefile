run:
	elm make src/Main.elm --output elm.js

live:
	elm-live src/Main.elm --open -- --output elm.js