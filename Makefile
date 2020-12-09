run:
	elm make src/Main.elm --debug --output elm.js

live:
	elm-live src/Main.elm --open -- --debug --output elm.js