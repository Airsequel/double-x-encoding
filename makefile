.PHONY: test-elm
test-elm:
		cd Elm && elm make Test.elm --output=elm-tests.js


.PHONY: test-haskell
test-haskell:
		stack runhaskell Haskell/Test.hs


.PHONY: test-javascript
test-javascript:
		deno run JavaScript/test.ts


.PHONY: test
test: test-elm test-haskell test-javascript
