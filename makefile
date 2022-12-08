.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: test
test: test-elm test-haskell test-javascript


.PHONY: test-elm
test-elm:
		cd Elm && elm make Test.elm --output=elm-tests.js


.PHONY: test-haskell
test-haskell:
		stack runhaskell Haskell/Test.hs


.PHONY: test-javascript
test-javascript:
		deno --unstable run JavaScript/test.ts
