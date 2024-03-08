.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: test
test: test-elm test-haskell test-javascript


.PHONY: test-elm
test-elm:
	@printf "\n\n========== ELM TESTS ==========\n\n"
	cd Elm && npx elm-test


.PHONY: test-haskell
test-haskell:
	@printf "\n\n========== HASKELL TESTS ==========\n\n"
	stack runhaskell Haskell/Test.hs


.PHONY: test-javascript
test-javascript:
	@printf "\n\n========== JAVASCRIPT TESTS ==========\n\n"
	bun run JavaScript/test.ts
