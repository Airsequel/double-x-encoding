.PHONY: test
test:
		deno run JavaScript/test.ts
		stack runhaskell Haskell/Test.hs
