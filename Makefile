test: build run

build: Main.hs
	ghc Main.hs -o puck

run: puck
	./puck
