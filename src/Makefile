
Main: *.hs **/*.hs
	ghc --make Main.hs -o Main -threaded \
		-fforce-recomp -dcore-lint -odir dist/ -hidir dist/

run: Main
	./Main "../"

notes:
	pandoc \
		--from=markdown \
		--to=html \
		-o ../presentations/sunroof-tfp13/sunroof-tfp13.html \
		../presentations/sunroof-tfp13/sunroof-tfp13.md
	pandoc \
		--from=markdown \
		--to=html \
		-o ../presentations/hermit-tfp13/hermit-tfp13.html \
		../presentations/hermit-tfp13/hermit-tfp13.md

clean:
	rm -f Main
	rm -f ../presentations/sunroof-tfp13/sunroof-tfp13.html
	rm -f ../presentations/hermit-tfp13/hermit-tfp13.html
