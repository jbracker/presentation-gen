
PRES_GEN_BIN = ./.cabal-sandbox/bin/presentation-gen
PRESENTATION_DIR = ./presentations

run:
	$(PRES_GEN_BIN) "./"

notes:
	pandoc \
		--from=markdown \
		--to=html \
		-o $(PRESENTATION_DIR)/sunroof-tfp13/sunroof-tfp13.html \
		$(PRESENTATION_DIR)/sunroof-tfp13/sunroof-tfp13.md
	pandoc \
		--from=markdown \
		--to=html \
		-o $(PRESENTATION_DIR)/hermit-tfp13/hermit-tfp13.html \
		$(PRESENTATION_DIR)/hermit-tfp13/hermit-tfp13.md

clean:
	rm -f ./sunroof-tfp13.html
	rm -f ./hermit-tfp13.html
