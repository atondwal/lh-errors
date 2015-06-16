LIQUID=liquid

all: 
	for file in *.hs; do $(LIQUID) $$file; done

