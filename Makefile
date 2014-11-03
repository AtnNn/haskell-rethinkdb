presentation.pdf: presentation.tex muni.png
	xelatex $<

presentation.tex: presentation.pandoc
	pandoc --template beamer.tex.template -t beamer $< --standalone \
	  | sed 's/\(DefineVerbatimEnvironment{Highlighting}.*\)}$$/\1,fontfamily=DejaVuSans-TLF}/' > $@

RenderMuni: RenderMuni.hs
	cabal exec -- ghc $<

muni.svg: RenderMuni
	./RenderMuni -w 600 -h 400 -o $@ 

muni.png: muni.svg
	convert $< $@
