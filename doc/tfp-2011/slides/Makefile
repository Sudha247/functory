slides-pp.pdf: slides-pp.tex state.mps
	pdflatex slides-pp
	grep -iw frametitle slides.tex | wc -l

state.mps: figures.ml
	mlpost -latex slides.tex figures.ml

%-pp.tex: %.tex
	latexpp $^ -o $@
clean: 
	rm -f *.dvi *.aux *.log *.pdf

