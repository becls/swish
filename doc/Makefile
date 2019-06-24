pdf :=\
  c-coding-standard.pdf\
  scheme-coding-standard.pdf\
  swish.pdf\

.SUFFIXES:

.PHONY: all
all: $(pdf)

%.pdf: %.tex sagian.sty swish.sty reference.bib
	./run-latex $*

%.pdf: %.dot
	dot -Tpdf -o $@ $<

swish :=\
  swish/*.tex\
  swish/event-mgr-events.pdf\
  swish/http-tree.pdf\
  swish/intro-sup-tree.pdf\

swish.pdf: $(swish)

.PHONY: clean
clean:
	rm -f $(pdf) *.aux *.bbl *.blg *.lof *.log *.lot *.out *.idx *.ind *.ilg *.toc *.thm swish/*.aux
	find . -iname "*.dot" | sed "s/\.dot$$/\.pdf/" | xargs rm -f
