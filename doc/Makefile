pdf :=\
  c-coding-standard.pdf\
  scheme-coding-standard.pdf\
  swish.pdf\

.SUFFIXES:

.PHONY: all
all: $(pdf)

%.pdf: %.tex sagian.sty swish.sty reference.bib
	./run-latex $*

%.pdf: %.dot font.conf
	FONTCONFIG_FILE=$${PWD}/font.conf dot -Tpdf -o $@ $<

font.conf:
	@echo '<?xml version="1.0"?>' > $@
	@echo '<!DOCTYPE fontconfig SYSTEM "fonts.dtd">' >> $@
	@echo '<fontconfig>' >> $@
	@echo "  <dir>$$(dirname $$(kpsewhich lmroman10-regular.otf))</dir>" >> $@
	@echo '</fontconfig>' >> $@

swish :=\
  swish/*.tex\
  swish/event-mgr-events.pdf\
  swish/http-tree.pdf\
  swish/intro-sup-tree.pdf\
  ../src/swish/swish-version.include\

swish.pdf: $(swish)

../src/swish/swish-version.include: ../src/swish/swish.h
	make -C ../src/swish swish-version.include

.PHONY: clean
clean:
	rm -f $(pdf) *.aux *.bbl *.blg *.lof *.log *.lot *.out *.idx *.ind *.ilg *.toc *.thm swish/*.aux font.conf
	find . -iname "*.dot" | sed "s/\.dot$$/\.pdf/" | xargs rm -f
