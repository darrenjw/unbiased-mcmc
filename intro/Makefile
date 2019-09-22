# Makefile

FILE=UnbiasedMCMC

TARGETS=$(FILE).R $(FILE)-slides.pdf $(FILE)-doc.pdf $(FILE).md $(FILE).html

FORCE:
	make $(TARGETS)

html: $(HTML)

%.R: %.Rmd
	Rscript -e "knitr::purl('$<')"

%.md: %.Rmd
	Rscript -e "knitr::knit('$<')"

%.pdf: %.Rmd
	Rscript -e "rmarkdown::render('$<')"

%.html: %.md
	pandoc $< -o $@

$(FILE)-slides.Rmd: $(FILE).Rmd beamer.yaml
	cat beamer.yaml $(FILE).Rmd > $(FILE)-slides.Rmd

$(FILE)-doc.Rmd: $(FILE).Rmd doc.yaml
	cat doc.yaml $(FILE).Rmd > $(FILE)-doc.Rmd

edit:
	emacs Makefile Readme.md *.yaml $(FILE).Rmd &

view: $(FILE)-slides.pdf
	evince $(FILE)-slides.pdf &

clean:
	rm -f *~ $(TARGETS) *.log



# eof


