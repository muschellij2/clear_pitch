name=CT_ICH_Segmentation
flowname=Imaging_Pipeline_Flowchart_with_Rigid
all: ${name}.pdf 
# date=2014Nov18
${flowname}.pdf: ${flowname}.tex
	pdflatex ${flowname}.tex

${name}.pdf: ${name}.Rnw \
	${flowname}.pdf	\
	figures/Reseg_Dice_Comparison.png \
	figures/Reseg_Volume_Comparison.png \
	ich_chapter.bib 
	if [ -e ${name}.aux ]; \
	then \
	rm ${name}.aux; \
	fi;
	Rscript parser.R;
	Rscript -e "library(knitr); knit('${name}.Rnw'); purl('${name}.Rnw')"
	pdflatex ${name}
	bibtex ${name}
	bibtex ${name}
	if [ -e ${name}1-blx.bib ]; \
	then \
	bibtex ${name}1-blx; \
	fi;
	if [ -e ${name}2-blx.bib ]; \
	then \
	bibtex ${name}2-blx; \
	fi;		
	pdflatex ${name}
	pdflatex ${name}
	Rscript copier.R;
	open ${name}.pdf

cleanall: 
	rm ${name}.pdf
	rm ${flowname}.pdf

clean: 
	rm ${name}.pdf
copy: 
	Rscript copier.R;

open: 
	open ${name}.pdf
adobe:
	adobe ${name}.pdf
#http://texblog.org/2012/10/22/multiple-bibliographies-with-biblatex/
#http://tex.stackexchange.com/questions/128196/problem-with-refsection

# code.tex: bsc-code.tex
#	sed -e '/\(Schunk\|Sinput\)/d' \
#		-e 's/\\\(begin\|end\){Soutput}/~~~~/; s/^> /    /' \
#		 bsc-code.tex | pandoc -w context > code.tex
	
