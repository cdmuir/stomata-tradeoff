# A good simple tutorial about Make can be found at http://kbroman.org/minimal_make/ 
R_OPTS=--no-save --no-restore --no-init-file --no-site-file

ms: ms/ms.Rmd ms/appendix2.Rmd Makefile figures/fig1.jpeg figures/fig2.tex objects/spatially-explicit-results.rds figures/fig3.pdf objects/spatially-explicit-scaling-results.rds objects/spatially-implicit-scaling-results.rds figures/fig4.pdf figures/figS1.pdf figures/figS2.pdf figures/figS3.pdf
	Rscript -e 'source("r/09_render-ms.R")'

figures/fig1.jpeg: r/01_make-fig1-2.R
	Rscript -e 'source("r/01_make-fig1-2.R")'
	
figures/fig2.tex: r/01_make-fig1-2.R
	Rscript -e 'source("r/01_make-fig1-2.R")'

objects/spatially-explicit-results.rds: r/02_spatially-explicit-model.R
	Rscript -e 'source("r/02_spatially-explicit-model.R")'

figures/fig3.pdf: objects/spatially-explicit-results.rds r/03_plot-spatially-explicit.R
	Rscript -e 'source("r/03_plot-spatially-explicit.R")'

objects/spatially-explicit-scaling-results.rds: r/04_find-scaling.R
	Rscript -e 'source("r/04_find-scaling.R")'

objects/spatially-implicit-scaling-results.rds: r/04_find-scaling.R
	Rscript -e 'source("r/04_find-scaling.R")'
	
figures/fig4.pdf: objects/spatially-explicit-scaling-results.rds objects/spatially-implicit-scaling-results.rds r/05_plot-scaling.R
	Rscript -e 'source("r/05_plot-scaling.R")'

figures/figS1.pdf: objects/spatially-explicit-results.rds r/03_plot-spatially-explicit.R
	Rscript -e 'source("r/03_plot-spatially-explicit.R")'

figures/figS2.pdf: objects/spatially-explicit-results.rds r/06_plot-spatially-implicit.R
	Rscript -e 'source("r/06_plot-spatially-implicit.R")'

figures/figS3.pdf: objects/spatially-implicit-scaling-results.rds r/04_find-scaling.R
	Rscript -e 'source("r/05_plot-scaling.R")'

clean: 
	\rm -f *~ *.Rout */*~ */*.Rout .RData Rplots.pdf

cleanall: 
	\rm -f *.aux *.bbl *.blg *.log *.pdf *~ *.Rout */*~ */*.Rout figures/*.jpeg figures/*.pdf figures/*.tex ms/appendix2.pdf ms/ms.pdf ms/appendix2.tex ms/ms.tex objects/*.rds */*.aux */*.log 
