# A good simple tutorial about Make can be found at http://kbroman.org/minimal_make/ 
R_OPTS=--no-save --no-restore --no-init-file --no-site-file

# Render manuscript from pre-saved output
ms: fig1 fig2 fig3 fig4 figS1 figS2 figS3 ms/ms.Rmd ms/stomata-tradeoff.bib ms/appendix2.Rmd
	Rscript -e 'source("r/09_render-ms.R")'

fig1: r/01_make-fig1-2.R
	Rscript -e 'source("r/01_make-fig1-2.R")'

fig2: r/01_make-fig1-2.R
	Rscript -e 'source("r/01_make-fig1-2.R")'

results: r/functions.R r/02_spatially-explicit-model.R
	Rscript -e 'source("r/02_spatially-explicit-model.R")'

fig3: objects/spatially-explicit-results.rds r/03_plot-spatially-explicit.R
	Rscript -e 'source("r/03_plot-spatially-explicit.R")'

isoclines: r/functions.R r/04_find-scaling.R
	Rscript -e 'source("r/04_find-scaling.R")'

fig4: objects/isocline-results.rds r/04_find-scaling.R
	Rscript -e 'source("r/05_plot-scaling.R")'

figS1: objects/spatially-explicit-results.rds r/03_plot-spatially-explicit.R
	Rscript -e 'source("r/03_plot-spatially-explicit.R")'

figS2: objects/spatially-explicit-results.rds r/06_plot-spatially-implicit.R
	Rscript -e 'source("r/06_plot-spatially-implicit.R")'

figS3: objects/isocline-results.rds r/04_find-scaling.R
	Rscript -e 'source("r/05_plot-scaling.R")'

clean: 
	\rm -f *~ *.Rout */*~ */*.Rout .RData Rplots.pdf
