# stomatal-tradeoff

[A stomatal model of tradeoffs between photosynthesis and pathogen defense](https://doi.org/10.1101/XXXXX). *bioRxiv*.

This project was developed by [Chris Muir](https://www.cdmuir.netlify.com).

More information about the study is available on [github](https://github.com/cdmuir/stomatal-tradeoff/blob/master/ms/ms.pdf).

## Downloading data and code 

1. Download or clone this repository to your machine.

```
git clone git@github.com:cdmuir/stomatal-tradeoff.git
```

2. Open `stomatal-tradeoff.Rproj` in [RStudio](https://www.rstudio.com/)

## Generating manuscript

You can source all the code you need in the correct order using `r/run-all.R`. Even if you don't want to run all the code, you may need to install some packages (`r/install-packages.R`) and attach them (`r/header.R`).

- To use premade R output, simply open `ms/ms.Rmd` and compile using RStudio.
- To re-run all scripts, source `r/run-all.R` in the R Console:

```
# This will take several minutes to run
source("r/run-all.R")
```
