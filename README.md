[![DOI](https://zenodo.org/badge/226997664.svg)](https://zenodo.org/badge/latestdoi/226997664)

# stomatal-tradeoff

This repository contains source code associated with the manuscript:

[A stomatal model of anatomical tradeoffs between gas exchange
and pathogen colonization](https://doi.org/10.3389/fpls.2020.518991). *Front. Plant Sci.* 11:518991.

This project was developed by [Chris Muir](https://cdmuir.netlify.app).

More information about the study is available on [github](https://github.com/cdmuir/stomatal-tradeoff/blob/master/ms/ms.pdf).

## Downloading data and code 

1. Download or clone this repository to your machine.

```
git clone git@github.com:cdmuir/stomatal-tradeoff.git
```

2. Open `stomatal-tradeoff.Rproj` in [RStudio](https://www.rstudio.com/)

## Generating manuscript

### Software requirements

At minimum, you will need [R](https://cran.r-project.org/) installed on your machine. If you want to run symbolic mathematics, you will also need to have [Python](https://www.python.org/) and [SymPy](https://www.sympy.org/en/index.html) installed. However, you can skip this and use the saved Python output in the repo.

Install additional packages by running `r/install-packages.R`.

### Generating manuscript with pre-saved outout

Open `ms/ms.Rmd` and knit using [RStudio](https://www.rstudio.com/).

You can also run the following code from the R console:

```{r}
rmarkdown::render(
  input = "ms/ms.Rmd",
  output_dir = "ms"
)
```

### Generating all results

You can re-run all analyses, figures, etc. using [GNU make](https://www.gnu.org/software/make/). Type `make --version` in a terminal/shell to see if it is already installed.

```
# Clear out previously saved output
make cleanall
# This will take several minutes to run
make
```
