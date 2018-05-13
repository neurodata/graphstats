# Graphstats: Sprint 4 New Features

## Background

This guide explains how to view the changes to the `graphstats` package made by Ronak and Coleman for Sprint 4 of Neuro Data Design Spring 2018. These include functions for the spectral analysis of graphs, seeded graph matching, and vertex nomination.

## Step 1: Clone GitHub Repository

You can find the `graphstats` GitHub repo here: https://github.com/neurodata/graphstats. Navigate to the containing directory in your machine, and run the following in the Terminal:
- `git clone https://github.com/neurodata/graphstats.git`
- `cd graphstats`
- `git checkout dev-ndd-sprint4`
This will move you to the development branch.

## Step 2: Install Dependencies

Because this package will be viewed in development stage, it is necessary to install the dependencies in `R`. There is no need to load them. Open Rstudio and run the following code in the `R` console, Rstudio console, or the Terminal version of `R`. (This will work even if you have some of these packages.)

```r
packages <- c("ggplot2",
              "reshape2",
              "stringr",
              "abind",
              "plyr",
              "clue",
              "igraph",
              "mclust",
              "fpc",
              "rARPACK",
              "testthat",
              "devtools")

install.packages(packages)
```

## Step 3: Open the Package, Build, and Test

In RStudio, go to File > Open Project and navigate to the `graphstats.Rproj` file in the `graphstats` repo. To ensure that the package builds correctly, run the following:

```r
devtools::document()
```
Next, run the following commands in order.
- `CTRL` + `SHIFT` + `B` to build the package.
- `CTRL` + `SHIFT` + `T` to test the package.

## Step 4: Read Vignettes

All vignettes can be found in the `vignettes` directory of the package. Open any file and click 'Knit' at the top of the RStudio toolbar to view the HTML rendered versions.
