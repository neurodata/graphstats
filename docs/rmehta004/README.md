# Graphstats: Sprint 4 Features

## Background

This guide explains how to view the changes to the `graphstats` package made by Ronak and Coleman for Sprint 4 of Neuro Data Design Spring 2018. These include functions for the spectral analysis of graphs, seeded graph matching, and vertex nomination.

## Step 1: Install Dependencies

Because this package will be viewed in development stage, it is necessary to install the dependencies. There is no need to load them. Run the following code.

```r
packages <- c("ggplot2",
              "reshape2",
              "stringr",
              "abind",
              "plyr",
              "igraph",
              "mclust",
              "fpc",
              "rARPACK")

install.packages(packages)
```

## Step 2: Clone GitHub Repository

You can find the `graphstats` GitHub repo here: https://github.com/neurodata/graphstats. Navigate to the containing directory in your machine, and run:
- `git clone https://github.com/neurodata/graphstats.git`
- `git checkout dev-sgc`
This will move you to the development branch.

## Step 3: Open in RStudio

Open RStudio, and go to File > Open Project and navigate to the `graphstats.Rproj` file in the `graphstats` repo. To ensure that the package builds correctly, run the following:
```r
devtools::document()
```

## Step 4: Build and Test

Next, run the following commands in order.
- CTRL + SHIFT + B to build the package.
- CTRL + SHIFT + T to test the package.

## Step 5: Read Vignettes

All vignettes can be found in the `vignettes` directory of the package. Open any file and click 'Knit' at the top of RStudio to view the HTML rendered versions.
