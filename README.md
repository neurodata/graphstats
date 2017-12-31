# Graphstats

A package for graph statistical algorithms.

- [Overview](#overview)
- [Repo Contents](#repo-contents)
- [System Requirements](#system-requirements)
- [Installation Guide](#installation-guide)
- [Demo](#demo)
 - [Instructions for Use](#instructions-for-use)
- [License](./LICENSE)
- [Issues](https://github.com/neurodata/graphstats/issues)

# Overview

A graph, or network, provides a mathematically intuitive representation of data with some sort of relationship between items. For example, a social network can be represented as a graph by considering all participants in the social network as nodes, with connections representing whether each pair of individuals in the network are friends with one another. Naively, one might apply traditional statistical techniques to a graph, which neglects the spatial arrangement of nodes within the network and is not utilizing all of the information present in the graph. In this package, we provide utilities and algorithms designed for the processing and analysis of graphs with specialized graph statistical algorithms.

# Repo Contents

- [R](./R): `R` package code.
- [docs](./docs): package documentation.
- [man](./man): package manual for help in R session.
- [tests](./tests): `R` unit tests written using the `testthat` package.
- [vignettes](./vignettes): `R` vignettes for R session html help pages.

# System Requirements

## Hardware Requirements

The `graphstats` package requires only a standard computer with enough RAM to support the operations defined by a user. For minimal performance, this will be a computer with about 2 GB of RAM. For optimal performance, we recommend a computer with the following specs:

RAM: 16+ GB  
CPU: 4+ cores, 3.3+ GHz/core

The runtimes below are generated using a computer with the recommended specs (16 GB RAM, 4 cores@3.3 GHz) and internet of speed 25 Mbps.

## Software Requirements

### OS Requirements

This package is supported for *Linux* operating systems. The package has been tested on the following systems:

Linux: Ubuntu 16.04  
Mac OSX:  
Windows:  

Before setting up the `graphstats` package, users should have `R` version 3.4.0 or higher, and several packages set up from CRAN.

#### Installing R version 3.4.2 on Ubuntu 16.04

the latest version of R can be installed by adding the latest repository to `apt`:

```
sudo echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" | sudo tee -a /etc/apt/sources.list
gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
gpg -a --export E084DAB9 | sudo apt-key add -
sudo apt-get update
sudo apt-get install r-base r-base-dev
```

which should install in about 20 seconds.

If you are having an issue that you believe to be tied to software versioning issues, please drop us an [Issue](https://github.com/neurodata/graphstats/issues). 

# Installation Guide

From an `R` session, type:

```
require(devtools)
install_github('neurodata/graphstats', build_vignettes=TRUE)  # install graphstats with the vignettes
require(graphstats)  # source the package now that it is set up
vignette("siem", package="graphstats")  # view one of the basic vignettes
```

The package should take approximately 20 seconds to install with vignettes on a recommended computer. 


# Demo

For interactive demos of the functions, please check out the vignettes built into the package. They can be accessed as follows:

```
require(graphstats)
vignette('siem')
```

# Usage

Below, we provide several real applications of functions in the `graphstats` pacakge.

## SIEM

+ fMRI/dMRI connectomics  
    - [within-modality ipsi vs contra-lateral connectivity](http://docs.neurodata.io/graphstats/siem/hemisphere_within.html)
    - [between-modality ipsi vs contra-lateral connectivity](http://docs.neurodata.io/graphstats/siem/hemisphere_across.html)
    - [within-modality homo vs heterotopic connectivity](http://docs.neurodata.io/graphstats/siem/bilateral_within.html)
    - [between-modality homo vs heterotopic connectivity](http://docs.neurodata.io/graphstats/siem/bilateral_across.html)
