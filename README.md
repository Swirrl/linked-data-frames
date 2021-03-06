# Linked Data Frames (LDF) <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/Swirrl/linked-data-frames.svg?branch=master)](https://travis-ci.com/Swirrl/linked-data-frames)
<!-- badges: end -->

Work with linked-data idiomatically in R using data frames.

Linked Data Frames have columns of RDF resources. These resources are expressed as S3 objects with rich descriptions.

We use the `vctrs` package to encapsulate RDF resources into vectors. This means they can be combined into data frames and dealt with as atomic values while still retaining orthogonal attributes for e.g. their label.

## Usage

### Downloading linked data frames

You can use this package to download linked data frames from the web.

Here we connect to an instance of PublishMyData to download a data cube.

```r
cube <- get_cube("http://gss-data.org.uk/data/gss_data/covid-19/ons-online-price-changes-for-high-demand-products#dataset")

head(cube) %>% as_tibble()
#> # A tibble: 6 x 5
#>   measure_type reference_period product                  percent unit_of_measure    
#>   <ldf_rsrc>   <ldf_ntrv>       <ldf_rsrc>                 <dbl> <ldf_rsrc>         
#> 1 Percent      2020-03-16 P7D   Antibacterial hand wipes     100 Price Change Indice
#> 2 Percent      2020-03-16 P7D   Antibacterial wipes          100 Price Change Indice
#> 3 Percent      2020-03-16 P7D   Baby food                    100 Price Change Indice
#> 4 Percent      2020-03-16 P7D   Cough and cold               100 Price Change Indice
#> 5 Percent      2020-03-16 P7D   Dried pasta                  100 Price Change Indice
#> 6 Percent      2020-03-16 P7D   Flour                        100 Price Change Indice
```

The cube is modelled as a tidy table of observations with a column for each component property. The columns whose values are RDF resources are given a special type: either `ldf_resource` or `ldf_interval`.

For examples of how to work with data cubes please see the following vignettes:

- `vignette("mapping-statistical-geography")`: explains how to download geographic resources and use them to plot a thematic map.
- `vignette("tabulate-datacube")`: explains how to transform a tidy table of observations by cross-tabulating them back to format used in the original publication.
- `vignette("merging-datacubes")`: explains how to combine data cubes together to enrich data and create novel analyses.

### Modelling RDF resources as vectors

Resources in RDF are identified with URIs that are described with a set of statements. Each statement specifies the value of a given property for the resource. We can represent this in R using a character vector for the URIs together with a data frame for the descriptions. That data frame includes a "uri" column to identify the resource being described in each row.

```r
uris <- c("http://example.net/id/apple",
          "http://example.net/id/banana",
          "http://example.net/id/carrot")
labels <- c("Apple","Banana","Carrot")
descriptions <- data.frame(uri=uris, label=labels)

food <- resource(uris, descriptions)
```

The `resource()` constructor returns a `ldf_resource` object that has a variety of methods defined on it, including the `format()` generic which allows us to use the labels instead of the URIs when printing to the console.

```r
food

#> <ldf_resource[3]>
#> [1] Apple  Banana Carrot
```

For a complete introduction to the LDF library please see the following vignettes:

- `vignette("introduction-to-ldf")`: explains how resources are represented and introduces functions like `description()`, `property()` and `curie()` that are available for working with them;
- `vignette("creating-ldf-resources")`: explains how to create resources from SPARQL queries or RDF files;
- `vignette("working-with-ldf-intervals")`: explains how to create interval resources from URIs and use them to plot a time series chart;

## Installation

You'll need to use devtools to install this package from github:

```r
install.packages("devtools")
devtools::install_github("Swirrl/linked-data-frames")
library(ldf)
```
