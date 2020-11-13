# Linked Data Frames (LDF)

Work with linked-data idiomatically in R using data frames.

Linked Data Frames have columns of RDF resources. These resources are expressed as S3 objects with rich descriptions.

We use the `vctrs` package to encapsulate RDF resources into vectors. This means they can be combined into data frames and dealt with as atomic values while still retaining orthogonal attributes for e.g. their label.

## Usage

Resources in RDF are identified with URIs that are described with a set of statements, each specifying the value of a given property for the resource. We can represent this in R using a character vector for the URIs together with a data frame for the descriptions. That data frame should include a "uri" column to identify the resource being described in each row.

```r
uris <- c("http://example.net/id/apple",
          "http://example.net/id/banana",
          "http://example.net/id/carrot")
labels <- c("Apple","Banana","Carrot")
descriptions <- data.frame(uri=uris, label=labels)

food <- resource(uris, descriptions)
```

The `resource` constructor returns a `ldf_resource` object that has a variety of methods defined on it, including the `format` generic which allows us to use the labels instead of the URIs when printing to the console.

```r
food

#> <ldf_resource[3]>
#> [1] Apple  Banana Carrot
```

For a complete introduction to `ldf_resource` vectors library please see the `vignette("working-with-ldf-resources")`.

A second type is provided `ldf_interval` specifically for working with the reference.data.gov.uk intervals. Read more in the `vignette("working-with-ldf-intervals")`.

You can also use this package to download resources from the web.

*Ultimately the functions in `pmd.r` ought to be extracted to a separate package*.

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

The table includes 3 columns that are `ldf_resource` types plus a normal double and one of the type `ldf_interval`.

For more information about this consult `vignette("downloading-datacubes")`.

## Installation

You'll need to use devtools to install this package from github:

```r
install.packages("devtools")
devtools::install_github("Swirrl/ldf")
```
