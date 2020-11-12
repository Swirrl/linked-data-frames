# Linked Data Frames (LDF)

Work with linked-data idiomatically in R using data frames.

Linked Data Frames have columns of RDF resources. These resources are expressed as S3 objects with rich descriptions.

We use the `vctrs` package to encapsulate RDF resources into vectors. This means they can be combined into data frames and dealt with as atomic values while still retaining orthogonal attributes for e.g. their label.

## Usage

Resources in RDF are identified with URIs that are described with a set of statements, each specifying the value of a given property for the resource. We can represent this in R using a character vector for the URIs together with a data frame for the descriptions. That data frame should include a URI column to identify the resource being described in each row.

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



## Installation

You'll need to use devtools to install this package from github:

```r
install.packages("devtools")
devtools::install_github("Swirrl/ldf")
```
