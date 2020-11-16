# resource has a URI and a label
# optionally has a  and a reference instead of a URI
# is merged on the basis of the URI
# is displayed on the bases of the label
# can have other attributes

#' @import vctrs
#' @importFrom rlang %||%
NULL

#' `resource` vector
#'
#' This represents an RDF resource with the data held in an orthogonal table.
#'
#' @param uri A character vector of URIs
#' @param data A data frame of descriptions (must have a `uri` column)
#' @param x Any vector
#' @return An S3 vector of class `ldf_resource`.
#' @export
#' @examples
#' resource("http://example.net")
#'
#' uris <- c("http://example.net/id/apple",
#'           "http://example.net/id/banana",
#'           "http://example.net/id/carrot")
#' labels <- c("Apple","Banana","Carrot")
#' data <- data.frame(uri=uris, label=labels)
#' r <- resource(uris, data)
resource <- function(uri=character(), data=NULL) {
  uri <- vec_cast(uri, character())
  validate_resource(new_resource(uri, data))
}

new_resource <- function(uri=character(), data=data.frame(uri=uri)) {
  vec_assert(uri, character())
  if(!is.null(data)) { stopifnot(inherits(data, "data.frame")) }
  new_vctr(uri, data=data, class = "ldf_resource", inherit_base_type=F)
}
# for compatibility with the S4 system
methods::setOldClass(c("ldf_resource", "vctrs_vctr"))

#' Validate a resource
#'
#' Checks whether a URI column is present in the data descriptions and that it includes a value
#' for every resource with no duplicates.
#'
#' Violations `stop` execution and report the error message.
#'
#' @param resource A vector of `ldf_resource`s
#' @return The resource (fluent interface for chaining)
#' @export
validate_resource <- function(resource) {
  if(!is.null(attr(resource, "data"))) {
    if(!("uri" %in% colnames(attr(resource, "data")))) {
      stop("Data must include a uri column")
    }

    uris <- attr(resource, "data")$uri
    if(anyDuplicated(uris) != 0) {
      stop("Data must not include duplicate uris")
    }
    target_uri <- uri(resource) %>% purrr::discard(is.na)
    missing_uri <- target_uri[!target_uri %in% uris]
    if(length(missing_uri)>0) {
      stop("Data must include all uris. Missing e.g. ", missing_uri[1])
    }
  }

  resource
}

#' @export
#' @rdname resource
is_resource <- function(x) {
  inherits(x, "ldf_resource")
}

#' Extract a property value from a description
#'
#' @param resource A vector of `ldf_resource`s
#' @param p A property (column name from the description)
#' @return A vector with the values of the property that apply to each resource
#' @export
property <- function(resource, p) {
  data <- attr(resource, "data")
  if(!(p %in% colnames(data))) {
    warning("Column missing from data=", p)
  }
  index <- match(resource, data$uri)

  if(inherits(data, "tbl")) {
    data[index,] %>% dplyr::pull(p)
  } else {
    data[index, p]
  }
}

#' Extract the URI from a resource vector
#'
#' @param resource A vector of `ldf_resource`s
#' @export
uri <- function(resource) {
  vec_data(resource)
}

#' Extract resource labels
#'
#' @param x A vector of `ldf_resource`s
#' @return A character vector of labels
#' @export
label <- function(x) {
  UseMethod("label")
}

#' @export
label.ldf_resource <- function(x) {
  property(x, "label")
}

#' @export
label.default <- label.ldf_resource

#' Extract resource sort priority
#'
#' Sort priority can be used to determine how to order resources
#'
#' @param resource A vector of `ldf_resource`s
#' @return A vector of sort prorities (typically numeric)
#' @export
sort_priority <- function(resource) {
  property(resource, "sort_priority")
}

default_prefixes <- function() {
  getOption("ldf_prefixes") %||% c(
    rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    rdfs="http://www.w3.org/2000/01/rdf-schema#",
    xsd="http://www.w3.org/2001/XMLSchema#",
    owl="http://www.w3.org/2002/07/owl#",
    skos="http://www.w3.org/2004/02/skos/core#",
    void="http://rdfs.org/ns/void#",
    dcat="http://www.w3.org/ns/dcat#",
    dcterms="http://purl.org/dc/terms/",
    qb="http://purl.org/linked-data/cube#",
    sdmxd="http://purl.org/linked-data/sdmx/2009/dimension#"
  )
}

#' Extract Compact URI from resources
#'
#' The URI is compacted using the specified prefixes.
#' If no prefixes are specified the defaults are taken from `default_prefixes` which can be
#' overriden by setting the global option "ldf_prefixes".
#'
#' @param resource A vector of `ldf_resource`s
#' @param prefixes A named character vector mapping from prefix to namespace
#' @return A character vector of compact URIs.
#' @export
curie <- function(resource, prefixes=default_prefixes()) {
  if(length(prefixes)>0) { # ought to check for names
    prefix_to_ns <- stats::setNames(paste0(names(prefixes),":"), unname(prefixes))
    stringr::str_replace_all(uri(resource), prefix_to_ns)
  } else {
    uri(resource)
  }
}

#' @export
format.ldf_resource <- function(x, ...) {
  suppressWarnings(
    output <- label(x) %||% curie(x)
  )
  format(output)
}

#' @export
vec_ptype_abbr.ldf_resource <- function(x, ...) {
  "ldf_rsrc"
}

# TODO: check

#' @export
vec_ptype2.ldf_resource.ldf_resource <- function(x, y, ...) new_resource()

# TODO: should this actually allow char to be lifted to resource?
# or change the next to achieve this?
#' @export
vec_ptype2.ldf_resource.character <- function(x, y, ...) character()

#' @export
vec_ptype2.character.ldf_resource <- function(x, y, ...) character()

#' @export
vec_cast.ldf_resource.ldf_resource <- function(x, to, ...) x

# Cast from character -> resource
#' @export
vec_cast.ldf_resource.character <- function(x, to, ...) resource(x)

# Cast from resource -> character
# Should this extract the URI instead?
#' @export
vec_cast.character.ldf_resource <- function(x, to, ...) vec_data(x)


#' @export
levels.ldf_resource <- function(...) { return(NULL) }

# escape hatch TODO: write-up
df_of_labels <- function(d, ...) {
  data.frame(lapply(d, function(x) {
    if(is_resource(x) | is_interval(x)) {
      label(x)
    } else {
      x
    }
  }), ...)
}
