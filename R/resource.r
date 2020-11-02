# resource has a URI and a label
# optionally has a  and a reference instead of a URI
# is merged on the basis of the URI
# is displayed on the bases of the label
# can have other attributes

#' @import vctrs
#' @importFrom rlang %||%
NULL

resource <- function(uri, data=NULL) {
  uri <- vec_cast(uri, character())
  validate_resource(new_resource(uri, data))
}

new_resource <- function(uri, data) {
  vec_assert(uri, character())
  if(!is.null(data)) { stopifnot(inherits(data, "data.frame")) }
  new_vctr(uri, data=data, class = "ldf_resource")
}

validate_resource <- function(resource) {
  if(!is.null(attr(resource, "data"))) {
    if(!("uri" %in% colnames(attr(resource, "data")))) {
      stop("Data must include a uri column")
    }

    uris <- attr(resource, "data")$uri
    if(anyDuplicated(uris) != 0) {
      stop("Data must not include duplicate uris")
    }
    if(!all(uri(resource) %in% uris)) {
      stop("Data must include all uris")
    }
  }

  resource
}

property <- function(resource, p) {
  data <- attr(resource, "data")
  if(!(p %in% colnames(data))) {
    warning("Column missing from data=", p)
  }
  index <- match(resource, data$uri)
  data[index, p]
}

uri <- function(resource) {
  vec_data(resource)
}

label <- function(resource) {
  property(resource, "label")
}

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

curie <- function(resource, prefixes=default_prefixes()) {
  if(length(prefixes)>0) { # ought to check for names
    prefix_to_ns <- stats::setNames(paste0(names(prefixes),":"), unname(prefixes))
    stringr::str_replace_all(uri(resource), prefix_to_ns)
  } else {
    uri(resource)
  }
}

format.ldf_resource <- function(resource) {
  suppressWarnings(
    label(resource) %||% curie(resource)
  )
}
