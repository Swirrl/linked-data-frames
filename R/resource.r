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
#' A vector of RDF resources with descriptions held in an orthogonal table.
#'
#' @param uri A character vector of URIs
#' @param description A data frame of descriptions (must have a `uri` column)
#' @param fill_missing A boolean specifying whether rows be added to the
#' description for missing URIs (defaults to `FALSE`)
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
#' description <- data.frame(uri=uris, label=labels)
#' r <- resource(uris, description)
resource <- function(uri=character(), description=data.frame(uri=unique(uri)), fill_missing=FALSE) {
  uri <- vec_cast(uri, character())
  if(fill_missing) { description <- fill_missing(description, uri) }
  validate_resource(new_resource(uri, description))
}

new_resource <- function(uri=character(), description=data.frame(uri=unique(uri))) {
  vec_assert(uri, character())
  if(!is.null(description)) { stopifnot(inherits(description, "data.frame")) }
  new_vctr(uri, description=description, class = "ldf_resource", inherit_base_type=F)
}
# for compatibility with the S4 system
methods::setOldClass(c("ldf_resource", "vctrs_vctr"))

#' Validate a resource
#'
#' Checks whether a URI column is present in the description and that it includes a value
#' for every resource with no duplicates.
#'
#' Violations `stop` execution and report the error message.
#'
#' @param resource A vector of `ldf_resource`s
#' @return The resource (fluent interface for chaining)
#' @export
validate_resource <- function(resource) {
  if(!is.null(description(resource))) {
    if(!("uri" %in% colnames(description(resource)))) {
      stop("Description must include a uri column")
    }

    uris <- description(resource)$uri
    if(anyDuplicated(uris) != 0) {
      stop("Description must not include duplicate uris")
    }
    target_uri <- uri(resource) %>% purrr::discard(is.na)
    missing_uri <- target_uri[!target_uri %in% uris]
    if(length(missing_uri)>0) {
      stop("Description must include all uris. Missing e.g. ", missing_uri[1])
    }
  }

  resource
}

#' @export
#' @rdname resource
is_resource <- function(x) {
  inherits(x, "ldf_resource")
}

#' Extract the description from a resource
#'
#' @param resource A vector of `ldf_resource`s
#' @return The resource's description - a type inheriting from data frame
#' @export
description <- function(resource) {
  attr(resource, "description")
}

#' Assign a description to a resource
#'
#' Replaces the existing description. To add properties to a description use in combination with
#' [merge_description()].
#'
#' @param resource A vector of `ldf_resource`s
#' @param value A new resource description
#' @examples
#' r <- resource("a")
#' description(r) <- data.frame(uri="a",label="A")
#' @export
`description<-` <- function(resource, value) {
  attr(resource, "description") <- value
  resource
}

#' Extract a property value from a resource description
#'
#' @param resource A vector of `ldf_resource`s
#' @param p A property (column name from the description)
#' @return A vector with the values of the property that apply to each resource
#' @export
property <- function(resource, p) {
  description <- description(resource)
  if(!(p %in% colnames(description))) {
    warning("Column missing from description: ", p)
  }
  index <- match(resource, description$uri)

  if(inherits(description, "tbl")) {
    if((p %in% colnames(description))) {
      description[index,] %>% dplyr::pull(p)
    } else {
      NULL
    }
  } else {
    description[index, p]
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

#' Combine resource descriptions
#'
#' Merges descriptions on URI, and attempts to merge columns from either description (filling with
#' `NA` where the column exists only in one description).
#'
#' @param x A resource description (inherits from data frame)
#' @param y A resource description (inherits from data frame)
#' @return A resource description (inherits from data frame)
#' @examples
#' a <- resource("a", data.frame(uri="a", label="A"))
#' description(a) <- merge_description(description(a), data.frame(uri="a", vowel=TRUE))
#' @export
merge_description <- function(x, y) {
  if(!is.null(x)) {
    common_columns <- intersect(colnames(x), colnames(y))
    dplyr::full_join(x, y, by=common_columns)
  } else {
    y
  }
}

#' Add rows to description for missing URIs
#'
#' The missing URIs will only be described with their URI
#'
#' @param description A resource description (inherits from data frame)
#' @param uri A character vector of URIs
#' @return A resource description with all URIs included
fill_missing <- function(description, uri) {
  missing <- setdiff(uri, description$uri)

  dplyr::bind_rows(description, data.frame(uri=missing))
}

#' @export
obj_print_footer.ldf_resource <- function(x, ...) {
  cat("Description: ", paste0(colnames(description(x)), collapse=", "), "\n", sep = "")
}

#' @export
vec_ptype_abbr.ldf_resource <- function(x, ...) {
  "ldf_rsrc"
}

#' @export
vec_ptype2.ldf_resource.ldf_resource <- function(x, y, ...) {
  new_resource(description=merge_description(description(x), description(y)))
}

# TODO: should this actually allow char to be lifted to resource?
#' @export
vec_ptype2.ldf_resource.character <- function(x, y, ...) character()

#' @export
vec_ptype2.character.ldf_resource <- function(x, y, ...) character()


#' @export
vec_cast.ldf_resource.ldf_resource <- function(x, to, ...) {
  all_description <- merge_description(description(x), description(to))

  new_resource(vec_data(x), description=all_description)
}

# Cast from character -> resource
#' @export
vec_cast.ldf_resource.character <- function(x, to, ...) resource(x)

# Cast from resource -> character
# Extracts the URI
#' @export
vec_cast.character.ldf_resource <- function(x, to, ...) vec_data(x)

# Rebuild the description
#' @export
vec_restore.ldf_resource <- function(x, to, ...) {
  all_description <- merge_description(description(x), description(to))

  # this enables subsetting to also subset descriptions
  # it's commented out as it's also stripping descriptions when rbinding
  # if(nrow(all_description)>0 & length(na.omit(vec_data(x))) > 0) {
  #   all_description <- all_description %>% dplyr::filter(uri %in% vec_data(x))
  # }

  new_resource(vec_data(x), description=all_description)
}

# #' @export
# vec_proxy.ldf_resource <- function(x, ...) {
#   unclass(x)
# }


#' @export
levels.ldf_resource <- function(...) { return(NULL) }

#' Convert a linked data frame to labels
#'
#' This takes a data frame containing `ldf_resource` or `ldf_interval` vectors
#' and converts those vectors into labels.
#'
#' The labels will either be character vectors or factors depending on the value of
#' `default.stringsAsFactors()`. This can be ridden by passing the argument
#' `stringsAsFactors=F`.
#'
#' @param d A linked data frame
#' @param ... Additional arguments passed to `data.frame`
#' @return A data frame with labels in place of any ldf vectors
#' @export
#' @examples
#' uris <- c("http://example.net/id/apple",
#'           "http://example.net/id/banana",
#'           "http://example.net/id/carrot")
#' labels <- c("Apple","Banana","Carrot")
#' description <- data.frame(uri=uris, label=labels)
#'
#' linked_data_frame <- data.frame(fruit=resource(uris, description))
#' labelled_data_frame <- as_dataframe_of_labels(linked_data_frame, stringsAsFactors=FALSE)
as_dataframe_of_labels <- function(d, ...) {
  data.frame(lapply(d, function(x) {
    if(is_resource(x) | is_interval(x)) {
      label(x)
    } else {
      x
    }
  }), ...)
}

#' Enrich a data frame of URIs with resources descriptions
#'
#' This takes a data frame containing URIs and attempts to download descriptions for them.
#'
#' The basic version just uses [get_label()].
#'
#' @param d A data frame containing URIs
#' @param endpoint A SPARQL endpoint
#' @return A data frame of resources
#' @export
as_dataframe_of_resources <- function(d, endpoint=default_endpoint()) {
  data.frame(lapply(d, function(x) {
    if(is.character(x) & !is_resource(x)) {
      desc <- get_label(x, endpoint)
      if(nrow(desc)>0) {
        resource(x, desc)
      } else {
        x
      }
    } else {
      x
    }
  }))
}
