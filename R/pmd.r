#library(SPARQL)

#'
#' @importFrom magrittr %>%
NULL

query <- function(query_string, endpoint="https://staging.gss-data.org.uk/sparql") {
  SPARQL::SPARQL(url="https://staging.gss-data.org.uk/sparql",
                 query=query_string,
                 format="csv",
                 parser_args = list(stringsAsFactors=F, na.strings=""))$results
}

as_variable_names <- function(x) {
  gsub(" ", "_", tolower(x))
}

#' Create bindings for a labelled property
#'
#' Creates a query fragment for predicate-object pairs.
#' The predicate is taken from the property's `uri`.
#' The object is a variable binding made from the property's `label`.
#'
#' @param properties A list or data frame with vectors named `uri` and `label`
#' @return a character vector of predicate-object bindings
predobj_binding <- function(properties) {
#  properties <- as.list(properties)
  properties$var <- as_variable_names(properties$label)
  glue::glue_data(properties, "<{uri}> ?{var};")
}

#' Create Basic Graph Pattern for subject binding labelled properties
#'
#' Creates a basic graph pattern a subject with predicate-object pairs.
#' The subject binding is interpolated directly, so it should be specified
#' accordingly (e.g. as a variable `"?uri"` or a URI `"<http://example.net>"`)
#' The predicate is taken from the property's `uri`.
#' The object is a variable binding made from the property's `label`.
#' Unless the pattern is to be contained in an OPTIONAL clause, it
#' is terminated with a "." character.
#'
#' @param subject A character vector of length 1 with the subject binding
#' @param properties A list or data frame with vectors named `uri` and `label`
#' @param optional Should the pattern be wrapped in an optional clause?
#' @return a character vector of basic graph patterns
subpredobj_binding <- function(subject, properties, optional=F) {
  stringr::str_interp(c(
    ifelse(optional,"OPTIONAL {",""),
    "${subject} ",
    glue::glue_collapse(predobj_binding(properties), sep=" "),
    ifelse(optional,"}"," .")
  ))
}

get_dimensions <- function(dataset_uri) {
  q <- stringr::str_interp(c(
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
    "PREFIX qb: <http://purl.org/linked-data/cube#>",
    "",
    "SELECT ?uri ?label ?codelist WHERE {",
    "  <${dataset_uri}> qb:structure/qb:component/qb:dimension ?uri .",
    "  ?uri rdfs:label ?label .",
    "  OPTIONAL { ?uri qb:codeList ?codelist }",
    "}"
  ))
  query(q)
}

get_measures <- function(dataset_uri) {
  q <- stringr::str_interp(c(
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
    "PREFIX qb: <http://purl.org/linked-data/cube#>",
    "",
    "SELECT ?uri ?label WHERE {",
    "  <${dataset_uri}> qb:structure/qb:component/qb:measure ?uri .",
    "  ?uri rdfs:label ?label .",
    "}"
  ))
  query(q)
}

get_attributes <- function(dataset_uri) {
  q <- stringr::str_interp(c(
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
    "PREFIX qb: <http://purl.org/linked-data/cube#>",
    "",
    "SELECT ?uri ?label WHERE {",
    "  <${dataset_uri}> qb:structure/qb:component/qb:attribute ?uri .",
    "  ?uri rdfs:label ?label .",
    "}"
  ))
  query(q)
}

get_components <- function(dataset_uri) {
  d <- get_dimensions(dataset_uri) %>% select(uri, label)
  m <- get_measures(dataset_uri)
  a <- get_attributes(dataset_uri)
  rbind(d,m,a)
}

get_observations <- function(dataset_uri,
                             dimensions=get_dimensions(dataset_uri),
                             measures=get_measures(dataset_uri),
                             attributes=get_attributes(dataset_uri)) {
  dimension_bindings <- glue::glue_collapse(predobj_binding(dimensions), sep="\n")
  measure_and_attribute_bindings <- rbind(measures, attributes) %>%
    purrr::pmap(function(uri, label) { subpredobj_binding("?uri", data.frame(uri=uri,label=label), optional=T)}) %>%
    glue::glue_collapse(sep="\n")

  q <- stringr::str_interp(c(
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
    "PREFIX qb: <http://purl.org/linked-data/cube#>",
    "",
    "SELECT * WHERE {",
    "  ?uri qb:dataSet <${dataset_uri}>;",
    dimension_bindings,
    measure_and_attribute_bindings,
    "}"
  ))

  query(q)
}

get_codelist <- function(codelist_uri) {
  q <- stringr::str_interp(c(
    "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>",
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",

    "SELECT * ",
    "WHERE {",
    "  ?uri skos:inScheme <${codelist_uri}>;",
    "    rdfs:label ?label .",
    "  OPTIONAL { ?uri skos:notation ?notation }",
    "  OPTIONAL { ?uri <http://www.w3.org/ns/ui#sortPriority> ?sort_priority }",
    "}"
  ))
  query(q)
}

get_label <- function(uri) {
  uri_binding <- glue::glue_collapse(glue::glue_data(list(uri=uri), "<{uri}>"), " ")

  q <- stringr::str_interp(c(
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",

    "SELECT * ",
    "WHERE {",
    "  VALUES ?uri { ${uri_binding} }",
    "  ?uri rdfs:label ?label .",
    "}"
  ))
  query(q)
}

#' Download Statistical Geographies
#'
#' Connects to the [ONS Geography Linked Date SPARQL interface](http://statistics.data.gov.uk/sparql) to
#' retrieve descriptions statistical geographies.
#'
#' The description includes the official name, GSS-code and optionally the URI of the parent geography and
#' WKT geometry (if it exists).
#'
#' @param geography_uri A character vector of URIs
#' @return A data frame of geography descriptions
#' @examples
#' \dontrun{
#' get_geography("http://statistics.data.gov.uk/id/statistical-geography/K02000001")
#' }
get_geography <- function(geography_uri) {
  geo_binding <- glue::glue_collapse(glue::glue_data(list(uri=unique(geography_uri)), "<{uri}>"), " ")

  boundaries <- glue::glue("
SELECT * WHERE {
  VALUES ?uri { `geo_binding` }

  ?uri
    <http://statistics.data.gov.uk/def/statistical-geography#officialname> ?label;
    <http://www.w3.org/2004/02/skos/core#notation> ?notation;
    .

  OPTIONAL {
    ?uri <http://www.opengis.net/ont/geosparql#hasGeometry>/<http://www.opengis.net/ont/geosparql#asWKT> ?boundary;
  }

  OPTIONAL {
    ?uri <http://statistics.data.gov.uk/def/statistical-geography#parentcode> ?parent;
  }
}", .open="`", .close="`")
  geo <- query(boundaries, "http://statistics.data.gov.uk/sparql")
  geo %>% dplyr::distinct(uri, .keep_all=T)
}

#' Download a DataCube
#'
#' Returns a data frame with one observation per row and a column per component.
#' Components may be dimensions, measures or attributes.
#'
#' Where the column represents an RDF Resource, it will have the type `ldf_resource` vector.
#'
#' @param dataset_uri A string
#' @return A data frame
#' @export
get_cube <- function(dataset_uri) {
  d <- get_dimensions(dataset_uri)
  m <- get_measures(dataset_uri)
  a <- get_attributes(dataset_uri)

  observations <- get_observations(dataset_uri, d, m, a)

  # apply codelists to coded properties (dimensions only atm)
  codelists <- setNames(d$codelist, as_variable_names(d$label)) %>%
    purrr::discard(is.na) %>%
    lapply(get_codelist)

  for (dimension in names(codelists)) {
    codelist <- codelists[[dimension]] %>% dplyr::distinct(uri, .keep_all=T)
    observations[,dimension] <- resource(observations[,dimension], codelist)
  }

  # create intervals for reference period dimension
  ref_period <- as_variable_names(d[d$uri=="http://purl.org/linked-data/sdmx/2009/dimension#refPeriod", "label"])
  observations[,ref_period] <- interval(observations[,ref_period])

  ref_area <- as_variable_names(d[d$uri=="http://purl.org/linked-data/sdmx/2009/dimension#refArea", "label"])
  if(length(ref_area)==1) {
    observations[,ref_area] <- resource(observations[,ref_area], get_geography(observations[,ref_area]))
  }

  # attributes and any remaining dimensions should just have their values labelled if possible
  remaining_d <- dplyr::filter(d, is.na(codelist) & !(uri %in% c("http://purl.org/linked-data/sdmx/2009/dimension#refPeriod", "http://purl.org/linked-data/sdmx/2009/dimension#refArea")))
  for (component in as_variable_names(c(a$label, remaining_d$label))) {
    description <- get_label(unique(observations[, component]))
    observations[, component] <- resource(observations[, component], description)
  }

  observations %>% dplyr::select(!uri)
}
