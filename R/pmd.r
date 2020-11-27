#' @importFrom magrittr %>%
NULL

#' Execute SPARQL select query and parse results
#'
#' @param query_string A string with the select query
#' @param endpoint A string defining the endpoint
#' @param format A string defining the format, either `"csv"` or `"json"`
#' @return A data frame with a column for each variable binding, and a row per result
#' @export
#' @examples
#' \dontrun{
#' query("SELECT * WHERE { ?s ?p ?o } LIMIT 10", "https://statistics.data.gov.uk/sparql")
#' }
query <- function(query_string, endpoint=default_endpoint(), format="csv") {
  mime <- switch(format,
                 csv="text/csv",
                 json="application/json",
                 "text/csv")

  response <- httr::POST(url=endpoint,
                         httr::accept(mime),
                         body=list(query=query_string),
                         encode="form")

  if(format=="csv") {
    httr::content(response, encoding="UTF-8", col_types=readr::cols())
  } else if (format=="json") {
    # TODO: use the binding type to parse the value - currently returns two columns for each binding
    parsed <- jsonlite::fromJSON(httr::content(response, encoding="UTF-8", "text"), simplifyVector = T)
    parsed$results$bindings
  }
}

default_endpoint <- function() {
  getOption("ldf.default_endpoint", "https://staging.gss-data.org.uk/sparql")
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

#' Download dimension properties
#'
#' @param dataset_uri A string
#' @param endpoint A string for the sparql endpoint
#' @return A data frame containing the dimension uris, labels and optionally codelists
#' @export
#' @examples
#' \dontrun{
#' get_cube("http://gss-data.org.uk/data/gss_data/covid-19/ons-online-price-changes-for-high-demand-products#dataset")
#' }
get_dimensions <- function(dataset_uri, endpoint=default_endpoint()) {
  q <- stringr::str_interp(c(
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
    "PREFIX qb: <http://purl.org/linked-data/cube#>",
    "",
    "SELECT ?uri ?label ?codelist WHERE {",
    "  <${dataset_uri}> qb:structure/qb:component ?component .",
    "  ?component qb:dimension ?uri .",
    "  ?uri rdfs:label ?label .",
    "  OPTIONAL { ?uri qb:codeList ?codelist }",
    "  OPTIONAL { ?component qb:codeList ?codelist }",
    "  OPTIONAL { ?component <http://publishmydata.com/def/qb/codesUsed> ?codelist }",
    "}"
  ))
  query(q, endpoint)
}

get_measures <- function(dataset_uri, endpoint=default_endpoint()) {
  q <- stringr::str_interp(c(
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
    "PREFIX qb: <http://purl.org/linked-data/cube#>",
    "",
    "SELECT ?uri ?label WHERE {",
    "  <${dataset_uri}> qb:structure/qb:component/qb:measure ?uri .",
    "  ?uri rdfs:label ?label .",
    "}"
  ))
  query(q, endpoint)
}

get_attributes <- function(dataset_uri, endpoint=default_endpoint()) {
  q <- stringr::str_interp(c(
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
    "PREFIX qb: <http://purl.org/linked-data/cube#>",
    "",
    "SELECT ?uri ?label WHERE {",
    "  <${dataset_uri}> qb:structure/qb:component/qb:attribute ?uri .",
    "  ?uri rdfs:label ?label .",
    "}"
  ))
  query(q, endpoint)
}

get_components <- function(dataset_uri, endpoint=default_endpoint()) {
  d <- get_dimensions(dataset_uri, endpoint) %>% dplyr::select(uri, label)
  m <- get_measures(dataset_uri, endpoint)
  a <- get_attributes(dataset_uri, endpoint)
  rbind(d,m,a)
}

get_observations <- function(dataset_uri,
                             endpoint=default_endpoint(),
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

  query(q, endpoint)
}

get_codelist <- function(codelist_uri, endpoint=default_endpoint()) {
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
  query(q, endpoint)
}

#' Download a Resource's label
#'
#' Provides for a very generic description, just requesting the `rdfs:label`.
#'
#' The vector of `uri`s passed to the function may contain duplicates, only
#' one description per URI will be returned (even if the database contains duplicate labels).
#'
#' If no label is found, the URI is used. This guarantees that there is a description of all URIs
#' which is required by [resource()].
#'
#' @param uri A character vector of URIs.
#' @param endpoint A string for the sparql endpoint
#' @return A data frame with column's for the `uri` and `label`
#' @examples
#' \dontrun{
#' get_label("http://purl.org/linked-data/cube#measureType")
#' }
get_label <- function(uri, endpoint=default_endpoint()) {
  uri <- unique(uri)
  uri_binding <- glue::glue_collapse(glue::glue_data(list(uri=uri), "<{uri}>"), " ")

  q <- stringr::str_interp(c(
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",

    "SELECT * ",
    "WHERE {",
    "  VALUES ?uri { ${uri_binding} }",
    "  ?uri rdfs:label ?label .",
    "}"
  ))
  results <- query(q, endpoint) %>%
    dplyr::distinct(uri, .keep_all=T) %>% # enforce one label per URI
    dplyr::mutate(label = as.character(label)) # reverse readr parsing for e.g. labels with numbers
  missing <- setdiff(uri, dplyr::pull(results, "uri"))
  vctrs::vec_rbind(results, data.frame(uri=missing, label=missing, stringsAsFactors = F))
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
#' @param endpoint A string for the sparql endpoint
#' @param include_geometry A boolean indicating whether the geometries should be downloaded (defaults to `FALSE`).
#' @return A data frame of geography descriptions
#' @export
#' @examples
#' \dontrun{
#' get_geography("http://statistics.data.gov.uk/id/statistical-geography/K02000001")
#' }
get_geography <- function(geography_uri, endpoint="http://statistics.data.gov.uk/sparql", include_geometry=FALSE) {
  geo_binding <- glue::glue_collapse(glue::glue_data(list(uri=unique(geography_uri)), "<{uri}>"), " ")
  geometry_clause <- if(include_geometry) {
    "OPTIONAL {
      ?uri <http://www.opengis.net/ont/geosparql#hasGeometry>/<http://www.opengis.net/ont/geosparql#asWKT> ?boundary;
    }"
  } else {
    ""
  }

  boundaries <- glue::glue("
SELECT * WHERE {
  VALUES ?uri { `geo_binding` }

  ?uri
    <http://statistics.data.gov.uk/def/statistical-geography#officialname> ?label;
    <http://www.w3.org/2004/02/skos/core#notation> ?notation;
    .

  `geometry_clause`

  OPTIONAL {
    ?uri <http://statistics.data.gov.uk/def/statistical-geography#parentcode> ?parent;
  }
}", .open="`", .close="`")
  geo <- query(boundaries, endpoint)
  geo %>% dplyr::distinct(uri, .keep_all=T)
}

#' Download a DataCube
#'
#' Returns a data frame with one observation per row and one column per component.
#' Components may be dimensions, measures or attributes.
#'
#' Where the column represents an RDF Resource, it will have the type `ldf_resource` vector.
#'
#' If the cube uses the `sdmx:refArea` dimension, it's values will be described using `get_geography`.
#' The descriptions will be retreived from [statistics.data.gov.uk](http://statistics.data.gov.uk/sparql)
#' instead of the specified endpoint.
#'
#' If the cube users the `sdmx:refPeriod` dimension, it's values will be described using `interval`s.
#'
#' @param dataset_uri A string
#' @param endpoint A string for the sparql endpoint
#' @param include_geometry A boolean indicating whether the geometries should be downloaded (defaults to `FALSE`).
#' @return A data frame
#' @export
#' @examples
#' \dontrun{
#' get_cube("http://gss-data.org.uk/data/gss_data/covid-19/ons-online-price-changes-for-high-demand-products#dataset")
#' }
get_cube <- function(dataset_uri, endpoint=default_endpoint(), include_geometry=FALSE) {
  d <- get_dimensions(dataset_uri, endpoint)
  m <- get_measures(dataset_uri, endpoint)
  a <- get_attributes(dataset_uri, endpoint)

  observations <- get_observations(dataset_uri, endpoint, d, m, a)

  # apply codelists to coded properties (dimensions only atm)
  codelists <- stats::setNames(d$codelist, as_variable_names(d$label)) %>%
    purrr::discard(is.na) %>%
    lapply(get_codelist, endpoint=endpoint)

  for (dimension in names(codelists)) {
    codelist <- codelists[[dimension]] %>% dplyr::distinct(uri, .keep_all=T)
    if(nrow(codelist)==0) {
      codelist_uri <- d %>%
        dplyr::filter(as_variable_names(label)==dimension) %>%
        dplyr::select(codelist)
      warning("Codelist empty or not found: ", codelist_uri)
    } else {
      observations[,dimension] <- resource(dplyr::pull(observations, dimension), codelist)
    }
  }

  # create intervals for reference period dimension
  ref_period <- as_variable_names(d[d$uri=="http://purl.org/linked-data/sdmx/2009/dimension#refPeriod", ] %>% dplyr::pull("label"))
  observations[,ref_period] <- interval(dplyr::pull(observations,ref_period))

  ref_area <- as_variable_names(d[d$uri=="http://purl.org/linked-data/sdmx/2009/dimension#refArea", ] %>% dplyr::pull("label"))
  if(length(ref_area)==1) {
    areas <- dplyr::pull(observations,ref_area)
    observations[,ref_area] <- resource(areas, get_geography(areas, "http://statistics.data.gov.uk/sparql", include_geometry))
  }

  # attributes and any remaining dimensions should just have their values labelled if possible
  remaining_d <- dplyr::filter(d, is.na(codelist) & !(uri %in% c("http://purl.org/linked-data/sdmx/2009/dimension#refPeriod", "http://purl.org/linked-data/sdmx/2009/dimension#refArea")))
  for (component in as_variable_names(c(a$label, remaining_d$label))) {
    component_values <- dplyr::pull(observations, component)
    description <- get_label(unique(component_values), endpoint)
    observations[, component] <- resource(component_values, description)
  }

  observations %>% dplyr::select(!uri)
}
