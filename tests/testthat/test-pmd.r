test_that("get_geography works", {
  vcr::use_cassette("get_geography", {
    uk <- get_geography("http://statistics.data.gov.uk/id/statistical-geography/K02000001")
  })
  expect_equal(uk$label, "United Kingdom")
})
# can show code or label
# can find parent
# can find geometry

test_that("query works", {
  vcr::use_cassette("query", {
    spo <- query("SELECT * WHERE { ?s ?p ?o } LIMIT 2")
  })
  expect_equal(nrow(spo), 2)
})

test_that("get_cube works", {
  vcr::use_cassette("get_cube", {
    cube <- get_cube("http://gss-data.org.uk/data/gss_data/covid-19/ons-online-price-changes-for-high-demand-products#dataset")
  })
  expect_equal(dim(cube), c(572,5)) # this might change when new data is loaded
})

test_that("get_label works", {
  vcr::use_cassette("get_label", {
    mt <- get_label("http://purl.org/linked-data/cube#measureType")
  })
  expect_equal(mt$label, "Measure type")
})

