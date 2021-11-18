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

describe("get_cube", {
  describe("works for gss-data", {
    test_that("a basic cube", {
      vcr::use_cassette("get_cube_gss_data", {
        cube <- get_cube("http://gss-data.org.uk/data/gss_data/covid-19/ons-online-price-changes-for-high-demand-products#dataset")
      })
      expect_equal(dim(cube), c(572,5)) # this might change when new data is loaded
    })

    test_that("a cube with sub-properties of sdmxd:refPeriod and sdmxd:refArea", {
      vcr::use_cassette("get_cube_gss_subproperties", {
        cube <- get_cube("http://gss-data.org.uk/data/gss_data/edvp/ofgem-warm-home-discount-scheme/nationexpenditure#dataset")
      })
      expect_equal(dim(cube), c(3, 6))
      # should be able to find intervals from sub-properties of sdmxd:refPeriod
      expect_s3_class(cube$period, "ldf_interval")
      # should be able to find geographies from sub-properties of sdmxd:refArea
      # nb: we can use this to tell it's a geography as it gets an official name instead of the rdfs:label with the GSS code
      expect_equal(sort(label(cube$nation)), c("England","Scotland","Wales"))
    })
  })

  test_that("works for statistics.gov.scot", {
    vcr::use_cassette("get_cube_scot", {
      cube <- get_cube("http://statistics.gov.scot/data/population-estimates-dependency",
                       endpoint="https://statistics.gov.scot/sparql")
    })
    expect_equal(dim(cube), c(940,5))
  })
})

describe("get_dimensions", {
  test_that("finds codelist via qb:codeList on a dimension property", {
    vcr::use_cassette("get_dimensions-dimension-codelist", {
      dimensions <- get_dimensions("http://gss-data.org.uk/data/gss_data/covid-19/mmo-ad-hoc-statistical-release-uk-sea-fisheries-statistics#dataset",
                                   endpoint="https://staging.gss-data.org.uk/sparql")
    })

    vessel_length <- dplyr::filter(dimensions, label=="Vessel Length")
    expect_equal(vessel_length$codelist,
                 "http://gss-data.org.uk/data/gss_data/covid-19/mmo-ad-hoc-statistical-release-uk-sea-fisheries-statistics#scheme/vessel-length")
  })

  test_that("finds codelist via qb:codeList on a component specification", {
    vcr::use_cassette("get_dimensions-component-codelist", {
      dimensions <- get_dimensions("http://statistics.gov.scot/data/gross-domestic-product-annual-output-by-industry",
                                   endpoint="https://statistics.gov.scot/sparql")
    })

    industry <- dplyr::filter(dimensions, label=="Industry Sector (SIC 07)")
    expect_equal(industry$codelist,
                 "http://statistics.gov.scot/def/code-list/gross-domestic-product-annual-output-by-industry/industrySector(sic07)")
  })

  test_that("finds codelist via pmd:codesUsed on a component specification", {
    vcr::use_cassette("get_dimensions-component-codesused", {
      dimensions <- get_dimensions("http://linked.nisra.gov.uk/data/tourism-data",
                                   endpoint="http://linked.nisra.gov.uk/sparql")
    })

    ref_area <- dplyr::filter(dimensions, label=="Reference Area")
    expect_equal(ref_area$codelist,
                 "http://linked.nisra.gov.uk/data/tourism-data/codes-used/geography_code")
  })
})




test_that("get_label works", {
  vcr::use_cassette("get_label", {
    mt <- get_label("http://purl.org/linked-data/cube#measureType")
  })
  expect_equal(mt$label, "Measure type")
})

