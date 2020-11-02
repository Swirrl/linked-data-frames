describe("resources can be constructed", {
  test_that("with URIs as strings", {
    r <- resource(c("http://example.net/id/1","http://example.net/id/2"))
    expect_s3_class(r, "ldf_resource")
    expect_equal(length(r),2)
  })

  test_that("with URIs as factors", {
    r <- resource(factor(c("http://example.net/id/1","http://example.net/id/2")))
    expect_type(uri(r), "character")
  })
})

describe("resources can have arbitrary descriptions", {
  uris <- c("http://example.net/id/1",
            "http://example.net/id/2",
            "http://example.net/id/3")

  test_that("properties retrieved via uri", {
    r <- resource(uris[3:1], data=data.frame(uri=uris, id=1:3))
    expect_equal(property(r, "id"), 3:1)
  })

  test_that("warning is given if property is missing", {
    r <- resource(uris, data=data.frame(uri=uris))
    expect_warning(property(r, "id"))
  })

  test_that("descriptions must include uri", {
    expect_error(resource(uris, data=data.frame(id=1:3)))
  })

  test_that("data can't contain duplicate uris", {
    expect_error(resource(uris, data=data.frame(uri=uris[c(1,1,2,3)])))
  })

  test_that("data must contain all uris", {
    expect_error(resource(uris, data=data.frame(uri=uris[1:2])))
  })
})

describe("accessors", {
  uris <- c("http://example.net/id/apple",
            "http://example.net/id/banana",
            "http://example.net/id/carrot")
  labels <- c("Apple","Banana","Carrot")
  sort_priorities <- 1:3
  data <- data.frame(uri=uris,
                     label=labels,
                     sort_priority=sort_priorities,
                     stringsAsFactors = F)
  r <- resource(uris, data)

  test_that("for uri", {
    expect_equal(uri(r), uris)
  })

  test_that("for label", {
    expect_equal(label(r), labels)
  })

  test_that("for sort priority", {
    expect_equal(sort_priority(r), sort_priorities)
  })
})

describe("curie function", {
  uris <- c("http://example.net/id/apple",
            "http://example.net/id/banana",
            "http://example.net/id/carrot")
  r <- resource(uris)

  test_that("returns full URIs without prefix", {
    expect_equal(curie(r), uris)
  })

  test_that("compacts URIs with prefix", {
    expect_equal(curie(r, c(eg="http://example.net/id/")),
                 c("eg:apple", "eg:banana", "eg:carrot"))
  })

  test_that("default prefixes set in option", {
    withr::with_options(
      list(ldf_prefixes=c(eg="http://example.net/id/")),
      expect_equal(curie(r), c("eg:apple", "eg:banana", "eg:carrot"))
    )
  })
})

describe("formatter", {
  uris <- c("http://example.net/id/apple",
            "http://example.net/id/banana",
            "http://example.net/id/carrot")

  test_that("defaults to curie", {
    r <- resource(uris)
    expect_equal(format(r), uris)
  })

  test_that("uses label if available", {
    labels <- c("Apple","Banana","Carrot")
    r <- resource(uris, data=data.frame(uri=uris, label=labels, stringsAsFactors = F))
    expect_equal(format(r), labels)
  })
})

# underlying type could be integer with uri mapping
# sorts by sort priority
# ensure merges via uri - and that data is merged
# subset data when uri is subset
