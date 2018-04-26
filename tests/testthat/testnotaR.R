context("notaR")

test_that("get_files works", {
    unlink("aves_cerrado.csv")
    get_files()
    expect_true(file.exists("aves_cerrado.csv"))
    unlink("aves_cerrado.csv")
    unlink("README")
})

test_that("get_exercise works", {
    ex = get_exercise(1)
    expect_is(ex, "list")
    # Default exercise included as example
    expect_identical(names(ex), c("tests", "preconditions"))
    expect_equal(dim(ex$tests), c(3,2))
})

test_that("correctoR works", {
    corr = correctoR(1, "x <- 1:4")
    expect_is(corr, "logical")
    expect_equal(length(corr), 3)
    expect_equal(sum(corr), 2)
    ex = list( tests = data.frame(conditions=c("exists('x')", "class(x) == 'integer'", 'length(x) == 5')), preconditions = 'y = 1:5')
    corr = correctoR(ex, "x <- 1")
    expect_is(corr, "logical")
    expect_equal(length(corr), 3)
    expect_equal(sum(corr), 1)
})

test_that("reportR works", {
    # A right answer
    corr = correctoR(1, "x <- 1:5")
    expect_equal(reportR(1, corr), "<p>Seu aproveitamento: <b>100%</b>.</p>")
    # TODO: add tests for a wrong answer and for an invalid answer
})
