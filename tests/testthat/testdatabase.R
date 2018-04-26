context("database")

test_that("database connection works", {
    con = connect()
    expect_is(con, "MySQLConnection")
})

test_that("database connection caches", {
    con1 = connect()
    con2 = connect()
    expect_identical(con1@Id, con2@Id)
})

test_that("no.results works", {
    con = connect()
    t1 = dbGetQuery(con, "SELECT 1 LIMIT 1")
    expect_false(no.results(t1))
    t2 = dbGetQuery(con, "SELECT 1 LIMIT 0")
    expect_true(no.results(t2))
})

