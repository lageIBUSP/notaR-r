#' Support to creating exercises
#' 
#' The \code{equivalent} function verifies whether two objects are
#' similar up to rounding errors. It should be used instead of \code{identical}
#' for tests that admit a small numerical inconsistency.
#' @param a,b Objects to be compared
#' @examples
#' identical (1, 1 / 3 * (1 + 1 + 1))
#' equivalent (1, 1 / 3 * (1 + 1 + 1))
#' @export
#' @rdname support
equivalent = function(a, b) 
    isTRUE(all.equal(a,b, tol=1e-7, check.attributes=FALSE))
