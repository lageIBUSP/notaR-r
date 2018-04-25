#' Suporte à correção
#' 
#' A função \code{equivalent} verifica se dois objetos são equivalentes. 
#' Deve ser usada no lugar de "identical" para testes que admitem 
#' pequenas inconsistências numéricas
#' @examples
#' identical (1, 1 / 3 * (1 + 1 + 1))
#' equivalent (1, 1 / 3 * (1 + 1 + 1))
#' @export
#' @rdname suporte
equivalent = function(a, b) 
    isTRUE(all.equal(a,b, tol=1e-7, check.attributes=FALSE))
