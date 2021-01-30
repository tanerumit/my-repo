
#' Add transperancy for the color brewer
#'
#' @param col a vector of colors
#' @param alpha desired alpha level
#'
#' @return returns values
#' @export
colorBrewerAlpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  as.vector(apply(sapply(col, col2rgb)/255, 2,
    function(x) rgb(x[1], x[2], x[3], alpha=alpha)))
}