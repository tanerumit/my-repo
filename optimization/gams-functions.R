#' Extract an object from a gdx file
#'
#' \code{extract_gdx()}
#' @param gdx a gdx file
#' @param item a gams object to be extracted
#' @return a data frame containing the extracted object
#' @export
extract_gdx <- function(gdx,item, ...) {

    res = rgdx(gdx,list(name=item), squeeze = FALSE, ...)
    df = data.frame(value=res$val[,res$dim+1])
    if (res$dim==0) { return(df) }
    for (i in 1:res$dim){
      if(res$domains[i]=='*'){
        colname=paste('V',i,sep="")
      }else{
        colname=res$domains[i]
      }
      df[[colname]] = factor(res$val[,i])
      labidx = as.numeric(levels(df[[colname]]))
      levels(df[[colname]]) = res$uels[[i]][labidx]
    }

    return(as_data_frame(df))
}