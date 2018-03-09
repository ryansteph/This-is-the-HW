#' Wrapper function for ggplot2 for data d
#'
#' @param x data.frame
#'
#' @return ggplot2
#' @export
#' @examples
#' data(d)
#' plotMyData(d)
plotMyData<-function(x){
  x%>% ggplot2::ggplot()+ggplot2::aes(x=x, y=p)+ggplot2::geom_point()
}
