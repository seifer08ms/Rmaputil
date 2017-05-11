#' trim number for breaks
#'
#' This function trims number for classes breaks
#'
#' @param x numerical value to be trimmed
#' @param keep.digits boolean value to indicate whether to keep digits in the given number.
#' @export
#' @examples
#' hello(fname="Your",lname="Name")

trimnum <- function(x,keep.digits=F) {
    x<-1234.12354
    x.round<-round(x)
    x.n<-as.character(x.round)%>%nchar
    x.n.keep<-floor(x.n/2)
    x.trim<-floor(x.round/10^x.n.keep)
    x.trim<-x.trim*10^x.n.keep
    x.trim
}
