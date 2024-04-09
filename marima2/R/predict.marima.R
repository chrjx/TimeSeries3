##' @title predict.marima
##' 
##' @description Forecasting of (multivariate) time series of
##' using marima type model.
##'
##' @export

predict.marima <- function(fit, newdata=NULL, nstart=1, nstep=1, dif.poly=NULL, check=TRUE){
  if(is.null(newdata)){
    newdata <- fit$DATA
  }else{
    newdata <- t(newdata[ ,row.names(fit$DATA)])
  }
  invisible(arma.forecast(newdata, fit, nstart, nstep, dif.poly, check))
}
