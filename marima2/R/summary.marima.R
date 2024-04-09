##' @title summary.marima
##'
##' @description Prints the summary
##' 
##' @export

summary.marima <- function(fit) {

  nms <- row.names(fit$DATA)

  newrow <- function(Name, Estimate, Pval){
          data.frame(Name = Name, Estimate = Estimate, Pval = Pval,
                     Stars = c("***","**","*",".","")[findInterval(Pval,c(0.001,0.01,0.05,0.1))+1])
  }

  # For each variable
  L <- list()
  # For each variable
  for(i in 1:length(nms)){
    # The table with the values
    tbl <- data.frame(Name=NA, Estimate=NA, Pval=NA, Stars=NA)[-1, ]
    # The AR part
    for(iii in 2:dim(fit$ar.estimates)[3]){
      if(fit$call.ar.pattern[i,i,iii]){
        # Make the line for the coef
        tbl <- rbind(tbl, newrow(pst("AR.l",iii-1),
                                 fit$ar.estimates[i,i,iii],
                                 fit$ar.pvalues[i,i,iii]))
      }
    }
    # For each input
    for(ii in (1:length(nms))[-i]){
      # For each lag
      for(iii in 2:dim(fit$ar.estimates)[3]){
        if(fit$call.ar.pattern[i,ii,iii]){
          # Make the line for the coef
          tbl <- rbind(tbl, newrow(pst(nms[ii],".l",iii-1),
                                   fit$ar.estimates[i,ii,iii],
                                   fit$ar.pvalues[i,ii,iii]))
        }
      }
    }
    # MA part
    # For each lag
    if(dim(fit$ma.estimates)[3] > 1){
      for(iii in 2:dim(fit$ma.estimates)[3]){
        if(fit$ma.estimate[i,i,iii] != 0){
          tbl <- rbind(tbl, newrow(pst("MA.l",iii-1),
                                   fit$ma.estimates[i,i,iii],
                                   fit$ma.pvalues[i,i,iii]))
        }
      }
    }
    if(nrow(tbl)){
      L[[nms[i]]] <- tbl
    }
  }

  return(L)
    # cat("Model dimension = kvar = ", fit$kvar, " N = ", fit$N, " \n")

    # if(pattern==TRUE){ cat('ar.pattern = \n')
    # print(short.form(x$call.ar.pattern,leading=F))
    # cat('ar.pattern = \n')
    # print(short.form(x$call.ma.pattern,leading=F))}

    ## cat("Averages for all variables:\n", x$averages, "\n")
    ## cat("Covariance(all data):\n")
    ## print(round(x$data.cov, 4))
    ## cat("Covariance(residuals):\n")
    ## print(round(x$resid.cov, 4))
    ## cat("Random variables are", x$randoms, "\n")

    ## if (pattern == TRUE) {
    ##     cat("\n")
    ##     cat("AR definition:\n")

    ##     AR <- short.form(x$call.ar.pattern, leading = FALSE)
    ##     print(AR)

    ##     cat("MA definition:\n")
    ##     MA <- short.form(x$call.ma.pattern, leading = FALSE)
    ##     print(MA)

    ##     if (pattern != TRUE) {
    ##         cat("No model identification output specified \n")
    ##     }
    ## }

    ## if (estimates == TRUE) {
    ##     cat("AR estimates:\n")
    ##     print(round(short.form(x$ar.estimates, leading = FALSE), 4))

    ##     if (fvalues == TRUE) {
    ##         cat("AR f-values (squared t-values):\n")
    ##         print(round(short.form(x$ar.fvalues, leading = FALSE), 4))
    ##     }
    ##     if (pvalues == TRUE) {
    ##         cat("AR p-values (%):\n")
    ##         print(round(100 * short.form(x$ar.pvalues, leading = FALSE), 2))
    ##     }
    ## }

    ## if (estimates != TRUE) {
    ##     cat("No estimated model output specified \n")
    ## }

    ## if (estimates == TRUE) {
    ##     cat("MA estimates:\n")
    ##     print(round(short.form(x$ma.estimates, leading = FALSE), 4))
    ##     if (fvalues == TRUE) {
    ##         cat("MA f-values (squared t-values):\n")
    ##         print(round(short.form(x$ma.fvalues, leading = FALSE), 2))
    ##     }
    ##     if (pvalues == TRUE) {
    ##         cat("MA p-values (%):\n")
    ##         print(round(100 * short.form(x$ma.pvalues, leading = FALSE), 2))
    ##     }
    ## }
    ## if (estimates != TRUE) {
    ##     cat("No estimated model output specified \n")
    ## }

}
