##' @title define_model2
##' 
##' @description Function to define multivariate arma model
##' (indicator form) for marima.
##'
##' @export

define_model2 <- function(..., data){
    L <- lapply(list(...), function(x){
        x <- as.character(as.formula(x))
        output <- x[[2]]
        x <- strsplit(x[[3]], "\\+")[[1]]
        # Go through the inputs
        inputs <- list()
        for(i in 1:length(x)){
            xx <- strsplit(x[i], "\\(")[[1]]
            inputs[[i]] <- eval(parse(text=paste0("c(",xx[2])))
            names(inputs)[i] <- gsub(" ","",xx[1])
        }
        return(list(output = output, inputs=inputs))
    })
    # Construct the MARIMA patterns
    # Find the output variables
    outputs <- unlist(lapply(L, function(x){ x$output }))
    # Find the input variables
    inputs <- unique(unlist(lapply(L, function(x){ names(x$inputs) })))
    inputs <- inputs[!inputs %in% c("AR","MA")]
    regvar <- inputs[!inputs %in% outputs]        
    # Take these from the data in this order
    dataout <- data[ ,c(outputs, regvar)]
    if(class(dataout)!="data.frame"){
        dataout <- data.frame(dataout)
        names(dataout) <- outputs
    }
    kvar <- ncol(dataout)
    # The lag patterns
    # First AR
    lagsall <- unlist(lapply(L, function(x){ x$input }))
    lags <- lagsall[grep("^MA", names(lagsall), invert=TRUE)]
    # Now put in the 1's
    arpat <- array(0, dim=c(kvar,kvar,max(lags)+1))
    arpat[ , ,1] <- diag(kvar)
    for(i in 1:length(outputs)){
        xx <- L[[i]]$inputs
        for(ii in 1:length(xx)){
            nm <- names(xx[ii])
            if(nm == "MA"){
                # Skip it
            }else if(nm == "AR"){
                # It is actually i in dataout
                arpat[i,i,xx[[ii]]+1] <- 1
            }else{
                # Find it in dataout
                ivar <- which(names(dataout) == nm)
                arpat[i,ivar,xx[[ii]]+1] <- 1
                ## ?? Maybe here also ivar,i must be set!
            }
        }
    }
    # The MA pattern
    lags <- lagsall[grep("^MA", names(lagsall))]
    if(length(lags) == 0){
        mapat <- array(0, dim=c(kvar,kvar,1))
    }else{
        mapat <- array(0, dim=c(kvar,kvar,max(lags)+1))
        for(i in 1:length(outputs)){
            xx <- L[[i]]$inputs
            for(ii in 1:length(xx)){
                nm <- names(xx[ii])
                if(nm == "MA"){
                    mapat[i,i,xx[[ii]]+1] <- 1
                }
            }
        }
    }
    mapat[ , ,1] <- diag(kvar)
    return(list(data=dataout, ar.pattern=arpat, ma.pattern=mapat, kvar=kvar))
}
