
######################################################
# create re samples for caret
#
create_samples <- function(data,nsamples,trait){
    require(caret)
    # minus one as we add Resample00. see below
    n=nsamples-1
    data_to_sample <- data
    trait <- trait
    resamps <- createResample(y=data_to_sample[,paste(trait,sep="")],
                         times=n,
                         list=FALSE)   
    # get original sample list
    Resample00 <- 1:dim(data_to_sample)[1]
    resamps <- cbind(Resample00,resamps)
    return(resamps)
}

######################################################
# smotest perc.over = 100, perc.under=200
#
smotest <- list(name = "SMOTE balanced 100 200",
                func = function (x, y) {
                  library(DMwR)
                  dat <- if (is.data.frame(x)) x else as.data.frame(x)
                  dat$.y <- y
                  dat <- SMOTE(.y ~ ., data = dat, perc.over = 100, perc.under=200)
                  list(x = dat[, !grepl(".y", colnames(dat), fixed = TRUE)],
                       y = dat$.y)
                  },
                first = TRUE)
