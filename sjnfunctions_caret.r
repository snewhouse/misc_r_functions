
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

