######################################################
# gbm Grid: place holder
#
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
                        n.trees = (1:30)*50,
                        shrinkage = c(0.01, 0.1, 1.0),
                        n.minobsinnode = c(10,20))

######################################################
# lasso: grid tune parameters: set aplha and lambda
# seq along 100 values of lambda
# alpha 0 = ridge
# alpha 0.5 = elastic net
# aplna 1.0 = lasso
#
lasso_grid_caret <- function(data, aplha_seq=seq(0, 1, 0.5) , lambda_length=100 ){
    require(caret)
    
    aplha_seq <- aplha_seq
    lambda_length <- lambda_length
    
    nobs <- length(data[1,])
    nvars <- length(data[,1])
    min_lambda <- ifelse(nobs<nvars,0.01,0.0001)
    max_lambda <- 0.1
    lamda_seq <- seq(min_lambda, max_lambda, length.out=lambda_length)
    
    glmnet_grid <- expand.grid(alpha=aplha_seq,
                           lambda=lamda_seq)
    glmnet_grid
}

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
