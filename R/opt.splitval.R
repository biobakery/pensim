opt.splitval <-
  function(optFUN="opt1D",testset="equal",scaling=TRUE,...){
    extra.vars <- list(...)
    if (!class(extra.vars$penalized)=="data.frame") stop("penalized object must be specified as a dataframe")
    if(testset[1]=="equal"){
      testset <- rep(TRUE,nrow(extra.vars$penalized))
      testset[sample(1:nrow(extra.vars$penalized),round(nrow(extra.vars$penalized)/2))] <- FALSE
      testset <- which(testset)
    }
    trainingset <- (1:nrow(extra.vars$penalized))[-testset]
    ##Define training and test data
    penalized.training <- extra.vars$penalized[trainingset,]
    if("unpenalized" %in% names(extra.vars)){
      unpenalized.training <- extra.vars$unpenalized[trainingset,]
    }
    pheno.training <- extra.vars$response[trainingset,]
    penalized.test <- extra.vars$penalized[testset,]
    if("unpenalized" %in% names(extra.vars)){
      unpenalized.test <- extra.vars$unpenalized[testset,]
    }
    pheno.test <- extra.vars$response[testset,]
    if(scaling){
      penalized.training <- scale(penalized.training)
      ##use scaling factors determined in training set to scale the test set:
      penalized.test <- sweep(penalized.test,2,attr(penalized.training,"scaled:center"))
      penalized.test <- sweep(penalized.test,2,attr(penalized.training,"scaled:scale"),FUN="/")
      penalized.training <- data.frame(penalized.training)
    }
    ##Assign training samples to extra.vars for regression:
    extra.vars$response <- pheno.training
    extra.vars$penalized <- penalized.training
    if("unpenalized" %in% names(extra.vars)){
      extra.vars$unpenalized <- unpenalized.training
    }
    ##regression call:
    output <- do.call(get(optFUN),args=extra.vars)
    ##coefficients:
    cc <- output[which.max(output[,"cvl"]),-(1:2)]  #coefficients
    ##predictions in test set:
    if("unpenalized" %in% names(extra.vars)){
      dat.test <- cbind(unpenalized.test,penalized.test)
      preds.test <- as.matrix(dat.test) %*% cc
    }else{
      preds.test <- as.matrix(penalized.test) %*% cc
    }
    return(preds.test)
  }

tmp <- data.frame(testvar1=1:nrow(dat.training),
                  testvar2=c(rep(1,20),rep(2,nrow(dat.training)-20)))

opt.splitval(nsim=2,optFUN="opt1D",testset=1:2,
             setpen="L2",
             response=surv.training,penalized=dat.training,fold=2,positive=FALSE,standardize=FALSE,trace=FALSE)

