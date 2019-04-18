Stacked <- function(training,traintarget, testing, nfolds, ncv, ncore)
{
    library(caret)
    library(randomForest)
    library(doParallel)
    library(nnet)
    set.seed(4051)
    
    ## set up the parallel computing 
    myCluster <- makeCluster(ncore, type = "PSOCK")
    registerDoParallel(myCluster)
    
    ## Create empty spaces for upper level training
    svmpred <- rep(NA, nrow(training))
    enetpred <- rep(NA, nrow(training))
    knnpred <- rep(NA, nrow(training))
    nnetpred <- rep(NA, nrow(training))
    svmRpred <- rep(NA, nrow(training))
    lmpred <- rep(NA, nrow(training))
    
    ## Create empty spaces for upper level testing
    svmtest <- rep(NA, nrow(testing))
    enettest <- rep(NA, nrow(testing))
    knntest <- rep(NA, nrow(testing))
    nnettest <- rep(NA, nrow(testing))
    
    ## Set up the initial value for lower bound
    lower <- 0
    
    ## Time tracking
    #system.time({
    
    
    #foreach (i = 1 : nfolds) %dopar%
    for (i in 1 : nfolds)
    {
        library(caret)
        library(e1071)
        library(kernlab)
        ## Splitting the data
        upper <-  i * nrow(training)/nfolds 
        validation <- training[(lower+1):upper,]
        trainset <- training[-((lower+1):upper),]
        target <- traintarget[-((lower+1):upper)]
        
        
        ## Model fitting for svmLinear
        svmTuned <- train(y = target, x = as.data.frame(trainset),  method = "svmLinear",  
                          preProc = c("center", "scale"),  tuneLength = 20
                          ,trControl = trainControl(method = "cv", number = ncv))
        svmpred[(lower+1):upper] <- predict(svmTuned, validation)
        svmpredict <- predict(svmTuned, testing)
        
        ## Model fitting for elastic net regression
        
        enetGrid <- expand.grid(.lambda = c(0, 0.01, 1), .fraction = seq(.05, 1, length = 20)) 
        enetTune <- train(y = target, x = as.data.frame(trainset), method = "enet", 
                          tuneGrid = enetGrid, trControl = trainControl(method = "cv", number = ncv), 
                          preProc = c("center", "scale"))
        enetpred[(lower+1):upper] <- predict(enetTune, validation)
        enetpredict <- predict(enetTune, testing)
        
        ## Model fitting for KNN
        knn <- train(y = target, x = as.data.frame(trainset),  method = "knn",  
                     preProc = c("center","scale"),  tuneLength = 20,  
                     trControl = trainControl(method = "cv", number = ncv))
        knnpred[(lower+1):upper] <- predict(knn, validation)
        knnpredict <- predict(knn, testing)
        
        ## Model fitting for LDA
        tooHigh <- findCorrelation(cor(trainset), cutoff = .75) 
        trainXnnet <- trainset[, -tooHigh] 
        testXnnet <- testing[, -tooHigh] 
        nnetGrid <- expand.grid(.decay = c(0, 0.01, .1), .size = c(1:10), .bag = FALSE) 
        nnetTune <- train(y = target, x = as.data.frame(trainset),  method = "avNNet",  
                          tuneGrid = nnetGrid,  trControl = trainControl(method = "cv", number = ncv),  
                          preProc = c("center", "scale"),  linout = TRUE,  trace = FALSE,  
                          MaxNWts = 10 * (ncol(trainXnnet) + 1) + 10 + 1, maxit = 500)
        nnetpred[(lower+1):upper] <- predict(nnetTune, validation)
        nnetpredict <- predict(nnetTune, testing)
        
        
        
        ## Model fitting for svmR
        
        svmR <- train(y = target, x = as.data.frame(trainset), data = trainset,  
                      method = "svmRadial",  tuneLength = 20,   preProc = c("center", "scale"), 
                      trControl = trainControl(method = "cv", number = ncv))
        svmRpred[(lower+1):upper] <- predict(svmR, validation)
        svmRpredict <- predict(svmR, testing)
        
        control <- trainControl("repeatedcv")
        
        # Train PLS model
        #mod1 <- train(y = target, x = as.data.frame(trainset), data = trainset,
        #             method = "pls",
        #            tuneLength = 20,
        #            trControl = control,
        #             preProc = c("center","scale"))
        # plspred[(lower+1):upper] <- predict(mod1, validation)
        # plsprediction <- predict(mod1, testing)
        
        
        
        print(i)
        lower <- upper
    }
    #})
    
    control <- trainControl("repeatedcv")
    
    
    ## Building upper level training set
    uppertrain <- as.data.frame(cbind(traintarget, svmpred, enetpred, knnpred, 
                                      nnetpred, svmRpred))
    print(uppertrain)
    ## Building upper level testing set
    uppertest <- as.data.frame(cbind(svmpredict, enetpredict, knnpredict, 
                                     nnetpredict, svmRpredict))
    names(uppertest) <- c("svmpred", "svmpred", "knnpred", "nnetpred", "svmRpred")
    print(uppertest)
    ## Fitting an nonlinear upper level model using neural networks with single layer perceptron
    
    #knn1 <- train(traintarget~., data = uppertrain,  method = "knn",  tuneLength = 10,  trControl = trainControl(method = "cv", number = ncv))
    
    #svmRTuned <- train(traintarget~., data = uppertrain,  method = "svmRadial",  tuneLength = 20,  trControl = trainControl(method = "cv", number = ncv))
    rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
    finalmodel <- train(traintarget~.,data  = uppertrain, method = "brnn", 
                        tuneLength = 2,
                        trControl = rctrl1, verbose =FALSE)
    finalpredict <- predict(finalmodel, uppertest)
    return(finalpredict)
    stopCluster(myCluster)
}

dat <- read.csv(file.choose())
train <- dat[, -17]
target <- train$Total.Suspended.Solid..TSS.
target <- log(target)
train$Total.Suspended.Solid..TSS. <- NULL
prediction <-Stacked(train, target, train, 5,5,6)
prediction <- exp(prediction)
target <- exp(target)
sqrt(sum((prediction - target)^2)/50)

train1 <- dat[,-16]
target1 <- train1$Nitrate
train1$Nitrate<-NULL
prediction1 <- Stacked(train1, target1,train, 5, 5, 6)
sqrt(sum((prediction1 - target1)^2)/50)

importance <- varImp(plsTune, scale=FALSE)
print(importance)
plot(importance)
plot("modelname")
