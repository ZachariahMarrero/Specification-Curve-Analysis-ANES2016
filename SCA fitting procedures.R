##########################################################################################
#These are our model fitting procedures. This script is still in the category of 'barely functions, cross your fingers and hope' but I thought it'd be good to upload anyway.  There are lots of annotations all throughout but there may be some weak spots and some other spots where there are vestiges of old code.  

#### To coincide with the fitting process, the results_frame will use NAs for each instance in which a variable isn't needed for a specification and will otherwise use the numerical index of the column.  

#The data are not standardized because this A) is a problem when different models end up using different sample sizes due to missingness (i.e., silently deleted/removed rows) and B) makes cross-validation easier. You can't standardize before cross-validating because you'll be leaking information from the test cases to the model training. (i.e. You'll overfit) We really wanted to cross-validate the models whereever possible because it's a strong but cheap robustness check. 

#Random dependence coefficient.  A type of non-linear correlation.  
rdc <- function(x,y,k=20,s=1/6,f=sin) {
  x <- cbind(apply(as.matrix(x),2,function(u)rank(u)/length(u)),1)
  y <- cbind(apply(as.matrix(y),2,function(u)rank(u)/length(u)),1)
  x <- s/ncol(x)*x%*%matrix(rnorm(ncol(x)*k),ncol(x))
  y <- s/ncol(y)*y%*%matrix(rnorm(ncol(y)*k),ncol(y))
  cancor(cbind(f(x),1),cbind(f(y),1))$cor[1]
}  


datas <-list(anes2016,nonwhite,white,republicans2016,independents2016,democrats2016)

for( i in 1:length(datas)) {
  print(i)
  Start.time <- Sys.time()  # Index start of processing 
  print(Start.time)
  anes2016 <- datas[[i]]
  Specifications <- expand.grid(
    
    DV=c( which(colnames(anes2016)%in%"supdem_mean")),
          which(colnames(anes2016)%in%"supdem1_V162263"),
          which(colnames(anes2016)%in%"supdem2_V162267"),
          which(colnames(anes2016)%in%"supdem3_V162290"),
          which(colnames(anes2016)%in%"rwa"),
          which(colnames(anes2016)%in%"rwa_2"),
          which(colnames(anes2016)%in%"childrear_sum"),
          which(colnames(anes2016)%in%"trumpliking_V162286"),
          which(colnames(anes2016)%in%"trump_gains"), #IS a factor
          which(colnames(anes2016)%in%"trump_vote"), #is a factor
          which(colnames(anes2016)%in%"trumpprimary_V161021a"), #Is a factor
          #which(colnames(anes2016)%in%"pol_participation_z")), #composite. z scored, reconstructed below
    IV=c( which(colnames(anes2016)%in%"digmedia1_V162370"),
          which(colnames(anes2016)%in%"digmedia2_V161495"),
          which(colnames(anes2016)%in%"digmedia3_V162018e"),#is factor, 2 levels
          which(colnames(anes2016)%in%"digmedia4_V162004"),
          which(colnames(anes2016)%in%"digmedia5_V161363d"), #is factor, 2 levels
          which(colnames(anes2016)%in%"digmedia_websites"),#composite sum
          which(colnames(anes2016)%in%"digital_media_z"),#composite. z-scored.
          which(colnames(anes2016)%in%"yahoo_V161444"),
          which(colnames(anes2016)%in%"daily_V161453"),
          which(colnames(anes2016)%in%"cnn_V161445"),
          which(colnames(anes2016)%in%"wapo_V161454"),
          which(colnames(anes2016)%in%"nbc_V161446"),
          which(colnames(anes2016)%in%"bi_V161455"),
          which(colnames(anes2016)%in%"huffpo_V161447"),
          which(colnames(anes2016)%in%"bbc_V161456"),
          which(colnames(anes2016)%in%"cbs_V161448"),
          which(colnames(anes2016)%in%"guard_V161457"),
          which(colnames(anes2016)%in%"usa_V161449"),
          which(colnames(anes2016)%in%"abc_V161458"),
          which(colnames(anes2016)%in%"buzz_V161450"),
          which(colnames(anes2016)%in%"nyt_V161451"),
          which(colnames(anes2016)%in%"fox_V161452"),
          which(colnames(anes2016)%in%"other_V161459"),
          which(colnames(anes2016)%in%"none_V161460"),
          NA),
    ###These covariate switches will replace the "Cov.Opts" line when the full run is executed
    # Cov1=c(which(colnames(anes2016)%in%"legmedia_z"),NA), #, #composite. z-scored.
    # Cov2=c(which(colnames(anes2016)%in%"polori2_V162289_z"),NA),#compsoite. z-scored.
    # Cov3=c(which(colnames(anes2016)%in%"relig"),NA), 
    # Cov4=c(which(colnames(anes2016)%in%"ses"),NA), 
    # Cov5=c(which(colnames(anes2016)%in%"edu_V161270"),NA),
    # Cov6=c(which(colnames(anes2016)%in%"gender_V161342"),NA),
    # Cov7=c(which(colnames(anes2016)%in%"ethn_V161310x"),NA),
    # Cov8=c(which(colnames(anes2016)%in%"age_V161267c"),NA),
    # Cov9=c(which(colnames(anes2016)%in%"tipi_o"),NA),
    # Cov10=c(which(colnames(anes2016)%in%"tipi_c"),NA),
    # Cov11=c(which(colnames(anes2016)%in%"tipi_e"),NA),
    # Cov12=c(which(colnames(anes2016)%in%"tipi_a"),NA),
    # Cov13=c(which(colnames(anes2016)%in%"tipi_n"),NA))
    Cov.opts = c("ALL","TIPI","Demographics","PoliticalOrientation","legacymedia",NA))
Specifications <- Specifications[-which(is.na(Specifications$IV) & is.na(Specifications$Cov.opts)), ] 
  #These are unique to the cross-validation procedure below.  Each of the variables here is already z-scored but we need to z-score on the training sets and then use the mean and sd of the training set vectors to z-score the test sets.  This prevents information about the data in the test sets leaking to the model fitting procedure. 
  crossvalidation.Varsets <- data.frame(Pol_participation = c(which(colnames(anes2016)%in%c("pol_participation1_V162198","pol_participation2_V161238","pol_participation3_V162018a","pol_participation4_V162018b","pol_participation5_V162018d","pol_participation6_V162017","pol_participation7_V162016","pol_participation8_V162014","pol_participation9_V162012","pol_participation10_V162011","pol_participation11_V162010"))), Digmedia =c(which(colnames(anes2016)%in%c("digmedia1_V162370","digmedia2_V161495","digmedia3_V162018e","digmedia4_V162004","digmedia5_V161363d")),rep(NA,6)),legmedia = c(which(colnames(anes2016)%in%c("legmedia1_V162003","legmedia2_V162002","legmedia3_V161363")),rep(NA,8)),poli = c(which(colnames(anes2016)%in%c("polori1_V162171", "polori2_V162289")),rep(NA,9)))
  
  
  #These pertain only to the Cov.opts line.  
  Covariate.frame <- data.frame(ALL = c(which(colnames(anes2016)%in%c("legmedia_z","polori_z","relig","ses","edu_V161270","gender_V161342","ethn_V161310x","age_V161267c","tipi_o","tipi_c","tipi_e","tipi_a","tipi_n"))),TIPI = c(which(colnames(anes2016)%in%c("tipi_o","tipi_c","tipi_e","tipi_a","tipi_n")),rep(NA,8)), Demographics = c(which(colnames(anes2016)%in%c("relig","ses","edu_V161270","gender_V161342","ethn_V161310x","age_V161267c")),rep(NA,7)),legacymedia =c(which(colnames(anes2016)%in%c("legmedia_z")),rep(NA,12)),PoliticalOrientation = c(which(colnames(anes2016)%in%c("polori_z")),rep(NA,12)))
  

  #Create a place to store results.  
  Capture.results <- setNames(data.frame(matrix(nrow=nrow(Specifications),ncol = 72)),nm=c("Mean.imputation.flag","r.squared","adj.rsquared","rmse","no.info.rate","model.p","model.f","f.df1","f.df2","IVpvalue","IVslope","IVstandarderror","AIC.model","AICc.model","RepCV10.10","RepCV10.10.sd","RepCV10.10.max","RepCV10.10.min","CV.violation.totals","CV.violation.flag","rcd.knn.Bivariate.DV.IV","MICe","Mic.e.minus.pearsonr.squared","TICe","GMIC","dcor","hhg.Minp.pvalue","hhg.Minp","zero.order.pearson.cor","zero.order.spearman.cor","partial.correlation","Any.dropped.Covs","Number.of.dropped.columns","GVLMA.globaltest.value","GVLMA.globaltest.pvalue","GVLMA.globaltest.decision","GVLMA.skewness.value","GVLMA.skewness.pvalue","GVLMA.skewness.decision","GVLMA.Kurtosis.value","GVLMA.Kurtosis.pvalue","GVLMA.Kurtosis.decision","GVLMA.LinkFunction.value","GVLMA.LinkFunction.pvalue","GVLMA.LinkFunction.decision","GVLMA.Heteroscedasticity.value","GVLMA.Heteroscedasticity.pvalue","GVLMA.Heteroscedasticity.decision","CV.violation.one.sided.flag","CV.violation.one.sided.totals","RepCV10.10.one.sided","RepCV10.10.one.sided.sd","RepCV10.10.one.sided.max","RepCV10.10.one.sided.min", "no.info.misclassifications","sum.of.predicted.probablity.errors", "mean.logloss","Multicolinearity","log.reg.overfit.flag", "Autocorrelation.lags1to10", "Acf.flag","shapiro.test.flag","one.sided.sampling.unreliable.estimate","test.join.RepCV10.10.sd","test.join.RepCV10.10.max","test.join.RepCV10.10.min","test.join.RepCV10.10","IV.CV.slopes.bin.sd","IV.CV.slopes.bin.max","IV.CV.slopes.bin.min","IV.CV.slopes.bin.minabs","IV.CV.slopes.bin"))
  
  Specifications <- cbind(Specifications,Capture.results) 
  
  
#Then we run the fitting over the rows of Specification.  When a variable isn't needed it gets assigned an NA above and is dropped. 
  
#Note: Factors are handled automatically by R.  They are dummy coded internally and R assigns the reference category to the first level of the vector.  So 3 levels where the first value is a 3 becomes 2 variables (2v3 and 1v3).  
  ###Example of R automatic internal coding: 
  # tests.factor <- factor(x=c(1,2,3),levels = c(1,2,3),labels = c("a","b","c"))
  # contrasts(tests.factor)
  if(!require(future)){install.packages("future")} 
  future::plan("multisession", workers = parallel::detectCores()) #Parallelize the processing to save time.  #This opens several R instances on your machine.  They run in the background but can be seen and terminated from your computer's activity monitor.
  #Warning: This will take up all your RAM and can make your machine unstable if you already have a lot of stuff using RAM.  Modern computers will attempt to expand the RAM by utilizing some of your hard drive space.  This is not desirable behavior because a) It hurts the longevity of your machine and b) reading and writing from the hard drive will slow down your processing.  You can mitigate these issues by having fewer applications open. In particular, you should close your internet browsers and you should close any open 'dataview' tabs at the top of RStudio window.  <- Those are ENORMOROUS memory hogs.  If your RAM is still maxed out, consider modifying the 'detectCores()' option above to detectCores()-1, which will save you exactly half of 1 core...or - 2 etc.

  
  #This is it..... This does all the work for us.  The next line after this is line 1017.
  #start here
  results <-   future.apply::future_apply(Specifications,1,function(x) {
    # x <- t(Specifications[500,])
    #In order to combine the covariates, which are called from an object that is not the 'specifications' object, we need to create a space for their column indexs to go inside of the function.  starts with z <- NA and then overwrite based on Cov in Specification object. 
    z <- NA

    if("ALL" %in% x[3]) {z <- Covariate.frame$ALL}
    
    if("TIPI" %in% x[3]){z <- Covariate.frame$TIPI[!is.na(Covariate.frame$TIPI)]}
    if("Demographics" %in%x [3]) {z <- Covariate.frame$Demographics[!is.na(Covariate.frame$Demographics)]}
    if("legacymedia" %in% x[3]){z <- Covariate.frame$legacymedia[!is.na(Covariate.frame$legacymedia)]}
    if("PoliticalOrientation" %in% x[3]){z <- Covariate.frame$PoliticalOrientation[!is.na(Covariate.frame$PoliticalOrientation)]}
    x <- x[-3]
    x <- unlist(c(x,z))
    x <- x[!is.na(x)]
    x <- as.numeric(x)
    
    #future_apply is just a parallelized version of base::apply(). Here, we take each row from object 'Specifications' and drop any NA values (where an NA value means that variable wasn't selected for modelling this specification).  
    
    lm.data <- anes2016[,x] #The row is filled with column indexes.  Those indexes will subset our dataframe. 
    
    colnames(lm.data)[1:2] <- c("DV","IV")  #The first elements of vector 'x' are the DV and IV by design.  They becomed the first columns in lm.data. 
    
    unique.values <- apply(lm.data[!is.na(lm.data$DV),],2,function(x){
      if(length(unique(x[!is.na(x)]))==0){1} else{length(unique(x[!is.na(x)]))}}) #For any variable if subsetting, based on deletion of rows that have missing values, yields a vector with no data or 1 unique value (a constant for every element of the vector) we need to remove that variable from consideration.  Alternatively we could impute missing values but it makes more sense to consider imputation techniques within their own designated specifications. 
    lm.data <- lm.data[!is.na(lm.data$DV),which(unique.values!=1)] 
    
    
    if(any(ncol(lm.data)==1,!is.element("IV",colnames(lm.data)))){
      #In the off chance that an analysis isn't feasible after removing constants and missing DV values this branch will return 'NA' for all results of that specification.  This is included to prevent the script from crashing late into the processing before any results are returned.
      results <- c(check=NA,This=NA)
    } else { #closes at end of function
      
      #Mean imputation if there are 0 complete cases in the rows of the lm.data dataframe. The functions lm() and glm() internally use listwise(a.k.a.casewise) deletion by default.  If no cases are complete then they will return errors.  To deal with this as simply as possible, we'll default to a simple mean imputation solution to run the analysis for that specification and the imputation will be flagged.
      Mean.imputation.flag <- nrow(lm.data[complete.cases(lm.data),])==0 #Returns True or False
      if(is.factor(lm.data$DV)){Mean.imputation.flag <-  length(unique(lm.data$DV[complete.cases(lm.data)]))==1} #If DV is factor complete cases greater than 0 while DV is reduced to 1 class which means models won't process. Returns True to increase case count via imputation  
      #For all variables except for the DV, convert NA values to mean of the variable.
      if(Mean.imputation.flag){
        for(i in 2:ncol(lm.data)) { 
          if(!is.factor(lm.data[,i])){ #To avoid mean imputing factors
            if(length(lm.data[is.na(lm.data[,i]),i])>0) {lm.data[is.na(lm.data[,i]), i] <- mean(unlist(lm.data[,i]), na.rm = TRUE)}}else{i}}}
      
      #For continuous DVs do lm()
      if(!is.factor(lm.data$DV)){
        
        lm.obj <- lm(formula=DV~.,data=lm.data) # The fit. This is OLS multiple regression
        lmsum <- summary(lm.obj)
        
        r.squared <- lmsum$r.squared # R-squared 
        
        adj.rsquared <- lmsum$adj.r.squared #adjusted R-squred (although AIC is better)
        rmse <- lmsum$sigma #This is the square root of the average of residuals 
        no.info.rate <- sqrt(mean((lm.data$DV[!is.na(lm.data$DV)]-mean(lm.data$DV,na.rm = T))^2)) # This is the error you would get if you predicted every value of the DV using the mean of the DV.  In other words, it's the error of the BEST fitting regression model where the slope is held at 0 and the intercept is allowed to vary. Most of the time the intercept will end up being the mean of the DV in this case.  The idea is that if adding IVs doesn't reduce the error less than this value, then the IVs do NOT reduce the average uncertainty about our DV...they convey no information. 
        
        #Repeated K-fold cross-validation. (10 by 10)
        #This is both a direct test to see whether or not a fitted model will generalize and a direct estimate of its 'prediction ability'.  The data is partitioned in to 10 segments at random.  9 of the 10 are used to fit a model and then the parameters of this fit model are applied against the data in the 10th segment.  The idea here is to create a complete separation of information between the model fitting stage and the model evaluation stage. After applying the model parameters to the test data the error can be calculated.  Across the 10 fit/test cycles, this error is averaged.  We compare this error to the 'no.info.rate' If the error is higher than the no.info.rate then it means our model makes predictions that are worse than a model that has a slope of 0 and and intercept at mean(DV), (i.e. predicts that every DV value is simply the mean of the dv).  double i.e., the fitted model would not predict and would stand essentially no chance of generalizing given that we should always expect that the data we have on hand is best approxiamted by other cases that we have on hand (triple i.e. that were collected in the same way, by the same people, at the same places, and same times). A third way to think about this is that if our model had a 'significant' pvalue but the out of sample error was higher than the no.info.rate, then the 'signficance' is illusory.  We possibly violated some modeling assumptions like normally distributed errors.
        
        #Note that ideally 10 fold CV will be repeated so as to ensure that the partitions are representative given that the data which belongs to any particular partition must necessarily not belong to another partition.  However, K-fold CV without repeats is still better than nothing. 
        
        #Nested for loops.  
        
        #We're going to change reconstruct the dataframe IF any triggers here. 
        if(!any(c("pol_participation_z","digital_media_z","legmedia_z","polori_z")%in%colnames(anes2016)[x])) {cross.val.data <- lm.data} else {
          cross.val.data <- anes2016[,c(x,unlist(crossvalidation.Varsets)[!is.na(unlist(crossvalidation.Varsets))])] }
        colnames(cross.val.data)[1:2] <- c("DV","IV")
        cross.val.data <- cross.val.data[!is.na(cross.val.data$DV),]
        
        #If any of these are not in the cross.val.data, we're going to remove them.
        if(!"pol_participation_z" %in% colnames(anes2016)[x]) {cross.val.data[,which(colnames(cross.val.data)%in% c("pol_participation1_V162198","pol_participation2_V161238","pol_participation3_V162018a","pol_participation4_V162018b","pol_participation5_V162018d","pol_participation6_V162017","pol_participation7_V162016","pol_participation8_V162014","pol_participation9_V162012","pol_participation10_V162011","pol_participation11_V162010"))] <- NULL} 
        if(!"digital_media_z" %in% colnames(anes2016)[x]) {cross.val.data[,which(colnames(cross.val.data)%in% c("digmedia1_V162370","digmedia2_V161495","digmedia3_V162018e","digmedia4_V162004","digmedia5_V161363d"))] <- NULL} #105
        if(!"legmedia_z" %in% colnames(anes2016)[x]) {cross.val.data[,which(colnames(cross.val.data)%in% c("legmedia1_V162003","legmedia2_V162002","legmedia3_V161363"))] <- NULL} #167
        if(!"polori_z" %in% colnames(anes2016)[x]) {cross.val.data[,which(colnames(cross.val.data)%in% c("polori1_V162171", "polori2_V162289"))] <- NULL} #171
        
      
        #The outer loop represents the 'repeated' and the inner loop represents the 'k-fold' 
        for(reps in 1:10){
          reps
          trigger <- FALSE
          while(trigger==FALSE){
            if(!exists('whilecontroller')){ whilecontroller <- 1} else {
              whilecontroller <- whilecontroller+1} 
          #Start by randomly shuffling the rows of the dataset so as to increase the representativeness of each partition. 
          cross.val.data<-cross.val.data[sample(nrow(cross.val.data)),] 
        
          
          #IF gender factor is part of variable set, recode and multiply by DV.index
          #has 3 levels:0,1,NA
          if("gender_V161342"%in%colnames(cross.val.data)){
          # gender.index <- replace(as.numeric(levels(anes2016$gender_V161342))[anes2016$gender_V161342],c(which(as.numeric(levels(anes2016$gender_V161342))[anes2016$gender_V161342]==1),which(as.numeric(levels(anes2016$gender_V161342))[anes2016$gender_V161342]==2)),values = c(10,5))
            DV.index <- car::recode(as.numeric(levels(cross.val.data$gender_V161342))[cross.val.data$gender_V161342],"1=10;0=.5;NA=66") }
          #   DV.index <- as.numeric(levels(gender.index))[gender.index]}
          # #IF ethnicity factor is part of the variable set, recode and multiply by DV.index
          # #has 7 levels: 1,2,3,4,5,6,NA
          if("ethn_V161310x"%in%colnames(cross.val.data)){
            Ethn.index <- car::recode(as.numeric(levels(cross.val.data$ethn_V161310x))[cross.val.data$ethn_V161310x],"0=21.1;1=10.9;2=.71;3=45.9;4=630;5=701.1;6=123.123;NA=.009871")
          
          if(exists("DV.index")){DV.index <- DV.index*Ethn.index} else {DV.index <- Ethn.index}}
          # #Maximum of 2*7*3: 42
          # print("hello.iv")
          if(is.factor(cross.val.data[,which(colnames(cross.val.data)%in%"IV")])){
            IV.index <- car::recode(as.numeric(levels(cross.val.data$IV))[cross.val.data$IV],"1=13.5;0=17;NA=71")
          # print("hello.iv.done")
          # #Create 10 partitions of approximatley equal size
            if(exists("DV.index")){
         DV.index <- DV.index*IV.index} else {DV.index <- IV.index} }
         
          if(exists("DV.index")){
            if(whilecontroller<2){
           folds <- caret::createFolds(DV.index,k=10)} else { folds <- caret::createFolds(DV.index,k=20)} } else {
             if(whilecontroller<2){
             folds <- cut(seq(1,nrow(cross.val.data)),breaks=10,labels=FALSE) } else {
               folds <- cut(seq(1,nrow(cross.val.data)),breaks=20,labels=FALSE)   }
           }
          
          #Create 10 partitions of approximatley equal size
        #  folds <- cut(seq(1,nrow(cross.val.data)),breaks=10,labels=FALSE)
          
          #The 10 fold cross validation inner loop
          for(k in 1:length(unique(folds))){
            
            #Segement your data by fold using the which() function 
            # testIndexes <- which(folds==k,arr.ind=TRUE)
            if(exists("DV.index")) {testIndexes <- folds[[k]]} else {
              testIndexes <- which(folds==k,arr.ind=TRUE)}
            
            testData <- cross.val.data[testIndexes, ]
            trainData <- cross.val.data[-testIndexes, ]
            
            # lm() and glm() can't make predictions if the test data contain unfitted factor levels.  This code ensures there are no 'new' levels in the test data.  This is the equivalent of removing a contrast from a model.  So if there were 3 levels but only 2 were used in training (1v2) then in the testing any (1v3) or (2v3) would be removed.
            for(i in 1:ncol(trainData)){
              if(unlist(lapply(trainData,is.factor))[i]) {
                
                vec1 <- data.frame(trainData[,i],trainData[,i])
                colnames(vec1)[1] <- "vec1"
                
                vec2 <- data.frame(testData[,i],testData[,i])
                colnames(vec2)[1] <- "vec2"
                #Read as: Extract the unique levels in the test data that are not in the train data
                levels.not.in.training <-!as.numeric(levels(vec2$vec2))[vec2$vec2] %in% as.numeric(levels(vec1$vec1))[vec1$vec1]
                vec2$vec2[levels.not.in.training] <- NA
                vec2$vec2 <- droplevels(vec2$vec2)
                #Read as: Remove all unused levels not in the test data. This doesn't convert any values to NA. It just redefines what levels belong to the factor.
                testData[,i] <- vec2$vec2
              }
            }
            # testData[!as.numeric(levels(testData$ethn_V161310x))[testData$ethn_V161310x]%in%as.numeric(levels(trainData$ethn_V161310x))[trainData$ethn_V161310x],grep("ethn_V161310x",colnames(testData))] <- NA
            # testData$ethn_V161310x <- droplevels(testData$ethn_V161310x)
            #Whenever working with standardized variables (z-scores), we need to standardize within the training set independently of the cases in the test set to prevent information about the test cases influencing the model fitting process.  Since we'll also need to standardize the test set for predictions, we'll keep the means and sample standard deviations of the training set variables that were standardized and apply them to the corresponding variables in the test set.  This mimcs the real-life scenario where we encounter a brand new data point and cannot compute a mean or sd. In the outlined conditions below, we are also creating composite variables by summation based on those standardized variables.  
            
            #Computes the means and sample sd for each variable
            trainmeans <- unlist(lapply(trainData,function(y){ if(is.factor(y)){NA} else {mean(y,na.rm=T)}}))
            trainsds <- unlist(lapply(trainData,function(y){ if(is.factor(y)){NA} else {sqrt(var(y,na.rm = T)*((length(y[!is.na(y)])-1)/length(y[!is.na(y)])))}}))
            
            
            #This is where we'll standardize.  This chunk 'rebuilds' each standardized variable based on the values computed above.  
            if("pol_participation_z" %in% colnames(anes2016)[x]){
              cols <- c(which(colnames(trainData)%in%c("pol_participation1_V162198","pol_participation2_V161238","pol_participation3_V162018a","pol_participation4_V162018b","pol_participation5_V162018d","pol_participation6_V162017","pol_participation7_V162016","pol_participation8_V162014","pol_participation9_V162012","pol_participation10_V162011","pol_participation11_V162010")))
              for (Qi in 1:length(cols)){
                trainData[,cols[Qi]] <-  (trainData[,cols[Qi]]-trainmeans[cols[Qi]])/trainsds[cols[Qi]]
                testData[,cols[Qi]] <-  (testData[,cols[Qi]]-trainmeans[cols[Qi]])/trainsds[cols[Qi]]}
              trainData$DV <- rowSums(trainData[,cols],na.rm = T)
              testData$DV <- rowSums(testData[,cols],na.rm = T)
              # trainData[,cols] <- NULL
              # testData[,cols]  <- NULL
              trainData[,which(colnames(trainData)%in%c("pol_participation1_V162198","pol_participation2_V161238","pol_participation3_V162018a","pol_participation4_V162018b","pol_participation5_V162018d","pol_participation6_V162017","pol_participation7_V162016","pol_participation8_V162014","pol_participation9_V162012","pol_participation10_V162011","pol_participation11_V162010"))] <- NULL
              testData[,which(colnames(testData)%in%c("pol_participation1_V162198","pol_participation2_V161238","pol_participation3_V162018a","pol_participation4_V162018b","pol_participation5_V162018d","pol_participation6_V162017","pol_participation7_V162016","pol_participation8_V162014","pol_participation9_V162012","pol_participation10_V162011","pol_participation11_V162010"))] <- NULL
            }
            if("digital_media_z" %in% colnames(anes2016)[x]){
              cols <- c(which(colnames(trainData)%in%c("digmedia1_V162370","digmedia2_V161495","digmedia3_V162018e","digmedia4_V162004","digmedia5_V161363d")))
              for (Qi in 1:length(cols)){
                trainData[,cols[Qi]] <-  (trainData[,cols[Qi]]-trainmeans[cols[Qi]])/trainsds[cols[Qi]]
                testData[,cols[Qi]] <-  (testData[,cols[Qi]]-trainmeans[cols[Qi]])/trainsds[cols[Qi]]}
              trainData$IV <- rowSums(trainData[,cols],na.rm = T)
              testData$IV <- rowSums(testData[,cols],na.rm = T)
              # trainData[,cols] <- NULL
              # testData[,cols]  <- NULL
              trainData[,which(colnames(trainData)%in%c("digmedia1_V162370","digmedia2_V161495","digmedia3_V162018e","digmedia4_V162004","digmedia5_V161363d"))] <- NULL
              testData[,which(colnames(testData)%in%c("digmedia1_V162370","digmedia2_V161495","digmedia3_V162018e","digmedia4_V162004","digmedia5_V161363d"))] <- NULL
            }
            if("legmedia_z" %in% colnames(cross.val.data)){
              cols <- c(which(colnames(trainData)%in%c("legmedia1_V162003","legmedia2_V162002","legmedia3_V161363")))
              for (Qi in 1:length(cols)){
                trainData[,cols[Qi]] <-  (trainData[,cols[Qi]]-trainmeans[cols[Qi]])/trainsds[cols[Qi]]
                testData[,cols[Qi]] <-  (testData[,cols[Qi]]-trainmeans[cols[Qi]])/trainsds[cols[Qi]]}
              trainData$legmedia_z <- rowSums(trainData[,cols],na.rm = T)
              testData$legmedia_z <- rowSums(testData[,cols],na.rm = T)
              # trainData[,cols] <- NULL
              # testData[,cols]  <- NULL
              trainData[,which(colnames(trainData)%in%c("legmedia1_V162003","legmedia2_V162002","legmedia3_V161363"))] <- NULL
              testData[,which(colnames(testData)%in%c("legmedia1_V162003","legmedia2_V162002","legmedia3_V161363"))] <- NULL
            }
            if("polori_z" %in% colnames(cross.val.data)){
              cols <- c(which(colnames(trainData)%in%c("polori1_V162171", "polori2_V162289")))
              for (Qi in 1:length(cols)){
                trainData[,cols[Qi]] <-  (trainData[,cols[Qi]]-trainmeans[cols[Qi]])/trainsds[cols[Qi]]
                testData[,cols[Qi]] <-  (testData[,cols[Qi]]-trainmeans[cols[Qi]])/trainsds[cols[Qi]]}
              trainData$polori_z <- rowSums(trainData[,cols],na.rm = T)
              testData$polori_z <- rowSums(testData[,cols],na.rm = T)
              # trainData[,cols] <- NULL
              # testData[,cols]  <- NULL
              trainData[,which(colnames(trainData)%in%c("polori1_V162171", "polori2_V162289"))] <- NULL
              testData[,which(colnames(trainData)%in%c("polori1_V162171", "polori2_V162289"))] <- NULL
              
            }
            
            #Use the test and train data partitions to fit, then evaluate.
            
            #Because lm() requires vectors with sd(x)>0 we have to remove any constants. The object 'CV.violations.flag' below will record the freq. of such cases for consideration later on.
            
            factors <- unlist(lapply(trainData,is.factor))
            
            unique.values2 <-apply(trainData,2,function(x){
              if(is.factor(x)){10} else {
              if(length(unique(x[!is.na(x)]))==0){1} else{length(unique(x[!is.na(x)]))}}})
            trainData <- trainData[,which(unique.values2!=1)] 
            
            factors <- unlist(lapply(trainData,is.factor))
            
            for (i in 1:ncol(trainData)) {
              i
              
              if(!factors[i]) {
                tmp <- as.numeric(as.data.frame(trainData[,i])[,1])
                #trainData[is.na(trainData[,i]),i] <- mean(tmp,na.rm = TRUE)
                # replace(trainData[,i],is.na(trainData[,i]),mean(tmp,na.rm = TRUE))
                tmp[is.na(tmp)] <- mean(tmp,na.rm = TRUE)
                trainData[,i] <- tmp
                
                remove(tmp)
              }
            }
            factors <- unlist(lapply(testData,is.factor))
            
            for (i in 1:ncol(testData)) {
              i
              
              if(!factors[i]) {
                tmp <- as.numeric(as.data.frame(testData[,i])[,1])
                #trainData[is.na(trainData[,i]),i] <- mean(tmp,na.rm = TRUE)
                # replace(trainData[,i],is.na(trainData[,i]),mean(tmp,na.rm = TRUE))
                tmp[is.na(tmp)] <- mean(tmp,na.rm = TRUE)
                testData[,i] <- tmp
                
                remove(tmp)
              }
            }
            
            
            
            #trainData <- apply(trainData,2,function(x){ if(is.factor(x)){x} else {x[is.na(x)] <- mean(x,na.rm=T)}})
            # for(i in 1:ncol(trainData)){
            #   if(!exists("tmp")){tmp <- c()}
            #   if(factors[i]){
            #     if(any(length(unique(trainData[complete.cases(trainData),i]))<=1, length(unique(trainData[,i]))==1)){
            #       tmp <- c(tmp,i)
            #     }
            #   }
            #   if(i == ncol(trainData)){trainData[,tmp] <- NULL }
            # }
            # remove(tmp)
            #Fit the model
            
           if( "NA/NaN/Inf in 'x'" %in% tryCatch(lm(formula=DV~.,data=trainData),error=function(w)return(w))$message){
             trainData[trainData==Inf] <- NA
             trainData[trainData==-Inf] <- NA
           }
           if( 'contrasts can be applied only to factors with 2 or more levels' %in% tryCatch(lm(formula=DV~.,data=trainData),error=function(w)return(w))$message){
             test.error <- NA
             if(!exists("error.10cv")){error.10cv <- c()} #creates an object to store each of the 10 error estimates for the Inner loop
             error.10cv <- c(error.10cv,test.error) 
             
             if(!exists("IV.CV.slopes")){IV.CV.slopes <- c()}
             
             IV.CV.slopes <- c(IV.CV.slopes,NA)
             
           } else {
            print("pre.lm.cont")
          
            lm.CV <- lm(formula=DV~.,data=trainData)
            print("hello4.lm.cv")
            if(is.numeric(trainData$IV)){testData$IV <- as.numeric(testData$IV)}
            #If a factor is used in the fitting, where the fitting has missing data, levels of that factor may go 'unused' due to internal casewise deletion.  If that occurs, but the 'unused' levels appear in the test set, R will throw an error about the levels because R will 'think' they are 'new' These conditions below, determine if a factor from ethnicity or gender was used in the model fitting and will ensure that the factor levels are consistent between the test and the levels ultimatley selected for the model in fitting, by taking the set of levels common to both objects. 
            if("ethn_V161310x"%in%colnames(trainData)){
              lm.CV$xlevels[["ethn_V161310x"]] <- union(lm.CV$xlevels[["ethn_V161310x"]], levels(testData$ethn_V161310x))}
            if("gender_V161342"%in%colnames(trainData)){
              lm.CV$xlevels[["gender_V161342"]] <- union(lm.CV$xlevels[["gender_V161342"]], levels(testData$gender_V161342))}
            
            #Use the model to predict the unfitted data
            preds <-  predict(lm.CV,newdata=testData[complete.cases(testData),])

              
            #evaluate the error for OLS reg this is the mean squared error or root mean squared error.
          print('lvl0')
              if(!exists("predbin")){predbin <- data.frame(ys=as.numeric(),responses=as.numeric())} 
            if(nrow(testData[complete.cases(testData),])==0){
              
              print('lvl1')
              toadd <- data.frame(ys=NA,responses=NA) } else {
                toadd <- data.frame(ys=testData$DV[complete.cases(testData)],responses=preds)#[!is.na(preds)])
              }
            
                predbin <- rbind(predbin,toadd)
              
            
            test.error <- sqrt(mean((testData$DV[complete.cases(testData)]-preds)^2)) #computes the test error
            if(!exists("error.10cv")){error.10cv <- c()} #creates an object to store each of the 10 error estimates for the Inner loop
            error.10cv <- c(error.10cv,test.error) 
    
    
            if(!exists("IV.CV.slopes")){IV.CV.slopes <- c()}
            
            IV.CV.slopes <- c(IV.CV.slopes,lm.CV$coefficients[grep("IV",names(lm.CV$coefficients))[1]])
            # IV.CV.slopes <- c(IV.CV.slopes, paste(names(lm.CV$coefficients),sep = " "))
          # }}}}
    # return(if(exists("IV.CV.slopes")){IV.CV.slopes}else{"nothing"})})
            
            Number.of.CV.violations <- ifelse(ncol(trainData)==length(unique.values2[which(unique.values2>1)]),length(unique.values2)-ncol(trainData),0) #How many columns were dropped if any columns were dropped at all. 
            if(!exists("CV.violations.capture")){CV.violations.capture <- c()}
            CV.violations.capture <- c(CV.violations.capture,Number.of.CV.violations)
           } #closes else 
            if(whilecontroller <2) {
            if(length(error.10cv[!is.na(error.10cv)])>=5) {  trigger <- TRUE }} else
            {if(length(error.10cv[!is.na(error.10cv)])>=15) {  trigger <- TRUE } }
         
            }
          } #closes 'while' and #switching back to the outer loop
            
         # }  
          
          CV.violations.capture <- sum(CV.violations.capture,na.rm = T) 
          
         
            test.join.error.10cv <- sqrt(mean((predbin$ys[complete.cases(predbin)]-predbin$responses[complete.cases(predbin)])^2)) #computes the test error

            
          
          #Error estimates are aggregated by taking the mean within each of the 10 fold CVs and then taking the mean of these means when the outer loop is complete.
        test.join.error.10cv <- mean(test.join.error.10cv,na.rm = T) 
          error.10cv <- mean(error.10cv,na.rm = T) # Mean of inner-loop CV. 
          IV.CV.slopes <- mean(IV.CV.slopes,na.rm = T)
          if(!exists("RepCV10.10")){
            RepCV10.10 <- c()
            test.join.RepCV10.10 <- c()
            IV.CV.slopes.bin <- c()
          }
          RepCV10.10 <- c(RepCV10.10,error.10cv)
          test.join.RepCV10.10 <- c(test.join.RepCV10.10,test.join.error.10cv)
          IV.CV.slopes.bin <- c(IV.CV.slopes.bin,IV.CV.slopes)
          if(!exists("CV.violation.totals")){CV.violation.totals <- c()}
          CV.violation.totals <-c(CV.violation.totals, CV.violations.capture)
          
          remove("IV.CV.slopes","test.join.error.10cv","error.10cv","CV.violations.capture") #Removes inner loop objects before moving on to the next 'rep'
        } #ends the outer loop
        IV.CV.slopes.bin <- IV.CV.slopes.bin[!is.nan(IV.CV.slopes.bin)]
        IV.CV.slopes.bin.sd <- sd(IV.CV.slopes.bin,na.rm=T)
        IV.CV.slopes.bin.max <- max(IV.CV.slopes.bin,na.rm=T)
        IV.CV.slopes.bin.min <- min(IV.CV.slopes.bin,na.rm = T)
        IV.CV.slopes.bin.minabs <- min(abs(IV.CV.slopes.bin[!is.na(IV.CV.slopes.bin)]),na.rm = T)
        IV.CV.slopes.bin <- mean(IV.CV.slopes.bin,na.rm = T)
        RepCV10.10.sd <- sd(RepCV10.10,na.rm = T) #sd of iiner loop means.  
        RepCV10.10.max <- max(RepCV10.10,na.rm = T) #determine worst error rate. 
        RepCV10.10.min <- min(RepCV10.10,na.rm = T) #determine best error rate. (for creating worst case scenario out-of-sample semipartial correlation estimate)
        test.join.RepCV10.10.sd <- sd(test.join.RepCV10.10,na.rm = T)
        test.join.RepCV10.10.max <- max(test.join.RepCV10.10,na.rm = T)
        test.join.RepCV10.10.min <- min(test.join.RepCV10.10,na.rm = T)
        test.join.RepCV10.10 <- mean(test.join.RepCV10.10,na.rm = T)
        
        RepCV10.10 <- mean(RepCV10.10,na.rm = T) #mean of the inner loop means. 
        CV.violation.totals <- sum(CV.violation.totals,na.rm = T) #Total frequency of violations
        CV.violation.flag <- ifelse(CV.violation.totals>0,TRUE,FALSE) #flag for violations
        
        
        #####Quick and dirty linear modeling assumptions checks
        
        Autocorrelation.lags1to10 <- as.numeric(length(which(car::durbinWatsonTest(lm.obj,max.lag=10)$p<.2)))
        Acf.flag <- Autocorrelation.lags1to10>1 
        shapiro.test.flag <-  stats::shapiro.test(lm.obj$residuals)$p.value<.2
        
        global.assumption.checks <- gvlma::gvlma(lm.obj) 
        
        # based on: Pena, EA and Slate, EH (2006). “Global validation of linear model assumptions,” J.\ Amer.\ Statist.\ Assoc., 101(473):341-354. 
        GVLMA.globaltest.value <- global.assumption.checks$GlobalTest$GlobalStat4$Value
        GVLMA.globaltest.pvalue <- global.assumption.checks$GlobalTest$GlobalStat4$pvalue
        GVLMA.globaltest.decision <- ifelse(global.assumption.checks$GlobalTest$GlobalStat4$Decision==1, "FALSE","TRUE")
        
        GVLMA.skewness.value <- global.assumption.checks$GlobalTest$DirectionalStat1$Value
        GVLMA.skewness.pvalue <- global.assumption.checks$GlobalTest$DirectionalStat1$pvalue
        GVLMA.skewness.decision <- ifelse(global.assumption.checks$GlobalTest$DirectionalStat1$Decision==1, "FALSE","TRUE")
        GVLMA.Kurtosis.value <- global.assumption.checks$GlobalTest$DirectionalStat2$Value
        GVLMA.Kurtosis.pvalue <- global.assumption.checks$GlobalTest$DirectionalStat2$pvalue
        GVLMA.Kurtosis.decision <- ifelse(global.assumption.checks$GlobalTest$DirectionalStat2$Decision==1, "FALSE","TRUE")
        GVLMA.LinkFunction.value <- global.assumption.checks$GlobalTest$DirectionalStat3$Value
        GVLMA.LinkFunction.pvalue <- global.assumption.checks$GlobalTest$DirectionalStat3$pvalue
        GVLMA.LinkFunction.decision <- ifelse(global.assumption.checks$GlobalTest$DirectionalStat3$Decision==1, "FALSE","TRUE")
        GVLMA.Heteroscedasticity.value <- global.assumption.checks$GlobalTest$DirectionalStat4$Value
        GVLMA.Heteroscedasticity.pvalue <- global.assumption.checks$GlobalTest$DirectionalStat4$pvalue
        GVLMA.Heteroscedasticity.decision <- ifelse(global.assumption.checks$GlobalTest$DirectionalStat4$Decision==1, "FALSE","TRUE")
        
        
        ####Extracting linear model summary statistics and parameter estimates.
        
        model.p <- pf(lmsum$fstatistic[1],lmsum$fstatistic[2],lmsum$fstatistic[3],lower.tail=FALSE) #Overall model pvalue 'global' value
        model.f <- lmsum$fstatistic[1] #F-value 
        f.df1 <- lmsum$fstatistic[2]  # Degrees of Freedom F(df1,df2)
        f.df2 <- lmsum$fstatistic[3]
        
        IVpvalue <- lmsum$coefficients[2,4] #Pvalue of target IV. 
        IVslope <-  lmsum$coefficients[2,1] #Slope estimate of target IV
        IVstandarderror <- lmsum$coefficients[2,2]
        AIC.model <- AIC(lm.obj) #AIC, it's relative.  Models with lower AIC fit the data better. But AIC does not convey information about the absolute goodness of the fit in that the rmse could still be worse than the no.info.rate
        AICc.model <- AICcmodavg::AICc(lm.obj) # AIC corrected for small samples.  When k = # of parameters in the model and n = sample size.  If n/k is less than 40 AICc is recommended (See: Burnham & Anderson, 2002)
        
        lm.cors <- lm.data[,c(1,2)]
        lm.cors <- lm.cors[complete.cases(lm.cors),]
        
        if(any(is.factor(lm.cors$DV),is.factor(lm.cors[,2]))) {rcd.knn.Bivariate.DV.IV <- NA} else {
          rcd.knn.Bivariate.DV.IV <- rcd::rcd(lm.cors$DV,lm.cors$IV,method = "knn")} #Robust Copula Dependence.  This is a form of estimating 'correlation' (broadly defined)' WITHOUT making assumptions regarding the parameters of the marginal pdfs. This number will be a more stable estimate of the relationship between the IV and DV regardless of how individual cases are dropped due to missingness. To the extent that this value is greater in magnitude than the linear correlation estimates, we can infer that this metric is implying nonlinear dependence.
        if(is.factor(lm.cors$IV)){
          MICe <- NA
          Mic.e.minus.pearsonr.squared <- NA
          TICe <- NA
          GMIC <- NA
          dcor <- NA} else {
            #MICe and TICe.  #Yakir A. Reshef, David N. Reshef, Hilary K. Finucane, Pardis C. Sabeti, and Michael Mitzenmacher. 2016. Measuring dependence powerfully and equitably. J. Mach. Learn. Res. 17, 1 (January 2016), 7406-7468.
            mic <- minerva::mine(lm.cors$DV,lm.cors$IV,est = "mic_e") #MIC and TIC.  There's no free lunch in estimation.  rcd isn't perfect for all forms of dependence.  So we include the Maximal Information Coefficient and the Total Information Coefficient
            MICe <- mic$MIC #Maximal informiaton coefficient 
            Mic.e.minus.pearsonr.squared <- mic$`MIC-R2` #MIC minus pearson r squared 
            TICe <- mic$TIC   #the total information coeffieicent 
            GMIC <- mic$GMIC #the generalized mean information coefficient 
            
            dcor <- energy::dcor2d(lm.cors$DV,lm.cors$IV)} #Székely, Gábor J.; Rizzo, Maria L.; Bakirov, Nail K. Measuring and testing dependence by correlation of distances. Ann. Statist. 35 (2007), no. 6, 2769--2794. doi:10.1214/009053607000000505. https://projecteuclid.org/euclid.aos/1201012979
        
        #Randomized dependence coefficient averaged over 50 estimations.  #D. Lopez-Paz, P. Hennig, and B. Scho ̈lkopf, “The randomized dependence coefficient,” in NeurIPS, 2013, pp. 1–9.
        RDC <-mean(unlist(lapply(seq_len(50), function(x)(rdc(lm.cors$DV,lm.cors$IV)))))
        
        # HHG.  The coefficients above lack strong power (with the exception of TICe possibly) to test for independence.  The HHG is a distribution free test specifically designed to maximize power for independence testing. 
        # Heller, R., Heller, Y., Kaufman S., Brill B, & Gorfine, M. (2016). Consistent Distribution-Free K-Sample and Independence Tests for Univariate Random Variables, JMLR 17(29):1-54
        if(is.factor(lm.cors$IV)){
          NullTable_for_N_Large_MXL_tables <- NA
          hhg.Minp.pvalue <- NA
          hhg.Minp <- NA} else{
            NullTable_for_N_Large_MXL_tables = HHG::Fast.independence.test.nulltable(nrow(lm.cors), variant = 'ADP-EQP-ML',nr.atoms = 30,nr.perm=200)  #Calcualtes Null distributions
            
            hhg.Minp.pvalue <- mean(unlist(lapply(seq_len(50), function(x)(HHG::Fast.independence.test(lm.cors$DV,as.vector(lm.cors$IV),NullTable_for_N_Large_MXL_tables)$MinP.pvalue))))
            
            hhg.Minp <- mean(unlist(lapply(seq_len(50), function(x)(HHG::Fast.independence.test(lm.cors$DV,as.vector(lm.cors$IV),NullTable_for_N_Large_MXL_tables)$MinP))))}
        
        if(is.factor(lm.cors$IV)){
          zero.order.pearson.cor <- ltm::biserial.cor(lm.cors$DV,lm.cors$IV)
          zero.order.spearman.cor <- NA
        } else {
          #zero-order pearson correlation 
          zero.order.pearson.cor <- cor(lm.cors$DV,lm.cors$IV)
          #zero-order spearman correlation 
          zero.order.spearman.cor <- cor(lm.cors$DV,lm.cors$IV,method = "spearman")}
        #partial correlation 
        partial.correlation <- rsq::pcor(lm(DV~.,data=lm.data[complete.cases(lm.data),]))$partial.cor[1] #Squared Partial correlation is “the proportion the variance in Y that is not explained by covariate(s) that can be uniquely by X”(Hayes, 2013). This can be acheived by parceling out variance from y shared with covariates, parceling out variance in x shared with covariates, then taking the correlation of the remaining variance between x and y
        # rsq::rsq.partial 
        # ppcor::spcor()
        #These are extra objects returned from the binary DV branch of the function below.  
        CV.violation.one.sided.flag <-NA
        CV.violation.one.sided.totals <-NA
        RepCV10.10.one.sided <- NA
        RepCV10.10.one.sided.sd <- NA
        RepCV10.10.one.sided.max <- NA 
        RepCV10.10.one.sided.min <- NA
        no.info.misclassifications <-NA
        sum.of.predicted.probablity.errors <-NA
        mean.logloss <- NA
        Multicolinearity <- NA
        log.reg.overfit.flag <- NA
        one.sided.sampling.unreliable.estimate <- NA
        
        # Mean.imputation.flag <- 1
        # r.squared <-2
        # adj.rsquared <-3
        # rmse <- 4
        # no.info.rate <- 5
        # model.p <-6
        # model.f <-7
        # f.df1 <-8
        # f.df2 <-9
        # IVpvalue <-10
        # IVslope <- 11
        # IVstandarderror <-12
        # AIC.model <- 13
        # AICc.model <- 14
        # RepCV10.10 <-15
        # RepCV10.10.sd <- 16
        # RepCV10.10.max <- 17
        # RepCV10.10.min <- 18
        # CV.violation.totals <- 19
        # CV.violation.flag <- 20
        # rcd.knn.Bivariate.DV.IV <- 21
        # MICe <- 22
        # Mic.e.minus.pearsonr.squared <- 23
        # TICe <-24
        # GMIC <-25
        # dcor <- 26
        # hhg.Minp.pvalue <-27
        # hhg.Minp <- 28
        # zero.order.pearson.cor <- 29
        # zero.order.spearman.cor <- 30
        # partial.correlation <- 31
        # Any.dropped.Covs <- 32
        # Number.of.dropped.columns <- 33
        # GVLMA.globaltest.value <- 34
        # GVLMA.globaltest.pvalue <- 35
        # GVLMA.globaltest.decision <- 36
        # GVLMA.skewness.value <- 37
        # GVLMA.skewness.pvalue <- 38
        # GVLMA.skewness.decision <- 39
        # GVLMA.Kurtosis.value <- 40
        # GVLMA.Kurtosis.pvalue <- 41
        # GVLMA.Kurtosis.decision <- 42
        # GVLMA.LinkFunction.value <- 43
        # GVLMA.LinkFunction.pvalue <- 44
        # GVLMA.LinkFunction.decision <- 45
        # GVLMA.Heteroscedasticity.value <- 46
        # GVLMA.Heteroscedasticity.pvalue <- 47
        # GVLMA.Heteroscedasticity.decision <- 48
        # CV.violation.one.sided.flag <- 49
        # CV.violation.one.sided.totals <- 50
        # RepCV10.10.one.sided <- 51
        # RepCV10.10.one.sided.sd <- 52
        # RepCV10.10.one.sided.max <- 53
        # RepCV10.10.one.sided.min <- 54
        # no.info.misclassifications <- 55
        # sum.of.predicted.probablity.errors <- 56
        # mean.logloss <- 57
        # Multicolinearity <- 58
        # log.reg.overfit.flag <- 59
        
        #Autocorrelation.lags1to10  
        #Acf.flag 
        # shapiro.test.flag
        
      } #Closes function option for Dependent variables that are continuous 
      
      #For dependent variables that are binary we'll use logisitc regression
      if(is.factor(lm.data$DV)) {
        # if(length(unique(lm.data$DV[!is.na(lm.data$DV)]))<2) [next]] #if DV has 1 level, return all NA
        
        #Fit the logistic regression
        null.glm <- glm(DV~1,data=lm.data[complete.cases(lm.data),],family = "binomial")
        log.reg <-  glm(DV~.,data=lm.data,family = "binomial")
        log.sum <- summary(log.reg)
        
        #Glm returns a warning message when there is an 'esentially perfect fit' that results in predicted probabilites of 0 or 1.... which is akin to getting an r.squared of 1.0.
        log.reg.overfit.flag <- "glm.fit: fitted probabilities numerically 0 or 1 occurred" %in% tryCatch(glm(DV~.,data=lm.data,family = "binomial"),warning=function(w)return(w))$message
        
        r.squared <- rsq::rsq(log.reg,type="n") #Nagelkerke N (1991) A note on a general definition of the coefficient of determination. Biometrika, 78: 691-692.
        adj.rsquared <-  rsq::rsq(log.reg,adj = TRUE,type = "v") #Zhang, D. (2016). A coefficient of determination for generalized linear models. The American Statistician
        
        #Checking model for multicolinearity violations. As was the case for the OLS regressions, we'll simply flag any cases with violations and deal with them later.  There are fewer assumptions to check here.  Three main are that multicolinearity is low, that the IVs are linearly related to the log-odds of the DV, and that the DV is a binary outcome. 
        # 1. multicolinearity flagged here. 2. IV logodds to be assessed with a host of non-linear correlation measures (e.g. rcd,rdc,MICe,dcor). 3.Binary outcome was assessed with 'is.factor()' condition. 
        
        Multicolinearity <- ifelse(length(log.reg$coefficients)==2,NA,as.numeric(length(car::vif(log.reg)[which(car::vif(log.reg)>10)])))
        
        #The 'no.info.rate' for a categorical problem is the amount of logloss that would occur if we were to use no information from our predictors and instead simply predict that every case belongs to the majority class... the modal class...we can characterize this in the total number of cases that are misclassified or in terms of an (quasi-average percentage error) (e.g. mean logloss which ranges from 0 to Inf)
        
        preds <- log.reg$fitted.values
        
        #predicted probabilites range from 0 to 1.  True probabilities are 0 or 1. Rather than using a threshold for determining the category (e.g. above or below 50%) it's better to make no decision and instead take the difference.  This way, a case which is actually class 1 but get predicted probablity of 80% is not characterized as 100% accurate.  Instead, such a case is 20% inaccurate. This approach allows us to better characterize the amount of uncertainty in the model's predictions.  One can imagine a scenario where a class 1 case is predicted with only 51% or even 50.01%.  Using the 'decision rule' approach would give us an incorrect amount of confidence in how well the model could distinguish this case.  
        
        sum.of.predicted.probablity.errors <- sum(Metrics::ll(actual=as.numeric(levels(lm.data$DV[complete.cases(lm.data)]))[lm.data$DV[complete.cases(lm.data)]],predicted=preds))
        
        mean.logloss <- Metrics::logLoss(actual=as.numeric(levels(lm.data$DV[complete.cases(lm.data)]))[lm.data$DV[complete.cases(lm.data)]],predicted=preds)
        
        # mean.logloss formula... (-1/n)*sum(true.Ys*log(pred.Ys)+(1-true.Ys)*log(1-preds.Ys)) where log = the natural log.
        
        #((-1/length(as.numeric(levels(lm.data$DV[!is.na(lm.data$DV)]))[lm.data$DV][!is.na(preds)]))* 
        
        #   sum(as.numeric(levels(lm.data$DV[!is.na(lm.data$DV)]))[lm.data$DV][!is.na(preds)]
        #   * log(preds[!is.na(preds)])
        #   + (1-as.numeric(levels(lm.data$DV[!is.na(lm.data$DV)]))[lm.data$DV][!is.na(preds)])
        #   * log(1-preds[!is.na(preds)]),na.rm = T))
        
        tbl <- table(lm.data$DV[complete.cases(lm.data)])
        
        majority.class <- names(tbl[order(tbl,decreasing = T)])[1]
        
        #This is the number of cases that would be misclassified if we simply chose the majority class for prediction every time. (i.e. we used none of the information form our predictors to make the prediction).
        no.info.misclassifications <- as.numeric(length(as.numeric(levels(lm.data$DV[complete.cases(lm.data)]))[lm.data$DV[complete.cases(lm.data)]])-tbl[majority.class])
        
        #This is the 'quasi-mean percentage error' from the no information classification scheme
        no.info.rate <- no.info.misclassifications/length(as.numeric(levels(lm.data$DV[complete.cases(lm.data)]))[lm.data$DV[complete.cases(lm.data)]])
        
        ###Cross-validation procedure for logistic regression.  Two-types.  1. 10 folds with 10 repeats.  2. one-sided sampling with 10 folds and 10 repeats 
        
        
        #We're going to change from lm.data to something else IF any triggers here. 
        if(!any(c("pol_participation_z","digital_media_z","legmedia_z","polori_z")%in%colnames(anes2016)[x])){cross.val.data <- lm.data}
        else {
          cross.val.data <- anes2016[,c(x,unlist(crossvalidation.Varsets)[!is.na(unlist(crossvalidation.Varsets))])] }
        colnames(cross.val.data)[1:2] <- c("DV","IV")
        cross.val.data <- cross.val.data[!is.na(cross.val.data$DV),]
        
        
        if(!"pol_participation_z" %in% colnames(anes2016)[x]) {cross.val.data[,which(colnames(cross.val.data)%in% c("pol_participation1_V162198","pol_participation2_V161238","pol_participation3_V162018a","pol_participation4_V162018b","pol_participation5_V162018d","pol_participation6_V162017","pol_participation7_V162016","pol_participation8_V162014","pol_participation9_V162012","pol_participation10_V162011","pol_participation11_V162010"))] <- NULL} 
        if(!"digital_media_z" %in% colnames(anes2016)[x]) {cross.val.data[,which(colnames(cross.val.data)%in% c("digmedia1_V162370","digmedia2_V161495","digmedia3_V162018e","digmedia4_V162004","digmedia5_V161363d"))] <- NULL} #105
        if(!"legmedia_z" %in% colnames(anes2016)[x]) {cross.val.data[,which(colnames(cross.val.data)%in% c("legmedia1_V162003","legmedia2_V162002","legmedia3_V161363"))] <- NULL} #167
        if(!"polori_z" %in% colnames(anes2016)[x]) {cross.val.data[,which(colnames(cross.val.data)%in% c("polori1_V162171", "polori2_V162289"))] <- NULL} #171
        
        
        #Nested for loops.  
        
        #The outer loop represents the 'repeated' and the inner loop represents the 'k-fold' 
        for(reps in 1:10){
          reps
          trigger <- FALSE
          while(trigger==FALSE){
            if(!exists('whilecontroller')){ whilecontroller <- 1} else {
              whilecontroller <- whilecontroller+1} 
  
          #Start by randomly shuffling the rows of the dataset so as to increase the representativeness of each partition. 
          cross.val.data<-cross.val.data[sample(nrow(cross.val.data)),]
          #When factors are present, we have to stratify the sampling so that the levels of the factor are balanced across the training and test sets as much as possible. 
          #With multiple factors, a simple solution is to convert to numeric and then multiply across rows of factors to produce a single column upon which to stratify.  It's important to choose new values for the respective factors such that the product of the mulitplication across the rows corresponds to UNIQUE combinations of the respective factors.  With 3 factors, each with 2 levels, that should be ((1,1,1),(1,1,2),(1,2,1),(1,2,2),(2,1,1),(2,1,2),(2,2,1),(2,2,2)) or 2*2*2 unique combinations. 
          #Recode DV in a new index.  
          #has 2 levels: 0,1
          DV.index <- car::recode(as.numeric(levels(cross.val.data$DV))[cross.val.data$DV],"0=3;1=22")
          # DV.index <- as.numeric(levels(DV.index))[DV.index] # Converts to numeric
          
          #IF gender factor is part of variable set, recode and multiply by DV.index
          #has 3 levels:0,1,NA
          if("gender_V161342"%in%colnames(cross.val.data)){
            gender.index <- car::recode(as.numeric(levels(cross.val.data$gender_V161342))[cross.val.data$gender_V161342],"1=10;0=.5;NA=.0001")
            DV.index <- gender.index*DV.index}
          #IF ethnicity factor is part of the variable set, recode and multiply by DV.index
          #has 7 levels: 1,2,3,4,5,6,NA
          if("ethn_V161310x"%in%colnames(cross.val.data)){
            Ethn.index <- car::recode(as.numeric(levels(cross.val.data$ethn_V161310x))[cross.val.data$ethn_V161310x],"0=.71;1=12.5;2=23.2;3=45.9;4=630;5=701.1;6=123.123;NA=.009871") 
            DV.index <- Ethn.index*DV.index}
          #Maximum of 2*7*3: 42
          print("hello.iv")
          if(is.factor(cross.val.data[,which(colnames(cross.val.data)%in%"IV")])){
            IV.index <- car::recode(as.numeric(levels(cross.val.data$IV))[cross.val.data$IV],"1=12345;0=6666;NA=8888")
            DV.index <- IV.index*DV.index}
          DV.index <- as.factor(DV.index)
          print("hello.iv.done")
          
            if(whilecontroller<2){
              folds <- caret::createFolds(DV.index,k=10)} else { folds <- caret::createFolds(DV.index,k=20)}  
          #Create 10 partitions of approximatley equal size
         # folds <- caret::createFolds(DV.index,k=10) #cross.val.data$DV[!is.na(cross.val.data$DV)]
          #For a binary outcome we need to ensure that each class has representation in the folds. Randomly partitioning without considering the distribution of classes would often result in folds where the minority class has no representation at all and the code will crash. The easiest solution is to create folds where the internal class distributions are approxiamtely the same as the class distributions for the full sample  (e.g. the distribution of classes in each fold will track prop.table(table(lm.data$DV))).The 5 lines below are essentially what caret::createFolds(lm.data$DV,k=10) does.
          # class1 <- lm.data[lm.data$DV==1,]
          # class0 <- lm.data[lm.data$DV==0,]
          # class1.folds <- cut(seq(1,nrow(class1)),breaks=10,labels=FALSE)
          # class0.folds <- cut(seq(1,nrow(class0)),breaks=10,labels=FALSE)
          #fold1 <- c(which(class1.folds==k,arr.ind=TRUE),which(class0.folds==k,arr.ind=TRUE))
          #The 10 fold cross validation/one-sided smapling inner loop
          for(k in 1:length(unique(folds))){
            #Segement your data  
            testIndexes <- folds[[k]]
            testData <- as.data.frame(cross.val.data[testIndexes, ])
            trainData <- as.data.frame(cross.val.data[-testIndexes, ])
            
            #Whenever working with standardized variables (z-scores), we need to standardize within the training set independently of the cases in the test set to prevent information about the test cases influencing the model fitting process.  Since we'll also need to standardize the test set for predictions, we'll keep the means and sample standard deviations of the training set variables that were standardized and apply them to the corresponding variables in the test set.  This mimcs the real-life scenario where we encounter a brand new data point and cannot compute a mean or sd. In the outlined conditions below, we are also creating composite variables by summation based on those standardized variables.  
            
            #Computes the means and sample sd for each variable
            trainmeans <- unlist(lapply(trainData,function(yi){ if(is.factor(yi)){NA} else {mean(yi,na.rm=T)}}))
            trainsds <- unlist(lapply(trainData,function(yi){ if(is.factor(yi)){NA} else {sqrt(var(yi,na.rm = T)*((length(yi[!is.na(yi)])-1)/length(yi[!is.na(yi)])))}}))
            print('mat error')   
            
            if("digital_media_z" %in% colnames(cross.val.data)){
              cols <- c(which(colnames(trainData)%in%c("digmedia1_V162370","digmedia2_V161495","digmedia3_V162018e","digmedia4_V162004","digmedia5_V161363d")))
              for (Qi in 1:length(cols)){
                trainData[,cols[Qi]] <- (trainData[,cols[Qi]]-trainmeans[cols[Qi]])/trainsds[cols[Qi]]
                testData[,cols[Qi]] <-  (testData[,cols[Qi]]-trainmeans[cols[Qi]])/trainsds[cols[Qi]]}
              trainData$IV <- rowSums(trainData[,cols],na.rm = T)
              testData$IV <- rowSums(testData[,cols],na.rm = T)
              # trainData[,cols] <- NULL
              # testData[,cols]  <- NULL
              trainData[,which(colnames(trainData)%in%c("digmedia1_V162370","digmedia2_V161495","digmedia3_V162018e","digmedia4_V162004","digmedia5_V161363d"))] <- NULL
              testData[,which(colnames(testData)%in%c("digmedia1_V162370","digmedia2_V161495","digmedia3_V162018e","digmedia4_V162004","digmedia5_V161363d"))] <- NULL
            }
            
            print('digme error')
            if("legmedia_z" %in% colnames(cross.val.data)){
              cols <- c(which(colnames(trainData)%in%c("legmedia1_V162003","legmedia2_V162002","legmedia3_V161363")))
              for (Qi in 1:length(cols)){
                trainData[,cols[Qi]] <-  (trainData[,cols[Qi]]-trainmeans[cols[Qi]])/trainsds[cols[Qi]]
                testData[,cols[Qi]] <-  (testData[,cols[Qi]]-trainmeans[cols[Qi]])/trainsds[cols[Qi]]}
              trainData$legmedia_z <- rowSums(trainData[,cols],na.rm = T)
              testData$legmedia_z <- rowSums(testData[,cols],na.rm = T)
              # trainData[,cols] <- NULL
              # testData[,cols]  <- NULL
            trainData[,which(colnames(trainData)%in%c("legmedia1_V162003","legmedia2_V162002","legmedia3_V161363"))] <- NULL
            testData[,which(colnames(testData)%in%c("legmedia1_V162003","legmedia2_V162002","legmedia3_V161363"))] <- NULL
            }
            print('legme error')
            if("polori_z" %in% colnames(cross.val.data)){
              cols <- c(which(colnames(trainData)%in%c("polori1_V162171", "polori2_V162289")))
              for (Qi in 1:length(cols)){
                trainData[,cols[Qi]] <-  (trainData[,cols[Qi]]-trainmeans[cols[Qi]])/trainsds[cols[Qi]]
                testData[,cols[Qi]] <-  (testData[,cols[Qi]]-trainmeans[cols[Qi]])/trainsds[cols[Qi]]}
              trainData$polori_z <- rowSums(trainData[,cols],na.rm = T)
              testData$polori_z <- rowSums(testData[,cols],na.rm = T)
              # trainData[,cols] <- NULL
              # testData[,cols]  <- NULL
              trainData[,c(which(colnames(trainData)%in%c("polori1_V162171", "polori2_V162289")))] <- NULL
              testData[,c(which(colnames(trainData)%in%c("polori1_V162171", "polori2_V162289")))] <- NULL
              
            }
            print('polor error')
            
            ###One-sided sampling. 
            #Sometimes the class balance in the outer dataset is 'extremely' imbalanced.  In these situations the model parameters can be biased towards the majority class and the out-of-sample predictions of the minority class will suffer as a result.  To mitigate this problem we can employ a second stage of partioning called one-sided sampling.  This technique handles this issue by (semi-) randomly subsampling the majority class in the training set so that the class distributions are more balanced. In our verion here we'll take any 'dropped' cases and move them over to the test data to make full use of the data (but this isn't always done/necessary if you have a very large sample to work with). By forcing a class balance during training the model may produce better out-of-sample predictions which allows us to make a stronger statement about the predictive utility of our predictors on our DV that is 'more' sample 'agnositc'. Note that there are other techniques, such as oversampling the minority class or SMOTE (both of which would require synthesizing data) that could also deal with the imbalance problem and that the class imbalance problem is an active area of research because of its diffulty. Because of the issue is unresolved one-sided sampling is arguably as good as any other technique.  For those who are concerned about manipulation, bear in mind that the models generated by cross-validation procedures are specifically evaluated against the out-of-sample predictions.  So if the estimated parameters don't generalize, the errors will still be worse than the no.info.rate. For perfectly balanced data the no.info.rate is -log(.5) ~ 0.6931472, where log is the natural log.
            print('mat error2')
            #Find the majority class
            train.tbl <- table(trainData$DV[!is.na(trainData$DV)])
            majority.class.trainData <- names(train.tbl[order(train.tbl,decreasing = T)])[1]
            
            #Index of cases for simplicity
            trainData$index <- 1:nrow(trainData)
            
            #New training dataset.  Start with the minority cases. 
            new.trainData <-trainData[trainData$DV != majority.class.trainData,]
            #Sub-set the majority class from the set of all majority class cases.  Here we're forcing the sub-set to be equal in size to the minority class (i.e. perfect class balance)
            majority.cases <- dplyr::sample_n(trainData[-new.trainData$index,],nrow(new.trainData),replace=FALSE)
            print('rbind error')
            #Combine the sub-set and the minority classes into new training set.
            new.trainData <- rbind(new.trainData,majority.cases)
            print('not rbind')
            #Separate the remaining majority class cases from old training set. 
            move.to.testData <- trainData[!trainData$index%in%new.trainData$index,]
            
            #Remove index columns 
            new.trainData$index <- NULL
            move.to.testData$index <- NULL
            trainData$index <- NULL
            print('rbind2')
            #Combine the remaining majority class cases with the test set.
            one.sided.sampling.testData <- rbind(testData[,which(colnames(testData)%in%colnames(move.to.testData))],move.to.testData)
            
            #cleanup
            remove(move.to.testData,majority.cases,train.tbl)
            
            # lm() and glm() can't make predictions if the test data contain unfitted factor levels.  This code is a check to that there are no 'new' levels in the test data.  This is the equivalent of removing a contrast from a model.  So if there were 3 levels but only 2 were used in training (1v2) then in the testing any (1v3) or (2v3) would be removed.
            for(i in 1:ncol(trainData)){
              if(unlist(lapply(trainData,is.factor))[i]) {
                vec1 <- data.frame(trainData[,i],trainData[,i])
                colnames(vec1)[1] <- "vec1"
                
                vec2 <- data.frame(testData[,i],testData[,i])
                colnames(vec2)[1] <- "vec2"
                
                vec3 <- data.frame(new.trainData[,i],new.trainData[,i])
                colnames(vec3)[1] <- "vec3"
                
                vec4 <- data.frame(one.sided.sampling.testData[,i],one.sided.sampling.testData[,i])
                colnames(vec4)[1] <- "vec4"
                print('rbi d3')
                #Read as: Extract the unique levels in the test data that are not in the train data
                levels.not.in.training <-!as.numeric(levels(vec2$vec2))[vec2$vec2] %in% as.numeric(levels(vec1$vec1))[vec1$vec1]
                vec2$vec2[levels.not.in.training] <-NA
                vec2$vec <- droplevels(vec2$vec)
                
                OSS.levels.not.in.training <-!as.numeric(levels(vec4$vec4))[vec4$vec4] %in% as.numeric(levels(vec3$vec3))[vec3$vec3]
                
                vec4$vec4[OSS.levels.not.in.training] <-NA
                vec4$vec4 <- droplevels(vec4$vec4)
                #Read as: Remove all unused levels not in the test data. This doesn't convert any values to NA. It just redefines what levels belong to the factor.
                
                testData[,i] <- vec2$vec
                one.sided.sampling.testData[,i] <- vec4$vec4
              }
            }
            
            print('mat error3')
            #Use the test and train data partitions to fit, then evaluate.
            
            #Because glm() requires vectors with sd(x)>0 we have to remove any constants. The objects 'CV.violations.flag', 'CV.violations.one.sided.flag' below will record the freq. of such cases for consideration later on. Here we're determing the number of unique values in each varaible.  For factors there is a redudant 'for' loop just in case the 'apply' function fails to treat the factor properly.
            factors <- unlist(lapply(trainData,is.factor))
            
            unique.values2 <-apply(trainData,2,function(x){
              if(is.factor(x)){10} else { 
              if(length(unique(x[!is.na(x)]))==0){1} else{length(unique(x[!is.na(x)]))}}})
            trainData <- trainData[,which(unique.values2!=1)] 
         
            factors <- unlist(lapply(trainData,is.factor))
            
            for (i in 1:ncol(trainData)) {
              i
              
              if(!factors[i]) {
                tmp <- as.numeric(as.data.frame(trainData[,i])[,1])
                #trainData[is.na(trainData[,i]),i] <- mean(tmp,na.rm = TRUE)
                # replace(trainData[,i],is.na(trainData[,i]),mean(tmp,na.rm = TRUE))
                tmp[is.na(tmp)] <- mean(tmp,na.rm = TRUE)
                trainData[,i] <- tmp
                
                remove(tmp)
              }
            }#Mean imputation  
            
            factors <- unlist(lapply(testData,is.factor))
            
            for (i in 1:ncol(testData)) {
              i
              
              if(!factors[i]) {
                tmp <- as.numeric(as.data.frame(testData[,i])[,1])
                #trainData[is.na(trainData[,i]),i] <- mean(tmp,na.rm = TRUE)
                # replace(trainData[,i],is.na(trainData[,i]),mean(tmp,na.rm = TRUE))
                tmp[is.na(tmp)] <- mean(tmp,na.rm = TRUE)
                testData[,i] <- tmp
                
                remove(tmp)
              }
            }
            
            #R is obnoxious with factors.  This repeats the same process above just to make sure factors were evaluated properly except this time we're also removing factors if after accounting for the internal casewise deletion of glm, they have 1 level remaining.  (That casewise deletion is unfortunate but required to do the matrix math that the function depends on)
            # factors <- unlist(lapply(trainData,is.factor))
            # for(i in 2:ncol(trainData)){
            #   if(!exists("tmp")){tmp <- c()}
            #   if(factors[i]){
            #     if(any(length(unique(trainData[complete.cases(trainData),i]))<=1, length(unique(trainData[,i]))==1)){
            #       tmp <- c(tmp,i)
            #     }
            #   }
            #   if(i == ncol(trainData)){trainData[,tmp] <- NULL }
            # }
            # remove(tmp)
            factors <- unlist(lapply(new.trainData,is.factor))
            
            unique.values.one.sided <-apply(new.trainData,2,function(x){
              if(is.factor(x)){10} else {
              if(length(unique(x[!is.na(x)]))==0){1} else{length(unique(x[!is.na(x)]))}}})
            new.trainData <- new.trainData[,which(unique.values.one.sided!=1)] 
          
              if(ncol(as.matrix(new.trainData))<=1) {one.sided.sampling.unreliable.estimate  <- TRUE
            new.trainData <- trainData} else {one.sided.sampling.unreliable.estimate <- FALSE}
            print("new.traindata.error.resolved")
            
            
            factors <- unlist(lapply(new.trainData,is.factor))
            
            for (i in 1:ncol(new.trainData)) {
              i
              
              if(!factors[i]) {
                tmp <- as.numeric(as.data.frame(new.trainData[,i])[,1])
                #trainData[is.na(trainData[,i]),i] <- mean(tmp,na.rm = TRUE)
                # replace(trainData[,i],is.na(trainData[,i]),mean(tmp,na.rm = TRUE))
                tmp[is.na(tmp)] <- mean(tmp,na.rm = TRUE)
                new.trainData[,i] <- tmp
                
                remove(tmp)
              }
            }
            factors <- unlist(lapply(one.sided.sampling.testData,is.factor))
            
            for (i in 1:ncol(one.sided.sampling.testData)) {
              i
              
              if(!factors[i]) {
                tmp <- as.numeric(as.data.frame(one.sided.sampling.testData[,i])[,1])
                #trainData[is.na(trainData[,i]),i] <- mean(tmp,na.rm = TRUE)
                # replace(trainData[,i],is.na(trainData[,i]),mean(tmp,na.rm = TRUE))
                tmp[is.na(tmp)] <- mean(tmp,na.rm = TRUE)
                one.sided.sampling.testData[,i] <- tmp
                
                remove(tmp)
              }
            }
            
            
            print("new.traindata.error")
          
            if(ncol(as.matrix(new.trainData))>1){
              factors <- unlist(lapply(new.trainData,is.factor))
              for(i in 1:ncol(new.trainData)){
                if(!exists("tmp")){tmp <- c()}
                if(factors[i]){
                  if(any(length(unique(new.trainData[complete.cases(new.trainData),i]))<=1, length(unique(new.trainData[,i]))==1)){
                    tmp <- c(tmp,i)
                  }
                }
                if(i == ncol(new.trainData)){new.trainData[,tmp] <- NULL }
              }
              remove(tmp)
            }
            print("hello4.new.traindata length 0 resolved again")
            #  for(i in 1:ncol(new.trainData)){
            # if(factors[i]){
            #   if(length(unique(new.trainData[complete.cases(new.trainData),i]))==1){
            #     new.trainData[,i] <- NULL
            #     unique.values.one.sided[i] <- 1
            #   } else {
            #   if(length(unique(new.trainData[,i]))==1) {
            #     new.trainData[,i] <- NULL
            #     unique.values.one.sided[i] <- 1
            #    }
            #   }
            #  }
            # }
            
            
            if( "NA/NaN/Inf in 'x'" %in% tryCatch(glm(formula=DV~.,data=trainData,family = "binomial"),error=function(w)return(w))$message){
              trainData[trainData==Inf] <- NA
              trainData[trainData==-Inf] <- NA
            }
            if( any(nrow(testData[complete.cases(testData),])==0,'contrasts can be applied only to factors with 2 or more levels' %in% tryCatch(glm(formula=DV~.,data=trainData,family = "binomial"),error=function(w)return(w))$message)){
              test.error <- NA
              if(!exists("error.10cv")){error.10cv <- c()} #creates an object to store each of the 10 error estimates for the Inner loop
              error.10cv <- c(error.10cv,test.error) 
              
              if(!exists("IV.CV.slopes")){IV.CV.slopes <- c()}
              
              IV.CV.slopes <- c(IV.CV.slopes,NA)
              
            } else {
            
            
            
            #Fit the models
            log.reg.CV <- glm(formula=DV~.,data=trainData,family = "binomial")
            print("dv not found here")
            if(all(ncol(as.matrix(new.trainData))>1,"DV"%in%colnames(new.trainData))){log.reg.one.sided.CV <- glm(formula=DV~.,data=new.trainData,family = "binomial")} else {log.reg.one.sided.CV <- log.reg.CV}
            print("hello5.new.train.fixed")
            #In some cases factor levels will be mismatched at this stage due to the requirement for complete cases during model fitting.  This just sets the internal definition of factor levels to the union of levels found across the training and test sets. 
            
            if(all("IV"%in%colnames(trainData),is.factor(trainData$IV))){
              log.reg.CV$xlevels[["IV"]] <- union(log.reg.CV$xlevels[["IV"]], levels(testData$IV))}
            if(ncol(as.matrix(new.trainData))>1){
              if(all("IV"%in%colnames(new.trainData),is.factor(new.trainData$IV))){
                log.reg.one.sided.CV$xlevels[["IV"]] <- union(log.reg.one.sided.CV$xlevels[["IV"]], levels(one.sided.sampling.testData$IV))}} else {
                  if(all("IV"%in%colnames(trainData),is.factor(trainData$IV))){
                    log.reg.one.sided.CV$xlevels[["IV"]] <- union(log.reg.one.sided.CV$xlevels[["IV"]], levels(one.sided.sampling.testData$IV))}}
            if("gender_V161342"%in%colnames(trainData)){
              log.reg.CV$xlevels[["gender_V161342"]] <- union(log.reg.CV$xlevels[["gender_V161342"]], levels(testData$gender_V161342))}
            if("ethn_V161310x"%in%colnames(trainData)){
              log.reg.CV$xlevels[["ethn_V161310x"]] <- union(log.reg.CV$xlevels[["ethn_V161310x"]], levels(testData$ethn_V161310x))}
            if("gender_V161342"%in%colnames(new.trainData)){
              log.reg.one.sided.CV$xlevels[["gender_V161342"]] <- union(log.reg.one.sided.CV$xlevels[["gender_V161342"]], levels(one.sided.sampling.testData$gender_V161342))}
            if("ethn_V161310x"%in%colnames(new.trainData)){
              log.reg.one.sided.CV$xlevels[["ethn_V161310x"]] <- union(log.reg.one.sided.CV$xlevels[["ethn_V161310x"]], levels(one.sided.sampling.testData$ethn_V161310x))}
            
            print("fixed.another.new.traindata error.glm")
            if('IV'%in%colnames(trainData)){
            if(is.numeric(trainData$IV)){testData$IV <- as.numeric(testData$IV)}
            if(is.numeric(new.trainData$IV)){ one.sided.sampling.testData$IV <- as.numeric( one.sided.sampling.testData$IV)}}
            print("fixed.another.new.traindata error2.glm")
            #Use the models to predict the unfitted data
            preds <-  predict(log.reg.CV,newdata=testData[complete.cases(testData),],type = "response")
            print("hello6")                  
            if(ncol(as.matrix(new.trainData))>1) { preds.one.sided <-  predict(log.reg.one.sided.CV,newdata= one.sided.sampling.testData[complete.cases(one.sided.sampling.testData),],type = "response")} else {preds.one.sided <-preds}
            print("hello7")
            
            
            #compute the test errors
            if(!exists("predbin")){predbin <- data.frame(ys=testData$DV[complete.cases(testData)],responses=preds[!is.na(preds)])} else {
              predbin <- rbind(predbin,data.frame(ys=testData$DV[complete.cases(testData)],responses=preds[!is.na(preds)]))
            }
            
            #for logistic reg this is the same as mean.logloss defined above(~line 250)
            
            test.error <- Metrics::logLoss(actual=as.numeric(levels(testData$DV[complete.cases(testData)]))[testData$DV[complete.cases(testData)]],predicted=preds)
            
            test.error.one.sided <- Metrics::logLoss(actual=as.numeric(levels(one.sided.sampling.testData$DV[complete.cases(one.sided.sampling.testData)]))[one.sided.sampling.testData$DV[complete.cases(one.sided.sampling.testData)]],predicted=preds.one.sided)
            
            #Capture the errors of each fold
            if(!exists("error.10cv")){error.10cv <- c()} #creates an object to store each of the 10 error estimates for the Inner loop
            error.10cv <- c(error.10cv,test.error)
            
            if(!exists("IV.CV.slopes")){ IV.CV.slopes <- c()}
           
            IV.CV.slopes <- c(IV.CV.slopes,log.reg.CV$coefficients[grep("IV",names(log.reg.CV$coefficients))[1]])
            
            if(!exists("error.10cv.one.sided")){error.10cv.one.sided <- c()} #creates an object to store each of the 10 error estimates for the Inner loop for one.sided sampling procedure
            error.10cv.one.sided <- c(error.10cv.one.sided,test.error.one.sided) 
            
            print("fixed.another.new.traindata error2.2")
            
            #How many CV violations were there in this fold
            #Read as 'How many columns were dropped if any columns were dropped at all?' 
            Number.of.CV.violations <- ifelse(ncol(trainData)==length(unique.values2[which(unique.values2>1)]),length(unique.values2)-ncol(trainData),0) #gets overwritten each fold
            
            Number.of.CV.violations.one.sided <- ifelse(ncol(new.trainData)==length(unique.values.one.sided[which(unique.values.one.sided>1)]),length(unique.values.one.sided)-ncol(new.trainData),0) #gets overwritten each fold
            print("fixed.another.new.traindata error3.3")
            
            
            #Store the Number of CV violations before fitting on the next fold
            if(!exists("CV.violations.capture")){CV.violations.capture <- c()}
            CV.violations.capture <- c(CV.violations.capture,Number.of.CV.violations)
            
            if(!exists("CV.violations.one.sided.capture")){CV.violations.one.sided.capture <- c()}
            CV.violations.one.sided.capture <- c(CV.violations.one.sided.capture,Number.of.CV.violations.one.sided)
            }
            if(whilecontroller <2) {
              if(length(error.10cv[!is.na(error.10cv)])>=5) {  trigger <- TRUE }} else { if(length(error.10cv[!is.na(error.10cv)])>=15) {  trigger <- TRUE } }
            
            }
          } #closes 'while' and  #switching back to the outer loop
            
            
            
          # }
          
          test.join.error.10cv <- Metrics::logLoss(actual=as.numeric(levels(predbin$ys[complete.cases(predbin)]))[predbin$ys[complete.cases(predbin$ys)]],predicted=predbin$responses)
          
          #Take the sum of the inner loop CV violations 
          CV.violations.capture <- sum(CV.violations.capture,na.rm = T) # gets overwritten each outer loop iter.
          CV.violations.one.sided.capture <- sum(CV.violations.one.sided.capture,na.rm = T) 
          
          #Store the CV violations of each outer loop to prevent overwritting
          if(!exists("CV.violation.totals")){CV.violation.totals <- c()}
          CV.violation.totals <-c(CV.violation.totals, CV.violations.capture)
          
          if(!exists("CV.violation.one.sided.totals")){CV.violation.one.sided.totals <- c()}
          CV.violation.one.sided.totals <-c(CV.violation.one.sided.totals, CV.violations.one.sided.capture)
          
          #Error estimates are aggregated by taking the mean within each of the 10 fold CVs and then taking the mean of these means when the outer loop is complete.
          
          #computes the mean of each Inner loop 10-fold CV.
          IV.CV.slopes <- mean(IV.CV.slopes,na.rm = T)
          error.10cv <- mean(error.10cv,na.rm = T) 
          error.10cv.one.sided <- mean(error.10cv.one.sided,na.rm = T)
          
          #Stores the means of the Inner loop CVs before starting the next outer loop iteration
          if(!exists("RepCV10.10")){RepCV10.10 <- c()
          test.join.RepCV10.10 <- c()
          IV.CV.slopes.bin <- c()}
          RepCV10.10 <- c(RepCV10.10,error.10cv)
          test.join.RepCV10.10 <- c(test.join.RepCV10.10,test.join.error.10cv)
          IV.CV.slopes.bin <- c(IV.CV.slopes.bin,IV.CV.slopes)
          
          if(!exists("RepCV10.10.one.sided")){RepCV10.10.one.sided <- c()}
          RepCV10.10.one.sided <- c(RepCV10.10.one.sided,error.10cv.one.sided)
          
          #Removes inner loop objects before moving on to the next 'rep'
          remove("IV.CV.slopes","test.join.error.10cv","error.10cv","CV.violations.capture","CV.violations.one.sided.capture") 
          
        }  #closes outer loop for cross-validation procedure
        
        #Finalize the CV error estimates.  
        #Aggregate the outer loop error estimates by taking the mean of the inner loop means. 
        RepCV10.10.sd <- sd(RepCV10.10,na.rm = T) #sd of iiner loop means.  
        RepCV10.10.max <- max(RepCV10.10,na.rm = T) #determine worst error rate. 
        RepCV10.10.min <- min(RepCV10.10,na.rm = T) #determine best error rate. (for creating worst case scenario out-of-sample semipartial correlation estimate)
        RepCV10.10 <- mean(RepCV10.10,na.rm = T) #mean of the iiner loop means
        IV.CV.slopes.bin <- IV.CV.slopes.bin[!is.nan(IV.CV.slopes.bin)]
        IV.CV.slopes.bin.sd <- sd(IV.CV.slopes.bin,na.rm = T)
        IV.CV.slopes.bin.max <- max(IV.CV.slopes.bin,na.rm = T)
        IV.CV.slopes.bin.min <- min(IV.CV.slopes.bin,na.rm = T)
        IV.CV.slopes.bin.minabs <- min(abs(IV.CV.slopes.bin[!is.na(IV.CV.slopes.bin)]))
        IV.CV.slopes.bin <- mean(IV.CV.slopes.bin,na.rm = T)
        
        test.join.RepCV10.10.sd <- sd(test.join.RepCV10.10,na.rm = T)
        test.join.RepCV10.10.max <- max(test.join.RepCV10.10,na.rm = T)
        test.join.RepCV10.10.min <- min(test.join.RepCV10.10,na.rm = T)
        test.join.RepCV10.10 <- mean(test.join.RepCV10.10,na.rm = T)
        
        RepCV10.10.one.sided.sd <- sd(RepCV10.10.one.sided,na.rm = T)  
        RepCV10.10.one.sided.max <- max(RepCV10.10.one.sided,na.rm = T) 
        RepCV10.10.one.sided.min <- min(RepCV10.10.one.sided,na.rm = T) 
        
        RepCV10.10.one.sided <- mean(RepCV10.10.one.sided,na.rm = T) 
        #Aggregate  outer loop CV violations by summing across the sum of inner loop violations
        CV.violation.totals <- sum(CV.violation.totals,na.rm = T) #Total frequency of violations
        CV.violation.one.sided.totals <- sum(CV.violation.one.sided.totals,na.rm = T) #Total frequency of violations
        
        #Create a flag for later use.  read as 'If CV violations are greater than 0, report true'
        CV.violation.flag <- ifelse(CV.violation.totals>0,TRUE,FALSE) #flag for violations
        CV.violation.one.sided.flag <- ifelse(CV.violation.one.sided.totals>0,TRUE,FALSE) #flag for violations
        
        ####Extracting logistic regression model summary statistics and parameter estimates.
        
        #Logistic regression doesn't give us an F statistic for the model fit.  So we'll take the chi-square of Liklihood difference.  This is the liklihood difference value
        model.f <- deviance(null.glm)-deviance(log.reg) 
        
        #And this is the pvalue for the chisquare of liklihood difference 
        model.p <- stats::pchisq(deviance(null.glm)-deviance(log.reg),
                                 df.residual(null.glm)-df.residual(log.reg),
                                 lower.tail=FALSE) #Is same as lmtest::lrtest(log.reg,null.glm)
        
        #Again, not an F-test. But df1 is similar to K and df2 is still residual degrees of freedom
        f.df1 <-log.reg$df.null-log.reg$df.residual
        f.df2 <- df.residual(log.reg) 
        
        IVpvalue <- log.sum$coefficients[2,4] #Pvalue of target IV. 
        IVslope <-  log.sum$coefficients[2,1] #Slope estimate of target IV
        IVstandarderror <- log.sum$coefficients[2,2]
        AIC.model <- AIC(log.reg) #AIC, it's relative.  Models with lower AIC fit the data better. But AIC does not convey information about the absolute goodness of the fit in that the rmse could still be worse than the no.info.rate
        AICc.model <- AICcmodavg::AICc(log.reg) # AIC corrected for small samples.  When k = # of parameters in the model and n = sample size.  If n/k is less than 40 AICc is recommended (See: Burnham & Anderson, 2002)
        
        lm.cors <- lm.data[,c(1,2)]
        lm.cors <- lm.cors[complete.cases(lm.cors),]
        
        rcd.knn.Bivariate.DV.IV <- NA
        #rcd.knn.Bivariate.DV.IV <- rcd::rcd(lm.cors$DV,lm.cors[,2],method = "knn")} #Robust Copula Dependence.  This is a form of estimating 'correlation' (broadly defined)' WITHOUT making assumptions regarding the parameters of the marginal pdfs. This number will be a more stable estimate of the relationship between the IV and DV regardless of how individual cases are dropped due to missingness. To the extent that this value is greater in magnitude than the linear correlation estimates, we can infer that this metric is implying nonlinear dependence.
        
        #MICe and TICe.  #Yakir A. Reshef, David N. Reshef, Hilary K. Finucane, Pardis C. Sabeti, and Michael Mitzenmacher. 2016. Measuring dependence powerfully and equitably. J. Mach. Learn. Res. 17, 1 (January 2016), 7406-7468.
        
        mic <- NA
        #mic <- minerva::mine(lm.cors$DV,lm.cors[,2],use = "pairwise.complete.obs",est = "mic_e") #MIC and TIC.  There's 'no free lunch' in estimation.  rcd isn't perfect for all forms of dependence.  So we include the Maximal Information Coefficient and the Total Information Coefficient
        MICe <- NA
        # MICe <- mic$MIC #Maximal informiaton coefficient 
        Mic.e.minus.pearsonr.squared <- NA
        # Mic.e.minus.pearsonr.squared <- mic$`MIC-R2` #MIC minus pearson r squared 
        TICe <- NA
        # TICe <- mic$TIC   #the total information coeffieicent 
        GMIC <- NA
        # GMIC <- mic$GMIC #the generalized mean information coefficient 
        
        dcor <- NA
        # dcor <- energy::dcor2d(lm.cors$DV,lm.cors[,2]) #Székely, Gábor J.; Rizzo, Maria L.; Bakirov, Nail K. Measuring and testing dependence by correlation of distances. Ann. Statist. 35 (2007), no. 6, 2769--2794. doi:10.1214/009053607000000505. https://projecteuclid.org/euclid.aos/1201012979
        
        
        #### For a factor DV these will compute but they may not be trustworthy.  They are included here for completeness by converting to numeric vectors
        
        #Randomized dependence coefficient averaged over 50 estimations.  #D. Lopez-Paz, P. Hennig, and B. Scho ̈lkopf, “The randomized dependence coefficient,” in NeurIPS, 2013, pp. 1–9.
        RDC <-mean(unlist(lapply(seq_len(50), function(x)(rdc(lm.cors$DV,lm.cors[,2])))))
        
        # HHG.  The coefficients above lack strong power (with the exception of TICe possibly) to test for independence.  The HHG is a distribution free test specifically designed to maximize power for independence testing. 
        # Heller, R., Heller, Y., Kaufman S., Brill B, & Gorfine, M. (2016). Consistent Distribution-Free K-Sample and Independence Tests for Univariate Random Variables, JMLR 17(29):1-54
        if(is.factor(lm.cors$IV)){
          
          lm.cors2 <- lm.cors[,2]
          lm.cors2 <- as.vector(lm.cors2)
          lm.cors2 <- as.data.frame(lm.cors2)
          
          NullTable_for_N_Large_MXL_tables <- NA
          hhg.Minp.pvalue <- NA
          hhg.Minp <- NA
          zero.order.pearson.cor <- NA
          
        } else {
          
          NullTable_for_N_Large_MXL_tables = HHG::Fast.independence.test.nulltable(nrow(lm.cors), variant = 'ADP-EQP-ML',nr.atoms = 30,nr.perm=200)  #Calcualtes Null distributions
          
          lm.cors2 <- lm.cors[,2]
          lm.cors2 <- as.vector(lm.cors2)
          lm.cors2 <- as.data.frame(lm.cors2)
          
          hhg.Minp.pvalue <- mean(unlist(lapply(seq_len(50), function(x)(HHG::Fast.independence.test(as.numeric(levels(lm.cors$DV))[lm.cors$DV],as.vector(lm.cors$IV),NullTable_for_N_Large_MXL_tables)$MinP.pvalue))))
          
          hhg.Minp <- mean(unlist(lapply(seq_len(50), function(x)(HHG::Fast.independence.test(as.numeric(levels(lm.cors$DV))[lm.cors$DV],as.vector(lm.cors$IV),NullTable_for_N_Large_MXL_tables)$MinP))))
          
          #point-biserial correlation 
          zero.order.pearson.cor <- ltm::biserial.cor(lm.cors$IV,lm.cors$DV)}
        #zero-order spearman correlation 
        zero.order.spearman.cor <- NA
        #partial correlation 
        partial.correlation <- rsq::pcor(glm(DV~.,data=lm.data[complete.cases(lm.data),],family = "binomial"),type = "n")$partial.cor[1] #Squared Partial correlation is “the proportion the variance in Y that is not explained by covariate(s) that can be uniquely by X”(Hayes, 2013). This can be acheived by parceling out variance from y shared with covariates, parceling out variance in x shared with covariates, then taking the correlation of the remaining variance between x and y
        
        
        rmse <- NA
        GVLMA.globaltest.value <- NA
        GVLMA.globaltest.pvalue <- NA
        GVLMA.globaltest.decision <- NA
        GVLMA.skewness.value <- NA
        GVLMA.skewness.pvalue <- NA
        GVLMA.skewness.decision <- NA
        GVLMA.Kurtosis.value <- NA
        GVLMA.Kurtosis.pvalue <- NA
        GVLMA.Kurtosis.decision <- NA
        GVLMA.LinkFunction.value <- NA
        GVLMA.LinkFunction.pvalue <- NA
        GVLMA.LinkFunction.decision <- NA
        GVLMA.Heteroscedasticity.value <- NA
        GVLMA.Heteroscedasticity.pvalue <- NA
        GVLMA.Heteroscedasticity.decision <- NA
        Autocorrelation.lags1to10 <- NA
        Acf.flag <- NA
        shapiro.test.flag <- NA
        # Mean.imputation.flag <- 1
        # r.squared <-2
        # adj.rsquared <-3
        # rmse <- 4
        # no.info.rate <- 5
        # model.p <-6
        # model.f <-7
        # f.df1 <-8
        # f.df2 <-9
        # IVpvalue <-10
        # IVslope <- 11
        # IVstandarderror <-12
        # AIC.model <- 13
        # AICc.model <- 14
        # RepCV10.10 <-15
        # RepCV10.10.sd <- 16
        # RepCV10.10.max <- 17
        # CV.violation.totals <- 16
        # CV.violation.flag <- 17
        # rcd.knn.Bivariate.DV.IV <- 18
        # MICe <- 19
        # Mic.e.minus.pearsonr.squared <- 20
        # TICe <-21
        # GMIC <-22
        # dcor <- 23
        # hhg.Minp.pvalue <-24
        # hhg.Minp <- 25
        # zero.order.pearson.cor <- 26
        # zero.order.spearman.cor <- 27
        # partial.correlation <- 28
        # Any.dropped.Covs <- 29
        # Number.of.dropped.columns <- 30
        # GVLMA.globaltest.value <- 31
        # GVLMA.globaltest.pvalue <- 32
        # GVLMA.globaltest.decision <- 33
        # GVLMA.skewness.value <- 34
        # GVLMA.skewness.pvalue <- 35
        # GVLMA.skewness.decision <- 36
        # GVLMA.Kurtosis.value <- 37
        # GVLMA.Kurtosis.pvalue <- 38
        # GVLMA.Kurtosis.decision <- 39
        # GVLMA.LinkFunction.value <- 40
        # GVLMA.LinkFunction.pvalue <- 41
        # GVLMA.LinkFunction.decision <- 42
        # GVLMA.Heteroscedasticity.value <- 43
        # GVLMA.Heteroscedasticity.pvalue <- 44
        # GVLMA.Heteroscedasticity.decision <- 45
        # CV.violation.one.sided.flag <- 46
        # CV.violation.one.sided.totals <- 47
        # RepCV10.10.one.sided <- 48
        # no.info.misclassifications <- 49
        # sum.of.predicted.probablity.errors <- 50
        # mean.logloss <- 51
        # Multicolinearity <- 52
        # log.reg.overfit.flag <- 53
      } # closes the logistic regression specification processing 
      
      
      Any.dropped.Covs <- ifelse(ncol(lm.data)==length(unique.values),"FALSE",TRUE) # If any variables were dropped due to having too few unique values, this is a flag in the results to note that the estimates are not based on using all the covariates defined by the row. 
      Number.of.dropped.columns <- ifelse(Any.dropped.Covs==TRUE,length(unique.values)-ncol(lm.data),NA) #How many columns were dropped if any columns were dropped at all. 
      
   
     
      result <- c(Mean.imputation.flag,r.squared,adj.rsquared,rmse,no.info.rate,model.p,model.f,f.df1,f.df2,IVpvalue,IVslope,IVstandarderror,AIC.model,AICc.model,RepCV10.10,RepCV10.10.sd,RepCV10.10.max,RepCV10.10.min,CV.violation.totals,CV.violation.flag,rcd.knn.Bivariate.DV.IV,MICe,Mic.e.minus.pearsonr.squared,TICe,GMIC,dcor,hhg.Minp.pvalue,hhg.Minp,zero.order.pearson.cor,zero.order.spearman.cor,partial.correlation,Any.dropped.Covs,Number.of.dropped.columns,GVLMA.globaltest.value,GVLMA.globaltest.pvalue,GVLMA.globaltest.decision,GVLMA.skewness.value,GVLMA.skewness.pvalue,GVLMA.skewness.decision,GVLMA.Kurtosis.value,GVLMA.Kurtosis.pvalue,GVLMA.Kurtosis.decision,GVLMA.LinkFunction.value,GVLMA.LinkFunction.pvalue,GVLMA.LinkFunction.decision,GVLMA.Heteroscedasticity.value,GVLMA.Heteroscedasticity.pvalue,GVLMA.Heteroscedasticity.decision,CV.violation.one.sided.flag,CV.violation.one.sided.totals,RepCV10.10.one.sided,RepCV10.10.one.sided.sd,RepCV10.10.one.sided.max,RepCV10.10.one.sided.min, no.info.misclassifications,sum.of.predicted.probablity.errors, mean.logloss,Multicolinearity,log.reg.overfit.flag, Autocorrelation.lags1to10, Acf.flag,shapiro.test.flag,one.sided.sampling.unreliable.estimate, test.join.RepCV10.10.sd,test.join.RepCV10.10.max, test.join.RepCV10.10.min, test.join.RepCV10.10,IV.CV.slopes.bin.sd,IV.CV.slopes.bin.max,IV.CV.slopes.bin.min,IV.CV.slopes.bin.minabs,IV.CV.slopes.bin)
      return(result)
      
      
    }
    
  })
  # https://stackoverflow.com/questions/26558631/predict-lm-in-a-loop-warning-prediction-from-a-rank-deficient-fit-may-be-mis #25: In predict.lm(lm.CV, newdata = testData[complete.cases(testData),  ... :
  
  
  #[1] "hello4.lm.cv"  Error: variable 'IV' was fitted with type "numeric" but type "nmatrix.1" was supplied
  # xxxx
  End.time <- Sys.time() #Index end of processing
  Time.diff <- End.time-Start.time #How much time did it take overall? 
  print(Time.diff)
  future::plan("future::sequential") #Close down extra R instances.  It's CRITICAL that you run this to close your extra sessions.  If you don't, your RStudio session can start throwing weird errors due to internal confusion about what processings you intend to execute.  The only way back from that is to relaunch RStudio.  
 
  
#Results object is a character matrix 66 rows deep and x amount of models wide.  This transforms that matrix and combines with the appropriate row in the Specifications frame.    
Specifications[,c(which(colnames(Specifications)%in%"Mean.imputation.flag"):which(colnames(Specifications)%in%"IV.CV.slopes.bin"))] <- t(results)  
  

#Gather up our results from each iteration of the 'for loop'
  if(!exists("results2")){results2 <- list()}
  results2 <- append(results2,list(Specifications))
}
# results3 <- append(results2,list(results2[301:360]))
# names(results3) <-names(datas)
names(results2) <- c("anes2016","nonwhite","white","republicans2016","independents2016","democrats2016")
save(results2,file="~/Desktop/Anes_phase_1_results_supdem_meanonly")
