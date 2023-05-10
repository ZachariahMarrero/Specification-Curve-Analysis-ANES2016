#Partial correlations.  Slope divided by SD of variables. 
#Don't need any other files other than the 'with ivslopes fixed. 
#imported as characters. 

#This script assumes that you have the object 'anes2016' active in your r session.  So you'll have have to load that in or run the cleaning script on the raw data first to get that set up. 
# data <- get(load(file.choose())) #Load in the results file if not already in the r session. 
data <- results2
# names(results2) <- names(datas)
# names(data)<- names(datas)
# data <- results2
# save(results2,file="~/Desktop/phase1_results")
#"anes2016"  <- This is not the anes2016 object. It's the full sample results condition.

# "nonwhite"        
# "white"            
# "republicans2016" 
# "independents2016" 
# "democrats2016" 

#Get out of sample R squared and out of sample squared semipartial correlation
for(i in 1:length(data)){
  data[[i]]$Cov.opts <- as.character(data[[i]]$Cov.opts)
  data[[i]]$outofsamplepreds.unbounded <- 1-(as.numeric(data[[i]]$RepCV10.10)/as.numeric(data[[i]]$no.info.rate))
  data[[i]]$outofsamplepreds <- 1-(as.numeric(data[[i]]$RepCV10.10)/as.numeric(data[[i]]$no.info.rate))
  data[[i]]$outofsamplepreds <- ifelse(data[[i]]$outofsamplepreds>0,data[[i]]$outofsamplepreds,0) #
No.ivs <-data[[i]][is.na(data[[i]]$IV),]
IVs.only <- data[[i]][is.na(data[[i]]$Cov.opts),]
#Merge IV only rsquared with each.  Take difference between it and semipartial once computed.
sync <- merge(data[[i]][,c(1,2,3,which(colnames(data[[i]])%in%"outofsamplepreds")
)],No.ivs[,c(1,3,which(colnames(No.ivs)%in%"outofsamplepreds"))],by=c("DV","Cov.opts"),all = TRUE)

sync <- merge(sync,IVs.only[,c(1,2,which(colnames(IVs.only)%in%"outofsamplepreds"))],by=c("DV","IV"),all = TRUE)
sync2 <- merge(data[[i]][,c(1,2,3,which(colnames(data[[i]])%in%"r.squared")
)],No.ivs[,c(1,3,which(colnames(No.ivs)%in%"r.squared"))],by=c("DV","Cov.opts"),all = TRUE)

sync$squaredoutofsample.semipartial <- sync[,4]-sync[,5] #models with IVs minus models without IVs
sync$squaredoutofsample.semipartial[is.na(sync$squaredoutofsample.semipartial)] <- sync$outofsamplepreds.x[is.na(sync$squaredoutofsample.semipartial)] #When NA, there were no Cov.opts, only an IV.  
sync$squaredoutofsample.semipartial[is.na(sync$IV)] <- 0

sync$squaredoutofsample.semipartial.bounded.at.zero <- sync$squaredoutofsample.semipartial
sync$squaredoutofsample.semipartial.bounded.at.zero[sync$squaredoutofsample.semipartial.bounded.at.zero<0] <- 0 #shows 0 as limit. The alternative indicates that inclusion of the IV hurts the performance 

sync$squaredoutofsample.semipartial.proportionofvarianceexplained <- ifelse(is.nan(sync$squaredoutofsample.semipartial/sync[,4]),0,sync$squaredoutofsample.semipartial/sync[,4])

sync$squredoutofsample.semipartial.changeraw <- sync$squaredoutofsample.semipartial-sync[,6] #Chnage in IVs by themselves vs IVs controlling for Covs
sync$squredoutofsample.semipartial.changeraw[is.na(sync$squredoutofsample.semipartial.changeraw)] <- 0
sync.1 <- sync[,c(1,2,3,which(colnames(sync)%in%c("squaredoutofsample.semipartial","squaredoutofsample.semipartial.bounded.at.zero","squaredoutofsample.semipartial.proportionofvarianceexplained","squredoutofsample.semipartial.changeraw")))]
data[[i]] <- merge(data[[i]],sync.1,by=c("DV","IV","Cov.opts"),all.x = TRUE)

sync2$squaredinsample.semipartial <- as.numeric(sync2[,4])-as.numeric(sync2[,5])
sync2$squaredinsample.semipartial[is.na(sync2$squaredinsample.semipartial)] <- 0
data[[i]] <- merge(data[[i]],sync2[,c(1,2,3,which(colnames(sync2)%in%c("squaredinsample.semipartial")))],all.x = TRUE)

data[[i]]$squaredsemipartialdifference <- data[[i]]$squaredoutofsample.semipartial-data[[i]]$squaredinsample.semipartial
data[[i]]$IVpvalue <- as.numeric(data[[i]]$IVpvalue)
data[[i]]$IV.CV.slopes.bin <- as.numeric(data[[i]]$IV.CV.slopes.bin)
data[[i]]$IVslope <- as.numeric(data[[i]]$IVslope)
data[[i]]$IVslopediff.abs <- abs(data[[i]]$IV.CV.slopes.bin)-abs(data[[i]]$IVslope)
data[[i]]$IVslopediff.raw <- data[[i]]$IV.CV.slopes.bin-data[[i]]$IVslope

data[[i]]$IVCVslope.minus2sd <- abs(data[[i]]$IV.CV.slopes.bin)-2*abs(as.numeric(data[[i]]$IV.CV.slopes.bin.sd))
data[[i]]$IVCVslopeminest.minus2sd <- abs(as.numeric(data[[i]]$IV.CV.slopes.bin.minabs))-2*abs(as.numeric(data[[i]]$IV.CV.slopes.bin.sd)) #Was the slope closest to 0 more than 2 sd from 0?
data[[i]]$model.p <- as.numeric(data[[i]]$model.p)
data[[i]]$r.squared <- as.numeric(data[[i]]$r.squared)
data[[i]]$adj.rsquared <- as.numeric(data[[i]]$adj.rsquared)
data[[i]]$r.squareddiff.out.minus.in <- data[[i]]$outofsamplepreds - data[[i]]$r.squared
data[[i]]$r.squared.out.minus.in.prop.of.in <- ((data[[i]]$outofsamplepreds - data[[i]]$r.squared)/data[[i]]$r.squared)
data[[i]]$GVLMA.globaltest.decision <- as.logical(data[[i]]$GVLMA.globaltest.decision)
data[[i]]$GVLMtrue <- ifelse(data[[i]]$GVLMA.globaltest.decision,1,0)
data[[i]]$GVLMtrue[is.na(data[[i]]$GVLMtrue)] <- 1 #Logistic regression models true due to fewer model assumptions
data[[i]]$outofsampleandmodelp <- ifelse(data[[i]]$outofsamplepreds>0&as.numeric(data[[i]]$model.p)<.05,1,0)
data[[i]]$all.conditions.agree <- ifelse(data[[i]]$outofsampleandmodelp==1 & data[[i]]$GVLMtrue == 1,1,0)

}

source(file.choose()) #select the Graphy.function.R
source(file.choose()) #select the Ploty.mcplot.function.R

if(!require("ggplot2")){install.packages("ggplot2")} else {library(ggplot2)}
if(!require("gtable")) {install.packages("gtable")}  else {library(gtable)}

#To plot, first get our data in the right format.  Graphy.function() will subset, organize, and assign color values to the chosen results frame.  
#There are several options to toggle for executing the funciton. 
# Graphy.function(data,DV1,IV,Effect,Color.coding.condition) 
#data needs to be a list (list()) of the results. Don't use data$republicans2016. but list(data$republicans2016) is fine.
#DV1 needs to be 1 of the DVs and is indicated by referring to the column name in 'anes2016' or to one of the unique DVs in the results (unique(data$anes2016$DV)).  
#IV can either be left out to use all IVs, specified as character vector of the desired column names in 'anes2016' (c("IV1","IV2")) or called by the numbers of the results.  unique(data$anes2016$IV)[1:5])
#Effect is the yaxis values for the upper plot.  For example: "parital.correlation" or "r.squared".  These can also be called by using the index of the column in the results object.  For example, 31 refers to  data$anes2016[,31] which is the "partial.correlation" column. 
#Color.coding.condition is how threshhold and criteria for coloring the results.  Any values 
#get Rep10cv with no covariates. 
# table(is.na(data$anes2016$IV))
# To get out-of-sample 'partial correlations' we'll first computed the out-ofsample error for each DV using all the covariates.  Then subtract this from the IVALL model? Subtract that from IV alone version? Ratio over 0?  becomes IVALL-ALL...but that's not pure because the 'all' can also have gains here.  ?

Effect.all <- c("IV.CV.slopes.bin","IVslope","IVslopediff.abs","IVslopediff.raw","IVCVslope.minus2sd","IVCVslopeminest.minus2sd","squaredinsample.semipartial","squaredoutofsample.semipartial","squaredoutofsample.semipartial.proportionofvarianceexplained","squaredsemipartialdifference","squaredoutofsample.semipartial.bounded.at.zero","r.squared","adj.rsquared","outofsamplepreds","outofsamplepreds.unbounded","r.squareddiff.out.minus.in","r.squared.out.minus.in.prop.of.in","partial.correlation","squredoutofsample.semipartial.changeraw")

Ylab.all <- c( "Avg IV slope over Cross-validation","IV slopes (Single fit over full sample)","Difference of absolute values of \n IV slopes and CV slopes","Difference of values of IV slopes and CV slopes","Absolute value of mean IV.CV.slopes minus 2 SD", "Minimum absolute IV.CV.slopes minus 2 SD","Squared semipartial correlation for IV from in-sample","Percentage of variance predicted by IV \n relative to no-info-rate (out-of-sample semipartial)","Proportion of predicted variance attributed to IV","Out-of-sample minus in-sample squared semipartial correlations","Squared out-of-sample semipartial bounded at zero","In-sample R-squared \n (sum((PredY-mean)^2)/sum((TrueY-mean)^2))","Adjusted R-squared","Out-of-sample R-squared ((1-sd(residuals)/sd(Y))| >=0)","Out-of-sample R-squared (1-sd(residuals)/sd(Y))","Out-of-sample R-squared minus In-sample R-squared (Overfitting index)","Percent reduction in R-squared ((R2Out-R2In)/R2In)","In-sample partial correlation","Out-of-Sample Squaredsemipartial minus \n Zero-order squared correlation")
unique(anes2016$pol_participation_z)
anes2016$party
Condition.all <- c("model.p","squaredoutofsample.semipartial.bounded.at.zero","IVpvalue","outofsamplepreds","outofsampleandmodelp","GVLMtrue","all.conditions.agree")

#Effect <- "IVslopevserror"

# partial correlation
# colored by model p, iv p, rsq, adjrsq, semi r, assumption check gvlm, 
# ylab <- "Avg IV slope over Cross-validation"
# ylab <- "Absolute Difference of single vs CV slopes"
# ylab <- "IV slopes (Single fit over full sample)"
# ylab <- "Difference of absolute values of IV slopes and CV slopes"
# ylab <- "Difference of values of IV slopes and CV slopes"
# Condition <- "model.p"
for(I1 in 1:18){
  Effect <- Effect.all[I1]
  ylab <- Ylab.all[I1]
  for(I2 in 1:7){
  condition <- Condition.all[I2]
 
  if(any(condition%in%c("model.p","IVpvalue"))){
    Be <- .05
    Ce <- "<"}
  if(any(condition%in%c("squaredoutofsample.semipartial.bounded.at.zero","outofsamplepreds","outofsampleandmodelp","GVLMtrue","all.conditions.agree"))){
    Be <- 0
    Ce <- ">"}
  if(!file.exists(file.path("~/Desktop/plots/"))){dir.create(file.path("~/Desktop/plots/"))}
  if(!file.exists(file.path("~/Desktop/plots/",paste(condition)))){ dir.create(file.path("~/Desktop/plots/",paste(condition)))}
pdf(paste0("~/Desktop/plots/",condition,"/",Effect,"coloredby",condition,".pdf"),width = 8, height = 12)

for(i in match(c("supdem_mean","supdem1_V162263","supdem2_V162267","supdem3_V162290","rwa","rwa_2","childrear_sum","trumpliking_V162286","trumpprimary_V161021a", "trump_vote","trump_gains","pol_participation_z"),colnames(anes2016))){

data[[4]][,which(colnames(data[[4]])%in%Effect)] <- round(as.numeric(data[[4]][,which(colnames(data[[4]])%in%Effect)]),digits=4)
colnames(data[[4]])[which(colnames(data[[4]])%in%Effect)]
data[[5]][,which(colnames(data[[5]])%in%Effect)] <- round(as.numeric(data[[5]][,which(colnames(data[[5]])%in%Effect)]),digits=4)

data[[6]][,which(colnames(data[[6]])%in%Effect)] <- round(as.numeric(data[[6]][,which(colnames(data[[6]])%in%Effect)]),digits=4)




Plotme <- Graphy.function(data=data[c(4,6,5)],DV1=i,Effect=Effect,Color.coding.condition=c(condition,Be,Ce))
# Plotme[,which(colnames(Plotme)%in%Effect)] <- round(Plotme[,which(colnames(Plotme)%in%Effect)],digits=4)
if(i==126) { Plotme[Plotme[,which(colnames(Plotme)%in%Effect)]< (-30),which(colnames(Plotme)%in%Effect)] <- 0}

if(i==126) { Plotme[Plotme[,which(colnames(Plotme)%in%Effect)]> 30,which(colnames(Plotme)%in%Effect)] <- 0}

if(i==130) { Plotme[Plotme[,which(colnames(Plotme)%in%Effect)]< (-10),which(colnames(Plotme)%in%Effect)] <- 0}

if(i==130) { Plotme[Plotme[,which(colnames(Plotme)%in%Effect)]> 10,which(colnames(Plotme)%in%Effect)] <- 0}

Plotme$Cov.opts[is.na(Plotme$Cov.opts)] <- "No Covariates"
Plotme$Cov.opts[Plotme$Cov.opts=="ALL"] <- "ALL Covariates"


#Ploty.mcplot.function() carries out the plot building.  'Effect' is the same as Effect in Graphy.function().  
temp.plot <- Ploty.mcplot.function(Plotme, Effect=Effect,cust=TRUE,ylab = ylab,Plot.title=colnames(anes2016)[i])
#To save out the plot, don't use the export option over in the plot window.  Instead use this or a pdf().  The reason being is that the plot dimensions from the export button are set according to your screen size and the resolution probabily isn't all that good. 

#Note: You can save multiple images to the same file by running the line plot command repeatedly after setting the file destination.  Before the file will be accessible, you have to run the dev.off() command so that R will.  
# pdf("~/Desktop/Nameofyourfiletosave.pdf", width = 8, height = 12) 


# png("~/Desktop/Nameofyourfiletosave2.png", width = 8, height = 12, units = 'in', res = 300)
# pdf("~/Desktop/SCA examples 3/testplot.pdf",width = 8, height = 12)

grid::grid.newpage() + grid::grid.draw(temp.plot)  #ignore the "integer(0)" 
}
# options("scipen"=0, "digits"=7)}
dev.off()
  }}
match("trump_gains",colnames(anes2016))
# unique(Plotme$DV)
# summary(as.numeric(data$anes2016$IVslope[which(data$anes2016$DV==132)]))
# #You have to run this line before the file will actually get saved even though it'll probably appear on your desktop before running this. 
# 
# data$anes2016$GVLMtrue
# Plotme$IVpvalue[Plotme$IV==grep("fox",colnames(anes2016))]