 This folder contains a whole bunch of plots generated based on our analysis.  The plots are color coded according to a variety of different conditions. In the original formalulation of specification curve analysis, you might compute a median effect and use a formal test to evaluate it. We didn't think this was necessarily the most reasonable choice as all of the specification are dervied from the same dataset.  An alternative would be to construct three sets: training, validation, and testing.  The training and validation data could be used for model estiamtion and the testing set for model testing.  In our implementation, we only used training and validation.  So, many of the plots are colored by their corresponding out-of-sample R-squared values. 

#adj.rsquared 
	- This is the adjusted required typically provided by summary(lm())

#IV.CV.slopes.bin 
	- This is the average slope of the Independent Variable taken across the cross-validation procedure.

#IVCVslope.minus2sd 
	- This is the IV.CV.slopes.bin minus 2 standard deviations, where the standard deviation is the standard deviation of the slope estimate taken from the distribution of IV slope estimates computed during the cross-validation procedure. 

#IVCVslopeminest.minus2sd
	- This is the difference between the slope from the cross-validation procedure that was closest to 0 and the standard deviation of the distribution of IV slopes from the cross-validation procedure.  The idea here is to highlight the 'worst case scenario' If the values are above 0 after this, then the result is quite robust. 

#IVslope
	- This is the IV slope taken from a model fit to the entire dataset

#IVslopediff.abs 
	- This is the difference of the absolute values of the IVslope and the IV.CV.slopes.bin.  larger values here means that the average slope from the cross-validation procedure was very different from the single fit.

#IVslopediff.raw
	- This is the same as IVslopediff.abs but no absolute values were taken.  As a consequence, if the IVslope was negative and the IV.CV.slopes.bin was positive, those models might be closer to 0. 

#outofsamplepreds.unbounded 
	- This is the out-of-sample r-squared, but it's unbounded at 0. When the value is negative, it means the model is not predictive.  If it's more negative than -.40, then in a lot of cases its predictions are actually worse than would be achieved by picking numbers randomly in the range of min(y)-max(y)

#outofsamplepreds 
	- This is the out-of-sample r-squared. Which is computed as 1 - sqrt(mean((true-Y minus pred-Y)^2)) divided by   sqrt(mean((true-Y minus mean-Y)^2))

#partial.correlation
	-These are just partial.correlations.  These are insample only though. So they're probably 'inflated' estimates in general.  To make an 'out-of-sample' partial correlation we'd have to do a lot more code. 

#r.squared.out.minus.in.prop.of.in 
	- This is the % change in estimated r squared that we get when we take the difference of r.squared from the out-of-sample estimate and the in-sample estimate while considering the in-sample estimate to be the baseline.  

#r.squared
	- This is in-sample r-squared.  For lm() this is sum((PredY minus meanY)^2)/sum((TrueY minus meanY)^2). For linear models this is supposed to be equivalent to the out-of-sample formula described above.

#r.squareddiff.out.minus.in
	- out-of-sample r-squared minus in-sample r-squared.

#squaredinsmaple.semipartial
	- This is the difference of r-squared between a model with the IV in and a model with the IV left out. Where the r-squared is the in-sample r-squared.

#squaredoutofsample.semipartial.bounded.at.zero
	- This is the difference of r-squared between a model with the IV in and a model with the IV left out. Where the r-squared is the out-of-sample r-squared and where negative values were set to 0 before taking the difference.  

#squaredoutofsample.semipartial.proportionofvarianceexplained
	- This is the ratio of the out-of-sample squared semipartial correlation and the total r-squared correlation.  If the total r-squared was 0 and the semipartial wasn't then the value here is 1.

#squaredoutofsample.semipartial
	- same as the bounded.at.zero version except this version isn't bounded at 0 and the interpretation is that the inclusion of the IV in the model where this value is negative means that the out-of-sample predictions are worse because of the inclusion of the IV. 

#squaredsemipartialdifference
	- This is the difference between the out-of-sample and insample squared semi-partial estimates. If this value is above 0 then it means the out-of-sample estimate was actually larger than the insample estimate.  ...which is interesting because it's suggesting that sample characteristics or measurement characteristics cause it to be underestimated when fitting on the whole dataset just once. 
