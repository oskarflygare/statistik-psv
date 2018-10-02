* Encoding: UTF-8.
* Run this first program to install the package WRS2. Once you have run this once, you don't need to run it again unless you reinstall R.

BEGIN PROGRAM R.
install.packages("WRS2")
END PROGRAM.

*This function computes robust ANCOVA for 2 independent groups and one covariate.
* It compares trimmed means. No parametric assumption (e.g. homogeneity) is made about the form of the regression lines.
* A running interval smoother is used. A bootstrap version which computes confidence intervals using a percentile t-bootstrap is provided as well (Wilcox, 2012).
* For your own use you will need to change 'Happiness', 'Dose' and 'Puppy_love' to be the variable names (from your SPSS file) for your outcome, two-category predictor, and covariate respectively.

BEGIN PROGRAM R.
library(WRS2)
mySPSSdata =  spssdata.GetDataFromSPSS(factorMode = "labels") 
ancboot(Happiness ~ Dose + Puppy_love, data = mySPSSdata, tr = 0.2, nboot = 1000)
END PROGRAM.
