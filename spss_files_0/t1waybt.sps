* Encoding: UTF-8.
* Run this first program to install the package WRS2. Once you have run this once, you don't need to run it again unless you reinstall R.

BEGIN PROGRAM R.
install.packages("WRS2")
END PROGRAM.

*Test of the hypothesis of equal trimmed means using a percentile t bootstrap method (Wilcox, 2012). For your own use you will need to change 'Happiness' and 'Dose' to be the variable names (from your SPSS file) for your outcome and predictor respectively.

BEGIN PROGRAM R.
library(WRS2)
mySPSSdata =  spssdata.GetDataFromSPSS(factorMode = "labels") 
t1waybt(Happiness~Dose, data = mySPSSdata, tr = 0.2, nboot = 1000)
mcppb20(Happiness~Dose, data = mySPSSdata,  tr = 0.2, nboot = 1000)
END PROGRAM.
