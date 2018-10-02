* Encoding: UTF-8.
* Run this first program to install the package WRS2. Once you have run this once, you don't need to run it again unless you reinstall R.

BEGIN PROGRAM R.
install.packages("WRS2")
END PROGRAM.

*This function computes robust tests of three-way independent factorial designs.
* It compares trimmed means (Wilcox, 2012).
* For your own use you will need to change 'outcome', 'predictorA', 'predictorB' and 'CredictorA' to be the variable names (from your SPSS file) for your outcome and three predictors.

BEGIN PROGRAM R.
library(WRS2)
mySPSSdata =  spssdata.GetDataFromSPSS(factorMode = "labels")
t3way(outcome ~ predictorA*predictorB*predictorC, data = mySPSSdata, tr = 0.2)
END PROGRAM.
