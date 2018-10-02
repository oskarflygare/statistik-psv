* Encoding: UTF-8.
* Run this first program to install the package WRS2. Once you have run this once, you don't need to run it again unless you reinstall R.

BEGIN PROGRAM R.
install.packages("WRS2")
END PROGRAM.


*Runs the robust t-test. For your own use you will need to change 'Mischief' and 'Cloak' to be the variable names (from your SPSS file) for your outcome and predictor respectively.

BEGIN PROGRAM R.
library(WRS2)
mySPSSdata =  spssdata.GetDataFromSPSS(factorMode = "labels")
yuenbt(Mischief~Cloak, data = mySPSSdata)
END PROGRAM.
