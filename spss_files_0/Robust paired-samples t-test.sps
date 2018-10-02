* Encoding: UTF-8.
* Run this first program to install the package WRS2. Once you have run this once, you don't need to run it again unless you reinstall R.

BEGIN PROGRAM R.
install.packages("WRS2")
END PROGRAM.


*Runs the robust t-test. For your own use you will need to change 'No_Cloak' and 'Cloak' to be the variable names (from your SPSS file) for the two conditions that you want to compare.
* It's important that you leave mySPSSdata$ before the variab;le names (and no space after the $).

BEGIN PROGRAM R.
library(WRS2)
mySPSSdata = spssdata.GetDataFromSPSS() 
yuend(mySPSSdata$No_Cloak, mySPSSdata$Cloak, tr = 0.2)
END PROGRAM.
