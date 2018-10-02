* Encoding: UTF-8.

*Test of the hypothesis of equal trimmed dependent means (Wilcox, 2012). For your own use you will need to change 'Happiness' and 'Dose' to be the variable names (from your SPSS file) for your outcome and predictor respectively.

BEGIN PROGRAM R.
if(require("WRS2")){
    print("WRS2 is loaded correctly")
} else {
    print("trying to install WRS2")
    install.packages("WRS2")
    if(require("WRS2")){
        print("WRS2 installed and loaded")
    } else {
        stop("could not install WRS2")
    }
}

if(require("reshape2")){
    print("reshape2 is loaded correctly")
} else {
    print("trying to install reshape2")
    install.packages("reshape2")
    if(require("reshape2")){
        print("reshape2 installed and loaded")
    } else {
        stop("could not install reshape2")
    }
}

mySPSSdata =  spssdata.GetDataFromSPSS(factorMode = "labels")
#Edit this line to replace celebrity with the variable name in your data that identifies cases
ID<-"celebrity" 
#Edit this line to replace the variables stick, testicle, eye, & witchetty with the variable names in your data that identify levels of the repeated measures variable.
# be careful to keep names in quotes and note names are case sensitive.
rmFactor<-c("stick", "testicle", "eye", "witchetty")

df<-melt(mySPSSdata, id.vars = ID, measure.vars = rmFactor)
names(df)[names(df) == ID] <- "id"

rmanova(df$value, df$variable, df$id, tr = 0.2)
rmmcp(df$value, df$variable, df$id, tr = 0.2)
END PROGRAM.
