* Encoding: UTF-8.
DESCRIPTIVES
  VARIABLES= day2  /SAVE.
EXECUTE .

COMPUTE zday2 = abs(zday2) .
EXECUTE .

RECODE
  zday2  (3.29 thru Highest = 1) (2.58 thru Highest=2)  (1.96 thru Highest=3)
  (Lowest thru 1.95=4)  .
EXECUTE .

VALUE LABELS zday2 
   4 'Normal range' 3 'Potential Outliers (z > 1.96)' 2 'Probable Outliers (z > 2.58)' 1 'Extreme (z-score > 3.29)' .

FREQUENCIES
  VARIABLES=zday2
  /ORDER=  ANALYSIS .

