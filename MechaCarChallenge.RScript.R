library(dplyr)
MechaCar_mpg <- read.csv("/Users/szk/Desktop/Analysis Projects/MechaCar_Statistical_Analysis-/MechaCar_mpg.csv"
 )
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle +
     ground_clearance + AWD, data=MechaCar_mpg)
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle +
             ground_clearance + AWD, data=MechaCar_mpg))
