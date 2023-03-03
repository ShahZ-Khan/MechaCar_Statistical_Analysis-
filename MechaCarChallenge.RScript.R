library(dplyr)
MechaCar_mpg <- read.csv("/Users/szk/Desktop/Analysis Projects/MechaCar_Statistical_Analysis-/MechaCar_mpg.csv"
 )
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle +
     ground_clearance + AWD, data=MechaCar_mpg)

summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle +
ground_clearance + AWD, data=MechaCar_mpg))

SuspensionCoil <- read.csv("/Users/szk/Desktop/Analysis Projects/MechaCar_Statistical_Analysis-/Suspension_Coil.csv")

total_summary <- SuspensionCoil %>% summarize(Mean = mean(PSI), 
                                              Median = median(PSI), 
                                              Variance = var(PSI),
                                              SD = sd(PSI))

lot_summary <- SuspensionCoil %>% group_by(Manufacturing_Lot) %>%
  summarize(Mean = mean(PSI), 
            Median = median(PSI), 
            Variance = var(PSI),
            SD = sd(PSI), .groups= 'keep')

t.test(SuspensionCoil$PSI, mu=1500)


t.test(subset(SuspensionCoil, Manufacturing_Lot == "Lot1")$PSI, mu = 1500)

t.test(subset(SuspensionCoil, Manufacturing_Lot == "Lot2")$PSI, mu = 1500)

t.test(subset(SuspensionCoil, Manufacturing_Lot == "Lot3")$PSI, mu = 1500)

