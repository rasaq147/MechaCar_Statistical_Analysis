library(dplyr)

#Import and read csv file
MechaCar_mpg <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

#Linear regression
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechaCar_mpg)

#Summary
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechaCar_mpg))

        ##Deliverable 2

#Import and read csv file
suspension<- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

#Total Summary
total_summary <- suspension %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = 'keep')

#Lot summary table 
lot_summary <- suspension %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = 'keep')


          ##Deliverable 3
#T.Test for each lot 
t.test(subset(suspension$PSI,suspension$Manufacturing_Lot=="Lot1"), mu=1500)
t.test(subset(suspension$PSI,suspension$Manufacturing_Lot=="Lot2"), mu=1500)
t.test(subset(suspension$PSI,suspension$Manufacturing_Lot=="Lot3"), mu=1500)
