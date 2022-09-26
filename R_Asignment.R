MedicalAppointments <- read.csv("MedicalAppointments.csv", header = TRUE, sep = ";")
head(MedicalAppointments)
#Correct class
sapply(MedicalAppointments,class)

#1->No of Neighbourhood
table(MedicalAppointments$Neighbourhood)

#2->Histogram
hist(MedicalAppointments$AppointmentID)

#3->Density plot
density(MedicalAppointments$Age)
plot(density(MedicalAppointments$Age), cex = 0.1)

#4->Missing values
na_values <- is.na(MedicalAppointments)
table(na_values)

#5->Wrong values
wrong_age<-subset(MedicalAppointments, Age<=0)
print(table(wrong_age$Age))

wrong_Diabetes<-subset(MedicalAppointments, Diabetes <0 | Diabetes >1)
wrong_Alcoholism<-subset(MedicalAppointments, Alcoholism <0 | Alcoholism >1)
wrong_Handcap<-subset(MedicalAppointments, Handcap <0 | Handcap >1)
print(table(wrong_Handcap$Handcap))

wrong_Hipertension<-subset(MedicalAppointments, Hipertension <0 | Hipertension >1)
wrong_Scholarship<-subset(MedicalAppointments, Scholarship <0 | Scholarship >1)
wrong_SMS_received<-subset(MedicalAppointments, SMS_received <0 | SMS_received >2)


#6->Line plots
install.packages("tidyverse")
library(tidyverse)
##Dage=density(MedicalAppointments$Age)
ggplot(MedicalAppointments, aes(x=Age, y=PatientId,colour=Gender)) + geom_line()

#7->Avg no of visits
visits<-subset(MedicalAppointments, No.show=="Yes")
table(visits$PatientId)
sapply(table(visits$PatientId), mean)

#8->higher number of visits
children<-subset(MedicalAppointments, No.show=="Yes"& Age<13 & Age>0)
teenagers<-subset(MedicalAppointments, No.show=="Yes"& Age<20 & Age>=13)
adults<-subset(MedicalAppointments, No.show=="Yes"& Age<50 & Age>=20)
elderlies<-subset(MedicalAppointments, No.show=="Yes"& Age>=50)

nrow(children)
nrow(teenagers)
nrow(adults)
nrow(elderlies)

#9->higher number of absence
children_absence<-subset(MedicalAppointments, No.show=="No"& Age<13 & Age>0)
teenagers_absence<-subset(MedicalAppointments, No.show=="NO"& Age<20 & Age>=13)
adults_absence<-subset(MedicalAppointments, No.show=="NO"& Age<50 & Age>=20)
elderlies_absence<-subset(MedicalAppointments, No.show=="NO"& Age>=50)

nrow(children_absence)
nrow(teenagers_absence)
nrow(adults_absence)
nrow(elderlies_absence)

#10->changes of date
date<-as.Date(MedicalAppointments$AppointmentDay)
month<-format(date, "%m")
plot(table(month, MedicalAppointments$No.show))
