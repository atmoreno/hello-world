# Database from Ontario
# Commands in R to calculate the probability of being at home/traveling/away/daytrip

#verification of the new database, using the actual number of days of the month
#this database will later have data from 3 years

#-------library-----------------------------
library("mlogit", lib.loc="C:/Users/AnaTsui/Documents/r/win-library/3.0")
library("ggplot2", lib.loc="C:/Users/AnaTsui/Documents/r/win-library/3.0")
library("reshape2", lib.loc="C:/Users/AnaTsui/Documents/r/win-library/3.0")
library("RColorBrewer", lib.loc="C:/Users/AnaTsui/Documents/r/win-library/3.0")



#--------------------------------------------------------------
#--------OTHER trip purpose------------------------------------
#--------------------------------------------------------------

  #all the sample (seasons are not distinguished)
    bbdd<-read.csv("C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/Ontario_3y_other.csv")
    colnames(bbdd)
    datos<- mlogit.data(bbdd, shape = "wide", choice = "Choice")
    modelo2<-mlogit(Choice~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=expansionFactor,data=datos)
    summary(modelo2)
    write.table(summary(modelo2)$CoefTable, "C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/models/other_m1.txt", sep="\t")
  
  #season as independent variable
    modelo2<-mlogit(Choice~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4+Season,weights=expansionFactor,data=datos)
    summary(modelo2)
    write.table(summary(modelo2)$CoefTable, "C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/models/other_m3.txt", sep="\t")
  
  #season as independent variable and its interaction with other variables
    modelo2<-mlogit(Choice~1 | Young*Season+Retired*Season+Female*Season+adultsInHousehold*Season+kidsInHousehold*Season+HighSchool*Season+PostSecondary*Season+University*Season+Employed*Season+income2*Season+income3*Season+income4*Season+Season,weights=expansionFactor,data=datos)
    summary(modelo2)
    write.table(summary(modelo2)$CoefTable, "C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/models/other_m4.txt", sep="\t")
    
  #model for winter
    bbdd1<-subset(bbdd,Season=='winter')
    datos<- mlogit.data(bbdd1, shape = "wide", choice = "Choice")
    modelo2<-mlogit(Choice~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=expansionFactor,data=datos)
    summary(modelo2)
    write.table(summary(modelo2)$CoefTable, "C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/models/other_m2W.txt", sep="\t")
    
  #model for summer
    bbdd2<-subset(bbdd,Season=='summer')
    datos<- mlogit.data(bbdd2, shape = "wide", choice = "Choice")
    modelo2<-mlogit(Choice~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=expansionFactor,data=datos)
    summary(modelo2)
    write.table(summary(modelo2)$CoefTable, "C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/models/other_m2S.txt", sep="\t")

#--------------------------------------------------------------
#--------HOLIDAY trip purpose------------------------------------
#--------------------------------------------------------------

  #all the sample (seasons are not distinguished)
    bbdd<-read.csv("C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/Ontario_3y_holiday.csv")
    colnames(bbdd)
    datos<- mlogit.data(bbdd, shape = "wide", choice = "Choice")
    modelo2<-mlogit(Choice~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=expansionFactor,data=datos)
    summary(modelo2)
    write.table(summary(modelo2)$CoefTable, "C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/models/holiday_m1.txt", sep="\t")
  
  #season as independent variable
    modelo2<-mlogit(Choice~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4+Season,weights=expansionFactor,data=datos)
    summary(modelo2)
    write.table(summary(modelo2)$CoefTable, "C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/models/holiday_m3.txt", sep="\t")
  
  #season as independent variable and its interaction with other variables
    modelo2<-mlogit(Choice~1 | Young*Season+Retired*Season+Female*Season+adultsInHousehold*Season+kidsInHousehold*Season+HighSchool*Season+PostSecondary*Season+University*Season+Employed*Season+income2*Season+income3*Season+income4*Season+Season,weights=expansionFactor,data=datos)
    summary(modelo2)
    write.table(summary(modelo2)$CoefTable, "C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/models/holiday_m4.txt", sep="\t")
  
  #model for winter
    bbdd1<-subset(bbdd,Season=='winter')
    datos<- mlogit.data(bbdd1, shape = "wide", choice = "Choice")
    modelo2<-mlogit(Choice~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=expansionFactor,data=datos)
    summary(modelo2)
    write.table(summary(modelo2)$CoefTable, "C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/models/holiday_m2W.txt", sep="\t")
  
  #model for summer
    bbdd2<-subset(bbdd,Season=='summer')
    datos<- mlogit.data(bbdd2, shape = "wide", choice = "Choice")
    modelo2<-mlogit(Choice~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=expansionFactor,data=datos)
    summary(modelo2)
    write.table(summary(modelo2)$CoefTable, "C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/models/holiday_m2S.txt", sep="\t")
  

#--------------------------------------------------------------
#--------VISIT trip purpose------------------------------------
#--------------------------------------------------------------

  #all the sample (seasons are not distinguished)
    bbdd<-read.csv("C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/Ontario_3y_visit.csv")
    colnames(bbdd)
    datos<- mlogit.data(bbdd, shape = "wide", choice = "Choice")
    modelo2<-mlogit(Choice~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=expansionFactor,data=datos)
    summary(modelo2)
    write.table(summary(modelo2)$CoefTable, "C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/models/visit_m1.txt", sep="\t")
    
  #season as independent variable
    modelo2<-mlogit(Choice~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4+Season,weights=expansionFactor,data=datos)
    summary(modelo2)
    write.table(summary(modelo2)$CoefTable, "C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/models/visit_m3.txt", sep="\t")
  
  #season as independent variable and its interaction with other variables
    modelo2<-mlogit(Choice~1 | Young*Season+Retired*Season+Female*Season+adultsInHousehold*Season+kidsInHousehold*Season+HighSchool*Season+PostSecondary*Season+University*Season+Employed*Season+income2*Season+income3*Season+income4*Season+Season,weights=expansionFactor,data=datos)
    summary(modelo2)
    write.table(summary(modelo2)$CoefTable, "C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/models/visit_m4.txt", sep="\t")
  
  #model for winter
    bbdd1<-subset(bbdd,Season=='winter')
    datos<- mlogit.data(bbdd1, shape = "wide", choice = "Choice")
    modelo2<-mlogit(Choice~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=expansionFactor,data=datos)
    summary(modelo2)
    write.table(summary(modelo2)$CoefTable, "C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/models/visit_m2W.txt", sep="\t")
    
  #model for summer
    bbdd2<-subset(bbdd,Season=='summer')
    datos<- mlogit.data(bbdd2, shape = "wide", choice = "Choice")
    modelo2<-mlogit(Choice~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=expansionFactor,data=datos)
    summary(modelo2)
    write.table(summary(modelo2)$CoefTable, "C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/models/visit_m2S.txt", sep="\t")


#--------------------------------------------------------------
#--------BUSINESS trip purpose------------------------------------
#--------------------------------------------------------------

#all the sample (seasons are not distinguished)
bbdd<-read.csv("C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/Ontario_3y_business.csv")
colnames(bbdd)
datos<- mlogit.data(bbdd, shape = "wide", choice = "Choice")
modelo2<-mlogit(Choice~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=expansionFactor,data=datos)
summary(modelo2)
write.table(summary(modelo2)$CoefTable, "C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/models/business_m1.txt", sep="\t")

#season as independent variable
modelo2<-mlogit(Choice~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4+Season,weights=expansionFactor,data=datos)
summary(modelo2)
write.table(summary(modelo2)$CoefTable, "C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/models/business_m3.txt", sep="\t")

#season as independent variable and its interaction with other variables
modelo2<-mlogit(Choice~1 | Young*Season+Retired*Season+Female*Season+adultsInHousehold*Season+kidsInHousehold*Season+HighSchool*Season+PostSecondary*Season+University*Season+Employed*Season+income2*Season+income3*Season+income4*Season+Season,weights=expansionFactor,data=datos)
summary(modelo2)
write.table(summary(modelo2)$CoefTable, "C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/models/business_m4.txt", sep="\t")

#model for winter
bbdd1<-subset(bbdd,Season=='winter')
datos<- mlogit.data(bbdd1, shape = "wide", choice = "Choice")
modelo2<-mlogit(Choice~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=expansionFactor,data=datos)
summary(modelo2)
write.table(summary(modelo2)$CoefTable, "C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/models/business_m2W.txt", sep="\t")

#model for summer
bbdd2<-subset(bbdd,Season=='summer')
datos<- mlogit.data(bbdd2, shape = "wide", choice = "Choice")
modelo2<-mlogit(Choice~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=expansionFactor,data=datos)
summary(modelo2)
write.table(summary(modelo2)$CoefTable, "C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/models/business_m2S.txt", sep="\t")


#--------------------------------------------------------------
#--------LEISURE trip purpose------------------------------------
#--------------------------------------------------------------

#all the sample (seasons are not distinguished)
bbdd<-read.csv("C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/Ontario_3y_leisure.csv")
colnames(bbdd)
datos<- mlogit.data(bbdd, shape = "wide", choice = "Choice")
modelo2<-mlogit(Choice~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=expansionFactor,data=datos)
summary(modelo2)
write.table(summary(modelo2)$CoefTable, "C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/models/_leisure_m1.txt", sep="\t")

#season as independent variable
modelo2<-mlogit(Choice~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4+Season,weights=expansionFactor,data=datos)
summary(modelo2)
write.table(summary(modelo2)$CoefTable, "C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/models/_leisure_m3.txt", sep="\t")

#season as independent variable and its interaction with other variables
modelo2<-mlogit(Choice~1 | Young*Season+Retired*Season+Female*Season+adultsInHousehold*Season+kidsInHousehold*Season+HighSchool*Season+PostSecondary*Season+University*Season+Employed*Season+income2*Season+income3*Season+income4*Season+Season,weights=expansionFactor,data=datos)
summary(modelo2)
write.table(summary(modelo2)$CoefTable, "C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/models/_leisure_m4.txt", sep="\t")

#model for winter
bbdd1<-subset(bbdd,Season=='winter')
datos<- mlogit.data(bbdd1, shape = "wide", choice = "Choice")
modelo2<-mlogit(Choice~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=expansionFactor,data=datos)
summary(modelo2)
write.table(summary(modelo2)$CoefTable, "C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/models/_leisure_m2W.txt", sep="\t")

#model for summer
bbdd2<-subset(bbdd,Season=='summer')
datos<- mlogit.data(bbdd2, shape = "wide", choice = "Choice")
modelo2<-mlogit(Choice~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=expansionFactor,data=datos)
summary(modelo2)
write.table(summary(modelo2)$CoefTable, "C:/Users/AnaTsui/Desktop/TUM/ONTARIO/3y_season/models/_leisure_m2S.txt", sep="\t")
