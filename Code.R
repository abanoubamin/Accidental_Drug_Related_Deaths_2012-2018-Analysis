#Read Data from csv file
Drug_data<-read.csv("C:\\Users\\Hussein Ata\\Desktop\\Accidental_Drug_Related_Deaths_2012-2018.csv",na.strings = "")
##############################################################################################################################################
#Data Preprocessing
#Make all DateType as charcters to replace null values
Drug_data$DateType<-as.character(Drug_data$DateType)
##############################################################################################################################################
#Make all Age values are not na
Drug_data$Age[is.na(Drug_data$Age)]<-floor(sum(Drug_data$Age[!is.na(Drug_data$Age)])/length(Drug_data$Age[!is.na(Drug_data$Age)]))
##############################################################################################################################################
#Make all Sex values are not chacters
Drug_data$Sex<-as.character(Drug_data$Sex)
##############################################################################################################################################
#Make all na values in Sex as Female
Drug_data$Sex[is.na(Drug_data$Sex)]<-"Male"
##############################################################################################################################################
#Make all Race values as characters 
Drug_data$Race<-as.character(Drug_data$Race)
##############################################################################################################################################
#Make all na values in Race white 
Drug_data$Race[is.na(Drug_data$Race)]<-"White"
##############################################################################################################################################
#Make all ResidenceCity and Residence country and Residence state are characters
Drug_data$ResidenceCity<-as.character(Drug_data$ResidenceCity)
Drug_data$ResidenceCounty<-as.character(Drug_data$ResidenceCounty)
Drug_data$ResidenceState<-as.character(Drug_data$ResidenceState)
##############################################################################################################################################
#Make all na valus in ResidenceCity and Residence country and Residence state are real values
#Resident city all records have real values
#ResidentCity
Drug_data<-Drug_data[!(is.na(Drug_data$ResidenceCity) | Drug_data$ResidenceCity==""), ]
#ResidentState
Drug_data$ResidenceState[is.na(Drug_data$ResidenceState)]<-"CT"
#ResidentCountry
Drug_data$ResidenceCounty[is.na(Drug_data$ResidenceCounty)&is.na(Drug_data$ResidenceState)]<-"HARTFORD"
Drug_data$ResidenceState[is.na(Drug_data$ResidenceCounty)&is.na(Drug_data$ResidenceState)]<-"CT"
Drug_data$ResidenceCounty[is.na(Drug_data$ResidenceCounty)&Drug_data$ResidenceCity=="SOUTHINGTON"]<-"HARTFORD"
Drug_data$ResidenceCounty[is.na(Drug_data$ResidenceCounty)&Drug_data$ResidenceCity=="NEW LONDON"]<-"NEW LONDON"
Drug_data$ResidenceCounty[is.na(Drug_data$ResidenceCounty)&Drug_data$ResidenceCity=="NORWICH"]<-"NEW LONDON"
Drug_data$ResidenceCounty[is.na(Drug_data$ResidenceCounty)&Drug_data$ResidenceCity=="NEW BRITAIN"]<-"HARTFORD"
Drug_data$ResidenceCounty[is.na(Drug_data$ResidenceCounty)&Drug_data$ResidenceCity=="WATERBURY"]<-"NEW HAVEN"
Drug_data$ResidenceCounty[is.na(Drug_data$ResidenceCounty)&Drug_data$ResidenceCity=="BRIDGEPORT"]<-"FAIRFIELD"
Drug_data$ResidenceCounty[is.na(Drug_data$ResidenceCounty)&Drug_data$ResidenceCity=="NEW HAVEN"]<-"NEW HAVEN"
Drug_data$ResidenceCounty[is.na(Drug_data$ResidenceCounty)&Drug_data$ResidenceCity=="WATERBURY"]<-"NEW HAVEN"
Drug_data$ResidenceCounty[is.na(Drug_data$ResidenceCounty)&Drug_data$ResidenceCity=="ENFIELD"]<-"HARTFORD"
Drug_data$ResidenceCounty[is.na(Drug_data$ResidenceCounty)&Drug_data$ResidenceCity=="HARTFORD"]<-"HARTFORD"
Drug_data$ResidenceCounty[is.na(Drug_data$ResidenceCounty)&Drug_data$ResidenceCity=="STRATFORD"]<-"FAIRFIELD"
Drug_data$ResidenceCounty[is.na(Drug_data$ResidenceCounty)&Drug_data$ResidenceCity=="HAMDEN"]<-"NEW HAVEN"
Drug_data$ResidenceCounty[is.na(Drug_data$ResidenceCounty)&Drug_data$ResidenceCity=="UNKNOWN"]<-"UNKNOWN"
Drug_data$ResidenceCounty[is.na(Drug_data$ResidenceCounty)&Drug_data$ResidenceCity=="SHELTON"]<-"FAIRFIELD"
Drug_data$ResidenceCounty[is.na(Drug_data$ResidenceCounty)&Drug_data$ResidenceCity=="GROTON"]<-"NEW LONDON"
Drug_data$ResidenceCounty[is.na(Drug_data$ResidenceCounty)&Drug_data$ResidenceCity=="OXFORD"]<-"NEW HAVEN"
Drug_data$ResidenceCounty[is.na(Drug_data$ResidenceCounty)&Drug_data$ResidenceCity=="SOMERS"]<-"TOLLAND"
Drug_data$ResidenceCounty[is.na(Drug_data$ResidenceCounty)&Drug_data$ResidenceCity=="NAUGATUCK"]<-"NEW HAVEN"
Drug_data$ResidenceCounty[is.na(Drug_data$ResidenceCounty)&Drug_data$ResidenceCity=="SOUTH WINDSOR"]<-"HARTFORD"
Drug_data$ResidenceCounty[is.na(Drug_data$ResidenceCounty)&Drug_data$ResidenceCity=="FAIRFIELD"]<-"FAIRFIELD"
Drug_data$ResidenceCounty[is.na(Drug_data$ResidenceCounty)]<-"FAIRFIELD"
##############################################################################################################################################
#DeathCity to characters and put real values for it >> make death city for na values the same as recident city
Drug_data$DeathCity<-as.character(Drug_data$DeathCity)
Drug_data<-Drug_data[!(is.na(Drug_data$DeathCity) | Drug_data$DeathCity==""), ]
##############################################################################################################################################
#DeathCounty to characters and put real values for it >>make death country for na values the same as recident country
Drug_data$DeathCounty<-as.character(Drug_data$DeathCounty)
Drug_data<-Drug_data[!(is.na(Drug_data$DeathCounty) | Drug_data$DeathCounty==""), ]
##############################################################################################################################################
#Make Location&LocationifOther as chracters and put real values based on other attributes 
Drug_data$Location<-as.character(Drug_data$Location)
Drug_data$LocationifOther<-as.character(Drug_data$LocationifOther)
Drug_data$Location[is.na(Drug_data$Location)&!is.na(Drug_data$LocationifOther[Drug_data$ID])]<-"Other"
Drug_data$Location[is.na(Drug_data$Location)&Drug_data$DeathCity[Drug_data$ID]==Drug_data$ResidenceCity[Drug_data$ID]]<-"Residence"
Drug_data$Location[is.na(Drug_data$Location)]<-"Hospital"
Drug_data$LocationifOther[is.na(Drug_data$LocationifOther)]<-"Traditional Place"
##############################################################################################################################################
#DescriptionofInjury of inurjy as characters and put(Drug Use)for na values 
Drug_data$DescriptionofInjury<-as.character(Drug_data$DescriptionofInjury)
Drug_data$DescriptionofInjury[is.na(Drug_data$DescriptionofInjury)]<-"Drug Use"
##############################################################################################################################################
#Replace Null values for DateType
Drug_data$DateType[is.na(Drug_data$DateType)]<-"DateofDeath"
##############################################################################################################################################
#Make InjuryPlace as characteres and put na values as other place (not Resident)
Drug_data$InjuryPlace<-as.character(Drug_data$InjuryPlace)
Drug_data$InjuryPlace[is.na(Drug_data$InjuryPlace)]<-"other"
Drug_data$InjuryPlace[is.na(Drug_data$InjuryPlace)&Drug_data$InjuryPlace=="Residence"]<-Drug_data$ResidenceCity[Drug_data$ID]
##############################################################################################################################################
#Make COD as characteres
Drug_data$COD<-as.character(Drug_data$COD)
##############################################################################################################################################
#Make OtherSignifican as characteres
Drug_data$OtherSignifican<-as.character(Drug_data$OtherSignifican)
##############################################################################################################################################
Drug_data$OtherSignifican[is.na(Drug_data$OtherSignifican)]<-"No other Significan"
##############################################################################################################################################
#Make all drugs as character to make their values (Y)or(N)
Drug_data$Heroin<-as.character(Drug_data$Heroin)
Drug_data$Cocaine<-as.character(Drug_data$Cocaine)
Drug_data$Fentanyl<-as.character(Drug_data$Fentanyl)
Drug_data$FentanylAnalogue<-as.character(Drug_data$FentanylAnalogue)
Drug_data$Oxycodone<-as.character(Drug_data$Oxycodone)
Drug_data$Oxymorphone<-as.character(Drug_data$Oxymorphone)
Drug_data$Ethanol<-as.character(Drug_data$Ethanol)
Drug_data$Hydrocodone<-as.character(Drug_data$Hydrocodone)
Drug_data$Benzodiazepine<-as.character(Drug_data$Benzodiazepine)
Drug_data$Methadone<-as.character(Drug_data$Methadone)
Drug_data$Amphet<-as.character(Drug_data$Amphet)
Drug_data$Tramad<-as.character(Drug_data$Tramad)
Drug_data$Morphine_NotHeroin<-as.character(Drug_data$Morphine_NotHeroin)
Drug_data$Hydromorphone<-as.character(Drug_data$Hydromorphone)
Drug_data$OpiateNOS<-as.character(Drug_data$OpiateNOS)
Drug_data$AnyOpioid<-as.character(Drug_data$AnyOpioid)
Drug_data$Other<-as.character(Drug_data$Other)
##############################################################################################################################################
#Make all drugs as (Y) or (N) to indicate whether its one of causes of death or not
Drug_data$Heroin[is.na(Drug_data$Heroin)]<- "N"
Drug_data$Cocaine[is.na(Drug_data$Cocaine)]<- "N"
Drug_data$Fentanyl[is.na(Drug_data$Fentanyl)]<- "N"
Drug_data$FentanylAnalogue[is.na(Drug_data$FentanylAnalogue)]<- "N"
Drug_data$Oxycodone[is.na(Drug_data$Oxycodone)]<- "N"
Drug_data$Oxymorphone[is.na(Drug_data$Oxymorphone)]<- "N"
Drug_data$Ethanol[is.na(Drug_data$Ethanol)]<- "N"
Drug_data$Hydrocodone[is.na(Drug_data$Hydrocodone)]<- "N"
Drug_data$Benzodiazepine[is.na(Drug_data$Benzodiazepine)]<- "N"
Drug_data$Methadone[is.na(Drug_data$Methadone)]<- "N"
Drug_data$Amphet[is.na(Drug_data$Amphet)]<- "N"
Drug_data$Tramad[is.na(Drug_data$Tramad)]<- "N"
Drug_data$Morphine_NotHeroin[is.na(Drug_data$Morphine_NotHeroin)]<- "N"
Drug_data$Hydromorphone[is.na(Drug_data$Hydromorphone)]<- "N"
Drug_data$OpiateNOS[is.na(Drug_data$OpiateNOS)]<- "N"
Drug_data$AnyOpioid[is.na(Drug_data$AnyOpioid)]<- "N"
Drug_data$Other[is.na(Drug_data$Other)]<- "One of taditional durgs"
##############################################################################################################################################
#Make MannerofDeath are chacters 
Drug_data$MannerofDeath<-as.character(Drug_data$MannerofDeath)
##############################################################################################################################################
#Perprocess values 
Drug_data$MannerofDeath[Drug_data$MannerofDeath=="accident"]<-"ACCIDENT"
Drug_data$MannerofDeath[Drug_data$MannerofDeath=="Accident"]<-"ACCIDENT"
write.csv(Drug_data, "DataSet.csv", row.names = FALSE)
##############################################################################################################################################


#(1)Detect relation between age and Date type
Drug_data<-read.csv("C:\\Users\\Hussein Ata\\Documents\\DataSet.csv",na.strings ="")
install.packages("ggplot2")
library(ggplot2)
ggplot(Drug_data,aes(x=Age,fill=DateType))+geom_density(alpha=.3,color=NA)
#########################################################################################
#(2)Detect relation between sex and date type
Drug_data<-read.csv("C:\\Users\\Hussein Ata\\Documents\\DataSet.csv",na.strings ="")
library(ggplot2)
ggplot(Drug_data,aes(x=DateType,fill=Sex))+geom_density(alpha=.3,color=NA)
#########################################################################################
#(3)Detect relation between date type and country
Drug_data<-read.csv("C:\\Users\\Hussein Ata\\Documents\\DataSet.csv",na.strings ="")
library(ggplot2)
ggplot(Drug_data,aes(x=DateType,fill=DeathCounty))+geom_density(alpha=.3,color=NA)
#########################################################################################
#(4)Detect relation between age and manner of death
Drug_data<-read.csv("C:\\Users\\Hussein Ata\\Documents\\DataSet.csv",na.strings ="")
library(ggplot2)
ggplot(Drug_data,aes(x=Age,fill=MannerofDeath))+geom_density(alpha=.3,color=NA)
#########################################################################################
#(5)Detect race of the highest number of people that taking drugs
Drug_data<-read.csv("C:\\Users\\Hussein Ata\\Documents\\DataSet.csv",na.strings ="")
dataset.RaceDate<-Drug_data$Race
dataset.RaceDate=as.numeric(dataset.RaceDate)
w1<-length(dataset.RaceDate[Drug_data$Race=="White"])
x1<-length(dataset.RaceDate[Drug_data$Race=="Black"])
y1<-length(dataset.RaceDate[Drug_data$Race=="Hispanic, White"])
w2<-length(dataset.RaceDate[Drug_data$Race=="Asian, Other"])
x2<-length(dataset.RaceDate[Drug_data$Race=="Unknown"])
y2<-length(dataset.RaceDate[Drug_data$Race=="Asian Indian"])
w3<-length(dataset.RaceDate[Drug_data$Race=="Native American, Other"])
x3<-length(dataset.RaceDate[Drug_data$Race=="Other"])
y3<-length(dataset.RaceDate[Drug_data$Race=="Hispanic, Black"])
w4<-length(dataset.RaceDate[Drug_data$Race=="Chinese"])
w<-c(w1,x1,y1,w2,x2,y2,w3,x3,y3,w4)
barplot(w,main = "Race Frequency",
        ylab = "Number Of Cases",
        col=rainbow(length(w))
        ,ylim = c(0,4000),names.arg = c("White","Black","Hispanic, White",
                                        "Asian, Other","Unknown","Asian Indian","Native American, Other","Other","Hispanic, Black","Chinese")
        ,las=2)
#########################################################################################
#(6)Detect relation between resident country and death country
Drug_data<-read.csv("C:\\Users\\Hussein Ata\\Documents\\DataSet.csv",na.strings ="")
library(ggplot2)
ggplot(Drug_data,aes(x=ResidenceCounty,fill=DeathCounty))+geom_density(alpha=.3,color=NA)
length(Drug_data$ResidenceCounty[Drug_data$ResidenceCounty=="FAIRFIELD"&Drug_data$DeathCounty=="FAIRFIELD"])
length(Drug_data$ResidenceCounty[Drug_data$ResidenceCounty=="HARTFORD"&Drug_data$DeathCounty=="HARTFORD"])
length(Drug_data$ResidenceCounty[Drug_data$ResidenceCounty=="LITCHFIELD"&Drug_data$DeathCounty=="LITCHFIELD"])
length(Drug_data$ResidenceCounty[Drug_data$ResidenceCounty=="MIDDLESEX"&Drug_data$DeathCounty=="MIDDLESEX"])
length(Drug_data$ResidenceCounty[Drug_data$ResidenceCounty=="NEW LONDON"&Drug_data$DeathCounty=="NEW LONDON"])
length(Drug_data$ResidenceCounty[Drug_data$ResidenceCounty=="TOLLAND"&Drug_data$DeathCounty=="TOLLAND"])
length(Drug_data$ResidenceCounty[Drug_data$ResidenceCounty=="USA"&Drug_data$DeathCounty=="USA"])
length(Drug_data$ResidenceCounty[Drug_data$ResidenceCounty=="WINDHAM"&Drug_data$DeathCounty=="WINDHAM"])
###############################################################################################
#(7)Determine what type of drug that is most taken 
NumberOfPeopleTakesHeroin<-length(Drug_data$Heroin[Drug_data$Heroin=="Y"])
NumberOfPeopleTakesCocaine<-length(Drug_data$Cocaine[Drug_data$Cocaine=="Y"])
NumberOfPeopleTakesFentanyl<-length(Drug_data$Fentanyl[Drug_data$Fentanyl=="Y"])
NumberOfPeopleTakesFentanylAnalogue<-length(Drug_data$FentanylAnalogue[Drug_data$FentanylAnalogue=="Y"])
NumberOfPeopleTakesOxycodone<-length(Drug_data$Oxycodone[Drug_data$Oxycodone=="Y"])
NumberOfPeopleTakesOxymorphone<-length(Drug_data$Oxymorphone[Drug_data$Oxymorphone=="Y"])
NumberOfPeopleTakesEthanol<-length(Drug_data$Ethanol[Drug_data$Ethanol=="Y"])
NumberOfPeopleTakesHydrocodone<-length(Drug_data$Hydrocodone[Drug_data$Hydrocodone=="Y"])
NumberOfPeopleTakesBenzodiazepine<-length(Drug_data$Benzodiazepine[Drug_data$Benzodiazepine=="Y"])
NumberOfPeopleTakesMethadone<-length(Drug_data$Methadone[Drug_data$Methadone=="Y"])
NumberOfPeopleTakesAmphet<-length(Drug_data$Amphet[Drug_data$Amphet=="Y"])
NumberOfPeopleTakesTramad<-length(Drug_data$Tramad[Drug_data$Tramad=="Y"])
NumberOfPeopleTakesMorphine_NotHeroin<-length(Drug_data$Morphine_NotHeroin[Drug_data$Morphine_NotHeroin=="Y"])
NumberOfPeopleTakesHydromorphone<-length(Drug_data$Hydromorphone[Drug_data$Hydromorphone=="Y"])
NumberOfPeopleTakesAnyOpioid<-length(Drug_data$AnyOpioid[Drug_data$AnyOpioid=="Y"])
Drug_vector<-c(NumberOfPeopleTakesHeroin
               ,NumberOfPeopleTakesCocaine
               ,NumberOfPeopleTakesFentanyl
               ,NumberOfPeopleTakesFentanylAnalogue
               ,NumberOfPeopleTakesOxycodone
               ,NumberOfPeopleTakesOxymorphone
               ,NumberOfPeopleTakesEthanol
               ,NumberOfPeopleTakesHydrocodone
               ,NumberOfPeopleTakesBenzodiazepine
               ,NumberOfPeopleTakesMethadone
               ,NumberOfPeopleTakesAmphet
               ,NumberOfPeopleTakesTramad
               ,NumberOfPeopleTakesMorphine_NotHeroin
               ,NumberOfPeopleTakesHydromorphone
               ,NumberOfPeopleTakesAnyOpioid)
barplot(Drug_vector,ylim = c(0,2000),main = "Cases that takes drug",ylab = "Number of cases",
        names.arg=c("Heroin","Cocaine","Fentanyl","FentanylAnalogue",
                     "xycodone","Oxymorphone","Ethanol","Hydrocodone","Benzodiazepine",
                               "Methadone","Amphet","Tramad","Morphine_NotHeroin","Hydromorphone",
                               "AnyOpioid"),col = rainbow(length(Drug_vector)),las=2)
###############################################################################################
#(8)Determine percentage of each location that person die in
NumerOfResidence<-length(Drug_data$Location[Drug_data$Location=="Residence"])
NumberOfHospital<-length(Drug_data$Location[Drug_data$Location=="Hospital"])
NumberOfother<-length(Drug_data$Location[Drug_data$Location=="Other"])
NumberOfHome<-length(Drug_data$Location[Drug_data$Location=="Convalescent Home Hospice"])
NumberOfNursing<-length(Drug_data$Location[Drug_data$Location=="Nursing Home"])
slices <- c(NumerOfResidence,NumberOfHome,NumberOfHospital,NumberOfother,NumberOfNursing)
lbls <- c("Residence","Home Hospice","Hospital","Other","Nursing Home")
pct <- slices/sum(slices)*100
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(slices, labels = lbls, col= rainbow (length(lbls)), main="Pie Chart of Countries")
###############################################################################################
#(9)Know distribution of age
hist(Drug_data$Age,main = "Age Distribution",col="orange",ylab = "Number of cases",xlab = "Age")
###############################################################################################
#(10)Get outliers of Age using Boxplot
Drug_data<-read.csv("C:\\Users\\Hussein Ata\\Documents\\DataSet.csv",na.strings ="")
boxplot(Drug_data$Age,col = "blue",main="Age BoxPlot",ylab="Age")
#######################################################################################
#(11)Determine the most common drug between males
MleHeroin<-length(Drug_data$Heroin[Drug_data$Sex=="Male"&Drug_data$Heroin=="Y"])
MleCocaine<-length(Drug_data$Cocaine[Drug_data$Sex=="Male"&Drug_data$Cocaine=="Y"])
MleFentanyl<-length(Drug_data$Fentanyl[Drug_data$Sex=="Male"&Drug_data$Fentanyl=="Y"])
MleFentanylAnalogue<-length(Drug_data$FentanylAnalogue[Drug_data$Sex=="Male"&Drug_data$FentanylAnalogue=="Y"])
MleOxycodone<-length(Drug_data$Oxycodone[Drug_data$Sex=="Male"&Drug_data$Oxycodone=="Y"])
MleEthanol<-length(Drug_data$Ethanol[Drug_data$Sex=="Male"&Drug_data$Ethanol=="Y"])
MleHydrocodone<-length(Drug_data$Hydrocodone[Drug_data$Sex=="Male"&Drug_data$Hydrocodone=="Y"])
MleBenzodiazepine<-length(Drug_data$Benzodiazepine[Drug_data$Sex=="Male"&Drug_data$Benzodiazepine=="Y"])
MleMethadone<-length(Drug_data$Methadone[Drug_data$Sex=="Male"&Drug_data$Methadone=="Y"])
MleAmphet<-length(Drug_data$Amphet[Drug_data$Sex=="Male"&Drug_data$Amphet=="Y"])
MleTramad<-length(Drug_data$Tramad[Drug_data$Sex=="Male"&Drug_data$Tramad=="Y"])
MleMorphine_NotHeroin<-length(Drug_data$Morphine_NotHeroin[Drug_data$Sex=="Male"&Drug_data$Morphine_NotHeroin=="Y"])
MleHydromorphone<-length(Drug_data$Hydromorphone[Drug_data$Sex=="Male"&Drug_data$Hydromorphone=="Y"])
MleOpiateNOS<-length(Drug_data$Tramad[Drug_data$Sex=="Male"&Drug_data$OpiateNOS=="Y"])
MleAnyOpioid<-length(Drug_data$AnyOpioid[Drug_data$Sex=="Male"&Drug_data$AnyOpioid=="Y"])
Male_Drugs<-c(MleHeroin,MleCocaine,MleFentanyl,MleFentanylAnalogue,MleOxycodone,MleEthanol,MleHydrocodone,
              MleBenzodiazepine,MleMethadone,MleAmphet,MleTramad,MleMorphine_NotHeroin,MleHydromorphone,
              MleOpiateNOS,MleAnyOpioid)
barplot(Male_Drugs,ylim = c(0,2000),main = "Cases that takes specific drug",ylab = "Number of cases",
        names.arg=c("Heroin","Cocaine","Fentanyl","FentanylAnalogue",
                    "xycodone","Oxymorphone","Ethanol","Hydrocodone","Benzodiazepine",
                    "Methadone","Amphet","Tramad","Morphine_NotHeroin","Hydromorphone",
                    "AnyOpioid"),col = rainbow(length(Male_Drugs)),las=2)

#######################################################################################
#(12)Determine the most common drug between Females.
FMleHeroin<-length(Drug_data$Heroin[Drug_data$Sex=="Female"&Drug_data$Heroin=="Y"])
FMleCocaine<-length(Drug_data$Cocaine[Drug_data$Sex=="Female"&Drug_data$Cocaine=="Y"])
FMleFentanyl<-length(Drug_data$Fentanyl[Drug_data$Sex=="Female"&Drug_data$Fentanyl=="Y"])
FMleFentanylAnalogue<-length(Drug_data$FentanylAnalogue[Drug_data$Sex=="Female"&Drug_data$FentanylAnalogue=="Y"])
FMleOxycodone<-length(Drug_data$Oxycodone[Drug_data$Sex=="Female"&Drug_data$Oxycodone=="Y"])
FMleEthanol<-length(Drug_data$Ethanol[Drug_data$Sex=="Female"&Drug_data$Ethanol=="Y"])
FMleHydrocodone<-length(Drug_data$Hydrocodone[Drug_data$Sex=="Female"&Drug_data$Hydrocodone=="Y"])
FMleBenzodiazepine<-length(Drug_data$Benzodiazepine[Drug_data$Sex=="Female"&Drug_data$Benzodiazepine=="Y"])
FMleMethadone<-length(Drug_data$Methadone[Drug_data$Sex=="Female"&Drug_data$Methadone=="Y"])
FMleAmphet<-length(Drug_data$Amphet[Drug_data$Sex=="Female"&Drug_data$Amphet=="Y"])
FMleTramad<-length(Drug_data$Tramad[Drug_data$Sex=="Female"&Drug_data$Tramad=="Y"])
FMleMorphine_NotHeroin<-length(Drug_data$Morphine_NotHeroin[Drug_data$Sex=="Female"&Drug_data$Morphine_NotHeroin=="Y"])
FMleHydromorphone<-length(Drug_data$Hydromorphone[Drug_data$Sex=="Female"&Drug_data$Hydromorphone=="Y"])
FMleOpiateNOS<-length(Drug_data$Tramad[Drug_data$Sex=="Female"&Drug_data$OpiateNOS=="Y"])
FMleAnyOpioid<-length(Drug_data$AnyOpioid[Drug_data$Sex=="Female"&Drug_data$AnyOpioid=="Y"])
Female_Drugs<-c(FMleHeroin,FMleCocaine,FMleFentanyl,FMleFentanylAnalogue,FMleOxycodone,FMleEthanol,FMleHydrocodone,
              FMleBenzodiazepine,FMleMethadone,FMleAmphet,FMleTramad,FMleMorphine_NotHeroin,FMleHydromorphone,
              FMleOpiateNOS,FMleAnyOpioid)
barplot(Female_Drugs,ylim = c(0,2000),main = "Cases that takes specific drug",ylab = "Number of cases",
        names.arg=c("Heroin","Cocaine","Fentanyl","FentanylAnalogue",
                    "xycodone","Oxymorphone","Ethanol","Hydrocodone","Benzodiazepine",
                    "Methadone","Amphet","Tramad","Morphine_NotHeroin","Hydromorphone",
                    "AnyOpioid"),col = rainbow(length(Female_Drugs)),las=2)
#########################################################################################################
#Determine median age for each type of drugs
par(mfrow=c(3,4))
drug.herwien<-Drug_data[Drug_data$Heroin=="Y",]
plot(drug.herwien$Heroin,drug.herwien$Age,ylab="age",xlab="heroin",col="red")
drug.cocaine<-Drug_data[Drug_data$Cocaine=="Y",]
plot(drug.cocaine$Cocaine,drug.cocaine$Age,ylab="age",xlab="cocaine",col="blue")
drug.Oxymorphone<-Drug_data[Drug_data$Oxymorphone=="Y",]
plot(drug.Oxymorphone$Oxymorphone,drug.Oxymorphone$Age,ylab="age",xlab="Oxymorphone",col="yellow")
drug.Ethanol<-Drug_data[Drug_data$Ethanol=="Y",]
plot(drug.Ethanol$Ethanol,drug.Ethanol$Age,ylab="age",xlab="Ethanol",col="gray")
drug.Hydrocodone<-Drug_data[Drug_data$Hydrocodone=="Y",]
plot(drug.Hydrocodone$Hydrocodone,drug.Hydrocodone$Age,ylab="age",xlab="Hydrocodone",col="orange")
drug.Benzodiazepine<-Drug_data[Drug_data$Benzodiazepine=="Y",]
plot(drug.Benzodiazepine$Benzodiazepine,drug.Benzodiazepine$Age,ylab="age",xlab="Benzodiazepine",col="white")
drug.Methadone<-Drug_data[Drug_data$Methadone=="Y",]
plot(drug.Methadone$Methadone,drug.Methadone$Age,ylab="age",xlab="Methadone",col="Purple")
drug.Amphet<-Drug_data[Drug_data$Amphet=="Y",]
plot(drug.Amphet$Amphet,drug.Amphet$Age,ylab="age",xlab="Amphet",col="pink")
drug.Tramad<-Drug_data[Drug_data$Tramad=="Y",]
plot(drug.Tramad$Tramad,drug.Tramad$Age,ylab="age",xlab="Tramad",col="tan")
drug.Hydromorphone<-Drug_data[Drug_data$Hydromorphone=="Y",]
plot(drug.Hydromorphone$Hydromorphone,drug.Hydromorphone$Age,ylab="age",xlab="Hydromorphone",col="Purple")
drug.OpiateNOS<-Drug_data[Drug_data$OpiateNOS=="Y",]
plot(drug.OpiateNOS$OpiateNOS,drug.OpiateNOS$Age,ylab="age",xlab="OpiateNOS",col="pink")
drug.AnyOpioid<-Drug_data[Drug_data$AnyOpioid=="Y",]
plot(drug.AnyOpioid$AnyOpioid,drug.AnyOpioid$Age,ylab="age",xlab="AnyOpioid",col="green")
#########################################################################################################
#Analytical metod being applied
#aperori
install.packages("arules", dependencies=TRUE)
library(arules)
d <- gsub('and', ',', Drug_data$COD)
d <- gsub(', ', ',', d)
d <- gsub(' ,', ',', d)
d <- gsub(' , ', ',', d)
d <- paste("{",d,"}",sep="") 
data <- data.frame(sapply(d,as.factor))
itemsets <- apriori(data, parameter=list(support=0.001, minlen=1, maxlen=10, target="frequent itemsets"))
inspect(head(sort(itemsets, by = "support"), 10))
#########################################################################################################
#Naive basysian on age (Training)
install.packages("e1071")
library(e1071)
Drug_data<-read.csv("C:\\Users\\Hussein Ata\\Documents\\DataSet.csv",na.strings ="")
ind<-sample(2,nrow(Drug_data),prob=c(0.7,0.3),replace=TRUE)
train.data<-Drug_data[ind==1,]
test.data<-Drug_data[ind==2,]
NBclassfier=naiveBayes(MannerofDeath ~ Date+DateType+Age+Sex+Race+ResidenceCity+ResidenceCounty+ResidenceState+DeathCity+
                                       DeathCounty+Location+LocationifOther+DescriptionofInjury+InjuryPlace+COD+
                                       OtherSignifican+OpiateNOS+AnyOpioid,data=train.data)
#Naive basysian on age (Testing)
testPred<-predict(NBclassfier,newdata=test.data)
table(testPred,test.data$MannerofDeath)

#########################################################################################################
