## upload csv files that were downloaded from survey123 quadrat veg survey
library(dplyr)
library(tidyselect)
library(tidyverse)
library(writexl)
#setwd("C:/Users/rickard/Documents/NCOS Veg monitoring/8-13 survey123 download")
getwd()
setwd("C:/Users/ricka/Documents/CCBER Vegetation")
parent<- read.csv("CCBER_VegMonitoring_QuadratTransects_4.csv")
str(parent)
parent<-parent[,c(2:8,12,16:37,42,43)]
colnames(parent)<-c("MasterID","Date","Monitors","vernal_pool","VP_name","VP_axis","Site","Transect_Name","Transect_length","Start_Time","Finish_Time","Notes","Transect_Distance","Transect_Side","VP_zone",
                    "Quadrat_Notes","p_bare_ground","p_Natural_thatch","p_mowed_thatch","Thatch_Notes","Count_Native_species","Sum_Native_cover","Count_nonnative_species","Sum_nonnative_cover",
                                           "Count_Unknown_species","Sum_Unknown_cover","Sum_Thatch","Sum_Other","Bare_Ground","Sum_all_cover","x","y")
parent<-subset(parent, parent$Site=="ncos")
parent$Date<-as.Date(parent$Date,format<-"%m/%d/%Y %I:%M:%S %p")
parent$Year<-format(parent$Date,format<-"%Y")
parent<-parent[parent$Year==2023,]
unique(parent$VP_name)

## Give vernal pools a transect name
parent<- within(parent, Transect_Name[VP_name == 1] <- 'VP-01')
parent<- within(parent, Transect_Name[VP_name == 2] <- 'VP-02')
parent<- within(parent, Transect_Name[VP_name == 3] <- 'VP-03')
parent<- within(parent, Transect_Name[VP_name == "VP-03"] <- 'VP-03')
parent<- within(parent, Transect_Name[VP_name == "VP_03"] <- 'VP-03')
parent<- within(parent, Transect_Name[VP_name == "VP_04"] <- 'VP-04')
parent<- within(parent, Transect_Name[VP_name == 5] <- 'VP-05')
parent<- within(parent, Transect_Name[VP_name == 6] <- 'VP-06')
parent<- within(parent, Transect_Name[VP_name == 7] <- 'VP-07')
parent<- within(parent, Transect_Name[VP_name == 8] <- 'VP-08')
parent<- within(parent, Transect_Name[VP_name == "VP-08"] <- 'VP-03')
#QA/QC
## analysis on Parent- this will check that each transect has 11 entries. Check vernal pool numbers are within 1-8, check anything that is checked as a vernal pool, but is not actually a vernal pool
parentQA<-subset(parent, parent$Site=="ncos")
parentQA$count<-1
str(parentQA)
transectcount<-aggregate(count~Transect_Name,parentQA,FUN=sum)
## Now create data sets for native, non natives, unknown and other cover to do statistical analysis on.  
natives<-read.csv("native_plants_begin_0.csv")
str(natives)
natives<-natives[,c(2:7)]
colnames(natives)<-c("GlobalID","Native_species","unlisted_native","Native_pcover","Native_Notes","MasterID")
Natives<-merge(parent,natives, by="MasterID", all.x=TRUE, all.y=TRUE)
Natives<-subset(Natives,Natives$Site=="ncos")
Natives$count<-1

nonnatives<-read.csv("nonnative_plants_begin_1.csv")
nonnatives<-nonnatives[,c(2:7)]
colnames(nonnatives)<-c("GlobalID","Nonnative_species","unlisted_nonnative","Nonnative_pcover","Nonnative_Notes","MasterID")
Nonnatives<-merge(parent,nonnatives, by="MasterID", all.x=TRUE, all.y=TRUE)
Nonnatives<-subset(Nonnatives,Nonnatives$Site=="ncos")
#write.csv(Nonnatives,"C:/Users/rickard/Documents/NCOS Veg monitoring/8-13 survey123 download/2021_Unlisted_nonnatives.csv")
#FgNonnatives$count<-1

othercover<-read.csv("other_cover_repeat_begin_3.csv")
str(othercover)
othercover<-othercover[,c(2:7)]
colnames(othercover)<-c("GlobalID","Othercover_type","unlisted_othercover","p_othercover","othercover_Notes","MasterID")
othercover<-merge(parent,othercover, by="MasterID", all.x=TRUE, all.y=TRUE)
othercover<-subset(othercover,othercover$Othercover_type!="")
othercover<-subset(othercover,othercover$Site=="ncos")
str(parent)
str(othercover)
str(Nonnatives)
str(Natives)

unknownplants<-read.csv("unknown_plants_begin_2.csv")
unknownplants<-unknownplants[,c(2:5)]
colnames(unknownplants)<-c("GlobalID","Unknown_Species_description","Unknown_species_pcover","MasterID")
Unknown<-merge(parent,unknownplants, by="MasterID",all.x=TRUE,all.y=TRUE)
Unknown<-subset(Unknown,Unknown$Unknown_Species_description!="")
Unknown<-subset(Unknown,Unknown$Site=="ncos")

## These are used for the written portion of the NCOS report
parent<-subset(parent, parent$Site=="ncos")
unique(Natives$Native_species)
unique(Nonnatives$Nonnative_species)
native_frequency<-aggregate(count~Native_species, Natives, FUN=sum, order=TRUE)
native_frequency[order(-native_frequency$count),c(1,2)]
str(Nonnatives)
nonnative_frequency<-aggregate(Count_nonnative_species~Nonnative_species, Nonnatives, FUN=sum, order=TRUE)
nonnative_frequency<-nonnative_frequency[order(-nonnative_frequency$Count_nonnative_species),c(1,2)]

native_frequency<-aggregate(count~Native_species, Natives, FUN=sum, order=TRUE)
native_frequency<-native_frequency[order(-native_frequency$count),c(1,2)]

## There are 71 natives and 67 nonnative species so far.  Salicornia pacifica was the most common spcies showing up in 180/630 quadrats (28%).  
## Festuca perennis is the most frequent nonnative species appearing 234/630 quadrats (37%)
## There are no unknown plants at ncos site. All sites will have 11 transect locations except vernal pools.

## data for report writing for over all sites
mean(parent$Sum_Native_cover, na.rm=TRUE)
mean(parent$Sum_nonnative_cover, na.rm=TRUE)
mean(parent$Sum_Thatch)
mean(parent$p_bare_ground)
mean(parent$Sum_Other)


##rbind files into 1. First get the same column headings
#Bare ground
str(parent)
masterbareground<-parent[,c(2,3,5,6,8,13,14,15,17)]
masterbareground$PSOC<-"Bare Ground"
masterbareground$Cover_Category<-"BARE GROUND"
colnames(masterbareground)[9]<-"Percent_Cover"

#Natives
str(Natives)
str(masterNatives)
str(mastersumNC)
unique(masterNatives$unlisted_native)
masterNatives<-Natives[,c(2,3,5,6,8,13,14,15,35,36,37)]
masterNatives$Cover_Category<-"NATIVE COVER"
colnames(masterNatives)[9]<-"PSOC"
colnames(masterNatives)[11]<-"Percent_Cover"

mastersumNC<-parent[,c(2,3,5,6,8,13,14,15,22)]
mastersumNC$PSOC<-"Sum of Native Cover"
mastersumNC$Cover_Category<-"NATIVE COVER"
colnames(mastersumNC)[9]<-"Percent_Cover"

#Natural Thatch
str(masterNaturalthatch)
str(mastersumthatch)
masterNaturalthatch<-parent[,c(2,3,5,6,8,13,14,15,18)]
masterNaturalthatch$PSOC<-"Natural Thatch"
masterNaturalthatch$Cover_Category<-"THATCH"
colnames(masterNaturalthatch)[9]<-"Percent_Cover"

mastersumthatch<-parent[,c(2,3,5,6,8,13,14,15,27)]
mastersumthatch$PSOC<-"Sum of Thatch Cover"
mastersumthatch$Cover_Category<-"THATCH"
colnames(mastersumthatch)[9]<-"Percent_Cover"

#Non-Natives
str(Nonnatives)
str(masterNonnatives)
unique(Nonnatives$unlisted_nonnative)
masterNonnatives<-Nonnatives[,c(2,3,5,6,8,13,14,15,35,37)]
masterNonnatives$Cover_Category<-"NON-NATIVE COVER"
colnames(masterNonnatives)[10]<-"Percent_Cover"
colnames(masterNonnatives)[9]<-"PSOC"

str(mastersumNNC)
mastersumNNC<-parent[,c(2,3,5,6,8,13,14,15,24)]
mastersumNNC$PSOC<-"Sum of Non-Native Cover"
mastersumNNC$Cover_Category<-"NON-NATIVE COVER"
colnames(mastersumNNC)[9]<-"Percent_Cover"

#Other Cover
str(othercover)
str(masterother)
str(mastersumother)
str(parent)
masterother<-othercover[,c(2,3,5,6,8,13,14,15,35,37)]
masterother$Cover_Category<-"OTHER COVER"
colnames(masterother)[10]<-"Percent_Cover"
colnames(masterother)[9]<-"PSOC"

mastersumother<-parent[,c(2,3,5,6,8,13,14,15,28)]
mastersumother$PSOC<-"Sum of Other Cover"
mastersumother$Cover_Category<-"OTHER COVER"
colnames(mastersumother)[9]<-"Percent_Cover"

#R bind the above sheets
masterNatives<-masterNatives[,c(1:9,11,12)]
df<-rbind(masterbareground,masterNatives,masterNaturalthatch,masterNonnatives,masterother,mastersumNC,mastersumNNC,mastersumother,mastersumthatch)
df<-rbind(masterbareground,masterNonnatives,masterNatives,masterNaturalthatch,masterother,mastersumNC,mastersumNNC,mastersumother,mastersumthatch)


## Add habitat column to the files
df<-df %>%
  mutate(Habitat = case_when(
    grepl(pattern = "VP", x = Transect_Name) ~ "Vernal Pool",
    grepl(pattern = "GL", x = Transect_Name) ~ "Perennial Grassland",
    grepl(pattern = "BM", x = Transect_Name) ~ "Seasonal Brackish Marsh",
    grepl(pattern = "FSP", x = Transect_Name) ~ "Seasonal Freshwater Pond",
    grepl(pattern = "PU", x = Transect_Name) ~ "Peripheral Uplands",
    grepl(pattern = "SA", x = Transect_Name) ~ "Sandy Annuals",
    grepl(pattern = "SF", x = Transect_Name) ~ "Sand Flat",
    grepl(pattern = "SML", x = Transect_Name) ~ "Salt Marsh",
    grepl(pattern = "SMM", x = Transect_Name) ~ "Salt Marsh",
    grepl(pattern = "SMR", x = Transect_Name) ~ "Remnant Salt Marsh",
    grepl(pattern = "SMT", x = Transect_Name) ~ "Transition/High Salt Marsh",
    grepl(pattern = "SMTH", x = Transect_Name) ~ "Transition/High Salt Marsh"
  ))

write.csv(df,"C:/Users/ricka/Documents/CCBER Vegetation/Quadrartdata_2023.csv")
write_xlsx(df,"C:/Users/ricka/Documents/CCBER Vegetation/Quadratdata_2023.xlsx")
