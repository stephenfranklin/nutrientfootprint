library(data.table)
library(ggplot2)
setwd("~/git_folder/water_footprint/")

#### get data ####
### plants ###
data <- "http://waterfootprint.org/media/downloads/Report47-Appendix-II.zip"
name <- "Report47-Appendix-II.zip"
if(!file.exists("Report47-Appendix-II.csv")) { 
    if(!file.exists(name)) 
        download.file(url = data, destfile = "Report47-Appendix-II.zip",method = "curl", mode="wb")
    name <- unzip("Report47-Appendix-II.zip", list=T)[[1]]
    ## file is named "Report47-Appendix-II.xlsx"
    if(!file.exists(name)) unzip("Report47-Appendix-II.zip")
}

### animals ###
data <- "http://waterfootprint.org/media/downloads/Report48-Appendix-V.zip"
name <- "Report48-Appendix-V.zip"
if(!file.exists("Report48-Appendix-V.csv")){
    if(!file.exists(name)) 
        download.file(url = data, destfile = "Report48-Appendix-V.zip",method = "curl", mode="wb")
    name <- unzip("Report48-Appendix-V.zip", list=T)[[1]]
    ## file is named "Report48-Appendix-V.xlsx"
    if(!file.exists(name)) unzip("Report48-Appendix-V.zip")
}
### Use Excel to convert the files to csv.

##### Plant data -- import data #####
system.time(
    plants <- fread("Report47-Appendix-II.csv",na.strings=c("NA","-",""),
                   stringsAsFactors = T, skip=4, select=c(1:10,3058),header=T )
)

## grep("California",colnames(plants))
## two from Mexico. third one is US. column 3058

setnames(plants, c("Product_code_FAOSTAT","Product_code_HS",
                    "Product_code_SITC","Product_description_HS",
                    "Product_description_FAOSTAT","Root_product_HS",
                    "Product_fraction","Value_fraction","WFtype",
                    "Global_avg","California"))

plants <- plants[2:1063,]
View(plants)
View(plants[1001:nrow(plants),])

is.factor(plants$WFtype)
levels(plants$WFtype)
plants$WFtype <- as.factor(plants$WFtype)
plants$Product_description_HS <- as.factor(plants$Product_description_HS)
levels(plants$Product_description_HS)
plants$Product_description_FAOSTAT <- as.factor(plants$Product_description_FAOSTAT)
levels(plants$Product_description_FAOSTAT)
plants$Product_code_HS <- as.factor(plants$Product_code_HS)
levels(plants$Product_code_HS)

#### make total footprint
### identify each footprint group and assign group number.
wfgroup <- rep(seq(1, nrow(plants)/3, by=1),each=3)
length(wfgroup)
plants[ ,wfgroup:=factor(.N) ]  ## .N indicates the size of a column in data.table. It's a "special variable".
plants$wfgroup<-as.factor(wfgroup)

### total footprint column
plants[ ,Global_avg_footprint:=numeric(.N) ]
plants[ ,California_footprint:=numeric(.N) ]
View(plants)

### sum total footprint
California_footprint<-plants[,sum(California,na.rm=T),by=wfgroup]
Global_avg_footprint<-plants[,sum(Global_avg,na.rm=T),by=wfgroup]
nrow(California_footprint)
nrow(Global_avg_footprint)
class(California_footprint)

### Remove NA rows ("Blue" and "Grey")
plants_g <- plants[WFtype=="Green"]
nrow(plants_g)
View(plants_g)
##### Note: Important that the order didn't change here. 

### Slap in total footprints.
plants_g$Global_avg_footprint<-Global_avg_footprint$V1
plants_g$California_footprint<-California_footprint$V1


### Combine Product names
Products <- plants_g[,c("Product_description_HS","Product_description_FAOSTAT"),with=F]
Products$index <- seq(from = 1, to=nrow(Products))
setkey(Products, "Product_description_HS")
for (i in "Product_description_HS") 
    Products[is.na(get(i)), (i) := Products[i,"Product_description_FAOSTAT",with=F]]
View(Products)
setkey(Products, "index")
##### Note: Important that the order is the same as plants_g. 


### Slap in Products
plants_g$Products<-Products$Product_description_HS

sum(!is.na(plants_g$Product_description_HS))         ## 305
sum(!is.na(plants_g$Product_description_FAOSTAT))    ## 146
sum(!is.na(plants_g$Products))                       ## 354 is good.
nrow(plants_g)

### Minimize DT
plants_cg <- plants_g[,c("Products","California_footprint","Global_avg_footprint"),with=F]
View(plants_cg)

#### explore
setkey(plants_cg,"California_footprint")
qplot(data = tail(plants_cg,10), y=Products,x=California_footprint, )
View(plants_cg)





##### Animal data -- import data ####

## Each country column comprises four columns for four Production systems:
## Grazing    Mixed	Industrial	Weighted average.
## We want just the weighted averages 
## for the United States and the World Average.

## grep("United States",colnames(animal), ignore.case = T)  ## [1] 786 787 788 789
## grep("World Average",colnames(animal), ignore.case = T)  ## [1] 10

system.time(
    animal <- fread("Report48-Appendix-V.csv",na.strings=c("NA","-",""),
                    stringsAsFactors = T,skip=2,header=T,select=c(3,9,13,789) )
)
setnames(animal, c("Product_description_HS","WFtype",
                   "Global_wavg","US_wavg"))
animal <- animal[2:.N]

is.numeric(animal$Global_wavg)  ## FALSE
animal$Global_wavg <- as.numeric(animal$Global_wavg)
animal$US_wavg <- as.numeric(animal$US_wavg)

## Are all products described?
sum(!is.na(animal$Product_description_HS)) == nrow(animal)/3
## [1] TRUE probably.
is.factor(animal$WFtype)
levels(animal$WFtype)
animal$WFtype <- as.factor(animal$WFtype)
animal$Product_description_HS <- as.factor(animal$Product_description_HS)
is.factor(animal$Product_description_HS)
length(levels(animal$Product_description_HS)) == nrow(animal)/3
## [1] TRUE

#### make total footprint
### identify each footprint group and assign group number.
wfgroup <- rep(seq(1, nrow(animal)/3, by=1),each=3)
length(wfgroup)
animal[ ,wfgroup:=factor(.N) ]  ## .N indicates the size of a column in data.table. It's a "special variable".
animal$wfgroup<-as.factor(wfgroup)

### total footprint column
animal[ ,Global_avg_footprint:=numeric(.N) ]
animal[ ,California_footprint:=numeric(.N) ]
View(animal)

### sum total footprint
US_footprint<-animal[,sum(US_wavg,na.rm=T),by=wfgroup]
Global_avg_footprint<-animal[,sum(Global_wavg,na.rm=T),by=wfgroup]
nrow(US_footprint)
nrow(Global_avg_footprint)
class(US_footprint)

### Remove NA rows ("Blue" and "Grey")
animal_g <- animal[WFtype=="Green"]
nrow(animal_g)
View(animal_g)
##### Note: Important that the order didn't change here. 

### Slap in total footprints.
animal_g$Global_avg_footprint<-Global_avg_footprint$V1
animal_g$US_footprint<-US_footprint$V1

### Minimize DT and rename to match plants.
animal_cg <- animal_g[,c("Product_description_HS","US_footprint","Global_avg_footprint"),with=F]
## Note that for lack of more specific data that the California footprint is the US footprint.
setnames(animal_cg, c("Products","California_footprint", "Global_avg_footprint"))
View(animal_cg)

#### explore
setkey(animal_cg,"California_footprint")
qplot(data = tail(animal_cg,10), y=Products,x=California_footprint, )

## There's some non-food products which we should eliminate.
animal_cg[,food:=logical(.N)]
## Some key words for non-food / food are:
## live, waste, semen, hair, skin, hide, bend, leather
## carcass, cut, meat, edible, fresh, frozen, prepar, preserv, process, dried

nonfood <-"live(?!r)|waste|semen|hair|skin|hide|bend|leather"
## regex note: (?!x) is the "negative lookahead". 
animal_cg$food<-!grepl(nonfood, animal_cg$Products, perl=T,ignore.case = T)
animal_ed <- animal_cg[food==TRUE,]

View(animal_ed)

### explore 2
setkey(animal_ed,"California_footprint")
qplot(data = tail(animal_ed,20), y=Products,x=California_footprint, )

