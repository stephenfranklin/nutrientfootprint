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

#### import data ####
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
### identify each footprint group
wfgroup <- rep(seq(1, nrow(plants)/3, by=1),each=3)
length(wfgroup)
plants[ ,wfgroup:=factor(.N) ]
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
##### Note: important that the order didn't change here. 

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

### Slap in Products
plants_g$Products<-Products$Product_description_HS

sum(!is.na(plants_g$Product_description_HS))         ## 305
sum(!is.na(plants_g$Product_description_FAOSTAT))    ## 146
sum(!is.na(plants_g$Products)) 

### Minimize DT
plants_cg <- plants_g[,c("Products","California_footprint","Global_avg_footprint"),with=F]
View(plants_cg)

#### explore
qplot(data = tail(plants_cg,10), y=Products,x=California_footprint, )
setkey(plants_cg,"California_footprint")
View(plants_cg)
