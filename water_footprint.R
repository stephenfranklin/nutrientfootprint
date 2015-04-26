library(data.table)
library(ggplot2)
library(jsonlite)
library(curl)
library(plyr)
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
#View(plants)
#View(plants[1001:nrow(plants),])

#is.factor(plants$WFtype)
#levels(plants$WFtype)
plants$WFtype <- as.factor(plants$WFtype)
plants$Product_description_HS <- as.factor(plants$Product_description_HS)
#levels(plants$Product_description_HS)
plants$Product_description_FAOSTAT <- as.factor(plants$Product_description_FAOSTAT)
#levels(plants$Product_description_FAOSTAT)
plants$Product_code_HS <- as.factor(plants$Product_code_HS)
#levels(plants$Product_code_HS)

#### make total footprint
### identify each footprint group and assign group number.
wfgroup <- rep(seq(1, nrow(plants)/3, by=1),each=3)
#length(wfgroup)
plants[ ,wfgroup:=factor(.N) ]  ## .N indicates the size of a column in data.table. It's a "special variable".
plants$wfgroup<-as.factor(wfgroup)

### total footprint column
plants[ ,Global_avg_footprint:=numeric(.N) ]
plants[ ,California_footprint:=numeric(.N) ]
#View(plants)

### sum total footprint
California_footprint<-plants[,sum(California,na.rm=T),by=wfgroup]
Global_avg_footprint<-plants[,sum(Global_avg,na.rm=T),by=wfgroup]
#View(California_footprint)
#nrow(California_footprint)
#nrow(Global_avg_footprint)
#class(California_footprint)

### 0 Values to NA
California_footprint$V1[which(California_footprint$V1==0)] <- NA

### Remove NA rows ("Blue" and "Grey")
plants_g <- plants[WFtype=="Green"]
#nrow(plants_g)
#View(plants_g)
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
#View(Products)
setkey(Products, "index")
##### Note: Important that the order is the same as plants_g. 


### Slap in Products
plants_g$Products<-Products$Product_description_HS

sum(!is.na(plants_g$Product_description_HS))         ## 305
sum(!is.na(plants_g$Product_description_FAOSTAT))    ## 146
sum(!is.na(plants_g$Products))                       ## 354 is good.
#nrow(plants_g)

### Minimize DT
plants_cg <- plants_g[,c("Products","California_footprint","Global_avg_footprint"),with=F]
#View(plants_cg)

#### explore
setkey(plants_cg,"California_footprint")
qplot(data = tail(plants_cg,10), y=Products,x=California_footprint)

g <- ggplot(head(na.omit(plants_cg),10), aes(x=reorder(Products,-California_footprint), y=California_footprint))
g <- g + geom_bar(stat="identity") + coord_flip() 
g + xlab("Product") + ylab("water footprint (m^3/ton)")
#View(plants_cg)

### Let's narrow the products.
selected_p<- c(354,353,352,351,349,342,335,334,331,329,326,320,319,310,302,297,294,289,284,281,279,275,268,266,262,257,256,254,247,246,245,239,235,229,225,224,221,220,218,217,216,215,214,213,212,207,206,205,203,202,199,198,195,186,185,184,183,182,181,176)
selected_nc <- c(1,14,20,26,28,29,36,40,44,46,47,51,52,55,59,66,69,85,88,96,102,111,112,119,125,126,127,129,137,140,141,143,144,146,147,152)  
    ## nc = Not California: selected from foods that are NA (or 0) for California.
plants_s <- plants_cg[selected_p]

setkey(plants_s,"California_footprint")
qplot(data = tail(plants_s,10), y=Products,x=California_footprint, )
qplot(data = head(plants_s,10), y=Products,x=California_footprint, )


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

#is.numeric(animal$Global_wavg)  ## FALSE
animal$Global_wavg <- as.numeric(animal$Global_wavg)
animal$US_wavg <- as.numeric(animal$US_wavg)

## Are all products described?
#sum(!is.na(animal$Product_description_HS)) == nrow(animal)/3
## [1] TRUE probably.
#is.factor(animal$WFtype)
#levels(animal$WFtype)
animal$WFtype <- as.factor(animal$WFtype)
animal$Product_description_HS <- as.factor(animal$Product_description_HS)
#is.factor(animal$Product_description_HS)
#length(levels(animal$Product_description_HS)) == nrow(animal)/3
## [1] TRUE

#### make total footprint
### identify each footprint group and assign group number.
wfgroup <- rep(seq(1, nrow(animal)/3, by=1),each=3)
#length(wfgroup)
animal[ ,wfgroup:=factor(.N) ]  ## .N indicates the size of a column in data.table. It's a "special variable".
animal$wfgroup<-as.factor(wfgroup)

### total footprint column
animal[ ,Global_avg_footprint:=numeric(.N) ]
animal[ ,California_footprint:=numeric(.N) ]
#View(animal)

### sum total footprint
US_footprint<-animal[,sum(US_wavg,na.rm=T),by=wfgroup]
Global_avg_footprint<-animal[,sum(Global_wavg,na.rm=T),by=wfgroup]
#nrow(US_footprint)
#nrow(Global_avg_footprint)
#class(US_footprint)

### 0 Values to NA
US_footprint$V1[which(US_footprint$V1==0)] <- NA
#View(US_footprint)

### Remove NA rows ("Blue" and "Grey")
animal_g <- animal[WFtype=="Green"]
#nrow(animal_g)
#View(animal_g)
##### Note: Important that the order didn't change here. 

### Slap in total footprints.
animal_g$Global_avg_footprint<-Global_avg_footprint$V1
animal_g$US_footprint<-US_footprint$V1

### Minimize DT and rename to match plants.
animal_cg <- animal_g[,c("Product_description_HS","US_footprint","Global_avg_footprint"),with=F]
## cg = CA + Global
## Note that for lack of more specific data that the California footprint is the US footprint.
setnames(animal_cg, c("Products","California_footprint", "Global_avg_footprint"))
#View(animal_cg)

#### explore
setkey(animal_cg,"California_footprint")
qplot(data = tail(animal_cg,10), y=Products,x=California_footprint, )

## There's some non-food products which we should eliminate, 
##          e.g. Semen bovine 1064393    1142704.
animal_cg[,food:=logical(.N)]
## Some key words for non-food / food are:
## live, waste, semen, hair, skin, hide, bend, leather
## carcass, cut, meat, edible, fresh, frozen, prepar, preserv, process, dried
nonfood <-"live(?!r)|waste|semen|hair|skin|hide|bend|leather"
## regex note: (?!x) is the "negative lookahead". 
animal_cg$food<-!grepl(nonfood, animal_cg$Products, perl=T,ignore.case = T)
animal_ed <- animal_cg[food==TRUE,]
#View(animal_ed)

### explore
setkey(animal_ed,"California_footprint")
qplot(data = tail(animal_ed,20), y=Products,x=California_footprint, )

### Let's narrow the products.
selected_a<- c(60,57,40,35,32,20,15,11,6) 
## Note: removing animal_ed[65] as an outlier, though a remarkable one.
##  Horse, ass, mule or hinny meat, fresh, chilled or frozen    47317	51779
##  Bovine meat cured    21909	23799
animal_s <- animal_ed[selected_a,1:3,with=F]

setkey(animal_s,"California_footprint")
qplot(data = tail(animal_s,20), y=Products,x=California_footprint, )



#### Combine DTs #####
water <- rbind(plants_s,animal_s)
setkey(water,California_footprint)
#nrow(water)  ## 71 items
#qplot(Products, data=tail(water,20), geom="bar", weight=California_footprint, ylab="footprint (m^3/ton)") + coord_flip()

g <- ggplot(tail(water,10), aes(x=reorder(Products,-California_footprint), y=California_footprint))
g <- g + geom_bar(stat="identity") + coord_flip() 
g + xlab("Product") + ylab("water footprint (m^3/ton)")

g <- ggplot(head(water,10), aes(x=reorder(Products,-California_footprint), y=California_footprint))
g <- g + geom_bar(stat="identity") + coord_flip() 
g + xlab("Product") + ylab("water footprint (m^3/ton)")


#### Here we painstakingly wrangle the USDA data.
usda_api_key <- readLines("./usda_api_key.R")
## Get a USDA api key and paste it into a file.
##      Be sure to end the line with a carriage return.
nutrients <- fromJSON(paste0("http://api.nal.usda.gov/usda/ndb/list?format=json&lt=n&sort=n&max=200&api_key=",usda_api_key)) 
protein <- nutrients$list$item$id[grep("^Protein",nutrients$list$item$name)]
kcal <- nutrients$list$item$id[grep("^Energy",nutrients$list$item$name)][1]
groups <- fromJSON(paste0("http://api.nal.usda.gov/usda/ndb/list?format=json&lt=g&sort=n&max=200&api_key=",usda_api_key))

### Beef. "gm" is grams protein per 100 grams food.
beefg <- groups$list$item$id[grep("^Beef",groups$list$item$name)]
beeflist.prot <- fromJSON(paste0("http://api.nal.usda.gov/usda/ndb/nutrients/?format=json&api_key=",usda_api_key,"&max=1000&nutrients=",protein,"&fg=",beefg))
beef.prot <- rbind.fill(beeflist.prot$report$foods$nutrients)
beef.foods <- rbind.fill(beeflist.prot$report$foods)
beef.pf <- cbind(beef.prot,beef.foods)
### Keep only raw cuts.
beef.pf <- as.data.table(beef.pf[grep("raw",beef.pf$name),])
### Finally, return the median.
beef.protein <- median(beef.pf$gm)

### Pork.
porkg <- groups$list$item$id[grep("^Pork",groups$list$item$name)]
porklist.prot <- fromJSON(paste0("http://api.nal.usda.gov/usda/ndb/nutrients/?format=json&api_key=",usda_api_key,"&max=1000&nutrients=",protein,"&fg=",porkg))
pork.prot <- rbind.fill(porklist.prot$report$foods$nutrients)
pork.foods <- rbind.fill(porklist.prot$report$foods)
pork.pf <- cbind(pork.prot,pork.foods)
### Keep only raw cuts.
pork.pf <- as.data.table(pork.pf[grep("raw",pork.pf$name),])
### Finally, return the median.
pork.protein <- median(pork.pf$gm)




#### Next we want to convert these to liters per 100 grams.
#### And also gallons per 100 grams.
#### Because the USDA data is in measures of 100 grams.
#### So then we can get the liters or gallons per grams of protein.

#### The USDA numbers and descriptions of foods are different
#### than the water footprint data.
#### The list is just big enough at 71 (+ 36 for the non-CA data) 
#### that I'm tempted to automate the matching.
#### Which will involve regex and some algorithm.
#### That might not be not worth the time.
#### It might be better to just do it manually.
#### but then it's not reproducible.

### e.g. USDA doesn't have "bovine", and a search for "beef"
### yields 1154 results with a variety of protein content.
### "beef ground" yield 49 choices.
### Perhaps the best choice is: 
### "name": "Beef, ground, unspecified fat content, cooked",
### "ndbno": "23220"  (about 25 g protein)
### But "beef chuck" which yields 179 choices of higher protein values (up to 36 g).
### There's also a nutrient list view for which a group 
### such as "Beef Products" will be listed with their protein content.
### I could take the median maybe. or take a couple of products.

### And also raw meat has a lower protein content because it has
### a higher water content.
### Surely cutting out fat and cooking out water,
### increases the proportion of protein in that section.
### So that skews the water footprint.
### There needs to be some accounting for the portion of 
### the carcass that is used. So maybe the median is the best option.
### maybe the median value of just the raw cuts.