#### water_footprint.R
#### author: Stephen Franklin
#### date: 2015 April/May
#### Description: This program takes two sets of water footprint data 
####        (for plant-source and animal-source foods) from http://waterfootprint.org/
####        as well as USDA Nutrient data from http://www.ars.usda.gov/Services/docs.htm?docid=8964
####        and returns "water_footprint_table.RData".

#### License
#### This work is licensed under the Creative Commons Attribution-ShareAlike 4.0 International License. To view a copy of this license, visit http://creativecommons.org/licenses/by-sa/4.0/.
#### <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" href="http://purl.org/dc/dcmitype/Dataset" property="dct:title" rel="dct:type">water_footprint.R</span> by <a xmlns:cc="http://creativecommons.org/ns#" href="http:\\stephenfranklin.info" property="cc:attributionName" rel="cc:attributionURL">Stephen Franklin</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">Creative Commons Attribution-ShareAlike 4.0 International License</a>.<br />Based on a work at <a xmlns:dct="http://purl.org/dc/terms/" href="http://waterfootprint.org/en/resources/water-footprint-statistics/" rel="dct:source">http://waterfootprint.org/en/resources/water-footprint-statistics/</a>.<br />Permissions beyond the scope of this license may be available at <a xmlns:cc="http://creativecommons.org/ns#" href="http://www.ars.usda.gov/Services/docs.htm?docid=8964" rel="cc:morePermissions">http://www.ars.usda.gov/Services/docs.htm?docid=8964</a>.

#### LIBRARIES ####
library(data.table)
library(ggplot2)
library(jsonlite)
library(curl)
library(plyr)

##### FUNCTIONS #####

make_unigram <- function(product){
    #  greps for the first word in the Product column.
    product<-as.character(product)
    thisprod <- unlist( strsplit(product,"[^a-zA-Z]",perl=T) )[1]
    thisprod <- sub("(?<!ie|oe)s$","",thisprod, perl=T)
    thisprod
} 
### example:  sapply(animal_s$Products,unigram)

make_nutrient_list <- function(nutrient, group_id){
    #  takes a nutrient and group(s).
    #  returns a nutrient list.
    if(!is.character(group_id)) stop("group_id must be a string, e.g. c(\"0200\")\n")
    groupids <- paste0("&fg=",group_id[1])
    for(i in group_id[-1]) {
        groupids <- paste0(groupids,"&fg=",i)
    }
    nutrientlist <- fromJSON(paste0("http://api.nal.usda.gov/usda/ndb/nutrients/?format=json&api_key=",usda_api_key,"&max=1500&nutrients=",nutrient,groupids))
    nutrlist <- nutrientlist$report$foods  ## Drill down to the foods.
    ## Check for total greater than max:
    if(nutrientlist$report$total > 1500) {  ## API maxes out at 1500. jerks.
        reports_needed <- round((nutrientlist$report$total - 1500)/1500,0)
        for(i in 1:reports_needed){
            offset <- i*1500
            nutrlist.N <- fromJSON(paste0("http://api.nal.usda.gov/usda/ndb/nutrients/?format=json&api_key=",usda_api_key,"&offset=",offset,"&max=1500&nutrients=",nutrient,groupids))
            nutrlist <- rbind.fill(nutrlist, nutrlist.N$report$foods)
        }
    }
    nutrlist.nutr <- rbind.fill(nutrlist$nutrients) ## separate out the nutrient sublist
    nutrlist <- cbind(nutrlist,nutrlist.nutr) ## slap those columns on. 
    nutrlist
}
## e.g  NN1 <- as.data.table( make_nutrient_list(n.protein,c("1100","0900","2000","1600","1200")) )

usda_median <- function(nutrient_list,product,filter1=NULL,filter2=NULL){
    ## product: char: single word (from make_unigram()).
    ## nutrient_list: from make_nutrient_list().
    ## filter1: char: product list will include this string, e.g. "\\braw\\b".
    ## filter2: char: product list will exclude this string. e.g. "\\bdried\\b".
    ## Returns the median.
    product<-as.character(product)
    product <- paste0("\\b",product)  ## word boundary
    productlist <- as.data.table(nutrient_list[grep(product,
                                nutrient_list$name,perl=T,ignore.case=T),])
    if(!missing(filter1)) {
        ### e.g. "raw". Keep only raw cuts.
        if(length( grep(filter1,productlist$name,perl=T,ignore.case=T) ) > 0)
            productlist <- as.data.table(productlist[grep(filter1,
                                    productlist$name,perl=T,ignore.case=T),])
    }
    if(!missing(filter2)) {
        if(length( grep(filter2,productlist$name,perl=T,ignore.case=T) ) > 0)
            productlist <- productlist[grep(filter2,productlist$name,perl=T,
                                            ignore.case=T,invert=T),]
    }
    # Finally, return the median.
    product.med <- as.numeric(median(productlist$gm))
    #list(product.med, productlist)     ## testing individual
    #list(product.med, productlist$gm)  ## testing group via lapply
    product.med
}

#### CONSTANTS and ALIASES ####
usda_api_key <- readLines("./usda_api_key.R")
## Get a USDA api key and paste it into a file.
##      Be sure to end the line with a carriage return.
usda_groups <- fromJSON(paste0("http://api.nal.usda.gov/usda/ndb/list?format=json&lt=g&sort=n&max=200&api_key=",usda_api_key))
usda_nutrients <- fromJSON(paste0("http://api.nal.usda.gov/usda/ndb/list?format=json&lt=n&sort=n&max=200&api_key=",usda_api_key)) 
n.protein <- usda_nutrients$list$item$id[grep("^Protein",usda_nutrients$list$item$name)]
n.kcal <- usda_nutrients$list$item$id[grep("^Energy",usda_nutrients$list$item$name)][1]
n.water <- usda_nutrients$list$item$id[grep("^Water",usda_nutrients$list$item$name)]
n.carb <- usda_nutrients$list$item$id[grep("^Carbohydrate",usda_nutrients$list$item$name)]
n.fat <- usda_nutrients$list$item$id[grep("lipid",usda_nutrients$list$item$name)]

cf.m3_t.L_100g <- 0.1  ## cubic meters per metric ton to Liters per 100 grams.
cf.m3_t.gal_100g <- .026 ## m^3 per m.ton to US liquid gallons per 100 grams. 
cf.m3_t.gal_oz <- .007489 ## m^3/m.ton to US liq gallons per avoirdupois ounces.


##### GET DATA #####
setwd("~/git_folder/water_footprint/")

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
##!! Use Excel to convert the file to csv. !!##

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
rm(data)
rm(name)
##!! Use Excel to convert the file to csv. !!##

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
##!! Note: Important that the order is the same as plants_g. 

### Slap in Products
plants_g$Products<-Products$Product_description_HS
rm(Products)
rm(California_footprint)
rm(Global_avg_footprint)
rm(wfgroup)

sum(!is.na(plants_g$Product_description_HS))         ## 305
sum(!is.na(plants_g$Product_description_FAOSTAT))    ## 146
sum(!is.na(plants_g$Products))                       ## 354 is good.
#nrow(plants_g)

### Minimize DT
plants_cg <- plants_g[,c("Products","California_footprint","Global_avg_footprint"),with=F]
rm(plants)
#View(plants_cg)



### explore
setkey(plants_cg,"California_footprint")
#qplot(data = tail(plants_cg,10), y=Products,x=California_footprint)

#g <- ggplot(head(na.omit(plants_cg),10), aes(x=reorder(Products,-California_footprint), y=California_footprint))
#g <- g + geom_bar(stat="identity") + coord_flip() 
#g + xlab("Product") + ylab("water footprint (m^3/ton)")
#View(plants_cg)

### Let's narrow the products.
selected_p<- c(354,353,352,351,349,342,335,334,331,329,326,320,319,310,302,297,294,289,284,281,279,275,268,266,262,257,256,254,247,246,245,239,235,229,225,224,221,220,218,217,216,215,214,213,212,207,206,205,203,202,199,198,195,186,185,184,183,182,181,176)
selected_nc <- c(1,14,20,26,28,29,36,40,44,46,47,51,52,55,59,66,69,85,88,96,102,111,112,119,125,126,127,129,137,140,141,143,144,146,147,152)  
    ## nc = Not California: selected from foods that are NA (or 0) for California.
plants_s <- plants_cg[c(selected_p,selected_nc)]
rm(plants_g)

setkey(plants_s,"California_footprint")
#qplot(data = tail(plants_s,10), y=Products,x=California_footprint)
#qplot(data = head(plants_s[complete.cases(plants_s)],10), y=Products,x=California_footprint)

# View(plants_s)


#### Animal data -- import data ####

##!! Each country column comprises four columns for four Production systems:
##!! Grazing    Mixed	Industrial	Weighted average.
##!! We want just the weighted averages 
##!! for the United States and the World Average.

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
rm(animal)
rm(US_footprint)
## cg = CA + Global
## Note that for lack of more specific data that the California footprint is the US footprint.
setnames(animal_cg, c("Products","California_footprint", "Global_avg_footprint"))
#View(animal_cg)

### explore
setkey(animal_cg,"California_footprint")
#qplot(data = tail(animal_cg,10), y=Products,x=California_footprint)

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
rm(animal_g)
#View(animal_ed)

### explore
#qplot(data = tail(animal_ed,20), y=Products,x=Global_avg_footprint)

### Let's narrow the products.
setkey(animal_ed,"Global_avg_footprint") ## to include goat
selected_a<- c(4,5,13,17,22,32,35,36,41,52,60)
# Products
# 1:  Milk not concentrated & unsweetened exceeding 1% not exceeding 6% fat
# 2: Yogurt concentratd o not,sweetend o not,flavourd o contg fruit o cocoa
# 3:                                           Milk and cream nes sweetened
# 4:                       Eggs, bird, in shell, fresh, preserved or cooked
# 5: Dom fowl,duck,goose&guinea fowl meat&meat offal prep/presvd exc livers
# 6:                                                             Cheese nes
# 7:                                    Goat meat, fresh, chilled or frozen
# 8:                                                                 Butter
# 9:                                      Swine cuts, fresh or chilled, nes
# 10:                                 Sheep cuts, boneless, fresh or chilled
# 11:                                 Bovine cuts boneless, fresh or chilled

##!! Notes:
## I chose the meats that were described as fresh and boneless.
## Excluded meats that were offal, liver, or preserved.
## Excluded animal_ed[65] as an outlier, though a remarkable one.
##  Horse, ass, mule or hinny meat, fresh, chilled or frozen    47317	51779
##  Bovine meat cured    21909	23799
## Also: why no fish?
animal_s <- animal_ed[selected_a,1:3,with=F]
setkey(animal_s,"California_footprint")
rm(animal_cg)
#qplot(data = animal_s, y=Products,x=California_footprint)
#View(animal_s)


#### Combine DTs #####
waterf <- rbind(plants_s,animal_s)
setkey(waterf,California_footprint, Global_avg_footprint)
#nrow(water)  ## 107 Global  ## 71 items for California, 
#qplot(Products, data=tail(water,20), geom="bar", weight=Global_avg_footprint, ylab="footprint (m^3/ton)") + coord_flip()

#g <- ggplot(tail(waterf,10), aes(x=reorder(Products,-California_footprint), y=California_footprint))
#g <- g + geom_bar(stat="identity") + coord_flip() 
#g + xlab("Product") + ylab("water footprint (m^3/ton)")

#g <- ggplot(head(waterf[complete.cases(waterf)],10), aes(x=reorder(Products,-California_footprint), y=California_footprint))
#g <- g + geom_bar(stat="identity") + coord_flip() 
#g + xlab("Product") + ylab("water footprint (m^3/ton)")

##!! Initial observation is that high protein products use the most water.


#### Here we painstakingly wrangle the USDA data. ####

### USDA Animal Data ###

## Change names ##
#View(animal_s)
animal_s[grep("yogurt",animal_s[,Products],ignore.case=T)
         ,"Products"]<-"yogurt"
animal_s[grep("goat",animal_s[,Products],ignore.case=T)
         ,"Products"]<-"goat"
animal_s[grep("cream",animal_s[,Products],ignore.case=T)
         ,"Products"]<-"cream"
animal_s[grep("(milk)((?!cream).)*$",animal_s[,Products],ignore.case=T,perl=T)
         ,"Products"]<-"milk"  ## "((?!word).)*$" negative lookahead for a word to the end of the string.
animal_s[grep("eggs",animal_s[,Products],ignore.case=T)
         ,"Products"]<-"egg"
animal_s[grep("fowl",animal_s[,Products],ignore.case=T)
         ,"Products"]<-"poultry"
animal_s[grep("cheese",animal_s[,Products],ignore.case=T)
         ,"Products"]<-"cheese"
animal_s[grep("swine",animal_s[,Products],ignore.case=T)
         ,"Products"]<-"pork"
animal_s[grep("sheep",animal_s[,Products],ignore.case=T)
         ,"Products"]<-"lamb"
animal_s[grep("bovine",animal_s[,Products],ignore.case=T)
         ,"Products"]<-"beef"

# Unfortunately, we get paltry results from "poultry" as search term for the USDA lists.
poultry <- data.table(Products=c("chicken","turkey","duck"),
                      animal_s[Products=="poultry", .(California_footprint)],
                      animal_s[Products=="poultry", .(Global_avg_footprint)])
animal_s <- rbind(animal_s,poultry)
animal_s <- animal_s[(Products!="poultry")]

animal.protein.list <- make_nutrient_list(n.protein, c("1300","0100","1700","1000","0500"))
animal.kcal.list <- make_nutrient_list(n.kcal, c("1300","0100","1700","1000","0500"))
animal.water.list <- make_nutrient_list(n.water, c("1300","0100","1700","1000","0500"))
animal.fat.list <- make_nutrient_list(n.fat, c("1300","0100","1700","1000","0500"))
animal.carb.list <- make_nutrient_list(n.carb, c("1300","0100","1700","1000","0500"))

animal_s$protein <- sapply(animal_s$Products,usda_median,nutrient_list=animal.protein.list,
       filter1="\\braw\\b",filter2="\\b(dry|dried|condensed|evaporated)\\b")
animal.proteins.all <- sapply(animal_s$Products,usda_median,nutrient_list=animal.protein.list,
                            filter2="\\b(dry|dried|condensed|evaporated)\\b")
animal_s$carb <- sapply(animal_s$Products,usda_median,nutrient_list=animal.carb.list,
                        filter1="\\braw\\b",filter2="\\b(dry|dried|condensed|evaporated)\\b")
animal_s$fat <- sapply(animal_s$Products,usda_median,nutrient_list=animal.fat.list,
                         filter1="\\braw\\b",filter2="\\b(dry|dried|condensed|evaporated)\\b")
animal_s$water <- sapply(animal_s$Products,usda_median,nutrient_list=animal.water.list,
                         filter1="\\braw\\b",filter2="\\b(dry|dried|condensed|evaporated)\\b")
animal_s$kcal <- sapply(animal_s$Products,usda_median,nutrient_list=animal.kcal.list,
                        filter1="\\braw\\b",filter2="\\b(dry|dried|condensed|evaporated)\\b")
animal_s$mass <- rowSums(animal_s[, .(protein,carb,fat,water)])
#quantile(animal_s$mass)
    ### The mass should be close to 100,
    ### as the nutrients are measured in grams per 100 grams of product.

### add plant/animal factor:
animal_s[,kingdom:=factor(.N)]
animal_s$kingdom <- factor(animal_s$kingdom,levels = c("plant","animal"))
animal_s$kingdom <- "animal"  ### note as.numeric "plant" = 1, "animal" = 2
as.numeric(animal_s$kingdom)
#View(animal_s)


### USDA plant data ####
## These plants aren't *easily* grepped in a function,
## mostly because the study's names are not similar to the USDA's names.
## e.g. "aubergines(egg-plants)" vs. "eggplants"
## Or because the products themselves weren't easy to match.
## e.g. sugar, coffee, cocoa, and tea are all plants;
## the USDA lists them as many diverse products, but not as raw plants.
## *!*!* Also, what does "nes" mean? ... I'm going with "not explicitly stated".
badgreps_cali <- c(1,9,13,20,21,25,29,31,33,35,36,37,41,42,43,45,46,48,53,54,56)
## ...from old version of plants_s with key California_footprint
badgreps_glob <- c(2,4,5,6,7,9,12,13,14,15,17,18,19,20,24,25,26,27:36)
## ...from new plants_s which includes product not in California.
badgreps <- (c(badgreps_cali + 36, badgreps_glob))
plants_t <- plants_s[-badgreps,]
rm(plants_cg)
## ... that was kind of a waste of effort for only 9 extra observations; that's what I get for trying to be overly inclusive.
# View(plants_t)

# Get 1 big nutrient list for all relevant groups.
plant_g <- c("2000","0900","1600","1200","1100")

### Change names ###
plants_t$Products <- sapply(plants_t$Products,make_unigram)

### Make nutrients lists ###
plant.protein.list <- make_nutrient_list(n.protein, plant_g)
plant.kcal.list <- make_nutrient_list(n.kcal, plant_g)
plant.water.list <- make_nutrient_list(n.water, plant_g)
plant.fat.list <- make_nutrient_list(n.fat, plant_g)
plant.carb.list <- make_nutrient_list(n.carb, plant_g)

### nutrient medians ###
plants_t$protein <- sapply(plants_t$Products,usda_median,nutrient_list=plant.protein.list,
                           filter1="\\braw\\b")
plants_t$carb <- sapply(plants_t$Products,usda_median,nutrient_list=plant.carb.list,
                        filter1="\\braw\\b")
plants_t$fat <- sapply(plants_t$Products,usda_median,nutrient_list=plant.fat.list,
                       filter1="\\braw\\b")
plants_t$water <- sapply(plants_t$Products,usda_median,nutrient_list=plant.water.list,
                         filter1="\\braw\\b")
plants_t$kcal <- sapply(plants_t$Products,usda_median,nutrient_list=plant.kcal.list,
                        filter1="\\braw\\b")
plants_t$mass <- rowSums(plants_t[, c("protein","carb","fat","water"),with=F])
#quantile(plants_t$mass)  ## should be near 100.

### add plant/animal factor ###
plants_t[,kingdom:=factor(.N)]
plants_t$kingdom <- factor(plants_t$kingdom,levels = c("plant","animal"))
plants_t$kingdom <- "plant"  ### note as.numeric "plant" = 1, "animal" = 0



#### Combine plants_t and animal_s ####
water <- as.data.table(rbind.fill(plants_t,animal_s))
setkey(water,California_footprint,Global_avg_footprint)
rm(waterf)
rm(animal_ed)
rm(plants_s)

### Conversion factors ###
# cf.m3_t.L_100g    <- 0.1      ## cubic meters per metric ton to Liters per 100 grams.
# cf.m3_t.gal_100g  <- .026     ## m^3 per m.ton to US liquid gallons per 100 grams. 
# cf.m3_t.gal_oz    <- .007489  ## m^3/m.ton to US liq gallons per avoirdupois ounces.

water$CA_gal_per_oz <- cf.m3_t.gal_oz * water$California_footprint
water$G_gal_per_oz <- cf.m3_t.gal_oz * water$Global_avg_footprint
water$CA_L_per_g_protein <- cf.m3_t.L_100g * water$California_footprint / water$protein
water$G_L_per_g_protein <- cf.m3_t.L_100g * water$Global_avg_footprint / water$protein
water$CA_gal_per_g_protein  <- cf.m3_t.gal_100g * water$California_footprint / water$protein
water$G_gal_per_g_protein <- cf.m3_t.gal_100g * water$Global_avg_footprint / water$protein

save(water,file = "water_footprint_table.RData")


#### Tables and plots ####

#setkey(water,protein)
#rm(California_footprint, Global_avg_footprint, Products, protein)
#attach(water)


#p <- qplot(water, x=California_footprint,y=protein,
#      col = kingdom) 
#p + scale_colour_manual(values=c("green4", "darkred"))

#plot(water, x=Global_avg_footprint,y=protein,
#      col = kingdom) + scale_colour_manual(values=c("green4", "darkred"))

#p <- ggplot( water, aes(x=Global_avg_footprint, y=protein, 
#    colour=factor(kingdom)) )
#p + geom_point() + scale_colour_manual(values=c("green4", "darkred"))



# par(mfrow = c(1,1))
# pairs(water[,c(2:8),with=F], panel = panel.smooth, 
#       main = "water footprint data", 
#       col = 2 + (as.numeric(water$kingdom) < 2)) ## color: red=2,green=3, plant=1,animal=2
# cor(Global_avg_footprint, water$protein)
# cor(complete.cases(California_footprint),protein)
# is.(water$California_footprint)
# 
# ggpairs(water[,c(3:6),with=F],colours=water$kingdom, alpha=1) scale_colour_manual(values=c("green4", "darkred"))
# 
# wfit0 <- lm(Global_avg_footprint ~ protein + fat + water + carb + kcal + kingdom, water)
# summary(wfit0)$r.squared
# summary(wfit0)$coefficients

### Global_avg: We would expect a 390 m3/ton increase in the footprint for each 1 gram increase in protein.
### The amount of protein in the food explains 61% of the variance in the data.
### This makes sense because protein synthesis requires water.
### https://www.youtube.com/watch?v=H8WJ2KENlK0&index=3&list=PL3EED4C1D684D3ADF
### https://www.youtube.com/watch?v=itsb2SqR-R0&index=11&list=PL3EED4C1D684D3ADF
### CA: 450 m3/ton increase. r2 is 52%.

### Do animal-based foods have a bigger water footprint than plant-based foods?
# wfit1 <- lm(Global_avg_footprint ~ as.numeric(kingdom), water)
# summary(wfit1)$r.squared
# summary(wfit1)$coefficients
# ### Well, it seems (p-value = 0.01) 
# ### there is a somewhat significant increase in the water footprint of 
# ### animal-based foods over plant-based foods.
# ### But it only explains 13% of the variance.
# 
# ### Holding protein constant:
# wfit2 <- lm(Global_avg_footprint ~ kingdom + protein, water)
# summary(wfit2)$r.squared
# summary(wfit2)$coefficients
# ### It seems protein content is very important in predicting water footprint.
# 
# ## Examining the interaction between kingdom and protein
# wfit3 <- lm(Global_avg_footprint ~ protein * kingdom, water)
# summary(wfit3)$r.squared
# summary(wfit3)$coefficients
### The reference category for `kingdom` is "1" -- plants, 
### which has and intercept of 155 m3/ton. 
### The slope for *animals* is 416. 
### The change in intercept for *animals* is 1685.
### And the change in slope for *animals* is -125.

### The interaction isn't significant (p: 0.3 > 0.05)

### Let's plot the regression lines for animal versus plant sources,
### with protein as an interaction:
# par(mfrow = c(1,1))
# plot(protein,Global_avg_footprint,pch=19)
# points(protein,Global_avg_footprint,pch=19,col=((as.numeric(kingdom)<2)*1+2))
# ## Again, the as.numeric() turns the factor "0" into a 1
# ## and the factor "1" into a 2.
# abline(c(wfit3$coeff[1],wfit3$coeff[2]),col="green",lwd=3)
# abline(c(wfit3$coeff[1] + wfit3$coeff[3],wfit3$coeff[2] +wfit3$coeff[4]),col="red",lwd=3)
# ### The two food sources seem closely correlated, and that agrees
# ### with the interaction being insignificant.
# 
# ### But let's check the residuals
# par(mfrow = c(1,1))
# ew <- wfit3$residuals 
# plot(kingdom, ew,
#      xlab = "kingdom (0=plant)",
#      ylab = "Residuals")
# 
# ### compared to fit2
# par(mfrow = c(1,1))
# ew <- wfit2$residuals 
# plot(kingdom, ew,
#      xlab = "kingdom (0=plant)",
#      ylab = "Residuals")
# ### Very similar. Lots of outliers in plants.
# 
# ## resdiuals and leverage
# par(mfrow = c(2, 2))
# plot(wfit3)
# ### Three products -- beef, butter, almonds -- have significant leverage in the model. 
# 
# ### Let's see the model without them:
# par(mfrow = c(1,1))
# plot(protein[c(-12,-45,-46)],Global_avg_footprint[c(-12,-45,-46)],pch=19)
# points(protein[c(-12,-45,-46)],Global_avg_footprint[c(-12,-45,-46)],pch=19,col=((as.numeric(kingdom[c(-12,-45,-46)])<2)*1+2))
# abline(c(wfit3$coeff[1],wfit3$coeff[2]),col="green",lwd=3)
# abline(c(wfit3$coeff[1] + wfit3$coeff[3],wfit3$coeff[2] +wfit3$coeff[4]),col="red",lwd=3)
# ### Pretty much the same.
# 
# ### Analysis of variance to compare the three models:
# anova(wfit0, wfit1, wfit2, wfit3)
# #### anova shows that wfit2 
# #### (predicting the footprint by kingdom holding protein constant)
# #### is the best model.
# 
# ###  
# library(MASS)
# wfit.s <- stepAIC(lm(Global_avg_footprint ~ ., data = water[,c(3:8),with=F]), trace = 0) 
# summary(wfit.s)$coeff
# summary(wfit.s)$r.squared



#### The USDA numbers and descriptions of foods are different
#### than the water footprint data.

### e.g. USDA doesn't have "bovine", and a search for "beef"
### yields 1154 results with a variety of protein content.
### "beef ground" yield 49 choices.
### Perhaps the best choice is: 
### "name": "Beef, ground, unspecified fat content, cooked",
### "ndbno": "23220"  (about 25 g protein)
### But "beef chuck" which yields 179 choices of higher protein values (up to 36 g).


