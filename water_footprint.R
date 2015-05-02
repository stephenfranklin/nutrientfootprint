#### Conversions:
#### 1000 cubic meters / metric ton
#### = 120 gallons / pound
#### = 26.4 gallons / 100 grams  
#### ~or~ 4000 m3/t = 100 gal / 100 g

## 1/2 lb hamburger: 14000 m3/t = 14*120/2 = 840 gallons
## 1/2 lb almonds:   13000 m3/t = 13*120/2 = 780 gallons

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
## Also: why no fish?
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
n.protein <- nutrients$list$item$id[grep("^Protein",nutrients$list$item$name)]
n.kcal <- nutrients$list$item$id[grep("^Energy",nutrients$list$item$name)][1]
groups <- fromJSON(paste0("http://api.nal.usda.gov/usda/ndb/list?format=json&lt=g&sort=n&max=200&api_key=",usda_api_key))


# write a function that
#   greps for the first word in the Product column.
make_unigram <- function(product){
  product<-as.character(product)
  thisprod <- unlist( strsplit(product,"[^a-zA-Z]",perl=T) )[1]
  thisprod <- sub("s$","",thisprod)
  thisprod
} 
### example:  sapply(animal_s$Products,unigram)

# write a function that:
#  takes a nutrient and group(s).
#  returns a nutrient list.
make_nutrient_list <- function(nutrient, group_id){
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

# write a function that: 
#   takes a product, nutrient list, filter (optional), invert filter (optional). 
#   subsets nutrient list by product + filters, e.g. "raw"
#   returns the median.
#   (Use lapply or sapply for a list of products.)
usda_median <- function(product,nutrient_list,filter1=NULL,filter2=NULL){
  ## product: char: single word (from make_unigram()).
  ## nutrient_list: from make_nutrient_list().
  ## filter1: char: product list will include this string, e.g. "\\braw\\b".
  ## filter2: char: product list will exclude this string. e.g. "\\bdried\\b".
  product<-as.character(product)
  product <- paste0("\\b",product)  ## word boundary
  productlist <- as.data.table(nutrient_list[grep(product,
                                                 nutrient_list$name,perl=T,ignore.case=T),])
  if(!missing(filter1)) {
    ### e.g. "raw". Keep only raw cuts.
    if(length( grep(filter1,productlist$name,perl=T,ignore.case=T) ) > 0)
      productlist <- as.data.table(productlist[grep(filter1,productlist$name,perl=T,ignore.case=T),])
  }
  if(!missing(filter2)) {
    if(length( grep(filter2,productlist$name,perl=T,ignore.case=T) ) > 0)
      productlist <- productlist[grep(filter2,productlist$name,perl=T,ignore.case=T,invert=T),]
  }
  # Finally, return the median.
  product.med <- as.numeric(median(productlist$gm))
  #list(product.med, productlist)     ## testing individual
  #list(product.med, productlist$gm)  ## testing group via lapply
  product.med
}



### Obtain the median protein for cuts of raw meat.
### See 'groups' for good arguments.
### "gm" is grams protein per 100 grams food.
### product greps for the group number from the groups list.
### filter1 filters with grep (use "\\b" for a word boundary).
### filter2 filters the inverse from grep.
usda_raw_median <- function(product, filter1=NULL,filter2=NULL) {
    if(!is.character(product)) stop("enter a string, i.e. usda_raw_median(\"beef\").\n")
    #product.ch <- paste0("^",product)
    productg <- groups$list$item$id[grep(product,groups$list$item$name, ignore.case = T)]
    productlist.prot <- fromJSON(paste0("http://api.nal.usda.gov/usda/ndb/nutrients/?format=json&api_key=",usda_api_key,"&max=1000&nutrients=",protein,"&fg=",productg))
    product.prot <- rbind.fill(productlist.prot$report$foods$nutrients)
    product.foods <- rbind.fill(productlist.prot$report$foods)
    product.pf <- cbind(product.prot,product.foods)
    if(!missing(filter1)) {
    ### e.g. "raw". Keep only raw cuts.
        product.pf <- as.data.table(product.pf[grep(filter1,product.pf$name,perl=T,ignore.case=T),])
    }
    if(!missing(filter2)) {
        product.pf <- product.pf[grep(filter2,product.pf$name,perl=T,ignore.case=T,invert=T),]
    }
    ### Finally, return the median.
    product.protein <- median(product.pf$gm)
    product.protein
    #product.pf
}

beef.protein <- usda_raw_median("beef","\\braw\\b")  ## Bovine.
pork.protein <- usda_raw_median("pork","\\braw\\b")  ## Swine.
lamb.protein <- usda_raw_median("lamb","\\braw\\b")  ## Sheep.
poultry.protein <- usda_raw_median("poultry","\\braw\\b")  ## Fowl.
egg.protein <- usda_raw_median("egg","\\braw\\b")
butter.protein <- usda_raw_median("dairy","\\bbutter\\b")
cheese.protein <- usda_raw_median("dairy","cheese")
milk.protein <- usda_raw_median("dairy","milk", "\\b(dry|dried|condensed|evaporated)\\b")
yogurt.protein <- usda_raw_median("dairy","yogurt")

animal_s[,protein:=numeric(.N)]
animal_s[grep("yogurt",animal_s[,Products],ignore.case=T)
         ,"protein"]<-yogurt.protein
animal_s[grep("milk",animal_s[,Products],ignore.case=T)
         ,"protein"]<-milk.protein
animal_s[grep("eggs",animal_s[,Products],ignore.case=T)
         ,"protein"]<-egg.protein
animal_s[grep("fowl",animal_s[,Products],ignore.case=T)
         ,"protein"]<-poultry.protein
animal_s[grep("cheese",animal_s[,Products],ignore.case=T)
         ,"protein"]<-cheese.protein
animal_s[grep("butter",animal_s[,Products],ignore.case=T)
         ,"protein"]<-butter.protein
animal_s[grep("swine",animal_s[,Products],ignore.case=T)
         ,"protein"]<-pork.protein
animal_s[grep("sheep",animal_s[,Products],ignore.case=T)
         ,"protein"]<-lamb.protein
animal_s[grep("bovine",animal_s[,Products],ignore.case=T)
         ,"protein"]<-beef.protein
#View(animal_s)
# Recall that grams protein is measured per 100 grams of product.

### add plant/animal factor:
animal_s[,kingdom:=factor(.N)]
animal_s$kingdom <- factor(animal_s$kingdom,levels = c("plant","animal"))
animal_s$kingdom <- "animal"  ### note as.numeric "plant" = 1, "animal" = 2
as.numeric(animal_s$kingdom)





### USDA plant data
## These plants aren't easily grepped in a function.
## From plants_s with key California_footprint
badgreps <- c(1,9,13,20,21,25,29,31,33,35,36,37,41,42,43,45,46,48,53,54,56)
length(badgreps)

plants_t <- plants_s[-badgreps,]
View(plants_t)

# Get 1 big nutrient list for all relevant groups.
plant_g <- c("2000","0900","1600","1200","1100")
plantlist.prot1 <- fromJSON(paste0("http://api.nal.usda.gov/usda/ndb/nutrients/",
                "?format=json&api_key=",usda_api_key,"&max=1500&nutrients=",
                protein,"&fg=",plant_g[1],"&fg=",plant_g[2],"&fg=",plant_g[3],
                "&fg=",plant_g[4],"&fg=",plant_g[5]))
plantlist.prot2 <- fromJSON(paste0("http://api.nal.usda.gov/usda/ndb/nutrients/",
                                   "?format=json&api_key=",usda_api_key,"&max=1500",
                                   "&offset=1500&nutrients=",
                                   protein,"&fg=",plant_g[1],"&fg=",plant_g[2],"&fg=",plant_g[3],
                                   "&fg=",plant_g[4],"&fg=",plant_g[5]))
plantlist.prot <- rbind.fill(plantlist.prot1$report$foods,plantlist.prot2$report$foods)
plantlist.nutr <- rbind.fill(plantlist.prot$nutrients) ## separate out the nutrient sublist
plantlist.prot <- cbind(plantlist.prot,plantlist.nutr) ## slap those columns on.
#View(plantlist.prot)

plants_t[,protein:=numeric(.N)]

### add plant/animal factor:
plants_t[,kingdom:=factor(.N)]
plants_t$kingdom <- factor(plants_t$kingdom,levels = c("plant","animal"))
plants_t$kingdom <- "plant"  ### note as.numeric "plant" = 1, "animal" = 0



# write a function that:
#   grep plants_t for the first word in the Product column.
#   subsets nutrient list by that word + "raw"
#   returns the median to plants_s$protein.
# lapply or sapply function to plants_s.
usda_plants_raw_median <- function(product,nutrient,filter=NULL){
    ## product: string from which the first word will be taken.
    ## nutrient: use protein or kcal or sthg from DT nutrients.
    ## filter: string with which plantlist.prot will be filtered.
    product<-as.character(product)
    thisplant <- unlist( strsplit(product,"[^a-zA-Z]",perl=T) )[1]
    thisplant <- sub("s$","",thisplant)
    thisplant <- paste0("\\b",thisplant)
    plantlist <- as.data.table(plantlist.prot[grep(thisplant,
                            plantlist.prot$name,perl=T,ignore.case=T),])
    if(!missing(filter)) {
        ### e.g. "raw". Keep only raw cuts.
        if(length( grep(filter,plantlist$name,perl=T,ignore.case=T) ) > 0)
            plantlist <- as.data.table(plantlist[grep(filter,plantlist$name,perl=T,ignore.case=T),])
    }
    ### Finally, return the median.
    product.protein <- as.numeric(median(plantlist$gm))
    #list(product.protein, plantlist)     ## testing individual
    #list(product.protein, plantlist$gm)  ## testing group via lapply
    product.protein
}

plants_t$protein <- lapply(plants_t$Products,usda_plants_raw_median,protein,"\\braw\\b")

### Combine plants_t and animal_s
water <- as.data.table(rbind.fill(plants_t,animal_s))
setkey(water,California_footprint)
water$protein <- as.numeric(water$protein)


### Conversion factors
cf.m3_t.L_100g <- 0.1  ## cubic meters per metric ton to Liters per 100 grams.
cf.m3_t.gal_100g <- .026 ## m^3 per m.ton to US liquid gallons per 100 grams. 
cf.m3_t.gal_oz <- .007489 ## m^3/m.ton to US liq gallons per avoirdupois ounces.



water[,CA_gal_per_oz:=numeric(.N)]
water[,G_gal_per_oz:=numeric(.N)]
water[,CA_L_per_g_protein:=numeric(.N)]
water[,G_L_per_g_protein:=numeric(.N)]
water[,CA_gal_per_g_protein:=numeric(.N)]
water[,G_gal_per_g_protein:=numeric(.N)]

water$CA_gal_per_oz <- cf.m3_t.gal_oz * water$California_footprint
water$G_gal_per_oz <- cf.m3_t.gal_oz * water$Global_avg_footprint
water$CA_L_per_g_protein <- cf.m3_t.L_100g * water$California_footprint / water$protein
water$G_L_per_g_protein <- cf.m3_t.L_100g * water$Global_avg_footprint / water$protein
water$CA_gal_per_g_protein  <- cf.m3_t.gal_100g * water$California_footprint / water$protein
water$G_gal_per_g_protein <- cf.m3_t.gal_100g * water$Global_avg_footprint / water$protein

### Tables and plots

setkey(water,protein)
rm(California_footprint, Global_avg_footprint, Products, protein)
attach(water)

qplot(water, x=water$California_footprint,y=water$protein,
      color = water$kingdom)
qplot(water, x=water$Global_avg_footprint,y=water$protein,
      col = water$kingdom)


par(mfrow = c(1,1))
pairs(water[,c(2,3,4,5,8,9),with=F], panel = panel.smooth, 
      main = "water footprint data", 
      col = 2 + (as.numeric(water$kingdom) < 2)) ## color: red=2,green=3, plant=1,animal=2
cor(water$protein,water$California_footprint)

wfit0 <- lm(Global_avg_footprint ~ protein, water)
summary(wfit0)$r.squared
summary(wfit0)$coefficients

### Global_avg: We would expect a 390 m3/ton increase in the footprint for each 1 gram increase in protein.
### The amount of protein in the food explains 61% of the variance in the data.
### This makes sense because protein synthesis requires water.
### https://www.youtube.com/watch?v=H8WJ2KENlK0&index=3&list=PL3EED4C1D684D3ADF
### https://www.youtube.com/watch?v=itsb2SqR-R0&index=11&list=PL3EED4C1D684D3ADF
### CA: 450 m3/ton increase. r2 is 52%.

### Do animal-based foods have a bigger water footprint than plant-based foods?
wfit1 <- lm(Global_avg_footprint ~ as.numeric(kingdom), water)
summary(wfit1)$r.squared
summary(wfit1)$coefficients
### Well, it seems (p-value = 0.01) 
### there is a somewhat significant increase in the water footprint of 
### animal-based foods over plant-based foods.
### But it only explains 13% of the variance.

### Holding protein constant:
wfit2 <- lm(Global_avg_footprint ~ kingdom + protein, water)
summary(wfit2)$r.squared
summary(wfit2)$coefficients
### It seems protein content is very important in predicting water footprint.

## Examining the interaction between kingdom and protein
wfit3 <- lm(Global_avg_footprint ~ protein * kingdom, water)
summary(wfit3)$r.squared
summary(wfit3)$coefficients
### The reference category for `kingdom` is "1" -- plants, 
### which has and intercept of 155 m3/ton. 
### The slope for *animals* is 416. 
### The change in intercept for *animals* is 1685.
### And the change in slope for *animals* is -125.

### The interaction isn't significant (p: 0.3 > 0.05)

### Let's plot the regression lines for animal versus plant sources,
### with protein as an interaction:
par(mfrow = c(1,1))
plot(protein,Global_avg_footprint,pch=19)
points(protein,Global_avg_footprint,pch=19,col=((as.numeric(kingdom)<2)*1+2))
## Again, the as.numeric() turns the factor "0" into a 1
## and the factor "1" into a 2.
abline(c(wfit3$coeff[1],wfit3$coeff[2]),col="green",lwd=3)
abline(c(wfit3$coeff[1] + wfit3$coeff[3],wfit3$coeff[2] +wfit3$coeff[4]),col="red",lwd=3)
### The two food sources seem closely correlated, and that agrees
### with the interaction being insignificant.

### But let's check the residuals
par(mfrow = c(1,1))
ew <- wfit3$residuals 
plot(kingdom, ew,
     xlab = "kingdom (0=plant)",
     ylab = "Residuals")

### compared to fit2
par(mfrow = c(1,1))
ew <- wfit2$residuals 
plot(kingdom, ew,
     xlab = "kingdom (0=plant)",
     ylab = "Residuals")
### Very similar. Lots of outliers in plants.

## resdiuals and leverage
par(mfrow = c(2, 2))
plot(wfit3)
### Three products -- beef, butter, almonds -- have significant leverage in the model. 

### Let's see the model without them:
par(mfrow = c(1,1))
plot(protein[c(-12,-45,-46)],Global_avg_footprint[c(-12,-45,-46)],pch=19)
points(protein[c(-12,-45,-46)],Global_avg_footprint[c(-12,-45,-46)],pch=19,col=((as.numeric(kingdom[c(-12,-45,-46)])<2)*1+2))
abline(c(wfit3$coeff[1],wfit3$coeff[2]),col="green",lwd=3)
abline(c(wfit3$coeff[1] + wfit3$coeff[3],wfit3$coeff[2] +wfit3$coeff[4]),col="red",lwd=3)
### Pretty much the same.

### Analysis of variance to compare the three models:
anova(wfit0, wfit1, wfit2, wfit3)
#### anova shows that wfit2 
#### (predicting the footprint by kingdom holding protein constant)
#### is the best model.

###  
library(MASS)
wfit.s <- stepAIC(lm(Global_avg_footprint ~ ., data = water[,c(3,4,5),with=F]), trace = 0) 
summary(fit.s)$coeff




#### The USDA numbers and descriptions of foods are different
#### than the water footprint data.

### e.g. USDA doesn't have "bovine", and a search for "beef"
### yields 1154 results with a variety of protein content.
### "beef ground" yield 49 choices.
### Perhaps the best choice is: 
### "name": "Beef, ground, unspecified fat content, cooked",
### "ndbno": "23220"  (about 25 g protein)
### But "beef chuck" which yields 179 choices of higher protein values (up to 36 g).
