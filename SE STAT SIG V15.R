## added text to show new branch working
## Set wd
setwd('C:/Users/kmc00/OneDrive - CEFAS/R_PROJECTS/SedStatChange')
#getwd()

## Call libraries
library(dplyr)
library(broom)
library(rgdal)
library(sp)
library(sf)
library(raster)
library(ggplot2)
library(rgdal)
library(maptools)
library(plyr)

#### IMPORT POLYGONS FROM AWS ####
#install.packages("RPostgreSQL")
require("RPostgreSQL")

## create a connection. Save the password
pw <- {
  "r01S81nypG!"
}
logged= FALSE;

## loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

## Creates a connection to the postgres database. Note that "con" will be used later in each connection to the database
con =  dbConnect(drv, dbname = "roimar",
                 host = "testdb.cdyo1tzhwsqv.eu-west-2.rds.amazonaws.com", port = 5432,
                 user = "postgres", password = pw)
rm(pw) # removes the password

## See list of tables
dbListTables(con)

regions <- st_read_db(con, query =  "select region, region_name, area_shape_km2 , geom from ma_tool.extraction_regions", geom_column = 'geom' ) %>% as( "Spatial") 

piz <- st_read_db(con, query =  "select * from ma_tool.extraction_areas", geom_column = 'geom' ) %>% as( "Spatial") 

#siz <- st_read_db(con, query =  "select  * from ma_tool.extraction_areas_siz where region = 'sc'", geom_column = 'geom' ) %>% as( "Spatial") 
siz <- st_read_db(con, query =  "select  * from ma_tool.extraction_areas_siz", geom_column = 'geom' ) %>% as( "Spatial") 

## Plot PIZs, SIZs and regions
plot(piz)
plot(siz)
plot(regions)

## Load piz and siz from AWS and save as shapefiles for use when not connected to AWS
writeOGR(obj=piz, ".", layer="DATA/piz2", driver="ESRI Shapefile") #dsn=td,
writeOGR(obj=siz, ".", layer="DATA/siz2", driver="ESRI Shapefile") #dsn=td,
writeOGR(obj=regions, ".", layer="DATA/regions2", driver="ESRI Shapefile") #dsn=td,



#### 2. IMPORT POLYGONS FROM DATA FOLDER ####

## Load piz and siz fromm DATA folder
piz<-readOGR("DATA/piz2.shp")
siz<-readOGR("DATA/siz2.shp") 
regions<-readOGR("DATA/regions2.shp")
ref <- readOGR("DATA/SC_REF_POLYGONS_REV3.shp")

## Reinstate full column names (these are lost in the writeOGR step)
names(piz)=c("fid","gid","region","region_name","area_numbe","area_name","sub_type","company","area_shape","perimeter_shape","area_shape_km2","input_date","replaced","replaced_by","updated","updated_date","droped","droped_date")
names(siz)=c("fid","gid","gid_piz","region","region_name","area_numbe","area_name","sub_type","company","area_shape","perimeter_shape","input_date","replaced","replaced_by","updated","updated_date","droped","droped_date")
names(regions)=c("region","region_name","area_shape_km2")

plot(piz)
plot(siz)
plot(regions)
plot(ref)

## To see the attributes data
piz@data
#View(piz@data)

## Add attributes info for Ref
ref@data$Box=c("Box 1","Box 2","Box 3","Box 4","Box 5","Box 6")
ref@data$Region <- c("South Coast","South Coast","South Coast","South Coast","South Coast","South Coast")
ref@data$Sub_Region <- c("West IOW","East IOW","East IOW","East IOW","Owers","Hastings")
#View(ref@data)



## Add subregion information to attributes table for PIZ
piz@data$sub_region <- ifelse(piz@data$area_numbe == "501/1"|
                                piz@data$area_numbe == "501/2",
                                "Thames Offshore",
                        ifelse(piz@data$area_numbe == "447"|
                                piz@data$area_numbe == "509/1"|
                                piz@data$area_numbe == "509/2"|
                                piz@data$area_numbe == "510/1"|
                                piz@data$area_numbe == "510/2",
                                "Thames South",
                        ifelse(piz@data$area_numbe == "498"|
                                piz@data$area_numbe == "507/1"|
                                piz@data$area_numbe == "507/2"|
                                piz@data$area_numbe == "507/3"|
                                piz@data$area_numbe == "507/4"|
                                piz@data$area_numbe == "507/5"|
                                piz@data$area_numbe == "507/6",
                                "Thames North",
                        ifelse(piz@data$area_numbe == "472"|
                                piz@data$area_numbe == "476"|
                                piz@data$area_numbe == "486/1"|
                                piz@data$area_numbe == "486/2"|
                                piz@data$area_numbe == "486/3"|
                                piz@data$area_numbe == "486/4"|
                                piz@data$area_numbe == "486/5",
                                "Bristol Channel True",
                        ifelse(piz@data$area_numbe == "470/1"|
                                piz@data$area_numbe == "470/2"|
                                piz@data$area_numbe == "455"|
                                piz@data$area_numbe == "459"|
                                piz@data$area_name == "Bedwyn Sands",
                                "Severn Estuary",      
                        ifelse(piz@data$area_numbe == "506"|
                                piz@data$area_numbe == "483"|
                                piz@data$area_numbe == "484"|
                                piz@data$area_numbe == "492"|
                                piz@data$area_numbe == "515/2"|
                                piz@data$area_numbe == "515/1",
                                "Humber Offshore",
                        ifelse(piz@data$area_numbe == "514/1"|
                                piz@data$area_numbe == "514/2"|
                                piz@data$area_numbe == "514/3"|
                                piz@data$area_numbe == "514/4"|
                                piz@data$area_numbe == "493"|
                                piz@data$area_numbe == "197"|
                                piz@data$area_numbe == "400"|
                                piz@data$area_numbe == "106/1"|
                                piz@data$area_numbe == "106/2"|
                                piz@data$area_numbe == "106/3"|
                                piz@data$area_numbe == "480"|
                                piz@data$area_numbe == "439"|
                                piz@data$area_numbe == "481/1"|
                                piz@data$area_numbe == "481/2",
                                "Humber Inshore",      
                        ifelse(piz@data$area_numbe == "511"|
                                piz@data$area_numbe == "512"|
                                piz@data$area_numbe == "228"|
                                piz@data$area_numbe == "240"|
                                piz@data$area_numbe == "254"|
                                piz@data$area_numbe == "212"|
                                piz@data$area_numbe == "296"|
                                piz@data$area_numbe == "494",
                                "Anglian Inshore",
                        ifelse(piz@data$area_numbe == "513/1"|
                                piz@data$area_numbe == "513/2"|
                                piz@data$area_numbe == "401/2B"|
                                piz@data$area_numbe == "525"|
                                 piz@data$area_numbe == "430"| 
                                piz@data$area_numbe == "242-361",
                                "Anglian Offshore",
                              ifelse(piz@data$area_numbe == "407"|
                                piz@data$area_numbe == "340"|
                                piz@data$area_numbe == "372/1"|
                                piz@data$area_numbe == "451"|
                                piz@data$area_numbe == "395/2"|
                                piz@data$area_numbe == "395/1"|
                                piz@data$area_numbe == "351",
                                "East IOW",
                         ifelse(piz@data$area_numbe == "127"|
                                piz@data$area_numbe == "137"|
                                piz@data$area_numbe == "500/1"|
                                piz@data$area_numbe == "500/2"|
                                piz@data$area_numbe == "500/3"|
                                piz@data$area_numbe == "500/4"|
                                piz@data$area_numbe == "500/5"|
                                piz@data$area_numbe == "500/6",
                                "West IOW",
                        ifelse(piz@data$area_numbe == "499"|
                                piz@data$area_numbe == "435/2"|
                                piz@data$area_numbe == "435/1"|
                                piz@data$area_numbe == "396/1"|
                                piz@data$area_numbe == "488"|
                                piz@data$area_numbe == "453"|
                                piz@data$area_numbe == "396/2",
                                "Owers",
                               ifelse(piz@data$area_numbe == "460",                                                                                                                 
                                      "Hastings",
                                NA)))))))))))))

## Add subregion information to attributes table for SIZ
siz@data$sub_region <- ifelse(siz@data$area_numbe == "501/1"|
                               siz@data$area_numbe == "501/2",
                              "Thames Offshore",
                        ifelse(siz@data$area_numbe == "447"|
                                siz@data$area_numbe == "509/1"|
                                siz@data$area_numbe == "509/2"|
                                siz@data$area_numbe == "510/1"|
                                siz@data$area_numbe == "510/2",
                                "Thames South",
                        ifelse(siz@data$area_numbe == "498"|
                                siz@data$area_numbe == "507/1"|
                                siz@data$area_numbe == "507/2"|
                                siz@data$area_numbe == "507/3"|
                                siz@data$area_numbe == "507/4"|
                                siz@data$area_numbe == "507/5"|
                                siz@data$area_numbe == "507/6",
                                "Thames North",
                        ifelse(siz@data$area_numbe == "472"|
                                siz@data$area_numbe == "476"|
                                siz@data$area_numbe == "486/1"|
                                siz@data$area_numbe == "486/2"|
                                siz@data$area_numbe == "486/3"|
                                siz@data$area_numbe == "486/4"|
                                siz@data$area_numbe == "486/5",
                                "Bristol Channel True",
                        ifelse(siz@data$area_numbe == "470/1"|
                                siz@data$area_numbe == "470/2"|
                                siz@data$area_numbe == "455"|
                                siz@data$area_numbe == "459"|
                                siz@data$area_name == "Bedwyn Sands",
                                "Severn Estuary",      
                        ifelse(siz@data$area_numbe == "506"|
                                siz@data$area_numbe == "483"|
                                siz@data$area_numbe == "484"|
                                siz@data$area_numbe == "492"|
                                siz@data$area_numbe == "515/2"|
                                siz@data$area_numbe == "515/1",
                                "Humber Offshore",
                        ifelse(siz@data$area_numbe == "514/1"|
                                siz@data$area_numbe == "514/2"|
                                siz@data$area_numbe == "514/3"|
                                siz@data$area_numbe == "514/4"|
                                siz@data$area_numbe == "493"|
                                siz@data$area_numbe == "197"|
                                siz@data$area_numbe == "400"|
                                siz@data$area_numbe == "106/1"|
                                siz@data$area_numbe == "106/2"|
                                siz@data$area_numbe == "106/3"|
                                siz@data$area_numbe == "480"|
                                siz@data$area_numbe == "439"|
                                siz@data$area_numbe == "481/1"|
                                siz@data$area_numbe == "481/2",
                                "Humber Inshore",      
                        ifelse(siz@data$area_numbe == "511"|
                                siz@data$area_numbe == "512"|
                                siz@data$area_numbe == "228"|
                                siz@data$area_numbe == "240"|
                                siz@data$area_numbe == "254"|
                                siz@data$area_numbe == "212"|
                                siz@data$area_numbe == "296"|
                                siz@data$area_numbe == "494",
                                "Anglian Inshore",
                        ifelse(siz@data$area_numbe == "513/1"|
                                siz@data$area_numbe == "513/2"|
                                siz@data$area_numbe == "401/2B"|
                                siz@data$area_numbe == "525"|
                                siz@data$area_numbe == "430"| 
                                siz@data$area_numbe == "242-361",
                                "Anglian Offshore",
                          ifelse(siz@data$area_numbe == "407"|
                                 siz@data$area_numbe == "340"|
                                 siz@data$area_numbe == "372/1"|
                                 siz@data$area_numbe == "451"|
                                 siz@data$area_numbe == "395/2"|
                                 siz@data$area_numbe == "395/1"|
                                 siz@data$area_numbe == "351",
                                 "East IOW",
                          ifelse(siz@data$area_numbe == "127"|
                                 siz@data$area_numbe == "137"|
                                 siz@data$area_numbe == "500/1"|
                                 siz@data$area_numbe == "500/2"|
                                 siz@data$area_numbe == "500/3"|
                                 siz@data$area_numbe == "500/4"|
                                 siz@data$area_numbe == "500/5"|
                                 siz@data$area_numbe == "500/6",
                                 "West IOW",
                          ifelse(siz@data$area_numbe == "499"|
                                siz@data$area_numbe == "435/2"|
                                siz@data$area_numbe == "435/1"|
                                siz@data$area_numbe == "396/1"|
                                siz@data$area_numbe == "488"|
                                siz@data$area_numbe == "453"|
                                siz@data$area_numbe == "396/2",
                                "Owers",
                          ifelse(siz@data$area_numbe == "460",                                                                                                                 
                                "Hastings",
                               NA)))))))))))))

#View(siz@data)

## Plot only licences from WestIOW sub_region
piz.wiow <- subset(piz, sub_region=="West IOW")
plot(piz.wiow)
piz.eiow<- subset(piz, sub_region=="East IOW")
plot(piz.eiow)
piz.owers<- subset(piz, sub_region=="Owers")
plot(piz.owers)
piz.hunoff <- subset(piz, sub_region=="Humber Offshore")
plot(piz.hunoff)
piz.humins <- subset(piz, sub_region=="Humber Inshore")
plot(piz.humins)


#### 3. IMPORT MONITORING DATA ####
## Load SC monitoring data. Proportions of major sediment fractions by RSMP code, with coordinates
mondat=read.csv("DATA/SCSEDMONDATAINCPOS2017.csv",header=T,na.strings=c("NA", "-","?","<null>"),stringsAsFactors=F,check.names=FALSE)
mondat
dim(mondat) #550 10

## Add col for time (m = Monitoring)
mondat$time="m"

## Change name of col1 from 'RSMP station code' to 'Code'
colnames(mondat)[1] <- "Code"
#View(mondat)

## Create a df for monitoring positions
pts_m=SpatialPoints(mondat[,(10:9)], 
                  proj4string = CRS(proj4string(piz)))



#### 4. RETRIEVE INFO (FROM GIS LAYERS) BY SAMPLE ####

## Extract data by sample
pizgis=over(pts_m,piz) # PIZ
sizgis=over(pts_m,siz) # SIZ
contgis=over(pts_m,regions) # CONTEXT
refgis=over(pts_m,ref) # REF

## Add station codes to extracted data
stations=mondat[,1]# vector for station codes
pizgis$Code <- stations
sizgis$Code <- stations
contgis$Code <- stations
refgis$Code <- stations

## Add Treatment column
pizgis$Treatment <- "PIZ"
sizgis$Treatment <- "SIZ"
contgis$Treatment <- "REF"
refgis$Treatment <- "REF"

## Remove records from above objects that are not relevant (i.e. not associated with the relevant treatment)
pizgis2 <- pizgis[!is.na(pizgis$area_numb),] # PIZ, remove NAs
dim(pizgis2)#206

## 2 step process for SIZ: i).  Remove records with no area (i.e. Context and Ref)
sizgis2 <- sizgis[!is.na(sizgis$area_numb),]
dim(sizgis2)#411

## ii) Remove records any stations which are also present in the PIZ object
sizgis3 <-sizgis2[!sizgis2$Code %in% pizgis2$Code, , drop = FALSE]
dim(sizgis3)#206

## Drop non-ref stations
refgis2 <- refgis[!is.na(refgis$Box),]
dim(refgis2)#81

## Drop non-context stations by removing stations present in piz, siz and ref
contgis2 <- contgis[!contgis$Code %in% pizgis2$Code, , drop = FALSE]#remove piz stations
contgis3 <- contgis2[!contgis2$Code %in% sizgis3$Code, , drop = FALSE]#remove siz stations
contgis4 <- contgis3[!contgis3$Code %in% refgis2$Code, , drop = FALSE]#remove ref stations
dim(contgis4)

## Now make sure all GIS query objects have required fields: Code, Region, Sub-region, Treatment, Area
# PIZ
names(pizgis2)
pizgis3 <- pizgis2 [,c(20,4,19,21,5)]
colnames(pizgis3) <- c("Code","Region","Sub_region","Treatment","Area")

# SIZ
names(sizgis4)
sizgis4 <- sizgis3[,c(20,5,19,21,6)]
colnames(sizgis4) <- c("Code","Region","Sub_region","Treatment","Area")

# REF
names(refgis2)
refgis3 <- refgis2[,c(5,3,4,6,2)]
names(refgis3)
colnames(refgis3) <- c("Code","Region","Sub_region","Treatment","Area")

# CONTEXT
names(contgis4)
contgis5 <- contgis4[,c(4,2,5)]
colnames(contgis5) <- c("Code","Region","Treatment")
contgis5$Sub_region <- NA
contgis5$Area <- "Context"
contgis6=contgis5[,c(1,2,4,3,5)]
names(contgis6)

## Now bring GIS query objects together
treatall <- rbind(pizgis3,sizgis4,refgis3,contgis6)
#View(treatall)

## Order object 'treatall' by Code so it's ready ro cbind to monitoring data
treatall2=treatall[order(treatall$Code),]



#### 5. IMPORT BASELINE DATA ####
## Bring in  SC baseline data and match to monitoring data
basdat=read.csv("DATA/SCSEDBASDATAINCPOS.csv",header=T,na.strings=c("NA", "-","?","<null>"),stringsAsFactors=F,check.names=FALSE)
basdat
dim(basdat)# 771  10

## Add col for 'time' (b = Baseline)
basdat$time="b"

## Change name of col1 to 'Code'
colnames(basdat)[1] <- "Code"
#View(basdat)



#### 6. MATCH MONITORING AND BASELINE DATA ####

## Select baseline samples where there is an accompanying monitoring sample
basdat2=basdat %>%
  filter(Code %in% mondat$Code)
dim(basdat2)# 203 20
dim(mondat)
#View(basdat5)

## Make sure there are no stations in the monitoring set without a corresponding baseline station
mondat2=mondat %>%
  filter(Code %in% basdat2$Code)

mondat2
dim(mondat2)
#View(mondat2)

## Now join the baseline df to the monitoring df
data=rbind(basdat2,mondat2)
#View(data)
dim(data)#514


## Now stack treatall2 on treatall2 so this object can be joinied to df data (m and b)
treatall3=rbind(treatall2,treatall2)
dim(treatall3)

## only keep stations in treatall2 that are in data
treatall4 <- treatall3 %>% semi_join(data, by = "Code") 
dim(test)
#View(test)
## Now add mon and baseline sed data to gis treatment object
data2 <- cbind(treatall4,data[,2:11])
#View(data2)
names(data2)
###############################################################
#20/08/2019
## Create a col for Licence no. and PIZ
data2$site <- paste(data2$Area,data2$Treatment)

## Drop cols Treatment and Area_Numbe from df data 2
names(data2)
data3=data2[,c(1,6:12,15,16,2:4)]
names(data3)

## Check and update col types
str(data3)
data3$site=as.factor(data3$site)
data3$time=as.factor(data3$time)
data3$Treatment=as.factor(data3$Treatment)
data3$Sub_region=as.factor(data3$Sub_region)
names(data3)
str(data3)
## Get dat

#############################################################
#21/08/2019
## Repeat data so outputs can be produced for PIZ, SIZ REF
data3mod=data3
data3mod$site <- data3mod$Treatment
datatest=rbind(data3,data3mod)
dim(datatest)
names(datatest)

## Repeat data so outputs can be produced for subregion PIZ, SIZ REF
data3sub <- data3
data3sub$site <- paste(data3sub$Sub_region,data3sub$Treatment)
datatest2=rbind(datatest,data3sub)
data3=datatest2
View(data3)
#############################################################
## 21/08/2019 pm Get object data3 into sensible order
## Reorder rows
str(data3)
target <- data3$site
target
## Change order of treatments



target <- c("PIZ",
            "SIZ",
            "REF",
            
            "West IOW PIZ",
            "West IOW SIZ",
            "West IOW REF",
            "East IOW PIZ",
            "East IOW SIZ",
            "East IOW REF",
            "Owers PIZ",
            "Owers SIZ",
            "Owers REF",
            "Hastings PIZ",
            "Hastings SIZ",
            "Hastings REF",
            "127 PIZ",    
            "137 PIZ",
            "340 PIZ",
            "351 PIZ",
            "372/1 PIZ",
            "395/1 PIZ",
            "395/2 PIZ",
            "396/1 PIZ",
            "407 PIZ",
            "435/1 PIZ",
            "435/2 PIZ",            
            "451 PIZ",
            "460 PIZ",
            "488 PIZ",
            "500/3 PIZ",
            
            
            
            
            
            
            "127 SIZ",
            "137 SIZ",
            "340 SIZ",
            "351 SIZ",
            "372/1 SIZ",
            "395/1 SIZ",
            "395/2 SIZ",
            "396/1 SIZ",
            "407 SIZ",
            "435/1 SIZ",
            "435/2 SIZ",
            "451 SIZ", 
            "453 PIZ",
            "460 SIZ",
            "488 SIZ",
            "500/3 SIZ",
            
            
            
            
            
            
            
            
            "NA REF" , 
            "Context REF",
            "Box 1 REF", 
            "Box 2 REF", 
            "Box 3 REF", 
            "Box 4 REF", 
            "Box 5 REF", 
            "Box 6 REF" 
            
)

require(gdata)
data3$site <- reorder.factor(data3$site, new.order=target)
require(dplyr)
data4 <- data3 %>%
  arrange(site)

View(data4)

data3=data4


##############################################
#### PIZ SED SUMMARY BASELINE/MONITORING ####
## Get summary data by area
#library(plyr)
detach("package:plyr", unload=TRUE) 
library(dplyr)

## Data by Area
sumdata=data3[1:10]%>%
  group_by(site,time) %>%
  summarise(
    count = n(),
    sc = mean(SC, na.rm = TRUE),  
    fS = mean(fS, na.rm = TRUE),
    mS = mean(mS, na.rm = TRUE),
    cS = mean(cS, na.rm = TRUE),
    fG = mean(fG, na.rm = TRUE),
    mG = mean(mG, na.rm = TRUE),
    cG = mean(cG, na.rm = TRUE))
sumdata
View(sumdata)



######################################################################################################################
#20/0/2019
#### PIZ WILCOX TESTS  4 SITES ####
## Take relevant columns
names(data3)
## Take relevant columns
data4=data3[,c(10:9,2:8)]
str(data4)

## Creat a vector for site
site = as.character(data4$site)

## Identify the number of sites for each site
table(site)

## Vector for site names
#site.names = c("127 PIZ", "137 PIZ", "340 PIZ", "351 PIZ", "372/1 PIZ","395/1 PIZ", "395/2 PIZ", "396/1 PIZ", "407 PIZ","435/1 PIZ", "435/2 PIZ", "451 PIZ", "453 PIZ", "460 PIZ","488 PIZ", "500/3 PIZ")
site.names =as.character(levels(data4$site))
## Number of sites
nsites = length(site.names)# 16

## Matrix for p-values
pmatrix = matrix(999, ncol=7, nrow=nsites)

## You just do a loop over the N sites. Select out the rows for the jth site (j=1 to N) on each iteration of the loop. And then do the Wilcoxon tests for that site
for (j in 1:nsites) {
  use = data4[site==site.names[j],]
  for (k in 3:9) {
    varb = use[,k][use$time=="b"]
    varm = use[,k][use$time=="m"]
    pmatrix[j,(k-2)] = wilcox.test(varb ,varm, alt="two.sided",paired=T)$p.value
  }
}

round(pmatrix,3)
## Change pmatrix from a matrix to a dataframe
pmatrix=as.data.frame(pmatrix)
pmatrix

## Add in column names
colnames(pmatrix)=c("cG","mG","fG","cS","mS","fS","SC")
pmatrix

## Add in row names (site)
pmatrix$site=site.names
pmatrix

## Change order of df
pmatrix2=pmatrix[,c(8,1:7)]
pmatrix2

## Round p-values to 3 dp
pmatrix2$cG=round(pmatrix2$cG,3)
pmatrix2$mG=round(pmatrix2$mG,3)
pmatrix2$fG=round(pmatrix2$fG,3)
pmatrix2$cS=round(pmatrix2$cS,3)
pmatrix2$mS=round(pmatrix2$mS,3)
pmatrix2$fS=round(pmatrix2$fS,3)
pmatrix2$SC=round(pmatrix2$SC,3)
pmatrix2
View(pmatrix2)



###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################


#### PIZ CBIND SUMMARY AND WILCOX TEST ####

## Split piz summary data into baseline and monitoring subsets
sumdata_b=sumdata[which(sumdata$time=="b"),]
sumdata_m=sumdata[which(sumdata$time=="m"),]

## Check same number of columns
dim(sumdata_b)#16 10
dim(sumdata_b)# 16 10
dim(pmatrix2)# 16 8


## Make sure all objects are class df
sumdata_b=as.data.frame(sumdata_b)#16 10
sumdata_m=as.data.frame(sumdata_m)# 16 10
class(sumdata_b)#16 10
class(sumdata_m)
class(pmatrix2)

## Stitch together the results of Wilcox tests (df pmatrix2) with baseline and monitoring sed percentages
piz_pvalues_means=cbind(sumdata_b,sumdata_m,pmatrix2)
View(piz_pvalues_means)
## Change col names
names(piz_pvalues_means)
colnames(piz_pvalues_means)=c("site","time","count","sc_b","fS_b","mS_b","cS_b","fG_b","mG_b","cG_b","site","time",  "count","sc_m","fS_m","mS_m","cS_m","fG_m","mG_m","cG_m","site","cG_p","mG_p","fG_p","cS_p","mS_p","fS_p","SC_p")

#View(piz_pvalues_means)
names(piz_pvalues_means)

## Get all to the top
#n_row=nrow(piz_pvalues_means)

## Get cols in sensible order
#piz_pvalues_means2=piz_pvalues_means[c(n_row,1:n_row-1),c(1,11,8:2,12,22,13,23,14,24,15,25,16,26,17,27,18,28)]
#piz_pvalues_means2=piz_pvalues_means[,c(1,11,8:2,12,22,13,23,14,24,15,25,16,26,17,27,18,28)]
#piz_pvalues_means2=piz_pvalues_means[,c(1,11,8:2,12:18,22:28)]
piz_pvalues_means2=piz_pvalues_means[,c(1,3,4:10,14:20,28:22)]

View(piz_pvalues_means2)

## Calculate change in sed fractions between baseline and monitoring
names(piz_pvalues_means2)
piz_pvalues_means2$sc_change <- piz_pvalues_means2$sc_m-piz_pvalues_means2$sc_b
piz_pvalues_means2$fS_change <- piz_pvalues_means2$fS_m-piz_pvalues_means2$fS_b
piz_pvalues_means2$mS_change <- piz_pvalues_means2$mS_m-piz_pvalues_means2$mS_b
piz_pvalues_means2$cS_change <- piz_pvalues_means2$cS_m-piz_pvalues_means2$cS_b
piz_pvalues_means2$fG_change <- piz_pvalues_means2$fG_m-piz_pvalues_means2$fG_b
piz_pvalues_means2$mG_change <- piz_pvalues_means2$mG_m-piz_pvalues_means2$mG_b
piz_pvalues_means2$cG_change <- piz_pvalues_means2$cG_m-piz_pvalues_means2$cG_b

## Take cols of interest: change and p-values
str(piz_pvalues_means2)
#pizchange <- piz_pvalues_means2[,c(1:9,24:30)]
pizchange <- piz_pvalues_means2
View(pizchange)

## get into order by n
class(pizchange)
str(pizchange)
#pizchange <- pizchange[order(-pizchange$count),]

## Round p-values to 3 dp
pizchange$sc_change=round(pizchange$sc_change,1)
pizchange$fS_change=round(pizchange$fS_change,1)
pizchange$mS_change=round(pizchange$mS_change,1)
pizchange$cS_change=round(pizchange$cS_change,1)
pizchange$fG_change=round(pizchange$fG_change,1)
pizchange$mG_change=round(pizchange$mG_change,1)
pizchange$cG_change=round(pizchange$cG_change,1)

## change order ofcols
names(pizchange)
pizchange=pizchange[,c(1:16,24:30,17:23)]

## Create a nice table
View(pizchange)
pizchange2 <- pizchange
#colnames(pizchange2) <- c("Site", "n","SCp", "fSp", "mSp", "cSp", "fGp", "mGp", "cGp", "SC", "fS", "mS", "cS", "fG", "mG", "cG","SC", "fS", "mS", "cS", "fG", "mG", "cG","SC", "fS", "mS", "cS", "fG", "mG", "cG")
#pizchange2 <- pizchange2[,c(1:2,10,3,11,4,12,5,13,6,14,7,15,8,16,9)]
View(pizchange2)

#### nICE TABLE FOR REPORTING PIZ DATA ####

# https://cran.r-project.org/web/packages/flextable/vignettes/layout.html#manage-headers-and-footers
library(flextable)
library(officer)

## This works
#piztab <- flextable(pizchange2)
#piztab

#ft <- flextable(head(pizchange2))
#class(pizchange2)
#names(piztab)
#piztab <- flextable(pizchange2,col_keys=c("Site", "n","SC(p)", "fS(p)", "mS(p)", "cS(p)", "fG(p)", "mG(p)", "cG(p)", "SC", "fS", "mS", "cS", "fG", "mG", "cG"))
ft <- flextable(pizchange2)
names(pizchange2)

#################################
ft <- flextable(pizchange2,col_keys=c("site" ,"count",
 "sc_b",      "fS_b",      "mS_b",      "cS_b",      "fG_b" ,     "mG_b",      "cG_b"  ,"sc_m",      "fS_m",      "mS_m" ,     "cS_m",      "fG_m" ,     "mG_m" ,     "cG_m",      "sc_change","fS_change", "mS_change", "cS_change" ,"fG_change" ,"mG_change" ,"cG_change",     "SC_p" ,     "fS_p",      "mS_p",      "cS_p" ,     "fG_p" ,     "mG_p" ,    "cG_p"))

###########################



#ft <- flextable(pizchange2,col_keys=c(
#                         "Site",
#                         "n",
#                         "SCp",
#                         "fSp",
 #                        "mSp",
 #                        "cSp",
 #                        "fGp",
 #                        "mGp",
 #                        "cGp",
 #                        "SC",
 #                        "fS",
 #                        "mS",
 #                        "cS",
 #                        "fG",
 #                        "mG",
 #                        "cG"))

## Change row names
#ft <- set_header_labels(ft,SCp="SC",fSp="fS",mSp="mS", cSp="cS", fGp="fG", mGp="mG", cGp="cG")
#ft <- set_header_labels(ft,SCp="SC",fSp="fS",mSp="mS", cSp="cS", fGp="fG", mGp="mG", cGp="cG")
ft <- set_header_labels(ft,site="Site",count="n",
                        
                        sc_b="SC",fS_b="fS",mS_b="mS",cS_b="cS",fG_b="fG",mG_b="mG",cG_b="cG",
                        sc_m="SC",fS_m="fS",mS_m="mS",cS_m="cS",fG_m="fG",mG_m="mG",cG_m="cG",
                        sc_change="SC",fS_change="fS",mS_change="mS",cS_change="cS",fG_change="fG",mG_change="mG",cG_change="cG",
                        SC_p="SC",fS_p="fS",mS_p="mS",cS_p="cS",fG_p="fG",mG_p="mG",cG_p="cG")

## Add secondary header row
ft <- add_header_row(ft, values=c("","","Baseline","","","","","","","Monitoring","","","","","","","Change","","","","","","","P-values","","","","","",""),top=TRUE)

## Add table description
ft <- add_header_lines(ft,values=c("Table. 1. P-values and change in sediment composition by fraction between baseline and 2018 monitoring"))

## Make header bold
ft <- bold(ft,part="header")

### align text
ft <- align(ft,align="left", part="header")
ft <- align(ft,align="left", part="body")

##Change number of dp
#col_keys=c(
#  "Site",
#  "n",
#  "SCp",
#  "fSp",
#  "mSp",
#  "cSp",
#  "fGp",
#  "mGp",
#  "cGp",
#  "SC",
#  "fS",
#  "mS",
#  "cS",
#  "fG",
#  "mG",
#  "cG")
#ft <- colformat_num(ft,  col_keys = col_keys,digits = 2)


## Align text in Site column
#ft <- align_text_col(ft, align = "left")
#ft <- align_nottext_col(ft, align = "right")

#ft=hline(ft, 1:5, j = 1:3, border = NULL, part = "body")


ft



