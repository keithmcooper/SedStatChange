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

#### IMPORT POLYGONS ####
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


#### IMPORT MONITORING DATA ####
## Load SC monitoring data
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

#### PIZ MONITORING DATA ####
## Plot monitoring data points on PIZ polygons
plot(piz, axes = TRUE)
points(pts_m,col = "blue", cex = 0.4,pch=20)

## Identify sample locations
mondat2=over(pts_m, piz)
#View(mondat2)
mondat3=cbind(mondat,mondat2)
#View(mondat3)
dim(mondat3)#550 20

## Remove any samples not within a PIZ
mondat4=mondat3[!is.na(mondat3$area_numbe),]
dim(mondat4)# 206 20
#View(mondat4)

## Plot df 'test3' to make sure all samples are within PIZ extents
pts_m2=SpatialPoints(mondat4[,(10:9)], 
                     proj4string = CRS(proj4string(piz)))
plot(piz, axes = TRUE)
points(pts_m2,col = "blue", cex = 0.4,pch=20)


#### PIZ BASELINE DATA ####
## Bring in  SC baseline data
basdat=read.csv("DATA/SCSEDBASDATAINCPOS.csv",header=T,na.strings=c("NA", "-","?","<null>"),stringsAsFactors=F,check.names=FALSE)
basdat
dim(basdat)# 771  10

## Add col for 'time' (Baseline or Monitoring)
basdat$time="b"

## Change name of col1 to 'Code'
colnames(basdat)[1] <- "Code"
#View(basdat)

## Create a df for monitoring positions
pts_b=SpatialPoints(basdat[,(10:9)], 
                  proj4string = CRS(proj4string(piz)))

## Plot monitoring data points on PIZ polygons
plot(piz, axes = TRUE)
points(pts_b,col = "red", cex = 0.4,pch=20)

## Identify sample locations
basdat2=over(pts_b, piz)
#View(basdat2)
basdat3=cbind(basdat,basdat2)
#View(basdat3)
dim(basdat3)#771 20

## Remove any samples not within a PIZ
basdat4=basdat3[!is.na(basdat3$area_numbe),]
dim(basdat4)# 265 20
#View(basdat4)

## Plot df 'test_b3' to make sure all samples are within PIZ extents
pts_b2=SpatialPoints(basdat4[,(10:9)], 
                    proj4string = CRS(proj4string(piz)))
plot(piz, axes = TRUE)
points(pts_b2,col = "red", cex = 0.4,pch=20)


#### PIZ MATCHED MONITORING AND BASELINE DATA ####
## Select baseline samples where there is an accompanying monitoring sample
basdat5=basdat4 %>%
  filter(Code %in% mondat4$Code)
dim(basdat5)# 203 20
#View(basdat5)

## Make sure there are no stations in the monitoring set without a corresponding baseline station
mondat5=mondat4 %>%
  filter(Code %in% basdat5$Code)
mondat5
#View(mondat2)

## Now join the baseline df to the monitoring df
data=rbind(basdat5,mondat5)
#View(data)

## Add column for PIZ
data$Treatment="PIZ"

## Just take the required columns from df data (Code, seds, time, area_numbe,Treatment)
names(data)
str(data)
data2=data[,c(1:8,11,16,30)]
#View(data2)
names(data2)

## Create a col for Licence no. and PIZ
data2$site <- paste(data2$area_numbe,data2$Treatment)

## Drop cols Treatment and Area_Numbe from df data 2
data3=data2[,c(1:9,12)]
#View(data3)

## Check and update col types
str(data3)
data3$site=as.factor(data3$site)
data3$time=as.factor(data3$time)
#View(data3)

#### PIZ SED SUMMARY BASELINE/MONITORING ####
## Get summary data by area
#library(plyr)
detach("package:plyr", unload=TRUE) 
#library(dplyr)
sumdata=data3%>%
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
#View(sumdata)


#### PIZ WILCOX TESTS ####
## Take relevant columns
data4=data3[,c(10:9,2:8)]
data4

## Creat a vector for site
site = as.character(data4$site)

## Identify the number of sites for each site
table(site)

## Vector for site names
site.names = c("127 PIZ", "137 PIZ", "340 PIZ", "351 PIZ", "372/1 PIZ",
               "395/1 PIZ", "395/2 PIZ", "396/1 PIZ", "407 PIZ",
               "435/1 PIZ", "435/2 PIZ", "451 PIZ", "453 PIZ", "460 PIZ",
               "488 PIZ", "500/3 PIZ")

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
piz_pvalues_means=cbind(pmatrix2,sumdata_b,sumdata_m)

## Change col names
names(piz_pvalues_means)
colnames(piz_pvalues_means)=c("site","cG_p","mG_p","fG_p","cS_p","mS_p","fS_p","SC_p","site","time","count","sc_b","fS_b","mS_b","cS_b","fG_b","mG_b","cG_b","site","time",  "count","sc_m","fS_m","mS_m","cS_m","fG_m","mG_m","cG_m")

#View(piz_pvalues_means)
names(piz_pvalues_means)

## Get cols in sensible order
piz_pvalues_means2=piz_pvalues_means[,c(1,11,8:2,12,22,13,23,14,24,15,25,16,26,17,27,18,28)]
View(piz_pvalues_means2)


#### SIZ MONITORING DATA ####
## Plot monitoring data points on SIZ polygons
plot(siz, axes = TRUE)
points(pts_m,col = "blue", cex = 0.4,pch=20)

## Start with mondat3 - this is the monitoring samples with piz affiliation. You can therefore remove samples from within PIZ
mondat3_SIZ=mondat3[is.na(mondat3$area_numbe),]#monitoring samples minus those in PIZ
#View(mondat3_SIZ)

## Drop the cols from mondat3_SIZ that were associated with the PIZ file 
mondat3_SIZ2=mondat3_SIZ[,1:11]
#View(mondat3_SIZ2)#PIZ stations removed

## Monitoring sample positions (non PIZ stations)
pts_msiz=SpatialPoints(mondat3_SIZ2[,(10:9)], 
                    proj4string = CRS(proj4string(siz)))

plot(siz)
points(pts_msiz,col = "blue", cex = 0.01,pch=20)

## Get SIZ affiliations for monitoring sample locations
mon_nonpiz=over(pts_msiz,siz)
#View(mon_nonpiz)

## Make a df for non PIZ monitoring samples with SIZ affiliation
mon_nonpiz=cbind(mondat3_SIZ2,mon_nonpiz)
#View(mon_nonpiz)
dim(mon_nonpiz)#550 20

## Remove any samples not within a SIZ
mon_nonpiz2=mon_nonpiz[!is.na(mon_nonpiz$area_numbe),]
dim(mon_nonpiz2)# 206 20
#View(mon_nonpiz2)

## Plot points' to make sure all are within SIZ extents
pts_msiz2=SpatialPoints(mon_nonpiz2[,(10:9)], 
                     proj4string = CRS(proj4string(siz)))
plot(siz, axes = TRUE)
points(pts_msiz2,col = "blue", cex = 0.01,pch=20)

## Produce df for monitoring stations by SIZ. Note that intersecting samples will appear more than once.
e <- extract(siz, pts_msiz2)
#View(e)# Note there are 205 point IDs - this is the number of stations within SIZs

## Remove duplicates based on cols "point.ID" and "area_numbe". Duplicates occur where sites have more than once licence holder.
e2=e[!duplicated(e[c("point.ID","area_numbe")]),]
dim(e2)

## Now need to merge dfs for monitoring data and list of stations by SIZ.
# First need to have a common column to do merge. Add col 'point.ID' to df 'mon_nonpiz2'
mon_nonpiz2$point.ID <- seq.int(nrow(mon_nonpiz2)) # first add a col for point.ID
names(mon_nonpiz2)
#mon_nonpiz3=mon_nonpiz2[,c(1:11,30)]
#View(mon_nonpiz2)

# Now do the merge
siz_stm=merge(e2,mon_nonpiz2, by="point.ID")
#View(siz_stm)
dim(siz_stm)

## Add in unique identifier based on Code and area_numb.x
siz_stm$ID=paste(siz_stm$Code,siz_stm$area_numbe.x)


#### SIZ BASELINE DATA ####
## Plot monitoring data points on SIZ polygons
plot(siz, axes = TRUE)
points(pts_b,col = "red", cex = 0.4,pch=20)

## Start with basdat3 - these are the baseline samples with piz affiliation. You can therefore remove samples from within PIZ
basdat3_SIZ=basdat3[is.na(basdat3$area_numbe),]#baseline samples minus those in PIZ
#View(basdat3_SIZ)

## Drop the cols from basdat3_SIZ that were associated with the PIZ file 
basdat3_SIZ2=basdat3_SIZ[,1:11]
#View(basdat3_SIZ2)

## Monitoring sample positions (non PIZ stations)
pts_bsiz=SpatialPoints(basdat3_SIZ2[,(10:9)], 
                       proj4string = CRS(proj4string(siz)))

plot(siz)
points(pts_bsiz,col = "red", cex = 0.01,pch=20)

## Get SIZ affiliations for baseline sample locations
bas_nonpiz=over(pts_bsiz,siz)
#View(bas_nonpiz)

## Make a df for non PIZ baseline samples with SIZ affiliation
bas_nonpiz1=cbind(basdat3_SIZ2,bas_nonpiz)
#View(bas_nonpiz1)
dim(bas_nonpiz1)#550 20

## Remove any baseline samples not within a SIZ
bas_nonpiz2=bas_nonpiz1[!is.na(bas_nonpiz1$area_numbe),]
dim(bas_nonpiz2)# 206 20
#View(bas_nonpiz2)

## Plot points to make sure all are within SIZ extents
pts_bsiz2=SpatialPoints(bas_nonpiz2[,(10:9)], 
                        proj4string = CRS(proj4string(siz)))
plot(siz, axes = TRUE)
points(pts_bsiz2,col = "green", cex = 0.01,pch=20)

## Produce df for baseline stations by SIZ. Note that intersecting samples will appear more than once.
eb <- extract(siz, pts_bsiz2)
## Note there are 259 point IDs - this is the number of stations within SIZs )good)
#View(eb)

## Remove duplicates based on cols "point.ID" and "area_numbe"
eb2=eb[!duplicated(eb[c("point.ID","area_numbe")]),]
dim(eb2)

## Now need to merge dfs for baseline data and list of stations by SIZ.
# First need to have a common column to do merge. Add col 'point.ID' to df 'bas_nonpiz2'
bas_nonpiz2$point.ID <- seq.int(nrow(bas_nonpiz2)) # first add a col for point.ID
names(bas_nonpiz2)
#bas_nonpiz3=bas_nonpiz2[,c(1:11,30)]
#View(bas_nonpiz2)

# Now do the merge
siz_stb=merge(eb2,bas_nonpiz2, by="point.ID")
#View(siz_stb)
dim(siz_stb)

## Add in unique identifier based on Code and area_numb.x
siz_stb$ID=paste(siz_stb$Code,siz_stb$area_numbe.x)


#### SIZ MATCHED MONITORING AND BASELINE DATA ####
## Select baseline samples where there is an accompanying monitoring sample
siz_st_b=siz_stb %>%
  filter(ID %in% siz_stm$ID)
dim(siz_st_b)# 204 20
#View(siz_st_b)


## Make sure there are no stations in the monitoring set without a corresponding baseline station
siz_st_m=siz_stm %>%
  filter(ID %in% siz_st_b$ID)
siz_st_m
dim(siz_st_m)
#View(siz_st_m)

## Now join the baseline df to the monitoring df
data_SIZ=rbind(siz_st_b,siz_st_m)
#View(data_SIZ)

## Add column for PIZ
data_SIZ$Treatment="SIZ"

## Just take the required columns from df data (Code, seds, time, area_numbe,Treatment)
names(data_SIZ)
str(data_SIZ)
#View(data_SIZ)
data_SIZ2=data_SIZ[,c(21,28:22,31,8,51)]
#View(data_SIZ2)
names(data_SIZ2)

## Create a col for Licence no. and PIZ
data_SIZ2$site <- paste(data_SIZ2$area_numbe.x,data_SIZ2$Treatment)
names(data_SIZ2)

## Drop cols Treatment and Area_Numbe from df data 2
data_SIZ3=data_SIZ2[,c(1,8:2,9,12)]
#View(data_SIZ3)

## Check and update col types
str(data_SIZ3)
data_SIZ3$site=as.factor(data_SIZ3$site)
data_SIZ3$time=as.factor(data_SIZ3$time)
#View(data_SIZ3)
summary(data_SIZ3$time)


#### SIZ SED SUMMARY BASELINE/MONITORING ####
## Get summary data by area
#library(plyr)
detach("package:plyr", unload=TRUE) 
#library(dplyr)
sumdata_SIZ=data_SIZ3%>%
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
sumdata_SIZ
#View(sumdata_SIZ)

#### SIZ WILCOX TESTS ####

## Take relevant columns
data5=data_SIZ3[,c(10:9,2:8)]
data5

## Create a vector for site
siteSIZ = as.character(data5$site)
str(data5)

## Identify the number of sites for each site
table(siteSIZ)

## Vector for site names
#site.names = c("127 SIZ",   "137 SIZ",   "340 SIZ",   "351 SIZ", "372/1 SIZ", "395/1 SIZ", "395/2 SIZ", "396/1 SIZ", "396/2 SIZ", "435/1 SIZ", "435/2 SIZ",   "451 SIZ",   "453 SIZ",   "460 SIZ",   "488 SIZ", "500/3 SIZ")
site.namesSIZ =levels(data5$site)
  
## Number of sites
nsitesSIZ = length(site.namesSIZ)# 16

## Matrix for p-values
pmatrixSIZ = matrix(999, ncol=7, nrow=nsitesSIZ)

## You just do a loop over the N sites. Select out the rows for the jth site (j=1 to N) on each iteration of the loop. And then do the Wilcoxon tests for that site
for (j in 1:nsitesSIZ) {
  use = data5[site==site.namesSIZ[j],]
  for (k in 3:9) {
    varb = use[,k][use$time=="b"]
    varm = use[,k][use$time=="m"]
    pmatrixSIZ[j,(k-2)] = wilcox.test(varb ,varm, alt="two.sided",paired=T)$p.value
  }
}

round(pmatrixSIZ,3)

## Change pmatrix from a matrix to a dataframe
pmatrixSIZ=as.data.frame(pmatrixSIZ)
pmatrixSIZ

## Add in column names
colnames(pmatrixSIZ)=c("cG","mG","fG","cS","mS","fS","SC")
pmatrixSIZ

## Add in row names (site)
pmatrixSIZ$site=site.namesSIZ
pmatrixSIZ

## Change order of df
pmatrixSIZ2=pmatrixSIZ[,c(8,1:7)]
pmatrixSIZ2

## Round p-values to 3 dp
pmatrixSIZ2$cG=round(pmatrixSIZ2$cG,3)
pmatrixSIZ2$mG=round(pmatrixSIZ2$mG,3)
pmatrixSIZ2$fG=round(pmatrixSIZ2$fG,3)
pmatrixSIZ2$cS=round(pmatrixSIZ2$cS,3)
pmatrixSIZ2$mS=round(pmatrixSIZ2$mS,3)
pmatrixSIZ2$fS=round(pmatrixSIZ2$fS,3)
pmatrixSIZ2$SC=round(pmatrixSIZ2$SC,3)
pmatrixSIZ2

#### SIZ CBIND SUMMARY AND WILCOX TEST ####

## Split piz summary data into baseline and monitoring subsets
sumdata_SIZ_b=sumdata[which(sumdata_SIZ$time=="b"),]
sumdata_SIZ_m=sumdata[which(sumdata_SIZ$time=="m"),]

## Check same number of columns
dim(sumdata_SIZ_b)#16 10
dim(sumdata_SIZ_m)# 16 10
dim(pmatrixSIZ2)# 16 8


## Make sure all objects are class df
sumdata_SIZ_b=as.data.frame(sumdata_SIZ_b)#16 10
sumdata_SIZ_m=as.data.frame(sumdata_SIZ_m)# 16 10
class(sumdata_SIZ_b)#16 10
class(sumdata_SIZ_m)
class(pmatrixSIZ2)

## Stitch together the results of Wilcox tests (df pmatrix2) with baseline and monitoring sed percentages
siz_pvalues_means=cbind(pmatrixSIZ2,sumdata_SIZ_b,sumdata_SIZ_m)

## Change col names
names(siz_pvalues_means)
colnames(siz_pvalues_means)=c("site","cG_p","mG_p","fG_p","cS_p","mS_p","fS_p","SC_p","site","time","count","sc_b","fS_b","mS_b","cS_b","fG_b","mG_b","cG_b","site","time",  "count","sc_m","fS_m","mS_m","cS_m","fG_m","mG_m","cG_m")

#View(piz_pvalues_means)
names(siz_pvalues_means)

## Get cols in sensible order
siz_pvalues_means2=siz_pvalues_means[,c(1,11,8:2,12,22,13,23,14,24,15,25,16,26,17,27,18,28)]
View(siz_pvalues_means2)

#### JOIN PIZ AND SIZ RESULTS
options(scipen=999)# stop sci notation
results=rbind(piz_pvalues_means2,siz_pvalues_means2)
View(results)
names(results)

## Round p-values to 3 dp
results$SC_p=round(results$SC_p,3)
results$fS_p=round(results$fS_p,3)
results$mS_p=round(results$mS_p,3)
results$cS_p=round(results$cS_p,3)
results$fG_p=round(results$fG_p,3)
results$mG_p=round(results$mG_p,3)
results$cG_p=round(results$cG_p,3)
results$SC_b=round(results$SC_b,3)
results$fS_b=round(results$fS_b,3)
results$mS_b=round(results$mS_b,3)
results$cS_b=round(results$cS_b,3)
results$fG_b=round(results$fG_b,3)
results$mG_b=round(results$mG_b,3)
results$cG_b=round(results$cG_b,3)
results$SC_m=round(results$SC_m,3)
results$fS_m=round(results$fS_m,3)
results$mS_m=round(results$mS_m,3)
results$cS_m=round(results$cS_m,3)
results$fG_m=round(results$fG_m,3)
results$mG_m=round(results$mG_m,3)
results$cG_m=round(results$cG_m,3)
View(results)
names(results)

## get p-values next to baseline and monitoring data for each sed fraction
results2=results[,c(1,2,3,10,11,4,12,13,5,14,15,6,16,17,7,18,19,8,20,21,9,22,23)]
View(results2)


####  CONTEXT DATA ####
## Plot monitoring data points on PIZ polygons
plot(piz, axes = TRUE)
points(pts_m,col = "blue", cex = 0.4,pch=20)

## ID samples within PIZ
con_piz=over(pts_m,piz)
View(con_piz)

con_siz==over(pts_m,siz)

con_piz_siz=cbind(as.data.frame(con_piz$area_numbe,con_siz$area_numbe))

#####################################################################################
tb <- table(e[,1])
i <- as.integer(names(tb[tb>1]))
points(pts_msiz2[i,], pch=20, col='purple')

e




##############################


## https://gis.stackexchange.com/questions/201231/point-in-overlapping-polygons-using-r

library(raster)
n <- 100
set.seed(0)
x <- runif(n) * 360 - 180
y <- runif(n) * 180 - 90
xy <- cbind(x, y)
p1 <- rbind(c(-180,-20), c(-140,55), c(10, 0), c(-140,-60), c(-180,-20))
hole <- rbind(c(-150,-20), c(-100,-10), c(-110,20), c(-150,-20))
p1 <- list(p1, hole)
p2 <- rbind(c(-10,0), c(140,60), c(160,0), c(140,-55), c(-10,0))
p3 <- rbind(c(-125,0), c(0,60), c(40,5), c(15,-45), c(-125,0))
pols <- spPolygons(p1, p2, p3)

plot(pols, col=rainbow(3, alpha=.5),axes = TRUE)
points(xy)

e <- extract(pols, xy)

e



tb <- table(e[,1])
i <- as.integer(names(tb[tb>1]))
points(xy[i,], pch=20, col='red')

e
##############################################################################################
## Plot SIZs
plot(siz,axes = TRUE)

## Overlay the monitoring stations
points(pts_m,col = "blue", cex = 0.4,pch=20)

e <- extract(siz, pts_m)      
