#######################Division-wise Bangladesh Map of EBF##########################
#                            Mohammad Nayeem Hasan                                 #
####################################################################################

require(foreign)
require(MASS)
require(pROC)
require(survey)
require(ResourceSelection)
require(ROCR)
require(car)
require(ggplot2)
require(maptools)
library(nnet)
library(FSA)
library(caret)
require(mapproj)
require(rgdal)
require(car)
library(MASS)
require(foreign)
require(ggplot2)
require(maptools)
library(tidyverse)
library(betareg)
library(car)
library(gapminder)
library(dplyr)
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(psych) # for descriptive analysis
library(forecast)
library(lmtest)
library(tseries)
library(forecast)
library(MASS)
library(tseries)
library(forecast)
library(lubridate)
library(ggplot2)
library(zoo)
library(Rcpp)
library(prophet)
library(data.table)
library(dplyr)
library(psych)
require(MASS) # to access Animals data sets
require(scales) # to access break formatting functions
library(mgcv)
library(GGally)
library(mgcv)
library(visreg)
library(tidyverse)
library(ggrepel)
library(maptools)
library(RColorBrewer)
library(rgeos)
library(rgdal)
library(sp)
library(sf)
library(ggrepel)
library(ggplot2)
library(tidyverse)


setwd('E:\\ResearchProject\\Baker Bhai\\Health Insurance')

#graph2
q <- readShapeSpatial('E:\\ResearchProject\\Baker Bhai\\Health Insurance\\bgd_admbnda_adm1_bbs_20180410.shp')
q_1 <- fortify(q)


HI <- read.csv("E:\\ResearchProject\\Baker Bhai\\Health Insurance\\HI.csv")

q_1$prev <- ifelse(q_1$id==0,HI$HI[1],
                   ifelse(q_1$id==1,HI$HI[2],
                          ifelse(q_1$id==2,HI$HI[3],
                                 ifelse(q_1$id==3,HI$HI[4],
                                        ifelse(q_1$id==4,HI$HI[5],
                                               ifelse(q_1$id==5,HI$HI[6],
                                                      ifelse(q_1$id==6,HI$HI[7],HI$HI[8])))))))


centroids.df <- as.data.frame(coordinates(q))
names(centroids.df) <- c("Longitude", "Latitude")
centroids.df$name <- c('    Barisal',
                       'Chittagong\n',
                       'Dhaka','Khulna\n',
                       'Mymensingh\n',
                       'Rajshahi',
                       'Rangpur\n','Sylhet')


x <- ggplot(q_1, aes(x=long, y=lat)) +geom_polygon(aes(group=group,fill=prev),colour= "lightgrey")+coord_map()+
  geom_text(data=centroids.df,aes(label = name, x = Longitude, y = Latitude),color='black',size=5)+
  scale_fill_distiller(name='HI (%)',palette ="BuPu", direction=1)+
  theme(legend.text = element_text(size = 20),
        plot.title = element_text(size = 20))+ ggtitle("Division-wise Health insurance coverage") + labs(fill = "Dengue Deaths \n(log10)") +
  theme_void()

# Add scale and North arrow
library(ggspatial)
x2 <- x +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(3.5, "in"), pad_y = unit(0.5, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )

x3 <- x2 + annotate(geom="text", x=90.25, y=21, label="Map of Bangladesh",
                                  color="black", size=5)
x3







sldata <- read.csv("DistrictWise.csv", header = T)


shp <- readOGR(dsn = "bgd_adm_bbs_20201113_SHP", "bgd_admbnda_adm2_bbs_20201113")

head(shp@data)
shp@data$ADM2_EN

sldata <- sldata[order(sldata$ADM2_EN),]
sldata$ADM2_EN
joined_df <- merge(shp@data, sldata, by = "ADM2_EN")

q_1 <- fortify(shp, region = "ADM2_EN")


library(dplyr)

q_1 <- q_1 %>%
  mutate(prev = case_when(q_1$id=="Bagerhat" ~ sldata$Cases[1],q_1$id=="Bandarban" ~ sldata$Cases[2],q_1$id=="Barguna" ~ sldata$Cases[3],q_1$id=="Barisal" ~ sldata$Cases[4],
                          q_1$id=="Bhola" ~ sldata$Cases[5],q_1$id=="Bogra" ~ sldata$Cases[6],q_1$id=="Brahamanbaria" ~ sldata$Cases[7],q_1$id=="Chandpur" ~ sldata$Cases[8],
                          q_1$id=="Chittagong" ~ sldata$Cases[9],q_1$id=="Chuadanga" ~ sldata$Cases[10],q_1$id=="Comilla" ~ sldata$Cases[11],q_1$id=="Cox's Bazar" ~ sldata$Cases[12],
                          q_1$id=="Dhaka" ~ sldata$Cases[13],q_1$id=="Dinajpur" ~ sldata$Cases[14],q_1$id=="Faridpur" ~ sldata$Cases[15],q_1$id=="Feni" ~ sldata$Cases[16],
                          q_1$id=="Gaibandha" ~ sldata$Cases[17],q_1$id=="Gazipur" ~ sldata$Cases[18],q_1$id=="Gopalganj" ~ sldata$Cases[19],q_1$id=="Habiganj" ~ sldata$Cases[20],
                          q_1$id=="Jamalpur" ~ sldata$Cases[21],q_1$id=="Jessore" ~ sldata$Cases[22],q_1$id=="Jhalokati" ~ sldata$Cases[23],q_1$id=="Jhenaidah" ~ sldata$Cases[24],
                          q_1$id=="Joypurhat" ~ sldata$Cases[25],q_1$id=="Khagrachhari" ~ sldata$Cases[26],q_1$id=="Khulna" ~ sldata$Cases[27],q_1$id=="Kishoreganj" ~ sldata$Cases[28],
                          q_1$id=="Kurigram" ~ sldata$Cases[29],q_1$id=="Kushtia" ~ sldata$Cases[30],q_1$id=="Lakshmipur" ~ sldata$Cases[31],q_1$id=="Lalmonirhat" ~ sldata$Cases[32],
                          q_1$id=="Madaripur" ~ sldata$Cases[33],q_1$id=="Magura" ~ sldata$Cases[34],q_1$id=="Manikganj" ~ sldata$Cases[35],q_1$id=="Maulvibazar" ~ sldata$Cases[36],
                          q_1$id=="Meherpur" ~ sldata$Cases[37],q_1$id=="Munshiganj" ~ sldata$Cases[38],q_1$id=="Mymensingh" ~ sldata$Cases[39],q_1$id=="Naogaon" ~ sldata$Cases[40],
                          q_1$id=="Narail" ~ sldata$Cases[41],q_1$id=="Narayanganj" ~ sldata$Cases[42],q_1$id=="Narsingdi" ~ sldata$Cases[43],q_1$id=="Natore" ~ sldata$Cases[44],
                          q_1$id=="Nawabganj" ~ sldata$Cases[45],q_1$id=="Netrakona" ~ sldata$Cases[46],q_1$id=="Nilphamari" ~ sldata$Cases[47],q_1$id=="Noakhali" ~ sldata$Cases[48],
                          q_1$id=="Pabna" ~ sldata$Cases[49],q_1$id=="Panchagarh" ~ sldata$Cases[50],q_1$id=="Patuakhali" ~ sldata$Cases[51],q_1$id=="Pirojpur" ~ sldata$Cases[52],
                          q_1$id=="Rajbari" ~ sldata$Cases[53],q_1$id=="Rajshahi" ~ sldata$Cases[54],q_1$id=="Rangamati" ~ sldata$Cases[55],q_1$id=="Rangpur" ~ sldata$Cases[56],
                          q_1$id=="Satkhira" ~ sldata$Cases[57],q_1$id=="Shariatpur" ~ sldata$Cases[58],q_1$id=="Sherpur" ~ sldata$Cases[59],q_1$id=="Sirajganj" ~ sldata$Cases[60],
                          q_1$id=="Sunamganj" ~ sldata$Cases[61],q_1$id=="Sylhet" ~ sldata$Cases[62],q_1$id=="Tangail" ~ sldata$Cases[63],q_1$id=="Thakurgaon" ~ sldata$Cases[64],
  ))

centroids.df <- as.data.frame(coordinates(shp))
names(centroids.df) <- c("Longitude", "Latitude")
#centroids.df$name <- c("Bagerhat",      "Bandarban",     "Barguna",       "Barisal",       
#                       "Bhola",         "Bogra",         "Brahamanbaria",
#                       "Chandpur",      "Chittagong",    "Chuadanga",     "Comilla",       
#                       "Cox's Bazar",   "Dhaka",         "Dinajpur",     
#                       "Faridpur",      "Feni",          "Gaibandha",     "Gazipur",       
#                       "Gopalganj",     "Habiganj",      "Jamalpur",     
#                       "Jessore",       "Jhalokati",     "Jhenaidah",     "Joypurhat",     
#                       "Khagrachhari",  "Khulna",        "Kishoreganj",  
#                       "Kurigram",      "Kushtia",       "Lakshmipur",    "Lalmonirhat",   
#                       "Madaripur",     "Magura",        "Manikganj",    
#                       "Maulvibazar",   "Meherpur",      "Munshiganj",    "Mymensingh",    
#                       "Naogaon",       "Narail",        "Narayanganj",  
#                       "Narsingdi",     "Natore",        "Nawabganj",     "Netrakona",     
#                       "Nilphamari",    "Noakhali",      "Pabna",        
#                       "Panchagarh",    "Patuakhali",    "Pirojpur",      "Rajbari",       
#                       "Rajshahi",      "Rangamati",     "Rangpur",      
#                       "Satkhira",      "Shariatpur",    "Sherpur",       "Sirajganj",     
#                       "Sunamganj",     "Sylhet",        "Tangail",      
#                       "Thakurgaon")


centroids.df$name <- c("Bag",      "Ban",     "Bar",       "Bar",       
                       "Bho",         "Bog",         "Bra",
                       "Cha",      "Chi",    "Chu",     "Com",       
                       "Cox",   "Dha",         "Din",     
                       "Far",      "Fen",          "Gai",     "Gaz",       
                       "Gop",     "Hab",      "Jam",     
                       "Jes",       "Jha",     "Jhe",     "Joy",     
                       "Kha",  "Khu",        "Kis",  
                       "Kur",      "Kus",       "Lak",    "Lal",   
                       "Mad",     "Mag",        "Man",    
                       "Mau",   "Meh",      "Mun",    "Mym",    
                       "Nao",       "Nar",        "Nar",  
                       "Nar",     "Nat",        "Naw",     "Net",     
                       "Nil",    "Noa",      "Pab",        
                       "Pan",    "Pat",    "Pir",      "Raj",       
                       "Raj",      "Ran",     "Ran",      
                       "Sat",      "Sha",    "She",       "Sir",     
                       "Sun",     "Syl",        "Tan",      
                       "Tha")

Casesmap1 <- ggplot(data = q_1 , aes(x = long, y = lat)) + 
  geom_polygon(aes(group=group,fill=prev),colour= "lightgrey")+coord_map()+
  geom_text(data=centroids.df,aes(label = name, x = Longitude, y = Latitude),color='black',size=3)+
  scale_fill_distiller(name='HI (%)',palette ="BuPu", direction=1)+
  theme(legend.text = element_text(size = 20),
        plot.title = element_text(size = 20))+ ggtitle("District-wise Health insurance coverage") + labs(fill = "Dengue Deaths \n(log10)") +
  theme_void()

Casesmap1

# Add scale and North arrow
library(ggspatial)
Casesmap2 <- Casesmap1 +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(3.5, "in"), pad_y = unit(0.5, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )

Casesmap3 <- Casesmap2 + annotate(geom="text", x=90.25, y=21, label="Map of Bangladesh",
                        color="black", size=5)
Casesmap3

library(gridExtra)
tiff("Finalmap.tiff", units="in", width=12, height=6, res=300)
gridExtra::grid.arrange(x3, Casesmap3, nrow=1, ncol=2)
dev.off()

library(geosphere)
library(ape)
sldata


ozone.dists <- as.matrix(dist(cbind(sldata$Longitude, sldata$Latitude)))

ozone.dists.inv <- 1/ozone.dists
diag(ozone.dists.inv) <- 0

ozone.dists.inv[1:5, 1:5]


Moran.I(sldata$Cases, ozone.dists.inv)
