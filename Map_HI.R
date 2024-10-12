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
        plot.title = element_text(size = 20))+ ggtitle("Division-wise Health insurance (HI) coverage (%)") + labs(fill = "Dengue Deaths \n(log10)") +
  theme_void()


library(gridExtra)
tiff("HIMap.tiff", units="in", width=6, height=6, res=300)
gridExtra::grid.arrange(x, nrow=1, ncol=1)
dev.off()




