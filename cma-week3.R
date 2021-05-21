### week 3-cma###

library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times





## Import the downloaded csv ##################################################

caro <- read_delim("caro60.csv",",") # adjust path
caro
# caro <- st_as_sf(caro, coords = c("E", "N"), crs = 2056, remove = FALSE)



### Tasks and Inpust###



# Task 1: Segmentation
# a)Specify a temporal windows v for in which to measure Euclidean distances
#    The sampling interval for this dataset is 1 minute. Use a temporal window  
#   v of 6 minutes, i.e. a window size of 6 positions (n±3).




# Measure the (euclidean) distance from every point to every other point within this temporal window (v)
caro <- caro %>%
  mutate(
    nMinus3 = sqrt((lag(E,3)-E)^2+(lag(E,3)-E)^2),     # distanze to pos -3
    nMinus2 = sqrt((lag(E,2)-E)^2+(lag(E,2)-E)^2),    # distance to pos -2 minutes
    nMinus1 = sqrt((lag(E,1)-E)^2+(lag(N,1)-N)^2),   # distance to pos -1 minutes
    nPlus1  = sqrt((E-lead(E,1))^2+(N-lead(N,1))^2), # distance to pos +1 mintues
    nPlus2  = sqrt((E-lead(E,2))^2+(N-lead(N,2))^2),   # distance to pos +2 minutes
    nPlus3  = sqrt((E-lead(E,3))^2+(N-lead(N,3))^2))   # distance to post + 3
caro



# calcualte mean distance for each row (nMinus3,nMinus2,nMinus1, nPlus1,nPlus2, nPlus3)

caro <- caro %>%
  rowwise() %>%
  mutate(
    stepMean = mean(c(nMinus3,nMinus2, nMinus1,nPlus1,nPlus2,nPlus3))
  ) %>%
  ungroup() 
caro




# Task 2: specify and appley threshold d
#
#
# explore the values to specify threshold
summary(caro)

# stepMean ist 6.9715   -> nehmen wir als Grenze



#  Remove “static points”: These are points where the average distance is less than a given threshold. 
#  his segments the trajectory into subtrajectories
#  here we use the mena value as a threshold.


caro <- caro %>% 
  ungroup() %>%
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))








# Task 3: Visualize segmented trajectories


ggplot(data=caro, aes(x=E, y= N)) +
  geom_path() +
  geom_point(aes(colour=static)) +
  coord_fixed() +
  theme(legend.position = "right")








### Task 4: Segment-based analysis



# create a unique ID for each segment that we can us a a grouping variable
rle_id <- function(vec){
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times=x))
}

# assigne unique IDs to subtrajectories
caro <- caro %>%
  mutate(segment_ID = rle_id(static))
caro



# Visualize moving segments  with segment_ID as grouping variable to determine the segmetns duration.

caro_segment <- caro %>%
  filter(!static)

ggplot(data=caro_segment, aes(x=E, y= N)) +
  geom_path(aes(color=segment_ID), show.legend= F) +
  geom_point(aes(colour=segment_ID), show.legend = T) +
  coord_fixed() +
  theme_light()+
  labs(title="all segments (uncleaned")
# hier sind noch die static drin, inkl static points



ggplot(data=caro_segment, aes(x=E, y= N)) +
  geom_path(aes(color=segment_ID), show.legend= F) +
  geom_point(aes(colour=segment_ID), show.legend = FALSE) +
  coord_fixed() +
  theme_light()+
  labs(title="all segments (uncleaned")



# # remove segments which are shorter than 5 minutes (segments < 5)
# use segment_ID as grouping variable

table(caro_segment$segment_ID)

caro_segment <- caro_segment %>%
  group_by(segment_ID) %>%
  mutate(duration = as.integer(difftime(max(DatetimeUTC),min(DatetimeUTC),"mins"))) %>%
  filter(duration >5)

ggplot(data=caro_segment, aes(x=E, y= N)) +
  geom_path(aes(color=segment_ID)) +
  geom_point(aes(colour=segment_ID), show.legend = F) +
  coord_fixed() +
  theme_light()+
  labs(title="long segments (removed segments <5)")







### Task 5: Similarity measures

pedestrian <- read_delim("pedestrian.csv", ",")
is.data.frame(pedestrian)

ggplot(data=pedestrian, aes(E,N))+
  geom_point(data=dplyr::select(pedestrian,-TrajID),alpha=0.1)+      # hier wird das gesamte Datenset angezeigt (in der Visualisaton in grau)
  geom_path(aes(E,N,color=as.factor(TrajID)))+
  geom_point(aes(E,N,color=as.factor(TrajID)))+
  facet_wrap(~TrajID)+
  labs(title="Visual comparison of the 6 trajectories", 
       subtitle= "Each subplot higlights a trajectory", header=T)





## Task 6: Calculate similarity

install.packages("SimilarityMeasures")
library(SimilarityMeasures)
help(package = "SimilarityMeasures")


# similarityMeasures works not with data.frame but with matrix

table(pedestrian)
# vorbereiten der Daten
# unterteilen nach ID
# wir brauchen nur die Koordinaten
# und wir brauchen sie als MAtrix

pede<- pedestrian

pede1<- pede%>%
  filter(TrajID==1)%>%      # unterteilen nach ID
  dplyr::select(E,N)%>%     # nur Koordinaten
  as.matrix()               # als Matrix

pede2<- pede%>% 
  filter(TrajID ==2)%>%
  dplyr::select(E,N) %>%
  as.matrix()

pede3<- pede%>% 
  filter(TrajID ==3)%>%
  dplyr::select(E,N) %>%
  as.matrix()

pede4<- pede%>% 
  subset(TrajID ==4)%>%
  dplyr::select(E,N) %>%
  as.matrix()

pede5<- pede%>% 
  filter(TrajID ==5)%>%
  dplyr::select(E,N) %>%
  as.matrix()

pede6<- pede%>% 
  filter(TrajID ==6)%>%
  dplyr::select(E,N) %>%
  as.matrix()

is.matrix(pede6)

# geht sicher auch eleganter



# comparing trajectories 

# DTW
DTW1_1<-DTW(pede1,pede1) # 0
DTW1_2<-DTW(pede1,pede2, pointSpacing= -1)  #  3650.025
DTW1_3<-DTW(pede1,pede3, pointSpacing= -1)  # 50785.510
DTW1_4<-DTW(pede1,pede4, pointSpacing= -1)  #  5906.787
DTW1_5<-DTW(pede1,pede5,pointSpacing= -1)  #  2178.411
DTW1_6<-DTW(pede1,pede6, pointSpacing= -1)  #  1152.718




# EditDist
EditDist1_1<-EditDist(pede1,pede1, pointDistance=20)  # 0
EditDist1_2<-EditDist(pede1,pede2, pointDistance=20)  # 45
EditDist1_3<-EditDist(pede1,pede3, pointDistance=20)  # 47
EditDist1_4<-EditDist(pede1,pede4, pointDistance=20)  # 42
EditDist1_5<-EditDist(pede1,pede5, pointDistance=20)  # 28
EditDist1_6<-EditDist(pede1,pede6, pointDistance=20)  # 27


#Frechet
Frechet1_1<-Frechet(pede1,pede1,testLeash=-1)  # 0
Frechet1_2<-Frechet(pede1,pede2,testLeash=-1)  # 28.5
Frechet1_3<-Frechet(pede1,pede3,testLeash=-1)  # 2308
Frechet1_4<-Frechet(pede1,pede4,testLeash=-1)  # 1069
Frechet1_5<-Frechet(pede1,pede5,testLeash=-1)  # 718
Frechet1_6<-Frechet(pede1,pede6,testLeash=-1)  # 39


# LCSS
LCSS1_1<-LCSS(pede1,pede1, pointSpacing= 10, pointDistance= 5, 
              errorMarg= 0.5)             # 47
LCSS1_2<-LCSS(pede1,pede2, 10,5,0.5)            # 3
LCSS1_3<-LCSS(pede1,pede3, 10,5,0.5)            # 1
LCSS1_4<-LCSS(pede1,pede4, 10,5,0.5)            # 6
LCSS1_5<-LCSS(pede1,pede5, 10,5,0.5)            # 11
LCSS1_6<-LCSS(pede1,pede6, 10,5,0.5)            # 11


# rbind

dtw <- c(DTW1_1,DTW1_2,DTW1_3,DTW1_4,DTW1_5,DTW1_6)
edt <- c(EditDist1_1,EditDist1_2,EditDist1_3,EditDist1_4,EditDist1_5,EditDist1_6)
fre <- c(Frechet1_1,Frechet1_2,Frechet1_3,Frechet1_4,Frechet1_5,Frechet1_6)
lcss<- c(LCSS1_1,LCSS1_2,LCSS1_3,LCSS1_4,LCSS1_5,LCSS1_6)


# combine to a data frame
sim.mea <- data.frame(dtw,edt,fre,lcss)
sim.mea$traj <- c(1,2,3,4,5,6)            # add column 

View(sim.mea)



# Visualisation
p_dtw <- ggplot(sim.mea, aes(traj, dtw, fill=traj), axis.title.x=element_blank ()) +
  geom_bar(stat= "identity", show.legend = F )+
  theme(legend.position = "none", show.legend= F)+
  theme_light()+
  labs(title="Distance Time Wraping",  x ="", y = "")
p_dtw


p_edt <-ggplot(sim.mea, aes(traj, edt, fill=traj), axis.title.x=element_blank ()) +
  geom_bar(stat= "identity", show.legend = F )+
  theme(legend.position = "none", show.legend= F)+
  theme_light()+
  labs(title="Edit Distance",  x ="", y = "")
p_edt

p_fre <- ggplot(sim.mea, aes(traj, fre, fill=traj), axis.title.x=element_blank ()) +
  geom_bar(stat= "identity", show.legend = F )+
  theme(legend.position = "none", show.legend= F)+
  theme_light()+
  labs(title="Frechet Calculation", x ="", y = "")
p_fre

p_lcss <- ggplot(sim.mea, aes(traj, lcss, fill=traj)) +
  geom_bar(stat= "identity", show.legend = F )+
  theme(legend.position = "none", show.legend= F)+
  theme_light()+
  labs(title="Longest Common Subsequence",  x ="", y = "")
p_lcss


# put together with cowplot

library(cowplot)
grid <- plot_grid(p_dtw,p_edt, p_fre, p_lcss)

title <- ggdraw() + draw_label("Computed similarities using differnet measures \n between trajectory 1 to all other trajectories", fontface='bold')
plot_grid(title, grid, ncol = 1,rel_heights = c(0.1, 1))
