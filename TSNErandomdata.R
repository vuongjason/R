
library(Rtsne)
library(ggplot2)
library(dplyr)

df<-read.csv("/Users/jasonvuong/Dropbox/COG/orientation_random.csv") 
#100 31

df<-filter(df, mRS90!="NA") %>% select(-ends_with("fsl"),
                                       -starts_with("NIHSS"),
                                       -starts_with("volume_cat"),
                                       -starts_with("disc"),
                                       -starts_with("los"),
                                       -starts_with("TOAST"),
                                       -starts_with("mRS7")) %>% 
  mutate(Disability=mRS90>2)

df_final<-na.omit(df)

#perplexity is related to importance of neighbours
#or number of nearest neighbour k involved in many manifold learners
#df8<-df[,c(2:8)]

#tSNE of FLAIR final infarct data
aniT<-Rtsne(as.matrix(df_final[,c(2:7,10:17)]),dims = 2, perplexity = 10)
tsne_plot <- data.frame(x = aniT$Y[,1], y = aniT$Y[,2])
tsne_plot<-cbind(tsne_plot,df_final)
p<-ggplot(tsne_plot) + 
  geom_point(aes(x=x, y=y, color=as.factor(Disability),shape=as.factor(region)))+
  labs(shape="Arterial territory",color="Disability")
p

#tSNE of random-generated COG and orientation
aniT2<-Rtsne(as.matrix(df_final[,c(19:24)]),dims = 2, perplexity = 10)
tsne_plot2 <-data.frame(x = aniT2$Y[,1], y = aniT2$Y[,2])
tsne_plot2<-cbind(tsne_plot2,df_final)
p2<-ggplot(tsne_plot2) + 
  geom_point(aes(x=x, y=y, color=as.factor(Disability),shape=as.factor(region)))+
  labs(shape="Arterial territory",color="Disability")
p2
