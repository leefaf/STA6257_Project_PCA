#library(AER); data(CASchools); View(CASchools)
#library(MASS); data(bacteria); View(bacteria)


library(dplyr)
library(ggplot2)
library(data.table)


# READ & VIEW DATA
conspiracy<- (read.csv("https://raw.githubusercontent.com/bjcarr08/sampleData/main/kaggleConspiracyTheoriesData.csv", stringsAsFactors = T))[,-1]
View(conspiracy)
DescTools::Desc(conspiracy)[[1]]$abstract


# REMOVE ROWS WITH NAs
conspiracy<- conspiracy[complete.cases(conspiracy),]


# STANDARDIZED VARIABLES
conspiracy.Stdz<- conspiracy %>% mutate(across(.cols=truther911:vaportrail, scale))


# LONG DATA
library(tidyr)
conspiracy.Long<- conspiracy %>% pivot_longer(!y, names_to="conspiracy", values_to="score", values_transform=list(score=as.numeric))
conspiracy.Stdz.Long<- conspiracy.Stdz %>% pivot_longer(!y, names_to="conspiracy", values_to="stdzScore", values_transform=list(stdzScore=as.numeric))


# FREQUENCY HISTOGRAMS
ggplot(conspiracy.Long, aes(score, fill=conspiracy, color=conspiracy)) +
  geom_histogram(alpha=0.2, breaks=seq(0,5,1)) +
  lemon::facet_rep_wrap(.~conspiracy, nrow=4, labeller="label_both", repeat.tick.labels=T) +
  labs(title="Raw Scores") +
  theme_bw() +
  theme(legend.position="none")

ggplot(conspiracy.Stdz.Long, aes(stdzScore, fill=conspiracy, color=conspiracy)) +
  geom_histogram(alpha=0.2, breaks=seq(-2,3,1)) +
  lemon::facet_rep_wrap(.~conspiracy, nrow=4, labeller="label_both", repeat.tick.labels=T) +
  labs(title="Standardized Scores") +
  theme_bw() +
  theme(legend.position="none")



#summary(pc.cr <- princomp(conspiracy.Stdz[,-9])) # 1st 2 PCs account for 63% of variance
#loadings(pc.cr)  # blank cells are small, not zero
#biplot(pc.cr, pc.biplot=T, cex=0.6, xpd=T, main="Biplot")
#screeplot(pc.cr, type="lines", col="blueviolet", main="Screeplot")



# PCA
conspiracy<- conspiracy %>% filter(y!="Not Sure") # removed rows where participant marked 'not sure' as political party

yLabs<- c("Very Liberal", "Liberal", "Somewhat Liberal", "Middle of the Road", "Somewhat Conservative", "Conservative", "Very Conservative")

conspiracy$y<- factor(conspiracy$y, ordered=T, levels=yLabs)

#conspiracy %>% count(y)

pca<- princomp(conspiracy[,-9])
PC1<- pca$scores[,1]
PC2<- pca$scores[,2]
PCV1<- pca$loadings[,1]
PCV2<- pca$loadings[,2]
politicalView<- conspiracy[,9]
PCVlabs<- row.names(pca$loadings)

require(grid)

ggplot() +
  geom_point(aes(x=PC1, y=PC2, fill=politicalView), colour='black', pch=21, size=2.5, alpha=0.3) + 
  scale_fill_manual(values=c("navy", "blue", "lightblue1", "white", "indianred1", "red", "red4")) +   
  coord_fixed(ratio=1) +
  geom_segment(aes(x=0, y=0, xend=PCV1*12, yend=PCV2*10), arrow=arrow(length=unit(1/2, 'picas')), color="black") + 
  geom_text(aes(x=PCV1*14, y=PCV2*10), label=PCVlabs, size=4) 


summary(pca)
pca$loadings



# UNUSED CODE
# RE-CODE y AS INT
# conspiracy.Int<- conspiracy %>% 
#                    filter(!(y %like% "Other|Sure")) %>% 
#                    mutate(y = ifelse(y=="Very Liberal", 0, 
#                                      ifelse(y=="Liberal", 1,
#                                             ifelse(y=="Somewhat Liberal", 2, 
#                                                    ifelse(y=="Middle of the Road", 3, 
#                                                           ifelse(y=="Somewhat Conservative", 4, 
#                                                                  ifelse(y=="Conservative", 5, 6)))))))
