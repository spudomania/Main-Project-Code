#Permanova and plot for PH0000520

library(vegan)

P520 = data.frame(binary$PH0000520, over_50_cols)

P520_yes <- subset(P520, binary.PH0000520 == 1)

p520_no <- subset(P520, binary.PH0000520 == 0)

perma_520_df = read.csv("perma_520_df.csv")

perma_520_df = rbind(P520_yes,p520_no)

colnames(perma_520_df)

View(perma_520_df)

P520_matrix <- as.matrix(perma_520_df[,2:83])

P520.mat<-sqrt(P520_matrix)

P520.dist<-vegdist(P520.mat, method='bray')

set.seed(36)

colnames(perma_520_df)

P520.div<-adonis2(P520.dist~binary.PH0000520, data=perma_520_df, permutations = 999, method="bray", strata="PLOT")

View(P520.div)

dispersion_520<-betadisper(P520.dist, group=perma_520_df$binary.PH0000520)
permutest(dispersion_520)

plot(dispersion_520, hull=FALSE, ellipse=TRUE)

P_520_MDS<-metaMDS(P520.mat, distance="bray", k=2, trymax=35, autotransform=TRUE) 

stressplot(P_520_MDS)

NMDS1 <- P_520_MDS$points[,1] ##also found using scores(birdMDS)
NMDS2 <- P_520_MDS$points[,2]
P520.plot<-cbind(perma_520_df, NMDS1, NMDS2)

#PH000520 PERMANOVA PLOT

ggplot(P520.plot, aes(NMDS1, NMDS2, color=binary.PH0000520))+
  geom_point(position=position_jitter(.1), shape=3)+##separates overlapping points ##draws 95% confidence interval ellipses
  stat_ellipse(aes(fill=binary.PH0000520), alpha=.2,type='t',size =1, geom="polygon")+
  theme_minimal()

#Permanova and plot for PH0000522

P522 = data.frame(binary$PH0000522, over_50_cols)

P522_yes <- subset(P522, binary.PH0000522 == 1)

p522_no <- subset(P522, binary.PH0000522 == 0)

perma_522_df = rbind(P522_yes,p522_no)

perma_522_df$binary.PH0000522 <- as.character(perma_522_df$binary.PH0000522)

dim(perma_522_df)

P522_matrix <- as.matrix(perma_522_df[,2:83])

P522.mat<-sqrt(P522_matrix)

P522.dist<-vegdist(P522.mat, method='bray')

set.seed(36)

colnames(perma_522_df)

P522.div<-adonis2(P522.dist~binary.PH0000522, data=perma_522_df, permutations = 999, method="bray", strata="PLOT")

View(P522.div)

dispersion_522<-betadisper(P522.dist, group=perma_522_df$binary.PH0000522)
permutest(dispersion_522)

plot(dispersion_522, hull=FALSE, ellipse=TRUE)

P_522_MDS<-metaMDS(P522.mat, distance="bray", k=2, trymax=35, autotransform=TRUE) 

stressplot(P_522_MDS)

NMDS1 <- P_522_MDS$points[,1] ##also found using scores(birdMDS)
NMDS2 <- P_522_MDS$points[,2]
P522.plot<-cbind(perma_522_df, NMDS1, NMDS2)

#PH000522 PERMANOVA PLOT

ggplot(P522.plot, aes(NMDS1, NMDS2, color=binary.PH0000522))+
  geom_point(position=position_jitter(.1), shape=3)+##separates overlapping points ##draws 95% confidence interval ellipses
  stat_ellipse(aes(fill=binary.PH0000522), alpha=.2,type='t',size =1, geom="polygon")+
  theme_minimal()

#for menopausal status

P786 = data.frame(non_binary$PH0000786, over_50_cols)

P786_pre <- subset(P786, non_binary.PH0000786 == 1)

p786_current <- subset(P786, non_binary.PH0000786 == 2)

p786_post <- subset(P786, non_binary.PH0000786 == 3)

perma_786_df = rbind(P786_pre,p786_current,p786_post)

perma_786_df$non_binary.PH0000786 <- as.character(perma_786_df$non_binary.PH0000786)

dim(perma_786_df)

P786_matrix <- as.matrix(perma_786_df[,2:83])

P786.mat<-sqrt(P786_matrix)

P786.dist<-vegdist(P786.mat, method='bray')

set.seed(36)

colnames(perma_786_df)

P786.div<-adonis2(P786.dist~non_binary.PH0000786, data=perma_786_df, permutations = 999, method="bray", strata="PLOT")

View(P786.div)

dispersion_786<-betadisper(P786.dist, group=perma_786_df$non_binary.PH0000786)
permutest(dispersion_786)

plot(dispersion_786, hull=FALSE, ellipse=TRUE)

P_786_MDS<-metaMDS(P786.mat, distance="bray", k=2, trymax=35, autotransform=TRUE) 

stressplot(P_786_MDS)

NMDS1 <- P_786_MDS$points[,1] ##also found using scores(birdMDS)
NMDS2 <- P_786_MDS$points[,2]
P786.plot<-cbind(perma_786_df, NMDS1, NMDS2)

#PH000786 PERMANOVA PLOT

ggplot(P786.plot, aes(NMDS1, NMDS2, color=non_binary.PH0000786))+
  geom_point(position=position_jitter(.1), shape=3)+##separates overlapping points ##draws 95% confidence interval ellipses
  stat_ellipse(aes(fill=non_binary.PH0000786), alpha=.2,type='t',size =1, geom="polygon")+
  theme_minimal()

#for PH0000788

require(vegan)

P788 = data.frame(binary$PH0000788, over_50_cols)

P788_yes <- subset(P788, binary.PH0000788 == 1)

p788_no <- subset(P788, binary.PH0000788 == 0)

perma_788_df = rbind(P788_yes,p788_no)

perma_788_df$binary.PH0000788 <- as.character(perma_788_df$binary.PH0000788)

dim(perma_788_df)

P788_matrix <- as.matrix(perma_788_df[,2:83])

P788.mat<-sqrt(P788_matrix)

P788.dist<-vegdist(P788.mat, method='bray')

set.seed(36)

colnames(perma_788_df)

P788.div<-adonis2(P788.dist~binary.PH0000788, data=perma_788_df, permutations = 999, method="bray", strata="PLOT")

View(P788.div)

dispersion_788<-betadisper(P788.dist, group=perma_788_df$binary.PH0000788)
permutest(dispersion_788)

plot(dispersion_788, hull=FALSE, ellipse=TRUE)

P_788_MDS<-metaMDS(P788.mat, distance="bray", k=2, trymax=35, autotransform=TRUE) 

stressplot(P_788_MDS)

NMDS1 <- P_788_MDS$points[,1] ##also found using scores(birdMDS)
NMDS2 <- P_788_MDS$points[,2]
P788.plot<-cbind(perma_788_df, NMDS1, NMDS2)

#PH000788 PERMANOVA PLOT

ggplot(P788.plot, aes(NMDS1, NMDS2, color=binary.PH0000788))+
  geom_point(position=position_jitter(.1), shape=3)+##separates overlapping points ##draws 95% confidence interval ellipses
  stat_ellipse(aes(fill=binary.PH0000788), alpha=.2,type='t',size =1, geom="polygon")+
  theme_minimal()

#for PH0000789


P789 = data.frame(binary$PH0000789, over_50_cols)

P789_yes <- subset(P789, binary.PH0000789 == 1)

p789_no <- subset(P789, binary.PH0000789 == 0)

perma_789_df = rbind(P789_yes,p789_no)

perma_789_df$binary.PH0000789 <- as.character(perma_789_df$binary.PH0000789)

dim(perma_789_df)

P789_matrix <- as.matrix(perma_789_df[,2:83])

P789.mat<-sqrt(P789_matrix)

P789.dist<-vegdist(P789.mat, method='bray')

set.seed(36)

colnames(perma_789_df)

P789.div<-adonis2(P789.dist~binary.PH0000789, data=perma_789_df, permutations = 999, method="bray", strata="PLOT")

View(P789.div)

dispersion_789<-betadisper(P789.dist, group=perma_789_df$binary.PH0000789)
permutest(dispersion_789)

plot(dispersion_789, hull=FALSE, ellipse=TRUE)

P_789_MDS<-metaMDS(P789.mat, distance="bray", k=2, trymax=35, autotransform=TRUE) 

stressplot(P_789_MDS)

NMDS1 <- P_789_MDS$points[,1] ##also found using scores(birdMDS)
NMDS2 <- P_789_MDS$points[,2]
P789.plot<-cbind(perma_789_df, NMDS1, NMDS2)

#PH000789 PERMANOVA PLOT

ggplot(P789.plot, aes(NMDS1, NMDS2, color=binary.PH0000789))+
  geom_point(position=position_jitter(.1), shape=3)+##separates overlapping points ##draws 95% confidence interval ellipses
  stat_ellipse(aes(fill=binary.PH0000789), alpha=.2,type='t',size =1, geom="polygon")+
  theme_minimal()

#for PH0000577

P577 = data.frame(binary$PH0000577, over_50_cols)

P577_yes <- subset(P577, binary.PH0000577 == 1)

p577_no <- subset(P577, binary.PH0000577 == 0)

perma_577_df = rbind(P577_yes,p577_no)

perma_577_df$binary.PH0000577 <- as.character(perma_577_df$binary.PH0000577)

dim(perma_577_df)

P577_matrix <- as.matrix(perma_577_df[,2:83])

P577.mat<-sqrt(P577_matrix)

P577.dist<-vegdist(P577.mat, method='bray')

set.seed(36)

colnames(perma_577_df)

P577.div<-adonis2(P577.dist~binary.PH0000577, data=perma_577_df, permutations = 999, method="bray", strata="PLOT")

View(P577.div)

dispersion_577<-betadisper(P577.dist, group=perma_577_df$binary.PH0000577)
permutest(dispersion_577)

plot(dispersion_577, hull=FALSE, ellipse=TRUE)

P_577_MDS<-metaMDS(P577.mat, distance="bray", k=2, trymax=35, autotransform=TRUE) 

stressplot(P_577_MDS)

NMDS1 <- P_577_MDS$points[,1] ##also found using scores(birdMDS)
NMDS2 <- P_577_MDS$points[,2]
P577.plot<-cbind(perma_577_df, NMDS1, NMDS2)

#PH000577 PERMANOVA PLOT

ggplot(P577.plot, aes(NMDS1, NMDS2, color=binary.PH0000577))+
  geom_point(position=position_jitter(.1), shape=3)+##separates overlapping points ##draws 95% confidence interval ellipses
  stat_ellipse(aes(fill=binary.PH0000577), alpha=.2,type='t',size =1, geom="polygon")+
  theme_minimal()

#for PH0000610


P610 = data.frame(binary$PH0000610, over_50_cols)

P610_yes <- subset(P610, binary.PH0000610 == 1)

p610_no <- subset(P610, binary.PH0000610 == 0)

perma_610_df = rbind(P610_yes,p610_no)

perma_610_df$binary.PH0000610 <- as.character(perma_610_df$binary.PH0000610)

dim(perma_610_df)

P610_matrix <- as.matrix(perma_610_df[,2:83])

P610.mat<-sqrt(P610_matrix)

P610.dist<-vegdist(P610.mat, method='bray')

set.seed(36)

colnames(perma_610_df)

P610.div<-adonis2(P610.dist~binary.PH0000610, data=perma_610_df, permutations = 999, method="bray", strata="PLOT")

View(P610.div)

dispersion_610<-betadisper(P610.dist, group=perma_610_df$binary.PH0000610)
permutest(dispersion_610)

plot(dispersion_610, hull=FALSE, ellipse=TRUE)

P_610_MDS<-metaMDS(P610.mat, distance="bray", k=2, trymax=35, autotransform=TRUE) 

stressplot(P_610_MDS)

NMDS1 <- P_610_MDS$points[,1] ##also found using scores(birdMDS)
NMDS2 <- P_610_MDS$points[,2]
P610.plot<-cbind(perma_610_df, NMDS1, NMDS2)

#PH000610 PERMANOVA PLOT

ggplot(P610.plot, aes(NMDS1, NMDS2, color=binary.PH0000610))+
  geom_point(position=position_jitter(.1), shape=3)+##separates overlapping points ##draws 95% confidence interval ellipses
  stat_ellipse(aes(fill=binary.PH0000610), alpha=.2,type='t',size =1, geom="polygon")+
  theme_minimal()

#for PH0000776


P776 = data.frame(binary$PH0000776, over_50_cols)

P776_yes <- subset(P776, binary.PH0000776 == 1)

p776_no <- subset(P776, binary.PH0000776 == 0)

perma_776_df = rbind(P776_yes,p776_no)

perma_776_df$binary.PH0000776 <- as.character(perma_776_df$binary.PH0000776)

dim(perma_776_df)

P776_matrix <- as.matrix(perma_776_df[,2:83])

P776.mat<-sqrt(P776_matrix)

P776.dist<-vegdist(P776.mat, method='bray')

set.seed(36)

colnames(perma_776_df)

P776.div<-adonis2(P776.dist~binary.PH0000776, data=perma_776_df, permutations = 999, method="bray", strata="PLOT")

View(P776.div)

dispersion_776<-betadisper(P776.dist, group=perma_776_df$binary.PH0000776)
permutest(dispersion_776)

plot(dispersion_776, hull=FALSE, ellipse=TRUE)

P_776_MDS<-metaMDS(P776.mat, distance="bray", k=2, trymax=35, autotransform=TRUE) 

stressplot(P_776_MDS)

NMDS1 <- P_776_MDS$points[,1] ##also found using scores(birdMDS)
NMDS2 <- P_776_MDS$points[,2]
P776.plot<-cbind(perma_776_df, NMDS1, NMDS2)

#PH000776 PERMANOVA PLOT

ggplot(P776.plot, aes(NMDS1, NMDS2, color=binary.PH0000776))+
  geom_point(position=position_jitter(.1), shape=3)+##separates overlapping points ##draws 95% confidence interval ellipses
  stat_ellipse(aes(fill=binary.PH0000776), alpha=.2,type='t',size =1, geom="polygon")+
  theme_minimal()

#for PH0000773


P773 = data.frame(binary$PH0000773, over_50_cols)

P773_yes <- subset(P773, binary.PH0000773 == 1)

p773_no <- subset(P773, binary.PH0000773 == 0)

perma_773_df = rbind(P773_yes,p773_no)

perma_773_df$binary.PH0000773 <- as.character(perma_773_df$binary.PH0000773)

dim(perma_773_df)

P773_matrix <- as.matrix(perma_773_df[,2:83])

P773.mat<-sqrt(P773_matrix)

P773.dist<-vegdist(P773.mat, method='bray')

set.seed(36)

colnames(perma_773_df)

P773.div<-adonis2(P773.dist~binary.PH0000773, data=perma_773_df, permutations = 999, method="bray", strata="PLOT")

View(P773.div)

dispersion_773<-betadisper(P773.dist, group=perma_773_df$binary.PH0000773)
permutest(dispersion_773)

plot(dispersion_773, hull=FALSE, ellipse=TRUE)

P_773_MDS<-metaMDS(P773.mat, distance="bray", k=2, trymax=35, autotransform=TRUE) 

stressplot(P_773_MDS)

NMDS1 <- P_773_MDS$points[,1] ##also found using scores(birdMDS)
NMDS2 <- P_773_MDS$points[,2]
P773.plot<-cbind(perma_773_df, NMDS1, NMDS2)

#PH000773 PERMANOVA PLOT

ggplot(P773.plot, aes(NMDS1, NMDS2, color=binary.PH0000773))+
  geom_point(position=position_jitter(.1), shape=3)+##separates overlapping points ##draws 95% confidence interval ellipses
  stat_ellipse(aes(fill=binary.PH0000773), alpha=.2,type='t',size =1, geom="polygon")+
  theme_minimal()


#for PH0000779


P779 = data.frame(binary$PH0000779, over_50_cols)

P779_yes <- subset(P779, binary.PH0000779 == 1)

p779_no <- subset(P779, binary.PH0000779 == 0)

perma_779_df = rbind(P779_yes,p779_no)

perma_779_df$binary.PH0000779 <- as.character(perma_779_df$binary.PH0000779)

dim(perma_779_df)

P779_matrix <- as.matrix(perma_779_df[,2:83])

P779.mat<-sqrt(P779_matrix)

P779.dist<-vegdist(P779.mat, method='bray')

set.seed(36)

colnames(perma_779_df)

P779.div<-adonis2(P779.dist~binary.PH0000779, data=perma_779_df, permutations = 999, method="bray", strata="PLOT")

View(P779.div)

dispersion_779<-betadisper(P779.dist, group=perma_779_df$binary.PH0000779)
permutest(dispersion_779)

plot(dispersion_779, hull=FALSE, ellipse=TRUE)

P_779_MDS<-metaMDS(P779.mat, distance="bray", k=2, trymax=35, autotransform=TRUE) 

stressplot(P_779_MDS)

NMDS1 <- P_779_MDS$points[,1] ##also found using scores(birdMDS)
NMDS2 <- P_779_MDS$points[,2]
P779.plot<-cbind(perma_779_df, NMDS1, NMDS2)

#PH000779 PERMANOVA PLOT

ggplot(P779.plot, aes(NMDS1, NMDS2, color=binary.PH0000779))+
  geom_point(position=position_jitter(.1), shape=3)+##separates overlapping points ##draws 95% confidence interval ellipses
  stat_ellipse(aes(fill=binary.PH0000779), alpha=.2,type='t',size =1, geom="polygon")+
  theme_minimal()

#FOR PH0000792 

P792 = data.frame(binary$PH0000792, over_50_cols)

P792_yes <- subset(P792, binary.PH0000792 == 1)

p792_no <- subset(P792, binary.PH0000792 == 0)

perma_792_df = rbind(P792_yes,p792_no)

perma_792_df$binary.PH0000792 <- as.character(perma_792_df$binary.PH0000792)

dim(perma_792_df)

P792_matrix <- as.matrix(perma_792_df[,2:83])

P792.mat<-sqrt(P792_matrix)

P792.dist<-vegdist(P792.mat, method='bray')

set.seed(36)

colnames(perma_792_df)

P792.div<-adonis2(P792.dist~binary.PH0000792, data=perma_792_df, permutations = 999, method="bray", strata="PLOT")

View(P792.div)

dispersion_792<-betadisper(P792.dist, group=perma_792_df$binary.PH0000792)
permutest(dispersion_792)

plot(dispersion_792, hull=FALSE, ellipse=TRUE)

P_792_MDS<-metaMDS(P792.mat, distance="bray", k=2, trymax=35, autotransform=TRUE) 

stressplot(P_792_MDS)

NMDS1 <- P_792_MDS$points[,1] ##also found using scores(birdMDS)
NMDS2 <- P_792_MDS$points[,2]
P792.plot<-cbind(perma_792_df, NMDS1, NMDS2)

#PH000792 PERMANOVA PLOT

ggplot(P792.plot, aes(NMDS1, NMDS2, color=binary.PH0000792))+
  geom_point(position=position_jitter(.1), shape=3)+##separates overlapping points ##draws 95% confidence interval ellipses
  stat_ellipse(aes(fill=binary.PH0000792), alpha=.2,type='t',size =1, geom="polygon")+
  theme_minimal()

#PH0000770

P770 = data.frame(binary$PH0000770, over_50_cols)

P770_yes <- subset(P770, binary.PH0000770 == 1)

p770_no <- subset(P770, binary.PH0000770 == 0)

perma_770_df = rbind(P770_yes,p770_no)

perma_770_df$binary.PH0000770 <- as.character(perma_770_df$binary.PH0000770)

dim(perma_770_df)

P770_matrix <- as.matrix(perma_770_df[,2:83])

P770.mat<-sqrt(P770_matrix)

P770.dist<-vegdist(P770.mat, method='bray')

set.seed(36)

colnames(perma_770_df)

P770.div<-adonis2(P770.dist~binary.PH0000770, data=perma_770_df, permutations = 999, method="bray", strata="PLOT")

View(P770.div)

dispersion_770<-betadisper(P770.dist, group=perma_770_df$binary.PH0000770)
permutest(dispersion_770)

plot(dispersion_770, hull=FALSE, ellipse=TRUE)

P_770_MDS<-metaMDS(P770.mat, distance="bray", k=2, trymax=35, autotransform=TRUE) 

stressplot(P_770_MDS)

NMDS1 <- P_770_MDS$points[,1] ##also found using scores(birdMDS)
NMDS2 <- P_770_MDS$points[,2]
P770.plot<-cbind(perma_770_df, NMDS1, NMDS2)

#PH000770 PERMANOVA PLOT

ggplot(P770.plot, aes(NMDS1, NMDS2, color=binary.PH0000770))+
  geom_point(position=position_jitter(.1), shape=3)+##separates overlapping points ##draws 95% confidence interval ellipses
  stat_ellipse(aes(fill=binary.PH0000770), alpha=.2,type='t',size =1, geom="polygon")+
  theme_minimal()

#For PH0000767 ectopic pregnancy 

P767 = data.frame(binary$PH0000767, over_50_cols)

P767_yes <- subset(P767, binary.PH0000767 == 1)

p767_no <- subset(P767, binary.PH0000767 == 0)

perma_767_df = rbind(P767_yes,p767_no)

perma_767_df$binary.PH0000767 <- as.character(perma_767_df$binary.PH0000767)

dim(perma_767_df)

P767_matrix <- as.matrix(perma_767_df[,2:83])

P767.mat<-sqrt(P767_matrix)

P767.dist<-vegdist(P767.mat, method='bray')

set.seed(36)

colnames(perma_767_df)

P767.div<-adonis2(P767.dist~binary.PH0000767, data=perma_767_df, permutations = 999, method="bray", strata="PLOT")

View(P767.div)

dispersion_767<-betadisper(P767.dist, group=perma_767_df$binary.PH0000767)
permutest(dispersion_767)

plot(dispersion_767, hull=FALSE, ellipse=TRUE)

P_767_MDS<-metaMDS(P767.mat, distance="bray", k=2, trymax=35, autotransform=TRUE) 

stressplot(P_767_MDS)

NMDS1 <- P_767_MDS$points[,1] ##also found using scores(birdMDS)
NMDS2 <- P_767_MDS$points[,2]
P767.plot<-cbind(perma_767_df, NMDS1, NMDS2)

#PH000767 PERMANOVA PLOT

ggplot(P767.plot, aes(NMDS1, NMDS2, color=binary.PH0000767))+
  geom_point(position=position_jitter(.1), shape=3)+##separates overlapping points ##draws 95% confidence interval ellipses
  stat_ellipse(aes(fill=binary.PH0000767), alpha=.2,type='t',size =1, geom="polygon")+
  theme_minimal()
w

View(p.value_microbes_PH0000773)
View(p.value_microbes_PH0000776)

View(p.value_microbes_PH0000522)

View(output_PH0000520_new)

