#for menopausal status

P786 = data.frame(non_binary$PH0000786, over_50_cols)

P786_pre <- subset(P786, non_binary.PH0000786 == 1)

p786_current <- subset(P786, non_binary.PH0000786 == 2)

p786_post <- subset(P786, non_binary.PH0000786 == 3)

perma_786_df_pre_current = rbind(P786_pre,p786_current)

perma_786_df_pre_post = rbind(P786_pre, p786_post)

perma_786_df_post_current = rbind(p786_post,p786_current)

#Permanova for premenopause and currently going through menopause 

perma_786_df_pre_current$non_binary.PH0000786 <- as.character(perma_786_df_pre_current$non_binary.PH0000786)

dim(perma_786_df_pre_current)

P786_matrix <- as.matrix(perma_786_df_pre_current[,2:83])

P786.mat<-sqrt(P786_matrix)

P786.dist<-vegdist(P786.mat, method='bray')

set.seed(36)

colnames(perma_786_df_pre_current)

P786.div_pre_current<-adonis2(P786.dist~non_binary.PH0000786, data=perma_786_df_pre_current, permutations = 999, method="bray", strata="PLOT")

View(P786.div_pre_current)

dispersion_786<-betadisper(P786.dist, group=perma_786_df_pre_current$non_binary.PH0000786)
permutest(dispersion_786)

plot(dispersion_786, hull=FALSE, ellipse=TRUE)

P_786_MDS<-metaMDS(P786.mat, distance="bray", k=2, trymax=35, autotransform=TRUE) 

stressplot(P_786_MDS)

NMDS1 <- P_786_MDS$points[,1] ##also found using scores(birdMDS)
NMDS2 <- P_786_MDS$points[,2]
P786.plot<-cbind(perma_786_df_pre_current, NMDS1, NMDS2)

#PH000786 PERMANOVA PLOT

ggplot(P786.plot, aes(NMDS1, NMDS2, color=non_binary.PH0000786))+
  geom_point(position=position_jitter(.1), shape=3)+##separates overlapping points ##draws 95% confidence interval ellipses
  stat_ellipse(aes(fill=non_binary.PH0000786), alpha=.2,type='t',size =1, geom="polygon")+
  theme_minimal()


#Permanova for Pre menopausal and those going through menopause

#for menopausal status

P786 = data.frame(non_binary$PH0000786, over_50_cols)

P786_pre <- subset(P786, non_binary.PH0000786 == 1)

p786_current <- subset(P786, non_binary.PH0000786 == 2)

p786_post <- subset(P786, non_binary.PH0000786 == 3)

perma_786_df_pre_post = rbind(P786_pre,p786_current)

perma_786_df_pre_post = rbind(P786_pre, p786_post)

perma_786_df_post_current = rbind(p786_post,p786_current)

#Permanova for premenopause and those who are post menopausal

perma_786_df_pre_post$non_binary.PH0000786 <- as.character(perma_786_df_pre_post$non_binary.PH0000786)

dim(perma_786_df_pre_post)

P786_matrix <- as.matrix(perma_786_df_pre_post[,2:83])

P786.mat<-sqrt(P786_matrix)

P786.dist<-vegdist(P786.mat, method='bray')

set.seed(36)

colnames(perma_786_df_pre_post)

P786.div<-adonis2(P786.dist~non_binary.PH0000786, data=perma_786_df_pre_post, permutations = 999, method="bray", strata="PLOT")

View(P786.div)

dispersion_786<-betadisper(P786.dist, group=perma_786_df_pre_post$non_binary.PH0000786)
permutest(dispersion_786)

plot(dispersion_786, hull=FALSE, ellipse=TRUE)

P_786_MDS<-metaMDS(P786.mat, distance="bray", k=2, trymax=35, autotransform=TRUE) 

stressplot(P_786_MDS)

NMDS1 <- P_786_MDS$points[,1] ##also found using scores(birdMDS)
NMDS2 <- P_786_MDS$points[,2]
P786.plot<-cbind(perma_786_df_pre_post, NMDS1, NMDS2)

#PH000786 PERMANOVA PLOT

ggplot(P786.plot, aes(NMDS1, NMDS2, color=non_binary.PH0000786))+
  geom_point(position=position_jitter(.1), shape=3)+##separates overlapping points ##draws 95% confidence interval ellipses
  stat_ellipse(aes(fill=non_binary.PH0000786), alpha=.2,type='t',size =1, geom="polygon")+
  theme_minimal()

#Permanova for post menopause and currently going through menopause 

perma_786_df_post_current$non_binary.PH0000786 <- as.character(perma_786_df_post_current$non_binary.PH0000786)

dim(perma_786_df_post_current)

P786_matrix <- as.matrix(perma_786_df_post_current[,2:83])

P786.mat<-sqrt(P786_matrix)

P786.dist<-vegdist(P786.mat, method='bray')

set.seed(36)

colnames(perma_786_df_post_current)

P786.div<-adonis2(P786.dist~non_binary.PH0000786, data=perma_786_df_post_current, permutations = 999, method="bray", strata="PLOT")

View(P786.div)

dispersion_786<-betadisper(P786.dist, group=perma_786_df_post_current$non_binary.PH0000786)
permutest(dispersion_786)

plot(dispersion_786, hull=FALSE, ellipse=TRUE)

P_786_MDS<-metaMDS(P786.mat, distance="bray", k=2, trymax=35, autotransform=TRUE) 

stressplot(P_786_MDS)

NMDS1 <- P_786_MDS$points[,1] ##also found using scores(birdMDS)
NMDS2 <- P_786_MDS$points[,2]
P786.plot<-cbind(perma_786_df_post_current, NMDS1, NMDS2)

#PH000786 PERMANOVA PLOT

ggplot(P786.plot, aes(NMDS1, NMDS2, color=non_binary.PH0000786))+
  geom_point(position=position_jitter(.1), shape=3)+##separates overlapping points ##draws 95% confidence interval ellipses
  stat_ellipse(aes(fill=non_binary.PH0000786), alpha=.2,type='t',size =1, geom="polygon")+
  theme_minimal()





taxa_output_meno_status <- apply(meno_status_taxa_df[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000786 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = meno_status_taxa_df, REML=F, na.action=na.omit)
  
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})
