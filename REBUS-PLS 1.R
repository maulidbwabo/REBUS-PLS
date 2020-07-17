#A Doctoral Candidate in Management Science and Enginerring 
#Jiangsu University (China)
# I developed these codes amid of spring and summer 
# Maulid Bwabo
#I develop these codes and published paper to SSCI Indexed Journal (31.01.2020) 
# only if you havent load it
library(plspm)
Gov= edit(Governace_3)
# rows of the path matrix
Transparency =c(0,0,0,0,0)
Accountability =c(0,0,0,0,0)
Competition=c(0,0,0,0,0)
Legal=c(1,1,1,0,0)
Value=c(1,0,1,1,0)

# matrix created by row binding
Governance_3_path =rbind(Transparency,Accountability,Competition,Legal,Value)
# inner model matrix

# list of blocks(outer model)
Governance_3_blocks=list(1:9, 10:17, 18:26, 27:34,35:41)

# vector of modes (reflective)
Governance_3_mods =rep("A",5)

# apply plspm
Governance_3_pls =plspm(Governace_3, Governance_3_path, Governance_3_blocks, modes = Governance_3_mods)
#workable arguement
loc.model.1_Governance_3_pls =plspm(Governace_3, Governance_3_path, Governance_3_blocks, modes = Governance_3_mods)
# running bootstrap validation
Governace_3=plspm(Governace_3, Governance_3_path,Governance_3_blocks, modes = Governance_3_mods,
                  boot.val = TRUE, br = 10,00)

# plot the inner matrix
plot(Governance_3_pls)

# bootstrapped path coefficients
Governance_3_pls$boot

# summarized results
summary(Governance_3_pls)

# running bootstrap validation
Governace_3 =plspm(Governace_3, Governance_3_path,Governance_3_blocks, modes = Governance_3_mods,
                   boot.val = TRUE, br = 10,00)
# bootstrap results
Governance_3l$boot
# Rebus Test 
#global 

# Calculate global plspm
Gov_inner= matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,0,1,1,0), 5, 5, byrow=TRUE)
dimnames(Gov_inner) = list(c("Transparency", "Accountability", "Competition", "Legal","Value"),
                           c("Transparency", "Accountability", "Competition", "Legal","Value"))

Gov_outer =list(1:9,10:17,18:26,27:34,35:41)
Gov_mod = c("A","A", "A","A","A") # reflective indicators
Gov_outer = list(c(1,2,3,4,5), c(6,7,8,9,10), c(11,12,13))
Gov_global = plspm(Governace_3, Gov_inner,
                   Gov_outer, modes=Gov_mod)

Gov_global
summary(Gov_global)
summary(local_rebus)
#cluster 
## Then compute cluster analysis on residuals of global model
Gov_clus = res.clus(Gov_global)
## To complete REBUS, run iterative algorithm
rebus_Gov = it.reb(Gov_global, Gov_clus, nk=4,
                   stop.crit=0.005, iter.max=100)
## You can also compute complete outputs
## for local models by running:
local_rebus = local.models(Gov_global, rebus_Gov)

# Display plspm summary for first local model
summary(local_rebus$loc.model.1)
summary(local_rebus$loc.model.2)
summary(local_rebus$loc.model.3)
summary(local_rebus$loc.model.4)
summary(loc.model.1)
summary(loc.model.2)
# apply rebus.test
Gov_permu = rebus.test(Gov_global, rebus_Gov)
# inspect sim.rebus
Gov_permu
Gov_permu$test_1_2
Gov_permu$test_1_3
Gov_permu$test_1_4
Gov_permu$test_2_3
Gov_permu$test_2_4
Gov_permu$test_3_4
Gov_permu$test_2_1

#Boot
# inner model matrix

Gov_path =rbind(Transparency,Accountability,Competition,Legal,Value) 
Gov_val =plspm(Governace_3, Gov_path, Gov_outer, modes = Gov_mod,
               boot.val = TRUE, br = 10,00)
Gov_val =plspm( Governace_3,Gov_inner, Gov_outer, modes = Gov_mod,
                boot.val = TRUE, br = 200)
Gov_val=plspm(Governace_3, Gov_inner,Gov_outer, modes=Gov_mod,
              boot.val = TRUE, br = 200)

loc.model.1_val$boot

# plot the inner matrix
Governance_3_pls$unidim

# plotting loadings of the outer model
plot(Governance_3_pls, what ="loadings", arr.width = 0.1)

# inner model resutls
Governance_3_pls$inner_model

# hierarchical cluster analysis on the LV scores
Governance_3_hclus =hclust(dist(Governance_3_pls$scores), method ="ward.D")

# plot dendrogram
plot(Governance_3_hclus, xlab ="", sub ="", cex = 0.8)
abline(h = 40, col ="#bc014655", lwd = 4)

# cut tree to obtain 4 clusters
clusters =cutree(Governance_3_hclus, k =4)

#R commander
library(Rcmdr)

# how many observations in each clsuter?
table(clusters)

# latent variable scores in data frame
Governance_3_scores =as.data.frame(Governance_3_pls$scores)

# add clusters to data frame
Governance_3_scores$Cluster =as.factor(clusters)

# what does the data look like?
head(Governance_3_scores, n = 3)

# package plyr
library(plyr)

# calculate cluster centroids
centroids =ddply(Governance_3_scores,.(Cluster), summarise,
                 AvgTransparency =mean(Transparency),AvgAccountability=mean(Accountability),AvgLegal=mean(Legal), AvgCompetitionn =mean(Competition),
                 AvgValue =mean(Value))
#Show centroid
centroids

# package ggplot2
library(ggplot2)

# ggplot: Transparency -vs- Value
ggplot(data = Governance_3_scores,
       aes(x = Transparency, y = Value, label =rownames(Governance_3_scores))) +
  geom_text(size = 4, alpha = 0.8,aes(color = Cluster))

# ggplot: Transparency -vs- Legal
ggplot(data = Governance_scores,
       aes(x = Transparency, y = Legal, label =rownames(Governance_scores))) +
  geom_text(size = 4, alpha = 0.8,aes(color = Cluster))

# ggplot: Accountability -vs- Legal
ggplot(data = Governance_scores,
       aes(x = Accountability, y = Legal, label =rownames(Governance_scores))) +
  geom_text(size = 4, alpha = 0.8,aes(color = Cluster))

# ggplot: Competition  -vs- Value
ggplot(data = Governance_scores,
       aes(x = Competition, y = Value, label =rownames(Governance_scores))) +
  geom_text(size = 4, alpha = 0.8,aes(color = Cluster))
# ggplot: Competition -vs- Legal
ggplot(data = Governance_scores,
       aes(x = Competition, y = Legal, label =rownames(Governance_scores))) +
  geom_text(size = 4, alpha = 0.8,aes(color = Cluster))

# ggplot: Legal-vs- Value
ggplot(data = Governance_scores,
       aes(x = Legal, y = Value, label =rownames(Governance_scores))) +
  geom_text(size = 4, alpha = 0.8,aes(color = Cluster))

#apply REBUS 
Governance_3_rebus =rebus.pls(Governance_3_pls)

# whats in Governance_rebus
Governance_3_rebus

# GoF global model
Governance_3_pls$gof 

# teams in class 1
names(Governance_3_rebus$segments[Governance_3_rebus$segments == 1])

# teams in class 2
names(Governance_3_rebus$segments[Governance_3_rebus$segments == 2])

# teams in class 3
names(Governance_3_rebus$segments[Governance_3_rebus$segments == 3])

# teams in class 4
names(Governance_3_rebus$segments[Governance_3_rebus$segments == 4])

# teams in class 5
names(Governance_rebus$segments[Governance_rebus$segments == 5])

# local plspm models
locs =local.models(Governance_3_pls, Governance_3_rebus)

# whats in locs?
locs

# plot inner models
plot(locs$loc.model.1, main ="Class 1")
plot(locs$loc.model.2, main ="Class 2")
plot(locs$loc.model.3, main ="Class 3")
plot(locs$loc.model.4, main ="Class 4")
plot(locs$loc.model.5, main ="Class 5")
summary(locs$loc.model.1)
summary(locs$loc.model.2)
summary(locs$loc.model.3)
Governance_3_pls=c(locs$boot$paths)
summary(glob$.model)
summary(locs$loc.model.4)
summary(locs$loc.model.3)
summary(locs$loc.model.2)
summary(locs$loc.model.1)
Summary(locs$loc.model.1)
Summary(locs$loc.model.2)
Summary()
summary("class 1")
# data frame of loadings
rebus_loads =as.data.frame(cbind(Governance_3_pls$outer_model$loading,
                                 Governance_3_rebus$loadings))  
#add column names
colnames(rebus_loads) =c("Global",paste("Class", 1:3, sep=""))  
# create factor with names of indicators
aux_inds =factor(rownames(rebus_loads),
                 levels=c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10",
                          "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "C1", 
                          "C2", "C3", "C5" ,"C7", "TR", "CO","NTA" ,"VFM", 
                          "L1" ,"L3", "L4","L5", "L6", "L7", "L9", "V1",
                          "V2", "V3","V4","V5","V6","V9"))
# add factor of indicator
rebus_loads$indicator = aux_inds
# add constructs
rebus_loads$construct =rep(rownames(Governance_3_path),sapply(Governance_3_blocks, length))
# what does it look like?
head(rebus_loads)
#Melting of the data frame 
# package reshape
library(reshape)
# melt loadings
melt_loads =melt(rebus_loads, id.vars =c("indicator","construct"),
                 variable_name ="model")  

# what does it look like?
head(melt_loads) 
# package RColorBrewer
library(RColorBrewer)

# plot loadings
ggplot(data=melt_loads,aes(x=indicator, y=value, fill=construct)) +
  # apply bar geom
  geom_bar(stat ="identity") +
  # require facetting based on model  
  facet_wrap(~ model) +
  # x-axis label
  xlab("Indicators") +
  # y-axis-label
  ylab("Loadings") +
  # add colors of bars
  scale_fill_manual(values =brewer.pal(9,"PuOr")[c(1,2,3,4,5)]) +
  # labels of x-axis at an angle of 90 degrees
  theme(axis.text.x =element_text(angle = 90, hjust = 1))
#Permutations Test 
# apply rebus.test
Governance_3_test =rebus.test(Governance_3_pls, Governance_3_rebus)
# check results
Governance_3_test 

# apply rebus.test
Governance_3_test =rebus.test(Governance_3_pls, Governance_3_rebus)
# check results
Governance_3_test  
# class 1 -vs- class 2
Governance_3_test$test_1_2  
# class 1 -vs- class 3
Governance_3_test$test_1_3
# class 1 -vs- class 4
Governance_3_test$test_1_4
# class 2 -vs- class 3
Governance_3_test$test_2_3
# class 2 -vs- class 4
Governance_3_test$test_2_4
# class 3 -vs- class 4
Governance_3_test$test_3_4
# plot path coefficients
plot(Governance_3_pls)

#Rebus Hypothesis 
rebus.pls(pls, Y= NULL, stop.crit = 0.005,
          iter.max = 100)
local.models(pls, y, Governace_3 = NULL)
#I am acknowledging these codes have been developed after reading the PLS-PM handbook.
#The book explicitly gives astutely bumper to bumper approach regarding the REBUS-PLS. 
#I do appreciate the time and effort from Prof. Gaston Sanchez who have been prolific endeavour to share intellectuals and coding skills regarding the PLS-PM.     