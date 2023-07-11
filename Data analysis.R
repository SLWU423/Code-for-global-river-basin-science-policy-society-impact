library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel)
library(ggpubr)


klg.lst<- read_excel(...,sheet = 1, col_names = TRUE)

Rivernames = unique(klg.lst$River)

klg.lstbyriver<- split(klg.lst, klg.lst$River)

split.byyr<- function(df){
  g<- split(df, df$Year)
  return(g)
}

klg.lstbyriverbyyr<- lapply(klg.lstbyriver, split.byyr)

library(reshape2)

###### Establish the knowlegde network by river by year ######
# this function will form kw-disp matrix, having kw in rows and disp in columns
get.2modegraph<- function(df){
  
  #need to distinguish kw and disp
  df[which(df[,1]=="Art"), 1]<- "Art-kw"
  df[which(df[,2]=="Art"), 2]<- "Art-dis"
  df[which(df[,1]=="Forestry"), 1]<- "Forestry-kw"
  df[which(df[,2]=="Forestry"), 2]<- "Forestry-dis"
  df[which(df[,1]=="History"), 1]<- "History-kw"
  df[which(df[,2]=="History"), 2]<- "History-dis"
  df[which(df[,1]=="Law"), 1]<- "Law-kw"
  df[which(df[,2]=="Law"), 2]<- "Law-dis"
  df[which(df[,1]=="Religion"), 1]<- "Religion-kw"
  df[which(df[,2]=="Religion"), 2]<- "Religion-dis"
  df[which(df[,1]=="Transportation"), 1]<- "Transportation-kw"
  df[which(df[,2]=="Transportation"), 2]<- "Transportation-dis"
  
  #use of operation similar to pivot table in Excel rather than standard 2-mode network operations
  newdf<- as.matrix(dcast(df, df$"Mapped kw"~df$"Disp", value.var = "Weight", fun.aggregate = sum))
  newdf2<- apply(newdf, 1, as.numeric) #matrix transposed
  newdf2<- newdf2[-1,]
  
  return(as.matrix(newdf2))
}

Mat.byriverbyyr<- list()

for (i in 1:72){
  Mat.byriverbyyr[[i]]<- lapply(klg.lstbyriverbyyr[[i]], get.2modegraph)
}


###### Calculate Degree of Multidisciplinarity ######
Mat.byriverbyyr.dm<- list()

cal.DM<- function(df){
  totalcell = nrow(df)*ncol(df)
  DM = 1-(sum(df==0)/totalcell)
  return(DM)
}

for (i in 1:72){
  Mat.byriverbyyr.dm[[i]]<- lapply(Mat.byriverbyyr[[i]], cal.DM)
}



###### calculate Degree of issue-interconnectivity ######
get.1modegraph.kw<- function(df){
  
  df[which(df[,1]=="Art"), 1]<- "Art-kw"
  df[which(df[,2]=="Art"), 2]<- "Art-dis"
  df[which(df[,1]=="Forestry"), 1]<- "Forestry-kw"
  df[which(df[,2]=="Forestry"), 2]<- "Forestry-dis"
  df[which(df[,1]=="History"), 1]<- "History-kw"
  df[which(df[,2]=="History"), 2]<- "History-dis"
  df[which(df[,1]=="Law"), 1]<- "Law-kw"
  df[which(df[,2]=="Law"), 2]<- "Law-dis"
  df[which(df[,1]=="Religion"), 1]<- "Religion-kw"
  df[which(df[,2]=="Religion"), 2]<- "Religion-dis"
  df[which(df[,1]=="Transportation"), 1]<- "Transportation-kw"
  df[which(df[,2]=="Transportation"), 2]<- "Transportation-dis"
  
  g<- graph_from_data_frame(df, vertices = NULL)
  
  g_el<- as_edgelist(g)
  colnames(g_el)<- c("kw", "disp")
  V(g)$type<- ifelse(V(g)$name %in% g_el[,"kw"], TRUE, FALSE)
  E(g)$weight<- E(g)$"Weight"
  
  gproj<- bipartite.projection(g, V(g)$type, multiplicity = TRUE)
  
  gproj.kw<- gproj$proj2
  
  measures.kw<- degree(gproj$proj2)
  
  return(measures.kw)
}


Mat.byriverbyyr.di<- list()

for (i in 1:72){
  Mat.byriverbyyr.di[[i]]<- lapply(klg.lstbyriverbyyr[[i]], get.1modegraph.kw)
}



###### Trend analysis ######
library(Kendall)
library(funtimes)
library(trend)

#read in yearly data by river on DM, DI, society, and policy indicators
lst.byriver<- read_excel()
  
temp.lst<- list()
temp.lst2<- list()

mkres<- list()
slres<- list()

for (i in 1:72) {
  temp.lst[[i]]<- lst.byriver[[i]]
  temp.lst2[[i]]<-lst.byriver[[i]]
  
  temp.lst[[i]]<- apply(temp.lst[[i]][,c(3:5)],2, function(x) ts(x, start = 1962, frequency = 0.2))
  temp.lst2[[i]]<- apply(temp.lst2[[i]][,c(3:5)],2, function(x) ts(x, start = 1962, frequency = 0.2))
  
  mkres[[i]]<- apply(temp.lst[[i]][,c(1:3)],2, MannKendall)
  slres[[i]]<- apply(temp.lst2[[i]][,c(1:3)], 2, sens.slope)
  
  #combres<- rbind(mkres, slres)
}

mkres.ul<- unlist(mkres, recursive = FALSE, use.names = TRUE)
mkres.ul2<- unlist(mkres.ul, recursive = FALSE, use.names = TRUE)
slres.ul<- unlist(slres, recursive = FALSE, use.names = TRUE)
slres.ul2<- unlist(slres.ul, recursive = FALSE, use.names = TRUE)



###### linear models ######

uniq_river<- unique(lst.byriver$River)

lm.cal<- function(x){
  lm(x~lst.byriver[[i]][["DM"]]+lst.byriver[[i]][["DI"]],data = lst.byriver)}

lm.temp<- list()
lm.res<- list()
lm.res2<- list()
lmall<- list()

for (i in uniq_river){
  lm.temp[[i]]<- apply(lst.byriver[[i]][,c(3:8)],2,lm.cal)
  lm.res[[i]]<- sapply(lm.temp[[i]],'[[',1)
  lm.res2[[i]]<- sapply(lm.temp[[i]],summary)
  lmall[[i]]<- rbind(lm.res[[i]], lm.res2[[i]])
}


lmall<- list()
lmall.RA<- list()
lmall.RU<- list()
lmall.RG<- list()
lmall.SOC<- list()
lmall.ENV<- list()
lmall.ECO<- list()

lm.coef.pvalue.RA<- list()
lm.coef.pvalue.RU<- list()
lm.coef.pvalue.RG<- list()
lm.coef.pvalue.SOC<- list()
lm.coef.pvalue.ENV<- list()
lm.coef.pvalue.ECO<- list()

lm.adjrsq.RA<- list()
lm.adjrsq.RU<- list()
lm.adjrsq.RG<- list()
lm.adjrsq.SOC<- list()
lm.adjrsq.ENV<- list()
lm.adjrsq.ECO<- list()

for (i in uniq_river){
  lmall[[i]]<- lst.byriver[[i]]
  
  lmall.RA[[i]]<- lmall[[i]][[1]]
  lmall.RU[[i]]<- lmall[[i]][[2]]
  lmall.GC[[i]]<- lmall[[i]][[3]]
  
  lmall.SOC[[i]]<- lmall[[i]][[4]]
  lmall.ENV[[i]]<- lmall[[i]][[5]]
  lmall.ECO[[i]]<- lmall[[i]][[6]]
  
  
  lm.coef.pvalue.RA[[i]]<- summary(lmall.RA[[i]])$coefficients
  lm.adjrsq.RA[[i]]<- summary(lmall.RA[[i]])$adj.r.squared
  lm.coef.pvalue.RU[[i]]<- summary(lmall.RU[[i]])$coefficients
  lm.adjrsq.RU[[i]]<- summary(lmall.RU[[i]])$adj.r.squared
  lm.coef.pvalue.GC[[i]]<- summary(lmall.GC[[i]])$coefficients
  lm.adjrsq.GC[[i]]<- summary(lmall.GC[[i]])$adj.r.squared
  
  lm.coef.pvalue.SOC[[i]]<- summary(lmall.SOC[[i]])$coefficients
  lm.adjrsq.SOC[[i]]<- summary(lmall.SOC[[i]])$adj.r.squared
  lm.coef.pvalue.ENV[[i]]<- summary(lmall.ENV[[i]])$coefficients
  lm.adjrsq.ENV[[i]]<- summary(lmall.ENV[[i]])$adj.r.squared
  lm.coef.pvalue.ECO[[i]]<- summary(lmall.ECO[[i]])$coefficients
  lm.adjrsq.ECO[[i]]<- summary(lmall.ECO[[i]])$adj.r.squared
}



###### Clustering analysis ###### 
library(factoextra)
library(FactoMineR)
library(purrr)
library(cluster)
library(NbClust)

lmall.policy<- cbind(lm.coef.RA, lm.coef.RU, lm.coef.RG)
lmall.society<- cbind(lm.coef.SOC, lm.coef.ENV, lm.coef.ECO)

set.seed(233333)

Hclust1<- NbClust(lmall.policy[,c(2:7)], distance='euclidean', method = "ward.D2")
dist1<- get_dist(lmall.policy[,c(2:7)], method = 'euclidean')
Hclust1<- hclust(dist1, method = "ward.D2")


Hclust2<- NbClust(lmall.society[,c(2:7)], distance='euclidean', method = "ward.D2")
dist2<- get_dist(lmall.society[,c(2:7)], method = 'euclidean')
Hclust2<- hclust(dist2, method = "ward.D2")

#Append cluster group to table
cut_ward1 <- cutree(Hclust1, k = 2)
lm.lst_cl_RE <- mutate(lmall.policy, cluster = cut_ward1)

cut_ward2 <- cutree(Hclust2, k = 2)
lm.lst_cl_IM <- mutate(lmall.society, cluster = cut_ward2)




###### Pareto front for optimization ######

library(GA)

lmall.RE0<- subset(lmall.policy, RECluster ==0)
lmall.RE1<- subset(lmall.policy, RECluster ==1)
lmall.RE2<- subset(lmall.policy, RECluster ==2)

lmall.RE.avg <- lmall.sum %>%
  group_by(RECluster) %>%
  dplyr::summarize_at(vars(RA.coef.DOM, RA.coef.DEG, RA.coef.int, RU.coef.DOM, RU.coef.DEG, RU.coef.int, RG.coef.DOM, RG.coef.DEG, RG.coef.int), list(name = mean))


# Define the objective function
Objfun <- function(x) {
  y1 <- x[1]*lmall.RE.avg$RA.coef.DOM_name[3] + x[2]*lmall.RE.avg$RA.coef.DEG_name[3] + lmall.RE.avg$RA.coef.int_name[3]
  y2 <- x[1]*lmall.RE.avg$RU.coef.DOM_name[3] + x[2]*lmall.RE.avg$RU.coef.DEG_name[3] + lmall.RE.avg$RU.coef.int_name[3]
  y3 <- x[1]*lmall.RE.avg$RG.coef.DOM_name[3] + x[2]*lmall.RE.avg$RG.coef.DEG_name[3] + lmall.RE.avg$RG.coef.int_name[3]
  return(c(y1, -y2, -y3))
}

# Define the search space / boundary conditions for decision variables
lower_bound <- c(0, 0)
upper_bound <- c(1, 1)

# Set GA control parameters directly in the ga() function
set.seed(666)
result <- nsga2R::nsga2R(fn=Objfun, varNo=2, objDim = 3,
                         generations = 1000, popSize = 100, 
                         lowerBounds = lower_bound, upperBounds = upper_bound)
plot(result$objectives)
dev.off()

# Print the Pareto optimal solutions
Opt_result3 <- result$objectives %>% 
  as.data.frame() %>% 
  bind_cols(as.data.frame(result$parameters)) %>% 
  filter(result$paretoFrontRank == 1) %>% 
  mutate_all(.funs = function(x){round(x,3)}) %>%
  distinct() %>%  
  rename("RA" = V1...1, "RU" = V2...2, "RG" = V3,
         "DOM" = V1...4, "DEG" = V2...5)

Opt_result4 <- Opt_result3 %>% 
  as.data.frame() %>% 
  filter(RA %in% c(min(RA), max(RA)) | RU %in% c(min(RU), max(RU)) | RG %in% c(min(RG), max(RG)))

library(plotly)
plot_ly(Opt_result3, x = ~RA, y = ~RU, z = ~RG) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Resource availability'),
                      yaxis = list(title = 'Resource utilization'),
                      zaxis = list(title = 'Governance capacity')))
par(mfrow=c(1,3))
plot(x = Opt_result3$RA, y = Opt_result3$RU, xlab = 'Resource availability (RA)', ylab= 'Resource utilization (RU)')
plot(x = Opt_result3$RA, y = Opt_result3$RG, xlab = 'Resource availability (RA)', ylab= 'Governance capacity (GC)')
plot(x = Opt_result3$RU, y = Opt_result3$RG, xlab = 'Resource utilization (RU)', ylab= 'Governance capacity (GC)')




lmall.IM0<- subset(lmall.society, IMCluster ==0)
lmall.IM1<- subset(lmall.society, IMCluster ==1)
lmall.IM2<- subset(lmall.society, IMCluster ==2)

lmall.IM.avg <- lmall.sum %>%
  group_by(IMCluster) %>%
  dplyr::summarize_at(vars(SOC.coef.DOM, SOC.coef.DEG, SOC.coef.int, ENV.coef.DOM, ENV.coef.DEG, ENV.coef.int, ECO.coef.DOM, ECO.coef.DEG, ECO.coef.int), list(name = mean))


# Define the objective function
Objfun_IM <- function(x) {
  #y1 <- x[1]*lmall.RE.avg$RA.coef.DOM_name[2] + x[2]*lmall.RE.avg$RA.coef.DEG_name[2] + lmall.RE.avg$RA.coef.int_name[2]
  #y2 <- x[1]*lmall.RE.avg$RU.coef.DOM_name[2] + x[2]*lmall.RE.avg$RU.coef.DEG_name[2] + lmall.RE.avg$RU.coef.int_name[2]
  #y3 <- x[1]*lmall.RE.avg$RG.coef.DOM_name[2] + x[2]*lmall.RE.avg$RG.coef.DEG_name[2] + lmall.RE.avg$RG.coef.int_name[2]
  
  y4 <- x[1]*lmall.IM.avg$SOC.coef.DOM_name[2] + x[2]*lmall.IM.avg$SOC.coef.DEG_name[2] + lmall.IM.avg$SOC.coef.int_name[2]
  y5 <- x[1]*lmall.IM.avg$ENV.coef.DOM_name[2] + x[2]*lmall.IM.avg$ENV.coef.DEG_name[2] + lmall.IM.avg$ENV.coef.int_name[2]
  y6 <- x[1]*lmall.IM.avg$ECO.coef.DOM_name[2] + x[2]*lmall.IM.avg$ECO.coef.DEG_name[2] + lmall.IM.avg$ECO.coef.int_name[2]
  return(c( -y4, y5, -y6))
}

# Define the search space / boundary conditions for decision variables
lower_bound <- c(0, 0)
upper_bound <- c(1, 1)

# Set GA control parameters directly in the ga() function
set.seed(12333)
result_IM <- nsga2R::nsga2R(fn=Objfun_IM, varNo=2, objDim = 3,
                            generations = 1000, popSize = 100, 
                            lowerBounds = lower_bound, upperBounds = upper_bound)
plot(result_IM$objectives)
dev.off()

# Print the Pareto optimal solutions
Opt_result_IM <- result_IM$objectives %>% 
  as.data.frame() %>% 
  bind_cols(as.data.frame(result_IM$parameters)) %>% 
  filter(result_IM$paretoFrontRank == 1) %>% 
  mutate_all(.funs = function(x){round(x,3)}) %>%
  distinct() %>%  
  rename("SOC" = V1...1, "ENV" = V2...2, "ECO" = V3, 
         "DOM" = V1...4, "DEG" = V2...5)

Opt_result_IM2 <- Opt_result_IM %>% 
  as.data.frame() %>% 
  filter(SOC %in% c(min(SOC)) | ENV %in% c( min(ENV)) | ECO %in% c(min(ECO)) )

plot_ly(Opt_result_IM, x = ~abs(SOC), y = ~abs(ENV), z = ~abs(ECO)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Social'),
                      yaxis = list(title = 'Environmental'),
                      zaxis = list(title = 'Economic')))
par(mfrow=c(1,3))
plot(x = Opt_result_IM$SOC, y = Opt_result_IM$ENV, xlab = 'Social', ylab= 'Environmental')
plot(x = Opt_result_IM$SOC, y = Opt_result_IM$ECO, xlab = 'Social', ylab= 'Economic')
plot(x = Opt_result_IM$ENV, y = Opt_result_IM$ECO, xlab = 'Environmental', ylab= 'Economic')