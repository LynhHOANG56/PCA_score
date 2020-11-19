################# Input
data1 <- read.csv("dt1new.csv", header = T, sep = ","); # Dep
head(data1); dim(data1); colnames(data1)

data2 <- read.csv("dt2new.csv", header = T, sep = ","); # Commune
head(data2); dim(data2); colnames(data2) ## 

colnames(data1[,c(5:34)])==colnames(data2[,c(8:37)]) ## check colonnes des 30 variables

RegGLMNB <- function(inp, N){
  AICop <- Po <- c()
  for (i in 1:N){
    aic <- c()
    m <- combn(N,i); 
    n <- dim(combn(N,i))[2]; 
    for (k in 1:n){
      col <- m[,k]; col
      f <- as.formula(paste('ALD30 ~ ', 
            paste(c(colnames(inp)[col],'offset(log(Pop))'), collapse='+'))); 
      reg <- glm.nb(f, data = inp) 
      aic[k] <- reg$aic
    }
    AICop[i] <- min(aic)
    Po[i] <- which(aic==min(aic))
  }
  AICop
  Po
  is <- which(AICop == min(AICop)); is
  ks <- Po[is]
  ms<- combn(N,is); 
  col <- ms[,ks]; col
  f <- as.formula(paste('ALD30 ~ ', 
         paste(c(colnames(inp)[col],'offset(log(Pop))'), collapse='+'))); 
  reg <- glm.nb(f, data = inp) 
  return(reg)
}
inpALD30_1 <- data1[,c(5:14,4,36,35)]; colnames(inpALD30_1); N1=10
inpALD30_3 <- data1[,c(23:34,4,36,35)]; colnames(inpALD30_3); N3=12

library(MASS) ## glm.nb
Regression1 <- RegGLMNB (inpALD30_1,N1);  summary(Regression1)
Regression3 <- RegGLMNB (inpALD30_3,N3);  summary(Regression3)

##### ALD30-Silo1 : Salaire + TauxChomageLag + TauxClub + TauxBac + TauxOuvrierLag
dt_dep <- data1[,c(7,10,13,5,12)]; colnames(dt_dep); dim(dt_dep)
dt_com <- data2[,c(10,13,16,8,15)]; colnames(dt_com); dim(dt_com)

##### ALD30-Silo3 : TauxTransportCommun + TauxVoiture + TauxPasLag + TauxMarcheLag + TauxVeloLag
dt_dep <- data1[,c(27:31)]; colnames(dt_dep); dim(dt_dep)
dt_com <- data2[,c(30:34)]; colnames(dt_com); dim(dt_com)

################# PCA avec factoextra
library(factoextra)
library(FactoMineR)
library(corrplot)
res.pca <- prcomp(dt_dep, scale = TRUE) # ACP des 96 deps
#res.pca <- PCA(dt_dep, scale.unit = TRUE, graph = F)


# Results for Variables/Individuals
res.var <- get_pca_var(res.pca)
res.ind <- get_pca_ind(res.pca)

######## Score des 96 deps
score_dep <- res.ind$coord[,c(1:2)]; summary(score_dep); 

######## Score des 34881 communes
#  Prédire en utilisant PCA les Individus supplémentaires : 34881 communes
ind.sup <- dt_com # 34881 communes
ind.sup.coord <- predict(res.pca, newdata = ind.sup)
score_com <- ind.sup.coord[,c(1:2)]; summary(score_com);

#####ALD30-silo1-Dep
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  -3.0869 -0.6667 -0.2449  0.0000  0.3812  6.3513 
#####ALD30-silo1-Com
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -14.972  -3.540  -2.631  -2.701  -1.777   5.141 
#####ALD30-silo3-Dep
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -3.1283 -1.1267 -0.2935  0.0000  1.0829  4.9784 
#####ALD30-silo3-Com
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -26.5700  -3.1766  -1.7307  -2.2261  -0.7095  12.3121


##### Predict ALD30 sur la carte ald30silo1&3
ALD30_pred_Dep_Silo1 <- predict(Regression1, type="response", se.fit=F)
ALD30_pred_Dep_Silo3 <- predict(Regression3, type="response", se.fit=F)
summary(ALD30_pred_Dep_Silo1)
summary(ALD30_pred_Dep_Silo3)

ALD30_pred_Com_Silo1 <- predict(Regression1, newdata = data2, type="response", se.fit=F)
ALD30_pred_Com_Silo3 <- predict(Regression3, newdata = data2, type="response", se.fit=F)
summary(ALD30_pred_Com_Silo1)
summary(ALD30_pred_Com_Silo3)

# summary(ALD30_pred_Dep_Silo1)
#      Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   16529   61134  107882  129663  161185  523356 
# summary(ALD30_pred_Com_Silo1)
#      Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#   0.00    25.21    55.87   219.36   137.51 36887.64 
# summary(ALD30_pred_Dep_Silo3)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   16724   59506  108149  129802  165787  549234 
# summary(ALD30_pred_Com_Silo3)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#   0.00    30.72    69.43   336.22   180.16 81427.53 


