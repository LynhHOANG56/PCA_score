################# Input
dt1 <- read.csv("Data_96departements.csv", header = T, sep = ","); # Dep
head(dt1); dim(dt1); colnames(dt1)

dt2 <- read.csv("Data_34881communes.csv", header = T, sep = ","); # Commune
head(dt2); dim(dt2); colnames(dt2)

colnames(dt1[,c(4:33)])==colnames(dt2[,c(7:36)]) ## check colonnes des 30 variables

##### ALD-MDS : SalaireLag + TauxClub + TransportCommunLag + NbVille.CentreLag
dt_dep <- dt1[,c(10,12,32,18)]; colnames(dt_dep); dim(dt_dep)
dt_com <- dt2[,c(13,15,35,21)]; colnames(dt_com); dim(dt_com)

##### ALD30 : Salaire + TauxClub + TauxChomageLag + PasTransportLag + MarcheLag
#dt_dep <- dt1[,c(6,12,9,28,29)]; colnames(dt_dep); dim(dt_dep)
#dt_com <- dt2[,c(9,15,12,31,32)]; colnames(dt_com); dim(dt_com)


################# PCA avec factoextra
library(factoextra)
library(FactoMineR)
library(corrplot)
res.pca <- prcomp(dt_dep, scale = TRUE) # ACP des 96 deps
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 70)) # scree plot
# Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val
# Results for Variables/Individuals
res.var <- get_pca_var(res.pca)
res.ind <- get_pca_ind(res.pca)
# Le graphique de corrélation des variables
fviz_pca_var(res.pca,
             col.var = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
#La qualité de représentation des variables sur la carte de l’ACP
corrplot(res.var$cos2, is.corr=F)    ### Matrix cos2 des vars

######## Score des 96 deps
score_dep <- res.ind$coord[,c(1:2)]; summary(score_dep); 

#  > summary(score_dep[,1]) : ALD-MDS
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  -6.1656 -0.2067  0.2455  0.0000  0.7457  2.4959 

# > summary(score_dep[,1]) : ALD30
#     Min. 1st Qu.  Median    Mean  3rd Qu.    Max. 
#  -7.0720 -0.5758  0.1962  0.0000  1.0624  2.9055 


######## Score des 34881 communes
#  Prédire en utilisant PCA les Individus supplémentaires : 34881 communes
ind.sup <- dt_com # 34881 communes
ind.sup.coord <- predict(res.pca, newdata = ind.sup)
score_com <- ind.sup.coord[,c(1:2)]; summary(score_com);

#  > summary(score_com[,1]) : ALD-MDS
#       Min.  1st Qu.   Median     Mean    3rd Qu.     Max. 
#    -6.5945 -1.3452    -1.0403  -1.1483  -0.7835    11.9673 

#  > summary(score_com[,1]) : ALD30
#       Min.    1st Qu.  Median    Mean   3rd Qu.    Max. 
#   -9.6628   -1.5939   -0.9055  -0.9856 -0.2350   12.0082 

