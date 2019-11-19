setwd("C://Users/Matt&Kez/Box Sync/Objective 1/Analysis/Pre-analysis/Correlations & PCA/Text files")
setwd("C://Users/mnn1/Box Sync/Objective 1/Analysis/Pre-analysis/Correlations/Text files")
library("FactoMineR")
library("factoextra")
library("corrplot")

###### Full macroeconomic dataset
dat <- read.table("macroeconomic_vars_yr.txt", header=T)
str(dat)

##### MACROECONOMIC VARIABLES ####

## Subset of economic variables - GDP, FDI, Inds_GDP, Agri_GDP, Dev flow agri, Dev flow env
datEcon <- dat[1:23, 2:7]

## Run PCA
econPCA <- PCA(datEcon, scale.unit=TRUE, ncp=5, graph=TRUE)

## Show Eigenvalues.  Eigenvalues measure the amount of variation retained by each principal component
eig.val <- get_eigenvalue(econPCA)
eig.val

## Scree plot
EconScree <- fviz_eig(econPCA, addlabels = TRUE, ylim = c(0, 80))


## Extract results for variables
Econvars <- get_pca_var(econPCA)
Econvars

## Plot of variables correlation with PC
Econvars_plot <- fviz_pca_var(econPCA, col.var = "black")
Econvars_plot

## Plot of variable correlation with PC coloured by their cos2 value
Econvars_plot_CorrCos2 <- fviz_pca_var(econPCA, col.var = "cos2",
                                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                                       repel = TRUE # Avoid text overlapping
)
Econvars_plot_CorrCos2

## The quality of representation of the variables on factor map is called cos2 (square cosine, squared coordinates## ).  Can visualize 

Econvars_cos2 <-corrplot(Econvars$cos2, is.corr=FALSE)

## Barplot of cos2

Econvars_cos2Bar <- fviz_cos2(econPCA, choice = "var", axes = 1:2)
Econvars_cos2Bar

## The contribution of variables to PC's describes how well each variable explains variability 

# Contributions of variables to PC1
Econvars_contrib_PC1 <- fviz_contrib(econPCA, choice = "var", axes = 1)
# Contributions of variables to PC2
Econvars_contrib_PC2 <- fviz_contrib(econPCA, choice = "var", axes = 2)
Econvars_contrib_PC1
Econvars_contrib_PC2
# Contributions of variables to PC1 and PC2
fviz_contrib(econPCA, choice = "var", axes = 1:2)

## Correlation plot with variables coloured based on their contributions
Econvars_plot_corrContrib <- fviz_pca_var(econPCA, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
Econvars_plot_corrContrib

#### PCA on Macroeconomic variables using princomp() to get loadings
prin_comp_econ <- prcomp(datEcon, scale.=T)
prin_comp_econ$rotation
biplot(prin_comp_econ, scale=0)
prin_comp_econ$x

# rotate via varimax
varimax <- varimax(prin_comp_econ$rotation)

# rotate via varimax and get new scores (taken from https://stats.stackexchange.com/questions/59213/how-to-compute-varimax-rotated-principal-components-in-r)

rawLoadings     <- prin_comp_econ$rotation[,1:6] %*% diag(prin_comp_econ$sdev, 6, 6)
scores <- scale(prin_comp_econ$x[,1:6]) %*% varimax(rawLoadings)$rotmat
print(scores[1:6,])

#### COMMODITY VARIABLES ####
## Subset of commodity variables - armi, rice_med, rub_med, corn_med, sug_med
datCom <- dat[1:23, c(8,9,12,15,18)]

## Run PCA
comPCA <- PCA(datCom, scale.unit=TRUE, ncp=5, graph=TRUE)

## Show Eigenvalues.  Eigenvalues measure the amount of variation retained by each principal component
eig.val <- get_eigenvalue(comPCA)
eig.val

## Scree plot
comScree <- fviz_eig(comPCA, addlabels = TRUE, ylim = c(0, 90))


## Extract results for variables
Comvars <- get_pca_var(comPCA)
Comvars

## Plot of variables correlation with PC
Comvars_plot <- fviz_pca_var(comPCA, col.var = "black")
Comvars_plot

## Plot of variable correlation with PC coloured by their cos2 value
Comvars_plot_CorrCos2 <- fviz_pca_var(comPCA, col.var = "cos2",
                                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                                       repel = TRUE # Avoid text overlapping
)
Comvars_plot_CorrCos2

## The quality of representation of the variables on factor map is called cos2 (square cosine, squared coordinates## ).  Can visualize 

Comvars_cos2 <-corrplot(Comvars$cos2, is.corr=FALSE)

## Barplot of cos2

Comvars_cos2Bar <- fviz_cos2(comPCA, choice = "var", axes = 1:2)
Comvars_cos2Bar

## The contribution of variables to PC's describes how well each variable explains variability 

# Contributions of variables to PC1
Comvars_contrib_PC1 <- fviz_contrib(comPCA, choice = "var", axes = 1)
# Contributions of variables to PC2
Comvars_contrib_PC2 <- fviz_contrib(comPCA, choice = "var", axes = 2)
Comvars_contrib_PC1
Comvars_contrib_PC2
# Contributions of variables to PC1 and PC2
fviz_contrib(comPCA, choice = "var", axes = 1:2)

## Correlation plot with variables coloured based on their contributions
Comvars_plot_corrContrib <- fviz_pca_var(comPCA, col.var = "contrib",
                                          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
Comvars_plot_corrContrib

#### PCA on Commodity variables using princomp() to get loadings
prin_comp_comm <- prcomp(datCom, scale.=T)
prin_comp_comm$rotation
biplot(prin_comp_comm, scale=0)
prin_comp_comm$x


#### PRODUCER PRICE VARIABLES ####
## Subset data for prod_rice, prod_rub, prod_cass, prod_corn, prod_sug

datProd <- dat[1:23, 21:25]

## Run PCA
prodPCA <- PCA(datProd, scale.unit=TRUE, ncp=5, graph=TRUE)

## Show Eigenvalues.  Eigenvalues measure the amount of variation retained by each principal component
eig.val <- get_eigenvalue(prodPCA)
eig.val

## Scree plot
prodScree <- fviz_eig(prodPCA, addlabels = TRUE, ylim = c(0, 80))


## Extract results for variables
Prodvars <- get_pca_var(prodPCA)
Prodvars

## Plot of variables correlation with PC
Prodvars_plot <- fviz_pca_var(prodPCA, col.var = "black")
Prodvars_plot

## Plot of variable correlation with PC coloured by their cos2 value
Prodvars_plot_CorrCos2 <- fviz_pca_var(prodPCA, col.var = "cos2",
                                      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                                      repel = TRUE # Avoid text overlapping
)
Prodvars_plot_CorrCos2

## The quality of representation of the variables on factor map is called cos2 (square cosine, squared coordinates## ).  Can visualize 

Prodvars_cos2 <-corrplot(Prodvars$cos2, is.corr=FALSE)

## Barplot of cos2

Prodvars_cos2Bar <- fviz_cos2(prodPCA, choice = "var", axes = 1:2)
Prodvars_cos2Bar

## The contribution of variables to PC's describes how well each variable explains variability 

# Contributions of variables to PC1
Prodvars_contrib_PC1 <- fviz_contrib(prodPCA, choice = "var", axes = 1)
# Contributions of variables to PC2
Prodvars_contrib_PC2 <- fviz_contrib(prodPCA, choice = "var", axes = 2)
Prodvars_contrib_PC1
Prodvars_contrib_PC2
# Contributions of variables to PC1 and PC2
fviz_contrib(prodPCA, choice = "var", axes = 1:2)

## Correlation plot with variables coloured based on their contributions
Prodvars_plot_corrContrib <- fviz_pca_var(prodPCA, col.var = "contrib",
                                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
Prodvars_plot_corrContrib

#### PCA on Producer price variables using princomp() to get loadings
prin_comp_prod <- prcomp(datProd, scale=T)
prin_comp_prod$rotation
biplot(prin_comp_prod, scale=0)
prin_comp_prod$x



