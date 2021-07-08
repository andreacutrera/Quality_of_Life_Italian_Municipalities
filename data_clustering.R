################################################################################
##########                 Statistical Learning Project              ###########
##########                 Andrea Pio Cutrera - 965591               ###########
##########                   Municipalities in Italy                 ###########
################################################################################

##################### load all the packages required
library(dplyr)
library(factoextra)
library(tidyverse)
library(ggplot2)
####################

#read the csv I prepared in the first preprocessing and cleaning
################################################################################
setwd('/Users/andreacutrera/Desktop/Municipalities-Provinces')

prov <- read.csv('prov.csv', sep = ',', header = T)
municip <- read.csv('municipalities.csv', sep = ',', header = T)

#in all of them delete the first column
municip <- municip[,-1]
prov <- prov[,-1]

rownames(prov) <- prov$codice_provincia
prov <- prov[,-c(1,2)]
rownames(municip) <- municip$comune
municip <- municip[,-1]

#now my dataset is made up by a bunch of int-num variables (58)
#114 Provinces (observations for 2011)
str(prov)
dim(prov)
rownames(prov)
colnames(prov)

str(municip)
dim(municip)
rownames(municip)
colnames(municip)
################################################################################
my_vars <- c('variazione_pop_residente', 
          'densita_umana', 'mobilita_privata',
          'vecchiaia', 'verde_urbano_pro_capite',
          'coppie_giovani_con_figli', 'presenza_universitaria', 'pendolarismo',
          'dinamismo_economico', 'tasso_funzione_ricettiva', 
          'visitatori_luoghi_cultura', 'intrattenimento',
          'digital_divide', 'stazioni_accessibili', 'posti_letto_ospedale',
          'biblioteche', 'abbandono_scolastico_2grado', 'disoccupazione',
          'famiglie_rischio_disagio_economico',
          'anziani_soli', 'suicidio', 'affollamento_abitazioni',
          'servizi_abitazione', 'popolazione_straniera', 'gini', 
          'occupazione_ita_straniera', 'occupazione_m_f', 'suolo_agricolo_utilizzato',
          'rifiuti_urbani_pro_capite', 'mobilita_lenta', 'acqua_potabile', 
          'raccolta_differenziata', 'impianti_fotovoltaici', 
          'auto_e5_e6', 'centri_eccellenza')




####################################################
#UNSUPERVISED
####################################################

#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
municip %>% select(all_of(my_vars)) -> municip_subset

colSums(is.na(municip_subset))
#stazioni accessibili 350 missing values
municip_subset %>% drop_na() -> municip_subset
colSums(is.na(municip_subset))
#35 variables, all regressors for 7571 MUNICIPALITIES
dim(municip_subset)

#try to cluster municipalities
mun_standard <- scale(municip_subset) # To standarize the variables
summary(mun_standard)
str(mun_standard)


set.seed(12345)
k.means.fit <- kmeans(mun_standard, 6) # try with k = 6
str(k.means.fit)

k.means.fit$cluster

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
    }
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(mun_standard, nc=5)

#the optimal choice is 4 clusters
k.means.fit <- kmeans(mun_standard, 4) #  k = 4

#disoccupazione, auto_e5_e6, occupazione_m_f, popolazione_straniera, visitatori_luoghi_cultura
par(mfrow=c(2,2))
boxplot(municip_subset$disoccupazione ~ k.means.fit$cluster, 
        xlab = "Clusters", ylab = "Disoccupazione")

#boxplot(municip_subset$auto_e5_e6 ~ k.means.fit$cluster, 
#        xlab = "Clusters", ylab = "Auto euro5, euro6")

boxplot(municip_subset$occupazione_m_f ~ k.means.fit$cluster, 
        xlab = "Clusters", ylab = "Rapporto occupazione maschile/femminile")

boxplot(municip_subset$popolazione_straniera ~ k.means.fit$cluster, 
        xlab = "Clusters", ylab = "Popolazione straniera")

boxplot(municip_subset$visitatori_luoghi_cultura ~ k.means.fit$cluster, 
        xlab = "Clusters", ylab = "Visitatori luoghi cultura")


library(cluster)
clusplot(mun_standard, k.means.fit$cluster, 
         main='2D representation of the Cluster solution',
         color=TRUE, shade=F,
         labels=0, lines=0, )

#try to cluster PROVINCES
prov %>% select(all_of(my_vars)) -> prov
prov_standard <- scale(prov)  # To standarize the variables
summary(prov_standard)

set.seed(12345)
k.means.fit <- kmeans(prov_standard, 4) # k = 4
str(k.means.fit)

k.means.fit$cluster[k.means.fit$cluster == 1]
k.means.fit$cluster[k.means.fit$cluster == 2]
k.means.fit$cluster[k.means.fit$cluster == 3]
k.means.fit$cluster[k.means.fit$cluster == 4]

#wssplot(prov_standard, nc=5)

#disoccupazione, auto_e5_e6, occupazione_m_f, popolazione_straniera, visitatori_luoghi_cultura

boxplot(prov$disoccupazione ~ k.means.fit$cluster, 
        xlab = "Clusters", ylab = "Disoccupazione")

boxplot(prov$auto_e5_e6 ~ k.means.fit$cluster, 
        xlab = "Clusters", ylab = "Auto euro5, euro6")

boxplot(prov$occupazione_m_f ~ k.means.fit$cluster, 
        xlab = "Clusters", ylab = "Rapporto occupazione maschile/femminile")

boxplot(prov$popolazione_straniera ~ k.means.fit$cluster, 
        xlab = "Clusters", ylab = "Popolazione straniera")

boxplot(prov$visitatori_luoghi_cultura ~ k.means.fit$cluster, 
        xlab = "Clusters", ylab = "Visitatori luoghi cultura")


library(cluster)
clusplot(prov_standard, k.means.fit$cluster, 
         main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels = 3, lines=0)


################################################################################
##########         PCA
################################################################################
library(FactoMineR)
library(factoextra)

municip_subset <- scale(municip_subset)
pca <- PCA(municip_subset,  graph = FALSE)
get_eig(pca)
fviz_screeplot(pca, addlabels = TRUE, ylim = c(0, 20))

var <- get_pca_var(pca)
var
var$coord
var$contrib
var$cor

fviz_pca_var(pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

#for PROVINCES
pca <- PCA(prov_standard,  graph = FALSE)
get_eig(pca)[1:10,]
fviz_screeplot(pca, addlabels = TRUE, ylim = c(0, 30))

var <- get_pca_var(pca)
var
var$coord
var$contrib
var$cor

fviz_pca_var(pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

#economical
fviz_contrib(pca, choice = "var", axes = 1, top = 10)
#social-cultural
fviz_contrib(pca, choice = "var", axes = 2, top = 10)
#transportation
fviz_contrib(pca, choice = "var", axes = 3, top = 10)
#demographic
fviz_contrib(pca, choice = "var", axes = 4, top = 10)
#green
fviz_contrib(pca, choice = "var", axes = 5, top = 10)

ind <- get_pca_ind(pca)
ind
fviz_pca_ind(pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)


area <- c(0,2,1,0,2,1,1,2,0,0,0,2,0,2,2,2,2,0,0,0,0,0,0,0,0,1,2,0,2,0,2,0,1,2,1,0,1,2,2,1,0,2,1,1,2,0,1,0,2,1,2,1,1,2,1,0,0,2,2,2,0,2,0,0,0,2,0,2,2,1,1,1,2,1,1,2,0,1,0,2,0,2,1,2,1,2,0,0,0,2,1,0,2,0,0,1,1,2,0,0,2,2,2,2,1,2,2,2,2,2,0,2,0,1)
length(area)
dim(prov_standard)
prov_pca <- cbind(area, prov_standard)
prov_pca <- as.data.frame(prov_pca)
prov_pca$area <- as.factor(prov_pca$area)

pca <- PCA(prov_pca[,-1], graph = FALSE)
fviz_pca_ind(pca,
             label = "none", # hide individual labels
             habillage = prov_pca$area, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE # Concentration ellipses
)


#MAP
library(maptools)
library(ggplot2)
library(ggalt)
library(ggthemes)
library(tibble)
library(viridis)

# get italy region map
italy_map <- map_data("italy")

# your data will need to have these region names
print(unique(italy_map$region))

# we'll simulate some data for this
#set.seed(1492)
choro_dat <- data_frame(region=unique(italy_map$region),
                        value=sample(100, length(region)))

clusters <- c(0,4,2,2,2,2,4,2,2,2,
              2,2,2,2,2,2,2,2,2,1,
              2,1,2,2,2,4,4,2,4,2,
              4,2,2,2,2,2,4,2,4,0,
              0,4,2,4,4,4,4,2,4,4,
              4,2,4,4,4,4,4,4,4,0,
              4,4,1,4,3,4,3,4,3,3,
              3,3,3,4,1,3,3,3,3,3,
              4,4,3,2,3,3,3,3,3,3,
              3,3,3,3,4)

choro_dat$value <- clusters

# we'll use this in a bit
italy_proj <- "+proj=aea +lat_1=38.15040684902542
+lat_2=44.925490198742295 +lon_0=12.7880859375"

gg <- ggplot()

# lay down the base layer
gg <- gg + geom_map(data=italy_map, map=italy_map,
                    aes(long, lat, map_id=region),
                    color="#b2b2b2", size=0.1)
# fill in the regions with the data
gg <- gg + geom_map(data=choro_dat, map=italy_map,
                    aes(fill=value, map_id=region),
                    color="#b2b2b2", size=0.1)


gg <- gg + scale_fill_viridis(name="Value")

gg

