################################################################################
##########                 Statistical Learning Project              ###########
##########                 Andrea Pio Cutrera - 965591               ###########
##########                   Municipalities in Italy                 ###########
################################################################################

##################### load all the packages required
library(dplyr)
library(factoextra)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(leaps)
library(olsrr)
library(car)
library(ggpubr)
library(Metrics)
library(gridExtra)
library(class)
library(ISLR)
library(caret)
####################

#read the csv I prepared in the first preprocessing and cleaning
################################################################################
setwd('/Users/andreacutrera/Desktop/Municipalities-Provinces')

prov <- read.csv('prov.csv', sep = ',', header = T)
municip <- read.csv('municipalities.csv', sep = ',', header = T)
ranking <- read.csv('rank.csv', header = T)

#in all of them delete the first column
municip <- municip[,-1]
ranking <- ranking[,-1]
prov <- prov[,-1]

rownames(prov) <- prov$codice_provincia
prov <- prov[,-1]

#now my dataset is made up by a bunch of int-num variables (58)
#114 Provinces (observations for 2011)
str(prov)
dim(prov)
rownames(prov)
colnames(prov)
################################################################################
my_vars <- c('provincia', 'variazione_pop_residente', 
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
my_vars
length(my_vars)

#subset of PROVINCES with variables choosen
prov %>% select(all_of(my_vars)) -> prov_subset 

str(prov)
str(prov_subset)

#59 vs 36 variables --------> 114 PROVINCES
dim(prov)
dim(prov_subset)

# now I add the ranking and the response variable from the other source
dataset <- merge(prov_subset, ranking, by = 'provincia')
str(dataset)
dim(dataset)

################################################################################
###########      SUPERVISED
################################################################################

ggplot(dataset, aes(x=life_quality))+
  geom_histogram(color = '#186D6F', fill = '#31BBBF') +
  xlab("Quality of Life Score") +
  ylab("Number of Countries")

shapiro.test(1/(dataset$life_quality))
shapiro.test((dataset$life_quality)**(1/2))
shapiro.test(log(dataset$life_quality))
shapiro.test((dataset$life_quality)**2)

#FULL model of regression -----> Adj. R2 = 0.7778
full.model <- lm(life_quality~.-provincia-rank, data = dataset)
summary(full.model)

#try with cross validation - Quite high error ---> THE FULL MODEL
set.seed(12345)
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(life_quality~.-provincia-rank, data = dataset, 
               method = "lm",
               trControl = train.control)
# Summarize the results
print(model)

#train-test split
set.seed(12345)

#deleting the names and the ranking
#2:36 are the regressors
#38 is the response
train = sample(1:nrow(dataset), 0.7*nrow(dataset))
train_set = dataset[train, -c(1, 37)]
test_set = dataset[-train, -c(1, 37, 38)]

test_target <- dataset[-train, 38]

full.model <- lm(life_quality~., data = train_set)
summary(full.model)

p <- predict(full.model, test_set)
root_mse_full_model = rmse(test_target, p)
#30.48 The mean squared error
root_mse_full_model

#STEPWISE FORWARD search - model selection
regfit.fwd=regsubsets(life_quality~.-rank-provincia,data=dataset,method="forward", nvmax=10)
summary(regfit.fwd)


#MODEL with 5 variables selected by most of the criteria, even the BIC
#forward search selects these variables
#disoccupazione, auto_e5_e6, occupazione_m_f, popolazione_straniera, visitatori_luoghi_cultura
reg.summary<-summary(regfit.fwd)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",types="l")
plot(reg.summary$rsq,xlab="Number of Variables",ylab="RSQ",types="l")
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",types="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="ADJ_Rsq",types="l")
plot(reg.summary$cp,xlab="Number of Variables",ylab="cp",types="l")

#to see them combined
par(mfrow=c(2,2))
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",types="l")
plot(reg.summary$cp,xlab="Number of Variables",ylab="cp",types="l")
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",types="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="ADJ_Rsq",types="l")

#train my model with the 5 variables selected by forward search ---> ADJ. R2 = 0.75
fwd.model <- lm(life_quality~disoccupazione+auto_e5_e6+occupazione_m_f+popolazione_straniera+visitatori_luoghi_cultura,data = train_set)
summary(fwd.model)

#compute predictions
p <- predict(fwd.model, test_set)
root_mse_fwd_model = rmse(test_target, p)
#22.06 The mean squared error ---- decreased
root_mse_fwd_model

#put together the 2 rmse to show it in the paper
as.data.frame(cbind(root_mse_full_model, root_mse_fwd_model))


#check residuals --------------> They are normally distributed
ols_test_normality(fwd.model)
#par(mfrow=c(1,2))
plot_1 <- ols_plot_resid_qq(fwd.model)
plot_2 <- ols_plot_resid_hist(fwd.model)
grid.arrange(plot_1, plot_2, nrow = 1)

#try with cross validation --- variables selected
#I had an attempt to see how a 10-fold cv can help us estimating the rmse
#it is about 23%, the same as the one used in the train test split 
#given that the model with all the data has less bias I train the model of regression
#with all observations
set.seed(12345)
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(life_quality~disoccupazione+auto_e5_e6+occupazione_m_f+
                 popolazione_straniera+visitatori_luoghi_cultura, 
               data = dataset, 
               method = "lm",
               trControl = train.control)
# Summarize the results ----> very close to the one found before
print(model)

#FINAL REGRESSION MODEL ---- I train it with all my observations

#MODEL with 5 variables selected by most of the criteria, even the BIC
#forward search selects these variables
#disoccupazione, auto_e5_e6, occupazione_m_f, popolazione_straniera, visitatori_luoghi_cultura

fwd.model <- lm(life_quality~disoccupazione+auto_e5_e6+occupazione_m_f+popolazione_straniera+visitatori_luoghi_cultura,data = dataset)
summary(fwd.model)


##### CAN I MAKE A prediction on the MUNICIPALITIES -- Yes, we can!
my_vars[1] <- "comune"
municip %>% select(all_of(my_vars)) -> mun_subset
#7921 MUNICIPALITIES ---- 36 variables
dim(mun_subset)

predictions <- fwd.model %>% predict(mun_subset)

#save the predictions with the label of the MUNICIPALITY
quality_of_life_municipality <- as.data.frame(cbind(mun_subset[,1], round(predictions, 2)))

#char in numeric
str(quality_of_life_municipality)

quality_of_life_municipality$V2 <- as.numeric(quality_of_life_municipality$V2)
str(quality_of_life_municipality)
names(quality_of_life_municipality) <- c("comune", "life_quality")

#now load the geography
geo <- read.csv("/Users/andreacutrera/Desktop/Municipalities-Provinces/geography.csv", sep = ";")
names(geo)[3] <- "comune"

geo_mun <- merge(quality_of_life_municipality, geo, by = "comune")

str(geo_mun)

#descriptive statistics of the predicted score for quality of life
summary(geo_mun$life_quality)
min(geo_mun$life_quality)
max(geo_mun$life_quality)

rownames(geo_mun) <- geo_mun$comune
Boxplot( geo_mun$life_quality , data = geo_mun, color = 'blue', 
        id=list(labels=rownames(geo_mun)), location = 'avoid')

geo_mun$life <- cut(geo_mun$life_quality, breaks = c(150,400,480,500,570),
                    labels = c('low', 'medium', 'high', 'very_high'))

#distribution between the 4 levels of my categorization
str(geo_mun)
table(geo_mun$life)

#CATEGORY of geographical area
geo_mun$geo <- as.factor(geo_mun$geo)
str(geo_mun)

#Region comparison between scores
ggboxplot(geo_mun, x = "deominazione_geo", y = "life_quality", color = "geo", 
          legend = "none") +
  geom_hline(yintercept = mean(geo_mun$life_quality), linetype = 2)+ # Add horizontal line at base mean
  ylim(150, 600)+
  xlab("Macro Area")+
  ylab("Life Quality Index")

#CATEGORY of REGIONAL area
geo_mun$Codice_Regione <- as.factor(geo_mun$Codice_Regione)
str(geo_mun)

geo_mun <- arrange(geo_mun, life_quality)

#ASCENDING order, FROM LOW ------------------> HIGH quality of life score
ggboxplot(geo_mun, x = "Denominazione_Regione", y = "life_quality", color = "geo", 
          legend = "none", x.text.angle = 60) +
  geom_hline(yintercept = mean(geo_mun$life_quality), linetype = 2)+ # Add horizontal line at base mean
  ylim(150, 600)+
  xlab("Region") 

#reversed axis
ggboxplot(geo_mun, x = "Denominazione_Regione", y = "life_quality", color = "Denominazione_Regione", 
          legend = "none", sort.val = "desc", sort.by.groups = FALSE,
          x.text.angle = 90, rotate = TRUE, ggtheme = theme_minimal()) +
  geom_hline(yintercept = mean(geo_mun$life_quality), linetype = 2)+ # Add horizontal line at base mean
  ylim(150, 600)+
  xlab("Region") 

#frequencies of low-medium-high-very_high municipalities for area
geo_mun_for_bar <- geo_mun %>% 
  group_by(deominazione_geo, life) %>%
  summarise(n=n()) %>%
  mutate(freq=n/sum(n))
geo_mun_for_bar <- arrange(geo_mun_for_bar, life)

#Share of low-medium-high-very_high municipalities------AREA
ggplot(geo_mun_for_bar, aes(x=reorder(deominazione_geo,freq), fill=life, group=life)) +
  geom_bar(aes(y=freq), stat = 'identity', position = 'dodge')+
  xlab('Group areas')+
  ylab("Relative frequency")


geo_mun_for_bar <- geo_mun %>% 
  group_by(Denominazione_Regione, life) %>%
  summarise(n=n()) %>%
  mutate(freq=n/sum(n))

#Share of low-medium-high-very_high municipalities------REGIONS
ggplot(geo_mun_for_bar, aes(x=reorder(Denominazione_Regione,freq), fill=life, group=life)) +
  geom_bar(aes(y=freq), stat = 'identity', position = 'dodge')+
  coord_flip()+
  ylab("Relative frequency by group")+
  xlab("Regions")

#to see the regions' names
unique(geo_mun_for_bar$Denominazione_Regione)

#PALETTE FOR CLASSIFICATION
#c("#EC7063", "#F4D03F", "#52BE80", "#5DADE2")
#THESE REGIONS have only LOW and MEDIUM classificated municipalities
#"Sicilia", "Puglia", "Campania", "Basilicata", Molise", Sardegna"

a <- ggplot(subset(geo_mun_for_bar, Denominazione_Regione == "Sicilia"), aes(x=" ", y=freq, fill=life)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  ggtitle("Sicilia") +
  scale_fill_manual(values=c("#EC7063", "#F4D03F"))
b <- ggplot(subset(geo_mun_for_bar, Denominazione_Regione == "Puglia"), aes(x=" ", y=freq, fill=life)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void()+
  ggtitle("Puglia") +
  scale_fill_manual(values=c("#EC7063", "#F4D03F"))
c <- ggplot(subset(geo_mun_for_bar, Denominazione_Regione == "Campania"), aes(x=" ", y=freq, fill=life)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void()+
  ggtitle("Campania") +
  scale_fill_manual(values=c("#EC7063", "#F4D03F"))
d <- ggplot(subset(geo_mun_for_bar, Denominazione_Regione == "Basilicata"), aes(x=" ", y=freq, fill=life)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void()+
  ggtitle("Basilicata") +
  scale_fill_manual(values=c("#EC7063", "#F4D03F"))
e <- ggplot(subset(geo_mun_for_bar, Denominazione_Regione == "Sardegna"), aes(x=" ", y=freq, fill=life)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void()+
  ggtitle("Sardegna") +
  scale_fill_manual(values=c("#EC7063", "#F4D03F"))
f <- ggplot(subset(geo_mun_for_bar, Denominazione_Regione == "Molise"), aes(x=" ", y=freq, fill=life)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void()+
  ggtitle("Molise") +
  scale_fill_manual(values=c("#EC7063", "#F4D03F"))
g <- ggplot(subset(geo_mun_for_bar, Denominazione_Regione == "Calabria"), aes(x=" ", y=freq, fill=life)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void()+
  ggtitle("Calabria") +
  scale_fill_manual(values=c("#EC7063", "#F4D03F", "#52BE80", "#5DADE2"))
h <- ggplot(subset(geo_mun_for_bar, Denominazione_Regione == "Lazio"), aes(x=" ", y=freq, fill=life)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void()+
  ggtitle("Lazio") +
  scale_fill_manual(values=c("#EC7063", "#F4D03F", "#52BE80", "#5DADE2"))
i <- ggplot(subset(geo_mun_for_bar, Denominazione_Regione == "Abruzzo"), aes(x=" ", y=freq, fill=life)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void()+
  ggtitle("Abruzzo") +
  scale_fill_manual(values=c("#EC7063", "#F4D03F", "#52BE80", "#5DADE2"))

grid.arrange(a, b, c, d, e, f, g, h, i, nrow = 3)

j <- ggplot(subset(geo_mun_for_bar, Denominazione_Regione == "Umbria"), aes(x=" ", y=freq, fill=life)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void()+
  ggtitle("Umbria")+
  scale_fill_manual(values=c("#F1C40F", "#52BE80", "#5DADE2"))
k <- ggplot(subset(geo_mun_for_bar, Denominazione_Regione == "Marche"), aes(x=" ", y=freq, fill=life)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void()+
  ggtitle("Marche")+
  scale_fill_manual(values=c("#EC7063", "#F4D03F", "#52BE80", "#71A0F1"))
l <- ggplot(subset(geo_mun_for_bar, Denominazione_Regione == "Friuli-Venezia Giulia"), aes(x=" ", y=freq, fill=life)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void()+
  ggtitle("Friuli-Venezia Giulia")+
  scale_fill_manual(values=c("#EC7063", "#F4D03F", "#52BE80", "#71A0F1"))
m <- ggplot(subset(geo_mun_for_bar, Denominazione_Regione == "Liguria"), aes(x=" ", y=freq, fill=life)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void()+
  ggtitle("Liguria")+
  scale_fill_manual(values=c("#EC7063", "#F4D03F", "#52BE80", "#71A0F1"))
n <- ggplot(subset(geo_mun_for_bar, Denominazione_Regione == "Piemonte"), aes(x=" ", y=freq, fill=life)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void()+
  ggtitle("Piemonte")+
  scale_fill_manual(values=c("#EC7063", "#F4D03F", "#52BE80", "#71A0F1"))
o <- ggplot(subset(geo_mun_for_bar, Denominazione_Regione == "Veneto"), aes(x=" ", y=freq, fill=life)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  ggtitle("Veneto")+
  scale_fill_manual(values=c("#EC7063", "#F4D03F", "#52BE80", "#71A0F1"))
p <- ggplot(subset(geo_mun_for_bar, Denominazione_Regione == "Lombardia"), aes(x=" ", y=freq, fill=life)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void()+
  ggtitle("Lombardia")+
  scale_fill_manual(values=c("#EC7063", "#F4D03F", "#52BE80", "#71A0F1"))
q <- ggplot(subset(geo_mun_for_bar, Denominazione_Regione == "Toscana"), aes(x=" ", y=freq, fill=life)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void()+
  ggtitle("Toscana")+
  scale_fill_manual(values=c("#F1C40F", "#52BE80", "#5DADE2"))
r <- ggplot(subset(geo_mun_for_bar, Denominazione_Regione == "Trentino-Alto Adige/Südtirol"), aes(x=" ", y=freq, fill=life)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void()+
  ggtitle("Trentino-Alto Adige")+
  scale_fill_manual(values=c("#F1C40F", "#52BE80", "#5DADE2"))
s <- ggplot(subset(geo_mun_for_bar, Denominazione_Regione == "Emilia-Romagna"), aes(x=" ", y=freq, fill=life)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void()+
  ggtitle("Emilia-Romagna")+
  scale_fill_manual(values=c("#EC7063", "#F4D03F", "#52BE80", "#71A0F1"))
t <- ggplot(subset(geo_mun_for_bar, Denominazione_Regione == "Valle d'Aosta/Vallée d'Aoste"), aes(x=" ", y=freq, fill=life)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void()+
  ggtitle("Valle d'Aosta")+
  scale_fill_manual(values=c("#F1C40F", "#52BE80", "#5DADE2"))

grid.arrange(j, k, l, m, n, o, p, q, r, s, t, nrow = 4)

#to sum up the pies
grid.arrange(a, b, c, d, e, f, g, h, i, nrow = 3)
grid.arrange(j, k, l, m, n, o, p, q, r, s, t, nrow = 4)


################################################################################
############   KNN method for classification
################################################################################

#disoccupazione, auto_e5_e6, occupazione_m_f, 
#popolazione_straniera, visitatori_luoghi_cultura
data_for_knn <- mun_subset[,c(1,19,28,25,35,12)]
str(data_for_knn)

data_for_knn <- merge(data_for_knn, geo_mun, by = 'comune')
rownames(data_for_knn) <- data_for_knn$comune
str(data_for_knn)

data_for_knn <- data_for_knn[,-c(1,7,8,9,10,11,12,13)]
normalize <-function(x) {(x-min(x))/(max(x)-min(x))}
data <- as.data.frame(lapply(data_for_knn[,-6], normalize))
summary(data)
colSums(is.na(data))

#train set
set.seed(12345)

train = sample(1:nrow(data), 0.7*nrow(data))
train_set <- data[train,]
test_set <- data[-train,]

target_labels <- data_for_knn[train, 6]
test_labels <- data_for_knn[-train, 6]

#1-NN has an accuracy of 92.73%
knn.model <- knn(train = train_set, test = test_set, cl = target_labels, k = 1)

tab <- table(knn.model, test_labels)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
tab

#try to make cross validation with all the dataset
set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 5)
#scale data
data_for_knn[,c(1:5)] <- as.data.frame(lapply(data_for_knn[,-6], normalize))

#------------------------KNN CV-------------------------------------------------
knnFit <- train(life ~ ., data = data_for_knn, method = "knn", trControl = ctrl, preProcess = c("center","scale"),tuneLength = 20)
knnFit

#11-NN the one with best accuracy
knnFit$bestTune
plot(knnFit)

#check the accuracy with train test split
#13-NN has an accuracy of 94.12%
knn.model <- knn(train = train_set, test = test_set, cl = target_labels, k = 13)

tab <- table(knn.model, test_labels)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
tab

####################################################
#UNSUPERVISED
####################################################
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
my_vars <- c('comune', 'variazione_pop_residente', 
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
municip %>% select(all_of(my_vars)) -> municip_subset

colSums(is.na(municip_subset))
#stazioni accessibili 350 missing values
municip_subset %>% drop_na() -> municip_subset
colSums(is.na(municip_subset))


#try to cluster municipalities and provinces
rownames(municip_subset) <- municip_subset$comune
mun_standard <- scale(municip_subset[,-1]) # To standarize the variables
summary(mun_standard)

str(mun_standard)




set.seed(12345)
k.means.fit <- kmeans(mun_standard, 5) # k = 5
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


#4-means
set.seed(12345)
k.means.fit <- kmeans(mun_standard, 4) # k = 4
str(k.means.fit)

summary(municip_subset[k.means.fit$cluster==1,"vecchiaia"])

boxplot(municip_subset$vecchiaia ~ k.means.fit$cluster)

summary(mun[k.means.fit$cluster==1,"occupazione"])
summary(mun[k.means.fit$cluster==2,"occupazione"])
summary(mun[k.means.fit$cluster==3,"occupazione"])
summary(mun[k.means.fit$cluster==4,"occupazione"])
summary(mun[k.means.fit$cluster==5,"occupazione"])

boxplot(municip_subset$occupazione ~ k.means.fit$cluster)
boxplot(municip_subset$presenza_universitaria ~ k.means.fit$cluster)
boxplot(municip_subset$pendolarismo ~ k.means.fit$cluster)
boxplot(municip_subset$digital_divide ~ k.means.fit$cluster)

library(cluster)
clusplot(mun_standard, k.means.fit$cluster, 
         main='2D representation of the Cluster solution',
         color=TRUE, shade=FALSE,
         labels=0, lines=0)

#provinces
prov_standard <- scale(prov[,-1])  # To standarize the variables
summary(prov_standard)

set.seed(12345)
k.means.fit <- kmeans(prov_standard, 4) # k = 6
str(k.means.fit)

k.means.fit$cluster[k.means.fit$cluster == 1]
k.means.fit$cluster[k.means.fit$cluster == 2]
k.means.fit$cluster[k.means.fit$cluster == 3]
k.means.fit$cluster[k.means.fit$cluster == 4]

wssplot(prov_standard, nc=5)

summary(prov[k.means.fit$cluster==1,"vecchiaia"])
summary(prov[k.means.fit$cluster==2,"vecchiaia"])
summary(prov[k.means.fit$cluster==3,"vecchiaia"])
summary(prov[k.means.fit$cluster==4,"vecchiaia"])
summary(prov[k.means.fit$cluster==5,"vecchiaia"])
summary(prov[k.means.fit$cluster==6,"vecchiaia"])

boxplot(prov$vecchiaia ~ k.means.fit$cluster)

summary(prov[k.means.fit$cluster==1,"occupazione"])
summary(prov[k.means.fit$cluster==2,"occupazione"])
summary(prov[k.means.fit$cluster==3,"occupazione"])
summary(prov[k.means.fit$cluster==4,"occupazione"])
summary(prov[k.means.fit$cluster==5,"occupazione"])
summary(prov[k.means.fit$cluster==6,"occupazione"])

boxplot(prov$occupazione ~ k.means.fit$cluster)

boxplot(prov$presenza_universitaria ~ k.means.fit$cluster)
boxplot(prov$pendolarismo ~ k.means.fit$cluster)
boxplot(prov$digital_divide ~ k.means.fit$cluster)

boxplot(prov$abbandono_scolastico_2grado ~ k.means.fit$cluster)
boxplot(prov$anziani_soli~ k.means.fit$cluster)
boxplot(prov$suicidio~ k.means.fit$cluster)
boxplot(prov$affollamento_abitazioni~ k.means.fit$cluster)
boxplot(prov$gini~ k.means.fit$cluster)
boxplot(prov$occupazione_m_f~ k.means.fit$cluster)
boxplot(prov$occupazione_ita_straniera~ k.means.fit$cluster)

library(cluster)
clusplot(prov_standard, k.means.fit$cluster, 
         main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels = 3, lines=0)

for(k in 2:6){
  k.m.fit <- kmeans(prov_standard, k) # k = 3
  clusplot(prov_standard, k.m.fit$cluster, 
           main=sprintf("2D representation of the Cluster solution\n k = %d",k),
           color=TRUE, shade=TRUE,
           labels=2, lines=0)
}

#trying pca on MUNICIPALITIES
rownames(municip_subset) <- municip_subset$comune
mun_standard <- scale(municip_subset[,-1]) # To standarize the variables
summary(mun_standard)


pca <- prcomp(municip_subset[,-1], scale. = T)
fviz_eig(pca)

fviz_pca_ind(pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#trying pca on MUNICIPALITIES which are main Province
cols_to_remove <- c("provincia", "rank", "life_quality")
dataset <- dataset[, !(colnames(dataset) %in% cols_to_remove)]
pca <- prcomp(dataset, scale. = T)
fviz_eig(pca)

fviz_pca_ind(pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


