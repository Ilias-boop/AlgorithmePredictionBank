#########################################################
## ------------------ R script ----------------------- ##
#########################################################


## Libraries
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(dplyr)
library(corrplot)
library(psy)
library(pheatmap)
library(rpart)
library(rpart.plot)
library(prettyR)
library(randomForest)

# importer la data
data <- read.csv(file="C:/Users/AKIL/Desktop/FouilleDonnées2/bank-additional/bank-additional-full.csv", header=TRUE, sep=";")

#######################################################################################
## ------------------ Statistique descriptive & Explorations ----------------------- ##
#######################################################################################

## Observation de la variable Y 
summary(data$y)
tabY <- table(data$y)
tabY

colors <- c("yellow2","olivedrab3")
pie(table(data$y), 
    labels = paste(round(prop.table(table(data$y))*100,2), "%", sep = ""), 
    col = colors,
    main = "Les clients ont-il un compte à terme ?")
legend(.9, .1, c("Yes", "No"), cex = 0.9, fill = colors)

## Observation de la variable âge
summary(data$age)

boxplot(data$age, 
        col="olivedrab2",
        ylab='Age des clients',
        xlab="Distribution des clients selon l'âge")

age <- data$age
hist(age, prob=TRUE,
     col = "olivedrab2",
     xlab='Distribution des âges des clients',
     ylab='Nombre de clients',
     main="Distribution des clients selon leur âge")
curve(dnorm(x, mean=mean(age), sd=sd(age)), add=TRUE)

foo <- data$age
hist(foo, prob=TRUE)
curve(dnorm(x, mean=mean(foo), sd=sd(foo)), add=TRUE)

qqnorm(data$age); qqline(data$age)


## Observation de la variable marital
tab <- round(prop.table(table(data$marital))*100,2)
tab

colors <- c("yellow2","olivedrab3", "cadetblue2")

pie(tab, 
    labels = paste(round(prop.table(table(data$marital))*100,2), "%", sep = ""), 
    col = colors,
    main = "Statut matrimonial des clients")

legend(-0.2, 1, c("Divorcé", "Marié", "Célibataire"), cex = 0.9, fill = colors)


labels <- c("Divorcé", "Marié", "Célibataire")
colors <- c("yellow2","olivedrab3", "cadetblue2")

par(mfcol = c(1, 2))
for (i in 1:1) { 
  barplot(tab)
  pie(tab, labels=labels, colors=colors)
  legend(.9, .1, c("Divorcé", "Marié", "Célibataire"), cex = 0.9, fill = colors)
}

tabMar <- table(data$marital)
tabMar

summary(data$marital)



## Observation de la variable education
tabEdu <- round(prop.table(table(data$education))*100,2)
tabEdu

colors <- c("yellow2",    #1
            "olivedrab3", #2
            "aliceblue",  #3
            "bisque2",    #4
            "cadetblue2", #5
            "blueviolet", #6
            "aquamarine3" #7
) 

pie(tabEdu, 
    labels = paste(round(prop.table(table(data$education))*100,2), "%", sep = ""), 
    col = colors,
    main = "Niveau de formation des clients")

legend(.25, .05, c("Bases : 4 ans", "Bases : 6 ans", "Bases ; 9 ans", "Lycée", "Illétré", "Education professionnelle", "Diplôme universitaire"), cex = 0.9, fill = colors)

par(mfcol = c(1, 2))
for (i in 1:1) {  
  barplot(tab)
  pie(tab)
}

## Observation de la variable default
tabDef <- table(data$default, useNA='always')
tabDef

summary(tabDef)

tabDef = prop.table(tabDef)
tabDef = round(tabDef*100,2)
tabDef

colors <- c("yellow2","olivedrab3")
pie(tabDef, 
    labels = paste(round(prop.table(table(data$default))*100,2), "%", sep = ""), 
    col = colors,
    main = "Les clients ont-il un défaut de paiement ?")
legend(.9, .1, c("Yes", "No"), cex = 0.9, fill = colors)


## Observation de la variable housing
summary(data$housing)

tabHouse <- table(data$housing)
tabHouse

colors <- c("yellow2","olivedrab3")
pie(tabHouse, 
    labels = paste(round(prop.table(table(data$housing))*100,2), "%", sep = ""), 
    col = colors,
    main = "Les clients ont-il un crédit immobilier ?")
legend(-0.25, 1.08, c("Yes", "No"), cex = 0.9, fill = colors)

tabHouse = prop.table(tabHouse)
tabHouse = round(tabHouse*100,1)
tabHouse

#Observation de la variable Loan
tabLoan <- table(data$loan)
tabLoan

summary(tabLoan)

colors <- c("yellow2","olivedrab3")
pie(tabLoan, 
    labels = paste(round(prop.table(table(data$loan))*100,2), "%", sep = ""), 
    col = colors,
    main = "Les clients ont-il un prêt personnel en cours ?")
legend(-0.2, 1.07, c("Yes", "No"), cex = 0.9, fill = colors)

tabLoan = prop.table(tabLoan)
tabLoan = round(tabLoan*100,1)
tabLoan

#Observation de la variable Contact
tabContact <- table(data$contact)
tabContact

tabContact = prop.table(tabContact)
tabContact = round(tabContact*100,1)
tabContact

colors <- c("yellow2","olivedrab3")
pie(tabContact, 
    labels = paste(round(prop.table(table(data$contact))*100,2), "%", sep = ""), 
    col = colors,
    main = "Sur quel  numéro de téléphone contacter le client ?")
legend(-0.2, 1.07, c("Portable", "Fixe"), cex = 0.9, fill = colors)

par(mfcol = c(1, 2))
for (i in 1:1) {  
  barplot(tabContact)
  pie(tabContact)
}

#Observation de la variable Day
tabDay <- table(data$day)
tabDay = prop.table(tabDay)
tabDay = round(tabDay*100,1)
barplot(tabDay)

#Observation de la variable Month
tabMonth <- table(data$month)
tabMonth = prop.table(tabMonth)
tabMonth = round(tabMonth*100,1)
barplot(tabMonth)

#Observation de la variable Duration
tabDur <- table(data$duration)
summary(tabDur)

summary(data$duration)

dur <- data$duration
hist(dur, prob=TRUE,
     col = "olivedrab2",
     xlab='Distribution de la durée des clients',
     ylab='Nombre de clients',
     main="Distribution des clients selon la durée")
curve(dnorm(x, mean=mean(dur), sd=sd(dur)), add=TRUE)

qqnorm(dur); qqline(dur)

#Observation de la variable Campaign
tabCamp <- table(data$campaign)
summary(data$campaign)

par(mfcol = c(1, 2))
for (i in 1:1) {  
  hist(data$campaign)
  boxplot(data$campaign)
}

camp <- data$campaign
hist(camp, prob=TRUE,
     col = "olivedrab2",
     xlab='La dernière campagne',
     ylab='Nombre de clients',
     main="La distribution des valeurs pour la dernière campagne")
curve(dnorm(x, mean=mean(camp), sd=sd(camp)), add=TRUE)

qqnorm(camp); qqline(camp)

plot(data$campaign, type="h")

#Observation de la variable Pdays
summary(data$pdays)
info1 <- (data$pday==999)

summary(info1)

#Observation de la variable Previous
summary(data$previous)
plot(data$previous)
hist(data$previous)
qqnorm(data$previous); qqline(data$previous)

info <- (data$previous=="0")
summary(info)

#Observation de la variable Poutcome
summary(data$poutcome)
tabOut <- table(data$poutcome)
tabOut

tabOut = prop.table(tabOut)
tabOut = round(100*tabOut,2)
tabOut

## Observation de la variable Job
# Les données, proportionnellement et en %
round(prop.table(table(data$job))*100,2)

# Piechart
colors <- c("aliceblue",  #1
            "bisque2",    #2
            "blueviolet", #3
            "brown",      #4
            "aquamarine3",#5
            "cadetblue2", #6
            "burlywood3", #7
            "coral",      #8
            "darkcyan",   #9
            "darkgoldenrod1", #10
            "darkolivegreen2",  #11
            "darkred"     #12
)
pie(table(data$job), 
    labels = paste(round(prop.table(table(data$y))*100,2), "%", sep = ""), 
    col = colors, main = "Les situations professionnelles des clients")
legend(.9, .1, c("admin", 
                 "retired", 
                 "unemployed", 
                 "blue-collar", 
                 "self-employed", 
                 "entrepreneur", 
                 "services", 
                 "housemaid", 
                 "student", 
                 "management", 
                 "technician"),
       cex = 0.9, fill = colors)



#####################################################################
## ------------------ Nettoyage de données ----------------------- ##
#####################################################################

# Nettoyage de données: valeurs manquantes
table(data$job) # unknown=330
table(data$education) # unknown=1731

table(bank_addi_full$poutcome)

# eliminer la variable poutcome (80% de unknown)
data <- data[,-15]

# eliminer la variable duration
data <- data[,-11]

# eliminer les lignes avec des valeurs unknown dans les autres variables
data <- data[!apply(data, 1, function(r) any(r %in% c("unknown"))),]


#Transformer la variable numérique pdays en variable catégorielle
data$pdays <- as.factor(ifelse(data$pdays < 7, "Moins d'une semaine",
                               ifelse(data$pdays < 20, 'Entre une semaine et 20 jours', 
                                      ifelse(data$pdays < 999, 'Plus de 20 jours', 
                                             ifelse(data$pdays == 999, 'Non contacté', 'E')))))


# verification
table(data$job) 
table(data$education)
table(data$pdays)

# variable cible
round(prop.table(table(data$y)),3)


## Définir un échantillon d'entrainement et de test

# convertir les variables char en factor
data <- data %>% mutate_if(is.character,as.factor)
summary(data)

set.seed(1)
# 80% pour l'entrainement and 20% pour le test
smp_size <- floor(0.80 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]



#######################################################################################
## ------------------ Tests statistiques de dépendance et corrélation -------------- ##
#######################################################################################

## Matrices & cercles de corrélation, & hypersphère

var2 <- c("age", "duration", "pdays", "previous")
corrplot(cor(data[,var2], use="complete.obs", method = "spearman"), 
         type="lower", 
         order = 'hclust', 
         addCoef.col = 'white', 
         cl.ratio = 0.2, 
         bg="olivedrab2" )


var = c("emp.var.rate", "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed", "age", "campaign", "pdays", "previous")
mdspca(data[,var])
sphpca(data[,var])

cha <- hclust(dist(t(scale(data[,var]))), method="ward.D2")
plot(cha, xlab=" ", ylab ="", main ="Classification hierarchique")
obj <- cor(data[,var], use="pairwise.complete.obs")

##Test du Khi-2 pour les variables catégorielles
##Cette démarche est répétée pour l'ensemble des variables catégorielles.

# Réaliser le tableau de contingence
tabYJob <- table(data$job, data$y)

#Effectuer le test du Khi2
khi <- chisq.test(tabYJob, correct=FALSE)

#Préparation d'une visualisation en plus, pour la distribution du tableau de contingence
tabYJobBar = t(tabYJob)
barplot(tabYJobBar, horiz = TRUE, beside=TRUE, xlab="Professions", legend=colnames(tabYJob))

# impression du résultat du test du khi-2
print(khi)
# impression du tableau de contingence
print(tabYJob)


##Visualisation de la normalité de la distribution
##Ce code a été répété sur chacune des variables quantitatives.

# cibler la variable
camp <- data$campaign

#histogramme avec ligne de l'évolution des moyennes et écart-type
hist(camp, prob=TRUE,
     col = "olivedrab2",
     xlab='La dernière campagne',
     ylab='Nombre de clients',
     main="La distribution des valeurs pour la dernière campagne")
curve(dnorm(x, mean=mean(camp), sd=sd(camp)), add=TRUE)

#visualisation de la distribution réelle face à une distribution théorique : si les lignes se suivent, la distribution de la variable suit une loi normale.
qqnorm(camp); qqline(camp)

##Test de Wilcoxon
## Ce test est répété pour chacune des variables catégorielles.

#Sélectionner la donnée et réaliser le test
montest=wilcox.test(duration~y, data=data)
#voir les résultats du test
montest

##Comparaison des comportements des distributions des variables quantitatives
##Ce test est répété pour chacune des variables catégorielles.

colors <- c("yellow2","olivedrab3")
boxplot(data$previous~data$y, 
        col = colors, 
        ylab="previous", 
        xlab="Présence d'un compte à terme",
        main = "Comportement des distributions des groupes selon y")


##################################################################
## ------------------ Modélisations & Prédiction -------------- ##
##################################################################


## Modèles choisis: Abre de décision, Random Forest et Logistic regression

## I. ARBRE DE DECISION ##

#construction de l'arbre
data.Tree <- rpart(y ~., 
                   data=data.train, 
                   method = "class",
                   control=rpart.control(minsplit = 5, cp =0))

#Affichage du résultat
plot(data.Tree, uniform=TRUE, branch=0.5, margin=0.1)
text(data.Tree, all=FALSE, une.n=TRUE)

#Elagage
plotcp(data.Tree)

#Affichage du cp optimal
print(data.Tree$cptable[which.min(data.Tree$cptable[,4]),1])

data.Tree_Opt <- prune(data.Tree,cp=data.Tree$cptable[which.min(data.Tree$cptable[,4]),1])

prp(data.Tree_Opt,extra=1)

### VALIDATION ###

#Prédiction du modèle sur les données de test
data.test_Predict <- predict(data.Tree_Opt,newdata=data.test, type= "class")

#Matrice de confusion
mc<-table(data.test$y,data.test_Predict)
print(mc)

mc <- round(prop.table(mc)*100,2)
mc

fourfoldplot(mc, 
             color = c("red", "green"),
             conf.level = 0,
             margin = 1,
             main = "matrice de confusion de l'arbre de décision")

#Erreur de classement
erreur.classement<-1.0-(mc[1,1]+mc[2,2])/sum(mc)
print(erreur.classement)

#Taux de prédiction
prediction=mc[2,2]/sum(mc[2,])
print(prediction)

#Accuracy
accuracy=(mc[1,1])+(mc[2,2])/sum(mc)
print(accuracy)

#Recall
recall=mc[1,2]/(mc[2,1]+mc[2,2])
print(recall)

#Affichage des règles de construction de l'arbre
print(data.Tree_Opt)

###Calcul d'erreur par validation croisée###
n <- nrow (data)
K <- 20

#Tirer d'une permutation
alea=runif(n)
rang=rank(alea)
rang[1:20]

#Taille de chaque échantillon
taille <- n%/%K
taille

#Composition des blocs
blocs=(rang-1)%/%taille+1
table(blocs)
blocs[blocs==21]=20
blocs=as.factor(blocs)

#Calcul de l'erreur
error.cv=numeric(0)
for(k in 1:K){
  arbre=rpart(y~., data=data[blocs!=k,], method="class")
  pred=predict(object = arbre, newdata=data[blocs==k,], type="class")
  mc <- table(data$y[blocs==k], pred)
  err=1-((mc[1,1]+mc[2,2])/sum(mc))
  error.cv=rbind(error.cv,err)
}
error.cv
mean(error.cv)

## Paramètre de complexité ##
printcp(data.Tree_Opt)
data.Tree_Opt$cptable
plotcp(data.Tree_Opt)



## II. Random Forest ##

# L'algorithme Random Forest  
data2_RandomForest <- randomForest(y~.,
                                   data=train, 
                                   ntree = 500, ## paramètre du nombre de forêts
                                   mtry = 2,    ## paramètre du nombre d'essais
                                   na.action = na.omit)
# Résultats de l'algorithme
print(data2_RandomForest)

# Les éléments "out of bag"
hist(data2_RandomForest$oob.times, main="distribution des éléments 'out of bag' de la classification")

#Répartition des votes pour chaque individu
data2_RandomForest$votes[1:10,]

#Importance de chaque variable dans le modèle aléatoire
varImpPlot(data2_RandomForest, main="importance des variables dans la classification")

##PREDICTION RANDOM FOREST##
test$predicted <- predict(data2_RandomForest, test)

#Matrice de confusion du résultat du test de prédiction
table <- table(test$predicted, test$y)
table <- round(prop.table(table)*100,2)
fourfoldplot(table, 
             color = c("red", "green"),
             conf.level = 0,
             margin = 1,
             main = "matrice de confusion de la forêt aléatoire")

#Erreur de classement
erreur.classement<-1.0-(table[1,1]+table[2,2])/sum(table)
print(erreur.classement)

#Taux de prédiction
prediction=table[2,2]/sum(table[2,])
print(prediction)

########################################
#### RANDOM FOREST #### AMELIORATIONS

#Vérifier que j'ai bien toutes les variables #
colnames(data)
data <- data[,-11]

#Transformer la variable numérique pdays en variable catégorielle
data$pdays <- as.factor(ifelse(data$pdays < 7, "Moins d'une semaine",
                               ifelse(data$pdays < 20, 'Entre une semaine et 20 jours', 
                                      ifelse(data$pdays < 999, 'Plus de 20 jours', 
                                             ifelse(data$pdays == 999, 'Non contacté', 'E')))))


#Vérifier les types des variables
str(data)


#Remettre en quantitatives celles qui sont indiquées numériques
#data[, i] <- as.numeric(as.character(data[, i]))
data <- data %>% mutate_if(is.character, as.factor)
# + normaliser si besoin

#RandomForest avec toutes les variables et modalités par défaut
set.seed(123)

library(randomForest)
fit <- randomForest(y ~ ., data = data, na.action = na.roughfix)

#Consultation du résultat
print(fit)

# L'importance des variables sélectionnées par l'algorithme
varImpPlot(fit,
           sort = TRUE,
           n.var = 21,
           main = "Le classement des variables d'importance pour la randomForest")

fit$importance[order(fit$importance, decreasing = TRUE),]

plot(y ~ duration, data = data)
plot(y ~ euribor3m, data = data)
plot(y ~ age, data = data)
plot(y ~ job, data = data)

##Attention : nous ne mesurons pas la diminution du taux d'erreur,
# ni la diminution du taux "accuracy". 
# Il s'agit d'observer la diminution dans la pureté du noeud décisionnel. 

importanceOrder=order(-fit$importance)
names=rownames(fit$importance)[importanceOrder][1:19] ##je ne regarde que les 10 1ère sur 21
par(mfrow=c(5, 3), xpd=NA)

for (name in names) 
  partialPlot(fit, 
              data, 
              eval(name), 
              main=name, 
              xlab=name,
              ylim=c(-.7,.19)
  )

par(mfrow=c(1,1), xpd=NA)
library(rpart)
fit=rpart(factor(y)~., data)
plot(fit)
text(fit)

tmp=rownames(fit$splits)
allVars=colnames(attributes(fit$terms)$factors)  
rownames(fit$splits)=1:nrow(fit$splits)
splits=data.frame(fit$splits)
splits$var=tmp
splits$type=""
frame=as.data.frame(fit$frame)
index=0

for(i in 1:nrow(frame)){
  if(frame$var[i] != "<leaf>"){
    index=index + 1
    splits$type[index]="primary"
    if(frame$ncompete[i] > 0){
      for(j in 1:frame$ncompete[i]){
        index=index + 1
        splits$type[index]="competing"}}
    if(frame$nsurrogate[i] > 0){
      for(j in 1:frame$nsurrogate[i]){
        index=index + 1
        splits$type[index]="surrogate"}}}}

splits$var=factor(as.character(splits$var))
splits=subset(splits, type != "surrogate")

out=aggregate(splits$improve,
              list(Variable = splits$var),
              sum, na.rm = TRUE)
allVars=colnames(attributes(fit$terms)$factors)

if(!all(allVars %in% out$Variable)){
  missingVars=allVars[!(allVars %in% out$Variable)]
  zeros=data.frame(x = rep(0, length(missingVars)), Variable = missingVars)
  out=rbind(out, zeros)}

out2=data.frame(Overall = out$x)
rownames(out2)=out$Variable
out2

barplot(out2, col = "cadetblue")

VI_T=out2
barplot(unlist(VI_T/sum(VI_T)), 
        names.arg=1:10)

barplot(t(VI_F/sum(VI_F)))

### Choisir la valeur ntree ###
plot(fit$err.rate[, 1], type = "l", xlab = "nombre d'arbres", ylab = "erreur OOB")

### Le nombre de forêts sur uniquement les variables sélectionnées ##
dataSelect <- subset(data, select = c(
  age,
  emp.var.rate,
  euribor3m,
  nr.employed,
  pdays,
  poutcome,
  previous,
  y)
) #les variables sélectionnées

## Test arbres : 1800 - 1550 - 1200
set.seed(123)
fit <- randomForest(y ~ ., 
                    data = dataSelect, 
                    ntree = 1200, 
                    mtry = 7, 
                    na.action = na.roughfix)
print(fit)

plot(fit$err.rate[, 1],
     col = 'darkgreen',
     type = "l",
     xlab = "nombre d'arbres", 
     ylab = "erreur OOB")

### OPTIMISER LES ntry #### de 1 à 7
# Echec d'installation des packages CARET et RECIPES qui auraient 
# permis une optimisation comparée dans un seul tableau R.
set.seed(123)
fit <- randomForest(y ~ ., 
                    data = dataSelect, 
                    ntree = 1550,
                    mtry = 7,
                    na.action = na.roughfix)
print(fit)

#Commentaire : Nous conservons 1550 arbres et 7 variables (ntry).

##PREDICTION RANDOM FOREST##
#Séparation train / test en 80/20
train <- dataSelect %>% sample_frac(0.8)
test <- anti_join(dataSelect, train)

# Algorithme Random Forest  
dataSelect_RandomForest <- randomForest(y~.,
                                        data=train, 
                                        ntree = 1550, 
                                        mtry = 7, 
                                        na.action = na.omit)

test$predicted <- predict(dataSelect_RandomForest, test)

#Matrice de confusion du résultat du test de prédiction
table <- table(test$predicted, test$y)

table <- round(prop.table(table)*100,2)
fourfoldplot(table, 
             color = c("red", "green"),
             conf.level = 0,
             margin = 1,
             main = "matrice de confusion de la forêt aléatoire")


#Erreur de classement
erreur.classement<-1.0-(table[1,1]+table[2,2])/sum(table)
print(erreur.classement)

#Taux de prédiction
prediction=table[2,2]/sum(table[2,])
print(prediction)

#Accuracy
accuracy=(table[1,1])+(table[2,2])/sum(table)
print(accuracy)

#Recall
recall=table[1,2]/(table[2,1]+table[2,2])
print(recall)




## III. Régression Logistique
# 1er modèle: utiliser tous les prédicteurs
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE)
glm1 <- train(y ~., data = train,
              method = "glm",
              trControl = ctrl)
summary(glm1)


#Matrice de confusion et visualisation
pred_log <- predict(glm1, newdata = test)
confusionMatrix(pred_log, test$y)

mc<-table(test$y,pred_log)
print(mc)

fourfoldplot(mc, 
             color = c("red", "green"),
             conf.level = 0,
             margin = 1,
             main = "matrice de confusion de la régression logistique")

#Erreur de classement
erreur.classement<-1.0-(mc[1,1]+mc[2,2])/sum(mc)
print(erreur.classement)

#Taux de prédiction
prediction=mc[2,2]/sum(mc[2,])
print(prediction)

#Accuracy
accuracy=(mc[1,1])+(mc[2,2])/sum(mc)
print(accuracy)

#Recall
recall=mc[1,2]/(mc[2,1]+mc[2,2])
print(recall)



# Variable Importance
glm1_imp <- varImp(glm1, scale = FALSE, competes = FALSE)
glm1_imp

# 2eme modèle: ne garder que les prédicteurs les plus significatifs au modèle
glm2 <- train(y ~job+education+contact+pdays+previous+housing+month+day_of_week+campaign+previous+emp.var.rate+cons.price.idx+cons.conf.idx+nr.employed, 
              data = train,
              method = "glm",
              trControl = ctrl)
summary(glm2)


# Tester le modèle
predtest.glm2 <- predict(glm2, test, type = 'raw')
predicted.classes2 <- ifelse(predtest.glm2 > 0.5, "yes", "no")

#Matrice de confusion et visualisation
pred_log2 <- predict(glm2, newdata = test)
confusionMatrix(pred_log2, test$y)

mc2<-table(test$y,pred_log2)
print(mc2)

fourfoldplot(mc2, 
             color = c("red", "green"),
             conf.level = 0,
             margin = 1,
             main = "matrice de confusion du second modèle")

#Erreur de classement
erreur.classement2<-1.0-(mc2[1,1]+mc2[2,2])/sum(mc2)
print(erreur.classement2)

#Taux de prédiction
prediction2=mc2[2,2]/sum(mc2[2,])
print(prediction2)


summary(
  resamples(
    list(
      model1 = glm1, 
      model2 = glm2 
    )
  )
)$statistics
