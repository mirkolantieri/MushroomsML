# Codice sviluppato sotto la versione R 3.6.1 (2019-07-05) -- "Action of the Toes"

# Pacchetti necessari per l'esecuzione del codice
# install.packages("ggplot2")
# install.packages("rpart")
# install.packages("rattle")
# install.packages("rpart.plot")
# install.packages("RColorBrewer")
# install.packages("caret")
# install.packages("e1071")
# install.packages("ROCR")
# install.packages("pROC")
# install.packages("kernlab")


mushrooms.df <- read.csv("mushrooms.csv")
str(mushrooms.df)
mushrooms.df$veil.type <- NULL #covariata con un singolo livello

# rendiamo il dataset più leggibile
levels(mushrooms.df$class)<- c("edible","poisonous")
levels(mushrooms.df$cap.shape)<-c("bell","conical","flat","knobbed","sunken","convex") 
levels(mushrooms.df$cap.surface)<- c("fibrous","grooves","smooth","scaly")
levels(mushrooms.df$cap.color)<- c("buff","cinnamon","red","gray","brown","pink","green","purple","white","yellow")
levels(mushrooms.df$bruises)<- c("bruisesno","bruisesyes")
levels(mushrooms.df$odor)<-c("almond","creosote","foul","anise","musty","nosmell","pungent","spicy","fishy")
levels(mushrooms.df$gill.attachment)<- c("attached","free")
levels(mushrooms.df$gill.spacing)<- c("close","crowded")
levels(mushrooms.df$gill.size)<-c("broad","narrow")
levels(mushrooms.df$gill.color)<- c("buff","red","gray","chocolate","black","brown","orange","pink","green","purple","white","yellow")
levels(mushrooms.df$stalk.shape)<- c("enlarging","tapering")
levels(mushrooms.df$stalk.surface.above.ring)<-c("fibrous","silky","smooth","scaly")
levels(mushrooms.df$stalk.surface.below.ring)<-c("fibrous","silky","smooth","scaly")
levels(mushrooms.df$stalk.color.above.ring)<- c("buff","cinnamon","red","gray","brown","orange","pink","white","yellow")
levels(mushrooms.df$stalk.color.below.ring)<- c("buff","cinnamon","red","gray","brown","orange","pink","white","yellow")
levels(mushrooms.df$veil.color)<- c("brown","orange","white","yellow")
levels(mushrooms.df$ring.number)<-c("none","one","two")
levels(mushrooms.df$ring.type)<- c("evanescent","flaring","large","none","pendant")
levels(mushrooms.df$spore.print.color)<- c("buff","chocolate","black","brown","orange","green","purple","white","yellow")
levels(mushrooms.df$population)<- c("abundant","clustered","numerous","scattered","several","solitary")
levels(mushrooms.df$habitat)<-c("woods","grasses","leaves","meadows","paths","urban","waste")

mushrooms.df$stalk.root[mushrooms.df$stalk.root == "?"] <- NA
levels(mushrooms.df$stalk.root)<- c(NA,"bulbous","club","equal","rooted")

library(ggplot2)
############### RIMOZIONE DEI MISSING ###############
ggplot(mushrooms.df, aes(x = class, y = stalk.root, col = class)) + 
  geom_jitter() + ggtitle("Classification of instances, based on Stalk root") +
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "purple"))
table(mushrooms.df$stalk.root)

# l'attributo stalk.root non classifica bene le istanze
# nel plot l'unico valore con "buona" classificazione è "rooted"
# tuttavia la numerosità è di 192 --> 192/8124 = 0.236 --> 2.4%
mushrooms.df$stalk.root <- NULL
######################################################

summary(mushrooms.df) # considerazioni sul dataset
sum(is.na(mushrooms.df)) # valori na


# visualizzazione delle distribuzioni delle istanze in base agli attributi
ggplot(mushrooms.df, aes(x = cap.color, y = bruises, col = class)) + 
  geom_jitter() + ggtitle("Relation between Cap color, Bruises and their classification") +
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "purple"))


ggplot(mushrooms.df, aes(x = gill.color, y = spore.print.color, col = class)) + 
  geom_jitter() + ggtitle("Relation between Gill color, Spore print color and their classification") +
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "purple"))


ggplot(mushrooms.df, aes(x = population, y = odor, col = class)) + 
  geom_jitter() + ggtitle("Relation between Population, Odor and their classification") +
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "purple"))


ggplot(mushrooms.df, aes(x = class, y = gill.color, col = class)) + 
  geom_jitter(width = 0.25) + ggtitle("Classification of instances, based on Gill color ") +
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "purple"))


ggplot(mushrooms.df, aes(x = class, y = odor, col = class)) + 
  geom_jitter(width = 0.25) + ggtitle("Classification of instances, based on Odor") +
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "purple"))



sub <- subset(mushrooms.df, select = c("spore.print.color", "class"),
                odor %in% "nosmell")

#spore.print.color
ggplot(sub, aes(x = class, y = spore.print.color, col = class)) + 
  geom_jitter(width = 0.25) + ggtitle("Classification of instances, where Odor = \"nosmell\",
                                      based on Spore print color") +
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "purple"))

table(sub$spore.print.color, sub$class)
table(sub$spore.print.color %in% "white", sub$class)[2,]

# rapporto tra la numerosità di classificazioni nel dataset completo
class.table <- table(mushrooms.df$class)
bp <- barplot(class.table, xlab="Classifications", ylab="Amount", 
              main="Distribution of classifications",
              col = c("green", "purple") , border = "white", ylim = c(0, 5000))
text(bp, class.table, paste(class.table), pos = 3, cex = 1)

# funzione per la divisione del dataset in training/test set
split.data = function(data, p = 0.7, s = 1){
  set.seed(s)
  index = sample(1:dim(data)[1])
  train = data[index[1:floor(dim(data)[1] * p)], ]
  test = data[index[((ceiling(dim(data)[1] * p)) + 1):dim(data)[1]], ]
  return(list(train=train, test=test)) 
}

# partizionamento dataset
allset <- split.data(mushrooms.df, p = 0.7, s = 42)
mushrooms.training <- allset$train
mushrooms.test <- allset$test

# rapporto tra le classificazioni nel training set
class.table <- table(mushrooms.training$class)
bp <- barplot(class.table, xlab="Classifications", ylab="Amount", 
        main="Distribution of classifications",
        col = c("green", "purple") , border = "white", ylim = c(0, 3200))
text(bp, class.table, paste(class.table), pos = 3, cex = 1) 


### PRIMO ALGORITMO: ALBERO DI DECISIONE
library("rpart")
library("rattle")
library("rpart.plot")
library("RColorBrewer")
library("caret")

# allenamento del modello considerando tutti gli attributi e cp di default
decisionTree <- rpart(class ~ ., data = mushrooms.training, method = "class")
fancyRpartPlot(decisionTree)
printcp(decisionTree)
plotcp(decisionTree, upper = "splits")

# predizione sul test set
decisionTree.Prediction <- predict(decisionTree, mushrooms.test, type = "class")

# matrice di confusione per l'albero di decisione con cp di default
confusion.matrix.decisionTree <- confusionMatrix(mushrooms.test$class, decisionTree.Prediction)
confusion.matrix.decisionTree   # 11 classificazioni errate --> stesso risultato

# allenamento del modello considerando gli attributi odor e spore.print.color. cp di default
decisionTree <- rpart(class ~ odor + spore.print.color, data = mushrooms.training, method = "class")
fancyRpartPlot(decisionTree)
printcp(decisionTree)
plotcp(decisionTree, upper = "splits")

# predizione sul test set
decisionTree.Prediction <- predict(decisionTree, mushrooms.test, type = "class")

# matrice di confusione per l'albero, considerati gli attributi odor e spore.print.color
# cp di default
confusion.matrix.decisionTree <- confusionMatrix(mushrooms.test$class, decisionTree.Prediction)
confusion.matrix.decisionTree   # 11 classificazioni errate

# voglio tentare di risolvere il problema dell'overfitting
# creo training set e test set in cui ignoro i due attributi precedenti
mushrooms.subset.training <- mushrooms.training
mushrooms.subset.training$odor <- NULL
mushrooms.subset.training$spore.print.color <- NULL
mushrooms.subset.test <- mushrooms.test
mushrooms.subset.test$odor <- NULL
mushrooms.subset.training$spore.print.color <- NULL

# rapporto tra le classificazioni nel training set
class.table <- table(mushrooms.subset.training$class)
bp <- barplot(class.table, xlab="Classifications", ylab="Amount", 
              main="Distribution of classifications",
              col = c("green", "purple") , border = "white", ylim = c(0, 3200))
text(bp, class.table, paste(class.table), pos = 3, cex = 1) 


decisionTree.subset <- rpart(class ~ ., data = mushrooms.subset.training, method = "class")
fancyRpartPlot(decisionTree.subset)
printcp(decisionTree.subset)
plotcp(decisionTree.subset, upper = "splits")
# predizione sul test set
decisionTree.subset.Prediction <- predict(decisionTree.subset, mushrooms.subset.test, type = "class")

# matrice di confusione per l'albero di decisione allenato senza gli attributi odor e spore.print.color
confusion.matrix.decisionTree.subset <- confusionMatrix(mushrooms.subset.test$class, 
                                                        decisionTree.subset.Prediction)
# il risultato generalizza meglio ma l'albero è molto complesso
confusion.matrix.decisionTree.subset


# potiamo l'albero valutando le informazioni ottenute da printcp(decisionTree.subset)
# soglia <0.087163, 0.055069> --> 3 split
# soglia <0.055069 , 0.025894> --> 4 split

prunedTree <- prune(decisionTree.subset, cp = 0.038) # 4 split 
fancyRpartPlot(prunedTree)

# predizione sul test set con prunedTree
prunedTree.Prediction <- predict(prunedTree, mushrooms.subset.test, type = "class")
confusion.matrix.prunedTree = confusionMatrix(mushrooms.subset.test$class, prunedTree.Prediction)
confusion.matrix.prunedTree

prunedTree <- prune(decisionTree.subset, cp = 0.069) # 3 split
fancyRpartPlot(prunedTree)

# predizione sul test set con prunedTree
prunedTree.Prediction <- predict(prunedTree, mushrooms.subset.test, type = "class")
confusion.matrix.prunedTree = confusionMatrix(mushrooms.subset.test$class, prunedTree.Prediction)
confusion.matrix.prunedTree



### SECONDO ALGORITMO: MACCHINA A VETTORI DI SUPPORTO
library("e1071")
library("caret")

# allenamento SVM considerando tutti gli attributi, costo di default = 1
svm.model <- svm(class ~ ., data = mushrooms.training, kernel = 'linear')
print(svm.model)

svm.pred <- predict(svm.model, mushrooms.test)
confusionMatrix(mushrooms.test$class, svm.pred)

# allenamento SVM considerando gli attributi odor e spore.print.color, costo di default = 1
svm.model <- svm(class ~ odor + spore.print.color, data = mushrooms.training, kernel = 'linear')
print(svm.model)

svm.pred <- predict(svm.model, mushrooms.test)
confusionMatrix(mushrooms.test$class, svm.pred)

tuned <- tune.svm(class ~ ., data = mushrooms.subset.training, kernel='linear',
                  cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100))
# miglior costo = 5
summary(tuned)

# allenamento SVM considerando tutti gli attributi, tranne odor, spore.print.color
# costo = 5
svm.model <- svm(class ~ ., data = mushrooms.subset.training, kernel='linear', cost = 5)
print(svm.model)

svm.pred <- predict(svm.model, mushrooms.subset.test)
confusionMatrix(mushrooms.subset.test$class, svm.pred)


# precision, recall e F-measure dei due modelli di riferimento

# modello: albero di decisione risultato dalla potatura a 3 split
confusionMatrix(mushrooms.subset.test$class, prunedTree.Prediction, mode = "prec_recall")
confusionMatrix(mushrooms.subset.test$class, prunedTree.Prediction, mode = "prec_recall",
                positive = "poisonous")

# modello: SVM sul subset (tutti gli attributi tranne odor e spore.print.color)
# kernel lineare
confusionMatrix(mushrooms.subset.test$class, svm.pred, mode = "prec_recall")
confusionMatrix(mushrooms.subset.test$class, svm.pred, mode = "prec_recall",
                positive = "poisonous")


######### ROC Performance ##########
library(ROCR)
# modello: SVM sul subset (tutti gli attributi tranne odor e spore.print.color)
# kernel lineare, costo = 5

svm.fit <- svm(class ~ ., data = mushrooms.subset.training, kernel='linear',
               cost = 5, prob = TRUE)
pred <- predict(svm.fit, mushrooms.subset.test, prob = TRUE)

# estraggo le probabilità dalla predizione
pred.prob <- attr(pred, "probabilities")
# probabilità delle etichette "poisonous"
pred.to.roc <- pred.prob[, 2] 

pred.rocr <- prediction(pred.to.roc, mushrooms.subset.test$class)

perf.rocr <- performance(pred.rocr, measure = "auc", x.measure = "cutoff")
perf.tpr.rocr <- performance(pred.rocr, "tpr", "fpr")

plot(perf.tpr.rocr, colorize = TRUE , main = paste("AUC:",(perf.rocr@y.values)))
abline(a=0, b=1)

opt.cut <- function(perf, pred){
  cut.ind <- mapply(FUN = function(x, y, p){
    d <- (x - 0)^2 + (y-1)^2
    ind <- which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}

print(opt.cut(perf.tpr.rocr, pred.rocr))

acc.perf <- performance(pred.rocr, measure = "acc")
plot(acc.perf)

ind <- which.max( slot(acc.perf, "y.values")[[1]] )
acc <- slot(acc.perf, "y.values")[[1]][ind]
cutoff <- slot(acc.perf, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))


########## Confronto tra i modelli di riferimento #############
library(pROC)
library(kernlab)
# modello albero di riferimento (class ~ gill.color + ring.type + population)
# modello SVM con due attributi (class ~ odor + spore.print.color)
# modello SVM di riferimento    (class ~ ., data = mushrooms.subset.training, kernel='linear', cost = 5)

control <- trainControl(method = "repeatedcv", number = 10, repeats = 5,
                       classProbs = TRUE, summaryFunction = twoClassSummary)

rpart.model <- train(class ~ gill.color + ring.type + population, data = mushrooms.training, method = "rpart",
                     metric = "ROC", trControl = control)

svm.model <- train(class ~ ., data = mushrooms.subset.training, method = "svmLinear", 
                   cost = 5, metric = "ROC", trControl = control)

svm.model1 <- train(class ~ odor + spore.print.color, data = mushrooms.training, method = "svmLinear", 
                    metric = "ROC", trControl = control)

rpart.probs <- predict(rpart.model, mushrooms.test[,! names(mushrooms.test) %in% c("class")],
                       type = "prob")
svm.probs <- predict(svm.model, mushrooms.subset.test[,! names(mushrooms.subset.test) %in% c("class")],
                     type = "prob")
svm.probs1 <- predict(svm.model1, mushrooms.test[,! names(mushrooms.test) %in% c("class")],
                     type = "prob")

rpart.ROC <- roc(response = mushrooms.test$class, predictor = rpart.probs$edible,
                 levels = levels(mushrooms.test$class))
plot(rpart.ROC, type = "S", col = "red")

svm.ROC <- roc(response = mushrooms.subset.test$class, predictor = svm.probs$edible,
               levels = levels(mushrooms.subset.test$class))
plot(svm.ROC, add = TRUE, col = "orange")

svm.ROC1 <- roc(response = mushrooms.test$class, predictor = svm.probs1$edible,
               levels = levels(mushrooms.test$class))
plot(svm.ROC1, add = TRUE, col = "green")

rpart.ROC
svm.ROC
svm.ROC1

cv.values <- resamples(list(rpart = rpart.model, svm = svm.model, svm1 = svm.model1))
summary(cv.values)
dotplot(cv.values, metric = "ROC")

bwplot(cv.values, layout = c(3, 1))
splom(cv.values, metric = "ROC")

cv.values$timings

