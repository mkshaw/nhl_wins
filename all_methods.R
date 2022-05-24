# 0. Load libraries & data ---- 
library(e1071)# for support vector machines
library(tidyverse)
library(dplyr) # for data manipulation
library(tree) # for single trees
library(caret) # for confusion matrix
library(pROC) # for ROC curve
library(randomForest) # bagging and random forest
library(gbm) # for boosting

nhl_training <- read.csv("nhl2013.csv") %>% 
    select(-c(Date, Team, PDO, win.streak, standing, win_loss, GlDiff))
  nhl_training$Final = factor(nhl_training$Final, c(0, 1), labels = c("Loss", "Win"))

  nhl_training$PP_perc <- nhl_training$PP_perc/100
  nhl_training$PK_perc <- nhl_training$PK_perc/100
  
  nhl_test <- read.csv("nhl2022.April3.csv") %>% 
    select(-c(Date, Team, GlDiff))
  nhl_test$Final = factor(nhl_test$Final, c(0, 1), labels = c("Loss", "Win"))
  
  nhl_test$PP_perc <- nhl_test$PP_perc/100
  nhl_test$PK_perc <- nhl_test$PK_perc/100
  
  # 1 Single Tree -----------------------------------------------------------
  
  # Training
  
  ctree.fit = tree(Final ~ ., nhl_training)
  ctree.fit
  
  # So this single tree has a single node, which doesn't work for pruning or any prediction...
  
  # Test
  
  ctree.pred.y.tt = predict(ctree.fit, nhl_test, type = "class")
  confusionMatrix(data = ctree.pred.y.tt, reference = nhl_test$Final, positive = "Win")
  ctree.prob.y.tt = predict(ctree.fit, nhl_test, type = "vector")
  ctree.roc.tt = roc(nhl_test$Final, ctree.prob.y.tt[, 2], auc = T)
  plot(ctree.roc.tt,print.auc=TRUE,legacy.axes=TRUE,
       ylab = "True Positive Rate",xlab = "False Positive Rate",main = "ROC",
       auc.polygon=TRUE,grid=TRUE)
  
  # 2 Bagging ---------------------------------------------------------------
  
  # Train bagged trees
  
  bag.fit = randomForest(Final ~., nhl_training, mtry = 7, importance = T)
  bag.fit
  varImpPlot(bag.fit, type = 2)
  
  # Test bagged trees
  bag.pred.y.tt = predict(bag.fit, nhl_test)
  confusionMatrix(data = bag.pred.y.tt, reference = nhl_test$Final, positive = "Win")
  bag.prob.y.tt = predict(bag.fit, nhl_test, type = "prob")
  bag.roc.tt = roc(nhl_test$Final, bag.prob.y.tt[, 2], auc = TRUE)
  plot(bag.roc.tt, print.auc = T, legacy.axes = T,
       ylab = "True Positive Rate", xlab = "False Positive Rate", main = "ROC",
       auc.polygon = T, grid = T)
  
  # 3 Random Forest ---------------------------------------------------------
  
  # Train random forest
  RF.fit = randomForest(Final ~ ., nhl_training, importance = TRUE)
  RF.fit
  varImpPlot(RF.fit, type = 2)
  
  # Test random forest
  RF.predicted = predict(RF.fit, nhl_test)
  confusionMatrix(data = RF.predicted, reference = nhl_test$Final, positive = "Win")
  RF.probability = predict(RF.fit, nhl_test, type = "prob")
  RF.roc = roc(nhl_test$Final, RF.probability[, 2], auc = TRUE)
  plot(RF.roc, print.auc = T, legacy.axes = T,
       ylab = "True Positive Rate", xlab = "False Positive Rate",
       main = "ROC", auc.polygon = T, grid = T)
  
  # 4 Boosting --------------------------------------------------------------
  
  # Return nhl data to numeric
  nhl_training_boosting <- read.csv("nhl2013.csv") %>% 
    select(-c(Date, Team, PDO, win.streak, standing, win_loss, GlDiff))
  
  nhl_test_boosting <- read.csv("nhl2022.April3.csv") %>% 
    select(-c(Date, Team,GlDiff))
  
  # Train boosted trees
  
  boost.fit = gbm(Final ~ ., nhl_training_boosting,
                  distribution = "bernoulli",
                  cv.folds = 100)
  gbm.perf(boost.fit, method = "cv")
  summary(boost.fit)
  
  # Test boosted trees
  boost.prob = predict(boost.fit, nhl_test, type = "response")
  boost.pred = ifelse(boost.prob > .5, 1, 0)
  boost.pred = factor(boost.pred, c(0, 1), c("Loss", "Win"))
  confusionMatrix(data = boost.pred, reference = nhl_test$Final, positive = "Win")
  boost.roc = roc(nhl_test$Final, boost.prob, auc = T)
  plot(boost.roc, print.auc = T, legacy.axes = T,
       ylab = "True Positive Rate", xlab = "False Positive Rate", main = "ROC",
       auc.polygon = T, grid = T)
  
 #=============================================================================
  
  
# 2. Support Vector Classifier  ----
  ##2-1. Train the model [ Support Vector Classifier] ----
  
  #.001, .01, .1, 1, 10
  
  #first I tried .001, .01, .1, 1, 10. 0.1 was the best. 
  #Then tried the sequence below and 0.06734151 is the best
  
  Cs = seq(from = .001, to = 10, length.out= 100)
  
  
  svc.cv = tune(svm, Final ~ ., data = nhl_training, kernel = "linear",
           ranges = list(cost = Cs))
  plot(svc.cv) # Draw the plot of CVE against C hyper-parameter 
  summary(svc.cv)
  
  #probability = TRUE to be able to calculate predicted values and ROC curves
  svc.fit = svm(Final ~ ., data = nhl_training, kernel = "linear", cost = svc.cv$best.parameters$cost, probability = TRUE)
  
  summary(svc.fit)
  
  
  
  ##2-2. Test [ Support Vector Classifier]
  
  
  svc.pred.y.tt = predict(svc.fit, nhl_test,decision.values=TRUE,probability = TRUE)
  head(svc.pred.y.tt)
  
  confusionMatrix(data = svc.pred.y.tt, reference = nhl_test$Final, positive = "Win")
  
  svc.prob.y.tt = attr(svc.pred.y.tt, "probabilities")
  head(svc.prob.y.tt)
  svc.prob.y.tt = svc.prob.y.tt[,1]
  head(svc.prob.y.tt)
  svc.roc.tt = roc(nhl_test$Final,svc.prob.y.tt,auc=TRUE) # plot the ROC curve with AUC 
  plot(svc.roc.tt,print.auc=TRUE,legacy.axes=TRUE,
       ylab = "True Positive Rate",xlab = "False Positive Rate",main = "ROC",
       auc.polygon=TRUE,grid=TRUE)

# 3. Support Vector Machine with polynomial kernel trick ----
  ##3-1. Train the model [Support Vector Machine with polynomial kernel trick] ----
  #Cs = c(.001, .01, .1, 1, 10) 
  Cs =seq(from = .001, to = 10, length.out= 100)
  svm.p.cv = tune(svm, Final ~ ., data = nhl_training, kernel = "polynomial",
                ranges = list(degree = c(1,2,3), cost = Cs))
  plot(svm.p.cv, nlevels = 100) 
  summary(svm.p.cv)
  
  
  svm.p.fit = svm(Final ~ ., data = nhl_training, kernel = "polynomial", 
                 cost = svm.p.cv$best.parameters$cost, degree = svm.p.cv$best.parameters$degree,
                 probability = TRUE)
  summary(svm.p.fit)
  
  
  ##3-2. Test [Support Vector Machine with polynomial kernel trick]
  svm.p.pred.y.tt = predict(svm.p.fit, nhl_test,decision.values=TRUE,probability = TRUE)
  head(svm.p.pred.y.tt)
  

  confusionMatrix(data = svm.p.pred.y.tt, reference = nhl_test$Final, positive = "Win")
  
  svm.p.prob.y.tt = attr(svm.p.pred.y.tt, "probabilities")
  head(svm.p.prob.y.tt)
  svm.p.prob.y.tt = svm.p.prob.y.tt[,1]
  head(svm.p.prob.y.tt)
  svm.p.roc.tt = roc(nhl_test$Final,svm.p.prob.y.tt,auc=TRUE) # plot the ROC curve with AUC 
  plot(svm.p.roc.tt,print.auc=TRUE,legacy.axes=TRUE,
       ylab = "True Positive Rate",xlab = "False Positive Rate",main = "ROC",
       auc.polygon=TRUE,grid=TRUE) 
  
# 4. Support Vector Machine with radial kernel trick ----
  ##4-1. Train the model [Support Vector Machine with radial kernel trick] ----

  Cs =seq(from = .001, to = 10, length.out= 10)
  gamma = seq(from = 0.1, to = 0.001, length.out= 10)
  
  svm.r.cv = tune(svm, Final ~ ., data = nhl_training, kernel = "radial",
                  ranges = list(gamma = Cs, cost = gamma))
  plot(svm.r.cv, nlevels = 100)
  summary(svm.r.cv)
  svm.r.fit = svm(Final ~ ., data = nhl_training, kernel = "radial", 
                  cost = svm.r.cv$best.parameters$cost, gamma = svm.r.cv$best.parameters$gamma,
                  probability = TRUE)
  summary(svm.r.fit)
  
  ##4-2. Test [Support Vector Machine with radial kernel trick] ----
  svm.r.pred.y.tt = predict(svm.r.fit, nhl_test,decision.values=TRUE,probability = TRUE)
  head(svm.r.pred.y.tt)
  
 
  confusionMatrix(data = svm.r.pred.y.tt, reference = nhl_test$Final, positive = "Win")
  
  svm.r.prob.y.tt = attr(svm.r.pred.y.tt, "probabilities")
  head(svm.r.prob.y.tt)
  svm.r.prob.y.tt = svm.r.prob.y.tt[,1]
  head(svm.r.prob.y.tt)
  svm.r.roc.tt = roc(nhl_test$Final,svm.r.prob.y.tt,auc=TRUE) # plot the ROC curve with AUC 
  plot(svm.r.roc.tt,print.auc=TRUE,legacy.axes=TRUE,
       ylab = "True Positive Rate",xlab = "False Positive Rate",main = "ROC",
       auc.polygon=TRUE,grid=TRUE) 

  
  # 2. Logistic Regression ----
  ## 2.1 Run a model [Logistic Regression Logistic Regression ] ----
  log.fit <- glm(Final ~ ., data = nhl_training, family = binomial)
  summary(log.fit)
  Exp_b_CI=exp(cbind(coef(log.fit), confint(log.fit))) # obtain estimates of exp(B) and their 95%CI 
  
  ## 2.2 Additional Analysis ----
  ### on training sample ----
  log.prob.y.tr = predict(log.fit, type = "response") # calculate individual probabilities
  # type = "response" tells R to calculate P(Y = 1|X) for each observation
  log.pred.y.tr = ifelse(log.prob.y.tr>.5,"Win","Loss") # calculate predicted values using .5 cutoff
  log.pred.y.tr = as.factor(log.pred.y.tr)
  
  
  confusionMatrix(data = log.pred.y.tr, reference = as.factor( nhl_training$Final), positive = "Win") # obtain confusion matrix
  
  log.roc.tr = roc(nhl_training$Final,log.prob.y.tr,auc=TRUE) # plot the ROC curve with AUC 
  plot(log.roc.tr,print.auc=TRUE,legacy.axes=TRUE,
       ylab = "True Positive Rate",xlab = "False Positive Rate",main = "ROC",
       auc.polygon=TRUE,grid=TRUE)
  # legacy.axes = TRUE if the x-axis must be plotted as increasing FPR(1-specificity)
  # auc.polygon = TRUE if you want to color the area under the ROC curve. 
  
  ### on test sample [Logistic Regression ]  ----
  log.prob.y.tt = predict(log.fit, nhl_test, type = "response")
  log.pred.y.tt = ifelse(log.prob.y.tt>.5,"Win","Loss")
  log.pred.y.tt = as.factor(log.pred.y.tt)
  confusionMatrix(data = log.pred.y.tt, reference = nhl_test$Final, positive = "Win")
  log.roc.tt = roc(nhl_test$Final,log.prob.y.tt,auc=TRUE) # plot the ROC curve with AUC 
  plot(log.roc.tt,print.auc=TRUE,legacy.axes=TRUE,
       ylab = "True Positive Rate",xlab = "False Positive Rate",main = "ROC",
       auc.polygon=TRUE,grid=TRUE)
  
  
  
  # 6. Model Comparison (ROC & AUC) ----
  
  plot(ctree.roc.tt, legacy.axes = T, col = "black", lwd = 4,
       ylab = "True Positive Rate", xlab = "False Positive Rate",
       main = "ROC", grid = T)
  plot(bag.roc.tt, legacy.axes = T, col = "brown", lwd = 4, add = T)
  plot(RF.roc, legacy.axes = T, col = "cyan3", lwd = 4, add = T)
  plot(boost.roc, legacy.axes = T, col = "purple", lwd = 4, add = T)
  
  plot(svc.roc.tt,legacy.axes=T,col="cornflowerblue",lwd=4,add=T)
  plot(svm.p.roc.tt,legacy.axes=T,col="yellow",lwd=4,add=T)
  plot(svm.r.roc.tt,legacy.axes=T,col="deeppink2",lwd=4,add=T)
  plot(log.roc.tt,legacy.axes=T,col="darksalmon",lwd=4,add=T)
  
  
  legend("bottomright",legend=c("single tree", "bagging", "random forest", "boosting","SVC","SVM(P)","SVM(R)","lm"),col=c("black", "brown", "cyan3", "purple","cornflowerblue","yellow","deeppink2","darksalmon"),lwd=4)
  
  cat("\n AUC(tree)                   : ",ctree.roc.tt$auc,
      "\n AUC(bagging)                : ",bag.roc.tt$auc,
      "\n AUC(random forest)          : ",RF.roc$auc,
      "\n AUC(boosting)               : ",boost.roc$auc,
      "\n AUC(SVC)                    : ",svc.roc.tt$auc,
      "\n AUC(SVM(Polynomial))        : ",svm.p.roc.tt$auc,
      "\n AUC(SVM(Radial))            : ",svm.r.roc.tt$auc,
      "\n AUC(Logistic regression)    : ",log.roc.tt$auc
      )
  
  
 