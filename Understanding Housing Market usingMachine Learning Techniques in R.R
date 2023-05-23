library('dplyr')
library('Amelia')
library('tidyr')
library('ggplot2')
library('VIM')
library('mice')
library('tracerer')
library('patchwork')
library('CatEncoders')
library('corrplot')
library('plyr')
library('caret')
library('purrr')
library('car')
library('Hmisc')
library('ggpubr')
library('randomForest')
library('MASS')
library("caret")
library("ggplot2")
library('lattice')
library('ipred')
library('moments')
library('funModeling')
library('modelr')
library('e1071')
library('doParallel')
library('rpart')
library('rpart.plot')
library('Metrics')
library('ggfortify')
library('NbClust')
library('factoextra')

############---------------------------FUNCTIONS --------------------###########
myLabelEncoder <- function(colname){
  colname <- factor(colname)
  colname <- as.numeric(colname)
}


############---------------------------FUNCTIONS --------------------###########

#########___________Question 1. Provide numerical and graphical summaries of the data set and make any initial comments that you deem appropriate.___________##########


house <- read.csv(file.choose())
house_copy = house
str(house_copy)
dim(house_copy)

######------ checking NA count present per column--------------------#########
initial_na_count <-sapply(house_copy, function(y) sum(length(which(is.na(y)))))
initial_na_count <- data.frame(initial_na_count)
initial_na_count

missmap(house_copy) 

#######--------- Checking proportion of NAs per column-----------------#######
aggr(house_copy, col=mdc(1:2), numbers=TRUE, sortVars=TRUE,
     labels=names(house_copy),
     cex.axis=.5, gap=1,
     ylab=c("Proportion of missings","Missing Combinations"))


#######----------- Checking skewness and kurtosis of SalePrice---------#######

skewness_Sale <- skewness(house_copy$SalePrice)
kurtosis_Sale <- kurtosis(house_copy$SalePrice)
cat("Skewness:", skewness_Sale, "\n")
cat("Kurtosis:", kurtosis_Sale, "\n")

###----- Based on above values SalePrice column is rightly skewed (1.88) and has heavy tails and larger peak(kurtosis = 6.5) than normal distribution----####
###-------------------- log-transform of Saleprice------------------------####

house_copy$SalePrice <- log(house_copy$SalePrice)
ggplot(data = house_copy, aes(x = SalePrice))+ geom_histogram(aes( y = ..density..,colour = 'SalePrice')) + geom_density(size = 1)+scale_y_continuous(labels = scales::comma)

###---------------QQPlot for SalePrice------------------------------------#####
qqPlot(house_copy$SalePrice)

###---- Checking distribution of 'MasVnrArea'. Replaced by mode. ---------#####
ggplot(data = house_copy,aes(x = MasVnrArea)) + geom_histogram()
house_copy %>% 
  mutate(MasVnrArea = if_else(is.na(MasVnrArea), 
                              calc_mode(MasVnrArea), 
                              MasVnrArea))

###------- Checking distribution of 'LotFrontage'. Replaced by mean ------######
ggplot(data = house_copy,aes(x = LotFrontage)) + geom_histogram(aes(y = ..density..),colour = 1, fill = "#42f5ef") +geom_density(size = 1)
house_copy$LotFrontage[is.na(house_copy$LotFrontage)]<-mean(house_copy$LotFrontage,na.rm=TRUE)

###------ Counting the number of houses where no garage is present i.e. GarageArea should be '0'-------#####
ggplot(data = house_copy,aes(x = GarageArea)) + geom_histogram(aes(y = ..density..),colour = 1, fill = "#f5ce42", binwidth = 30) + geom_density(size = 1)
nrow(house_copy[house_copy$GarageArea == 0,])
### count of houses with no garage is 81 which suggests that also GarageType and GarageCond have 81 NA values for those subsequent houses.

### Counting the number of houses where no basement is present i.e. TotalBsmtSF should be '0'
ggplot(data = house_copy,aes(x = TotalBsmtSF)) + geom_histogram(aes(y = ..density..),colour = 1, fill = "#9dd0e0") +geom_density(lwd = 1, color = 'black',linetype = 1)
nrow(house_copy[house_copy$TotalBsmtSF == 0,])
### count of houses with no basement is 37 which suggests that also BsmtQual and BsmtCond have 37 NA values for those subsequent houses.



###--------- Replacing all currently present NA values to '0'-----------########
house_copy[is.na(house_copy)] <- 0
final_na_count <-sapply(house_copy, function(y) sum(length(which(is.na(y)))))
final_na_count <- data.frame(final_na_count)
final_na_count

str(house_copy)

######-------------Checking distribution of numerical columns-----------########
plot_num(house_copy, bin = 20)

#####-------- Applying Ordinal Encoding to GarageCond, BsmtQual, BsmtCond, ExterQual, ExterCond, KitchenQual-------#########
########------------- Replacement for ordinal features(Ordinal Encoding)----------------#####
house_copy_1 <- house_copy

house_copy_1 <- house_copy_1 %>% mutate(GarageCond = case_when(GarageCond == 0 ~ 0, GarageCond == 'Po' ~ 1,GarageCond == 'Fa' ~ 2,
                                                              GarageCond == 'TA' ~ 3,GarageCond == 'Gd' ~ 4,GarageCond == 'Ex' ~ 5))

house_copy_1 <- house_copy_1 %>% mutate(BsmtQual = case_when(BsmtQual == 0 ~ 0, BsmtQual == 'Po' ~ 1,BsmtQual == 'Fa' ~ 2,
                                                             BsmtQual == 'TA' ~ 3,BsmtQual == 'Gd' ~ 4,BsmtQual == 'Ex' ~ 5))

house_copy_1 <- house_copy_1 %>% mutate(BsmtCond = case_when(BsmtCond == 0 ~ 0, BsmtCond == 'Po' ~ 1,BsmtCond == 'Fa' ~ 2,
                                                             BsmtCond == 'TA' ~ 3,BsmtCond == 'Gd' ~ 4,BsmtCond == 'Ex' ~ 5))

house_copy_1 <- house_copy_1 %>% mutate(ExterQual = case_when(ExterQual == 0 ~ 0, ExterQual == 'Po' ~ 1,ExterQual == 'Fa' ~ 2,
                                                              ExterQual == 'TA' ~ 3,ExterQual == 'Gd' ~ 4,ExterQual == 'Ex' ~ 5))

house_copy_1 <- house_copy_1 %>% mutate(ExterCond = case_when(ExterCond == 0 ~ 0, ExterCond == 'Po' ~ 1,ExterCond == 'Fa' ~ 2,
                                                             ExterCond == 'TA' ~ 3,ExterCond == 'Gd' ~ 4,ExterCond == 'Ex' ~ 5))

house_copy_1 <- house_copy_1 %>% mutate(KitchenQual = case_when(KitchenQual == 0 ~ 0, KitchenQual == 'Po' ~ 1,KitchenQual == 'Fa' ~ 2,
                                                             KitchenQual == 'TA' ~ 3,KitchenQual == 'Gd' ~ 4,KitchenQual == 'Ex' ~ 5))


######-------- LEVEL Encoding for Nominal features(Level Encoding)-------#######
house_apply <- apply(house_copy_1[ , c(4,5,6,7,8,9,10,11,12,16,17,18,22,26,36,38,41,43,44,45,49,50)], 2, myLabelEncoder)
house_new <- house_copy_1
house_new[ , colnames(house_new) %in% colnames(house_apply)] <- house_apply
status(house_new)
dim(house_new)

###-------------- Removing Utilities and Id columns as all rows of Utilities have same value except one and ID is not a predictor variable---------####
house_df <- house_new[, !names(house_new) %in% c('Utilities','Id')]
dim(house_df)

###----------------Correlation plot among the features----------------##########
corr_mat <- cor(house_df)
corrplot(corr_mat, method = 'circle', type = 'lower', diag = FALSE, insig = "blank", sig.level = 0.01)


#####----------------------Pairwise correlation ( > 0.7)--------------#########
inds   <- which(corr_mat > .7, arr.ind = TRUE) # arr.ind = TRUE will return you a matrix of indices
r2     <- corr_mat[inds]
rows   <- rownames(corr_mat)[inds[, 1]]
cols   <- colnames(corr_mat)[inds[, 2]]
result_corr_mat <- cbind(rows, cols, r2)
result_corr_mat

### -------- Based on Multicolinearity we removed following columns 'X1stFlrSF', 'X2ndFlrSF', 'TotRmsAbvGrd', 'ExterQual','PoolQual'---------######
house_df1 <- house_df[, !names(house_df) %in% c('X1stFlrSF', 'X2ndFlrSF','TotRmsAbvGrd', 'ExterQual', 'PoolQual')]

### -------- Based on CorrPlot we removed 'MiscFeature', 'MiscVal' as negligibly related to SalePrice and other cols------------######
house_df1 <- house_df1[, !names(house_df1) %in% c('MiscFeature', 'MiscVal')]

dim(house_df1)


#### ----------------COllinearity Check with SalePrice------------------########
### -----checking the most important features that affect SalePrice----------###

feature_names <- colnames(house_df1)[1:42]
for (cols in feature_names){
  coeff <- cor(house_df1$SalePrice,house_df1[,cols])
  if(coeff > 0.4 | coeff < -0.4){
    print(paste('Corr coeff between SalePrice and',cols,'is',coeff))
  }
}


#####............. For Model Selection and feature importance verification we are performing Linear Regresssion and Stepwise Selection.............###
#####------------------------Linear Regression------------------------###########
model1 <- lm(SalePrice~., data = house_df1)
summary(model1)

#####------------Stepwise Seletion --------###############
step1<-step(model1,method="both")
summary(step1)


####------------------- Selecting columns based on Stepwise selection model ----------------#########
house_df2 <- house_df1[, names(house_df1) %in% c('LotArea','Street','Condition2','BldgType',
                                                 'OverallQual','OverallCond','YearBuilt','RoofMatl',
                                                 'MasVnrArea','Foundation','BsmtQual','PoolArea',
                                                 'GrLivArea', 'BedroomAbvGr','TotalBsmtSF','FullBath',
                                                 'KitchenQual','Functional', 'Fireplaces', 'GarageArea','GarageType',
                                                 'GarageCond','SaleCondition','SalePrice', 'PoolQC','YrSold')]

dim(house_df2)


#################### --------------------------Plotting graphs for important features----------------------#################

library("ggpubr")
ggscatter(house_new, x = "GrLivArea", y = "SalePrice",
          add = "reg.line", conf.int = TRUE,
          cor.method = "pearson",color = 'black',size =1, shape = 24,fill = 'red',
          xlab = "Living Area above ground", ylab = "Sales Price")

ggscatter(house_new, x = "GarageArea", y = "SalePrice",
          add = "reg.line", conf.int = TRUE,
          cor.method = "pearson",color = 'black',size =1, shape = 24,fill = 'dodgerblue3',
          xlab = "Garage Area", ylab = "Sales Price")

ggscatter(house_new, x = "MasVnrArea", y = "SalePrice",
          add = "reg.line", conf.int = TRUE,
          cor.method = "pearson",color = 'black',size =1, shape = 24,fill = 'orange',
          xlab = "Veneer Area", ylab = "Sales Price")

ggscatter(house_new, x = "TotalBsmtSF", y = "SalePrice",
          add = "reg.line", conf.int = TRUE,
          cor.method = "pearson",color = 'black',size =1, shape = 24,fill = 'maroon',
          xlab = "Total BasementSF", ylab = "Sales Price")


ggplot(data =house_new ,aes(x = as.factor(OverallQual),y = SalePrice, color = as.factor(OverallQual))) + geom_boxplot() + xlab('Overall Quality')

ggplot(data =house_new ,aes(x = as.factor(ExterQual),y = SalePrice, color = as.factor(ExterQual))) + geom_boxplot() + xlab('External Quality')

ggplot(data =house_new ,aes(x = as.factor(FullBath),y = SalePrice, color = as.factor(FullBath))) + geom_boxplot() + xlab('Number of Bathrooms')

ggplot(data =house_new ,aes(x = as.factor(Fireplaces),y = SalePrice, color = as.factor(Fireplaces))) + geom_boxplot() + xlab('Number of Fireplaces')

ggplot(data =house_new ,aes(x = as.factor(BsmtQual),y = SalePrice, color = as.factor(BsmtQual))) + geom_boxplot() + xlab('Basement Quality')

house_new$YearBuilt <- as.character(house_new$YearBuilt)
SalesVsYear <- aggregate(house_new["SalePrice"],by=house_new["YearBuilt"],sum)
ggplot(data = SalesVsYear, aes(x = YearBuilt, y = SalePrice)) + geom_bar(stat = 'identity',color = 'white', fill ='steelblue') +
  scale_y_continuous(labels = scales::comma) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))




#######______________ Question 2. Divide houses based on their overall condition (OverallCond) as follows:__________##########

###---- Divide houses based on their overall condition (OverallCond)------######
house_df2$OverallCond <-  ifelse(house_new$OverallCond <= 3, 'Poor', ifelse(house_new$OverallCond <= 6, 'Average','Good' ))
#write.csv(house_df2,'M:\\house_df2.csv')



#####----------------- Selecting features based on Stepwise selection summary-----------------#########
house_df3 <- house_df2[, names(house_df2) %in% c('LotArea','Street','Condition2','BldgType',
                                                 'OverallQual','OverallCond','YearBuilt','RoofMatl',
                                                 'MasVnrArea','Foundation','BsmtQual','PoolArea',
                                                 'GrLivArea', 'BedroomAbvGr','TotalBsmtSF','FullBath',
                                                 'KitchenQual','Functional', 'Fireplaces', 'GarageArea','GarageType',
                                                 'GarageCond','SaleCondition','SalePrice', 'PoolQC','YrSold')]

house_df3$OverallCond = as.factor(house_df3$OverallCond)


#######______________ Question 2.A ==> Fit Logistic Regression Model__________##########
###########-----------------Logistic Regression----------------#################


##.....splitting dataset into training and testing.......####
trainIndex <- caret::createDataPartition(house_df3$OverallCond, p = .8, 
                                  list = FALSE)

head(trainIndex)
train   <- house_df3[trainIndex, ]
test   <- house_df3[-trainIndex, ]

scaler <- preProcess(train, method = 'range')
train <- predict(scaler, train)
test<- predict(scaler, test)

##......Implementing Logistic regression model .........#####
set.seed(42)
LR <- nnet::multinom(OverallCond ~ ., data = train,family="binomial")
summary(LR)

predicted.classes <- predict(LR, test)
predicted.classes

mean(predicted.classes == test$OverallCond)


#####-........missclasification error...................#######
cm <- table(predicted.classes,test$OverallCond)
print(cm)
1-sum(diag(cm))/sum(cm) #error



#######______________ Question 2.B ==> Fit Random Forest Model__________##########
set.seed(42)
modelRF2 <- randomForest(OverallCond ~., data = train, ntree = 800)
predictionsRF2 <- predict(modelRF2,newdata = test)
accuracy_score <- mean(predictionsRF2 == test$OverallCond)
accuracy_score

plot(modelRF2)

#####-........missclasification error...................#######
cm_RF <- table(predictionsRF2,test$OverallCond)
print(cm_RF)
1-sum(diag(cm_RF))/sum(cm_RF) #error

#####----------BootStrapping of RandomForest Model ----------###############
mypredict <- function(object, newdata){predict(object, newdata = newdata, type = c("response"))}
forest.house_Cond2 <- errorest(OverallCond ~., data=train, model = randomForest, estimator = "boot", predict= mypredict)
forest.house_Cond2


#######______________ Question 3.A ==> Predicting House Prices__________##########
#########---------------------------Linear Regression ------------------############
#####----------------- Selecting features based on Stepwise selection summary-----------------#########

house_df_reg <- house_df2[, names(house_df2) %in% c('LotArea','Street','Condition2','BldgType',
                                                    'OverallQual','YearBuilt','RoofMatl',
                                                    'MasVnrArea','Foundation','BsmtQual','PoolArea',
                                                    'GrLivArea', 'BedroomAbvGr','TotalBsmtSF','FullBath',
                                                    'KitchenQual','Functional', 'Fireplaces', 'GarageArea','GarageType',
                                                    'GarageCond','SaleCondition','SalePrice', 'PoolQC','YrSold')]

house_df_reg$OverallCond <- house_new$OverallCond
house_df_reg$YearBuilt <- as.numeric(house_df_reg$YearBuilt)
str(house_df_reg)
dim(house_df_reg)

trainIndex <- caret::createDataPartition(house_df_reg$SalePrice, p = .7, 
                                         list = FALSE)
head(trainIndex)
train_reg   <- house_df_reg[trainIndex, ]
test_reg   <- house_df_reg[-trainIndex, ]

dim(train_reg)
dim(test_reg)
######_______________________linear Regression _______________________________##
set.seed(42)
lm_model <- lm(SalePrice ~ ., data = train_reg)
summary(lm_model)
prediction <- predict(lm_model, newdata = test_reg)
lmr_RMSE <- sqrt(mean((test_reg$SalePrice - predict(lm_model, test_reg))^2))
lmr_RMSE

R2_lm = rsquare(lm_model, data = test_reg)
R2_lm


######____________________Random Forest Regression ___________________________##

#####----------------- Selecting features based on Stepwise selection summary-----------------#########

set.seed(42)
rfr_model <- randomForest(SalePrice ~ ., data = train_reg, ntree = 800,mtry = 5, importance= TRUE)

rfr_fit.tree = rpart(SalePrice ~ ., data=train_reg, method="anova",control=rpart.control(cp_value=0.08))
rfr_fit.tree
rpart.plot(rfr_fit.tree, box.palette = 'GnBu', branch.lty = 3, shadow.col = 'gray', nn = TRUE)


rfr_predict <- predict(rfr_model, test_reg)
rfr_RMSE <- sqrt(mean((test_reg$SalePrice - predict(rfr_model, test_reg))^2))
rfr_RMSE

R2_rfr = rsquare(rfr_model, data = test_reg)
R2_rfr

####----------------Variable importance  ---------------#####
importance(rfr_model)
varImpPlot(rfr_model)

plot(rfr_model, main = "Error rate of random forest")

#####__________________Support Vector Regression__________________######


svm_model <- svm(SalePrice ~ ., data = train_reg)
svm_predict <- predict(svm_model, test_reg)
svm_RMSE <- sqrt(mean((test_reg$SalePrice - predict(svm_model, test_reg))^2))
svm_RMSE


R2_SVM = rsquare(svm_model, data = test_reg)
R2_SVM

registerDoParallel(cores = 4)

###########------------------Tuning the SVM model------------------------#######
svm_best_model = tune(svm, SalePrice ~ ., data = train_reg,ranges=list(epsilon = seq(0,1,0.1), cost = 2^(seq(0.5,8,.5))))

#Print optimum value of parameters
print(svm_best_model)

#Plotting Performance of SVM Regression model
plot(svm_best_model)

BstModel=svm_best_model$best.model
svm_predict_tuned <- predict(BstModel, test_reg)
svm_RMSE_tuned <- sqrt(mean((test_reg$SalePrice - predict(BstModel, test_reg))^2))
svm_RMSE_tuned

R2_SVM_tuned = rsquare(BstModel, data = test_reg)
R2_SVM_tuned



#############-------- Comparing performances of the modes ------------##########

dev.size(inches = c(10, 8))
dev.new()
par(mfrow=c(2,2))
plot(rfr_predict, test_reg$SalePrice, main = 'Linear Regression')
abline(0,1, col = 'Blue', lwd = 2)
plot(prediction, test_reg$SalePrice, main = 'Random Forest Regression')
abline(0,1, col = 'Red', lwd = 2)
plot(svm_predict, test_reg$SalePrice,  main = 'SV Regression')
abline(0,1, col = 'Violet', lwd = 2)
plot(svm_predict_tuned, test_reg$SalePrice,  main = 'SV Regression Tuned')
abline(0,1, col = 'Green', lwd = 2)

#######______________ Question 3.B ==> ReSampling__________##########
########### ------------Cross-Validation ------------------##########

mypredict <- function(object, newdata)
  predict(object, newdata = newdata, type = c("response"))


set.seed(42)
cv1 <- ?errorest(SalePrice ~ ., data=train_reg,model = randomForest, estimator = "cv", method = "repeated", 
               repeats = 5, folds = 10, predict = mypredict)
print(cv1$error)
plot(cv1, type = "b", xlab = "Number of Trees", ylab = "Error")

########### ------------Bootstrapping ------------------##########
set.seed(42)
cv2 <- errorest(SalePrice ~ ., data=train_reg,model = randomForest, estimator = "boot", predict = mypredict,
                est.para = control.errorest(nboot = 10))
print(cv2$error)

################-----------------------------------------------------------------#############################


rf_model <- function(x, y) { rf <- randomForest(x = x, y = y, importance = TRUE) 
                                return(rf) }
# perform bootstrap sampling and calculate error rates 
rf_error <- errorest(SalePrice ~ ., data = train_reg, model = rf_model, estimator = "cv", predict.type = "response", B = 50,parallel = "multicore") 
# plot error rate by number of trees 
plot(rf_error$error$ntree, rf_error$error$error, type = "l", ylim = c(0,max(rf_error$error$error)), ylab = "Out-of-bag error rate", xlab = "Number of trees")
lines(rf_error$error$ntree, rf_error$error$cv_error, col = "red") 
legend("topright", legend = c("Out-of-bag error", "Cross-validation error"), col = c("black", "red"), lty = 1)


#######______________ Question 4 ==> Research Question’__________##########


#In this clustering analysis we would like to tackle the
#following question
#How the houses can be clustered according to the total Living area so that the buyer can meet their budget and size requirement

ggplot(data = house, aes(x = GrLivArea))+ geom_histogram(aes( y = ..density..,colour = 'GrLivArea'))

house_Research = data.frame(house$GrLivArea, house$SalePrice)
names(house_Research)[names(house_Research) == "house.GrLivArea"] <- "LivingArea"
names(house_Research)[names(house_Research) == "house.SalePrice"] <- "SellingPrice"

library(cluster)
# Elbow method
fviz_nbclust(house_Research, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")


kmean <- kmeans(house_Research, 4)
kmean$centers
autoplot(kmean, house_Research, frame = TRUE)

library(cluster)
clusters = kmeans(house_Research, 4)
dev.size(inches = c(10, 8))
dev.new()
clusplot(house_Research, clusters$cluster, color=T, shade=F,labels=0,lines=0, main='k-Means Cluster Analysis')



