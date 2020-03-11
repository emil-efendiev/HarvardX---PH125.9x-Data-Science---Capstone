# title: "Biomechanical features of orthopedic patients"
# subtitle: 'HarvardX - PH125.9x Data Science - Capstone - Project 2'
# author: "Emil Efendiev"
# date: "03-March-2020"
# type: R script

# The code here follows exact order of the corresponding R markdown file
# To make the code easier to read, I reference each relevant chunk in R markdown
# before start of each code. Format: CH followed by the chunk number, then S 
# followed by the section number (in the relevant markdown file). 
# Also, below is an example of a visual chunk separator
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# Code consists of 4 parts:
# 1. Project initialization
# 2. Dataset exploration
# 3. Building machine learning algorithms
# 4. Overview of results

# Please note that code in part 2 is for dataset vizualization only
# It can be skipped without any harm to the other parts of the code





##########################################################################
# Part 1 - Project initialization
##########################################################################

# CH1_S1_Initialization_code (Chunk name in R markdown)

# Below is the code, which install libraries, downloads the 
# dataset, and splits it into train and test subsets.

# Installing libraries
if(!require(tidyverse)) 
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) 
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) 
  install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ranger)) 
  install.packages("ranger", repos = "http://cran.us.r-project.org")
if(!require(naivebayes)) 
  install.packages("naivebayes", repos = "http://cran.us.r-project.org")
if(!require(kernlab)) 
  install.packages("kernlab", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) 
  install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(knitr)) 
  install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(e1071)) 
  install.packages("e1071", repos = "http://cran.us.r-project.org")

# Dataset URL
urlKG1 <- 'https://raw.githubusercontent.com/emil-efendiev/'
urlKG2 <- 'HarvardX---PH125.9x-Data-Science---Capstone/master/BFOP_2R.csv'
urlKG <- (paste(urlKG1,urlKG2,sep=""))
rm(urlKG1,urlKG2)

# importing dataset
df <- read.csv(urlKG)

# Split data into train (75%) and test sets (25%)
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead

test_index <- createDataPartition(y = df$class, times = 1, p = 0.25, list = FALSE)

train <- df[-test_index,]
test <- df[test_index,]










##########################################################################
# Part 2 - Dataset exploration
##########################################################################

# CH2_S2_Head

# Extracting top records

head(train)





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH3_S2_Summary 

# Overview of summary information

summary(train)





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH4_S2_Chart1 

# Pelvic incidence by classes chart

train %>% 
  ggplot(aes(x=class, y=train[,1])) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=1,
               outlier.size=1) +
  xlab('Class') +
  ylab('Pelvic incidence') +
  ggtitle('Pelvic incidence by classes') +
  theme_economist()





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH5_S2_Chart2 

# Pelvic tilt by classes chart

train %>% 
  ggplot(aes(x=class, y=train[,2])) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=1,
               outlier.size=1) +
  xlab('Class') +
  ylab('Pelvic tilt') +
  ggtitle('Pelvic tilt by classes') +
  theme_economist()





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH6_S2_Chart3 

# Lumbar lordosis angle by classes chart

train %>% 
  ggplot(aes(x=class, y=train[,3])) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=1,
               outlier.size=1) +
  xlab('Class') +
  ylab('Lumbar lordosis angle') +
  ggtitle('Lumbar lordosis angle by classes') +
  theme_economist()





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH7_S2_Chart4 

# Sacral slope by classes chart

train %>% 
  ggplot(aes(x=class, y=train[,4])) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=1,
               outlier.size=1) +
  xlab('Class') +
  ylab('Sacral slope') +
  ggtitle('Sacral slope by classes') +
  theme_economist()





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH8_S2_Chart5 

# Pelvic radius by classes chart

train %>% 
  ggplot(aes(x=class, y=train[,5])) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=1,
               outlier.size=1) +
  xlab('Class') +
  ylab('Pelvic radius') +
  ggtitle('Pelvic radius by classes') +
  theme_economist()





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH9_S2_Chart6 

# Grade of spondylolisthesis by classes chart

train %>% 
  ggplot(aes(x=class, y=train[,6])) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=1,
               outlier.size=1) +
  xlab('Class') +
  ylab('Grade of spondylolisthesis') +
  ggtitle('Grade of spondylolisthesis by classes') +
  theme_economist()





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH10_S2_Cor_table 

# Correlation table

# Creating table
Cor_Tab <- cor(train[,1:6])

# Changing format of the table to make it look better and display the table
colnames(Cor_Tab) = c ("PI", "PT", "LLA", "SS", "PR", "DS")
rownames(Cor_Tab) = c ("PI", "PT", "LLA", "SS", "PR", "DS")
formatC(Cor_Tab, digits = 2, format = 'f', big.mark = ',') %>% knitr::kable()










##########################################################################
# Part 3 - Building machine learning algorithms
##########################################################################

# CH11_S2_Key_Parameters

# Define key parameters for train and test sets

Tr_Pre <- as.matrix(train[,2:6]) #Train_Predictors
Tr_Cl <- train$class #Train_Class
Te_Pre <- as.matrix(test[,2:6]) #Test_Predictors
Te_Cl <- test$class #Test_Class





# ----------------------------------------
# Approach 1 - Generalized Linear Model
# ----------------------------------------

# CH12_S2_GLM_model

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead

train_glm <- train(x = Tr_Pre, y = Tr_Cl, method = "glm")

y_hat_glm <- predict(train_glm, Te_Pre)

m1acc <- confusionMatrix(y_hat_glm, Te_Cl)$overall['Accuracy']

cat('Model accuracy is equal to ', 
    formatC(m1acc, digits = 5, format = 'f', big.mark = ','), '. ',  
    'Below is the confusion matrix:', sep = "")

confusionMatrix(y_hat_glm, Te_Cl)$table





# ----------------------------------------
# Approach 2 - k-Nearest Neighbors
# ----------------------------------------

# CH13_S2_KNN_model

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead

train_knn <- train(x = Tr_Pre, y = Tr_Cl, method = "knn", 
                   tuneGrid = data.frame(k = seq(5, 60, 1)))

y_hat_knn <- predict(train_knn, Te_Pre)

m2acc <- confusionMatrix(y_hat_knn, Te_Cl)$overall['Accuracy']

cat('Model accuracy is equal to ', 
    formatC(m2acc, digits = 5, format = 'f', big.mark = ','), '. ',  
    'Below is the confusion matrix:', sep = "")

confusionMatrix(y_hat_knn, Te_Cl)$table

# Shows optimal k from finalModel. 
cat('Optimal k is ', 
    formatC(train_knn$finalModel$k, digits = 0, 
            format = 'f', big.mark = ','), 
    sep = "")





# ----------------------------------------
# Approach 3 - Random Forest
# ----------------------------------------

# CH14_S2_RF_model

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead

# Setting tuning parameters for the train function
grid <- expand.grid(mtry=seq(1, 5, 1), 
                    splitrule = c("extratrees", "gini"), 
                    min.node.size = seq(1, 50, 10))

train_rf <- train(x = Tr_Pre, y = Tr_Cl, method = "ranger", tuneGrid = grid)

y_hat_rf <- predict(train_rf, Te_Pre)

m3acc <- confusionMatrix(y_hat_rf, Te_Cl)$overall['Accuracy']

cat('Model accuracy is equal to ', 
    formatC(m3acc, digits = 5, format = 'f', big.mark = ','), '. ',  
    'Below is the confusion matrix:', sep = "")

confusionMatrix(y_hat_rf, Te_Cl)$table





# ----------------------------------------
# Approach 4 - Naive Bayes
# ----------------------------------------

# CH15_S2_NB_model

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead

# Lines of codes for manual tuning:
# grid <- expand.grid(laplace=seq(0, 5, 0.5), 
#                     usekernel = c(TRUE,FALSE), adjust=seq(0, 5, 0.5))
# tuneGrid = grid to be added to train function

# Setting train control argument for the train function
control <- trainControl(method = "cv", number = 10, p = .9)

train_nb <- train(x = Tr_Pre, y = Tr_Cl, method = "naive_bayes", trControl = control) 

y_hat_nb <- predict(train_nb, Te_Pre)

m4acc <- confusionMatrix(y_hat_nb, Te_Cl)$overall['Accuracy']

cat('Model accuracy is equal to ', 
    formatC(m4acc, digits = 5, format = 'f', big.mark = ','), '. ',  
    'Below is the confusion matrix:', sep = "")

confusionMatrix(y_hat_nb, Te_Cl)$table





# ----------------------------------------
# Approach 5 - SVM with Polynomial Kernel
# ----------------------------------------

# CH16_S2_SVM_model

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead

train_svm <- train(x = Tr_Pre, y = Tr_Cl, method = "svmPoly")

y_hat_svm <- predict(train_svm, Te_Pre)

m5acc <- confusionMatrix(y_hat_svm, Te_Cl)$overall['Accuracy']

cat('Model accuracy is equal to ', 
    formatC(m5acc, digits = 5, format = 'f', big.mark = ','), '. ',  
    'Below is the confusion matrix:', sep = "")

confusionMatrix(y_hat_svm, Te_Cl)$table










##########################################################################
# Part 4 - Overview of results
##########################################################################

# CH17_S3_Results_table

models <- c('1. Generalized Linear Model', 
            '2. k-Nearest Neighbors', 
            '3. Random Forest', 
            '4. Naive Bayes', 
            '5. Support Vector Machines with Polynomial Kernel')

acc_values <- formatC(c(m1acc, m2acc, m3acc, m4acc, m5acc), digits = 5, big.mark=",")

acc_table  <- data.frame(Accuracy = acc_values, row.names = models)

acc_table %>% knitr::kable()





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH18_S3_TopImpact_KNN}

# Top three predictors for KNN

plot(varImp(train_knn), top=3, main="Top predictors - KNN")


