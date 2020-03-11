# title: "MovieLens Project"
# subtitle: 'HarvardX - PH125.9x Data Science - Capstone - Project 1'
# author: "Emil Efendiev"
# date: "18/02/2020"
# type: R script

# The code here follows exact order of the corresponding R markdown file
# To make the code easier to read, I reference each relevant chunk in R markdown
# before start of each code. Format: CH followed by the chunk number, then S 
# followed by the section number. Also, below is an example of a visual chunk separator
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# Code consists of 4 sections:
# 1. Project initialization
# 2. Dataset exploration
# 3. Building machine learning algorithm
# 4. Overview of results

# Please note that code in the section 2 is for dataset vizualization only
# It can be skipped without any harm to the other sections of the code





##########################################################################
# Section 1 - Project initialization
##########################################################################

# CH1_S1_Dataset_download_and_Split (Chunk name in R markdown)

# Below is the code (provided by the organizers), which downloads the 
# dataset, and splits it into train (edx) and test (validation) subsets.

# Note: this process could take a couple of minutes

if(!require(tidyverse)) 
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) 
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) 
  install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
 # https://grouplens.org/datasets/movielens/10m/
 # http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
 download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
 colnames(movies) <- c("movieId", "title", "genres")
 movies <- as.data.frame(movies) %>% 
   mutate (movieId = as.numeric(levels(movieId))[movieId], 
           title = as.character(title), 
           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
 edx <- movielens[-test_index,]
 temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
      semi_join(edx, by = "movieId") %>%
      semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
 edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH2_S1_download_libraries 

# In addition, below is the code for ggthemes and knitr libraries installation 
# in case they are not installed yet (might cause errors otherwise).

if(!require(ggthemes)) 
  install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(knitr)) 
  install.packages("knitr", repos = "http://cran.us.r-project.org")





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH3_S1_RMSE_function

# Below is RMSE function code. TV is for True Value. PV is for Predicted Value

RMSE <- function(PV, TV) {
    sqrt(mean((PV - TV)^2))
  }










##########################################################################
# Section 2 - Dataset exploration
##########################################################################

# CH4_S2_edx_head

# Extracting top 4 (looks better than extracting without specifying a number) records

head(edx,4)





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH5_S2_edx_summary 

# Overview of summary information

summary(edx)





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH6_S2_Rows_Columns

# Extracting actual number of records

matrix(c(formatC(nrow(edx), digits = 0, format = 'f', big.mark = ','), 
         formatC(ncol(edx), digits = 0, format = 'f', big.mark = ',')), 
       dimnames = list(c("Number of rows","Number of colums"),""))





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH7_S2_Unique_Users_Movies

# Calculating number of unique records for userId and movieId

edx %>% 
  summarize(number_of_users = formatC(n_distinct(userId), 
                                      digits = 0, format = 'f', 
                                      big.mark = ','),
            number_of_movies = formatC(n_distinct(movieId),
                                       digits = 0, format = 'f', 
                                       big.mark = ','))





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH8_S2_Chart1

# Create a chart showing Number of ratings per movie

edx %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
    geom_histogram(bins = 50, color = "black") + 
    scale_x_log10() + 
    xlab("Number of ratings - logarithmic scale") +
    ylab("Number of movies") +
    ggtitle("Number of ratings per movie") +
    theme_economist()





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH9_S2_Chart2

# Create a chart showing Number of ratings per user

edx %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
    geom_histogram(bins = 50, color = "black") + 
    scale_x_log10() +
    xlab("Number of ratings - logarithmic scale") +
    ylab("Number of users") +
    ggtitle("Number of ratings per user")+
    theme_economist()





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH10_S2_Chart3

# Create a random matrix of random 100 records of each movies and users 
# (thus having 10,000 records in total) to see how sparse this matrix is
# Highlighted are the cells of the chart where a user left a rating for a movie

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead

users <- sample(unique(edx$userId), 100) # creates a temporary object for calculation

edx %>% 
  filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% 
  select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% 
  t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
  abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

rm(users) # removes unused object





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH11_S2_Chart4

# Create a chart showing common rating for a movie

CPR <- as.data.frame(table(edx$rating)) # creates a temporary object for calculation

ggplot(CPR, aes(x=Var1,y = Freq/1000000)) + 
  geom_bar(stat = "identity") + 
  xlab("Rating") +
  ylab("Number of ratings (in m)") +
  ggtitle("Number of ratings per rating category")+
  theme_economist()

rm (CPR) # removes unused object





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH12_S2_Chart5

# Create a chart showing average rating
  
edx %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating)) %>%
    ggplot(aes(b_u)) +
    geom_histogram(bins = 30, color = "black") +
    xlab("Mean rating") + 
    ylab("Number of users") +
    ggtitle("Mean movie rating") +
    scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
    theme_economist()





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH13_S2_Chart6

# Create a chart showing distribution of ratings by movie release year

# creates a temporary object for calculation
edx_with_year <- edx %>% mutate(year = as.numeric(str_sub(title,-5,-2)))

edx_with_year %>%
  group_by(year) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(year, rating)) +
    scale_y_continuous(limits = c(3, 4)) +
    xlab("Year") + 
    ylab("Mean rating") +
    ggtitle("Average rating by movie periods") +
    geom_smooth(se=FALSE) +
    theme_economist()

rm(edx_with_year) # removes unused object










##########################################################################
# Section 3 - Building machine learning algorithm
##########################################################################

# ----------------------------------------
# Approach 1 - Simple model
# ----------------------------------------

# CH14_S3_1_Edx_average

# Calculate edx dataset average

edx_av <- mean(edx$rating)

cat('The average of the edx dataset is ', 
    formatC(edx_av, digits = 2, format = 'f', big.mark = ','), sep = "")





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH15_S3_1_RMSE1

# Calculate RMSE for approach 1 and store it as AP1_RMSE

AP1_RMSE <- RMSE(edx_av, validation$rating)

cat('RMSE is equal to ', 
    formatC(AP1_RMSE, digits = 5, format = 'f', big.mark = ','), sep = "")





# ----------------------------------------
# Approach 2 - Enhancing the system using "movie effect"
# ----------------------------------------

# CH16_S3_2_BI_calc

# Calculate movie bias / effect (b_i)

AP2_movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - edx_av))





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH17_S3_2_Predictions

# Build predictions for the validation dataset

predicted_ratings_BI <- edx_av + validation %>% 
  left_join(AP2_movie_avgs, by='movieId') %>%
  pull(b_i)





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH18_S3_2_RMSE2

# Calculate RMSE for approach 2 and store it as AP2_RMSE

AP2_RMSE <- RMSE(predicted_ratings_BI, validation$rating)

cat('RMSE for our enhanced model is equal to ',
    formatC(AP2_RMSE, digits = 5, format = 'f', big.mark = ','), sep = "")





# ----------------------------------------
# Approach 3 - Enhancing the system using "user effect"
# ----------------------------------------

# CH19_S3_3_BU_calc

# Calculate user bias / effect (b_u)

AP3_user_avgs <- edx %>% 
  left_join(AP2_movie_avgs, by = 'movieId') %>%
  group_by(userId) %>% 
  summarize(b_u = mean(rating - edx_av - b_i))





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH20_S3_3_Predictions

# Build predictions for the validation dataset

predicted_ratings_BI_BU <- validation %>% 
  left_join(AP2_movie_avgs, by='movieId') %>%
  left_join(AP3_user_avgs, by = 'userId') %>%
  mutate(PRBIBU = edx_av + b_i + b_u) %>%
  pull(PRBIBU)





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH21_S3_3_RMSE3

# Calculate RMSE for approach 3 and store it as AP3_RMSE

AP3_RMSE <- RMSE(predicted_ratings_BI_BU, validation$rating)

cat('RMSE for our model enhanced with movie and user effects is equal to ', 
    formatC(AP3_RMSE, digits = 5, format = 'f', big.mark = ','), sep = "")





# ----------------------------------------
# Approach 4 - Regularization
# ----------------------------------------

# CH22_S3_4_Split_edx_set 

# Split edx dataset into two partitions - train and test

# Train set will be 10% of edx data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
 edx_train <- edx[-test_index,]
 temp <- edx[test_index,]

# Make sure userId and movieId in edx_test set are also in edx_train set
edx_test <- temp %>% 
      semi_join(edx_train, by = "movieId") %>%
      semi_join(edx_train, by = "userId")

# Add rows removed from edx_test set back into edx_train set
removed <- anti_join(temp, edx_test)
 edx_train <- rbind(edx_train, removed)

rm(test_index, temp, removed)





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH23_S3_4_Reg_Fun_RMSE_mod

# Create a function, which would take lambda as an argument, make predictions 
# (adjusting dataset average with user and movie biases) and calculate relevant RMSE
# All this done with edx dataset (split into train and test partitions)

Reg_Fun_RMSE_mod <- function(l){

  mu <- mean(edx_train$rating)
  
  # movie bias element
  b_i <- edx_train %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  # user bias element
  b_u <- edx_train %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))

  # predictions
  predicted_ratings <- 
    edx_test %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, edx_test$rating))
}





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH24_S3_4_LMRMSE

# Now, let's try lambdas from 0 to 10 with a step of 0.2 to determine the one 
# which minimizes RMSE

lambdas <- seq(0, 10, 0.2)

rmses <- sapply(lambdas, Reg_Fun_RMSE_mod)

LMRMSE <- lambdas[which.min(rmses)] # LMRMSE - lamda min RMSE

cat('Lambda which minimizes RMSE is equal to ', LMRMSE, sep = "")





# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# CH25_S3_4_RMSE4

# Make predictions (for the validation dataset) and calculate RMSE

# movie bias element
b_i_m <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - edx_av)/(n()+LMRMSE))

# user bias element
b_u_m <- edx %>% 
  left_join(b_i_m, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - edx_av - b_i)/(n()+LMRMSE))

# predictions
predicted_ratings_m <- 
  validation %>% 
  left_join(b_i_m, by = "movieId") %>%
  left_join(b_u_m, by = "userId") %>%
  mutate(pred = edx_av + b_i + b_u) %>%
  pull(pred)

AP4_RMSE <- RMSE(predicted_ratings_m, validation$rating)

cat('RMSE is equal to ', 
    formatC(AP4_RMSE, digits = 5, format = 'f', big.mark = ','), sep = "")










##########################################################################
# Section 4 - Overview of results
##########################################################################

# CH26_S4_Results_table 

RMSE_models <- c('1. Simple Average', 
                 '2. Adjusted by movie effect', 
                 '3. Adjusted by movie and user effects' , 
                 '4. Regularized movie and user effects')

RMSE_values <- formatC(c(AP1_RMSE,AP2_RMSE,AP3_RMSE,AP4_RMSE), digits = 5, big.mark=",")

RMSE_table  <- data.frame(RMSE = RMSE_values, row.names = RMSE_models)

RMSE_table%>% knitr::kable()

