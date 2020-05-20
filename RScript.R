## Alejandro Jiménez
## MovieLens Project - HarvardX: PH125.9x
## May 20, 2020

# Load data sets for analysis. Code provided by HarvardX: PH125.9x

################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
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

#Change format of timestamp to year
edx <- edx%>% 
  mutate(timestamp = year(as_datetime(timestamp))) %>% 
  rename(date = timestamp)

validation <- validation %>% 
  mutate(timestamp = year(as_datetime(timestamp))) %>% 
  rename(date = timestamp)

#Data Analysis#

#Rating distribution by a histogram

edx %>% ggplot(aes(rating)) + geom_histogram(bins = 10) +
  ggtitle("Rating Distribution") +
  xlab("Rating") +
  ylab("Counts")

#Rating distribution by Number of Ratings per Movie
#geom_smooth uses loess method

edx %>% group_by(movieId) %>% summarise(n_movieId=n(),rating = mean(rating)) %>%
  ggplot(aes(n_movieId,rating)) + 
  geom_point()+geom_smooth(method=loess) + 
  ggtitle("Rating vs number of ratings per movie") +
  xlab("Number of ratings per movie") + ylab("Average rating")

#Average Rate by Number of Ratings per User
#geom_smooth uses gam method

edx %>% group_by(userId) %>% summarise(n_userID=n(),rating = mean(rating)) %>%
  ggplot(aes(n_userID,rating)) + 
  geom_point()+geom_smooth() + 
  ggtitle("Rating vs number of ratings per user") +
  xlab("Number of ratings per user") + ylab("Average Rating")

#Average Rate per Date
#geom_smooth uses loess method

edx %>% group_by(date) %>% summarise(rating = mean(rating)) %>%
  ggplot(aes(date,rating)) + 
  geom_point()+geom_smooth()+
  ggtitle("Rating Average vs Year of the movie")+
  xlab("Year of the movie") +  ylab("Average Rating")

#Average Rate vs Genre
#geom_smooth uses loess method

edx %>% group_by(genres) %>% summarise(n = n(),rating =mean(rating)) %>%
  ggplot(aes(reorder(genres,rating),rating)) + 
  geom_point() +
  ggtitle("Rating Average vs number of rating per genre") +
  xlab("Genres") +  ylab("Average Rating") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

#Get average of total ratings
mu <- mean(edx$rating)

#Get RMSE with only the average rating to see improvement in analysis
results <- data.frame(Analysis = "Average of ratings",RMSE= RMSE(validation$rating,mu))
results %>% knitr::kable()

#Obtain b_i by substracting mu and averaging by movieID group
b_i <- edx %>% mutate(rating = rating - mu) %>% 
  group_by(movieId) %>% summarise(b_i = mean(rating))

#Obtain b_i for validation data to obtain RMSE
validation_bi <- validation %>% left_join(b_i, by= 'movieId') %>% .$b_i

#RMSE with mu and b_i effect
results <- bind_rows(results, data_frame(Analysis="Average and Movie Effect",  
                                RMSE = RMSE(validation$rating, mu+validation_bi)))
results %>% knitr::kable()

#Obtain b_u by substracting mu and b_i
b_u <- edx %>% left_join(b_i, by='movieId') %>%
  mutate(rating = rating - mu - b_i) %>% 
  group_by(userId) %>% summarise(b_u = mean(rating))

#Obtain b_u for validation data to obtain RMSE
validation_bu <- validation %>% left_join(b_u, by= 'userId') %>% .$b_u

#RMSE with mu, b_i and b_u effect
results <- bind_rows(results, data_frame(Analysis="Average, Movie and User Effect",  
          RMSE = RMSE(validation$rating, mu+validation_bi+validation_bu)))
results %>% knitr::kable()

#Obtain b_t by substracting mu, b_i and b_u
b_t <- edx %>% left_join(b_i, by='movieId') %>% 
  left_join(b_u, by='userId') %>%
  mutate(rating = rating - mu - b_i - b_u) %>% 
  group_by(date) %>% summarise(b_t = mean(rating))

#Obtain b_t for validation data to obtain RMSE
validation_bt <- validation %>% left_join(b_t, by= 'date') %>% .$b_t

#RMSE with mu, b_i, b_u and b_t effect
results <- bind_rows(results, data_frame(Analysis="Average, Movie, User and Year Effect",  
          RMSE = RMSE(validation$rating, mu+validation_bi+
                        validation_bu+validation_bt)))
results %>% knitr::kable()

#Obtain b_g by substracting mu, b_i, b_u and b_t
b_g <- edx %>% left_join(b_i, by='movieId') %>% 
  left_join(b_u, by='userId') %>%
  left_join(b_t, by='date') %>%
  mutate(rating = rating - mu - b_i - b_u - b_t) %>% 
  group_by(genres) %>% summarise(b_g = mean(rating))

#Obtain b_t for validation data to obtain RMSE
validation_bg <- validation %>% left_join(b_g, by= 'genres') %>% .$b_g

#RMSE with mu, b_i, b_u and b_t effect
results <- bind_rows(results, data_frame(Analysis="Average, Movie, User, Year and Genre Effect",  
                                         RMSE = RMSE(validation$rating, 
                                                     mu+validation_bi+
                                                       validation_bu+
                                                       validation_bt+
                                                       validation_bg)))
results %>% knitr::kable()


#Movie, User, Year and Genre Regularization

#Divide edx into train_set and cv_set (Cross Validation set)
#Method similar to obtain edx and validation data
set.seed(1, sample.kind="Rounding")
#Cross Validation  will be 10% of edx
cv_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-cv_index,]
temp <- edx[cv_index,]

# Make sure userId and movieId in cv set are also in train set
cv_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from cv set back into train set
removed <- anti_join(temp, cv_set)
train_set <- rbind(train_set, removed)

rm(temp, removed,cv_index)

#Set values of lambdas
lambda <- seq(0,12,0.5)

lambda_rmse <- sapply(lambda, function(lambda){

  #Obtain mu
  
  mu <- mean(train_set$rating)
  
  #Obtain b_i
  
  b_i <- train_set %>% mutate(rating = rating - mu) %>% 
    group_by(movieId) %>% summarise(b_i = sum(rating)/(n()+lambda))
  
  #Obtain b_i
  
  b_u <- train_set %>% left_join(b_i, by='movieId') %>%
    mutate(rating = rating - mu - b_i) %>% 
    group_by(userId) %>% summarise(b_u = sum(rating)/(n()+lambda))
  
  #Obtain b_t
  
  b_t <- train_set %>% left_join(b_i, by='movieId') %>% 
    left_join(b_u, by='userId') %>%
    mutate(rating = rating - mu - b_i - b_u) %>% 
    group_by(date) %>% summarise(b_t = sum(rating)/(n()+lambda))
  
  #Obtain b_g
  
  b_g <- train_set %>% left_join(b_i, by='movieId') %>% 
    left_join(b_u, by='userId') %>%
    left_join(b_t, by='date') %>%
    mutate(rating = rating - mu - b_i - b_u - b_t) %>% 
    group_by(genres) %>% summarise(b_g = sum(rating)/(n()+lambda))
  
  #Obtain b_i, b_u, b_t and b_g for CV set
  
  cv_prediction <- cv_set %>% left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>% left_join(b_t, by='date') %>%
    left_join(b_g, by= 'genres') %>% 
    mutate(prediction = mu + b_i + b_u + b_t + b_g) %>% .$prediction
  
  RMSE(cv_prediction,cv_set$rating)

})


#Plot RMSE vs lambda
data.frame(lambda = lambda, rmse = lambda_rmse) %>% 
  ggplot(aes(lambda,rmse)) + geom_point() +
  ggtitle("RMSE vs Lambda") +
  xlab("Lambda") + ylab("RMSE")

#Lambda that minimizes RMSE
optimal_lambda <- lambda[which.min(lambda_rmse)]
optimal_lambda

#Obtain RMSE with regularization in validation data


mu <- mean(edx$rating)

#Obtain b_i

b_i <- edx %>% mutate(rating = rating - mu) %>% 
  group_by(movieId) %>% summarise(b_i = sum(rating)/(n()+optimal_lambda))

#Obtain b_i

b_u <- edx %>% left_join(b_i, by='movieId') %>%
  mutate(rating = rating - mu - b_i) %>% 
  group_by(userId) %>% summarise(b_u = sum(rating)/(n()+optimal_lambda))

#Obtain b_t

b_t <- edx %>% left_join(b_i, by='movieId') %>% 
  left_join(b_u, by='userId') %>%
  mutate(rating = rating - mu - b_i - b_u) %>% 
  group_by(date) %>% summarise(b_t = sum(rating)/(n()+optimal_lambda))

#Obtain b_g

b_g <- edx %>% left_join(b_i, by='movieId') %>% 
  left_join(b_u, by='userId') %>%
  left_join(b_t, by='date') %>%
  mutate(rating = rating - mu - b_i - b_u - b_t) %>% 
  group_by(genres) %>% summarise(b_g = sum(rating)/(n()+optimal_lambda))

#Obtain b_i, b_u, b_t and b_g for validation set

validation_prediction <- validation %>% left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>% left_join(b_t, by='date') %>%
  left_join(b_g, by= 'genres') %>% 
  mutate(prediction = mu + b_i + b_u + b_t + b_g) %>% .$prediction

#RMSE with mu, b_i, b_u and b_t  with regularization effect

results <- 
  bind_rows(results, data_frame(Analysis=
                                  "Average, Movie, User, Year and Genre Effect with Regularization",  
                                         RMSE = RMSE(validation$rating, 
                                                     validation_prediction)))
results %>% knitr::kable()



#Create a vector of the genres

genres_vector <- as.vector(str_split(edx$genres,"\\|",simplify = TRUE))

#Remove from the vector values that aren't a genre

genres_vector <- genres_vector[!genres_vector %in% c("(no genres listed)","")]

#Ratio of the vector of genres
length(genres_vector)/length(edx$genres)

#Length of vector of genres
length(genres_vector)