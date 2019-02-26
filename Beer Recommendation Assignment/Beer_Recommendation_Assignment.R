#Include the required libraries

library(dplyr)
library(ggplot2)
library(recommenderlab)

##################### Data Preparation  #########################################
#1. Read the csv into ad dataframe
#2. Perform data cleaning in which we deal with
#         a.Missing Value
#         b.Duplicated rows
#         c.Duplicate ratings for same beer
#         d.Faulty rows
#         e.Emptry Rows
#3. Assumption : We consider each review profilename is unique and are different users.

#Read the beer data
df <- read.csv("beer_data.csv", header = T, sep = ',')


#Find more about the data


# There are total of 475984 rows 
nrow(df)

#There are three columns beerid of type int , review_profilename as factor  and review_overall of type number
str(df)

#Dimensions of the dataframe is 475984 and 3 columns
dim(df)

#We proceed to data cleaning ,checking for duplicates ,missing data and faulty rows

############### 1.Duplicate rows  ################

#Check for any duplciate rows
length(which(duplicated(df)))

#since there are duplicates ,we remove them
df <- df[!duplicated(df),]

############### 2. Missing data ##################

#check for empty beer id , since there are none ,no cleaning required.
length(which(df$beer_beerid == ''))


#check for empty review_overall, since there are 6 remove it
length(which(df$review_overall == ''|df$review_overall == 0))
df<- df[df$review_profilename != 0,]


#check for empty review_profilename, since there are 100,we remove them
length(which(df$review_profilename == ''))

#remove the rows with empty profile names
df<- df[df$review_profilename != '',]

############### 3. Faulty rows ####################

#group the data by beerid and the user who reviewed the beer.
beer_by_user_and_beer_id<- df %>% group_by(review_profilename,beer_beerid)%>%summarise(no_of_user_reviews=n())

#nrow(beer_by_user_and_beer_id)

#Find the rows where user has rated same beer more than once, remove them since one beer cannot have two
#ratings by same user.
temp_df <- beer_by_user_and_beer_id[!beer_by_user_and_beer_id$no_of_user_reviews > 1,]

#merge the two dataframes 
merged_df<-left_join(df,temp_df,by=c("beer_beerid","review_profilename"))

#removing faulty rows 
beer_data <- merged_df[!is.na(merged_df$no_of_user_reviews),]%>%select(beer_beerid,review_profilename,review_overall)

#cleaned data is stored in dataframe beer_data

############## Deciding on the value of N ##############

#group the data based on beer id and check how many reviews each beer has obtained.
beer_by_id <- beer_data %>% group_by(beer_beerid)%>%summarise(no_of_reviews=n()) %>% arrange(desc(no_of_reviews))

#total number of distinct beers available is 40290
total_beer = nrow(beer_by_id)

#If we look at the data ,we see there are lot of entries with ratings 1,let us find how much 
#percentage of beers is rated by only one person

beer_rating_1_or_less <- nrow(beer_by_id[beer_by_id$no_of_reviews <= 1,])
percent_of_data <- (beer_rating_1_or_less/total_beer)*100

#We see that around 45% of the beer is rated by only 1 person


#Beer with ratings more than 50 is 5.1%
beer_rating_50_or_more <- nrow(beer_by_id[beer_by_id$no_of_reviews >= 50,])
percent_of_data <- (beer_rating_50_or_more/total_beer)*100


#Beer with ratings more than 10 is 17.2%
beer_rating_10_or_more <- nrow(beer_by_id[beer_by_id$no_of_reviews >= 10,])
percent_of_data <- (beer_rating_10_or_more/total_beer)*100

#Let us find the median on the number of reviews a beer has on an average and we get 2
median(beer_by_id$no_of_reviews)


#Analysis based on number of ratings :
#  5% of the beer are rated only more than 50 people
# 17% of the beer are rated only more than 10 people
# 45% of the beer are rated only one time
# median of the no of reviews a beer has got is 2
# Considering the range is large, 2 seems to be an invalid.Hence, check the proportion of beers that have
# been rated less number of times


#So let us check overall how many of the beers are well rated to decide on the value of N

beer_review_ratio <- beer_by_id %>%group_by(no_of_reviews)%>%summarise(count=n())%>%arrange(desc(count))%>%mutate(proportion=(count/total_beer)*100)
beer_review_ratio

#We see that till 10 the gap in the number of reviews falls sharply from 44.8 to 1% ,after which
#the proportion starts decreasing.As we check the boxplots and Qplots we see that as the
#no of reviews approaches 50, the graph looks like a normal distribution with skew .

df_temp <- beer_by_id[beer_by_id$no_of_reviews >= 50,]

#Lets plot the histogram removing the beers with ratings less than 50 ,and can see that the spread
#relatively increased and moves towards the right from value of 2 to around 100.

qplot(df_temp$no_of_reviews, geom = "histogram",bins=50)
#ggplot(beer_by_name,aes(x=beer_beerid,y=no_of_reviews)) + geom_histogram(stat = "identity")

summary(df_temp)
#Considering the beer review ratio, we see more than 80% of the beer are hardly rated by users,so let
#us focus on the remaining beers which are rated consistently and check the median

median(df_temp$no_of_reviews)

#We have ignored all the unimportant beers from the dataset.Now for the rest,lets consider the median
#Lets take the median of the data N =99 as the appropriate value of N and build our final dataframe

temp_df <- beer_data%>%group_by(beer_beerid)%>%mutate(count=n())%>%filter(count > 99)%>%ungroup()


beer_df <- data.frame(temp_df)
#beer_df is the dataframe that contains the required data after choosing the appropriate value of N


#ggplot(beer_df,aes(x=beer_beerid,stat="identity"))+geom_histogram()

#reconfirm there are no NA values in the final dataset used for data exploration
which(is.na(beer_df))

#Create realRatingMatrix
beer_df <- beer_df[,c(2,1,3)]

#Convert dataframe to realRatingMatrix
r <- as(beer_df, "realRatingMatrix")


#Get some information
class(r)

range(getRatings(r))

dimnames(r)

head(rowCounts(r))
head(colCounts(r))
head(rowMeans(r))

#add code to visualize the graphs

################################ Data Exploration ###############################

#1.Determine how similar the first ten users are with each other and visualise it


similar_users <- similarity(r[1:10, ],
                            method = "cosine",
                            which = "users")

#Similarity matrix
as.matrix(similar_users)

#Visualise similarity matrix
image(as.matrix(similar_users), main = "User similarity")

#Analysis :We see that users 1,4,7,9 have some similarity,with 7 and 9 being more similar

#2.Compute and visualise the similarity between the first 10 beers

similar_items <- similarity(r[,1:10 ],
                            method = "cosine",
                            which = "items")
as.matrix(similar_items)

image(as.matrix(similar_items), main = "Item similarity")

#Analysis : We see that first 10 beers have high similarity with each other.

#3.What are the unique values of ratings?
#Assumption : unique value refers to different rating values given.

grouped_df<- beer_df%>%group_by(review_overall)%>%summarise(count=n())%>%arrange(desc(count))

#Analysis :In total there are 9 unique ratings 1,1.5 ,2,2.5,3.0,3.5,4.0,4.5
#we see that ratings 1.5 and 1 seem to be less frequently given and ratings 4.0 and 4.5 
#more frequently given by users.

###### 1. The average beer ratings ######

average_beer_ratings <- beer_df %>% group_by(beer_beerid)%>% summarize(avg_rating=mean(review_overall))
ggplot(average_beer_ratings, aes(beer_beerid,avg_rating))  +
  ggtitle("Ratings for different Beer IDs ")   +
  labs(x="Beer ID",y="Average Rating ")           +
  geom_line(color = "red") + geom_smooth(color="black")

summary(average_beer_ratings)

# Analysis : Average beer ratings is 3.86

###### 2. The average user ratings    ######

average_user_ratings <- beer_df %>% group_by(review_profilename)%>% summarize(avg_rating=mean(review_overall))
summary(average_user_ratings)


qplot(rowMeans(r),xlab = "Avg Rating by User", ylab = "Count",main = "Avg User Rating " )

#Analysis : Average user rating is 3.969


###### 3. The average number of ratings given to the beers  ######

avg_number_beer_ratings <- beer_df %>% group_by(beer_beerid)%>% summarize(number_of_ratings=n())
ggplot(avg_number_beer_ratings, aes(beer_beerid,number_of_ratings))  +
  ggtitle("No of ratings per beer ")   +
  labs(x="Beer ID ",y="Number ofratings ")           +
  geom_line() + geom_smooth()

qplot(colCounts(r),xlab = "No of Beer Ratings", ylab = "Count of ratings",main = " Avg No of Ratings for Beer " )

summary(avg_number_beer_ratings)

#Analysis :Average number of ratings given to Beers is 217


###### 4. The average number of ratings given by the users  ######
avg_number_user_ratings <- beer_df %>% group_by(review_profilename)%>% summarize(number_of_ratings=n())

qplot(rowCounts(r),xlab = "No of Ratings given by User", ylab = "Count",main = " Average number of user Ratings " )

summary(avg_number_user_ratings)

#Analysis :On an average, each user has rated 12.26 movies.

###################### Recommendation models ###################### 

#1. Divide your data into training and testing datasets
#           Experiment with 'split' and 'cross-validation' evaluation schemes


#Method 1
#Divide data into test and splitting into test and train
split_scheme <- evaluationScheme(r, method = "split", train = .7,
                           k = 1, given = -1, goodRating = 4)

#Method 2
#Do a 5-fold cross-validation 
cross_validation_scheme <- evaluationScheme(r, method = "cross-validation",
                            k = 4, given = -1, goodRating = 4)

#2.Build IBCF and UBCF models
algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30, minRating=3)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
)


# Run algorithms with both the schemes, predict next n beers
results_of_splitting <- evaluate(split_scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))

results_of_cross_validation <- evaluate(cross_validation_scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))


#3.Compare the performance of the two models and suggest the one that should be deployed
#  Plot the ROC curves for UBCF and IBCF and compare them


plot(results_of_splitting, annotate = 1:4, legend="topleft")
plot(results_of_cross_validation, annotate = 1:4, legend="topleft")

#Analysis : 
#1.We see that that both schemes fair almost same with cross validation shows some improvement in TPR 
#2.Comparing UBCF and IBCF models for both schemes we see that UBCF fairs better.
#3.The TPR for UBCF increases as number of predicted beers increases
#4.The TPR for IBCF does not increase much ,while the value even for 20 is below 0.01
#Hence, we suggest UBCF to be deployed





#4.Give the names of the top 5 beers that you would recommend to the users "cokes", "genog" & "giblet"
#Build recommender using UBCF,since we find that more efficient.

################### Find recommendation for user Cokes ########################

Rec.model=Recommender(r,method="UBCF")

#recommended top 5 items for user cokes
recommended.items.cokes <- predict(Rec.model, r["cokes",], n=5)

# to display them
as(recommended.items.cokes, "list")

#The beer recommended for user cokes :"4083"  "7971"  "22227" "34420" "2137" 

################### Find recommendation for user genog ########################


#recommended top 5 items for user genog
recommended.items.genog <- predict(Rec.model, r["genog",], n=5)

# to display them
as(recommended.items.genog, "list")

#The beer recommended for user genog :"1641"  "6076"  "57908" "1005"  "47026"

################### Find recommendation for user giblet ########################


#recommended top 5 items for user giblet
recommended.items.giblet <- predict(Rec.model, r["giblet",], n=5)

# to display them
as(recommended.items.giblet, "list")

#The beer recommended for user giblet :"141"   "7971"  "459"   "34420" "73" 
