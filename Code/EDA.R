setwd("C:/Ankit/Y2S1/DATA7202 Statistical Methods for Data Science/Assignment/Assignment_1/DATA7202_1")
raw_data = read.csv("Data/OnlineNewsPopularity.csv")

#remove non predictive columns
data = raw_data[c(3:61)]
sum(data$shares)

##---- 2. Data Exploration ----##

#---- Null data check ----#

#the function complete.cases() returns a logical vector indicating which cases are complete.
#using the function we find if there are any rows with null values
null_rows <- data[!complete.cases(data),]

#there are no rows with null values

#summary statistics of the dataset
summary(data)

#from the summary statistics we observe that some articles have no content [ n_token_content = 0], 
#We investigate this further below

#---- Articles without content exploration ----#

articles_with_no_content <- nrow(data[data$n_tokens_content==0,])

null_content_rows <- data[data$n_tokens_content==0,]

#1181 articles have no content.

summary(null_content_rows)
sum(null_content_rows$shares)
null_content_rows$image_flag <- 0
null_content_rows$video_flag <- 0

null_content_rows$image_flag[null_content_rows$num_imgs > 0] <- 1
null_content_rows$video_flag[null_content_rows$num_videos > 0] <- 1

sum(null_content_rows$image_flag)
sum(null_content_rows$video_flag)

#such articles have a minimum of 4 shares.
#of the 1181 articles having no content, 1080 articles have images and 816 articles have videos

#---- Negative shares by keyword exploration ----#
data_kw = data[c(18:26)]

data_kw$kw_min_min_flag <- 0
data_kw$kw_avg_min_flag <- 0
data_kw$kw_min_avg_flag <- 0

data_kw$kw_min_min_flag[data_kw$kw_min_min == -1] <- 1
data_kw$kw_avg_min_flag[data_kw$kw_avg_min == -1] <- 1
data_kw$kw_min_avg_flag[data_kw$kw_min_avg == -1] <- 1

sum(data_kw$kw_min_min_flag)
sum(data_kw$kw_avg_min_flag)
sum(data_kw$kw_min_avg_flag)

#---- Data channel distribution exploration ----#
sum(data$data_channel_is_lifestyle)
sum(data$data_channel_is_entertainment)
sum(data$data_channel_is_bus)
sum(data$data_channel_is_socmed)
sum(data$data_channel_is_tech)
sum(data$data_channel_is_world)

#---- Day distribution exploration ----#
sum(data$weekday_is_monday)
sum(data$weekday_is_tuesday)
sum(data$weekday_is_wednesday)
sum(data$weekday_is_thursday)
sum(data$weekday_is_friday)
sum(data$weekday_is_saturday)
sum(data$weekday_is_sunday)
sum(data$is_weekend)


#---- sum of shares across days and data channel ----#

data_monday_shares = data[c(30,59)]
data_tuesday_shares = data[c(31,59)]
data_wednesday_shares = data[c(32,59)]
data_thursday_shares = data[c(33,59)]
data_friday_shares = data[c(34,59)]
data_saturday_shares = data[c(35,59)]
data_sunday_shares = data[c(36,59)]

agg_monday = aggregate(data_monday_shares,
                by = list(data_monday_shares$weekday_is_monday),
                FUN = sum)

agg_tuesday = aggregate(data_tuesday_shares,
                       by = list(data_tuesday_shares$weekday_is_tuesday),
                       FUN = sum)

agg_wednesday = aggregate(data_wednesday_shares,
                       by = list(data_wednesday_shares$weekday_is_wednesday),
                       FUN = sum)

agg_thursday = aggregate(data_thursday_shares,
                       by = list(data_thursday_shares$weekday_is_thursday),
                       FUN = sum)

agg_friday = aggregate(data_friday_shares,
                       by = list(data_friday_shares$weekday_is_friday),
                       FUN = sum)

agg_saturday = aggregate(data_saturday_shares,
                       by = list(data_saturday_shares$weekday_is_saturday),
                       FUN = sum)

agg_sunday = aggregate(data_sunday_shares,
                       by = list(data_sunday_shares$weekday_is_sunday),
                       FUN = sum)

data_lifestyle_shares = data[c(12,59)]
data_entertainment_shares = data[c(13,59)]
data_bus_shares = data[c(14,59)]
data_scomed_shares = data[c(15,59)]
data_tech_shares = data[c(16,59)]
data_world_shares = data[c(17,59)]

agg_lifestyle = aggregate(data_lifestyle_shares,
                       by = list(data_lifestyle_shares$data_channel_is_lifestyle),
                       FUN = sum)

agg_entertainment = aggregate(data_entertainment_shares,
                        by = list(data_entertainment_shares$data_channel_is_entertainment),
                        FUN = sum)

agg_bus = aggregate(data_bus_shares,
                          by = list(data_bus_shares$data_channel_is_bus),
                          FUN = sum)

agg_scomed = aggregate(data_scomed_shares,
                         by = list(data_scomed_shares$data_channel_is_socmed),
                         FUN = sum)

agg_tech = aggregate(data_tech_shares,
                       by = list(data_tech_shares$data_channel_is_tech),
                       FUN = sum)

agg_world = aggregate(data_world_shares,
                         by = list(data_world_shares$data_channel_is_world),
                         FUN = sum)

#---- box plot of Shares across days and data channel ----#

shares_on_monday <- data_monday_shares[data_monday_shares$weekday_is_monday==1,]

boxplot(shares_on_monday$shares,
        main = "Article Shares on Monday",
        xlab = "Monday",
        ylab = "Shares"
)

summary(shares_on_monday)

shares_on_tuesday <- data_tuesday_shares[data_tuesday_shares$weekday_is_tuesday==1,]

boxplot(shares_on_tuesday$shares,
        main = "Article Shares on Tuesday",
        xlab = "Tuesday",
        ylab = "Shares"
)

summary(shares_on_tuesday)

shares_on_wednesday <- data_wednesday_shares[data_wednesday_shares$weekday_is_wednesday==1,]

boxplot(shares_on_wednesday$shares,
        main = "Article Shares on Wednesday",
        xlab = "Wednesday",
        ylab = "Shares"
)

summary(shares_on_wednesday)

shares_on_thursday <- data_thursday_shares[data_thursday_shares$weekday_is_thursday==1,]

boxplot(shares_on_thursday$shares,
        main = "Article Shares on Thursday",
        xlab = "Thursday",
        ylab = "Shares"
)

summary(shares_on_thursday)

shares_on_friday <- data_friday_shares[data_friday_shares$weekday_is_friday==1,]

boxplot(shares_on_friday$shares,
        main = "Article Shares on Friday",
        xlab = "Friday",
        ylab = "Shares"
)

summary(shares_on_friday)

shares_on_saturday <- data_saturday_shares[data_saturday_shares$weekday_is_saturday==1,]

boxplot(shares_on_saturday$shares,
        main = "Article Shares on Saturday",
        xlab = "Saturday",
        ylab = "Shares"
)

summary(shares_on_saturday)

shares_on_sunday <- data_sunday_shares[data_sunday_shares$weekday_is_sunday==1,]

boxplot(shares_on_sunday$shares,
        main = "Article Shares on Sunday",
        xlab = "Sunday",
        ylab = "Shares"
)

summary(shares_on_sunday)


shares_on_lifestyle <- data_lifestyle_shares[data_lifestyle_shares$data_channel_is_lifestyle==1,]

boxplot(shares_on_lifestyle$shares,
        main = "Article Shares on Lifestyle",
        xlab = "Lifestyle",
        ylab = "Shares"
)

summary(shares_on_lifestyle)

shares_on_entertainment <- data_entertainment_shares[data_entertainment_shares$data_channel_is_entertainment==1,]

boxplot(shares_on_entertainment$shares,
        main = "Article Shares on Entertainment",
        xlab = "Entertainment",
        ylab = "Shares"
)

summary(shares_on_entertainment)

shares_on_bus <- data_bus_shares[data_bus_shares$data_channel_is_bus==1,]

boxplot(shares_on_bus$shares,
        main = "Article Shares on Business",
        xlab = "Business",
        ylab = "Shares"
)

summary(shares_on_bus)

shares_on_scomed <- data_scomed_shares[data_scomed_shares$data_channel_is_socmed==1,]

boxplot(shares_on_scomed$shares,
        main = "Article Shares on Social Media",
        xlab = "Social Media",
        ylab = "Shares"
)

summary(shares_on_scomed)

shares_on_tech <- data_tech_shares[data_tech_shares$data_channel_is_tech==1,]

boxplot(shares_on_tech$shares,
        main = "Article Shares on Technology",
        xlab = "Technology",
        ylab = "Shares"
)

summary(shares_on_tech)

shares_on_world <- data_world_shares[data_world_shares$data_channel_is_world==1,]

boxplot(shares_on_world$shares,
        main = "Article Shares on World",
        xlab = "World",
        ylab = "Shares"
)

summary(shares_on_world)

#---- scatter plot of variables----#

png(filename=paste("./Plot/Scatter_Plot_1",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 1:4){
  plot(data[,j], data$shares, xlab=column[j], ylab='Shares')
}
dev.off()

png(filename=paste("./Plot/Scatter_Plot_2",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 5:8){
  plot(data[,j], data$shares, xlab=column[j], ylab='Shares')
}
dev.off()

png(filename=paste("./Plot/Scatter_Plot_3",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 9:12){
  plot(data[,j], data$shares, xlab=column[j], ylab='Shares')
}
dev.off()

png(filename=paste("./Plot/Scatter_Plot_4",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 13:16){
  plot(data[,j], data$shares, xlab=column[j], ylab='Shares')
}
dev.off()

png(filename=paste("./Plot/Scatter_Plot_5",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 17:20){
  plot(data[,j], data$shares, xlab=column[j], ylab='Shares')
}
dev.off()

png(filename=paste("./Plot/Scatter_Plot_6",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 21:24){
  plot(data[,j], data$shares, xlab=column[j], ylab='Shares')
}
dev.off()

png(filename=paste("./Plot/Scatter_Plot_7",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 25:28){
  plot(data[,j], data$shares, xlab=column[j], ylab='Shares')
}
dev.off()


png(filename=paste("./Plot/Scatter_Plot_8",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 29:32){
  plot(data[,j], data$shares, xlab=column[j], ylab='Shares')
}
dev.off()

png(filename=paste("./Plot/Scatter_Plot_9",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 33:36){
  plot(data[,j], data$shares, xlab=column[j], ylab='Shares')
}
dev.off()

png(filename=paste("./Plot/Scatter_Plot_10",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 37:40){
  plot(data[,j], data$shares, xlab=column[j], ylab='Shares')
}
dev.off()

png(filename=paste("./Plot/Scatter_Plot_11",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 41:44){
  plot(data[,j], data$shares, xlab=column[j], ylab='Shares')
}
dev.off()

png(filename=paste("./Plot/Scatter_Plot_12",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 45:48){
  plot(data[,j], data$shares, xlab=column[j], ylab='Shares')
}
dev.off()

png(filename=paste("./Plot/Scatter_Plot_13",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 49:52){
  plot(data[,j], data$shares, xlab=column[j], ylab='Shares')
}
dev.off()

png(filename=paste("./Plot/Scatter_Plot_14",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 53:56){
  plot(data[,j], data$shares, xlab=column[j], ylab='Shares')
}
dev.off()

png(filename=paste("./Plot/Scatter_Plot_15",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 57:58){
  plot(data[,j], data$shares, xlab=column[j], ylab='Shares')
}
dev.off()

#---- correlation among variables----#

install.packages("Hmisc")
library("Hmisc")
library("lattice")

res <- rcorr(as.matrix.data.frame(data))

res$r

res$p

#---- outliers detected by box plot----#

png(filename=paste("./Plot/Box_Plot_1",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 1:4){
  boxplot(data[,j], main = column[j])
}
dev.off()

png(filename=paste("./Plot/Box_Plot_2",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 5:8){
  boxplot(data[,j], main = column[j])
}
dev.off()

png(filename=paste("./Plot/Box_Plot_3",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 9:12){
  boxplot(data[,j], main = column[j])
}
dev.off()

png(filename=paste("./Plot/Box_Plot_4",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 13:16){
  boxplot(data[,j], main = column[j])
}
dev.off()

png(filename=paste("./Plot/Box_Plot_5",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 17:20){
  boxplot(data[,j], main = column[j])
}
dev.off()

png(filename=paste("./Plot/Box_Plot_6",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 21:24){
  boxplot(data[,j], main = column[j])
}
dev.off()

png(filename=paste("./Plot/Box_Plot_7",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 25:28){
  boxplot(data[,j], main = column[j])
}
dev.off()

png(filename=paste("./Plot/Box_Plot_8",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 29:32){
  boxplot(data[,j], main = column[j])
}
dev.off()

png(filename=paste("./Plot/Box_Plot_9",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 33:36){
  boxplot(data[,j], main = column[j])
}
dev.off()

png(filename=paste("./Plot/Box_Plot_10",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 37:40){
  boxplot(data[,j], main = column[j])
}
dev.off()

png(filename=paste("./Plot/Box_Plot_11",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 41:44){
  boxplot(data[,j], main = column[j])
}
dev.off()

png(filename=paste("./Plot/Box_Plot_12",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 45:48){
  boxplot(data[,j], main = column[j])
}
dev.off()

png(filename=paste("./Plot/Box_Plot_13",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 49:52){
  boxplot(data[,j], main = column[j])
}
dev.off()

png(filename=paste("./Plot/Box_Plot_14",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 53:56){
  boxplot(data[,j], main = column[j])
}
dev.off()

png(filename=paste("./Plot/Box_Plot_15",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
column = colnames(data)
for (j in 57:59){
  boxplot(data[,j], main = column[j])
}
dev.off()

##---- 3. general linear model (multiple regression) ----##

#---- check model assumptions ----#

#---- remove multicollinearity ----#
str(data)
#change categorical data to factor
data$data_channel_is_lifestyle <- as.factor(data$data_channel_is_lifestyle)
data$data_channel_is_entertainment <- as.factor(data$data_channel_is_entertainment)
data$data_channel_is_bus <- as.factor(data$data_channel_is_bus)
data$data_channel_is_socmed <- as.factor(data$data_channel_is_socmed)
data$data_channel_is_tech <- as.factor(data$data_channel_is_tech)
data$data_channel_is_world <- as.factor(data$data_channel_is_world)
data$weekday_is_monday <- as.factor(data$weekday_is_monday)
data$weekday_is_tuesday <- as.factor(data$weekday_is_tuesday)
data$weekday_is_wednesday <-as.factor(data$weekday_is_wednesday)
data$weekday_is_thursday <- as.factor(data$weekday_is_thursday)
data$weekday_is_friday <- as.factor(data$weekday_is_friday)
data$weekday_is_saturday <- as.factor(data$weekday_is_saturday)
data$weekday_is_sunday <- as.factor(data$weekday_is_sunday)
data$is_weekend <- as.factor(data$is_weekend)


#Removing variables with very high correlation
data_processed = data[,(!names(data) %in% c('n_non_stop_unique_tokens','n_non_stop_words','kw_max_min','self_reference_max_shares','LDA_02','self_reference_min_shares','kw_max_avg
'))]

#install.packages("car")
library("car")

lm.data_processed = lm(shares ~ ., data = data_processed)
car::vif(lm.data_processed)

alias(lm.data_processed)

#we remove variables weekday_is_sunday and is_weekend as they cause multicollinearity

data_processed = data_processed[,(!names(data_processed) %in% c('weekday_is_sunday','is_weekend'))]
lm.data_processed = lm(shares ~ ., data = data_processed)
car::vif(lm.data_processed)

#We iteratively remove variables till the maximum vif value is less than or equal to 3.

data_processed = data_processed[,(!names(data_processed) %in% c('rate_positive_words'))]
lm.data_processed = lm(shares ~ ., data = data_processed)
car::vif(lm.data_processed)

data_processed = data_processed[,(!names(data_processed) %in% c('kw_avg_avg'))]
lm.data_processed = lm(shares ~ ., data = data_processed)
car::vif(lm.data_processed)

data_processed = data_processed[,(!names(data_processed) %in% c('rate_negative_words'))]
lm.data_processed = lm(shares ~ ., data = data_processed)
car::vif(lm.data_processed)

data_processed = data_processed[,(!names(data_processed) %in% c('avg_negative_polarity'))]
lm.data_processed = lm(shares ~ ., data = data_processed)
car::vif(lm.data_processed)

data_processed = data_processed[,(!names(data_processed) %in% c('data_channel_is_world'))]
lm.data_processed = lm(shares ~ ., data = data_processed)
car::vif(lm.data_processed)

data_processed = data_processed[,(!names(data_processed) %in% c('global_sentiment_polarity'))]
lm.data_processed = lm(shares ~ ., data = data_processed)
car::vif(lm.data_processed)

data_processed = data_processed[,(!names(data_processed) %in% c('LDA_04'))]
lm.data_processed = lm(shares ~ ., data = data_processed)
car::vif(lm.data_processed)

data_processed = data_processed[,(!names(data_processed) %in% c('kw_max_max'))]
lm.data_processed = lm(shares ~ ., data = data_processed)
car::vif(lm.data_processed)

data_processed = data_processed[,(!names(data_processed) %in% c('avg_positive_polarity'))]
lm.data_processed = lm(shares ~ ., data = data_processed)
car::vif(lm.data_processed)

data_processed = data_processed[,(!names(data_processed) %in% c('LDA_00'))]
lm.data_processed = lm(shares ~ ., data = data_processed)
car::vif(lm.data_processed)

data_processed = data_processed[,(!names(data_processed) %in% c('weekday_is_wednesday'))]
lm.data_processed = lm(shares ~ ., data = data_processed)
car::vif(lm.data_processed)

#Build the model
glm.data_processed = glm(shares ~ ., data=data_processed)
summary(glm.data_processed)

png(filename=paste("./Plot/3.1_AssumptionChecka",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
plot(glm.data_processed)
dev.off()

png(filename=paste("./Plot/3.1_AssumptionCheckb",".png"), width = 1024, height = 768)
#install.packages("rcompanion")
#library("rcompanion")
plotNormalHistogram(residuals(glm.data_processed), breaks=50)
dev.off()

#---- transform variables & check assumptions ----#
#install.packages("MASS")
library(MASS)
library(dplyr)
box = boxcox(shares ~ ., data=data_processed)

cox = data.frame(box$x, box$y)
cox2 = cox[with(cox, order(-cox$box.y)),]
cox2[1,]

lambda = cox2[1, "box.x"]
data_transformed = data_processed%>%
  mutate(cox_shares = ((shares ^ lambda - 1)/lambda),
         log_n_tokens_content = log10(n_tokens_content + 1),
         log_n_unique_tokens = log10(n_unique_tokens + 1),
         log_num_hrefs = log10(num_hrefs + 1),
         log_num_self_hrefs = log10(num_self_hrefs + 1),
         log_num_imgs = log10(num_imgs + 1),
         log_num_videos = log10(num_videos + 1),
         log_average_token_length = log10(average_token_length + 1)
         )

data_transformed = data_transformed[,!(names(data_transformed) %in% c("shares","n_tokens_content", "n_unique_tokens", "num_hrefs",  "num_self_hrefs", "num_imgs", "num_videos", "average_token_length"))]

#Test transformed model
glm.data_transformed = glm(cox_shares ~ ., data=data_transformed)
summary(glm.data_transformed)

png(filename=paste("./Plot/3.2_Transformed_Data_a",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
plot(glm.data_transformed)
dev.off()

#install.packages("rcompanion")
library("rcompanion")
png(filename=paste("./Plot/3.2_Transformed_Data_b",".png"), width = 1024, height = 768)
plotNormalHistogram(residuals(glm.data_transformed), breaks=50)
dev.off()

#---- interaction between variables ----#

glm.data_interaction = glm(cox_shares ~ . + log_num_videos*kw_max_avg, data=data_transformed)
summary(glm.data_interaction)

png(filename=paste("./Plot/3.4_Interaction_Data_a",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
plot(glm.data_interaction)
dev.off()

#install.packages("rcompanion")
#library("rcompanion")
png(filename=paste("./Plot/3.4_Interaction_Data_b",".png"), width = 1024, height = 768)
plotNormalHistogram(residuals(glm.data_interaction), breaks=50)
dev.off()

#---- estimated model parameters ----#
parameters <- coefficients(summary(glm.data_transformed))

write.table(summary_table, file = "summary_table.txt", sep = ",", quote = FALSE )

#--- subsample of 1000 observations ---#

sample_data_linear = data_transformed[sample(nrow(data_transformed),1000),]

glm.sample_data_linear = glm(cox_shares ~ ., data=sample_data_linear)
summary(glm.sample_data_linear)

png(filename=paste("./Plot/8_Sample_Linear_Data_a",".png"), width = 1024, height = 768)
par(mfrow=c(2,2))
plot(glm.sample_data_linear)
dev.off()

#install.packages("rcompanion")
library("rcompanion")
png(filename=paste("./Plot/8_Sample_Linear_Data_b",".png"), width = 1024, height = 768)
plotNormalHistogram(residuals(glm.sample_data_linear), breaks=50)
dev.off()


##---- 4. logistic regression model  ----##

##--- check model assumption --- ##
logistic_data <- data %>%
  mutate(popular_article = if_else(shares>1400,1,0))
logistic_data <- logistic_data[,!(names(data) %in% c("shares"))]

glm.logistic_model <- glm(popular_article ~ ., data=logistic_data, family=binomial(link='logit'))
summary(glm.logistic_model)

logistic_data$prediction = predict(glm.logistic_model, type="response")

table(logistic_data$popular_article, logistic_data$prediction > 0.5)

#install.packages("pROC")
#library("pROC")
g <- roc(logistic_data$popular_article ~ logistic_data$prediction)

png(filename=paste("./Plot/4.1_ROC",".png"), width = 1024, height = 768)
plot(g, xlim=c(1,0), main='ROC')
dev.off()

#install.packages("ROCR")
#library("ROCR")

auc = performance(prediction(logistic_data$prediction,logistic_data$popular_article), "auc")
auc <- auc@y.values[[1]]
auc



probabilities <-  predict(glm.logistic_model, type="response")
predicted.classes <- ifelse(probabilities > 0.5 , "pos", "neg")
head(predicted.classes)

#Linearity assumption check

#Select only numeric predictors
mydata <- logistic_data %>%
  dplyr::select_if(is.numeric)
predictors <- colnames(mydata)

#install.packages("tidyr")

library("tidyr")

#Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

#install.packages("ggplot2")
library("ggplot2")

png(filename=paste("./Plot/4.1_Linearity_Assumption_a",".png"), width = 1024, height = 768)
ggplot(mydata,aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() +
  facet_wrap(~predictors, scales = "free_y")
dev.off()

# Influential values
png(filename=paste("./Plot/4.1_Influential_Values_a",".png"), width = 1024, height = 768)
plot(glm.logistic_model, which = 4, id.n = 3)
dev.off()

#Extract model results
#install.packages("broom")
library("broom")
glm.logistic_model.data <- augment(glm.logistic_model) %>%
  mutate(index = 1:n())

glm.logistic_model.data %>% top_n(3, .cooksd)

#Plot the standerdized residuals
png(filename=paste("./Plot/4.1_Influential_Values_b",".png"), width = 1024, height = 768)
ggplot(glm.logistic_model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = popular_article), alpha = .5)
dev.off()

glm.logistic_model.data <- glm.logistic_model.data %>% 
  filter(abs(.std.resid) > 3)

#Multicollinearity cehck

car::vif(glm.logistic_model)

alias(glm.logistic_model)

##--- model with transformed varables ---##

#Removing variables with very high correlation
logistic_data_processed = logistic_data[,(!names(data) %in% c('n_non_stop_unique_tokens','n_non_stop_words','data_channel_is_world','kw_max_min','kw_max_max','kw_avg_avg','self_reference_min_shares',
'self_reference_max_shares','weekday_is_wednesday','weekday_is_sunday','is_weekend','LDA_00','LDA_02','LDA_04','global_sentiment_polarity','rate_positive_words','rate_negative_words','avg_positive_polarity','avg_negative_polarity'))]


logistic_data_transformed = logistic_data_processed%>%
  mutate(log_n_tokens_content = log10(n_tokens_content + 1),
         log_n_unique_tokens = log10(n_unique_tokens + 1),
         log_num_hrefs = log10(num_hrefs + 1),
         log_num_self_hrefs = log10(num_self_hrefs + 1),
         log_num_imgs = log10(num_imgs + 1),
         log_num_videos = log10(num_videos + 1),
         log_average_token_length = log10(average_token_length + 1)
  )

logistic_data_transformed = logistic_data_transformed[,!(names(logistic_data_transformed) %in% c("n_tokens_content", "n_unique_tokens", "num_hrefs",  "num_self_hrefs", "num_imgs", "num_videos", "average_token_length"))]

glm.logistic_data_transformed <- glm(popular_article ~ ., data = logistic_data_transformed,family=binomial(link='logit'))
summary(glm.logistic_data_transformed)

logistic_data_transformed$prediction = predict(glm.logistic_data_transformed, type="response")
table(logistic_data_transformed$popular_article, logistic_data_transformed$prediction>0.5)

install.packages("pROC")
library("pROC")
g <- roc(logistic_data_transformed$popular_article ~ logistic_data_transformed$prediction)

png(filename=paste("./Plot/4.2_ROC",".png"), width = 1024, height = 768)
plot(g, xlim=c(1,0), main='ROC')
dev.off()

install.packages("ROCR")
library("ROCR")

auc = performance(prediction(logistic_data_transformed$prediction,logistic_data_transformed$popular_article), "auc")
auc <- auc@y.values[[1]]
auc



##--- model parameters ---##
parameters <- coefficients(summary(glm.logistic_data_transformed))
summary_table= data.frame(na.omit(confint(glm.logistic_data_transformed, level=0.95))[,1],
                          na.omit(confint(glm.logistic_data_transformed, level = 0.95))[,2])

write.table(summary_table, file = "summary_table_logistic2.txt", sep = ",", quote = FALSE )

##-- sample data -- #
sample_data_logistic = logistic_data_transformed[sample(nrow(logistic_data_transformed),1000),]

glm.sample_data_logistic <- glm(popular_article ~ ., data = sample_data_logistic,family=binomial(link='logit'))
summary(glm.sample_data_logistic)

sample_data_logistic$prediction = predict(glm.sample_data_logistic, type="response")
table(sample_data_logistic$popular_article, sample_data_logistic$prediction>0.5)

#install.packages("pROC")
library("pROC")
g <- roc(sample_data_logistic$popular_article ~ sample_data_logistic$prediction)

png(filename=paste("./Plot/8_Logistic_ROC",".png"), width = 1024, height = 768)
plot(g, xlim=c(1,0), main='ROC')
dev.off()

#install.packages("ROCR")
library("ROCR")

auc = performance(prediction(logistic_data_transformed$prediction,logistic_data_transformed$popular_article), "auc")
auc <- auc@y.values[[1]]
auc
