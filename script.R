library(tidyverse) #most datascience related libraries
library(FNN) #knn regression
library(corrplot) # correlation plot
library(tree) # Decision Tree
library(caret) #train control
options(warn=-1) # filter warnings

Vgsales <- read.csv("C:/Users/devka/OneDrive/Desktop/Video_Games_Sales_as_at_22_Dec_2016.csv",sep=",",na.strings=c(""," ","NA","N/A"))
head(Vgsales)

str(Vgsales)

na_count <-sapply(Vgsales, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

#drop all NA values
Vgsales <- na.omit(Vgsales)
str(Vgsales)

#drop all sales except global
Vgsales_df <- cbind(Vgsales[,-c(6:10)],Vgsales$Global_Sales)
Vgsales_df$Year_of_Release <- 2016 - Vgsales_df$Year_of_Release
names(Vgsales_df)[3] <- "Game_Age"
names(Vgsales_df)[12] <- "Global_Sales"
str(Vgsales_df)

summary(Vgsales_df)

#creating a function to control figure size
fig <- function(width, heigth){
  options(repr.plot.width = width, repr.plot.height = heigth)
}

fig(20,8)
Platform_bar <- ggplot(Vgsales_df, aes(x=Platform,fill =Platform)) + geom_bar() + theme(text = element_text(size=30))  
Platform_bar

fig(15,8)
Age_bar <- ggplot(Vgsales_df, aes(x=Game_Age)) + geom_bar(fill = "forestgreen") + theme(text = element_text(size=30))  
Age_bar

fig(15,8)
Platform_bar <- ggplot(Vgsales_df, aes(x=Genre,fill =Genre)) + geom_bar() + theme(text = element_text(size=30),axis.text.x=element_text(angle = 45,vjust = 0.4,size=30))  
Platform_bar

fig(12, 8)
Critic_Score_hist <- ggplot(Vgsales_df, aes(Critic_Score))
Critic_Score_hist + geom_histogram(binwidth = 4, color = "black",fill = "gold") + theme(text = element_text(size=30))

fig(12, 8)
Critic_Count_hist <- ggplot(Vgsales_df, aes(Critic_Count))
Critic_Count_hist + geom_histogram(binwidth = 4, color = "black",fill = "skyblue") + theme(text = element_text(size=30))

fig(20,8)
Vgsales_df %>% select(Name,User_Score) %>% arrange(desc(User_Score))%>% head(10)%>%
  ggplot(aes(x=Name,y=User_Score,fill=Name))+geom_bar(stat="identity")+
  theme(text = element_text(size=30),legend.position="right",axis.text.x=element_text(angle = 90,vjust = 0.5,hjust = 1,size=15))+labs(x="Game",y="User Scores",title="Top 10 user score games")+scale_fill_brewer(palette="PRGn")

fig(12, 8)
User_Count_Count_hist <- ggplot(Vgsales_df, aes(User_Count))
User_Count_Count_hist + geom_histogram(color = "black",fill = "pink") + theme(text = element_text(size=30))

fig(15,8)
Rating_bar <- ggplot(Vgsales_df, aes(x=Rating,fill =Rating)) + geom_bar() + theme(text = element_text(size=30))  
Rating_bar

fig(12, 8)
Global_Sales_hist <- ggplot(Vgsales_df, aes(Global_Sales))
Global_Sales_hist + geom_histogram(binwidth = 2, color = "black",fill = "firebrick1") + theme(text = element_text(size=30))

fig(20,8)
sales_by_platform <- ggplot(Vgsales_df, aes(Platform,Global_Sales,fill =Platform))
sales_by_platform +geom_bar(stat = "identity") + 
  theme(text = element_text(size=30),legend.position="right",axis.text.x=element_text(angle = 90,vjust = 0.5,hjust = 1,size=15))+labs(x="Platform",y="Global Sales",title="Sales by Platform")

Vgsales_df %>% select(Publisher,Global_Sales)%>%group_by(Publisher)%>%
  summarise(Total_sales=sum(Global_Sales))%>%arrange(desc(Total_sales))%>% head(10)%>%
  ggplot(aes(x=Publisher,y=Total_sales,fill=Publisher))+geom_bar(stat="identity")+
  theme(text = element_text(size=30),legend.position="right",axis.text.x=element_text(angle = 90,vjust = 0.5,hjust = 1,size=15))+labs(x="Publisher",y="Global Sales",title="Top 10 Publishers")+scale_fill_brewer(palette="Paired")

fig(20,8)
Vgsales_df %>% select(Developer,Global_Sales)%>%group_by(Developer)%>%
  summarise(Total_sales=sum(Global_Sales))%>%arrange(desc(Total_sales))%>% head(10)%>%
  ggplot(aes(x=Developer,y=Total_sales,fill=Developer))+geom_bar(stat="identity")+
  theme(text = element_text(size=30),legend.position="right",axis.text.x=element_text(angle = 90,vjust = 0.5,hjust = 1,size=15))+labs(x="Developer",y="Global Sales",title="Top 10 selling Developers")+scale_fill_brewer(palette="PuOr")

Vgsales_df %>% select(Name,Global_Sales) %>% arrange(desc(Global_Sales))%>% head(10)%>%
  ggplot(aes(x=Name,y=Global_Sales,fill=Name))+geom_bar(stat="identity")+
  theme(text = element_text(size=30),legend.position="right",axis.text.x=element_text(angle = 90,vjust = 0.5,hjust = 1,size=15))+labs(x="Developer",y="Total Sales",title="Top 10 selling Developers")+labs(x="Game",y="Global Sales",title="Top 10 selling games")+scale_fill_brewer(palette="Spectral")

Gs <- ggplot(Vgsales_df, aes(Game_Age,Global_Sales))
Gs + geom_jitter(color = "darkviolet") + theme(text = element_text(size = 30))

Cs <- ggplot(Vgsales_df, aes(Critic_Score,Global_Sales))
Cs + geom_jitter(color = "darkblue") + theme(text = element_text(size = 30))

Ccs <- ggplot(Vgsales_df, aes(Critic_Count,Global_Sales))
Ccs + geom_jitter(color = "red") + theme(text = element_text(size = 30))

Us <- ggplot(Vgsales_df, aes(User_Count,Global_Sales))
Us + geom_jitter(color = "orange") + theme(text = element_text(size = 30))

#FROM HERE TRYING FOR MACHINE LEARNING

#KNN REGRESSOR

#name column is an id column we can remove for good (only used for games Top ten sales)
Vgsales_df <- Vgsales_df[,-1]
knn.df <- Vgsales_df
head(knn.df)

# Identify the columns causing the issue (categorical columns)
categorical_columns <- c("Platform", "Genre", "Publisher", "Developer", "Rating")

# Convert categorical columns to numeric
for (col in categorical_columns) {
  knn.df[[col]] <- as.numeric(as.factor(knn.df[[col]]))
}

# Convert remaining columns to numeric (if not already numeric)
for (i in 1:ncol(knn.df)) {
  if (!is.numeric(knn.df[[i]])) {
    knn.df[[i]] <- as.numeric(knn.df[[i]])
  }
}

# Check the updated dataset
head(knn.df)




# Identify categorical columns
categorical_columns <- c("Platform", "Genre","Publisher","Developer", "Rating")

# Convert categorical columns to numeric by encoding as factors first
for (col in categorical_columns) {
  knn.df[[col]] <- as.numeric(as.factor(knn.df[[col]]))
}

# Scale the dataset excluding the target column
knn.df1 <- sapply(knn.df[, 1:(dim(knn.df)[2] - 1)], scale) # Apply scaling
knn.df1 <- as.data.frame(knn.df1) # Convert to dataframe

# Add the target column (Global_Sales) back
knn.df <- cbind(knn.df1, Global_Sales = knn.df$Global_Sales)

# Rename target column for consistency
names(knn.df)[ncol(knn.df)] <- "Global_Sales"

# Check the updated dataset
head(knn.df)


set.seed(5)
train.size <- floor(0.7*nrow(knn.df))
train.index <- sample(1:nrow(knn.df),train.size, replace = F)
train.set <- knn.df[train.index,]
test.set <- knn.df[-train.index,]
train.x <- train.set[,-11]
train.y <- train.set[,11]
test.x <- test.set[,-11]
test.y <- test.set[,11]

pred_003 <- FNN::knn.reg(train = train.x, test = test.x, y = train.y, k = 3)
diff3 = test.y-pred_003$pred
test_mse = mean(diff3^2)
test_mse

knn.df1 <- knn.df
for (i in 1:ncol(knn.df1))
{
  print(names(knn.df1)[i]) 
  print(summary(lm(knn.df1$Global_Sales~knn.df1[,i],knn.df1)))
}

k = seq(1,51,2)
i=1 
Mse=1                     

for (i in k)
{
  knn.mod <-  FNN::knn.reg(train = train.x, test = test.x, y = train.y, k = i)  
  Mse[i] <-mean((test.y - knn.mod$pred)^2)
  k=i  
  cat(k,'=',round(Mse[i],2),'\n')        
}

k = seq(1,51,2)
Mse <- na.omit(Mse)
Mseplot = data.frame(k,Mse)
ggplot(Mseplot,aes(x=k,y=Mse))+
  geom_line(col = "cyan")+
  expand_limits(y=Mse[0])+
  theme_bw() +
  geom_text(aes(label=round(Mse,2)),vjust = -0.5) +
  theme(text = element_text(size = 30))

#all the variables have given us a Mse of 2.59 at k = 19, let's try to find an optimal k for a model without the lower significance variables Game_Age, Genre, and Publisher.

set.seed(5)
train.x <- train.set[,-c(2,3,4,11)]
train.y <- train.set[,11]
test.x <- test.set[,-c(2,3,4,11)]
test.y <- test.set[,11]

k = seq(1,51,2)
i=1 
Mse=1                     

for (i in k)
{
  knn.mod <-  FNN::knn.reg(train = train.x, test = test.x, y = train.y, k = i)  
  Mse[i] <-mean((test.y - knn.mod$pred)^2)
  k=i  
  cat(k,'=',round(Mse[i],2),'\n')        
}

k = seq(1,51,2)
Mse <- na.omit(Mse)
Mseplot = data.frame(k,Mse)
ggplot(Mseplot,aes(x=k,y=Mse))+
  geom_line(col = "cyan")+
  expand_limits(y=Mse[0])+
  theme_bw() +
  geom_text(aes(label=round(Mse,2)),vjust = -0.5) +
  theme(text = element_text(size = 30))

#Now our mininmum Mse is 2.9 which is higher, which means that for this model we remain with all our variables.

#CROSS VALIDATION


fitted_value<- 0
for(i in 1:nrow(knn.df))
{
  test.set<- knn.df[i,]
  train.set<- knn.df[-i,]
  train.x <- train.set[,-11]
  train.y <- train.set[,11]
  test.x <- test.set[,-11]
  test.y <- test.set[,11]
  model1<-FNN::knn.reg(train = train.x, test = test.x, y = train.y, k = 19) 
  #when you fit the model, use the newdata argument to predict on a new row
  #also, fitted_value needs the index [i], so the each loop doesn't overwrite the previous
  fitted_value[i] <- model1$pred
  
}
knn.mse <- round(mean((fitted_value-test.y)^2),2)
knn.mse
knn.mae <- round(mean(abs(fitted_value-test.y)),2)
knn.mae

#LINEAR REGRESSION

#df copy
lr.df <- Vgsales_df
str(lr.df)

set.seed(5)
train.size <- floor(0.7*nrow(lr.df))
train.index <- sample(1:nrow(lr.df),train.size, replace = F)
train.set <- lr.df[train.index,]
test.set <- lr.df[-train.index,]

reg1 <- lm(Global_Sales ~., data = test.set)
predicted1 = predict(reg1, newdata = test.set)
Mse = mean((test.set$Global_Sales - predicted1)^2)
cat("Mse = ",Mse)

lr.df1 <- lr.df 
lr.df1$Platform <- as.numeric(lr.df1$Platform)
lr.df1$Genre <- as.numeric(lr.df1$Genre)
lr.df1$Publisher <- as.numeric(lr.df1$Publisher)
lr.df1$User_Score <- as.numeric(lr.df1$User_Score)
lr.df1$Developer <- as.numeric(lr.df1$Developer)
lr.df1$Rating <- as.numeric(lr.df1$Rating)
Vgs.cor = cor(lr.df1)
corrplot(Vgs.cor,method = "pie")

reg2 <- lm(Global_Sales ~Critic_Score + Critic_Count + User_Count, data = train.set)
summary(reg2)

predicted2 = predict(reg2, newdata = test.set)
Mse = mean((test.set$Global_Sales - predicted2)^2)
cat("Mse = ",Mse)

par(mfrow=c(2,2))
plot(reg2)

#library(fastDummies)
#mpg = dummy_cols(mpg, select_columns = "origin")

reg3 = lm(Global_Sales ~Platform + Game_Age + Genre + Critic_Score +Critic_Count + User_Count + Rating, data = train.set)
summary(reg3)

predicted = predict(reg3, newdata = test.set)
Mse = mean((test.set$Global_Sales - predicted)^2)
cat("Mse = ",Mse)

par(mfrow=c(2,2))
plot(reg3)

#3 FOLD CROSS VALIDATION

# Define training control
train.control <- trainControl(method = "cv", number = 3)

model <- train(Global_Sales ~., data = lr.df, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)

lr.mae <- 0.79
lr.mse <- 1.66

dt.df <- lr.df #data copy

set.seed(5)
tree.vgs=tree(Global_Sales~Critic_Score + Critic_Count + User_Count,dt.df,subset=train.index)
summary(tree.vgs)

#Plotting Regression Tree

plot(tree.vgs)
text(tree.vgs,pretty=0, cex = 2)

# Cross - Validation Analysis

cv.vgs=cv.tree(tree.vgs)
plot(cv.vgs$size,cv.vgs$dev,type='b')

prune.vgs=prune.tree(tree.vgs,best=6)

plot(prune.vgs)
text(prune.vgs,pretty=0,cex = 2)

yhat=predict(prune.vgs,newdata=dt.df[-train.index,])
yhat2=predict(tree.vgs,newdata=dt.df[-train.index,])
vgs.test=dt.df[-train.index,"Global_Sales"]
plot(yhat,vgs.test)
abline(0,1)

t8 = mean((yhat2-vgs.test)^2)
t6 = mean((yhat-vgs.test)^2)
dt.mae <- mean(abs(yhat-vgs.test))
cat("full tree mse = ", t8,"\n")
cat("6 leaf tree mse = ", t6)
dt.mse <- t8

MAE <- round(c(knn.mae, lr.mae,dt.mae),2)
MSE <- round(c(knn.mse, lr.mse,dt.mse),2)
Models <- c("K - Nearest Neighbors", "Linear Regression", "Decision Tree")

Models <- data.frame(Models,MAE,MSE)
Models

fig(20,8)
require(gridExtra)
plot1 <- ggplot(Models, aes(x=Models,y=MSE,fill=Models))+geom_bar(stat="identity")+
  theme(text = element_text(size=30),legend.position="right",axis.text.x=element_text(angle = 90,vjust = 0.5,hjust = 1,size=15))
plot2 <- ggplot(Models, aes(x=Models,y=MAE,fill=Models))+geom_bar(stat="identity")+
  theme(text = element_text(size=30),legend.position="right",axis.text.x=element_text(angle = 90,vjust = 0.5,hjust = 1,size=15))+expand_limits(y=0.6)
grid.arrange(plot1, plot2, ncol=2)

