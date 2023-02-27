traindata = read.csv("train.csv")
traindata

barplot(with(traindata, table(traindata$Transported, traindata$HomePlanet)), beside = TRUE, legend=TRUE)
barplot(with(traindata, table(traindata$Transported, traindata$CryoSleep)), beside = TRUE, legend=TRUE)
barplot(with(traindata, table(traindata$Transported, traindata$VIP)), beside = TRUE, legend=TRUE)


head(traindata,3)
str(traindata)
dim(traindata)
datatrain$Transported=as.integer(as.character(datatrain$Transported))

#test

testdata = read.csv("test.csv")
testdata
head(testdata,3)
str(testdata)
dim(testdata)
datatest$Transported = NaN

dim(datatest)

unique(datatrain$Transported)
unique(data$Transported)

data$Transported=as.integer(data$Transported)

##

data = bind_rows(traindata, testdata)
data
head(data,5)
colSums(is.na(data))
colSums(data=='')

data$Age[is.na(data$Age)] <- median(data$Age[!is.na(data$Age)] )
length(data$Age[is.na(data$Age)])

data$RoomService[is.na(data$RoomService)] <- median(data$RoomService[!is.na(data$RoomService)] )
length(data$RoomService[is.na(data$RoomService)])

data$FoodCourt[is.na(data$FoodCourt)] <- median(data$FoodCourt[!is.na(data$FoodCourt)] )
length(data$FoodCourt[is.na(data$FoodCourt)])

data$Spa[is.na(data$Spa)] <- median(data$Spa[!is.na(data$Spa)] )
length(data$Spa[is.na(data$Spa)])

data$VRDeck[is.na(data$VRDeck)] <- median(data$VRDeck[!is.na(data$VRDeck)] )
length(data$VRDeck[is.na(data$VRDeck)])

data$ShoppingMall[is.na(data$ShoppingMall)] <- median(data$ShoppingMall[!is.na(data$ShoppingMall)] )
length(data$ShoppingMall[is.na(data$ShoppingMall)])



data$Cabin[is.na(data$Cabin)] = c("B/0/P")

data$VIP[data$VIP=='']=c("True", "False") 
data$CryoSleep[data$CryoSleep=='']=c("True", "False") #median(data$VIP)#
unique(data$Transported)
data$HomePlanet[data$HomePlanet=='']=c("Europa", "Earth", "Mars") 
data$Destination[data$Destination=='']=c("TRAPPIST-1e", "55 Cancri e")
data$Name[data$Name=='']=c("NoName") 
data$Cabin[data$Cabin=='']=c("B/0/P")#[!is.na(datatrain$ShoppingMall)] )
length(data$Cabin[is.na(data$Cabin) | data$Cabin==''])
unique(data$cabin)
median(data$Cabin)
#data = data %>% clean_names()
library(readr)
library(janitor)
library(DataExplorer)
library(missRanger)
library(GGally)
library(performance)
library(see)
library(Boruta)
library(dlookr)
library(class)
library(caret)
library(gtools)
library(ROCR)
library(car)
library(broom)


clean_names(data)
str(data)
plot_missing(data)
plot(data$Transported)
str(data)
#data[data==""]
data

length(unique(data)
       sapply(data, length(unique(data))
              str(data)
              data$CryoSleep=as.integer(as.logical(data$CryoSleep))
              data$Transported=as.integer(as.character(data$Transported))
              data$VIP=as.integer(as.logical(data$VIP))
              data$PassengerId=as.factor(data$PassengerId)
              unique(data$Transported)
              data =data %>% 
                mutate(HomePlanet = as.factor(HomePlanet),
                       Destination = as.factor(Destination))
              
              data$Passenger_gp = str_sub(data$PassengerId, 1, 4);
              data$passenger_num = as.factor(str_sub(data$PassengerId, 6, 7));
              data$cabin_D = str_sub(data$cabin, 0, 1);
              data$cabin_N = (str_sub(data$cabin, 3, 3));
              data$cabin_S = (str_sub(data$cabin, -1));
              data[c('cabin_D', 'cabin_N', 'cabin_S')] <- [str_sub(data$Cabin, 0, 1), str_sub(data$cabin, 3, 3), str_sub(data$cabin, -1)]
              
              model2 = glm(Transported ~ HomePlanet + CryoSleep + Destination + Age + VIP
                           + RoomService + FoodCourt + ShoppingMall + Spa + VRDeck, data = train, 
                           family = binomial)
              summary(model2)
              