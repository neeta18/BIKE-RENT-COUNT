
#set working directory and load libraries
rm(list=ls(all=T))
setwd("C:/Users/NANINE/Desktop/EdWisor")
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','fastDummies')
lapply(x, require, character.only = TRUE)
rm(x)


#read the bike dataset
bike_data = read.csv("day.csv", header = T, na.strings = c(" ", "", "NA"))
View(bike_data)


######Exploratory Data Analysis##################
#convert categorical data's in factor
bike_data$season=as.factor(bike_data$season)
bike_data$mnth=as.factor(bike_data$mnth)
bike_data$yr=as.factor(bike_data$yr)
bike_data$holiday=as.factor(bike_data$holiday)
bike_data$weekday=as.factor(bike_data$weekday)
bike_data$workingday=as.factor(bike_data$workingday)
bike_data$weathersit=as.factor(bike_data$weathersit)
bike_data=subset(bike_data,select = -c(instant,casual,registered,dteday))
str(bike_data)
View(bike_data)


###Missing Values Analysis###############################################
# 1. checking for missing value
missing_val = data.frame(apply(bike_data,2,function(x){sum(is.na(x))}))
missing_val
#no missing values found


##############Outlier Analysis##########
# 1.BoxPlots - Distribution and Outlier Check
numeric_index = sapply(bike_data,is.numeric) #selecting only numeric

numeric_data = bike_data[,numeric_index]

cnames = colnames(numeric_data)

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(bike_data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "blue" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="cnt")+
           ggtitle(paste("Box plot of count for",cnames[i])))
}

gridExtra::grid.arrange(gn1,gn2,ncol=3)
gridExtra::grid.arrange(gn3,gn4,ncol=2)
#outliers in hum and windspeed observed and removed



#####################Feature Selection#################
## Correlation Plot 
corrgram(bike_data[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")



#Dimension Reduction####
#temp and atemp are highly correlated ,so lets drop atemp variable
bike_data= subset(bike_data,select = -c(atemp))


#converting multilevel categorical variable into binary dummy variable
cnames= c("season","yr", "mnth","holiday","workingday","weekday","weathersit")
bike_data_new=bike_data[,cnames]
cnt=data.frame(bike_data$cnt)
names(cnt)[1]="cnt"
bike_data_new <- fastDummies::dummy_cols(bike_data_new)
bike_data_new= subset(bike_data_new,select = -c(season,yr,mnth,holiday,workingday,weekday,weathersit))
d3 = cbind(bike_data_new,bike_data)
d3= subset(d3,select = -c(season,yr,mnth,holiday,workingday,weekday,weathersit,cnt))
bike_data_new=cbind(d3,cnt)
View(bike_data_new)


#split train and test
set.seed(1234)
train_index = sample(1:nrow(bike_data_new), 0.7 * nrow(bike_data_new))
train = bike_data_new[train_index,]
test = bike_data_new[-train_index,]


#MODEL DEVELOPMENT
#Decision tree regression  
fit = rpart(cnt ~ ., data = train, method = "anova")
predictions_DT = predict(fit, test[,-36])

#Random Forest Model
RF_model = randomForest(cnt ~ ., train, importance = TRUE, ntree = 200)
predictions_RF = predict(RF_model, test[,-36])
plot(RF_model)

#Linear regression model making
lm_model = lm(cnt ~., data = train)
predictions_LR = predict(lm_model,test[,-36])
plot(lm_model)


#evaluating MApe value
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))*100
}
MAPE(test[,36], predictions_DT)

MAPE(test[,36], predictions_RF)

MAPE(test[,36],  predictions_LR)

#lets check which variables are most important for modelling

varImpPlot(RF_model)

#Model development only with seven important variables
c_imp= c("temp","yr_0", "season_1","yr_1","hum","windspeed","mnth_1","cnt")
data_imp=bike_data_new[,c_imp]
View(data_imp)


#split train and test
set.seed(1234)
train_index_imp = sample(1:nrow(data_imp), 0.7 * nrow(data_imp))
train_imp = data_imp[train_index_imp,]
test_imp = data_imp[-train_index_imp,]

#MODEL DEVELOPMENT
###########Decision tree regression  #################
fit_imp = rpart(cnt ~ ., data = train_imp, method = "anova")
predictions_DT_imp = predict(fit_imp, test_imp[,-8])

#############Random Forest Model##########################
RF_model_imp = randomForest(cnt ~ ., train_imp, importance = TRUE, ntree = 200)
predictions_RF_imp = predict(RF_model_imp, test_imp[,-8])


#Linear regression model making
lm_model_imp = lm(cnt ~., data = train_imp)
predictions_LR_imp = predict(lm_model_imp,test_imp[,-8])

#evaluating MAPE value
MAPE(test_imp[,8], predictions_DT_imp)

MAPE(test_imp[,8], predictions_RF_imp)

MAPE(test_imp[,8],  predictions_LR_imp)


