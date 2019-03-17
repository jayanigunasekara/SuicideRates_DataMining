###############################################
# (02)Data Understanding ######################
###############################################

library("recommenderlab")
library(ggplot2)
setwd("D:/MLDM/Sem_2/Data Mining Practicals/Data Set/suicide-rates-overview-1985-to-2016")
suicide_rate<- read.csv2("suicide.csv",sep=",",header=TRUE, na.strings = c("","NA"))
summary(suicide_rate)

##############################################
#(03)Data Preparation ########################
##############################################

##Identify the number of null values 
sapply(suicide_rate, function(x) sum(is.na(x)))

##calculate and see the rate of null values
rate_of_null_values<-(sum(is.na(suicide_rate$HDI.for.year))/nrow(suicide_rate))*100

##Remove the column having 30% null values
if (rate_of_null_values >30) {
  suicide_rate$HDI.for.year<- NULL
  df<- suicide_rate
}
colnames(df)

##convert the catogorical data into numeric
##convert sex data into numeric
sex1<-as.factor(c("female","male"))
sex1
unclass(sex1)

##Then apply it to all the variables which are catogorical

must_convert<-sapply(df,is.factor)
M2<-sapply(df[,must_convert],unclass)
catogorical_to_numerical<-cbind(M2,df[,!must_convert])

#Identify the features useful for the analysis
catogorical_to_numerical$country.year<- NULL
catogorical_to_numerical$factor<-NULL

#construct a new data set with required features
df2<-(catogorical_to_numerical)
colnames(df2)

#seperate the data into training and testing
smp_size <- floor(0.75 * nrow(df2))

#set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(df2)), size = smp_size)

train_data <- df2[train_ind, ]
test_data <- df2[-train_ind, ]
dim(train_data)
dim(test_data)
colnames(train_data)

#Feature selection
train_data<-subset(train_data,select=c(country, sex ,age, suicides.100k.pop,generation, year,  gdp_per_capita....))
colnames(train_data)
#normalized the data#
scale_train_data<-scale(train_data, scale=TRUE)

##########################################
#(04)Modeling#############################
##########################################


library(psych)
#correlation between variables
pairs.panels(train_data,col="red")
cor(scale_train_data,method="pearson" )

#Discriptive analysis
win.graph(600,800,10)
library(ggplot2)
qplot(year,suicides.100k.pop,data=train_data[1:150,],color=factor(generation))

#classify using svm algorithm
svm_data<- subset(train_data ,select=c(year,suicides.100k.pop,generation))
library(e1071)
svm_sample<-svm_data[sample(nrow(svm_data), 150),]
mymodel<- svm(factor(generation)~.,data=svm_sample,kernel='radial')
summary(mymodel)
win.graph(600,800,10)
plot(mymodel, data=svm_sample,suicides.100k.pop~year)



##Confusion matrix and Misclassification
pred<- predict(mymodel,svm_sample)
tab<-table(predicted=pred,Actual=svm_sample$generation)
tab
1-sum(diag(tab))/sum(tab)

##tuning
set.seed(123)
tune_model<- tune(svm, factor(generation)~., data=svm_sample, ranges = list(epsilon=seq(0,1,0.1), cost=2^(2:4)))
plot(tune_model)
summary(tune_model)

##best model
best_model<- tune_model$best.model
summary(best_model)
plot(best_model, data=test_data[sample(nrow(svm_data), 150),],suicides.100k.pop~year)

##check the validation 
pred<- predict(best_model,svm_sample)
tab<-table(predicted=pred,Actual=svm_sample$generation)
tab
1-sum(diag(tab))/sum(tab)
##Test suicide number with respect to the country

library(ggplot2)
country_vs_suicide_rate<- aggregate(train_data$suicides.100k.pop, by=list(Country=train_data$country), FUN=sum)
data.frame(country_vs_suicide_rate)
qplot(country_vs_suicide_rate$Country,country_vs_suicide_rate$x )

#kmeans clustering
results<- kmeans(country_vs_suicide_rate,3)
results
results$size
results$cluster

plot(country_vs_suicide_rate$Country,country_vs_suicide_rate$x,col=results$cluster)
legend("bottomright", legend = paste("Group", 1:3), pch=1, col=1:3,cex = 0.5)
countries<- unique(suicide_rate$country)
Risk_analysed<-data.frame("country"=countries,"Suicide_Risk_According_To_The_Country"=results$cluster)
Risk_analysed

library(rworldmap)
library(RColorBrewer)
library(readxl)
library(pals)
##create a world map
worldmap_risk_countries<-joinCountryData2Map(Risk_analysed,nameJoinColumn = "country",joinCode = "NAME")
colourvariation<-RColorBrewer::brewer.pal(10,'Spectral')
risk<-mapCountryData(worldmap_risk_countries,nameColumnToPlot = "Suicide_Risk_According_To_The_Country",
               catMethod = 'fixedwidth',
               colourPalette =colourvariation,
               numCats = 3)

risk$legendText<- c("low","moderate","high")
do.call(addMapLegendBoxes,c(risk,x='bottomleft',title='Risk',horiz=FALSE,cex=0.5))
