library(dplyr)
library(ggplot2)
library(corrgram)
library(car)
setwd("C:\\Users\\FLIP-L014\\Documents\\R Practice\\EDA\\T2\\Code_Snippets")
## Example: Predicting Medical Expenses ----
## Step 2: Exploring and preparing the data ----
insurance <- read.csv("insurance.csv", 
                      stringsAsFactors = TRUE)

str(insurance)

summary(insurance)

#Does the data have any anomolies?


#Finding outliers & imputing them with mean
#we should take a call on them based on business domain

summary(insurance$charges)

chrgbox=boxplot(insurance$charges)
chrgout=chrgbox$out #139 values are turning out be outliers
length(chrgout)

quantile(insurance$charges,p=c(1:10)/10) #Top 10 percentile values of charges are turning out to be outliers

#For now I am not going to impute them. By imputing the outliers, especially DV,data may become biased

#It's not a good idea idea to impute observations of DV variable


#----------------------code to impute--------------------------#

#chrgbox$stats[5] #3rd IQR



#index1=which(insurance$charges %in% chrgout)

#mean(insurance$charges) #Mean value is very less

#We may add bias if we impute outliers with the mean
#Instead, let's impute them with 3rd IQR

#insurance$charges[index1]=chrgbox$stats[5] #imputing with 3rd IQR


#insurance$charges[index1]=mean(insurance$charges) #imputing with mean


summary(insurance$bmi)
bmibox=boxplot(insurance$bmi)
bmiout=bmibox$out
length(bmibox)

#Let's impute outlies of bmi variable with mean
index2=which(insurance$bmi %in% bmiout) #gives the positions in the data where outliers are present
insurance$bmi[index2]=mean(insurance$bmi,na.rm = TRUE) #---Shortlist the outliers from the dataset & impute


summary(insurance$age) 
x<-boxplot(insurance$age)
x

list<-x$out


index<-which(insurance$age %in% list)


insurance$age[index]

mean_sw<-mean(insurance$age,na.rm=TRUE)
insurance$age[index]<-mean_sw


#----------------missing value treatment---------------------#

#Check for missing values
colSums(is.na(insurance))

#bmi vriable has 2 missing values, since the no.of observations with missing values are very less
#compared to total no.of observations, we can afford to remove those 2 observations from our data

#insuranceNew<-na.omit(insurance) #omit observations
#dim(insurance)
#dim(insuranceNew)

#Another way is to impute missing values with mean
# insurance$bmi[is.na(insurance$bmi)]<-mean(insurance$bmi,na.rm=TRUE)

#A better way would be to impute with bmi value of a similar gorup based on DV
#By this way we are not going to add any bias

index3<-which(is.na(insurance$bmi))
insurance[index3,7]

quantile(insurance$bmi,c(p=(1:10)/10),na.rm = TRUE)

insurance%>%mutate(quantile=ntile(bmi,10))%>%group_by(charges,quantile)%>%summarise(N=n())%>%arrange(-charges)->dat1
class(dat1)

filter(dat1,charges>=10797.336 & charges<=11000) #filtering charges around our missing value and the respective bmi quantile they belong to

quantile(insurance$bmi,c(p=(1:10)/10),na.rm = TRUE)

(32.1000+33.6800)/2 #avg. of closest quantile

insurance[17,3]<-32.89


filter(dat1,charges>=2395 & charges<=2400)
(22.99+25.36)/2
insurance[18,3]<-24.17

colSums(is.na(insurance))


#Using randomForest package we can impute missing values


#install.packages("randomForest")
#library("randomForest")                   
#Using R package for dealing missing values
#insurance$bmi<-na.roughfix(insurance$bmi) 
#Quite useful while working with large datasets##median values


#----------------------Data Exploration/Profiling--------------#


#tabulations for categorical variables
table(insurance$sex)
table(insurance$region)
table(insurance$smoker)

table(insurance$sex,insurance$region,insurance$smoker)

#Data Aggregations

#group by multiple values 
aggregate(charges ~ sex , data=insurance, FUN=mean) #There is some difference in insurnace charges paid by female & male
aggregate(charges ~ smoker , data=insurance, FUN=mean) #People who smoke are paying pretty high charges compared to non smokers
aggregate(charges ~ region , data=insurance, FUN=mean) #People from southeast region are paying high insurance charges followed by northeast

aggregate(charges ~ region + smoker + sex, data=insurance, FUN=mean)
table(insurance$region,insurance$smoker)/nrow(insurance)#southeast has more no.of smoker. This could be the reason for mean charges paid by people from this region is high


#Data Visualisations

# summarize the charges variable
summary(insurance$charges)

# histogram of insurance charges
hist(insurance$charges) #positively skewed

hist(log(insurance$charges)) #log distribution of charges is close to normal distribution


ggplot(insurance,aes(x=charges)) + geom_histogram(aes(fill=as.factor(smoker)),position="dodge")
ggplot(insurance,aes(x=charges)) + geom_histogram(aes(fill=as.factor(smoker)),position="dodge") + facet_grid(region~sex)
ggplot(insurance,aes(x=charges)) + geom_histogram(aes(fill=as.factor(sex)),position="dodge")

#box plots : 
ggplot(insurance,aes(y=charges,x=sex,fill=as.factor(sex)))+ geom_boxplot() #Median charges paid by mal & female are same but the charges tend to vary for male than female
ggplot(insurance,aes(y=charges,x=sex,fill=as.factor(sex)))+ geom_boxplot() +facet_grid(region~smoker)

#scatter plots

#bmi vs charges
p<-ggplot(insurance,aes(x=bmi,y=charges,color=as.factor(sex)))
p+geom_point() #We can the lsight upward trend
p+geom_point()+facet_grid(.~smoker) #Very clear upward trend b/w bmi & charges for male & female somkers b
p+geom_point()+facet_grid(smoker~region)

#age vs charges
unique(insurance$age)
p<-ggplot(insurance,aes(x=age,y=charges,color=as.factor(sex)))
p+geom_point()
p+geom_point()+facet_grid(.~smoker)
p+geom_point()+facet_grid(smoker~region)

p+geom_point()+facet_grid(.~children)


attach(insurance)

cor(charges,bmi)
cor(charges,age)
cor(charges,children)


corrgram(insurance) #from corrgram package

# Red means Negative correlation 
# Blue means positive correaltion 

# Darker the color stronger the correlation

# exploring relationships among features: correlation matrix
cor(insurance[c("age", "bmi", "children", "charges")])

# visualing relationships among features: scatterplot matrix
pairs(insurance[c("age", "bmi", "children", "charges")])

#Using visualizations, tabulations & correlations we can conclude that smoker & age are correlated with charges
#Data Preparation
#Adding new variables

# add a higher-order "age" term
insurance$age2 <- insurance$age^2
cor(insurance$charges,insurance$age2) #not much difference even after transformation

# add an indicator for BMI >= 30
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)
aggregate(charges ~ bmi30,data=insurance,FUN=mean) #There is a some difference in the charges paid by two categories


unique(insurance$children)

table(as.factor(insurance$children))

insurance$child_cat=ifelse(insurance$children<=2,1,2)

aggregate(charges~child_cat, data=insurance, FUN=mean) #People with more than 2 children are paying slightly higher insurance charges


#Split the data into tain & test samples
set.seed(200)
sampling<-sort(sample(nrow(insurance), nrow(insurance)*.7))
head(sampling)
length(sampling)



train<-insurance[sampling,]
test<-insurance[-sampling,]
nrow(train)
nrow(test)


reg1<-lm(formula = charges ~age + bmi + sex+ children + smoker + region, data = train)
summary(reg1) 


#Let's remove insignificant varibale & run the model again 
reg2<-lm(formula = charges ~age + bmi + children + smoker + region, data = train)
summary(reg2)



#Our DV (charges) is not distrubuted normally. 
#Let's log transformation and see if we can improve the model

hist(insurance$charges)
hist(log(insurance$charges)) #Looks better

reg3<-lm(formula = log(charges) ~ age + sex + bmi + children + smoker + region, data = train)
summary(reg3)#Slight improvement in R^2 value


#Let's create dummy variables for region
x<-factor(insurance$region) #Using factor and model.matrix combination
dummies<-model.matrix(~x)
class(dummies)

y<-data.frame(dummies)

insurance<-cbind(insurance,y)



set.seed(200)
sampling<-sort(sample(nrow(insurance), nrow(insurance)*.7))
head(sampling)
length(sampling)



train<-insurance[sampling,]
test<-insurance[-sampling,]
nrow(train)
nrow(test)

reg4<-lm(formula = log(charges) ~ age + sex + bmi + children + smoker + xnorthwest
         +xsoutheast + xsouthwest, data = train)
summary(reg4) #not much change in R^2



hist(reg4$residuals)
plot(reg4$residuals)
plot(reg4$fitted.values,reg3$residuals) #There seems to be clear pattern, which suggests that the model is not good




#Let's try to add quadradic variable for age.There might be exponentional increase in charges when age increases after certain threshold
plot(insurance$charges,insurance$age)
insurance$age2<-(insurance$age)^2

#Bin bmi varibale into 2 categories
insurance$bmi30<-ifelse(insurance$bmi>=30,1,0)

#add an interaction variable bmi*smoker
#there might be higher effect on charges, when a person is a smoker with high bmi

reg5<-lm(formula = log(charges) ~ age +  age2 + sex + bmi + bmi30 + bmi*smoker + children + smoker + xnorthwest
         +xsoutheast + xsouthwest, data = train)
summary(reg5)

#let's add one more interaction variables age*smoker




reg6<-lm(formula = charges ~ age+ age2+ +bmi30 + bmi*smoker + age*smoker+ children + smoker + xnorthwest
         +xsoutheast + xsouthwest, data = train)

summary(reg6) 

#Let's remove insignificant variables from the above model and train the model again to if there is any improvement in the R^2 value

reg7<-lm(formula = charges ~ age + age2+ +bmi30 + bmi*smoker + children + smoker + xnorthwest
         +xsoutheast + xsouthwest, data = train)

summary(reg7) #The best model. All the variables are significant

plot(reg7$fitted.values,reg7$residuals) #there seems to be slight downward pattern which suggests heteroscedasticity

#check for multicollinearity
vif(reg7)


#There is multicollinearity because of the interaction varible


#Bin children variable into 2 categories
reg9<-lm(formula = charges ~ age+ age2+ +bmi30 + +smoker + bmi*smoker + +children + child_cat+ xnorthwest
         +xsoutheast + xsouthwest, data = train)

summary(reg9)
plot(reg9$fitted.values,reg9$residuals) #Heteroscedasticity still exists

vif(reg9)
qqnorm(reg9$residuals)

#let's standardize age & bmi variables to avoid multicollinearity due to higher order temr age^2

summary(train$age3)
insurance$age3=insurance$age-(mean(insurance$age))
insurance$bmi2=insurance$bmi-(mean(insurance$bmi))

reg10<-lm(formula = charges ~ age3 + age2 + bmi2 +bmi30 + bmi2*smoker + +children + child_cat+ xnorthwest
         +xsoutheast + xsouthwest, data = train)

summary(reg10)

plot(reg10$residuals)
plot(reg10$fitted.values,reg10$residuals)

vif(reg10) 


cor(predicted,actual) #correleation b/w predicted & actual values is high, which shows that the model is predicting well

predicted<-reg10$fitted.values
actual<-(train$charges)

dat<-data.frame(predicted,actual)

plot(dat$predicted,dat$actual)

abline(a=0,b=1, col="red")#most of the data points are close to the line




#Test the model on test dataset


pred<-predict(reg10,newdata = test)

test$predicted<-pred

plot(test$predicted,test$charges)
abline(a=0,b=1, col="red")


test$error<-(test$charges-test$predicted)/test$charges

mean(test$error)*100 #Error percentage seems okay

cor(test$charges,test$predicted)
