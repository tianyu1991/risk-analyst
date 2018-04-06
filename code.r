training.data.raw <- read.csv('risk_interview_prompt.csv',header=T,na.strings=c(""))
sapply(training.data.raw,function(x) sum(is.na(x)))
sapply(training.data.raw, function(x) length(unique(x)))
##Taking care of the missing values
data <- subset(training.data.raw,select=c(2:8))

data$state<-as.factor(substr(data$state,1,2))

data$fico[is.na(data$fico)] <- mean(data$fico,na.rm=T)
data$housing_payment[is.na(data$housing_payment)] <- mean(data$housing_payment,na.rm=T)
is.factor(data$booked)
is.numeric(data$rate)


##Clean Rate from factor to Numeric
Rate<- gsub("%","" , as.character(data$rate),ignore.case = TRUE)
data$rate<-as.numeric(Rate)
data$rate[is.na(data$rate)] <- mean(data$rate,na.rm=T)

sapply(data,function(x) sum(is.na(x)))


train <- data[1:4000,]
test <- data[4000:4999,]

model <- glm(booked ~.,family=binomial(link='logit'),data=train)
