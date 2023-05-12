#install required packages
install.packages("dplyr", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE) 
install.packages("statsr", dependencies = TRUE) 
install.packages("Metrics", dependencies = TRUE) 

#load required packages
library(dplyr)
library(ggplot2)
library(statsr)
library(Metrics)

#load the population data
titanic <- read.csv("C:/Users/LENOVO/Downloads/train.csv")
titanic

#count the missing data in age variable
sum(is.na(titanic$Age))


#remove the missing data and i save it in variable called titanic_dropna
#so I can keep both the original dataset and also the modified dataset in the working environment
titanic_dropna=na.omit(titanic)

#summarizes the statistics of the population based on Age
titanic_dropna %>%
  summarise(mu = mean(Age), pop_med = median(Age),
  pop_min=min(Age), pop_max=max(Age), sigma = sd(Age),
  pop_q1=quantile(Age,0.25), #firstquartile,25thpercentile
  pop_q3=quantile(Age,0.75)) #thirdquartile,75thpercentile
#histogram for the dataset's Age variable
ggplot(data=titanic_dropna,aes(x=Age))+geom_histogram(binwidth = 20)

#histogram for the dataset's Fare variable
ggplot(data=titanic_dropna,aes(x=Fare))+geom_histogram(binwidth = 20)

#Q3
#while calculating the average age, we noticed that the Titanic data is incomplete.
#Let’s see who these passengers are
Titanic_na <- titanic %>%
  filter(is.na(Age))

#Separate the age column of the titanic dataset into two groups based on gender
male = Titanic_na$Age[Titanic_na$Sex == 'male']
female = Titanic_na$Age[Titanic_na$Sex == 'female']
missingdata1 <- data.frame(age_male=male)
missingdata2 <- data.frame(age_female=female)

#to see whether those missing passengers survived or not
gender_survived_ratio <- Titanic_na %>%
  group_by(Sex, Survived) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))
gender_survived_ratio
mean(gender_survived_ratio$Count)


#Q7
mean(titanic_dropna$Age)
sd(titanic_dropna$Age)

#Q8
sampl<-titanic_dropna%>%
  sample_n(size=50)

sampl%>%
summarise(mu_sample = mean(Age), sample_med = median(Age),
sd_sample = sd(Age), sample_iqr = IQR(Age),
sample_min=min(Age),sample_max=max(Age),
sample_q1=quantile(Age,0.25),#firstquartile,25thpercentile
sample_q3=quantile(Age,0.75))#thirdquartile,75thpercentile

#Q9
sample_means50 <- titanic_dropna %>%
rep_sample_n(size=50, reps=50, replace=TRUE) %>%
summarise(x_bar = mean(Age))
ggplot(data = sample_means50,aes(x = x_bar)) + geom_histogram(binwidth=2)
mean(sample_means50$x_bar)
sd(sample_means50$x_bar)

#Q10
sample_means100 <- titanic_dropna %>%
rep_sample_n(size=50, reps=100, replace=TRUE) %>%
summarise(x_bar = mean(Age))
ggplot(data = sample_means100,aes(x = x_bar)) + geom_histogram(binwidth=2)
mean(sample_means100$x_bar)
sd(sample_means100$x_bar)

#Q11
sample_means1000 <- titanic_dropna %>%
rep_sample_n(size=50, reps=1000, replace=TRUE) %>%
summarise(x_bar = mean(Age))
ggplot(data = sample_means1000,aes(x = x_bar)) + geom_histogram(binwidth=2)
mean(sample_means1000$x_bar)
sd(sample_means1000$x_bar)

#Q13
sample_means_s20 <- titanic_dropna %>%
rep_sample_n(size=20, reps=1500, replace=TRUE) %>%
summarise(x_bar = mean(Age))
ggplot(data = sample_means_s20,aes(x = x_bar)) + geom_histogram(binwidth=5)
mean(sample_means_s20$x_bar)
sd(sample_means_s20$x_bar)

#Q14
sample_means_s100 <- titanic_dropna %>%
rep_sample_n(size=100, reps=1500, replace=TRUE) %>%
summarise(x_bar = mean(Age))
ggplot(data = sample_means_s100,aes(x = x_bar)) + geom_histogram(binwidth=5)
mean(sample_means_s100$x_bar)
sd(sample_means_s100$x_bar)

#Q15
sample_means_s200 <- titanic_dropna %>%
rep_sample_n(size=200, reps=1500, replace=TRUE) %>%
summarise(x_bar = mean(Age))
ggplot(data = sample_means_s200,aes(x = x_bar)) + geom_histogram(binwidth=5)
mean(sample_means_s200$x_bar)
sd(sample_means_s200$x_bar)


#Q17
#follows chi square
sample_U1500 <- titanic_dropna %>%
rep_sample_n(size=2, reps=1500, replace=TRUE) %>%
summarise(sample_variance = var(Age))
ggplot(data = sample_U1500,aes(x = sample_variance)) + geom_histogram(binwidth=20)


#Q18
#the histogram will be bigger because of the dof
sample_U1500 <- titanic_dropna %>%
rep_sample_n(size=50, reps=1500, replace=TRUE) %>%
summarise(sample_variance = var(Age))
ggplot(data = sample_U1500,aes(x = sample_variance)) + geom_histogram(binwidth=20)

#Q19 MME
n=50
x=rnorm(n, mean = mean(titanic_dropna$Age))
mean_est=sum(x)/n
mean_est
bias(29.69912,mean_est)

#Q19 MLE
sample = rnorm(50,mean=29.69912,sd=14.5265)
NLL = function(pars, data) {
  mu = pars[1]
  sigma = pars[2]
  -sum(dnorm(x = data, mean = mu, sd = sigma, log = TRUE))
}
mle = optim(par = c(mu = 0.2, sigma = 1.5), fn = NLL, data = sample, control = list(parscale = c(mu = 0.2, sigma =1.5)))
mle$par
bias(29.69912,28.59835)

#Q20 MME
n=200
x=rnorm(n, mean =mean(titanic_dropna$Age))
mean_est=sum(x)/n
mean_est
bias(29.69912,mean_est)

#Q20 MLE
sample = rnorm(200,mean=29.69912,sd=14.5265)
NLL = function(pars, data) {
  mu = pars[1]
  sigma = pars[2]
  -sum(dnorm(x = data, mean = mu, sd = sigma, log = TRUE))
}
mle = optim(par = c(mu = 0.2, sigma = 1.5), fn = NLL, data = sample, control = list(parscale = c(mu = 0.2, sigma =1.5)))
mle$par
bias(29.69912,29.40505)

#Q21
#Separate the age column of the titanic dataset into two groups based on gender
male = titanic$Age[titanic$Sex == 'male']
male = na.omit(male)
female = titanic$Age[titanic$Sex == 'female']
female = na.omit(female)
Dataset1age <- data.frame(age_male=male)
Dataset2age <- data.frame(age_female=female)
mean(Dataset1age$age_male)
mean(Dataset2age$age_female)

#sampling distribution for age_male
samplediff_means15000 <- Dataset1age %>%
  rep_sample_n(size=50, reps=15000, replace=TRUE) %>%
  summarise(x_barmale = mean(age_male))
ggplot(data = samplediff_means15000,aes(x = x_barmale)) + geom_histogram(binwidth=20)
mean(samplediff_means15000$x_barmale)

#sampling distribution for age_female
samplediff_means15000 <- Dataset2age %>%
  rep_sample_n(size=50, reps=15000, replace=TRUE) %>%
  summarise(x_barfemale = mean(age_female))
ggplot(data = samplediff_means15000,aes(x = x_barfemale)) + geom_histogram(binwidth=20)
mean(samplediff_means15000$x_barfemale)


#Q22
#Separate the survived column of the titanic dataset into two groups based on gender
survivedmale = titanic_dropna$Survived[titanic_dropna$Sex == 'male']
survivedfemale = titanic_dropna$Survived[titanic_dropna$Sex == 'female']
Dataset1survived <- data.frame(survived_male=survivedmale)
Dataset2survived <- data.frame(survived_female=survivedfemale)

#sampling distribution for survived_male with mean of xbar of males
samplediff_Survived15000 <- Dataset1survived %>%
  rep_sample_n(size=50, reps=15000, replace=TRUE) %>%
  summarise(x_barmale = mean(survived_male))
ggplot(data = samplediff_Survived15000,aes(x = x_barmale)) + geom_histogram(binwidth=20)
mean(samplediff_Survived15000$x_barmale)

#sampling distribution for survived_female with mean of xbar of females
samplediff_Survived15000 <- Dataset2survived %>%
  rep_sample_n(size=50, reps=15000, replace=TRUE) %>%
  summarise(x_barfemale = mean(survived_female))
ggplot(data = samplediff_Survived15000,aes(x = x_barfemale)) + geom_histogram(binwidth=20)
mean(samplediff_Survived15000$x_barfemale)

#sampling distribution for survived_male with sum of xbar of males
samplediff_Survived15000 <- Dataset1survived %>%
  rep_sample_n(size=50, reps=15000, replace=TRUE) %>%
  summarise(x_barmale = sum(survived_male))
ggplot(data = samplediff_Survived15000,aes(x = x_barmale)) + geom_histogram(binwidth=20)
mean(samplediff_Survived15000$x_barmale)

#sampling distribution for survived_female with sum of xbar of females
samplediff_Survived15000 <- Dataset2survived %>%
  rep_sample_n(size=50, reps=15000, replace=TRUE) %>%
  summarise(x_barfemale = sum(survived_female))
ggplot(data = samplediff_Survived15000,aes(x = x_barfemale)) + geom_histogram(binwidth=20)
mean(samplediff_Survived15000$x_barfemale)

#Q23
#ave +- z*se
mean <- 29.69912
sd <- 14.5265
n <- 10
error <- qnorm(0.05, lower.tail = FALSE)*sd/sqrt(n)
mean - error
mean + error

#Q24
#ave +- z*se
mean <- 29.69912
sd <- 14.5265
n <- 50
error <- qnorm(0.05, lower.tail = FALSE)*sd/sqrt(n)
mean - error
mean + error


#Q25
sampltimes <- titanic_dropna%>%
  sample_n(size=200)
sampltimes %>%
  summarise(mu_sample = mean(5*Age), var_sample = var(5*Age))


#Q26
sampladd <- titanic_dropna%>%
  sample_n(size=200)
sampladd %>%
  summarise(mu_sample = mean(5+Age), var_sample = var(5+Age))
