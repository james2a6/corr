rm(list=ls())
cat("\f")  

library(plm)
library(stargazer)
library(car)
library(lmtest)
library(gmm)
library(AER)



#load the dataset
data<- read.csv("final.csv", header = TRUE)
unique(data$year)

attach(data)
library(fastDummies)
data<-cbind(data, dummy_cols(year))
data<-cbind(data, dummy_cols(develop))
pdim(data)

data.panel<-pdata.frame(data, index = c("Countries", "year"))
pdim(data.panel)

data.panel$lgdp = log(data.panel$gdp)


#Different models
#Pooled OLS
attach(data.panel)
pols<- plm(lgdp ~ corr + hdi + pop + factor(develop) + 
             internet + political + fdi+ factor(year) ,
           data=data.panel, model = "pooling")

#RE
re<- plm(lgdp ~ corr + hdi + pop + factor(develop) + 
           internet + political + fdi+ factor(year),
         data=data.panel, model = "random", random.method = "nerlove")

summary(re)

#FD
fd<- plm(lgdp ~ corr + hdi + pop + factor(develop) + 
           internet + political + fdi+ factor(year),
         data=data.panel, model = "fd")

#FE
fe<- plm(lgdp ~ corr + hdi + pop + factor(develop) + 
           internet + political + fdi+ factor(year),
         data=data.panel, model = "within")

stargazer(pols, fd, fe, re, type= "text", 
          keep.stat = c("n","rsq"), omit=c("year"),omit.labels=c("Year Dummies"), 
          title = "Pooled OLS, FD, FE , RE")


#Which estimator is good
bptest(pols)

#reject the null, therefore there is variance in 
#the unobserved heterogeneity, hence we cannot consider Pooled OLS as a model

# Serial correlation test among the errors
data <-na.omit(data)
data.panel<- na.omit(data.panel)
data$pols.res<-pols$residuals


data$pols.lres<-lag(data$pols.res)
sc.pols<-lm(pols.res~pols.lres, data=data)
summary(sc.pols)
#reject the null, hence Pooled OLS is dropped


# FE VS RE
phtest(fe,re)

#fail to reject the null, RE is preferred

# FE VS FD
pwfdtest(fd)
#reject the null, serial correlation among differenced errors
#FE is preferred

# FE Serial Correlation Test
pwartest(fe)



#RE interpretation
re1<- plm(lgdp ~ corr + hdi + pop + .data_1*corr + .data_2*corr + .data_3*corr +
            internet + political + fdi+ factor(year),
          data=data.panel, model = "random", random.method = "nerlove")
summary(re1)



#POLS
pols1<- plm(lgdp ~ corr + hdi + pop + .data_1*corr + .data_2*corr + .data_3*corr +
            internet + political + fdi+ factor(year),
          data=data.panel, model = "pooling")

#FD
fd1<- plm(lgdp ~ corr + hdi + pop + .data_1*corr + .data_2*corr + .data_3*corr +
              internet + political + fdi+ factor(year),
            data=data.panel, model = "fd")

#FE
fe1<- plm(lgdp ~ corr + hdi + pop + .data_1*corr + .data_2*corr + .data_3*corr +
              internet + political + fdi+ factor(year),
            data=data.panel, model = "within")

#RE
re1<- plm(lgdp ~ corr + hdi + pop + .data_2*corr + .data_3*corr +
              internet + political + fdi+ factor(year),
            data=data.panel, model = "random")
summary(re1)



#Which estimator is good
bptest(pols1)

#reject the null, therefore there is variance in 
#the unobserved heterogeneity, hence we cannot consider Pooled OLS as a model

# Serial correlation test among the errors
data <-na.omit(data)
data.panel<- na.omit(data.panel)
data$pols.res<-pols1$residuals


data$pols.lres<-lag(data$pols.res)
sc.pols<-lm(pols.res~pols.lres, data=data)
summary(sc.pols)
#reject the null, hence Pooled OLS is dropped


# FE VS RE
phtest(fe1,re1)

#fail to reject the null, RE is preferred

# FE VS FD
pwfdtest(fd1)
#reject the null, serial correlation among differenced errors
#FE is preferred

# FE Serial Correlation Test
pwartest(fe1)

