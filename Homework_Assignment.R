## 1 Summation of i^2 + i^3

x = list(1,2,3)

mylist = list()
for (i in c(10:99)) {
  j = (i^2+i^3)
  mylist[(i-9)] = j
}
Reduce("+", mylist)

## 2 Fair Coin Toss
## Assume Heads = 1, Tails = 0

x = sample((0:1),1,replace=TRUE)
print (ifelse(x==1,"Heads","Tails"))

## in 1 line
print(ifelse(sample((0:1),1,replace=TRUE)==1,"Heads","Tails"))

## Step 2 in #2
total = 0
for (i in c(1:100)) {
  x = sample((0:1),1,replace=TRUE)
  total = total + x
}
total

## 3 Random Numbers
mu = 35 ## average
stdv = 10 ## standard deviation
n = 25 ## sample size students

rnorm(n, mu, stdv)

## Step 3 part 2
# It is creating undesireable decimals

## 4 Pasting Character Vectors
a = c("label")
b = (1:30)
paste(a,b)

## Pasting without spaces using "sep"
a = c("fn")
b = (1:30)
paste(a,b,sep="")

## 5 Random Numbers & Histograms
library(ggplot2)

df = data.frame()
for (i in c(1:1000)) {
x = sum(runif(20, min=0, max=1))
df[i,1] = x
}
names(df)[1] <- 'x'

mean = mean(df$x)
y <- rnorm(1000, mean=mean)
df <- cbind(df, y)

#### Histogram with normal density overlay using GGPLOT
ggplot(df, aes(x)) + geom_histogram(aes(y=..density..)) + geom_density(aes(y), stat="density")


### Histogramwith normal density overlay using hist() + lines()
xx <- seq(min(x), max(x), length=100)
hist(df$x, freq = FALSE, col = "grey")
lines(xx, dnorm(xx,mean=mean))


### 7

churn <- read.csv("/Users/16032/Downloads/churn.arff", skip=15, header=FALSE)
names(churn) <- c("COLLEGE", "INCOME", "OVERAGE", "LEFTOVER", 
                  "HOUSE", "HANDSET_PRICE", "OVER_15MINS_CALLS_PER_MONTH", 
                  "AVERAGE_CALL_DURATION", "REPORTED_SATISFACTION", 
                  "REPORTED_USAGE_LEVEL", "CONSIDERING_CHANGE_OF_PLAN", "LEAVE")


max(churn$INCOME)
churn$INCOME_GRP <- cut(churn$INCOME, c(0,35000,45000,65000,100000,160000))

ggplot(churn) + geom_density(aes(x=OVERAGE, y=..density.., colour = INCOME_GRP))

ggplot(churn, aes(fill=LEAVE, x=INCOME_GRP)) + geom_bar(position="dodge", stat="count")

ggplot(churn, aes(fill=REPORTED_SATISFACTION, x=LEAVE)) + geom_bar(position="dodge", stat="count")

ggplot(churn) + geom_density(aes(x=AVERAGE_CALL_DURATION, y=..density.., colour = LEAVE))

library(dplyr)

churn %>% group_by(LEAVE) %>% summarise(AVG_INCOME = mean(INCOME))
df1 <- churn %>% group_by(LEAVE,REPORTED_SATISFACTION) %>% summarise(n())

df2 <- df1[c(1:5),c(2:3)]
df3 <- df1[c(6:10),c(2:3)]

df4 <- merge(df2,df3,by="REPORTED_SATISFACTION")
names(df4) <- c("Reported Satisfaction", "Number Left", "Number Stayed")

df4

churn %>% group_by(LEAVE) %>% summarise(SD_INCOME = sd(INCOME))