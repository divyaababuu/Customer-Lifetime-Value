library("readxl")
library("ggplot2")
CLV.df <- read_excel("Data_CLV.xlsx", sheet = "Ex2")
head(CLV.df, n=7)
summary(CLV.df) #active is the money earned by the company, p is revenue from customer, c is the cost for the company to provide the service, i is the discount rate and r is the retention rate, AC is acquisition cost

ggplot(CLV.df, aes(x=t, y=active))+geom_line()+ylab("Customer")+xlab("Period")
ggplot(CLV.df, aes(x=t, y=r))+geom_line()+ylab("Retention Rate")+xlab("Period")

CLV.df$CLV <- (CLV.df$p-CLV.df$c)*CLV.df$r/(1+CLV.df$i)^CLV.df$t #formula
head(CLV.df)

ggplot(CLV.df, aes(x = t, y = CLV)) +
  geom_line() + ggtitle("CLV evolution") +
  ylab("CLV") + xlab("Period")

CLV <- apply(CLV.df, 2, sum)
# MARGIN: a vector giving the subscripts which the function will be applied over.
# E.g., 1 indicates rows, 2 indicates columns, c(1, 2) indicates rows and columns.
CLV
CLV[7]

#What happens if retention ratio has a constant value of 0.80 each year?
CLV.df$CLV2 <- (CLV.df$p-CLV.df$c)*0.8^CLV.df$t/(1+CLV.df$i)^(CLV.df$t)
CLV.df

ggplot(CLV.df, aes(x = t, y = CLV2)) + geom_line() +
  ylab("CLV2") + xlab("Period") + ggtitle("CLV 2 Evolution")

CLV <- apply(CLV.df, 2, sum)
CLV
CLV[7]
CLV[8]

#Data Manipulation
library("tidyverse")
library("dplyr")

head(mtcars)
str(mtcars)

#Manipulate Case
#filter() picks cases based on their values.
filter(mtcars, gear == 4) # filtering can be used with all standard logical operators
filter(mtcars, gear == 3 | gear == 4) # i.e. >, <, =>, <=, !=, ==
filter(mtcars, mpg > 21)

#arrange() orders the rows of a data frame by the values of selected columns.
arrange(mtcars, gear)
arrange(mtcars, gear, desc(mpg))
arrange(mtcars, desc(mpg)) # desc sorts in descending order

#select() picks variables based on their names.
select(mtcars, gear, mpg, hp)
select(mtcars, -drat) # - sign before name means remove variable/column

#slice() selects certain rows by row number/
  slice(mtcars, 1:3)

  #sample_n() and sample_frac() select random rows:
  sample_n(mtcars, 5) # numbers of rows to select - it's random
sample_frac(mtcars, .1) #fraction of rows to select - it's random

#mutate() adds new variables that are functions of existing variables.
mutate(mtcars, wt_mpg = wt * mpg)
mutate(mtcars, mpg_mean = mean(mpg), mpg_diff_mean = mpg - mpg_mean)
mutate(mtcars, mpg_mean_diff = mpg - mean(mpg))

#summarise() create a new data frame
summarise(mtcars, sd(disp))
summarise(mtcars, median(mpg), mean(mpg), max(mpg), min(mpg))

#Use group_by() to create a "grouped" copy of a table
group_by(mtcars, cyl)

#Pipe operator %>% lets you take the output of one function and sent it directly to the input of the next function. 
mtcars %>% # take dataframe, then
group_by(gear) %>% # group it by gear, then
summarise(mean(mpg)) # summarise the mean mpg for each level of gear


## 'summarise()' ungrouping output (override with '.groups' argument)
mtcars %>%
  filter(mpg > 21) %>%
  select(gear, mpg, hp) %>%
  arrange(gear)

mtcars_mpg <- mtcars %>%
  filter(mpg > 21) %>%
  select(gear, mpg, hp) %>%
  arrange(gear)
mtcars_mpg


#Redwine - Exercise
redwine <- read.csv("Data_redwine.csv", header=TRUE)
head(redwine)

#Create a data frame containing only wines with an fixed acidity of 8 or greater.
filter (redwine, fixed.acidity>8)
