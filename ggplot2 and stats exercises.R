demo <- read.csv("demographics.csv")

View(demo)

### how to filter (select) your data in base R using subsets

### a new data frame, demo2, will be created each time

### keep the married subjects only (one filter variable)

demo2 <- subset(demo, marital == "Married")


### retain the married subjects aged over 35 (two filter variables)

demo2 <- subset(demo, marital == "Married" & age > 35)



### keep the first three variables only (age, marital status, income)

demo3 <- subset(demo, marital == "Married" & age > 35, select = c(1:3))



### drop variables 4, 5, 6 and 8 (education, car price, car category, retired)

demo4 <- subset(demo, marital == "Married" & age > 35, select = -c(4:6, 8))



### how to filter (select) your data using the dplyr package

### a new data frame, demo2, will be created each time

### load the package

require(dplyr)

### keep the unmarried subjects only (one filter variable)

demo3 <- filter(demo, marital == "Unmarried")


### keep the unmarried subjects only aged under 50 (two filter variables)

demo2 <- filter(demo, marital == "Unmarried", age < 50)



### if you want to keep some variables only,
### you must first specify the variables you want to keep

### suppose we want to keep only the first three variables (age, marital status, income)

demo2 <- select(demo, age, marital, income)


### next we filter our new data frame demo2, 
### keeping only the unmarried persons aged under 50

demo2 <- filter(demo2, marital == "Unmarried", age < 50)



### how to recode a continous variable into a factor

### we want to create a categorical variable as follows
### subjects with income under 200 - low income
### subjects with income of 200 and more - high income

### a new variable, incat (income category), will be created

demo$incat[demo$income<200] = "Low income"
demo$incat[demo$income>=200] = "High income"



### now we want to create three groups by income
### low income - under 150
### medium income - between 150 and 300
### high income - 300 and more
### so we will have two cut points: 150 and 300

### a new variable, incat2, will be created

demo$incat2 = cut(demo$income, breaks=c(-Inf, 150, 300, Inf), labels=c("Low income", "Medium income", "High income"))




### how to sort a data frame. A new data frame, demo2, will be created each time

### sort by income, ascending (default)

demo2 <- demo[order(demo$income),]



### sort by income, descending

demo2 <- demo[order(-demo$income),]



### sort by income and age 

demo2 <- demo[order(demo$income, demo$age),]



### sort by income (ascending) and age (descending)

demo2 <- demo[order(demo$income, -demo$age),]



##########
### how to create frequency tables
##########

### we will build a table for the variable educ (education level)
### this table will contain the following:
### absolute frequencies (counts), cumulative absolute frequencies,
### relative frequencies and cumulatitve relative frequencies

### create the initial table (with the counts only)

mytable <- table(demo$educ, exclude = NULL)  ### the missing values will be excluded

print(mytable)
barplot(mytable)
### compute the cumulative counts (using the cumsum fuction)

cumul <- cumsum(mytable)

print(cumul)

### compute the relative frequencies

relative <- prop.table(mytable)

print(relative)

### compute the cumulative relative frequencies

n <- nrow(demo)  ### number of rows (cases) of the data frame demo

cumulfreq <- cumul/n

print(cumulfreq)

### create the final table with the cbind function

mytable2 <- cbind(Freq=mytable, Cumul=cumul, Relative=relative, CumFreq=cumulfreq)

print(mytable2)

###
### the commands above can be used with numeric variables as well
###


##########
### how to create cross-tables (with the xtabs function) 
##########

### we will build a cross table with the variables gender and carcat (car category)

ct <- xtabs(~gender+carcat, data=demo)

ftable(ct)


##########
### how to build a histogram with ggplot2
##########

### we will build the histogram for the variable income

library(ggplot2)
### build the histogram
### on the y axis we will represent the counts (absolute frequencies)

ggplot()+geom_histogram(data=demo, aes(x=income))

### change the bins color and border

ggplot()+
  geom_histogram(data=demo, aes(x=income), fill="red", color="black")

### represent the density on the y axis (relative frequencies)

ggplot()+
  geom_histogram(data=demo, aes(x=income, y=..density..), fill="red", color="black")

##########

### create a facet grid (multiple histograms)
### we will create a histogram for each combination of the variables 
### gender and marital status

ggplot()+
  geom_histogram(data=demo, aes(x=income, y=..density..), fill="red", color="black")+
  facet_grid(gender~marital)

### create multiple histograms on the same chart
### we will build a histogram for each gender category

ggplot()+
  geom_histogram(data=demo, aes(x=income, y=..density.., fill=gender), color="black")

### N. B. the "fill" variable must be a factor


##########
### how to build cumulative frequency line charts with ggplot2 
##########

### we will create a cumulative frequencies line for the variable income

### load the packages

require(ggplot2)

require(plyr)   ### we need the count function

### create a data frame with the unique income values

mydata <- count(demo, 'income')

View(mydata)

### compute the cumulative counts and percentages

cumul <- cumsum(mydata$freq)

cumperc <- cumul/nrow(demo)

### add the cumulative frequencies column to the iniatial mydata matrix

mydata <- cbind(mydata, cumperc)

View(mydata)

### plot the cumulative frequencies line (smooth)

ggplot()+geom_line(data=mydata, aes(x=income, y=cumperc))

### OR plot the stepped line

ggplot()+geom_step(data=mydata, aes(x=income, y=cumperc))

#################

### create grouped cumulative frequencies lines
### we will build a cumulative frequencies line chart for the variable income
### for each gender group

## first we create two databases, by gender, using the brackets

male <- demo[demo$gender=="Male",]

female <- demo[demo$gender=="Female",]

View(male)

View(female)

### for the male data frame, we get the unique income values
### then compute the cumulative relative frequencies

mydata_male <- count(male, "income")

cumulm <- cumsum(mydata_male$freq)

cumpercm <- cumulm/nrow(male)

### add the cumulative relative frequencies column

mydata_male <- cbind(mydata_male, cumpercm)

View(mydata_male)

### the same for the female data frame

mydata_female <- count(female, "income")

cumulf <- cumsum(mydata_female$freq)

cumpercf <- cumulf/nrow(female)

mydata_female <- cbind(mydata_female, cumpercf)

View(mydata_female)

### now we can build the chart

ggplot()+geom_line(data=mydata_male, aes(x=income, y=cumpercm), color="red")+
  geom_line(data=mydata_female, aes(x=income, y=cumpercf), color="blue")

### for a stepped line we must replace geom_line with geom_step

### add a legend to the chart

lgd <- scale_color_manual("Legend", values=c(Male="red", Female="blue"))

ggplot()+
  geom_line(data=mydata_male, aes(x=income, y=cumpercm, color="Male"), size=1.3)+
  geom_line(data=mydata_female, aes(x=income, y=cumpercf, color="Female"), size=1.3)+
  lgd




##########
### how to build column charts with ggplot2 
##########

### we will create a column chart representing the average income
### for each education level

### load the package

require(ggplot2)

### build the chart

ggplot(demo, aes(x=educ, y=income, fill=educ))+ 
  stat_summary(fun.y=mean, geom="bar")

### if you want the same color for the bins

ggplot(demo, aes(x=educ, y=income))+ 
  stat_summary(fun.y=mean, geom="bar", fill="red")

### N.B. if the grouping variable is not a factor,
### we must convert it into a factor

### to create a clustered bar chart (by the variable gender)
### position_dodge will put the columns side by side


ggplot(demo,aes(x=educ, y=income, fill=gender)) + 
  stat_summary(fun.y=mean, geom="bar", position=position_dodge())

### to stack the columns we use position_stack

ggplot(demo,aes(x=educ, y=income, fill=gender)) + 
  stat_summary(fun.y=mean, geom="bar", position=position_stack())



##########
### how to build boxplot charts with ggplot2 
##########

### we will create a boxplot for the variable income
### for each gender category

### load the package

require(ggplot2)

### create the plot

ggplot()+geom_boxplot(data=demo, aes(x=gender, y=income))+
  scale_x_discrete(labels=c("Female", "Male"))

### N.B. if the grouping variable is not a factor
### make sure you convert it into a factor first

### set the color of the outliers

ggplot()+geom_boxplot(data=demo, aes(x=gender, y=income), outlier.colour="red")+
  scale_x_discrete(labels=c("Female", "Male"))

### set the shape of the outliers

ggplot()+geom_boxplot(data=demo, aes(x=gender, y=income), outlier.colour="red", outlier.shape=4)+
  scale_x_discrete(labels=c("Female", "Male"))

### add a legend

lgd <- demo$gender

ggplot()+geom_boxplot(data=demo, aes(x=gender, y=income, fill=lgd), outlier.colour="red")+
  scale_x_discrete(labels=c("Female", "Male"))

###########

### build a clustered boxplot
### we will group the boxplots by gender and marital status

### the legend will represent the two marital statuses

lgd <- demo$marital

ggplot()+geom_boxplot(data=demo, aes(x=gender, y=income, fill=lgd))+
  scale_x_discrete(labels=c("Female", "Male"))


hw= read.csv("hw.csv")

##########
### how to build scatterplot charts with ggplot2 
##########

### we will create a scatterplot with the variables
### height and weight

### load the package

require(ggplot2)

### create the plot

ggplot()+geom_point(data=hw, aes(x=height, y=weight))+
  scale_x_continuous(limits=c(150,193))

#########

### build a clustered scatterplot
### by gender

lgd <- hw$gender

### get points of different colors

ggplot()+geom_point(data=hw, aes(x=height, y=weight, color=lgd))+
  scale_x_continuous(limits=c(150,193))

### get points of different shapes

ggplot()+geom_point(data=hw, aes(x=height, y=weight, shape=lgd))+
  scale_x_continuous(limits=c(150,193))

### get points of both different shapes and colors

ggplot()+geom_point(data=hw, aes(x=height, y=weight, shape=lgd, color=lgd))+
  scale_x_continuous(limits=c(150,193))

###########

### add a trendline to the scatterplot

### create a linear model
### with weight as the dependent variable and height as the explainer

model <- lm(weight~height, data=hw)

print(model)

### get the minimum and the maximum height

minh <- min(hw$height)

maxh <- max(hw$height)

### create a new vector height

height <- c(minh, maxh)

print(height)

### predict the weight based on the height, with the model above

fit <- predict(model, data.frame(height))

print(fit)

### create a data frame with the line end points

endpoints <- data.frame(height, fit)

View(endpoints)

### build the scatter plot with trend line

ggplot()+
  geom_point(data=hw, aes(x=height, y=weight))+
  geom_line(data=endpoints, aes(x=height, y=fit), color="red", size=1)


