#Reading the dataset
Denco_Corp <- read.csv("CustomerPartSales.csv")

#activate the installed libraries
library(dplyr)    # This library is to convert into character, Data manipulation
library(Amelia)   # To handle missing data
library(ggplot2)  # For Plotting, Data visualization
library(caTools)  # For splitting the data
library(ROCR)
library(class)
library(randomForest)
library(rpart)
library(rpart.plot)
library(caret)  #For confusion matrix
library(mlr)
library(knitr)
library(corrplot)
install.packages("tidyverse")
library(tidyverse) #For reordering the BAR chart

#Creating the subset for filteration process for Task-1
Denco1 <- subset(Denco_Corp, NUM_TRANSACTION_FOR_THIS_CUSTNAME > 125 & MARGIN_PERC > 80)

str(Denco1)

#Converting chr data into factor data type
Denco1$PARTNUM = as.factor(Denco1$PARTNUM)
Denco1$CUSTNAME = as.factor(Denco1$CUSTNAME)

#Bar Plot for finding out the loyal customers in Task-1
ggplot(Denco1, aes(x=CUSTNAME, y=NUM_TRANSACTION_FOR_THIS_CUSTNAME, fill=CUSTNAME))+ 
  stat_summary(fun=mean, geom="bar")

#Pie chart for Task-2 High Revenue Customer
#Data frame creation
df <- data.frame (
  "customer" = c ("TRIUMPH INSULATION", "CORNING SHARED SERVICES", "THERMAL PRODUCTS INC", "All others"),
  "SHARE_OF_TOTALREVENUE_FROM_THIS_CUST" = c (14.96, 5.40, 3.03, 76.61)
)
head(df)

#For positioning of Labels
df <- df %>%
  arrange(desc(customer)) %>%
  mutate(lab.ypos = cumsum(SHARE_OF_TOTALREVENUE_FROM_THIS_CUST) - 0.5*SHARE_OF_TOTALREVENUE_FROM_THIS_CUST)

#For pie chart firt need to draw bar plot
bp<- ggplot(df, aes(x="", y=SHARE_OF_TOTALREVENUE_FROM_THIS_CUST, fill=customer))+
  geom_bar(width = 1, stat = "identity")
bp

# pie chart by coord_polar function
pie <- bp + coord_polar("y", start=0)+ geom_text(aes(y = lab.ypos, label = SHARE_OF_TOTALREVENUE_FROM_THIS_CUST), color = "white")
pie

#Donut chart for Task-3, High revenue Parts
#Creation of data frame
df1 <- data.frame (
  "parts" = c ("733648000", "734370000", "733649000", "All others"),
  "SHARE_OF_TOTAL_REVENUE_FROM_THIS_PART" = c (12.53, 5.31, 2.37, 79.79)
)
head(df)

#For positioning of Labels
df1 <- df1 %>%
  arrange(desc(parts)) %>%
  mutate(lab.ypos = cumsum(SHARE_OF_TOTAL_REVENUE_FROM_THIS_PART) - 0.5*SHARE_OF_TOTAL_REVENUE_FROM_THIS_PART)
df1
#Manual colors selection for chart
mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")

#Donut Chart for revenue share form part (Task-3)
ggplot(df1, aes(x = 2, y = SHARE_OF_TOTAL_REVENUE_FROM_THIS_PART, fill = parts)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = SHARE_OF_TOTAL_REVENUE_FROM_THIS_PART), color = "white")+
  scale_fill_manual(values = mycols) +  theme_void()+  xlim(0.5, 2.5)

#Creating the subset for filtration process for Task-4
Denco2 <- subset(Denco_Corp, MARGIN > 1000000)
#Converting chr data into factor data type
Denco2$PARTNUM = as.factor(Denco2$PARTNUM)

#Bar Plot for finding out the highest profit magin parts
ggplot(Denco2, aes(x=PARTNUM, y=MARGIN, fill=PARTNUM))+ 
  stat_summary(fun=mean, geom="bar")
