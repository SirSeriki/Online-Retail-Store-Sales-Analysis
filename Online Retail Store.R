setwd("c:/Users/Work Station/Desktop/Data Analysis Projects/Datasets")

## Installing Packages

install.packages("plyr")
install.packages("tidyverse")
install.packages("here")
install.packages("skimr")
install.packages("janitor")
install.packages("ggplot2")
install.packages("plotly")

## Installing the libraries

library(tidyverse)
library(lubridate)
library(plyr)
library(dplyr)
library(janitor)
library(readr)
library(tidyr)
library(ggplot2)
library(scales) # Formatting numbers and values
library(forcats)
library(dplyr, warn.conflicts = FALSE)


## Collecting Data

salesGoods <- read.csv("business.retailsales.csv")
salesAnnual <- read.csv("business.retailsales2.csv")


# Exploratory Analysis

## Check for null values 
 
 is.null(salesGoods)   
 is.null(salesAnnual)
 
 

 ## statistical analysis of the datasets

colnames(salesGoods)
glimpse(salesGoods)
str(salesGoods)
head(salesGoods)
dim(salesGoods)
nrow(salesGoods)
summary(salesGoods)

## Annual Sales

colnames(salesAnnual)
glimpse(salesAnnual)
str(salesAnnual)
head(salesAnnual)
dim(salesAnnual)
nrow(salesAnnual)
summary(salesAnnual)



## Creating  summaries

# Sales per year
sales_by_Year <- salesAnnual %>% group_by(Year) %>%  #Sales per Year
  summarise(Total.Sales=sum(Total.Sales)) %>% ungroup
  arrange(sales_by_Year,desc(), group_by = FALSE)
  as.data.frame(sales_by_Year) ##Converting tibble to a dataframe for ease visualization and aggregation



 ## There was a Year on year increment between 2017 to 2019, with Y2 seeing an increment of 0.16 from Y1 and Y3 an increment of 0.21

# Total annual sales per month 
sales_by_month <- salesAnnual %>% group_by(Month) %>% #Sales per month
  summarise(Total.Sales=sum(Total.Sales)) %>% ungroup
  arrange(sales_by_month,desc(), group_by = TRUE)
  as.data.frame(sales_by_month) ##Converting tibble to a dataframe for ease visualization and aggregation


## December saw the highest sales, while the lowest sales were February 

# Top product type

Top_Product_Type <- salesGoods %>% group_by(Product.Type) %>% #Top Selling items
  summarise(Total.Net.Sales=sum(Total.Net.Sales)) %>% ungroup %>%
  head(arrange(Top_Product_Type,desc(Total.Net.Sales)), n = 20)
  Top_Product_Type$Product.Type[1] <- "Miscellaneous" # Renaming missing column name
  as.data.frame(Top_Product_Type) #Converting tibble to a dataframe for ease visualization and aggregation


# Top best
top_best <- head(arrange(Top_Product_Type,desc(Total.Net.Sales)), n = 9) #Ranking the top 9 best selling products by Total Net Sales


## Baskets were the highest selling products  

# Worst selling products
top_worst <- top_n(Top_Product_Type, -9)%>%
             head(arrange(top_worst,(Total.Net.Sales)), n = 9)
             as.data.frame(top_worst) ##Converting tibble to a dataframe for ease visualization and aggregation

                                        
## Gift baskets are the worst selling product 

# Checking to see if there is a correlation between the most discounted products and their net sales

Discounts <- salesGoods %>% group_by(Product.Type) %>%
            summarise(Discounts=sum(Discounts)) %>% ungroup
            head(arrange(Discounts,(Discounts)), n = 19)
            Discounts$Product.Type[1] <- "Miscellaneous" # Renaming a missing column name
            as.data.frame(Discounts) ##Converting tibble to a dataframe for ease visualization and aggregation
            # Renaming a missing column name

# Discounts per month 

Discounts_per_month <- salesAnnual %>% group_by(Month) %>%
               summarise(Discounts=sum(Discounts)) %>% ungroup
               head(arrange(Discounts_per_month,(Discounts)), n = 12)
               as.data.frame(Discounts_per_month) ##Converting tibble to a dataframe for ease visualization and aggregation


 ## December had the highest discount at -2790.56, while February had the least discounts at -554.75

# Checking the returns on products sold

Returns <- salesGoods %>% group_by(Product.Type) %>%
          summarise(Returns=sum(Returns)) %>% ungroup
          head(arrange(Returns,(Returns)), n = 19)
          Returns$Product.Type[1] <- "Miscellaneous" # Renaming a missing column name
          as.data.frame(Returns) # Converting tibble to a dataframe for ease visualization and aggregation
          

#Returns per month 

Returns_per_month <- salesAnnual %>% group_by(Month) %>%
         summarise(Returns=sum(Returns)) %>% ungroup
         head(arrange(Returns_per_month,(Returns)), n = 12)
         as.data.frame(Returns_per_month) # Converting tibble to a dataframe for ease visualization and aggregation
 
 ## December had the highest returns (negative returns), while April had the lowest. 

# Most shipped products per month

Shipping_per_month <- salesAnnual %>% group_by(Month) %>%
       summarise(Shipping=sum(Shipping)) %>% ungroup
       head(arrange(Shipping_per_month,(Shipping)), n = 19)
       as.data.frame(Shipping_per_month) # Converting tibble to a dataframe for ease visualization and aggregation

# Shipping per product type 

Shipping_totalOders <- salesAnnual %>% group_by(Total.Orders) %>%
       summarise(Shipping=sum(Shipping)) %>% ungroup
       head(arrange(Shipping_per_Product,(Shipping)), n = 19)
       order_by(Total.Sales)
       as.data.frame(Shipping_totalOders) # Converting tibble to a dataframe for ease visualization and aggregation
       

salesAnnual$Shipping
 
#Visualizations 

##Visualizing summary data

# Sales per Year
ggplot(sales_by_Year,
       aes(x = Year, 
           y = Total.Sales))+
  geom_bar(stat="identity",
           fill = "cornflowerblue",
           color = "green") +
  geom_text(aes(label=Total.Sales), vjust=1.6, color="black", size=4.0)+
  labs(x = "Year",y = "Sales", title  = "Sales per year ", subtitle = "2017-2019")+
  theme_minimal()

 ## Annual sales saw a year on year increase between 2017 to 2019 with an average growth rate of 0.16% between Y2 and Y1, and 0.21% between Y3 and Y2.

# Sales per Month 
sales_plot_month <- sales_by_month  %>% 
  ggplot()+
  geom_line(aes(x = Month, y = Total.Sales, group = 1))+
  scale_x_discrete(limits = month.name) + ## sorting the month column into a monthly order. 
  scale_y_continuous(labels = function(x) {
    scales::comma(x, big.mark = ".", decimal.mark = ",")
  })+
  labs(x = "Month",y = "Sales", title  = "Sales per month")+
  theme_minimal()

## November and December saw the highest figures in sales, while the lowest figures in sales were from February.

# Best selling products

ggplot(top_best,
       aes(x =  Product.Type , 
           y = Total.Net.Sales)) +
  geom_bar(stat="identity",
           fill = "cornflowerblue",
           color = "green") +
  scale_y_continuous(labels = scales::comma)+
  coord_flip()+
  labs(x = "Product Type",y = "Total Sales", title  = "Best selling products")

#Shipping per month

ggplot(Shipping_per_month,
       aes(x =  Month, 
           y = Shipping)) +
  geom_bar(stat="identity",
           fill = "cornflowerblue",
           color = "green") +
  scale_x_discrete(limits = month.name)+
  labs(x = "Month",y = "Shipping", title  = "Rate of shipping per month")+
  theme_minimal()

# Worst selling products

ggplot(top_worst,
       aes(x =  Product.Type , 
           y = Total.Net.Sales)) +
  geom_bar(stat="identity",
           fill = "red",
           color = "black") +
  scale_y_continuous(labels = scales::comma)+
  coord_flip()+
  labs(x = "Product Type",y = "Total Sales", title  = "Worst selling items")+
  theme_minimal()

#Discounts 

ggplot(Discounts,
       aes(x =  Product.Type , 
           y = Discounts)) +
  geom_bar(stat="identity",
           fill = "red",
           color = "black") +
  scale_y_continuous(labels = scales::comma)+
  coord_flip()+
  labs(x = "Product Type",y = "Discount per item", title  = "Discount")+
  theme_minimal()


# Discounts per month

ggplot(Discounts_per_month,
       aes(x =  Month, 
           y = Discounts)) +
  geom_bar(stat="identity",
           fill = "red",
           color = "black") +
  scale_x_discrete(limits = month.name)+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Month",y = "Discount", title  = "Discount per month")+
  theme_minimal()



# Returns product
ggplot(Returns,
       aes(x =  Product.Type, 
           y = Returns)) +
  geom_bar(stat="identity",
           fill = "red",
           color = "black")+
  scale_y_continuous(labels = scales::comma)+
  coord_flip()+
  labs(x = "Product Type",y = "Returns", title = "Returns per product")+
  theme_minimal()

# Returns per Month
ggplot(Returns_per_month,
       aes(x =  Month, 
           y = Returns)) +
  geom_bar(stat="identity",
           fill = "red",
           color = "black") +
  scale_x_discrete(limits = month.name)+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Month",y = "Returns", title = "Returns per month")+
  theme_minimal()

 ## March, July, October and December have the highest negative returns per month, while April and August have the lowest negative returns.



# Recommendations 
## 1. Develop a loyalty program that offers monthly discounts to even out the discounts through the year
## 2. Use the data generated from the customer loyalty program to fine tune future analysis and a possible ML model.
## 3. Include a customer rating system on the store's website, use the data from the rating system to guide a discount recommendation system based on discounts based on individual customer product rating.
