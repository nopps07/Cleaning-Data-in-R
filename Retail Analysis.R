## Retail store Analytics

#load

features1 <- Features_data_set

features2 <- read_csv("Features data set.csv")
sales <- read_csv("sales data-set.csv")
stores <- read_csv("stores data-set.csv")

#packages
library(dplyr)
library(ggplot2)


#Understand
head(features1)
head(sales)
head(stores)
tail(sales)

glimpse(features)
glimpse(sales)
glimpse(stores)

summary(features)
summary(sales)

#CLeaning and rangling and tidying
features <- select(features1, -c(MarkDown1, MarkDown2, MarkDown3, MarkDown4, MarkDown5))

sales_45 <- sales %>%
  filter(Store == "45")

sales_45_holiday <- sales_45 %>%
  filter(IsHoliday == TRUE)

check <- sales_45_holiday %>%
  group_by(Dept) %>%
  summarise(Weekly_Sales_mean = mean(Weekly_Sales))

#Visulization
ggplot(check, aes(x = Dept, y = Weekly_Sales_mean, color = Dept)) +
  geom_point(alpha = 2, size = 2.3)


#Tidy level 1
#Dept group 1 : 1 ~ 30
#Dept group 2 : 31 ~ 60
#Dept group 3 : 61 ~ 99
check2 <- check %>%
  mutate(Group = ifelse(Dept < 31, "1", 
                        ifelse(Dept < 61, "2", "3")))

ggplot(check2, aes(x = Dept, y = Weekly_Sales_mean, color = Group)) +
  geom_point(size = 2.3)


dept <- ggplot(check2, aes(x = Dept, y = Weekly_Sales_mean)) +
  geom_jitter(aes(col = Group, size = Weekly_Sales_mean)) +
  geom_smooth(aes(col = Group), method = "lm", se = F)

