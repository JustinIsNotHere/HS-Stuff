library(tidyverse)
library(readxl)
library(janitor)
data <- read_excel("Superstore.xlsx")
data = janitor::clean_names(data)
data= data[c('customer_name', 'category', 'department', 'discount', 'order_date', 'order_priority', 'order_quantity', 'profit', 'region', 'sales', 'ship_mode', 'shipping_cost', 'state')]
data=mutate(data, expenses = sales-profit)
data =mutate(data, large_discount = ifelse((discount >0.1), "Yes", "No"))
data
data2 <- data |>
  separate(customer_name, " ",
           into = c("n1","n2","n3"),
           remove=FALSE)
data2 |> view()
data2 <- data2 |>
  drop_na(3)
data2|> view()
n_distinct(data2$customer_name)
data <- data |>
  separate(customer_name, " ",
                          into = c("n1","n2","n3"),
                          remove=FALSE)
data <- data |>
  filter(is.na(n3))
data|> view()
data= data |> 
  relocate('customer_name', 'category', 'department', 'order_date', 'region', 'ship_mode', 'state')

east_sales <- data %>%
  filter(region == "East") %>%
  group_by(state) %>%
  summarise(sales = sum(sales))

state_profit <- data %>%
  group_by(state, department) %>%
  summarise(AvgProfit = mean(profit))

data$Month <- month(data$order_date)
data<- data |>
  separate(order_date, "-",
           into = c("year","month","day"),
           remove=FALSE)
data$month <- month(data$order_date)
month_quantity <- data %>%
  group_by(month) %>%
  summarise(total_quantity = sum(order_quantity)) %>%
  arrange(desc(total_quantity))

dept_discount <- data %>%
  group_by(department) %>%
  summarise(avg_discount = mean(discount), min_discount = min(discount), max_discount = max(discount), N = n())

ggplot(
  data = data,
  mapping = aes(x = month,year, y = profit)
) +
  geom_point(mapping = aes()) +
  geom_smooth(method = "lm")

plot1 <- data %>%
  group_by(month, order_priority) %>%
  summarise(n=n()) %>%
  mutate(prop=n/sum(n)) %>%
  ggplot(aes(x=month,y=prop,fill=order_priority))+
  geom_bar(stat="identity",position="fill")
plot1

plot2 <- data %>%
  group_by(department, order_priority) %>%
  summarise(n=n()) %>%
  mutate(prop=n/sum(n)) %>%
  ggplot(aes(x=department,y=prop,fill=order_priority))+
  geom_bar(stat="identity",position="fill")
plot2

data.lm <- lm(profit ~ department, data=data)
data.av <- aov(data.lm)
summary(data.av)