library(tidyverse)
library(actuar)
library(fastDummies)
library(viridis)
library(compiler)
library(lubridate)
library(psych)
set.seed(0207)


#Generate dataframe of models
#It is essential to define first the planned number of models in trading
#then the estimated proportion of different categories and sub-categories
#number of models 
model_num <- 2000
#Proportions of "Kinder", "Damen" and "Herren" categories
kind_prop <- 0.1
herr_prop <- 0.3
damen_prop <- 0.6

#Proportions of "Mädchen" and "Jungen" subcategories in "Kinder" category
maed_prop <- 0.65
jung_prop <- 0.35

mod_id <- seq(from = 1, to = model_num, by = 1)
mod_cat <- sample(x = c("Damen", "Herren", "Kinder"), size = model_num, replace = TRUE, prob = c(damen_prop, herr_prop, kind_prop))
mod_subcat1  <- ifelse(mod_cat=="Kinder", sample(x = c("Maedchen","Jungen"), size = model_num*kind_prop, replace=TRUE, prob = c(maed_prop, jung_prop)),mod_cat)
mod_subcat2 <- ifelse(mod_subcat1 =="Maedchen", sample(x = c("Sandalen","Stiefel","Sneaker","Flache Schuhe"), size = model_num*kind_prop*maed_prop, replace=TRUE),
                      ifelse(mod_subcat1 =="Jungen",  sample(x = c("Sandalen","Stiefel","Sneaker"), size = model_num*kind_prop*jung_prop, replace=TRUE),
                             ifelse(mod_subcat1 == "Damen",sample(x = c("Sandalen","Stiefel","Sneaker","Flache Schuhe","High Heels"), size = model_num*damen_prop, replace=TRUE),
                                    sample(x = c("Sandalen","Stiefel","Sneaker","Business"), size = model_num*herr_prop, replace=TRUE)))) 
mod_name <- paste("Model ", mod_id)


#The distribution of prices of each category is assumed follow log-normal distribution
#It is needed to assign values of mean and standard deviation of  
#each log normal distribution
damen_preis_mean <- 100
damen_preis_sd <- 15
damen_preis_shape <- sqrt(log(1 + (damen_preis_sd^2 / damen_preis_mean^2)))
damen_preis_loc <- log(damen_preis_mean^2 /sqrt(damen_preis_sd^2 + damen_preis_mean^2))
damen_preis_loc <- log(damen_preis_mean^2 /sqrt(damen_preis_sd^2 + damen_preis_mean^2))
damen_preis_loc <- log(damen_preis_mean^2 /sqrt(damen_preis_sd^2 + damen_preis_mean^2))

herr_preis_mean <- 120
herr_preis_sd <- 20
herr_preis_loc <- log(herr_preis_mean^2 /sqrt(herr_preis_sd^2 + herr_preis_mean^2))
herr_preis_shape <- sqrt(log(1 + (herr_preis_sd^2 / herr_preis_mean^2)))

kind_preis_mean <- 70
kind_preis_sd <- 10
kind_preis_loc <- log(kind_preis_mean^2 /sqrt(kind_preis_sd^2 + kind_preis_mean^2))
kind_preis_shape <- sqrt(log(1 + (kind_preis_sd^2 / kind_preis_mean^2)))

mod_num_damen = sum(mod_cat=="Damen")
mod_num_herr = sum(mod_cat=="Herren")
mod_num_kind = sum(mod_cat=="Kinder")

mod_preis  <- ifelse(mod_cat=="Damen", rlnorm(n = mod_num_damen, damen_preis_loc, damen_preis_shape),
                     ifelse(mod_cat=="Herren",rlnorm(n = mod_num_herr, herr_preis_loc, herr_preis_shape),
                            rlnorm(n = mod_num_kind, kind_preis_loc, kind_preis_shape)))

#Generate model list containing info of each model including 
#model id, model category, its corresponding subcat and its price
mod_ls <- data.frame(mod_id, mod_name, mod_cat, mod_subcat1, mod_subcat2, mod_preis)


#Generate dataframe of products
#Each product is based on one model with specific size
#List of shoes sizes of each category
kind_groesse <- c(19:38)
herr_groesse <- c(39:50)
damen_groesse <- c(35:43)
      
prod_num = nrow(mod_ls[mod_ls$mod_cat=="Damen",])*length(damen_groesse)+
nrow(mod_ls[mod_ls$mod_cat=="Herren",])*length(herr_groesse)+
nrow(mod_ls[mod_ls$mod_cat=="Kinder",])*length(kind_groesse)

prod_mod_id <- c()
prod_size <-c()

for (i in 1:nrow(mod_ls)) {
  if (mod_ls$mod_cat[i] == "Damen") {
    prod_mod_id<- c(prod_mod_id, rep(mod_ls$mod_id[i], times= length(damen_groesse)))
    prod_size <- c(prod_size, sample(damen_groesse))
    }
  else if (mod_ls$mod_cat[i] == "Herren"){
    prod_mod_id <- c(prod_mod_id,rep(mod_ls$mod_id[i], times= length(herr_groesse)))
    prod_size <- c(prod_size, sample(herr_groesse))
    
  } 
  else{
    prod_mod_id <- c(prod_mod_id, rep(mod_ls$mod_id[i], times= length(kind_groesse)))
    prod_size <- c(prod_size, sample(kind_groesse))
  } 
    
}

prod_id <- seq(from = 1, to = prod_num, by = 1)
prod_ls <- data.frame(prod_id, prod_mod_id, prod_size)
#rm(prod_id, prod_mod_id, prod_size)

#Generate dataframe of customers
#number of customers
cust_num <- 65000
cust_id = c(1:cust_num)
cust_prev_return_rate = round(rbeta(cust_num, shape1 = 1, shape2 = 6), digits = 2)

regions_ls = c("BB","BE","BY","BW","HB","HH","HE","MV","NI","NW","RP","SL","SN","ST","SH","TH")
cust_region = sample(regions_ls, size = cust_num, replace = TRUE)

#Generate a random of number of order of each customer in a year
#with assumption it follows Poisson distribution with lambda = 4
avg_order_per_cust = 4 
cust_order_num <- rztpois(cust_num, avg_order_per_cust)
cust_ls = data.frame(cust_id, cust_region, cust_order_num, cust_prev_return_rate)



#Each customers have a number of orders and each order contains a list of products
#Each of ordered product requires ordered quantity and a returned quantity

#Function rand_ts is used to generate random timestamp for each order
rand_ts <- function(num, start_ts=as.POSIXct("2020-01-01 00:00:00", origin = "1970-01-01"),
                    end_ts=as.POSIXct("2021-01-01 00:00:00",origin = "1970-01-01")){
  diff_ts <- as.numeric(difftime(end_ts, start_ts, unit="sec"))
  rand_diff <- sort(runif(num, 0, diff_ts))
  rand_ts <- start_ts + rand_diff
  
  return (rand_ts)
}
avg_prod_per_order = 4
#Generate order list containing customer id, order time and number of products per order
#order_ts <- c()
#prod_num_per_order <- c()
#order_cust_id <- c()
'for (i in 1:cust_num) {
  order_ts <- c(order_ts, rand_ts(cust_order_num[i]))
  prod_num_per_order <- c(prod_num_per_order, rztpois(cust_order_num[i], avg_prod_per_order))
  order_cust_id <- c(order_cust_id, rep(i, times=cust_order_num[i]))

}
'
#rm(order_ts)
order_date <- sapply(c(1:cust_num), function(x) rand_ts(cust_order_num[x]))
order_date <- unlist(order_date)
order_date <- as.POSIXct(order_date, origin = "1970-01-01")

hour_of_order <- sample(c(0:23), size = nrow(order_ls), prob=c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01,
                                                              0.015, 0.015, 0.015, 0.015, 0.015, 0.015,
                                                              0.03, 0.03, 0.03, 0.03, 0.03,
                                                              0.138, 0.138, 0.138, 0.138, 0.138), replace = TRUE)
min_of_order <- sample(c(0:59), size=nrow(order_ls), replace=TRUE)
sec_of_order <- sample(c(0:59), size=nrow(order_ls), replace=TRUE)
order_ts <- make_datetime(year(order_date), month(order_date), mday(order_date), hour_of_order, min_of_order, sec_of_order)
order_ls$order_ts <- order_ts
order_ls$hour_of_order <- hour_of_order

rm(order_date, hour_of_order, min_of_order, sec_of_order)

prod_num_per_order <- sapply(c(1:cust_num), function(x) rztpois(cust_order_num[x], avg_prod_per_order))
order_cust_id <- sapply(c(1:cust_num), function(x) rep(x, times=cust_order_num[x]))
prod_num_per_order <- unlist(prod_num_per_order)
order_cust_id <- unlist(order_cust_id)
order_ls <- data.frame(order_cust_id, order_ts, prod_num_per_order)

#order_ls$hour_of_order <- as.numeric(format(order_ls$order_ts,'%H'))
#order_ls <- order_ls[order(order_ls$order_ts),]
#order_ls <- order_ls[ , - which(names(order_ls) %in% c("order_ts","hour_of_order"))]


#Calculate total number of orders

order_num <- sum(cust_order_num)
order_ls$order_id <- c(1:order_num)

require(dplyr)

#Based on order list, order_df is created containing the detail of each order 
#including ids of ordered products in each order and quantity
ord.order_id <- c()
ord.prod_id <- c()

ord.prod_id <- sapply(c(1:order_num), function(x) sample(prod_ls$prod_id, size=order_ls$prod_num_per_order[x], replace=TRUE))
ord.prod_id <- unlist(ord.prod_id)
ord.order_id <- sapply(c(1:order_num), function(x) rep(order_ls$order_id[x], times= order_ls$prod_num_per_order[x]))
ord.order_id <- unlist(ord.order_id)
order_df <- data.frame(ord.order_id, ord.prod_id)
order_df$ord.qty <- rep(1, times = length(ord.order_id))


#Based on detail of orders in order_df, 
'order_ls <- order_ls %>%
  group_by(order_cust_id) %>%
  arrange(order_ts) %>%
  mutate(prev_total_qty = cumsum(prod_num_per_order) - prod_num_per_order)
'
#rm(full_df)
full_df <- merge(order_df, prod_ls, by.x="ord.prod_id", by.y="prod_id")
full_df <- merge(full_df, mod_ls, by.x="prod_mod_id", by.y="mod_id")
full_df <- merge(full_df, order_ls, by.x="ord.order_id", by.y="order_id")
full_df <- merge(full_df, cust_ls, by.x="order_cust_id", by.y="cust_id")

full_df <- dummy_cols(full_df, select_columns = "mod_subcat1")

full_df <- full_df %>%
  group_by(ord.order_id, mod_subcat1, mod_subcat2) %>%
  mutate(qty_same_subcat = sum(ord.qty))

full_df <- full_df %>%
  group_by(ord.order_id, mod_subcat1) %>%
  mutate(num_subcat_ord = n_distinct(mod_subcat2))


full_df <- full_df %>%
  group_by(ord.order_id) %>%
  mutate(num_cat_ord = n_distinct(mod_subcat1))


#full_df <- full_df[ , - which(names(full_df) %in% c("return_qty"))]

#Define model of probability
beta_qty_same_subcat <- log(2.8, exp(1))
beta_mix_subcat <- log(1.35, exp(1))
beta_num_subcat_ord <- log(1.35, exp(1))
beta_prev_return_rate <- log(7.4, exp(1))
beta_price <- log(0.98, exp(1))

beta_damen <- log(7.4,exp(1))
beta_herren <- log(2.8,exp(1))
beta_maed <- log(1.65,exp(1))

intercept <- -3.12

#Order dataframe of detailed order according to customer id and order timestamp
full_df <- full_df[order(full_df$order_cust_id, full_df$order_ts),]
item_num <- nrow(full_df)
#beta_mix_subcat * full_df$num_subcat_ord *  full_df$qty_same_subcat +
full_df$p <-  1/(1+exp(-(intercept + 
              beta_num_subcat_ord*full_df$num_subcat_ord + beta_qty_same_subcat*full_df$qty_same_subcat +
              beta_prev_return_rate*full_df$cust_prev_return_rate + beta_price*full_df$mod_preis +
              beta_damen * full_df$mod_subcat1_Damen + beta_herren * full_df$mod_subcat1_Herren +
              beta_maed * full_df$mod_subcat1_Maedchen +
              beta_mix_subcat * full_df$num_subcat_ord *  full_df$qty_same_subcat)))
random <- runif(item_num,min=0,max=1)
full_df$return_state <- rep(0,item_num)
full_df$return_state[random<=full_df$p] <- 1
#full_df$return_state <- rbinom(n=nrow(full_df), size=1, prob = full_df$p)
#full_df$return_qty <- ifelse(full_df$return_state==1,1,0)
mean(full_df$p)
mean(full_df$return_state)

'intercept_test <- -3.75
beta_jung_test <- -0.5
p_test <- 1/(1+exp(-( 
                       beta_num_subcat_ord*full_df$num_subcat_ord + beta_qty_same_subcat*full_df$qty_same_subcat +
                       beta_prev_return_rate*full_df$cust_prev_return_rate + beta_price*full_df$mod_preis +
                       beta_damen * full_df$mod_subcat1_Damen + beta_herren * full_df$mod_subcat1_Herren +
                       beta_maed * full_df$mod_subcat1_Maedchen + beta_jung_test * full_df$mod_subcat1_Jungen + 
                       beta_mix_subcat * full_df$num_subcat_ord *  full_df$qty_same_subcat)))
summary(p_test)
rm(intercept_test, beta_jung_test, p_test)
'summary(full_df$p)

rm(ord.order_id)
rm(ord.prod_id)
rm(order_cust_id)
rm(order_ts)
rm(prod_num_per_order)
rm(cust_id, cust_region, cust_order_num, cust_prev_return_rate)

#Plot to check distribution of variables

jpeg(filename = "C:/Users/hddt/OneDrive/Desktop/HS_Reutlingen/Courses/05.DataScience/Simulationsstudie/Charts/model_proportions.jpeg")
ggplot(data = mod_ls, aes(x=mod_subcat1, y = stat(prop), group=1, fill = factor(..x..)
                          , label = scales::percent(stat(prop)))) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", vjust=-0.5) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_viridis(option = "D") +
  scale_fill_viridis(discrete = TRUE, name = "Categories", labels = c("Damen", "Herren", "Maedchen", "Jungen")) +
  ggtitle("Proportion of four categories in model list") +
  xlab("Categories") +
  scale_x_discrete(limits = c("Damen", "Herren", "Maedchen", "Jungen")) +
  theme(legend.position = "bottom")
dev.off()

vline <- mod_ls %>%
        group_by(mod_subcat1) %>%
        summarise(mean = mean(mod_preis))
        
vline$label <- paste("Mean =", sprintf("%0.2f", round(vline$mean, digits = 2)))        
jpeg(filename = "C:/Users/hddt/OneDrive/Desktop/HS_Reutlingen/Courses/05.DataScience/Simulationsstudie/Charts/price_distribution.jpeg")
ggplot(data = mod_ls, aes(x=mod_preis, fill=mod_subcat1)) +
  geom_histogram(aes(y=..density..), binwidth = 5, alpha=0.5) +
  geom_density(alpha=0.6) +
  facet_wrap(. ~ mod_subcat1, ncol=2) +
  geom_vline(data = vline, aes(xintercept=mean), color="royalblue2", linetype="dashed", size=1) +
  geom_text(data = vline, mapping = aes(x = 110, y = 0.04, label = label),
            col = "royalblue2", size = 3, inherit.aes = TRUE) +
  scale_color_viridis(option = "D") +
  scale_fill_viridis(discrete = TRUE, name = "Product Price") +
  ggtitle("Distribution of products' prices") +
  xlab("Price") +
  labs(fill = "Product Price") +
  theme(legend.position = "bottom")
dev.off()

prod_ls_graph <- merge(prod_ls, mod_ls, by.x="prod_mod_id", by.y="mod_id")
jpeg(filename = "C:/Users/hddt/OneDrive/Desktop/HS_Reutlingen/Courses/05.DataScience/Simulationsstudie/Charts/size_distribution.jpeg")
ggplot(data = prod_ls_graph, aes(x=prod_size, fill=mod_subcat1)) +
  geom_bar(position="dodge", alpha=0.5) +
  facet_wrap(. ~ mod_subcat1, ncol=2) +
  scale_color_viridis(option = "D") +
  scale_fill_viridis(discrete = TRUE, name = "Category") +
  scale_x_discrete(limits=c(20, 25, 30, 35, 40, 45, 50))+
  ggtitle("Distribution of products' sizes by categories") +
  xlab("Size") +
  labs(fill = "Product Size") +
  theme(legend.position = "bottom")
dev.off()

jpeg(filename = "C:/Users/hddt/OneDrive/Desktop/HS_Reutlingen/Courses/05.DataScience/Simulationsstudie/Charts/prev_return_rate.jpeg")
ggplot(data = cust_ls, aes(x=cust_prev_return_rate)) +
  geom_density(color="#69b3a2", size=1.5) +
  xlim(0,1) +
  ggtitle("Historical one-year return rates of customers") +
  xlab("Return rate") +
  geom_vline(aes(xintercept=mean(cust_prev_return_rate)), color="#69b3a2", linetype="dashed", size=1.25) +
  geom_text(aes(x=0.2, y=4.5, label=paste("avg = ", sprintf("%0.2f", round(mean(cust_prev_return_rate), digits = 2)))), colour="#69b3a2")

dev.off()

jpeg(filename = "C:/Users/hddt/OneDrive/Desktop/HS_Reutlingen/Courses/05.DataScience/Simulationsstudie/Charts/cust_order_num.jpeg")
ggplot(data = cust_ls, aes(x=cust_order_num)) +
  geom_histogram(binwidth = 1, fill="royalblue2", color="white", alpha=0.5, position = "dodge") +
  xlim(0, 13) +
  ggtitle("Number of orders in one year of each customer") +
  xlab("Number of order") +
  geom_vline(aes(xintercept=mean(cust_order_num)), color="royalblue2", linetype="dashed", size=1.25) +
  geom_text(aes(x=4.75, label=paste("avg = ", sprintf("%0.2f", round(mean(cust_order_num)), digits = 2)), y=13700), colour="royalblue2", vjust=1)
dev.off()

jpeg(filename = "C:/Users/hddt/OneDrive/Desktop/HS_Reutlingen/Courses/05.DataScience/Simulationsstudie/Charts/prod_num_per_order.jpeg")
ggplot(data = order_ls, aes(x=prod_num_per_order)) +
  geom_histogram(binwidth = 1, fill="royalblue2", color="white", alpha=0.5, position = "dodge") +
  xlim(0, 13) +
  ggtitle("Number of products pro order") +
  xlab("Number of ordered products") +
  geom_vline(aes(xintercept=mean(prod_num_per_order)), color="royalblue2", linetype="dashed", size=1.25) +
  geom_text(aes(x=4.75, label=paste("avg = ", sprintf("%0.2f", round(mean(prod_num_per_order), digits = 2))), y=57000), colour="royalblue2", vjust=1.5)
dev.off()

jpeg(filename = "C:/Users/hddt/OneDrive/Desktop/HS_Reutlingen/Courses/05.DataScience/Simulationsstudie/Charts/customer_regions.jpeg")
ggplot(data = cust_ls, aes(x=cust_region, y = stat(prop), group=1, fill = factor(..x..)
                          )) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_viridis(option = "D") +
  scale_fill_viridis(discrete = TRUE, name = "Regions", label = regions_ls) +
  ggtitle("Proportion of customers in each region") +
  xlab("Region") +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
dev.off()

jpeg(filename = "C:/Users/hddt/OneDrive/Desktop/HS_Reutlingen/Courses/05.DataScience/Simulationsstudie/Charts/hours_of_orders.jpeg")
ggplot(data = order_ls, aes(x=hour_of_order, y = stat(prop), group=1, fill = factor(..x..))) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_viridis(option = "D") +
  scale_fill_viridis(discrete = TRUE, name = "Hour of oder", label = c(0:23)) +
  ggtitle("Proportion of oders in hour") +
  xlab("Hour of order") +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow=3,byrow=TRUE))
dev.off()

jpeg(filename = "C:/Users/hddt/OneDrive/Desktop/HS_Reutlingen/Courses/05.DataScience/Simulationsstudie/Charts/p_distribution.jpeg")
ggplot(data = full_df, aes(x=p)) +
  geom_histogram(aes(y=..density..), binwidth = 0.1, fill="#238A8DFF", color="white", alpha=0.5, position = "dodge") +
  xlim(0, 1) +
  ggtitle("Distribution of generated probability") +
  xlab("p") +
  geom_vline(aes(xintercept=mean(p)), color="#482677FF", linetype="dashed", size=1.25) +
  geom_text(aes(x=0.5, y=2.7, label=paste("avg = ", sprintf("%0.2f", round(mean(p), digits = 2)))), colour="#482677FF")
dev.off()

jpeg(filename = "C:/Users/hddt/OneDrive/Desktop/HS_Reutlingen/Courses/05.DataScience/Simulationsstudie/Charts/return_state.jpeg")
ggplot(data = full_df, aes(x=return_state, y = stat(prop), group=1, fill = factor(..x..)
                           , label = scales::percent(stat(prop)))) +
  geom_bar(position = "dodge", width=0.4, ) +
  geom_text(stat = "count", vjust=-0.5) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous("Return State", breaks = c(0,1)) +
  scale_fill_manual(values=(c("0" ="#B8DE29FF", "1"="#482677FF")))+
  ggtitle("Proportion of returned vs. not-returned products") +
  theme(legend.position = "none") +
  geom_text(aes(x=0.5, y=0.55, label=paste("avg = ", sprintf("%0.2f", round(mean(return_state), digits = 2)))), colour="#482677FF")
  
dev.off()

"#95D840FF"
jpeg(filename = "C:/Users/hddt/OneDrive/Desktop/HS_Reutlingen/Courses/05.DataScience/Simulationsstudie/Charts/num_subcat_ord.jpeg")
ggplot(data = full_df, aes(x=num_subcat_ord, y=..density..)) +
  geom_histogram(binwidth = 1, fill= "#95D840FF", color="white", alpha=0.4, position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Proportion of number of ordered subcategories") +
  xlab("Number of ordered subcateories in an order") +
  theme(legend.position = "none") 

dev.off()


jpeg(filename = "C:/Users/hddt/OneDrive/Desktop/HS_Reutlingen/Courses/05.DataScience/Simulationsstudie/Charts/qty_same_subcat.jpeg")
ggplot(data = full_df, aes(x=qty_same_subcat, y=..density..)) +
  geom_histogram( fill= "#FDE725FF", color="white", alpha=0.4, position = "dodge", binwidth=1) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Proportion of quantity of products in a same subcategory") +
  xlab("Quantity of products in a same subcategory in an order") +
  theme(legend.position = "none") 

dev.off()

summary(full_df)
describe(full_df[c("return_state","p","hour_of_order",
                     "cust_prev_return_rate","cust_region",
                     "prod_size","mod_preis","prod_num_per_order","qty_same_subcat",
                     "num_subcat_ord","num_cat_ord",
                     "mod_subcat1")])
