library(class)

rm(p3_sam_dt, p3_sam_train, p3_sam_test)
set.seed(010405)
p3_sam_n <- 10000
p3_sam_ind <- sample(c(1:nrow(full_df)), size=p3_sam_n, replace=TRUE)
p3_sam_dt <-setDT(full_df[p3_sam_ind,])
p3_control <- p3_sam_dt[,c("p")]
p3_sam_dt <- p3_sam_dt[,c("return_state","ord.order_id","ord.prod_id","ord.qty", "hour_of_order",
                    "order_cust_id", "cust_prev_return_rate","cust_region",
                    "prod_size","mod_preis","prod_num_per_order","qty_same_subcat",
                    "num_subcat_ord","num_cat_ord",
                    "mod_subcat1","mod_subcat2")]

p3_sam_dt <- dummy_cols(p3_sam_dt, select_columns = "mod_subcat1")
p3_sam_dt <- dummy_cols(p3_sam_dt, select_columns = "cust_region")

p3_sam_dt <- p3_sam_dt[ , !c("mod_subcat1","mod_subcat2","cust_region","ord.qty","ord.prod_id","order_cust_id")]

p3_train_ind <- sample.int(n = p3_sam_n, size = floor(.5*p3_sam_n), replace = FALSE)
#rm(p3_sam_train, p3_sam_test)
p3_sam_train <- p3_sam_dt[p3_train_ind,]
p3_sam_test <- p3_sam_dt[-p3_train_ind,]

p3_control_test <- p3_control[-p3_train_ind,]
testdata_min_mse <- var(p3_sam_test_y$return_state - p3_control_test$p)
#crosscheck : mean(p3_control_test$p*(1-p3_control_test$p))

p3_train_mse <- c()
p3_test_mse <- c()
#Model with only 1 explanatory variable
p3_var10_mod <- glm(return_state ~ prod_num_per_order + prod_size + mod_preis + cust_prev_return_rate +
                       hour_of_order + qty_same_subcat + num_cat_ord + num_subcat_ord +
                       mod_subcat1_Damen + mod_subcat1_Herren + mod_subcat1_Maedchen +
                       cust_region_BE +cust_region_BY + cust_region_HB + cust_region_HE +
                       cust_region_HH + cust_region_MV + cust_region_NI + cust_region_NW + cust_region_RP +
                       cust_region_SH + cust_region_SL + cust_region_SN + cust_region_ST +cust_region_TH,
                     data = p3_sam_train, family = "binomial")
summary(p3_var10_mod)

'mseP3Var10_train <- mean((p3_sam_train$return_state - predict(p3_var10_mod, p3_sam_train, type="response"))^2)
mseP3Var10_train
p3_train_mse <- cbind(p3_train_mse, mseP3Var10_test)
'
mseP3Var10_test <- mean((p3_sam_test$return_state - predict(p3_var10_mod, p3_sam_test, type="response"))^2)
mseP3Var10_test
#p3_test_mse <- cbind(p3_test_mse, mseP3Var10_test)

p3_var5_mod <- glm(return_state ~  mod_preis + cust_prev_return_rate +
                        qty_same_subcat + num_subcat_ord +
                        mod_subcat1_Damen + mod_subcat1_Herren + mod_subcat1_Maedchen,
                      data = p3_sam_train, family = "binomial")
summary(p3_var5_mod)
mseP3Var5_test <- mean((p3_sam_test$return_state - predict(p3_var5_mod, p3_sam_test, type="response"))^2)
mseP3Var5_test

train_mse_sing_var <- c()
test_mse_sing_var <- c()
p3_var1Price_mod <- glm(return_state ~  mod_preis,
                      data = p3_sam_train, family = "binomial")
summary(p3_var1Price_mod)
mseP3Var1Price_train <- mean((p3_sam_train$return_state - predict(p3_var1Price_mod, p3_sam_train, type="response"))^2)
mseP3Var1Price_train
train_mse_sing_var <- cbind(train_mse_sing_var, mseP3Var1Price_train)


mseP3Var1Price_test <- mean((p3_sam_test$return_state - predict(p3_var1Price_mod, p3_sam_test, type="response"))^2)
mseP3Var1Price_test
test_mse_sing_var <- cbind(test_mse_sing_var, mseP3Var1Price_test)

p3_var1RRate_mod <- glm(return_state ~  cust_prev_return_rate,
                        data = p3_sam_train, family = "binomial")
summary(p3_var1RRate_mod)
mseP3Var1RRate_train <- mean((p3_sam_train$return_state - predict(p3_var1RRate_mod, p3_sam_train, type="response"))^2)
mseP3Var1RRate_train
train_mse_sing_var <- cbind(train_mse_sing_var, mseP3Var1RRate_train)

mseP3Var1RRate_test <- mean((p3_sam_test$return_state - predict(p3_var1RRate_mod, p3_sam_test, type="response"))^2)
mseP3Var1RRate_test
test_mse_sing_var <- cbind(test_mse_sing_var, mseP3Var1RRate_test)

p3_var1SSubcat_mod <- glm(return_state ~  qty_same_subcat,
                        data = p3_sam_train, family = "binomial")
summary(p3_var1SSubcat_mod)
mseP3Var1SSubcat_train <- mean((p3_sam_train$return_state - predict(p3_var1SSubcat_mod, p3_sam_train, type="response"))^2)
mseP3Var1SSubcat_train
train_mse_sing_var <- cbind(train_mse_sing_var, mseP3Var1SSubcat_train)
  
mseP3Var1SSubcat_test <- mean((p3_sam_test$return_state - predict(p3_var1SSubcat_mod, p3_sam_test, type="response"))^2)
mseP3Var1SSubcat_test
test_mse_sing_var <- cbind(test_mse_sing_var, mseP3Var1SSubcat_test)

p3_var1Subcat_mod <- glm(return_state ~  num_subcat_ord,
                          data = p3_sam_train, family = "binomial")
summary(p3_var1Subcat_mod)
mseP3Var1Subcat_train <- mean((p3_sam_train$return_state - predict(p3_var1Subcat_mod, p3_sam_train, type="response"))^2)
mseP3Var1Subcat_train
train_mse_sing_var <- cbind(train_mse_sing_var, mseP3Var1Subcat_train)


mseP3Var1Subcat_test <- mean((p3_sam_test$return_state - predict(p3_var1Subcat_mod, p3_sam_test, type="response"))^2)
mseP3Var1Subcat_test
test_mse_sing_var <- cbind(test_mse_sing_var, mseP3Var1Subcat_test)

p3_var1Cat_mod <- glm(return_state ~ mod_subcat1_Damen + mod_subcat1_Herren + mod_subcat1_Maedchen,
                          data = p3_sam_train, family = "binomial")
summary(p3_var1Cat_mod)
mseP3Var1Cat_train <- mean((p3_sam_train$return_state - predict(p3_var1Cat_mod, p3_sam_train, type="response"))^2)
mseP3Var1Cat_train
train_mse_sing_var <- cbind(train_mse_sing_var, mseP3Var1Cat_train)

mseP3Var1Cat_test <- mean((p3_sam_test$return_state - predict(p3_var1Cat_mod, p3_sam_test, type="response"))^2)
mseP3Var1Cat_test
test_mse_sing_var <- cbind(test_mse_sing_var, mseP3Var1Cat_test)

train_mse_sing_var
test_mse_sing_var

p3_train_mse <- cbind(p3_train_mse, mseP3Var1SSubcat_train)
p3_test_mse <- cbind(p3_test_mse, mseP3Var1SSubcat_test)

train_mse_var2 <- c()
test_mse_var2 <- c()
p3_var2Price_mod <- glm(return_state ~  qty_same_subcat + mod_preis,
                        data = p3_sam_train, family = "binomial")
summary(p3_var2Price_mod)
mseP3Var2Price_train <- mean((p3_sam_train$return_state - predict(p3_var2Price_mod, p3_sam_train, type="response"))^2)
mseP3Var2Price_train
train_mse_var2 <- cbind(train_mse_var2, mseP3Var2Price_train)

mseP3Var2Price_test <- mean((p3_sam_test$return_state - predict(p3_var2Price_mod, p3_sam_test, type="response"))^2)
mseP3Var2Price_test
test_mse_var2 <- cbind(test_mse_var2, mseP3Var2Price_test)

p3_var2RRate_mod <- glm(return_state ~  qty_same_subcat + cust_prev_return_rate,
                          data = p3_sam_train, family = "binomial")
summary(p3_var2RRate_mod)
mseP3Var2RRate_train <- mean((p3_sam_train$return_state - predict(p3_var2RRate_mod, p3_sam_train, type="response"))^2)
mseP3Var2RRate_train
train_mse_var2 <- cbind(train_mse_var2, mseP3Var2RRate_train)

mseP3Var2RRate_test <- mean((p3_sam_test$return_state - predict(p3_var2RRate_mod, p3_sam_test, type="response"))^2)
mseP3Var2RRate_test
test_mse_var2 <- cbind(test_mse_var2, mseP3Var2RRate_test)

p3_var2Cat_mod <- glm(return_state ~  qty_same_subcat + mod_subcat1_Damen + mod_subcat1_Herren + mod_subcat1_Maedchen,
                        data = p3_sam_train, family = "binomial")
summary(p3_var2Cat_mod)
mseP3Var2Cat_train <- mean((p3_sam_train$return_state - predict(p3_var2Cat_mod, p3_sam_train, type="response"))^2)
mseP3Var2Cat_train
train_mse_var2 <- cbind(train_mse_var2, mseP3Var2Cat_train)

mseP3Var2Cat_test <- mean((p3_sam_test$return_state - predict(p3_var2Cat_mod, p3_sam_test, type="response"))^2)
mseP3Var2Cat_test
test_mse_var2 <- cbind(test_mse_var2, mseP3Var2Cat_test)

p3_var2Subcat_mod <- glm(return_state ~  qty_same_subcat + num_subcat_ord,
                        data = p3_sam_train, family = "binomial")
summary(p3_var2Subcat_mod)
mseP3Var2Subcat_train <- mean((p3_sam_train$return_state - predict(p3_var2Subcat_mod, p3_sam_train, type="response"))^2)
mseP3Var2Subcat_train
train_mse_var2 <- cbind(train_mse_var2, mseP3Var2Subcat_train)

mseP3Var2Subcat_test <- mean((p3_sam_test$return_state - predict(p3_var2Subcat_mod, p3_sam_test, type="response"))^2)
mseP3Var2Subcat_test
test_mse_var2 <- cbind(test_mse_var2, mseP3Var2Subcat_test)

train_mse_var2
test_mse_var2

p3_train_mse <- cbind(p3_train_mse, mseP3Var2Subcat_train)
p3_test_mse <- cbind(p3_test_mse, mseP3Var2Subcat_test)

p3_train_mse
p3_test_mse

#Model used to generate the population set
p3_var6Exp_mod <- glm(return_state ~  mod_preis + cust_prev_return_rate +
                        qty_same_subcat * num_subcat_ord +
                        mod_subcat1_Damen + mod_subcat1_Herren + mod_subcat1_Maedchen,
                      data = p3_sam_train, family = "binomial")
summary(p3_var6Exp_mod)
mseP3Var6Exp_train <- mean((p3_sam_train$return_state - predict(p3_var6Exp_mod, p3_sam_train, type="response"))^2)
mseP3Var6Exp_train

mseP3Var6Exp_test <- mean((p3_sam_test$return_state - predict(p3_var6Exp_mod, p3_sam_test, type="response"))^2)
mseP3Var6Exp_test

p3_train_mse <- cbind(p3_train_mse, mseP3Var6Exp_train)
p3_test_mse <- cbind(p3_test_mse, mseP3Var6Exp_test)

#Model with cross relationship and poly
p3_comp_mod <- glm(return_state ~  poly(mod_preis,4) + poly(cust_prev_return_rate,4) +
                        poly(qty_same_subcat,4) + poly(num_subcat_ord,4) + 
                     mod_preis *cust_prev_return_rate * qty_same_subcat * num_subcat_ord +
                        mod_subcat1_Damen + mod_subcat1_Herren + mod_subcat1_Maedchen,
                      data = p3_sam_train, family = "binomial")
summary(p3_comp_mod)
mseP3Comp_train <- mean((p3_sam_train$return_state - predict(p3_comp_mod, p3_sam_train, type="response"))^2)
mseP3Comp_train

mseP3Comp_test <- mean((p3_sam_test$return_state - predict(p3_comp_mod, p3_sam_test, type="response"))^2)
mseP3Comp_test

p3_train_mse <- cbind(p3_train_mse, mseP3Comp_train)
p3_test_mse <- cbind(p3_test_mse, mseP3Comp_test)

#KNN
p3_sam_train_x <- p3_sam_train[,-c("return_state")]
p3_sam_test_x <- p3_sam_test[,-c("return_state")]
p3_sam_train_x_std <- scale(p3_sam_train_x)
p3_sam_test_x_std <- scale(p3_sam_test_x)

p3_sam_train_y <- p3_sam_train[,c("return_state")]
p3_sam_test_y <- p3_sam_test[,c("return_state")]


#k=100
knn100_pred <- knn(train = p3_sam_train_x_std, test = p3_sam_train_x_std, cl = p3_sam_train_y$return_state, prob=TRUE, k=100)
table(knn100_pred, p3_sam_train_y$return_state)
confusionMatrix(table(knn100_pred, p3_sam_train_y$return_state))

knn100_prob <- (knn100_pred==1)*attr(knn100_pred,"prob") + (knn100_pred==0)*(1 - attr(knn100_pred,"prob"))#geschaetzte Wkeitsverteilung
mseKNN100_train <- mean((p3_sam_train_y$return_state - knn100_prob)^2)
mseKNN100_train
summary(attr(knn1_pred,"prob"))
knn100_pred_test <- knn(train = p3_sam_train_x_std, test = p3_sam_test_x_std, cl = p3_sam_train_y$return_state, prob=TRUE, k=1)
knn100_prob_test <- (knn100_pred_test==1)*attr(knn100_pred_test,"prob") + (knn100_pred_test==0)*(1 - attr(knn100_pred_test,"prob"))
mseKNN100_test <- mean((p3_sam_test_y$return_state - knn100_prob)^2)
mseKNN100_test

p3_train_mse <- cbind(p3_train_mse, mseKNN100_train)
p3_test_mse <- cbind(p3_test_mse, mseKNN100_test)

#KNN k=1
knn1_pred <- knn(train = p3_sam_train_x_std, test = p3_sam_train_x_std, cl = p3_sam_train_y$return_state, prob=TRUE, k=1)
table(knn1_pred, p3_sam_train_y$return_state)
confusionMatrix(table(knn1_pred, p3_sam_train_y$return_state))

knn1_prob <- (knn1_pred==1)*attr(knn1_pred,"prob") + (knn1_pred==0)*(1 - attr(knn1_pred,"prob"))
mseKNN1_train <- mean((p3_sam_train_y$return_state - knn1_prob)^2)
mseKNN1_train

knn1_pred_test <- knn(train = p3_sam_train_x_std, test = p3_sam_test_x_std, cl = p3_sam_train_y$return_state, prob=TRUE, k=1)
knn1_prob_test <- (knn1_pred_test==1)*attr(knn1_pred_test,"prob") + (knn1_pred_test==0)*(1 - attr(knn1_pred_test,"prob"))
mseKNN1_test <- mean((p3_sam_test_y$return_state - knn1_prob_test)^2)

mseKNN1_test
p3_train_mse <- cbind(p3_train_mse, mseKNN1_train)
p3_test_mse <- cbind(p3_test_mse, mseKNN1_test)

p3_train_mse
p3_test_mse

#Prepare data as long data frame for plot and use ggplot 
sing_mod_lbl <- c("Price","PrevReturnRate", "QtySameSubcat","NumSubcat","Categorie")
sing_mse_df <-  train_mse_sing_var%>% t() %>% data.frame() %>% setNames(c("MSE"))
sing_mse_df$model_name <- sing_mod_lbl
sing_mse_df$data_type <- rep("Train MSE",times=nrow(sing_mse_df))

sing_test_mse_df <- test_mse_sing_var%>% t() %>% data.frame() %>% setNames(c("MSE"))
sing_test_mse_df$model_name <- sing_mod_lbl
sing_test_mse_df$data_type <- rep("Test MSE",times=nrow(sing_test_mse_df))

sing_mse_df <- rbind(sing_mse_df, sing_test_mse_df)
  
jpeg(filename = "C:/Users/hddt/OneDrive/Desktop/HS_Reutlingen/Courses/05.DataScience/Simulationsstudie/Charts/Sing_MSE.jpeg")
ggplot(sing_mse_df, aes(x = model_name)) +
  geom_dotplot(aes(y= MSE, fill=data_type), binaxis='y', stackdir='center', binwidth = 0.002) +
  scale_x_discrete(limits=sing_mod_lbl) +
  scale_y_continuous(limits=c(0.19, 0.23)) +
  scale_fill_manual(values = c("Train MSE" = "#404788FF", "Test MSE"="#95D840FF")) +
  labs(title="Comparison of MSEs of models with one explanatory variables", x="Model Name", y="MSE") + 
  theme(legend.title=element_blank(), legend.position="right")
dev.off()


var2_mod_lbl <- c("SameSubcar+Price","SameSubcat+PrevReturnRate","SameSubcat+Categorie","SameSubcat+NumSubcat")
var2_mse_df <-  train_mse_var2%>% t() %>% data.frame() %>% setNames(c("MSE"))
var2_mse_df$model_name <- var2_mod_lbl
var2_mse_df$data_type <- rep("Train MSE",times=nrow(var2_mse_df))

var2_test_mse_df <- test_mse_var2%>% t() %>% data.frame() %>% setNames(c("MSE"))
var2_test_mse_df$model_name <- var2_mod_lbl
var2_test_mse_df$data_type <- rep("Test MSE",times=nrow(var2_test_mse_df))

var2_mse_df <- rbind(var2_mse_df, var2_test_mse_df)

ggplot(var2_mse_df, aes(x = model_name)) +
  geom_dotplot(aes(y= MSE, fill=data_type), binaxis='y', stackdir='center', binwidth = 0.002) +
  scale_x_discrete(limits=var2_mod_lbl) +
  scale_fill_manual(values = c("Train MSE" = "#404788FF", "Test MSE"="#95D840FF")) +
  scale_y_continuous(limits=c(0.165, 0.205)) +
  labs(title="Comparison of MSEs of models with two explanatory variables", x="Model Name", y="MSE") + 
  theme(legend.title=element_blank(), legend.position="right")


p3_mod_lbl <- c("LG-1Var","LG-2Var","LG-Optimal","LG-Poly+Inter","KNN100", "KNN1")
p3_mse_df <-  p3_train_mse%>% t() %>% data.frame() %>% setNames(c("MSE"))
p3_mse_df$model_name <- p3_mod_lbl
p3_mse_df$data_type <- rep("Train MSE",times=nrow(p3_mse_df))

p3_test_mse_df <- p3_test_mse%>% t() %>% data.frame() %>% setNames(c("MSE"))
p3_test_mse_df$model_name <- p3_mod_lbl
p3_test_mse_df$data_type <- rep("Test MSE",times=nrow(p3_test_mse_df))

p3_mse_df <- rbind(p3_mse_df, p3_test_mse_df)


min_mse_df <- rep(testdata_min_mse, times = nrow(p3_test_mse_df)) %>% data.frame() %>% setNames(c("MSE"))
min_mse_df$model_name <- p3_mod_lbl
min_mse_df$data_type <- rep("Min MSE",times=nrow(p3_test_mse_df))
p3_mse_df <- rbind(p3_mse_df, min_mse_df)


ggplot(p3_mse_df %>% filter(data_type != "Test MSE" %>% str_sort(data_type, decreasing=FALSE)),
       aes(x = model_name)) +
  geom_dotplot(aes(y= MSE, fill=data_type), binaxis='y', stackdir='center', binwidth = 0.015, alpha=0.8) +
  geom_dotplot(data = p3_mse_df %>% filter(data_type == "Test MSE"), aes(x = model_name, y= MSE, fill=data_type), 
               binaxis='y', stackdir='center', binwidth = 0.015, alpha = 0.8) +
  scale_x_discrete(limits=p3_mod_lbl) +
  scale_y_continuous(limits=c(0.0, 0.35)) +
  scale_fill_manual(values = c( "Test MSE"="#95D840FF","Train MSE" = "#404788FF", "Min MSE"="#DCE319FF")) +
  labs(title="Comparison of six examined models' MSEs", x="Model Name", y="MSE") + 
  theme(legend.title=element_blank(), legend.position="right")