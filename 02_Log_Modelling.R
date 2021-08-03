library(data.table)
library(fastDummies)
#library(GGally)
library(dplyr)
library(Hmisc)
library(car)
library(pROC)
library(caret)
library(psych)
rm(var10_log_mod,var3_log_mod,var5_log_mod,var6_log_mod)
rm(mseComplogmod,mseSimplogmod,mseVar10logmod,mseVar3logmod,mseVar5logmod,mseVar6logmod)
rm(sam_dt,sam_train,sam_train_x, sam_train_y, sam_train_cat,sam_train_num,sam_train_x_num)

set.seed(2021)
#Define the size of sample
sam_n <- 10000
sam_ind <- sample(c(1:nrow(full_df)), size=sam_n, replace=FALSE)
sam_dt <-setDT(full_df[sam_ind,])
sam_dt <- sam_dt[,c("return_state","ord.order_id","ord.prod_id","ord.qty", "hour_of_order",
          "order_cust_id", "cust_prev_return_rate","cust_region",
          "prod_size","mod_preis","prod_num_per_order","qty_same_subcat",
          "num_subcat_ord","num_cat_ord",
          "mod_subcat1","mod_subcat2")]


train_ind <- sample.int(n = sam_n, size = floor(.75*sam_n), replace = F)
sam_train <- sam_dt[train_ind]
sam_test <- sam_dt[-train_ind]

sam_train_x <- sam_train[,c("ord.order_id","ord.prod_id","ord.qty", "hour_of_order",
                         "order_cust_id", "cust_prev_return_rate","cust_region",
                         "prod_size","mod_preis","prod_num_per_order","qty_same_subcat",
                         "num_subcat_ord","num_cat_ord",
                         "mod_subcat1","mod_subcat2")]
sam_train_y <- sam_train[,c("return_state")]
sam_test_x <- sam_test[,c("ord.order_id","ord.prod_id","ord.qty", "hour_of_order",
                           "order_cust_id", "cust_prev_return_rate","cust_region",
                           "prod_size","mod_preis","prod_num_per_order","qty_same_subcat",
                           "num_subcat_ord","num_cat_ord",
                           "mod_subcat1","mod_subcat2")]
sam_test_y <- sam_test[,c("return_state")]

sam_train_x <- dummy_cols(sam_train_x, select_columns = "mod_subcat1")
sam_train_x <- dummy_cols(sam_train_x, select_columns = "cust_region")
sam_test_x <- dummy_cols(sam_test_x, select_columns = "mod_subcat1")
sam_test_x <- dummy_cols(sam_test_x, select_columns = "cust_region")


'sam_train_x <- sam_train_x %>%
    group_by(ord.order_id) %>%
    mutate(prod_num_per_order = sum(ord.qty))


sam_train_x <- sam_train_x %>%
    group_by(ord.order_id, mod_subcat1, mod_subcat2) %>%
    mutate(qty_same_subcat = sum(ord.qty))

sam_train_x <- sam_train_x %>%
    group_by(ord.order_id, mod_subcat1) %>%
    mutate(num_subcat_ord = n_distinct(mod_subcat2))


sam_train_x <- sam_train_x %>%
    group_by(ord.order_id) %>%
    mutate(num_cat_ord = n_distinct(mod_subcat1))
'
#class(sam_train_x)
#sam_train_x <- setDT(sam_train_x)
sam_train_x <- sam_train_x[ , !c("mod_subcat1","mod_subcat2","cust_region","ord.qty","ord.prod_id","order_cust_id","ord.order_id")]
sam_test_x <- sam_test_x[ , !c("mod_subcat1","mod_subcat2","cust_region","ord.qty","ord.prod_id","order_cust_id","ord.order_id")]

#Check correlation between variables
sam_train_x_num <- sam_train_x[, c("prod_num_per_order","prod_size","mod_preis","cust_prev_return_rate"
                                   ,"hour_of_order","qty_same_subcat", "num_cat_ord","num_subcat_ord")]
sam_train_num <- cbind(sam_train_y, sam_train_x_num)
rcorr(as.matrix(sam_train_num))
sam_train_cat <- cbind(sam_train_y, sam_train_x[,!c("prod_num_per_order","prod_size","mod_preis","cust_prev_return_rate"
                                                  ,"hour_of_order","qty_same_subcat", "num_cat_ord","num_subcat_ord")])
rcorr(as.matrix(sam_train_cat))
rcorr(as.matrix(sam_train_x_num))
#rm(sam_train_cat)
sam_train <- cbind(sam_train_y, sam_train_x)
sam_test <- cbind(sam_test_y, sam_test_x)

rcorr(as.matrix(sam_train))
rcorr(as.matrix(sam_train[,!c("return_state")]))

summary(sam_train_num)
psych::describe(sam_train_num)
psych::describe(sam_train_cat)
psych::describe(sam_train)
psych::describe(sam_test)

#Apply Log_Reg to build a model 
var10_log_mod <- glm(return_state ~ prod_num_per_order + prod_size + mod_preis + cust_prev_return_rate +
                     hour_of_order + qty_same_subcat + num_cat_ord + num_subcat_ord +
                     mod_subcat1_Damen + mod_subcat1_Herren + mod_subcat1_Maedchen +
                     cust_region_BE +cust_region_BY + cust_region_HB + cust_region_HE +
                     cust_region_HH + cust_region_MV + cust_region_NI + cust_region_NW + cust_region_RP +
                     cust_region_SH + cust_region_SL + cust_region_SN + cust_region_ST +cust_region_TH,
                     data = sam_train, family = "binomial")
summary(var10_log_mod)
vif(var10_log_mod)

mseVar10logmod <- mean((sam_train$return_state - predict(var10_log_mod, sam_train, type="response"))^2)
mseVar10logmod


mseVar10logmod_test <- mean((sam_test$return_state - predict(var10_log_mod, sam_test, type="response"))^2)
mseVar10logmod_test

var5_log_mod <- glm(return_state ~ mod_preis + cust_prev_return_rate +
                        qty_same_subcat + num_subcat_ord + mod_subcat1_Damen + mod_subcat1_Herren + mod_subcat1_Maedchen,
                     data = sam_train, family = "binomial")
summary(var5_log_mod)

vif(var5_log_mod)

mseVar5logmod <- mean((sam_train$return_state - predict(var5_log_mod, sam_train, type="response"))^2)
mseVar5logmod

mseVar5logmod_test <- mean((sam_test$return_state - predict(var5_log_mod, sam_test, type="response"))^2)
mseVar5logmod_test

cross_log_mod <- glm(return_state ~ cust_prev_return_rate + mod_preis *
                       num_subcat_ord * qty_same_subcat *  mod_subcat1_Damen *
                        mod_subcat1_Herren * mod_subcat1_Maedchen,
                        data = sam_train, family = "binomial")
na.omit(coef(summary(cross_log_mod)))
summary(cross_log_mod)$coefficients
mseCrosslogmod <- mean((sam_train$return_state - predict(cross_log_mod, sam_train, type="response"))^2)
mseCrosslogmod

mseCrosslogmod_test <- mean((sam_test$return_state - predict(cross_log_mod, sam_test, type="response"))^2)
mseCrosslogmod_test


crossAdj_log_mod <- glm(return_state ~ cust_prev_return_rate + mod_preis * num_subcat_ord * qty_same_subcat +
                           mod_subcat1_Damen +
                        mod_subcat1_Herren + mod_subcat1_Maedchen,
                    data = sam_train, family = "binomial")
summary(crossAdj_log_mod)$coefficients
mseCrossAdjlogmod <- mean((sam_train_y$return_state - predict(crossAdj_log_mod, sam_train, type="response"))^2)
mseCrossAdjlogmod

mseCrossAdjlogmod_test <- mean((sam_test$return_state - predict(crossAdj_log_mod, sam_test, type="response"))^2)
mseCrossAdjlogmod_test

crossAdj2_log_mod <- glm(return_state ~ cust_prev_return_rate + mod_preis + 
                             num_subcat_ord * qty_same_subcat *
                            mod_subcat1_Damen *
                            mod_subcat1_Herren * mod_subcat1_Maedchen,
                        data = sam_train, family = "binomial")
summary(crossAdj2_log_mod)$coefficients
mseCrossAdj2logmod <- mean((sam_train_y$return_state - predict(crossAdj2_log_mod, sam_train, type="response"))^2)
mseCrossAdj2logmod

mseCrossAdj2logmod_test <- mean((sam_test$return_state - predict(crossAdj2_log_mod, sam_test, type="response"))^2)
mseCrossAdj2logmod_test

crossAdj3_log_mod <- glm(return_state ~ cust_prev_return_rate + mod_preis + 
                             num_subcat_ord + qty_same_subcat *
                             mod_subcat1_Damen *
                             mod_subcat1_Herren * mod_subcat1_Maedchen,
                         data = sam_train, family = "binomial")
summary(crossAdj3_log_mod)$coefficients
mseCrossAdj3logmod <- mean((sam_train_y$return_state - predict(crossAdj3_log_mod, sam_train, type="response"))^2)
mseCrossAdj3logmod

mseCrossAdj3logmod_test <- mean((sam_test$return_state - predict(crossAdj3_log_mod, sam_test, type="response"))^2)
mseCrossAdj3logmod_test

crossAdj4_log_mod <- glm(return_state ~ cust_prev_return_rate + mod_preis + 
                             qty_same_subcat  +  num_subcat_ord *
                             mod_subcat1_Damen *
                             mod_subcat1_Herren * mod_subcat1_Maedchen,
                         data = sam_train, family = "binomial")
summary(crossAdj4_log_mod)$coefficients
mseCrossAdj4logmod <- mean((sam_train_y$return_state - predict(crossAdj4_log_mod, sam_train, type="response"))^2)
mseCrossAdj4logmod

mseCrossAdj4logmod_test <- mean((sam_test$return_state - predict(crossAdj4_log_mod, sam_test, type="response"))^2)
mseCrossAdj4logmod_test

crossAdj5_log_mod <- glm(return_state ~ cust_prev_return_rate + mod_preis * 
                             qty_same_subcat  +  num_subcat_ord +
                             mod_subcat1_Damen +
                             mod_subcat1_Herren + mod_subcat1_Maedchen,
                         data = sam_train, family = "binomial")

summary(crossAdj5_log_mod)$coefficients
mseCrossAdj5logmod <- mean((sam_train_y$return_state - predict(crossAdj5_log_mod, sam_train, type="response"))^2)
mseCrossAdj5logmod

mseCrossAdj5logmod_test <- mean((sam_test$return_state - predict(crossAdj5_log_mod, sam_test, type="response"))^2)
mseCrossAdj5logmod_test


crossAdj6_log_mod <- glm(return_state ~ cust_prev_return_rate + mod_preis * 
                              num_subcat_ord +  qty_same_subcat  +
                             mod_subcat1_Damen +
                             mod_subcat1_Herren + mod_subcat1_Maedchen,
                         data = sam_train, family = "binomial")

summary(crossAdj6_log_mod)$coefficients
mseCrossAdj6logmod <- mean((sam_train_y$return_state - predict(crossAdj6_log_mod, sam_train, type="response"))^2)
mseCrossAdj6logmod

mseCrossAdj6logmod_test <- mean((sam_test$return_state - predict(crossAdj6_log_mod, sam_test, type="response"))^2)
mseCrossAdj6logmod_test

var6_log_mod <- glm(return_state ~ mod_preis + cust_prev_return_rate +
                        qty_same_subcat * num_subcat_ord + mod_subcat1_Damen + mod_subcat1_Herren + mod_subcat1_Maedchen,
                    data = sam_train, family = "binomial")
summary(var6_log_mod)
vif(var6_log_mod)

mseVar6logmod <- mean((sam_train$return_state - predict(var6_log_mod, sam_train, type="response"))^2)
mseVar6logmod

mseVar6logmod_test <- mean((sam_test$return_state - predict(var6_log_mod, sam_test, type="response"))^2)
mseVar6logmod_test

cross_mse_train <- c()
cross_mse_test <- c()

cross_mse_train <- cbind(cross_mse_train, mseCrosslogmod)
cross_mse_train <- cbind(cross_mse_train, mseCrossAdjlogmod)
cross_mse_train <- cbind(cross_mse_train, mseCrossAdj2logmod)
cross_mse_train <- cbind(cross_mse_train, mseCrossAdj3logmod)
cross_mse_train <- cbind(cross_mse_train, mseCrossAdj4logmod)
cross_mse_train <- cbind(cross_mse_train, mseCrossAdj5logmod)
cross_mse_train <- cbind(cross_mse_train, mseCrossAdj6logmod)
cross_mse_train <- cbind(cross_mse_train, mseVar6logmod)

cross_mse_test <- cbind(cross_mse_test, mseCrosslogmod_test)
cross_mse_test <- cbind(cross_mse_test, mseCrossAdjlogmod_test)
cross_mse_test <- cbind(cross_mse_test, mseCrossAdj2logmod_test)
cross_mse_test <- cbind(cross_mse_test, mseCrossAdj3logmod_test)
cross_mse_test <- cbind(cross_mse_test, mseCrossAdj4logmod_test)
cross_mse_test <- cbind(cross_mse_test, mseCrossAdj5logmod_test)
cross_mse_test <- cbind(cross_mse_test, mseCrossAdj6logmod_test)
cross_mse_test <- cbind(cross_mse_test, mseVar6logmod_test)

which.min(cross_mse_train)
which.min(cross_mse_test)

cross_mse_train
cross_mse_test


poly_log_mod <- glm(return_state ~ poly(mod_preis,3) + poly(cust_prev_return_rate,3) +
                        poly(qty_same_subcat,3) * poly(num_subcat_ord,3) + mod_subcat1_Damen + mod_subcat1_Herren + mod_subcat1_Maedchen,
                    data = sam_train, family = "binomial")
summary(poly_log_mod)

msePolylogmod <- mean((sam_train$return_state - predict(poly_log_mod, sam_train, type="response"))^2)
msePolylogmod

msePolylogmod_test <- mean((sam_test$return_state - predict(poly_log_mod, sam_test, type="response"))^2)
msePolylogmod_test


'polyComp_log_mod <- glm(return_state ~ poly(mod_preis,3) * poly(cust_prev_return_rate,3) *
                        poly(qty_same_subcat,3) * poly(num_subcat_ord,3) + mod_subcat1_Damen + mod_subcat1_Herren + mod_subcat1_Maedchen,
                    data = sam_train, family = "binomial")
summary(polyComp_log_mod)

msePolyComplogmod <- mean((sam_train$return_state - predict(polyComp_log_mod, sam_train, type="response"))^2)
msePolyComplogmod
#train_mse <- cbind(train_mse, msePolyComplogmod)

msePolyComplogmod_test <- mean((sam_test$return_state - predict(polyComp_log_mod, sam_test, type="response"))^2)
msePolyComplogmod_test
#test_mse <- cbind(test_mse, msePolyComplogmod_test)
'
train_mse <- c()
test_mse <- c()

train_mse <- cbind(train_mse, mseVar10logmod)
train_mse <- cbind(train_mse, mseVar5logmod)
#train_mse <- cbind(train_mse, mseCrosslogmod)
#train_mse <- cbind(train_mse, mseCrossAdjlogmod)
train_mse <- cbind(train_mse, mseVar6logmod)
train_mse <- cbind(train_mse, msePolylogmod)

test_mse <- cbind(test_mse, mseVar10logmod_test)
test_mse <- cbind(test_mse, mseVar5logmod_test)
#test_mse <- cbind(test_mse, mseCrosslogmod_test)
#test_mse <- cbind(test_mse, mseCrossAdjlogmod_test)
test_mse <- cbind(test_mse, mseVar6logmod_test)
test_mse <- cbind(test_mse, msePolylogmod_test)

train_mse
test_mse
which.min(train_mse)
which.min(test_mse)

cv_control <- trainControl(method = "cv", number = 5)
cv_cross_log <- train(as.factor(return_state) ~ mod_preis * cust_prev_return_rate *
                          qty_same_subcat * num_subcat_ord +
                          mod_subcat1_Damen + mod_subcat1_Herren + mod_subcat1_Maedchen,
                      data = sam_train, method ="glm", family = "binomial", trControl = cv_control)
summary(cv_cross_log)

cv_var10_log <- train(as.factor(return_state) ~ prod_num_per_order + prod_size + mod_preis + cust_prev_return_rate +
                          hour_of_order + qty_same_subcat + num_cat_ord + num_subcat_ord +
                          mod_subcat1_Damen + mod_subcat1_Herren + mod_subcat1_Maedchen +
                          cust_region_BE +cust_region_BY + cust_region_HB + cust_region_HE +
                          cust_region_HH + cust_region_MV + cust_region_NI + cust_region_NW + cust_region_RP +
                          cust_region_SH + cust_region_SL + cust_region_SN + cust_region_ST +cust_region_TH,
                      data = sam_train, method ="glm", family = "binomial", trControl = cv_control)
summary(cv_var10_log)
cv_var6_log <- train(as.factor(return_state) ~ mod_preis + cust_prev_return_rate +
                         qty_same_subcat * num_subcat_ord +
                         mod_subcat1_Damen + mod_subcat1_Herren + mod_subcat1_Maedchen,
                     data = sam_train, method ="glm", family = "binomial", trControl = cv_control)
summary(cv_var6_log)

cv_mseVar10logmod <- mean((sam_train$return_state-predict(cv_var10_log, sam_train, type="prob")[,2])^2)
cv_mseCrosslogmod <- mean((sam_train$return_state-predict(cv_cross_log, sam_train, type="prob")[,2])^2)
cv_mseVar6logmod <- mean((sam_train$return_state-predict(cv_var6_log, sam_train, type="prob")[,2])^2)


cv_mseVar10logmod_test <- mean((sam_test$return_state-predict(cv_var10_log, sam_test, type="prob")[,2])^2)
cv_mseCrosslogmod_test <- mean((sam_test$return_state-predict(cv_cross_log, sam_test, type="prob")[,2])^2)
cv_mseVar6logmod_test <- mean((sam_test$return_state-predict(cv_var6_log, sam_test, type="prob")[,2])^2)

cv_mseVar10logmod_test
cv_mseCrosslogmod_test
cv_mseVar6logmod_test

par(pty ="s")
roc(sam_train_y$return_state, var5_log_mod$fitted.values, 
    legacy.axes=TRUE, percent=TRUE, 
    xlab="False Positive Percentage",
    ylab="True Positive Percentage",
    col="#377eb8", plot=TRUE,lwd=3, 
    print.auc=TRUE)
plot.roc(sam_train_y$return_state, var6_log_mod$fitted.values,
    percent =  TRUE, col="#4daf4a", lwd=3,
    print.auc=TRUE, add=TRUE, print.auc.y=40)
par(pty ="m")

mod_lbl <- c("10Var","5Var", "5Var+CrQtyNum","Poly3")
mse_df <-  train_mse%>% t() %>% data.frame() %>% setNames(c("MSE"))
mse_df$model_name <- mod_lbl
mse_df$data_type <- rep("Train",times=nrow(mse_df))

test_mse_df <- test_mse%>% t() %>% data.frame() %>% setNames(c("MSE"))
test_mse_df$model_name <- mod_lbl
test_mse_df$data_type <- rep("Test",times=nrow(test_mse_df))

mse_df <- rbind(mse_df, test_mse_df)
jpeg(filename = "C:/Users/hddt/OneDrive/Desktop/HS_Reutlingen/Courses/05.DataScience/Simulationsstudie/Charts/MSE_Mods.jpeg")
ggplot(mse_df, aes(x = model_name)) +
    geom_dotplot(aes(y= MSE, fill=data_type), binaxis='y', stackdir='center', binwidth = 0.0002) +
    scale_y_continuous(limits=c(0.157, 0.161)) +
    scale_fill_manual(values = c("#95D840FF","#404788FF"), labels = c("Test MSE", "Train MSE")) +
    labs(title="MSE of examined models on training and testing sets", x="Model Name", y="MSE") + 
    theme(legend.title=element_blank(), legend.position="right")
dev.off()

cross_mod_lbl <- c("FullCross","CrPrice", "CrSubcatCat","CrQtyCat","CrNumCat","CrPrQty","CrPrNum","CrQtyNum")
cross_mse_df <-  cross_mse_train%>% t() %>% data.frame() %>% setNames(c("MSE"))
cross_mse_df$model_name <- cross_mod_lbl
cross_mse_df$data_type <- rep("Train",times=nrow(cross_mse_df))

cross_test_mse_df <- cross_mse_test%>% t() %>% data.frame() %>% setNames(c("MSE"))
cross_test_mse_df$model_name <- cross_mod_lbl
cross_test_mse_df$data_type <- rep("Test",times=nrow(cross_test_mse_df))

cross_mse_df <- rbind(cross_mse_df, cross_test_mse_df)
cross_mse_df <- cross_mse_df %>%
                arrange(desc(cross_mse_train))

jpeg(filename = "C:/Users/hddt/OneDrive/Desktop/HS_Reutlingen/Courses/05.DataScience/Simulationsstudie/Charts/MSE_Cross_Mods.jpeg")
ggplot(cross_mse_df, aes(x = model_name)) +
    geom_dotplot(aes(y= MSE, fill=data_type), binaxis='y', stackdir='center', binwidth = 0.0002) +
    scale_x_discrete(limits=cross_mod_lbl) +
    scale_y_continuous(limits=c(0.157, 0.161)) +
    scale_fill_manual(values = c("#95D840FF","#404788FF"), labels = c("Test MSE", "Train MSE")) +
    labs(title="MSE of models with different interaction terms on training and testing sets", x="Model Name", y="MSE") + 
    theme(legend.title=element_blank(), legend.position="right")
dev.off()