library("dplyr")
library("ggpubr")
library(e1071)
library(viridis)
set.seed(0207)

rm(t_k, t_n)
rm(t_dt)
rm(train_k, train_n)
rm(train_dt, train_model, train_para)

train_k <- 1000
train_n <- c(1000, 10000, 50000)

train_para <-matrix(nrow=train_k, ncol=length(train_n))
colnames(train_para) <- train_n
#rm(train_para)

'for (i in 1:length(train_n)){
  for (j in 1:train_k){
    train_idx <- sample(c(1:nrow(full_df)), size=train_n[i], replace=FALSE)
    train_dt <-setDT(full_df[train_idx,])
    train_model <- glm(return_state ~ mod_preis + cust_prev_return_rate +
          qty_same_subcat * num_subcat_ord + mod_subcat1_Damen + mod_subcat1_Herren + mod_subcat1_Maedchen,
        data = train_dt, family = "binomial")  
    train_para[i,j] <- coef(train_model)["cust_prev_return_rate"]
    
  }
  
}
'
train_para <- sapply(1:length(train_n), function(x) sapply(1:train_k, function (y){
  train_idx <- sample(c(1:nrow(full_df)), size=train_n[x], replace=TRUE)
  train_dt <-setDT(full_df[train_idx,])
  train_model <- glm(return_state ~ mod_preis + cust_prev_return_rate +
                       qty_same_subcat * num_subcat_ord + mod_subcat1_Damen + mod_subcat1_Herren + mod_subcat1_Maedchen,
                     data = train_dt, family = "binomial")  
  train_para[y,x] <- coef(train_model)["cust_prev_return_rate"] 
} ))

colnames(train_para) <- train_n
rownames(train_para) <- c(1:train_k)
mean(train_para[,1])
mean(train_para[,2])
mean(train_para[,3])

sd(train_para[,1])
sd(train_para[,2])
sd(train_para[,3])

sim_sd <- c(sd(train_para[,1]), sd(train_para[,2]), sd(train_para[,3]))
sim_sd_df <- data.frame(train_n, sim_sd)

var(train_para[,1])
var(train_para[,2])
var(train_para[,3])

kurtosis(train_para[,1])
kurtosis(train_para[,2])
kurtosis(train_para[,3])


sqrt(var(train_para[,1])/(train_k))
sqrt(var(train_para[,2])/(train_k))
sqrt(var(train_para[,3])/(train_k))

shapiro.test(train_para[,1])
shapiro.test(train_para[,2])
shapiro.test(train_para[,3])


vline_train_para <- train_para_df %>%
  group_by(sam_size) %>%
  summarise(mean = mean(beta))

train_para_df <- reshape2::melt(train_para,measured.vars = train_n, varnames=c("sam_order","sam_size"), value.name = "beta")
jpeg(filename = "C:/Users/hddt/OneDrive/Desktop/HS_Reutlingen/Courses/05.DataScience/Simulationsstudie/Charts/Simulation_Distr.jpeg")
ggplot(data = train_para_df, aes(x=beta, color = factor(sam_size) , fill=factor(sam_size))) +
  geom_density(alpha=0.5) +
  geom_vline(data=vline_train_para, aes(xintercept=mean, color=factor(sam_size)),
             linetype="dashed")+
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis(discrete = TRUE, option = "D", name = "Sample size (n)") +
  guides(colour="none", fill = guide_legend(override.aes = list(colour = NA))) +
  labs(title="Distribution of a variable's coefficient  based on different sample sizes",
       x = "Estimated coeffcient of previous return rate variable", y="Density")
dev.off()  

ggplot(data = sim_sd_df, aes(x=factor(train_n), y = sim_sd, group=1)) +
  geom_line(size=1.5, color="#55C667FF") +
  geom_point() +
  geom_text(data = sim_sd_df, aes(label = round(sim_sd, 2)), vjust = +0.1, hjust = -0.3,
      show.legend = FALSE) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(title="Standard deviation of estimated coefficinet according to sample sizes",
       x = "Sample Size", y="Standard Deviation")
  