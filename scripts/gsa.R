library(randomForest)
library(rpart)
library(rpart.plot)
library(ggpubr)
library(parallel)
library(tidyverse)
library(rcartocolor)

sex_cue = "frequency"
connectivity = "none"

m = 2000

freq_50 = runif(m, min = 0.5, max = 0.8)
rho_freq = runif(m, min = 5, max = 15)
phi = runif(m, min = 1, max = 50)
f.max = runif(m, min = 0.7, max = 1)
#rho = runif(m, min = 0.6, max = 0.9)
#a_50 = runif(m, min = 7, max = 10)
c = runif(m, min = 1, max = 4)
f.frac = runif(m, min = 0.8, max = 1)
nat.mort = runif(m, min = 0.035, max = 0.045)
allee = runif(m, min = 1, max = 5)
b = runif(m, min = 0.01, max = 0.1)


out_gsa = mcmapply(get_pop_n, 
                              freq_50 = freq_50,
                              rho_freq = rho_freq,
                              phi = phi,
                              f.max = f.max,
                              #rho = rho,
                              #a_50 = a_50,
                              c = c,
                              f.frac = f.frac,
                              nat.mort = nat.mort,
                              allee = allee,
                              b = b,
                   MoreArgs = list(connectivity = connectivity, sex_cue = sex_cue, 
                                   agg = 5, n_agg_protected = 5),
                   SIMPLIFY = FALSE, mc.cores = parallel::detectCores()-1)

#saveRDS(out_gsa, "outputs/gsa/fixed_noconnect_out_gsa.rds")

gsa_tot_size = unname(unlist(lapply(out_gsa, `[`, "tot_size")))
gsa_avg_sexratio = rowMeans(1 - t(matrix(unname(unlist(lapply(out_gsa, `[`, "prop_female"))), nrow = 5)))
gsa_mean_age = unname(unlist(lapply(out_gsa, `[`, "mean_age_unprotected")))
#gsa_tot_catch = unname(unlist(lapply(out_gsa, `[`, "tot_catch_equil")))


RF_N_data <- data.frame(gsa_tot_size=gsa_tot_size,
                        freq_50 = freq_50,
                        rho_freq = rho_freq,
                        phi = phi,
                        f.max = f.max,
                        #rho = rho,
                        #a_50 = a_50,
                        c = c,
                        f.frac = f.frac,
                        allee = allee,
                        b = b,
                        nat.mort = nat.mort)

RF_sexratio_data <- data.frame(gsa_avg_sexratio = gsa_avg_sexratio, 
                               freq_50 = freq_50,
                               rho_freq = rho_freq,
                               phi = phi,
                               f.max = f.max,
                               #rho = rho,
                               #a_50 = a_50,
                               c = c,
                               f.frac = f.frac,
                               allee = allee,
                               b = b,
                               nat.mort = nat.mort)

RF_meanage_data <- data.frame(gsa_mean_age = gsa_mean_age,
                              freq_50 = freq_50,
                              rho_freq = rho_freq,
                              phi = phi,
                              f.max = f.max,
                              #rho = rho,
                              #a_50 = a_50,
                              c = c,
                              f.frac = f.frac,
                              allee = allee,
                              b = b,
                              nat.mort = nat.mort)





RF_N <- randomForest(gsa_tot_size~.,data=RF_N_data, mtry = 4, importance = TRUE, proximity=TRUE)
RF_sexratio <- randomForest(gsa_avg_sexratio~.,data=RF_sexratio_data, mtry = 4, importance=TRUE,proximity=TRUE)
RF_meanage <- randomForest(gsa_mean_age~.,data=RF_meanage_data, mtry = 4, importance=TRUE,proximity=TRUE)


print(RF_N) 


Neq_GSA <-
  data.frame(Parameter = rownames(RF_N$importance),
             Importance = RF_N$importance[, 1]) %>% 
  mutate(param_name = case_when(Parameter == "phi" ~ "Fertilization \nsteepness",
                                Parameter == "f.max" ~ "Max fertilization",
                                Parameter == "rho" ~ "Sex change \nsteepness",
                                Parameter == "a_50" ~ "50% sex change",
                                Parameter == "c" ~ "Aggregation time",
                                Parameter == "f.frac" ~ "Sex difference \nin aggregation time",
                                Parameter == "allee" ~ "Allee effect",
                                Parameter == "b" ~ "Beverton-Holt \nsteepness",
                                Parameter == "nat.mort" ~ "Natural mortality",
                                Parameter == "freq_50" ~ "50% sex change",
                                Parameter == "rho_freq" ~ "Sex change \nsteepness"
                                ))

Neq_GSA$Parameter <-
  factor(Neq_GSA$Parameter, levels = Neq_GSA$Parameter[order(Neq_GSA$Importance,decreasing = TRUE)])

p1 = Neq_GSA %>% 
  ggplot(aes(reorder(param_name, -Importance), Importance)) + 
  geom_bar(aes(fill = param_name), stat = "identity") + 
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    text = element_text(size=8),
    axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.4),
    axis.title.x = element_blank()
  ) +
  scale_fill_carto_d(guide = "none", palette = "Safe") +
  lims(y = c(0, NA)) +
  labs(subtitle  = "Long-term Population Size", y = "Importance")


Ageeq_GSA <-
  data.frame(Parameter = rownames(RF_meanage$importance),
             Importance = RF_meanage$importance[, 1]) %>% 
  mutate(param_name = case_when(Parameter == "phi" ~ "Fertilization \nsteepness",
                                Parameter == "f.max" ~ "Max fertilization",
                                Parameter == "rho" ~ "Sex change \nsteepness",
                                Parameter == "a_50" ~ "50% sex change",
                                Parameter == "c" ~ "Aggregation time",
                                Parameter == "f.frac" ~ "Sex difference \nin aggregation time",
                                Parameter == "allee" ~ "Allee effect",
                                Parameter == "b" ~ "Beverton-Holt \nsteepness",
                                Parameter == "nat.mort" ~ "Natural mortality",
                                Parameter == "freq_50" ~ "50% sex change",
                                Parameter == "rho_freq" ~ "Sex change \nsteepness"
  ))

Ageeq_GSA$Parameter <-
  factor(Ageeq_GSA$Parameter, levels = Ageeq_GSA$Parameter[order(Ageeq_GSA$Importance,decreasing = TRUE)])

p3 = Ageeq_GSA %>% 
  ggplot(aes(reorder(param_name, -Importance), Importance)) + 
  geom_bar(aes(fill = param_name), stat = "identity") + 
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    text = element_text(size=8),
    axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.4),
    axis.title.x = element_blank()
  ) +
  scale_fill_carto_d(guide = "none", palette = "Safe") +
  lims(y = c(0, NA)) +
  labs(subtitle  = "Long-term Mean Age", y = "Importance")


sexratio_GSA <-
  data.frame(Parameter = rownames(RF_sexratio$importance),
             Importance = RF_sexratio$importance[, 1]) %>% 
  mutate(param_name = case_when(Parameter == "phi" ~ "Fertilization \nsteepness",
                                Parameter == "f.max" ~ "Max fertilization",
                                Parameter == "rho" ~ "Sex change \nsteepness",
                                Parameter == "a_50" ~ "50% sex change",
                                Parameter == "c" ~ "Aggregation time",
                                Parameter == "f.frac" ~ "Sex difference \nin aggregation time",
                                Parameter == "allee" ~ "Allee effect",
                                Parameter == "b" ~ "Beverton-Holt \nsteepness",
                                Parameter == "nat.mort" ~ "Natural mortality",
                                Parameter == "freq_50" ~ "50% sex change",
                                Parameter == "rho_freq" ~ "Sex change \nsteepness"
  ))

sexratio_GSA$Parameter <-
  factor(sexratio_GSA$Parameter, levels = sexratio_GSA$Parameter[order(sexratio_GSA$Importance,decreasing = TRUE)])

p2 = sexratio_GSA %>% 
  ggplot(aes(reorder(param_name, -Importance), Importance)) + 
  geom_bar(aes(fill = param_name), stat = "identity") + 
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    text = element_text(size=8),
    axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.4),
    axis.title.x = element_blank()
  ) +
  scale_fill_carto_d(guide = "none", palette = "Safe") +
  lims(y = c(0, NA)) +
  labs(subtitle  = "Long-term Sex Ratio")



tiff("plots/gsa/freq_noconnect_importance.tiff", units="in", width=6.5, height=2.5, res=600)
ggarrange(p1,p2, p3, nrow = 1)
dev.off()



tiff("plots/gsa/pop_size_nonlogscale.tiff", units="in", width=5, height=3, res=600)
p1
dev.off()



rpartmod <- rpart(gsa_sd_sexratio ~., data = RF_sdsexratio_data)
rpart.plot(rpartmod)

