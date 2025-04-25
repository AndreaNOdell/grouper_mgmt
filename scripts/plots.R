library(ggplot2)
library(tidyverse)
library(PNWColors)


#####################################
##### spatial/temporal heatmaps #####
#####################################

tot_size = unname(unlist(lapply(out, `[`, "tot_size")))
prop_male = rowMeans(1 - t(matrix(unname(unlist(lapply(out, `[`, "prop_female"))), nrow = 5)))
prop_male_sd =  apply(1 - t(matrix(unname(unlist(lapply(out, `[`, "prop_female"))), nrow = 5)), 1, sd)
prop_male_unprotected = 1 - t(matrix(unname(unlist(lapply(out, `[`, "prop_female"))), nrow = 5))[,5]
tot_size_agg = t(matrix(unname(unlist(lapply(out, `[`, "tot_size_agg"))), nrow = 5))
unprotected_relative.size = t(matrix(unname(unlist(lapply(out, `[`, "tot_size_agg"))), nrow = 5))[,5]/t(matrix(unname(unlist(lapply(out, `[`, "tot_size_agg"))), nrow = 5))[,1]
#catch = t(matrix(unname(unlist(lapply(out, `[`, "catch_equil"))), nrow = 5))
tot_catch = unname(unlist(lapply(out, `[`, "tot_catch_equil")))
mean_age = unname(unlist(lapply(out, `[`, "mean_age_unprotected")))

unfished.tot_size = unname(unlist(lapply(unfished.out, `[`, "tot_size")))

mgmt_output = cbind(mgmt_strat, unprotected_relative.size, tot_size, prop_male_unprotected, tot_catch,mean_age) %>% 
  mutate(unfished_size = unfished.tot_size[1])

#mgmt_output %>% 
#  filter(connectivity == "none", sex_cue == "frequency", agg_protected %in% 1:4) %>% 
#  ggplot(aes(y = tot_size/max(tot_size), x = tot_catch/max(tot_catch))) +
#  geom_line(aes(col = as.factor(agg_protected))) +
#  geom_point(aes(shape = as.factor(agg_protected), col =  as.factor(closure_months)))

#heatmap of relative pop size for all combinations of temporal and spatial strats
tiff("plots/mgmt_effect/spat.vs.tmp.sexratio_F0.25.tiff", units="in", width=8, height=4, res=600)
mgmt_output %>% 
  filter(agg_protected %in% 1:5, connectivity == "none") %>% 
ggplot(aes(x = agg_protected, y = as.factor(closure_months))) +
  geom_tile(aes(fill = prop_male_unprotected)) +
  geom_text(aes(label=format(round(prop_male_unprotected, 2), nsmall = 2))) +
  facet_wrap(~sex_cue) +
  theme_classic() +
  scale_fill_gradient(low = "white", high = "red", na.value = NA) +
  labs(x = "Number of aggregations protected", y = "Length of seasonal ban (months)", fill= "Sex ratio", subtitle = "Moderate fishing") +
  theme(text = element_text(size = 12),
        legend.title=element_text(size=12))
dev.off()

#####################################
### Fishing impact base management ##
#####################################

# fishing effect (redistribute) on tot biomass  ------------------------
tot_size = unname(unlist(lapply(f.mort_effect_redistr, `[`, "tot_size")))
prop_male = rowMeans(1 - t(matrix(unname(unlist(lapply(f.mort_effect_redistr, `[`, "prop_female"))), nrow = 5)))
prop_male_unprotected = 1 - t(matrix(unname(unlist(lapply(f.mort_effect_redistr, `[`, "prop_female"))), nrow = 5))[,5]
unprotected_relative.size = t(matrix(unname(unlist(lapply(f.mort_effect_redistr, `[`, "tot_size_agg"))), nrow = 5))[,5]/t(matrix(unname(unlist(lapply(f.mort_effect_redistr, `[`, "tot_size_agg"))), nrow = 5))[,1]
#catch = t(matrix(unname(unlist(lapply(out, `[`, "catch_equil"))), nrow = 5))
tot_catch = unname(unlist(lapply(f.mort_effect_redistr, `[`, "tot_catch_equil")))
cv_catch = unname(unlist(lapply(f.mort_effect_redistr, `[`, "cv_catch_equil")))
cv_catch[is.na(cv_catch)] = 0
mean_age = unname(unlist(lapply(f.mort_effect_redistr, `[`, "mean_age_unprotected")))
max_f= max(unname(unlist(lapply(f.mort_effect_redistr, `[`, "fishing_info"))))

mgmt_output = cbind(mgmt_strat, tot_size,tot_catch, prop_male_unprotected, unprotected_relative.size, mean_age)

tiff("plots/fishing_effect/Palau.mgmt_f.effect.tiff", units="in", width=7, height=6, res=600)
mgmt_output %>% 
  filter(connectivity == "none") %>%
  group_by(sex_cue, phi) %>% 
  mutate(relative_tot_size = tot_size/max(tot_size)) %>%
  ungroup() %>% 
  #group_by(sex_cue) %>% 
  mutate(relative_tot_catch = tot_catch/max(tot_catch)) %>% 
  select(!c(tot_size, tot_catch)) %>% 
  pivot_longer(cols = c(relative_tot_size, prop_male_unprotected, relative_tot_catch, mean_age), names_to = "metric", values_to = "value") %>% 
  mutate(metric = case_when(metric == "relative_tot_size" ~ "Relative total size",
                            metric == "prop_male_unprotected" ~ "Sex ratio",
                            metric == "relative_tot_catch" ~ "Relative total catch",
                            metric == "mean_age" ~ "Mean age"),
         phi = case_when(phi == "6" ~ "sensitive",
                       phi == "40" ~ "robust")) %>% 
  ggplot(aes(x = f.mort, y = value)) +
  geom_vline(xintercept = c(0.1, 0.25), col = "darkgray", linetype = "dotted") +
  geom_line(aes(linetype = as.factor(phi), col = sex_cue), size = 1) +
  facet_wrap(~factor(metric, c("Relative total size", "Relative total catch", "Sex ratio", "Mean age")), scales = "free", ncol = 2) +
  scale_color_manual(values = c("#5d74a5", "#a45851")) +
  theme_classic() +
  theme(text = element_text(size = 12),
        legend.title=element_text(size=12),
        panel.grid.major = element_line(color = "lightgray", size = 0.1),
        panel.grid.minor = element_line(color = "lightgray", size = 0.05)) +
  labs(linetype = "Sensitivity to \nmale proportion", col = "Sex cue", x = "Fishing mortality", y = "")
dev.off()


#####################################
######## seasonal ban impact ########
#####################################

tot_size = unname(unlist(lapply(temp.out, `[`, "tot_size")))
prop_male_unprotected = 1 - t(matrix(unname(unlist(lapply(temp.out, `[`, "prop_female"))), nrow = 5))[,5]
unprotected_relative.size = t(matrix(unname(unlist(lapply(temp.out, `[`, "tot_size_agg"))), nrow = 5))[,5]/t(matrix(unname(unlist(lapply(temp.out, `[`, "tot_size_agg"))), nrow = 5))[,1]
#catch = t(matrix(unname(unlist(lapply(out, `[`, "catch_equil"))), nrow = 5))
tot_catch = unname(unlist(lapply(temp.out, `[`, "tot_catch_equil")))
mean_age = unname(unlist(lapply(temp.out, `[`, "mean_age_unprotected")))
max_f= max(unname(unlist(lapply(temp.out, `[`, "fishing_info"))))
rel_catch = tot_catch/tot_size

unfished_size = unname(unlist(lapply(unfished.temp.out, `[`, "tot_size")))
unifished_output = cbind(unfished_mgmt_strat, unfished_size)

mgmt_output = cbind(mgmt_strat, unprotected_relative.size, tot_size, prop_male_unprotected, tot_catch,mean_age, rel_catch)

mgmt_output_full= left_join(mgmt_output, unifished_output)


tiff("plots/temp_mgmt_effect/catch_2agg_phi6.tiff", units="in", width=6.5, height=5, res=600)
mgmt_output %>% 
  filter(agg_protected == 2, phi == 6) %>% 
  ggplot(aes(x = closure_months, y = rel_catch)) +
  geom_line(aes(col = as.factor(f.mort)), linewidth = 1) +
  facet_wrap(~sex_cue) +
  scale_color_manual(values = c("#ed8b00", "#dd4124"), labels = c("Less", "Moderate")) +
  scale_x_continuous(breaks = seq(0, 9, by=1)) +
  theme_classic() +
  theme(text = element_text(size = 16),
        panel.grid.major = element_line(color = "lightgray", size = 0.1),
        panel.grid.minor = element_line(color = "lightgray", size = 0.05)) +
  labs(x = "Length of seasonal ban (months)", y = "Relative long-term catch", col = 
         "Fishing intensity")
dev.off()

tiff("plots/temp_mgmt_effect/N_2agg_phi6.tiff", units="in", width=6.5, height=5, res=600)
mgmt_output_full %>% 
  filter(agg_protected == 2, phi == 6) %>% 
  ggplot(aes(x = closure_months, y = tot_size/unfished_size)) +
  geom_line(aes(col = as.factor(f.mort)), linewidth = 1) +
  facet_wrap(~sex_cue) +
  scale_color_manual(values = c("#ed8b00", "#dd4124"), labels = c("Less", "Moderate")) +
  scale_x_continuous(breaks = seq(0, 9, by=1)) +
  theme_classic() +
  theme(text = element_text(size = 16),
        panel.grid.major = element_line(color = "lightgray", size = 0.1),
        panel.grid.minor = element_line(color = "lightgray", size = 0.05)) +
  labs(x = "Length of seasonal ban (months)", y = "Population size relative to unfished", col = 
         "Fishing intensity")
dev.off()

tiff("plots/temp_mgmt_effect/sexratio_2agg_phi6.tiff", units="in", width=6.5, height=5, res=600)
mgmt_output %>% 
  filter(agg_protected == 2, phi == 6) %>% 
  ggplot(aes(x = closure_months, y = prop_male_unprotected)) +
  geom_line(aes(col = as.factor(f.mort)), linewidth = 1) +
  facet_wrap(~sex_cue) +
  scale_color_manual(values = c("#ed8b00", "#dd4124"), labels = c("Less", "Moderate")) +
  scale_x_continuous(breaks = seq(0, 9, by=1)) +
  theme_classic() +
  theme(text = element_text(size = 16),
        panel.grid.major = element_line(color = "lightgray", size = 0.1),
        panel.grid.minor = element_line(color = "lightgray", size = 0.05)) +
  labs(x = "Length of seasonal ban (months)", y = "Sex ratio", col = 
         "Fishing intensity")
dev.off()
  




##############################################
########## Make conceptual plots #############
##############################################
rho = 8
a_50 = 5
p_fixed = 1 / (1 + exp(-rho*(seq(3,11, length.out = 50) - a_50)))

rho_freq = 10
freq_50 = 0.67
p_freq = 1 / (1 + exp(-rho_freq*(seq(0,1, length.out = 50) - freq_50)))

colors <- c("Fixed" = "#5d74a5", "Frequency" = "#a45851")

tiff("plots/concep_figs/sex_transition.tiff", units="in", width=4, height=4, res=600)
ggplot() +
  scale_color_manual(values = colors) +
  geom_line(aes(x = seq(0,1, length.out = 50)*12, y = p_freq, color = "Frequency"), linewidth = 1) +
  geom_line(aes(x = seq(3,11, length.out = 50), y = p_fixed, color = "Fixed"), inherit.aes = FALSE, linewidth = 1) +
  theme_classic()  +
  theme(panel.grid.major = element_line(color = "lightgray", size = 0.1),
        panel.grid.minor = element_line(color = "lightgray", size = 0.05),
        text = element_text(size = 12),
        legend.position="bottom",
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.margin=margin(-10, 0, 0, 0)) +
  labs(x = "Age", y = expression(italic('p'['a'])), color = "Sex Transition") +
  scale_x_continuous(name = 'Age', sec.axis = sec_axis(~. /12,name = "Frequency"))
dev.off()



f.max = 1
phi6 = f.max*(1-exp(-6*seq(0,1,length.out=50)))
phi40 = f.max*(1-exp(-40*seq(0,1,length.out=50)))

linetype <- c("Sensitive" = "dashed", "Robust" = "solid")

tiff("plots/concep_figs/matingfunction.tiff", units="in", width=4, height=4, res=600)
ggplot() +
  scale_linetype_manual(values = linetype) +
  geom_line(aes(x = seq(0,1,length.out = 50), phi6, linetype = "Sensitive"), size = 1) +
  geom_line(aes(x = seq(0,1,length.out = 50), phi40, linetype = "Robust"), size = 1) +
  theme_classic() +
  theme(panel.grid.major = element_line(color = "lightgray", size = 0.1),
        panel.grid.minor = element_line(color = "lightgray", size = 0.05),
        text = element_text(size = 12),
        legend.key.width= unit(1, 'cm'),
        legend.position="bottom",
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.margin=margin(-10, 0, 0, 0)) +
  labs(x = "Proportion of adults that are male", y = expression(italic(psi['i,t'])), linetype = "Sensitivity")
dev.off()

c = 4
f.frac = 0.9
prop.f.sa = exp(-((1:12 - 6.5)^2)/(2*(c*f.frac)^2))
prop.m.sa = exp(-((1:12 - 6.5)^2)/(2*c^2))  

color <- c("Males" = "darkblue", "Females" = "hotpink3")
tiff("plots/concep_figs/seasonal.migration.tiff", units="in", width=4, height=3.6, res=600)
ggplot() +
  scale_color_manual(values = color) +
  geom_line(aes(x = 1:12, y = prop.f.sa, color = "Females"), size = 1) +
  geom_line(aes(x = 1:12, prop.m.sa, color = "Males"), size = 1) +
  theme_classic() +
  theme(panel.grid.major = element_line(color = "lightgray", size = 0.1),
        panel.grid.minor = element_line(color = "lightgray", size = 0.05),
        text = element_text(size = 12),
        legend.position="bottom",
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.margin=margin(-10, 0, 0, 0)) +
  labs(x = "Month", y = expression(italic('c'['s,t'])), 
       color = "Sex") +
  scale_x_continuous(breaks = 1:12)
dev.off()



############################
##### Catch timeseries #####
############################
tf = nrow(f.mort_effect_redistr[[13]]$catch)
low_f = rowSums(f.mort_effect_redistr[[13]]$catch[(tf-12*12):(tf-1),])   # no connectivity, fixed sex cue, phi = 6, f = 0.13
high_f = rowSums(f.mort_effect_redistr[[25]]$catch[(tf-12*12):(tf-1),]) # no connectivity, fixed sex cue, phi = 6, f = 0.51
plot(high_f, type = "l", lwd = 2, xlab = "Month", ylab = "", main = "Sensitive to sperm limitation", ylim = c(0, 250000), yaxt='n')
title(ylab="Catch", line=0, cex.lab=1.2)
lines(low_f, col = "gray50", lwd = 2, lty = 2)
legend("topright", col = c("black", "gray50"), lty = c(1,2), legend = c("f = 0.25", "f = 0.1"))


low_f = rowSums(f.mort_effect_redistr[[73]]$catch[(tf-12*12):(tf-1),])   # no connectivity, fixed sex cue, phi = 40, f = 0.13
high_f = rowSums(f.mort_effect_redistr[[85]]$catch[(tf-12*12):(tf-1),]) # no connectivity, fixed sex cue, phi = 40, f = 0.51
plot(low_f, type = "l", col = "gray50", lwd = 2, lty = 2, xlab = "Month", ylab = "", main = "Robust to sperm limitation", yaxt='n', ylim = c(0, 600000))
title(ylab="Catch", line=0, cex.lab=1.2)
lines(high_f, lwd = 2)
legend("topright", col = c("black", "gray50"), lty = c(1,2), legend = c("f = 0.25", "f = 0.1"))


#############################################
############# Connectivity #################
#############################################
tot_size = unname(unlist(lapply(f.mort_effect_redistr, `[`, "tot_size")))
prop_male = rowMeans(1 - t(matrix(unname(unlist(lapply(f.mort_effect_redistr, `[`, "prop_female"))), nrow = 5)))
prop_male_unprotected = 1 - t(matrix(unname(unlist(lapply(f.mort_effect_redistr, `[`, "prop_female"))), nrow = 5))[,5]
unprotected_relative.size = t(matrix(unname(unlist(lapply(f.mort_effect_redistr, `[`, "tot_size_agg"))), nrow = 5))[,5]/t(matrix(unname(unlist(lapply(f.mort_effect_redistr, `[`, "tot_size_agg"))), nrow = 5))[,1]
#catch = t(matrix(unname(unlist(lapply(out, `[`, "catch_equil"))), nrow = 5))
tot_catch = unname(unlist(lapply(f.mort_effect_redistr, `[`, "tot_catch_equil")))
cv_catch = unname(unlist(lapply(f.mort_effect_redistr, `[`, "cv_catch_equil")))
cv_catch[is.na(cv_catch)] = 0
mean_age = unname(unlist(lapply(f.mort_effect_redistr, `[`, "mean_age_unprotected")))
max_f= max(unname(unlist(lapply(f.mort_effect_redistr, `[`, "fishing_info"))))


mgmt_output = cbind(mgmt_strat, tot_size,tot_catch, prop_male_unprotected, unprotected_relative.size, mean_age)

tiff("plots/fishing_effect/connectivity.tiff", units="in", width=6, height=5, res=600)
mgmt_output %>% 
  select(connectivity, f.mort, tot_size, tot_catch, prop_male_unprotected, unprotected_relative.size) %>% 
  mutate(rel.tot_catch = tot_catch/max(tot_catch)) %>% 
  group_by(connectivity) %>% 
  mutate(rel.tot_size = tot_size/max(tot_size)) %>% 
  select(!tot_size, tot_catch) %>% 
  pivot_longer(cols = c(rel.tot_size, prop_male_unprotected, rel.tot_catch, unprotected_relative.size), names_to = "metric", values_to = "value") %>% 
  mutate(metric = case_when(metric == "rel.tot_size" ~ "Total size",
                            metric == "prop_male_unprotected" ~ "Sex ratio",
                            metric == "rel.tot_catch" ~ "Total catch",
                            metric == "unprotected_relative.size" ~ "Relative size")) %>% 
  ggplot(aes(x = f.mort, y = value)) +
  geom_vline(xintercept = c(0.1, 0.25), col = "darkgray", linetype = "dotted") +
  geom_line(aes(linetype = connectivity), size = 1) +
  facet_wrap(~factor(metric, c("Total size", "Total catch", "Sex ratio", "Relative size")), scales = "free", ncol = 2) +
  theme_classic() +
  theme(text = element_text(size = 10),
        legend.title=element_text(size=10),
        panel.grid.major = element_line(color = "lightgray", size = 0.1),
        panel.grid.minor = element_line(color = "lightgray", size = 0.05)) +
  labs(linetype = "Connectivity", x = "Fishing mortality", y = "") +
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("none", "connected"))
dev.off()



