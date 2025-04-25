library(parallel)
library(tidyverse)

# phi (fertilization steepness) scenarios = 6 (sensitive to males), 40 (not sensitive)

#############################################
####### Spatial and temporal outcomes #######
#############################################
connectivity = c("none", "distance") # "none"  "even"  "distance"
sex_cue = c("fixed", "frequency")   # "fixed"  "frequency"
n_agg_protected = 0:5
n_closure_months = 0:8
mgmt_strat = expand.grid(connectivity, sex_cue, n_agg_protected,n_closure_months)
colnames(mgmt_strat) = c("connectivity", "sex_cue", "agg_protected", "closure_months")

unfished.out = mcmapply(get_pop_n, 
                             sex_cue = sex_cue,
                             MoreArgs = list(n_agg_protected = 5, agg = 5, c = 3, freq_50 = 0.67, connectivity ="none",
                                             rho_freq = 10, f_redistribute = "yes", allee = 1, b = 0.01, phi = 40),
                             SIMPLIFY = FALSE, mc.cores = parallel::detectCores()-1)

out = mcmapply(get_pop_n, 
               connectivity = mgmt_strat$connectivity,
               sex_cue = mgmt_strat$sex_cue,
               n_agg_protected = mgmt_strat$agg_protected, 
               n_closure_months = mgmt_strat$closure_months, 
               MoreArgs = list(f.mort = 0.25, agg = 5, c = 3, freq_50 = 0.67, 
                               rho_freq = 10, f_redistribute = "yes", allee = 1, phi = 40,
                               b = 0.01),
               SIMPLIFY = FALSE, mc.cores = parallel::detectCores()-1)




#############################################
####### Fishing w/ base protections #########
#############################################
connectivity = c("none", "distance") # "none"  "even"  "distance"
sex_cue = c("fixed", "frequency")  # "fixed"  "frequency"
n_agg_protected = 2
n_closure_months = 7
f.mort = seq(0,0.6, length.out = 15)
phi = c(6, 40)
mgmt_strat = expand.grid(connectivity, sex_cue, f.mort, phi)
colnames(mgmt_strat) = c("connectivity", "sex_cue", "f.mort", "phi")

f.mort_effect_redistr = mcmapply(get_pop_n, 
                         f.mort = mgmt_strat$f.mort, 
                         connectivity = mgmt_strat$connectivity, 
                         sex_cue = mgmt_strat$sex_cue,
                         phi = mgmt_strat$phi,
                         MoreArgs = list(n_agg_protected = n_agg_protected, 
                                         n_closure_months = n_closure_months, 
                                         agg = 5, c = 3, freq_50 = 0.67, rho_freq = 10, 
                                         eggs.percap = 1500, f_redistribute = "yes", 
                                         allee = 1, b = 0.01, n.years = 100),
                         SIMPLIFY = FALSE, mc.cores = parallel::detectCores()-1)

f.mort_effect_noredistr = mcmapply(get_pop_n, 
                                 f.mort = mgmt_strat$f.mort, 
                                 connectivity = mgmt_strat$connectivity, 
                                 sex_cue = mgmt_strat$sex_cue,
                                 MoreArgs = list(n_agg_protected = n_agg_protected, n_closure_months = n_closure_months, 
                                                 agg = 5, c = 3,
                                                 freq_50 = 0.67, rho_freq = 10, f_redistribute = "no", allee = 5, phi = 3),
                                 SIMPLIFY = FALSE, mc.cores = parallel::detectCores()-1)



#############################################
########### Temporal outcomes ###############
#############################################
# protect 1 - 2 aggregations fully
# explore range of temporal closures 
# explore less and moderate fishing
# explore sex cues 

n_agg_protected = c(1, 2)
sex_cue = c("fixed", "frequency")  # "fixed"  "frequency"
f.mort = c(0.1, 0.25) 
n_closure_months = 0:9
phi = c(6, 40)
mgmt_strat = expand.grid(sex_cue, n_agg_protected, f.mort, n_closure_months, phi)
colnames(mgmt_strat) = c("sex_cue", "agg_protected", "f.mort", "closure_months", "phi")

unfished_mgmt_strat = expand.grid(sex_cue, phi)
colnames(unfished_mgmt_strat) = c("sex_cue", "phi")

unfished.temp.out = mcmapply(get_pop_n, 
                    sex_cue = unfished_mgmt_strat$sex_cue,
                    phi = unfished_mgmt_strat$phi,
                    MoreArgs = list(n_agg_protected = 5, agg = 5, c = 3, freq_50 = 0.67, connectivity ="none",
                                    rho_freq = 10, f_redistribute = "yes", allee = 1, b = 0.1),
                    SIMPLIFY = FALSE, mc.cores = parallel::detectCores()-1)

temp.out = mcmapply(get_pop_n, 
               sex_cue = mgmt_strat$sex_cue,
               n_agg_protected = mgmt_strat$agg_protected, 
               n_closure_months = mgmt_strat$closure_months,
               f.mort = mgmt_strat$f.mort,
               phi = mgmt_strat$phi,
               MoreArgs = list(agg = 5, c = 3, freq_50 = 0.67, connectivity ="none",
                               rho_freq = 10, f_redistribute = "yes", allee = 1, b = 0.1),
               SIMPLIFY = FALSE, mc.cores = parallel::detectCores()-1)




#############################################
############## Fishing only #################
#############################################
connectivity = c("none", "distance") # "none"  "even"  "distance"
sex_cue = c("fixed", "frequency")  # "fixed"  "frequency"
n_agg_protected = 0
n_closure_months = 0
f.mort = seq(0,0.6, length.out = 15)
phi = c(6, 40)
mgmt_strat = expand.grid(connectivity, sex_cue, f.mort, phi)
colnames(mgmt_strat) = c("connectivity", "sex_cue", "f.mort", "phi")

f.mort_effect_redistr = mcmapply(get_pop_n, 
                                 f.mort = mgmt_strat$f.mort, 
                                 connectivity = mgmt_strat$connectivity, 
                                 sex_cue = mgmt_strat$sex_cue,
                                 phi = mgmt_strat$phi,
                                 MoreArgs = list(n_agg_protected = n_agg_protected, 
                                                 n_closure_months = n_closure_months, 
                                                 agg = 5, c = 3, freq_50 = 0.67, rho_freq = 10, 
                                                 eggs.percap = 1500, f_redistribute = "yes", 
                                                 allee = 1, b = 0.01, n.years = 100),
                                 SIMPLIFY = FALSE, mc.cores = parallel::detectCores()-1)


# Calculate the moving average using filter()
moving_average <- stats::filter(rowSums(f.mort_effect_redistr[[93]]$catch), rep(1 / n, n), sides = 2)
# Print the result
print(moving_average)


#############################################
############## Connectivity #################
#############################################
connectivity = c("none", "distance") # "none"  "even"  "distance"
sex_cue = c("fixed")  # "fixed"  "frequency"
n_agg_protected = 2
n_closure_months = 7
f.mort = seq(0,0.6, length.out = 15)
mgmt_strat = expand.grid(connectivity, sex_cue, f.mort, phi)
colnames(mgmt_strat) = c("connectivity", "sex_cue", "f.mort")

f.mort_effect_redistr = mcmapply(get_pop_n, 
                                 f.mort = mgmt_strat$f.mort, 
                                 connectivity = mgmt_strat$connectivity, 
                                 sex_cue = mgmt_strat$sex_cue,
                                 MoreArgs = list(n_agg_protected = n_agg_protected, 
                                                 n_closure_months = n_closure_months, 
                                                 agg = 5, c = 3, freq_50 = 0.67, rho_freq = 10, 
                                                 eggs.percap = 1500, f_redistribute = "yes", 
                                                 allee = 1, b = 0.01, n.years = 100, phi = 40),
                                 SIMPLIFY = FALSE, mc.cores = parallel::detectCores()-1)



