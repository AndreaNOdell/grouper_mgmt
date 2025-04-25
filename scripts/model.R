# Load packages

# Set up parameters ------------------------------------------------

n.years = 100 # number of years
tf = 12*n.years + 1 # timesteps
agg = 5 # number of aggregations

# natural mortality
nat.mort = 0.4 / 12 # natural mortality

# fishing mortality
q = 30 # catch per day
effort = 5 # days per month
k = 1500

# log(K/(K-q*effort))

# sex-transition
rho = 0.8 # transition steepness
a_50 = 8 # age at 50% transition

# growth
linf = 65.6 # https://www.seaaroundus.org/data/#/taxa/606082
k = 0.14 # https://www.seaaroundus.org/data/#/taxa/606082
a.weight = 0.01148 # https://www.fishbase.se/summary/Plectropomus-areolatus.html
b.weight = 3.04 # https://www.fishbase.se/summary/Plectropomus-areolatus.html

# fertility
f.max = 0.8 # maximum fertilization success (asymptote)
phi = 10 # fertility steepness (5-10)
eggs.percap = 1500 # max eggs per capita in an unfished

allee = 1 # allee effect in broadcast spawning (1 = no effect, >1 = allee)
b = 0.7 # steepness parameter 




get_pop_n = function(
    n.years = 100, agg = 5, nat.mort = 0.0416, f.mort = 0.1, rho = 0.8, a_50 = 5, 
    f.max = 1, phi = 10, eggs.percap = 1500,  allee = 1, b = 0.01, n.start = 1000, 
    n_closure_months = 6, n_agg_protected = 1, connectivity = "none", sex_cue = "fixed", 
    rho_freq = 10, freq_50 = 0.67, c = 3, f.frac = 0.9, f_redistribute = "yes"){

    
# create reproduction function ----------------------------
  repro = function(m = m, n =  N[,m,], phi = phi){ # calculate juveniles (eggs successfully fertilized and survived)
    n.fem = n[3:12,]  # sb.fem = a.weight*(linf * (1 - exp(-k * c(3:12))))^b.weight * n[3:12]
    m.prop = colSums(n[13:nrow(n),])/colSums(n[3:nrow(n),])
    fert.success = f.max*(1-exp(-phi*m.prop))
    ((eggs.percap*(colSums(prop.f.sa[m] * n.fem))^(allee-1)) / (1 + b * colSums(prop.f.sa[m] * n.fem)^allee )) * colSums(prop.f.sa[m] * n.fem) * fert.success
  }
  
# assign age classes -----------------------------------------
  juveniles = 1:2
  females = 3:12
  males = 13:21
  n.age.j = length(juveniles)
  n.age.f = length(females)
  n.age.m = length(males)
  
  len.at.age = 454.8*(1-exp(-0.635*(1:12-(-0.309))))
  wt.at.age.kg = (0.0119*(len.at.age/10)^3.057)/1000
  wt.at.age.kg.vec = c(wt.at.age.kg, wt.at.age.kg[4:12])
  
# fertility ---------------------------------------------------
  prop.f.sa = exp(-((1:12 - 6.5)^2)/(2*(c*f.frac)^2))
  prop.j.sa = rep(0, 12)
  prop.m.sa = exp(-((1:12 - 6.5)^2)/(2*c^2))
  
  #plot(0:12, prop.f.sa, type = "l", col = "red", ylim = c(0,1))
  #lines(0:12, prop.m.sa, col = "blue")
  
# Create array with dimensions --------------------------------------
    # nrow = n.age.classes, ncol = tf, and n.set = n.aggregations 
    # rhodes et al. journal of fish biology
tf = 12*n.years + 1 # timesteps  
  
N = array(0, dim = c(n.age.j+n.age.f+n.age.m, tf, agg))

catch = matrix(0, tf, agg)
recruitment = matrix(0, tf, agg)
fishing_info = c()

# Specify starting population size
N[,1,] = rep(n.start, length(N[,1,]))


# connectivity matrix ------------------------
if(connectivity == "none"){
c_mat = matrix(0, agg, agg)
diag(c_mat) <- 1 
} else if(connectivity == "even"){
  c_mat = matrix(1/agg, agg, agg)
} else if(connectivity == "distance"){
  c_mat = matrix(0, agg, agg)
  
  #c_mat[,1] = c(0.7, 0.3, 0, 0, 0)
  #c_mat[,2] = c(0.4, 0.6, 0, 0, 0)
  #c_mat[,3] = c(0, 0, 0.7, 0.3, 0)
  #c_mat[,4] = c(0, 0, 0, 0.8, 0.2)
  #c_mat[,5] = c(0, 0, 0, 0, 1)

  c_mat[,1] = c(0.7, 0, 0, 0, 0.3)
  c_mat[,5] = c(0.2, 0, 0, 0, 0.8)
  c_mat[,3] = c(0, 0, 0.7, 0.3, 0)
  c_mat[,4] = c(0, 0, 0.2, 0.8, 0)
  c_mat[,2] = c(0, 1, 0, 0, 0)
}

# fishing mortality in and outside spawning aggregation in open aggregation --------------------
if(f_redistribute == "yes") {
  if(n_agg_protected == 5){
    fish.mort.sp = 0
  } else {
fish.mort.sp = f.mort/(1-(n_agg_protected/agg))
}
} else if(f_redistribute == "no") {
  fish.mort.sp = f.mort
}

# qE = (1-exp(-f))*K, so f = log(k/(k-qE)) where k is popsize when qE = 30*5

# fishing mortality on non-spawning individuals that venture out of the protected aggregation

# create a vector of 0 and 1's to indicate fishing season -----------------------------
fishing_months = rep(1, 12 - n_closure_months)
fishing_months = split(fishing_months, cut(seq_along(fishing_months), 2, labels = FALSE))
f_season = c(fishing_months$`2`, rep(0, n_closure_months), fishing_months$`1`)

# create fishing season matrix for each aggregation ----------------------------------
protected = c(rep(0, n_agg_protected), rep(1, agg-n_agg_protected)) 


# monthly time step experiencing natural and fishing mortality, and reproduction -----------------
for(m in 1:(tf-1)){
  # 1 year olds experience natural mortality and recruitment
  month = ifelse(m %% 12 == 0, 12, m %% 12)
  n_month = N[,m,]
  N[1,m+1,] = N[1,m,] * exp(-nat.mort) + repro(m = month, n = n_month, phi = phi)
  
  # sex cue
  if(sex_cue == "fixed"){
    p = 1 / (1 + exp(-rho*(replicate(agg, 3:11) - a_50)))
  } else if(sex_cue == "frequency"){
    p = 1 / (1 + exp(-rho_freq*(round(sweep(apply(N[3:11,m,] + rbind(rep(0,agg), N[13:20,m,]), 2, cumsum), 2, colSums(N[3:21,m,]), FUN = "/"), 2) - freq_50)))
  }
  
  # create matrix of proportion in spawning agg for each age at month t
  c_st = matrix(rep(c(prop.j.sa[month], rep(prop.f.sa[month], n.age.f), rep(prop.m.sa[month], n.age.m)), agg), ncol = agg)
  
  # everyone else experiences natural mortality and month-specific fishing mortality 
  N[2:nrow(N),m+1,] = N[2:nrow(N),m,] * exp(-nat.mort - (fish.mort.sp*c_st*f_season[month])%*%diag(protected))
  
  catch[m,] = colSums((N[2:nrow(N),m,] * (1 - exp(-(fish.mort.sp*c_st*f_season[month])%*%diag(protected))))*c(1, wt.at.age.kg.vec[3:length(wt.at.age.kg.vec)])) 
  
  fishing_info[m] = fish.mort.sp

  # Annual maturation, sex-transition, and age increase
  if(m%%12 == 0){
    # males move up an age class and females transition to males
    #N[nrow(N),m+1,] = N[nrow(N),m+1,] + 
    N[14:nrow(N),m+1,] = N[13:(nrow(N)-1),m+1,] + N[4:11,m+1,] * p[2:nrow(p),] 
    N[13,m+1,] = N[3,m+1,]*p[1,] # age 3 females that have transitioned to age 4 males
    
    # females move up an age class or transition to male
    N[4:12,m+1,] = N[3:11,m+1,]*(1-p) 
    
    # Age 2 juveniles mature to age 3 females and choose a spawning ground
    N[3,m+1,] = rowSums(N[2,m+1,]*c_mat) 
    
    # increase juveniles age 1 to 2
    N[2,m+1,] = N[1,m+1,]
    
    # add new juveniles
    N[1,m+1,] = repro(m = month, n = n_month, phi = phi)  # change to recruitment function
  }
  
  recruitment[m,] = repro(m = month, n = n_month, phi = phi)
  
}

annual_N = N[,seq(11,tf, 12),]
unprotected_annual_N = N[ , seq(11,tf, 12), 5]

if(agg == 1) {
  juv_equil_N = colMeans(colSums(N[juveniles,(ncol(N)-12*12):(ncol(N)-1), ] * wt.at.age.kg.vec[juveniles]))  # colSums(N[juveniles,seq(11,tf, 12),]*wt.at.age.kg.vec[juveniles])[n.years]
  male_equil_N = colMeans(colSums(N[males,(ncol(N)-12*12):(ncol(N)-1), ] * wt.at.age.kg.vec[males])) # colSums(N[males,seq(11,tf, 12),]*wt.at.age.kg.vec[males])[n.years]
  female_equil_N = colMeans(colSums(N[females,(ncol(N)-12*12):(ncol(N)-1), ] * wt.at.age.kg.vec[females])) # colSums(N[females,seq(11,tf, 12),]*wt.at.age.kg.vec[females])[n.years]
  } else if(agg > 1) {
    juv_equil_N = colMeans(colSums(N[juveniles,(ncol(N)-12*12):(ncol(N)-1), ] * wt.at.age.kg.vec[juveniles])) # colSums(N[juveniles,seq(11,tf, 12),]*wt.at.age.kg.vec[juveniles])[n.years,]
    male_equil_N = colMeans(colSums(N[males,(ncol(N)-12*12):(ncol(N)-1), ] * wt.at.age.kg.vec[males])) # colSums(N[males,seq(11,tf, 12),]*wt.at.age.kg.vec[males])[n.years,]
    female_equil_N = colMeans(colSums(N[females,(ncol(N)-12*12):(ncol(N)-1), ] * wt.at.age.kg.vec[females]))
  }

prop_female = female_equil_N/(male_equil_N+female_equil_N)
tot_size_agg = juv_equil_N + female_equil_N + male_equil_N
tot_adults_agg = female_equil_N + male_equil_N
tot_size = sum(female_equil_N) + sum(male_equil_N) + sum(juv_equil_N)
mean_catch_agg = colMeans(catch[(nrow(catch)-12*12):(nrow(catch)-1),])
tot_catch_equil = mean(rowSums(rowsum(catch[(nrow(catch)-12*12):(nrow(catch)-1),], rep(1:12, each = 12))))
cv_catch_equil = sd(rowSums(rowsum(catch[(nrow(catch)-12*12):(nrow(catch)-1),], rep(1:12, each = 12))))/mean(rowSums(rowsum(catch[(nrow(catch)-12*12):(nrow(catch)-1),], rep(1:12, each = 12))))
mean_age_unprotected = (sum(rowMeans(N[females, (tf-12*12):(tf-1), 5]) * 3:12) / sum(rowMeans(N[females, (tf-12*12):(tf-1), 5])) + sum(rowMeans(N[males, (tf-12*12):(tf-1), 5]) * 4:12) / sum(rowMeans(N[males, (tf-12*12):(tf-1), 5])))/2 # N[females, tf, 5]
mean_age_protected = (sum(N[females, tf, 1] * 3:12) / sum(N[females, tf, 1]) + sum(N[males, tf, 1] * 4:12) / sum(N[males, tf, 1]))/2

# Send output to global environment
output = list(male_equil_N, female_equil_N, annual_N, prop_female, f_season, tot_size_agg, tot_size, mean_catch_agg, tot_catch_equil, cv_catch_equil, unprotected_annual_N, mean_age_unprotected, recruitment, fishing_info, catch, tot_adults_agg, mean_age_protected)
names(output) = c("male_equil_N", "female_equil_N", " annual_N", "prop_female", "f_season", "tot_size_agg", "tot_size", "mean_catch_agg", "tot_catch_equil", "cv_catch_equil", "unprotected_annual_N", "mean_age_unprotected", "recruitment", "fishing_info", "catch", "tot_adults_agg", "mean_age_protected")
return(output) # sends as list

}
