
# This script generates Figure 4 in manuscript

# Set work directory ------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
getwd()


# Load packages -----------------------------------------------------------

library(deSolve)
library(ggplot2)
library(tictoc) 
library(Cairo) 


# source function files ---------------------------------------------------

source('decay_model_functions.R')


# Set Parameter values --------------------------------------------------------

#create function for the parameters
parameter_fun <-function(coverage_level, treatment.interval, treatment.decay){ 
  parameters <- c(E_max = 3500,    # maximum eggs laid per ovipositing female
                  t_o = 24,        # day, oviposition period
                  tau_o = 6,       # day, delay in oviposition 
                  k_E = 1/91,      # /day, egg-to-larva development rate 
                  mu_E = 1/10,     # /day, egg mortality rate 
                  TFR = 36.9/100,  # treatment fecundity reduction
                  mu_LQ = 0.05,    # /day, questing larva mortality 
                  alpha_L = 1/12,  # /day, larval questing rate
                  mu_LF = 1/175,   # /day, larva mortality rate during feeding
                  p_LT = 0.7,      # probability of larva coming into successful contact with the conidia
                  d_L = 5,         # day,feeding duration of larva
                  p_LI = 0.118656, # probability of larva becoming infected when feeding on an acutely infectious host
                  p_LC = 0.023,    # probability of larva becoming infected when feeding on an infectious carrier host
                  tau_L = 1.9,     # increase in feeding duration due to treatment effect 
                  mu_LD = 0.177388, # /day, mortality of developing larvae 
                  mu_LDF = 0.177388*9.886674, # Weibull #is the  additional mortality due to fungal effect 
                  k_L = 1/31,      # /day, larva-to-nymph development rate 
                  mu_NQ = 0.03,    # /day, questing nymph mortality
                  alpha_N = 1/20,  # /day, nymph questing rate
                  mu_NF = 1/270,   # /day, nymph mortality rate during feeding 
                  p_NT = 0.8,      # probability of nymph coming into successful contact with the conidia
                  p_NI = 0.118656, # probability of nymph becoming infected when feeding on an acutely infectious host
                  p_NC = 0.023,    # probability of nymph becoming infected when feeding on an infectious carrier host
                  d_N = 6,         # day,feeding duration of nymph
                  tau_N = 2.3,     # increase in feeding duration due to fungal treatment effect 
                  mu_ND = 0.065,   # /day, developing nymph mortality
                  mu_NDF = 0.065*9.886674, # Weibull # is the  additional mortality due to fungal effect 
                  k_N = 1/45,      # /day, nymph-to-adult development rate
                  mu_AQ = 0.01,    # /day, questing adult mortality
                  alpha_A = 1/28,  # /day, adult questing rate
                  mu_AF = 1/400,   # /day, Adult mortality rate during feeding
                  p_AT = 0.9,      # probability of adult coming into successful contact with the conidia
                  d_A = 8,         # day,feeding duration of adult
                  tau_A = 3,       # increase in feeding duration due to treatment effect 
                  mu_AD = 0.02,    # /day, developing adult mortality
                  mu_ADF = 0.02*9.886674, # Weibull # is the  additional mortality due to fungal effect 
                  k_o = 6,         # day, preoviposition period
                  k_tau = 2.3,     # day, increase in preoviposition period due to fungal effect 
                  zeta = 0.5,      # sex ratio
                  mu_AO = 0.02,    # /day, ovipositing adult mortality
                  mu_AOF = 0.02*9.886674, # Weibull # is the  additional mortality due to fungal effect  
                  p_HN = 0.09,     # Probability nymphal tick infects susceptible host
                  p_HA = 0.9,      # Probability adult tick infects susceptible host
                  mu_H = 0.0006859604, # Natural host mortality
                  mu_I = 0.25*0.014,   # /day, Mortality due to East Coast fever 
                  sigma = 1/15,        # /day, rate of host recovery from disease  
                  delta = treatment.decay, #/day, rate of clearance/decay of fungal conidial spores from treated cattle skin 
                  phi = treatment.interval,
                  coverage = coverage_level,
                  k_T = 5073450*7)  # environment's carrying capacity for the tick population (N_T)  
  
  return(parameters)
}


# Initial states ----------------------------------------------------------

H_US.init=26450.13  
H_TS.init=0 
H_UI.init=63.09236   
H_TI.init=0
H_UC.init=6131.778   
H_TC.init=0
E_US.init=4295116 
E_TS.init=0
LQ_US.init=353993.1   
LF_US.init=143400   
LD_US.init=136179.6   
LD_TS.init=0
LD_UI.init=622.3754  
LD_TI.init=0
NQ_US.init=54911.13  
NQ_UI.init=250.9578   
NF_US.init=16115.22    
NF_UI.init=73.65067  
ND_US.init=30653.33   
ND_TS.init=0
ND_UI.init=280.8273    
ND_TI.init=0 
AQ_US.init=14900.92  
AQ_UI.init=136.5133  
AF_US.init=4173.928   
AF_UI.init=38.23901  
AD_U.init=2820.648   
AD_T.init=0
AO_U.init=3811.686   
AO_T.init=0


initial.state <- c(H_US=H_US.init, H_TS=H_TS.init, H_UI=H_UI.init,
                   H_TI=H_TI.init, H_UC=H_UC.init, H_TC=H_TC.init,
                   E_US=E_US.init, E_TS=E_TS.init, LQ_US=LQ_US.init, 
                   LF_US=LF_US.init, LD_US=LD_US.init, LD_TS=LD_TS.init, 
                   LD_UI=LD_UI.init, LD_TI=LD_TI.init, NQ_US=NQ_US.init, 
                   NQ_UI =NQ_UI.init, NF_US=NF_US.init, NF_UI=NF_UI.init,
                   ND_US=ND_US.init, ND_TS=ND_TS.init, ND_UI=ND_UI.init, 
                   ND_TI=ND_TI.init, AQ_US=AQ_US.init, AQ_UI=AQ_UI.init, 
                   AF_US=AF_US.init, AF_UI=AF_UI.init, AD_U=AD_U.init, 
                   AD_T=AD_T.init, AO_U=AO_U.init, AO_T=AO_T.init)


# Time span and time step for simulation ----------------------------------

time <- seq(from = 0, to = 365, by = 0.01) # in days


# Define a vectors of coverage level, treatment interval & decay r --------

coverage <- seq(from = 0, to = 1, by = 0.05) # Vector of treatment coverage

phi <- c(7, 14, 21, 28)  # Vector of treatment interval in days: Weekly, biweekly, triweekly, and monthly treatment frequencies (in days)

delta <- c(1/1, 1/3, 1/5, 1/7, 1/10) # Vector of treatment decay in /day 


# Create an empty data frame with the necessary structure/columns ---------

results.treatment.df <- data.frame(time=double(), # duration of simulation
                                   incidence_in_cattle=double(), #refers to the number of new cases of ECF in cattle population over a defined (simulation) period of time
                                   biting_rate_nymph=double(), # number of bites by nymph per cow per unit time #exposure rate by all nymphal ticks
                                   biting_rate_infected_nymph=double(), # number of bites by infected nymph per cow per unit time #exposure rate by infected nymphal ticks
                                   biting_rate_nymph_treated_cattle=double(), # number of bites by nymph per treated cow per unit time #exposure rate by all nymphal ticks to treated cattle
                                   biting_rate_nymph_untreated_cattle=double(), # number of bites by infected nymph per cow per unit time #exposure rate by all nymphal ticks to untreated cattle
                                   biting_rate_adult=double(), # number of bites by adult tick per cow per unit time #exposure rate by all adult ticks
                                   biting_rate_infected_adult=double(), # number of bites by infected adult tick per cow per unit time #exposure rate by infected adult ticks
                                   biting_rate_adult_treated_cattle=double(), # number of bites by adult ticks per treated cow per unit time #exposure rate by all adult ticks to treated cattle
                                   biting_rate_adult_untreated_cattle=double(), # number of bites by adult tick per untreated cow per unit time #exposure rate by all adult ticks to untreated cattle
                                   adultick_host_ratio=double(), # tick to host ratio
                                   nymph_host_ratio=double(), # tick to host ratio
                                   Egg.pop=double(), # total egg population
                                   Quest.pop=double(), # total questing tick population
                                   Feed.pop=double(), # total feeding tick population
                                   Ovip.female.pop=double(), # total egg-laying tick population,
                                   H_total=double(), # total host population
                                   H_susceptible =double(), # total susceptible host population
                                   H_acute =double(), # total acutely infectious host population
                                   H_acute_prev =double(), # prevalence of acutely infectious host population
                                   H_carrier =double(), # total persistent carrier host population
                                   H_I=double(), # infected host population # attack rate # The final size of an epidemic/outbreak i.e., the total number of hosts experiencing infection during the outbreak
                                   H_I_prev=double(), # prevalence of infected host population
                                   Q_I_adult=double(), # infectious questing tick population
                                   Q_I_nymph=double(), # infectious questing tick population
                                   Q_I_adult_prev=double(), # prevalence of infectious questing tick population
                                   Q_I_nymph_prev=double(), # prevalence of infectious questing tick population
                                   F_I_adult=double(), # infectious feeding tick population
                                   F_I_nymph=double(), # infectious feeding tick population
                                   F_I_prev_adult=double(), # prevalence of infectious feeding tick population
                                   F_I_prev_nymph=double(), # prevalence of infectious feeding tick population
                                   tick.pop=double(), # overall tick population
                                   T_H_ratio=double(), #tick to host ratio with feeding ticks only
                                   T_H_ratio_2=double(), #tick to host ratio with all ticks 
                                   coverage=double(),
                                   phi=factor(),
                                   delta=factor()
)

# Nested loop for coverage levels and treatment intervals -----------------

for (coverage_level in coverage) {
  for (treatment.interval in phi) {
    for (treatment.decay in delta) {
    print(coverage_level)
    print(treatment.interval)
    print(treatment.decay)
    parameter <- parameter_fun(coverage_level, treatment.interval, treatment.decay)
    
    
    # Run the ODE model function with the current coverage level, and treatment interval and decay
    output.treatment <- run.model.by.treatment.interval.decay.and.coverage(coverage_level, treatment.interval, treatment.decay, 
                                                                           time, initial.state, param = parameter)
    
    output.treatment <- as.data.frame(output.treatment) 

        
    # Create a data frame row with the results
    
    # storing last time step (row) outputs 
    alpha_N <- 1/20  # /day, nymph questing rate
    alpha_A <- 1/28  # /day, adult questing rate
    sigma = 1/15    #/day, rate of host recovery from disease #duration of infectiousness # the average time a cattle with the disease remains acutely infectious
    result.treatment <- data.frame(time=time,
                                   incidence_in_cattle = ((output.treatment[nrow(output.treatment),"H_UI"] + 
                                                             output.treatment[nrow(output.treatment),"H_TI"])/(output.treatment[nrow(output.treatment),"H_UC"] +
                                                                                                                 output.treatment[nrow(output.treatment),"H_TC"] +
                                                                                                                 output.treatment[nrow(output.treatment),"H_UI"] +
                                                                                                                 output.treatment[nrow(output.treatment),"H_TI"] +
                                                                                                                 output.treatment[nrow(output.treatment),"H_US"] +
                                                                                                                 output.treatment[nrow(output.treatment),"H_TS"]))/sigma,
                                   biting_rate_nymph = alpha_N * ((output.treatment[nrow(output.treatment),"NQ_UI"] + 
                                                                     output.treatment[nrow(output.treatment),"NQ_US"])/(output.treatment[nrow(output.treatment),"H_US"] + 
                                                                                                                          output.treatment[nrow(output.treatment),"H_TS"] +
                                                                                                                          output.treatment[nrow(output.treatment),"H_UC"] + 
                                                                                                                          output.treatment[nrow(output.treatment),"H_TC"] +
                                                                                                                          output.treatment[nrow(output.treatment),"H_UI"] + 
                                                                                                                          output.treatment[nrow(output.treatment),"H_TI"])),
                                   biting_rate_infected_nymph= alpha_N * ((output.treatment[nrow(output.treatment),"NQ_UI"] + 
                                                                             output.treatment[nrow(output.treatment),"NQ_US"])/(output.treatment[nrow(output.treatment),"H_US"] + 
                                                                                                                                  output.treatment[nrow(output.treatment),"H_TS"] +
                                                                                                                                  output.treatment[nrow(output.treatment),"H_UC"] + 
                                                                                                                                  output.treatment[nrow(output.treatment),"H_TC"] +
                                                                                                                                  output.treatment[nrow(output.treatment),"H_UI"] + 
                                                                                                                                  output.treatment[nrow(output.treatment),"H_TI"])) *
                                     ((output.treatment[nrow(output.treatment),"NQ_UI"])/(output.treatment[nrow(output.treatment),"NQ_UI"] + 
                                                                                            output.treatment[nrow(output.treatment),"NQ_US"])),
                                   biting_rate_nymph_treated_cattle= alpha_N * ((output.treatment[nrow(output.treatment),"NQ_UI"] + 
                                                                                   output.treatment[nrow(output.treatment),"NQ_US"])/(output.treatment[nrow(output.treatment),"H_US"] + 
                                                                                                                                        output.treatment[nrow(output.treatment),"H_TS"] +
                                                                                                                                        output.treatment[nrow(output.treatment),"H_UC"] + 
                                                                                                                                        output.treatment[nrow(output.treatment),"H_TC"] +
                                                                                                                                        output.treatment[nrow(output.treatment),"H_UI"] + 
                                                                                                                                        output.treatment[nrow(output.treatment),"H_TI"])) *
                                     ((output.treatment[nrow(output.treatment),"H_TS"] +
                                         output.treatment[nrow(output.treatment),"H_TC"] +
                                         output.treatment[nrow(output.treatment),"H_TI"])/(output.treatment[nrow(output.treatment),"H_US"] + 
                                                                                             output.treatment[nrow(output.treatment),"H_TS"] +
                                                                                             output.treatment[nrow(output.treatment),"H_UC"] + 
                                                                                             output.treatment[nrow(output.treatment),"H_TC"] +
                                                                                             output.treatment[nrow(output.treatment),"H_UI"] + 
                                                                                             output.treatment[nrow(output.treatment),"H_TI"])),
                                   biting_rate_nymph_untreated_cattle=alpha_N * ((output.treatment[nrow(output.treatment),"NQ_UI"] + 
                                                                                    output.treatment[nrow(output.treatment),"NQ_US"])/(output.treatment[nrow(output.treatment),"H_US"] + 
                                                                                                                                         output.treatment[nrow(output.treatment),"H_TS"] +
                                                                                                                                         output.treatment[nrow(output.treatment),"H_UC"] + 
                                                                                                                                         output.treatment[nrow(output.treatment),"H_TC"] +
                                                                                                                                         output.treatment[nrow(output.treatment),"H_UI"] + 
                                                                                                                                         output.treatment[nrow(output.treatment),"H_TI"])) *
                                     ((output.treatment[nrow(output.treatment),"H_US"] + 
                                         output.treatment[nrow(output.treatment),"H_UC"] + 
                                         output.treatment[nrow(output.treatment),"H_UI"])/(output.treatment[nrow(output.treatment),"H_US"] + 
                                                                                             output.treatment[nrow(output.treatment),"H_TS"] +
                                                                                             output.treatment[nrow(output.treatment),"H_UC"] + 
                                                                                             output.treatment[nrow(output.treatment),"H_TC"] +
                                                                                             output.treatment[nrow(output.treatment),"H_UI"] + 
                                                                                             output.treatment[nrow(output.treatment),"H_TI"])),
                                   biting_rate_adult = alpha_A * ((output.treatment[nrow(output.treatment),"AQ_UI"] + 
                                                                     output.treatment[nrow(output.treatment),"AQ_US"])/(output.treatment[nrow(output.treatment),"H_US"] + 
                                                                                                                          output.treatment[nrow(output.treatment),"H_TS"] +
                                                                                                                          output.treatment[nrow(output.treatment),"H_UC"] + 
                                                                                                                          output.treatment[nrow(output.treatment),"H_TC"] +
                                                                                                                          output.treatment[nrow(output.treatment),"H_UI"] + 
                                                                                                                          output.treatment[nrow(output.treatment),"H_TI"])),
                                   biting_rate_infected_adult= alpha_A * ((output.treatment[nrow(output.treatment),"AQ_UI"] + 
                                                                             output.treatment[nrow(output.treatment),"AQ_US"])/(output.treatment[nrow(output.treatment),"H_US"] + 
                                                                                                                                  output.treatment[nrow(output.treatment),"H_TS"] +
                                                                                                                                  output.treatment[nrow(output.treatment),"H_UC"] + 
                                                                                                                                  output.treatment[nrow(output.treatment),"H_TC"] +
                                                                                                                                  output.treatment[nrow(output.treatment),"H_UI"] + 
                                                                                                                                  output.treatment[nrow(output.treatment),"H_TI"])) *
                                     ((output.treatment[nrow(output.treatment),"AQ_UI"])/(output.treatment[nrow(output.treatment),"AQ_UI"] + 
                                                                                            output.treatment[nrow(output.treatment),"AQ_US"])),
                                   biting_rate_adult_treated_cattle= alpha_A * ((output.treatment[nrow(output.treatment),"AQ_UI"] + 
                                                                                   output.treatment[nrow(output.treatment),"AQ_US"])/(output.treatment[nrow(output.treatment),"H_US"] + 
                                                                                                                                        output.treatment[nrow(output.treatment),"H_TS"] +
                                                                                                                                        output.treatment[nrow(output.treatment),"H_UC"] + 
                                                                                                                                        output.treatment[nrow(output.treatment),"H_TC"] +
                                                                                                                                        output.treatment[nrow(output.treatment),"H_UI"] + 
                                                                                                                                        output.treatment[nrow(output.treatment),"H_TI"])) *
                                     ((output.treatment[nrow(output.treatment),"H_TS"] +
                                         output.treatment[nrow(output.treatment),"H_TC"] +
                                         output.treatment[nrow(output.treatment),"H_TI"])/(output.treatment[nrow(output.treatment),"H_US"] + 
                                                                                             output.treatment[nrow(output.treatment),"H_TS"] +
                                                                                             output.treatment[nrow(output.treatment),"H_UC"] + 
                                                                                             output.treatment[nrow(output.treatment),"H_TC"] +
                                                                                             output.treatment[nrow(output.treatment),"H_UI"] + 
                                                                                             output.treatment[nrow(output.treatment),"H_TI"])),
                                   biting_rate_adult_untreated_cattle= alpha_A * ((output.treatment[nrow(output.treatment),"AQ_UI"] + 
                                                                                     output.treatment[nrow(output.treatment),"AQ_US"])/(output.treatment[nrow(output.treatment),"H_US"] + 
                                                                                                                                          output.treatment[nrow(output.treatment),"H_TS"] +
                                                                                                                                          output.treatment[nrow(output.treatment),"H_UC"] + 
                                                                                                                                          output.treatment[nrow(output.treatment),"H_TC"] +
                                                                                                                                          output.treatment[nrow(output.treatment),"H_UI"] + 
                                                                                                                                          output.treatment[nrow(output.treatment),"H_TI"])) * 
                                     ((output.treatment[nrow(output.treatment),"H_US"] + 
                                         output.treatment[nrow(output.treatment),"H_UC"] + 
                                         output.treatment[nrow(output.treatment),"H_UI"])/(output.treatment[nrow(output.treatment),"H_US"] + 
                                                                                             output.treatment[nrow(output.treatment),"H_TS"] +
                                                                                             output.treatment[nrow(output.treatment),"H_UC"] + 
                                                                                             output.treatment[nrow(output.treatment),"H_TC"] +
                                                                                             output.treatment[nrow(output.treatment),"H_UI"] + 
                                                                                             output.treatment[nrow(output.treatment),"H_TI"])),
                                   adultick_host_ratio=(output.treatment[nrow(output.treatment),"AF_US"] +
                                                          output.treatment[nrow(output.treatment),"AF_UI"])/(output.treatment[nrow(output.treatment),"H_US"] + 
                                                                                                               output.treatment[nrow(output.treatment),"H_TS"] +
                                                                                                               output.treatment[nrow(output.treatment),"H_UC"] + 
                                                                                                               output.treatment[nrow(output.treatment),"H_TC"] +
                                                                                                               output.treatment[nrow(output.treatment),"H_UI"] + 
                                                                                                               output.treatment[nrow(output.treatment),"H_TI"]),
                                   nymph_host_ratio=(output.treatment[nrow(output.treatment),"NF_US"] +
                                                       output.treatment[nrow(output.treatment),"NF_UI"])/(output.treatment[nrow(output.treatment),"H_US"] + 
                                                                                                            output.treatment[nrow(output.treatment),"H_TS"] +
                                                                                                            output.treatment[nrow(output.treatment),"H_UC"] + 
                                                                                                            output.treatment[nrow(output.treatment),"H_TC"] +
                                                                                                            output.treatment[nrow(output.treatment),"H_UI"] + 
                                                                                                            output.treatment[nrow(output.treatment),"H_TI"]),
                                   Egg.pop = output.treatment[nrow(output.treatment),"E_US"] + output.treatment[nrow(output.treatment),"E_TS"],
                                   Quest.pop = output.treatment[nrow(output.treatment),"AQ_UI"] + output.treatment[nrow(output.treatment),"AQ_US"] +
                                     output.treatment[nrow(output.treatment),"NQ_UI"] + output.treatment[nrow(output.treatment),"NQ_US"] +
                                     output.treatment[nrow(output.treatment),"LQ_US"],
                                   Feed.pop = output.treatment[nrow(output.treatment),"NF_UI"] + output.treatment[nrow(output.treatment),"NF_US"] +
                                     output.treatment[nrow(output.treatment),"AF_UI"] + output.treatment[nrow(output.treatment),"AF_US"] + 
                                     output.treatment[nrow(output.treatment),"LF_US"],
                                   Ovip.female.pop = output.treatment[nrow(output.treatment),"AO_U"] + output.treatment[nrow(output.treatment),"AO_T"],
                                   H_total = output.treatment[nrow(output.treatment),"H_UC"] + 
                                     output.treatment[nrow(output.treatment),"H_TC"] +
                                     output.treatment[nrow(output.treatment),"H_UI"] +
                                     output.treatment[nrow(output.treatment),"H_TI"] +
                                     output.treatment[nrow(output.treatment),"H_US"] +
                                     output.treatment[nrow(output.treatment),"H_TS"],
                                   
                                   H_susceptible = output.treatment[nrow(output.treatment),"H_US"] + output.treatment[nrow(output.treatment),"H_TS"],
                                   H_acute = output.treatment[nrow(output.treatment),"H_UI"] + output.treatment[nrow(output.treatment),"H_TI"],
                                   H_acute_prev = ((output.treatment[nrow(output.treatment),"H_UI"] + 
                                                      output.treatment[nrow(output.treatment),"H_TI"])/(output.treatment[nrow(output.treatment),"H_UC"] +
                                                                                                          output.treatment[nrow(output.treatment),"H_TC"] +
                                                                                                          output.treatment[nrow(output.treatment),"H_UI"] +
                                                                                                          output.treatment[nrow(output.treatment),"H_TI"] +
                                                                                                          output.treatment[nrow(output.treatment),"H_US"] +
                                                                                                          output.treatment[nrow(output.treatment),"H_TS"]))*100,
                                   H_carrier = output.treatment[nrow(output.treatment),"H_UC"] + output.treatment[nrow(output.treatment),"H_TC"],
                                   H_I = output.treatment[nrow(output.treatment),"H_UC"] + 
                                     output.treatment[nrow(output.treatment),"H_TC"] +
                                     output.treatment[nrow(output.treatment),"H_UI"] + 
                                     output.treatment[nrow(output.treatment),"H_TI"],
                                   
                                   H_I_prev = ((output.treatment[nrow(output.treatment),"H_UC"] +
                                                  output.treatment[nrow(output.treatment),"H_TC"] +
                                                  output.treatment[nrow(output.treatment),"H_UI"] +
                                                  output.treatment[nrow(output.treatment),"H_TI"])/(output.treatment[nrow(output.treatment),"H_UC"] +
                                                                                                      output.treatment[nrow(output.treatment),"H_TC"] +
                                                                                                      output.treatment[nrow(output.treatment),"H_UI"] +
                                                                                                      output.treatment[nrow(output.treatment),"H_TI"] +
                                                                                                      output.treatment[nrow(output.treatment),"H_US"] +
                                                                                                      output.treatment[nrow(output.treatment),"H_TS"]))*100,
                                   Q_I_adult = output.treatment[nrow(output.treatment),"AQ_UI"],
                                   Q_I_nymph = output.treatment[nrow(output.treatment),"NQ_UI"],
                                   Q_I_adult_prev = ((output.treatment[nrow(output.treatment),"AQ_UI"])/(output.treatment[nrow(output.treatment),"AQ_UI"] +
                                                                                                           output.treatment[nrow(output.treatment),"AQ_US"]))*100,
                                   Q_I_nymph_prev = ((output.treatment[nrow(output.treatment),"NQ_UI"])/(output.treatment[nrow(output.treatment),"NQ_UI"] +
                                                                                                           output.treatment[nrow(output.treatment),"NQ_US"]))*100,
                                   F_I_adult = output.treatment[nrow(output.treatment),"AF_UI"],
                                   F_I_nymph = output.treatment[nrow(output.treatment),"NF_UI"],
                                   F_I_prev_adult = ((output.treatment[nrow(output.treatment),"AF_UI"])/(output.treatment[nrow(output.treatment),"AF_UI"] +
                                                                                                           output.treatment[nrow(output.treatment),"AF_US"]))*100,
                                   F_I_prev_nymph = ((output.treatment[nrow(output.treatment),"NF_UI"])/(output.treatment[nrow(output.treatment),"NF_UI"] + 
                                                                                                           output.treatment[nrow(output.treatment),"NF_US"]))*100,
                                   tick.pop = (output.treatment[nrow(output.treatment),"LQ_US"] +
                                                 output.treatment[nrow(output.treatment),"LF_US"] +
                                                 output.treatment[nrow(output.treatment),"LD_US"] +
                                                 output.treatment[nrow(output.treatment),"LD_TS"] +
                                                 output.treatment[nrow(output.treatment),"LD_UI"] +
                                                 output.treatment[nrow(output.treatment),"LD_TI"] +
                                                 output.treatment[nrow(output.treatment),"NQ_US"] +
                                                 output.treatment[nrow(output.treatment),"NQ_UI"] +
                                                 output.treatment[nrow(output.treatment),"NF_US"] +
                                                 output.treatment[nrow(output.treatment),"NF_UI"] +
                                                 output.treatment[nrow(output.treatment),"ND_US"] +
                                                 output.treatment[nrow(output.treatment),"ND_TS"] +
                                                 output.treatment[nrow(output.treatment),"ND_UI"] +
                                                 output.treatment[nrow(output.treatment),"ND_TI"] +
                                                 output.treatment[nrow(output.treatment),"AQ_US"] +
                                                 output.treatment[nrow(output.treatment),"AQ_UI"] +
                                                 output.treatment[nrow(output.treatment),"AF_US"] +
                                                 output.treatment[nrow(output.treatment),"AF_UI"] +
                                                 output.treatment[nrow(output.treatment),"AD_U"] +
                                                 output.treatment[nrow(output.treatment),"AD_T"] +
                                                 output.treatment[nrow(output.treatment),"AO_U"] +
                                                 output.treatment[nrow(output.treatment),"AO_T"]),
                                   T_H_ratio = (output.treatment[nrow(output.treatment),"LF_US"] +
                                                  output.treatment[nrow(output.treatment),"NF_US"] +
                                                  output.treatment[nrow(output.treatment),"NF_UI"] +
                                                  output.treatment[nrow(output.treatment),"AF_US"] +
                                                  output.treatment[nrow(output.treatment),"AF_UI"])/(output.treatment[nrow(output.treatment),"H_US"] +
                                                                                                       output.treatment[nrow(output.treatment),"H_TS"] +
                                                                                                       output.treatment[nrow(output.treatment),"H_UC"] + 
                                                                                                       output.treatment[nrow(output.treatment),"H_TC"] +
                                                                                                       output.treatment[nrow(output.treatment),"H_UI"] + 
                                                                                                       output.treatment[nrow(output.treatment),"H_TI"]),
                                   T_H_ratio_2 = (output.treatment[nrow(output.treatment),"LQ_US"] +
                                                    output.treatment[nrow(output.treatment),"LF_US"] +
                                                    output.treatment[nrow(output.treatment),"LD_US"] +
                                                    output.treatment[nrow(output.treatment),"LD_TS"] +
                                                    output.treatment[nrow(output.treatment),"LD_UI"] +
                                                    output.treatment[nrow(output.treatment),"LD_TI"] +
                                                    output.treatment[nrow(output.treatment),"NQ_US"] +
                                                    output.treatment[nrow(output.treatment),"NQ_UI"] +
                                                    output.treatment[nrow(output.treatment),"NF_US"] +
                                                    output.treatment[nrow(output.treatment),"NF_UI"] +
                                                    output.treatment[nrow(output.treatment),"ND_US"] +
                                                    output.treatment[nrow(output.treatment),"ND_TS"] +
                                                    output.treatment[nrow(output.treatment),"ND_UI"] +
                                                    output.treatment[nrow(output.treatment),"ND_TI"] +
                                                    output.treatment[nrow(output.treatment),"AQ_US"] +
                                                    output.treatment[nrow(output.treatment),"AQ_UI"] +
                                                    output.treatment[nrow(output.treatment),"AF_US"] +
                                                    output.treatment[nrow(output.treatment),"AF_UI"] +
                                                    output.treatment[nrow(output.treatment),"AD_U"] +
                                                    output.treatment[nrow(output.treatment),"AD_T"] +
                                                    output.treatment[nrow(output.treatment),"AO_U"] +
                                                    output.treatment[nrow(output.treatment),"AO_T"])/(output.treatment[nrow(output.treatment),"H_US"] +
                                                                                                        output.treatment[nrow(output.treatment),"H_TS"] +
                                                                                                        output.treatment[nrow(output.treatment),"H_UC"] + 
                                                                                                        output.treatment[nrow(output.treatment),"H_TC"] +
                                                                                                        output.treatment[nrow(output.treatment),"H_UI"] + 
                                                                                                        output.treatment[nrow(output.treatment),"H_TI"]),
                                   coverage = coverage_level,
                                   phi = treatment.interval,
                                   delta = treatment.decay)



    # Append the result row to the main results data frame
    
    results.treatment.df <- rbind(results.treatment.df, result.treatment)  
    

    }
  }
}


## view the stored data from the calculations

View(results.treatment.df) 


##____________________________________________________________________________


# Figure 4 in manuscript --------------------------------------------------


## Adjust the cases relative to the baseline and store it in a new column called "adjusted..." 
results.treatment.df$adjusted_H_acute <- ((63.09235 - results.treatment.df$H_acute)/63.09235)*100

# Reverse the Reduction values to show a declining trend
results.treatment.df$adjusted_H_acute <- -results.treatment.df$adjusted_H_acute


View(results.treatment.df)


### Weekly treatment interval_______________________________________________  

weekly.cases.relative <- ggplot(subset(results.treatment.df, (phi %in% "7") & (delta %in% c("1", "0.333333333333333", "0.2", "0.142857142857143", "0.1"))), 
                                aes(x = coverage, y = adjusted_H_acute, group = factor(delta), color=factor(delta))) +
  geom_line() +
  geom_point() + 
  theme_bw() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1.05), breaks = seq(0, 1, 0.2)) +  
  scale_y_continuous(expand = c(0, 0), limits = c(-100, 1), breaks = seq(-100, 1, 10)) + 
  labs(title="Weekly treatment", x = "", y = "Change in acute infections (%)") +
  theme(plot.title = element_text(hjust=0.5),         
        axis.title.y = element_text(face="bold")) +  
  #theme(legend.position="top") +
  theme(legend.position="none") +  
  scale_color_discrete(name = "Fungal decay time",
                       breaks=c("1", "0.333333333333333", "0.2", "0.142857142857143", "0.1"),
                       labels=c("1 day", "3 days", "5 days", "7 days", "10 days")) 


weekly.cases.relative  


### Biweekly treatment interval_______________________________________________  

biweekly.cases.relative <- ggplot(subset(results.treatment.df, (phi %in% "14") & (delta %in% c("1", "0.333333333333333", "0.2", "0.142857142857143", "0.1"))), 
                                  aes(x = coverage, y = adjusted_H_acute, group = factor(delta), color=factor(delta))) +
  geom_line() +
  geom_point() + 
  theme_bw() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1.05), breaks = seq(0, 1, 0.2)) +  
  scale_y_continuous(expand = c(0, 0), limits = c(-100, 1), breaks = seq(-100, 1, 10)) + 
  labs(title="Biweekly treatment", x = "", y = "") +
  theme(plot.title = element_text(hjust=0.5),    
        axis.text.y=element_blank(),axis.ticks.y=element_blank()) +  
  #theme(legend.position="top") +
  theme(legend.position="none") +  
  scale_color_discrete(name = "Fungal decay time",
                       breaks=c("1", "0.333333333333333", "0.2", "0.142857142857143", "0.1"),
                       labels=c("1 day", "3 days", "5 days", "7 days", "10 days")) 

biweekly.cases.relative  


### Monthly treatment interval_______________________________________________  

monthly.cases.relative <- ggplot(subset(results.treatment.df, (phi %in% "28") & (delta %in% c("1", "0.333333333333333", "0.2", "0.142857142857143", "0.1"))), 
                                 aes(x = coverage, y = adjusted_H_acute, group = factor(delta), color=factor(delta))) +
  geom_line() +
  geom_point() + 
  theme_bw() +
  #scale_x_continuous(expand = c(0, 0), limits = c(0, 1.05), breaks = seq(0, 1, 0.1)) +  
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1.05), breaks = seq(0, 1, 0.2)) +  
  scale_y_continuous(expand = c(0, 0), limits = c(-100, 1), breaks = seq(-100, 1, 10)) + 
  labs(title="Monthly treatment", x = "", y = "") +
  theme(plot.title = element_text(hjust=0.5),    
        axis.text.y=element_blank(),axis.ticks.y=element_blank()) +  
  #theme(legend.position="top") +
  theme(legend.position="none") +  
  scale_color_discrete(name = "Fungal decay time",
                       breaks=c("1", "0.333333333333333", "0.2", "0.142857142857143", "0.1"),
                       labels=c("1 day", "3 days", "5 days", "7 days", "10 days")) 

monthly.cases.relative  



# Combining the plots for decay time using ggarrange ----------------------

library(ggpubr) 

decay.time.relative.cases <- ggarrange(weekly.cases.relative, 
                                       biweekly.cases.relative, 
                                       monthly.cases.relative, 
                                       labels = c("A", "B", "C"), 
                                       ncol = 3, nrow = 1,
                                       common.legend = TRUE, legend = "top")

decay.time.relative.cases

## Annotate the arranged figure

annotate_figure(decay.time.relative.cases, bottom = text_grob("Coverage", color = "black", face = "bold")) 



#Export image

#jpeg
ggsave("Figure_4.jpeg", plot = last_plot(), device = "jpeg", scale = 1,
       width = 7, height = 4, units = c("in", "cm", "mm", "px"), dpi = 300,
       limitsize = TRUE, bg = "white")

#TIFF
ggsave("Figure_4.tiff", plot = last_plot(), device = "tiff", scale = 1,
       width = 7, height = 7, units = c("in", "cm", "mm", "px"), dpi = 300,
       limitsize = TRUE, bg = "white")


