
# This script generates figures 3 and 5 in the manuscript


# Figure 3 script ---------------------------------------------------------


# Set work directory ------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
getwd()


# Load packages -----------------------------------------------------------

library(deSolve)
library(ggplot2)
library(tictoc) 
library(Cairo) 


# source function files ---------------------------------------------------

source('model_tick_functions.R')


# Set Parameter values --------------------------------------------------------

#create function for the parameters
parameter_fun <-function(coverage_level, treatment.interval){ 
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
                  tau_A = 3,       #increase in feeding duration due to treatment effect
                  mu_AD = 0.02,    # /day, developing adult mortality
                  mu_ADF = 0.02*9.886674,  # Weibull # is the  additional mortality due to fungal effect   
                  k_o = 6,         # day, preoviposition period
                  k_tau = 2.3,     # day, increase in preoviposition period due to fungal effect 
                  zeta = 0.5,      # sex ratio
                  mu_AO = 0.02,    # /day, ovipositing adult mortality
                  mu_AOF = 0.02*9.886674,  # Weibull # is the  additional mortality due to fungal effect   
                  p_HN = 0.09,     # Probability nymphal tick infects susceptible host
                  p_HA = 0.9,      # Probability adult tick infects susceptible host
                  mu_H = 0.0006859604, # Natural host mortality
                  mu_I = 0.25*0.014, # /day, Mortality due to East Coast fever
                  sigma = 1/15,    # /day, rate of host recovery from disease  
                  delta = 1/1,     # /day, rate of clearance/decay of fungal conidial spores from treated cattle skin 
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


# Define a vector of coverage levels and treatment intervals --------------

coverage <- seq(from = 0, to = 1, by = 0.05) # Vector of treatment coverage

phi <- c(7, 14, 21, 28)  # Vector of treatment interval in days: Weekly, biweekly, triweekly, and monthly treatment frequencies (in days)



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
                                   phi=factor()
)

# Nested loop for coverage levels and treatment intervals -----------------

for (coverage_level in coverage) {
  for (treatment.interval in phi) {
    print(coverage_level)
    print(treatment.interval)
    parameter <- parameter_fun(coverage_level, treatment.interval)
    
    
   # Run the ODE model function with the current coverage level and treatment interval
    output.treatment <- run.model.by.treatment.interval.and.coverage(coverage_level, treatment.interval, time, initial.state, 
                                                                     param = parameter)
    
    output.treatment <- as.data.frame(output.treatment) 
    

    # Create a data frame row with the results
    
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
                                   phi = treatment.interval)
    
    
    
    # Append the result row to the main results data frame
    
    results.treatment.df <- rbind(results.treatment.df, result.treatment)  
    

  }
}

## view the stored data from the calculations

View(results.treatment.df) 



# Figure 3 in manuscript --------------------------------------------------

## Adjust the cases relative to the baseline and store it in a new column called "adjusted..." 
results.treatment.df$adjusted_H_acute <- ((63.09235 - results.treatment.df$H_acute)/63.09235)*100

# Reverse the Reduction values to show a declining trend
results.treatment.df$adjusted_H_acute <- -results.treatment.df$adjusted_H_acute


View(results.treatment.df)


#### for PPT__________________________________________________  

acute.cases.host.relative <- ggplot(data = results.treatment.df, aes(x = coverage, y = adjusted_H_acute, group = factor(phi), color=factor(phi))) +
  geom_line() +
  geom_point() + 
  theme_bw() +
  #scale_x_continuous(expand = c(0, 0), limits = c(0, 1.05), breaks = seq(0, 1, 0.1)) +  
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1.05), breaks = seq(0, 1, 0.1)) +  
  scale_y_continuous(expand = c(0, 0), limits = c(-100, 1), breaks = seq(-100, 1, 10)) + 
  labs(title="", x = "Coverage", y = "Change in acute infections (%)") +
  theme(plot.title = element_text(hjust=0.5)) +  
  theme(legend.position="top") +
  scale_color_discrete(name = "Treatment interval",
                       breaks=c("28", "21", "14", "7"),
                       labels=c("Monthly", "Triweekly", "Biweekly", "Weekly")) 


acute.cases.host.relative  


#Export image

#jpeg
ggsave("Figure_3.jpeg", plot = last_plot(), device = "jpeg", scale = 1,
       width = 7, height = 5, units = c("in", "cm", "mm", "px"), dpi = 300,
       limitsize = TRUE, bg = "white")

#TIFF
ggsave("Figure_3.tiff", plot = last_plot(), device = "tiff", scale = 1,
       width = 7, height = 5, units = c("in", "cm", "mm", "px"), dpi = 300,
       limitsize = TRUE, bg = "white")









# Figure 5 script ---------------------------------------------------------

## Composite effects of EPF
### involves 'switching off' treatment effect parameters one-at-a-time

# (a). without mortality effect -------------------------------------------

# Create an empty data frame with the necessary structure/columns ---------

results.without.mortality.df <- data.frame(time=double(), # duration of simulation
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
                                           phi=factor()
)

# Nested loop for coverage levels and treatment intervals -----------------

for (coverage_level in coverage) {
  for (treatment.interval in phi) {
    print(coverage_level)
    print(treatment.interval)
    parameter <- parameter_fun(coverage_level, treatment.interval)
    parameter["mu_LDF"] <- 0 # 0.177388*9.886674
    parameter["mu_NDF"] <- 0 # 0.065*9.886674
    parameter["mu_ADF"] <- 0 # 0.02*9.886674
    parameter["mu_AOF"] <- 0 # 0.02*9.886674
    
    
    # Run the ODE model function with the current coverage level and treatment interval
    output.without.mortality <- run.model.by.treatment.interval.and.coverage(coverage_level, treatment.interval, time, initial.state, 
                                                                             param = parameter)
    
    output.without.mortality <- as.data.frame(output.without.mortality) 
    

    # Create a data frame row with the results
    
    alpha_N <- 1/20  # /day, nymph questing rate
    alpha_A <- 1/28  # /day, adult questing rate
    sigma = 1/15    #/day, rate of host recovery from disease #duration of infectiousness # the average time a cattle with the disease remains acutely infectious
    result.without.mortality <- data.frame(time=time,
                                           incidence_in_cattle = ((output.without.mortality[nrow(output.without.mortality),"H_UI"] + 
                                                                     output.without.mortality[nrow(output.without.mortality),"H_TI"])/(output.without.mortality[nrow(output.without.mortality),"H_UC"] +
                                                                                                                                         output.without.mortality[nrow(output.without.mortality),"H_TC"] +
                                                                                                                                         output.without.mortality[nrow(output.without.mortality),"H_UI"] +
                                                                                                                                         output.without.mortality[nrow(output.without.mortality),"H_TI"] +
                                                                                                                                         output.without.mortality[nrow(output.without.mortality),"H_US"] +
                                                                                                                                         output.without.mortality[nrow(output.without.mortality),"H_TS"]))/sigma,
                                           biting_rate_nymph = alpha_N * ((output.without.mortality[nrow(output.without.mortality),"NQ_UI"] + 
                                                                             output.without.mortality[nrow(output.without.mortality),"NQ_US"])/(output.without.mortality[nrow(output.without.mortality),"H_US"] + 
                                                                                                                                                  output.without.mortality[nrow(output.without.mortality),"H_TS"] +
                                                                                                                                                  output.without.mortality[nrow(output.without.mortality),"H_UC"] + 
                                                                                                                                                  output.without.mortality[nrow(output.without.mortality),"H_TC"] +
                                                                                                                                                  output.without.mortality[nrow(output.without.mortality),"H_UI"] + 
                                                                                                                                                  output.without.mortality[nrow(output.without.mortality),"H_TI"])),
                                           biting_rate_infected_nymph= alpha_N * ((output.without.mortality[nrow(output.without.mortality),"NQ_UI"] + 
                                                                                     output.without.mortality[nrow(output.without.mortality),"NQ_US"])/(output.without.mortality[nrow(output.without.mortality),"H_US"] + 
                                                                                                                                                          output.without.mortality[nrow(output.without.mortality),"H_TS"] +
                                                                                                                                                          output.without.mortality[nrow(output.without.mortality),"H_UC"] + 
                                                                                                                                                          output.without.mortality[nrow(output.without.mortality),"H_TC"] +
                                                                                                                                                          output.without.mortality[nrow(output.without.mortality),"H_UI"] + 
                                                                                                                                                          output.without.mortality[nrow(output.without.mortality),"H_TI"])) *
                                             ((output.without.mortality[nrow(output.without.mortality),"NQ_UI"])/(output.without.mortality[nrow(output.without.mortality),"NQ_UI"] + 
                                                                                                                    output.without.mortality[nrow(output.without.mortality),"NQ_US"])),
                                           biting_rate_nymph_treated_cattle= alpha_N * ((output.without.mortality[nrow(output.without.mortality),"NQ_UI"] + 
                                                                                           output.without.mortality[nrow(output.without.mortality),"NQ_US"])/(output.without.mortality[nrow(output.without.mortality),"H_US"] + 
                                                                                                                                                                output.without.mortality[nrow(output.without.mortality),"H_TS"] +
                                                                                                                                                                output.without.mortality[nrow(output.without.mortality),"H_UC"] + 
                                                                                                                                                                output.without.mortality[nrow(output.without.mortality),"H_TC"] +
                                                                                                                                                                output.without.mortality[nrow(output.without.mortality),"H_UI"] + 
                                                                                                                                                                output.without.mortality[nrow(output.without.mortality),"H_TI"])) *
                                             ((output.without.mortality[nrow(output.without.mortality),"H_TS"] +
                                                 output.without.mortality[nrow(output.without.mortality),"H_TC"] +
                                                 output.without.mortality[nrow(output.without.mortality),"H_TI"])/(output.without.mortality[nrow(output.without.mortality),"H_US"] + 
                                                                                                                     output.without.mortality[nrow(output.without.mortality),"H_TS"] +
                                                                                                                     output.without.mortality[nrow(output.without.mortality),"H_UC"] + 
                                                                                                                     output.without.mortality[nrow(output.without.mortality),"H_TC"] +
                                                                                                                     output.without.mortality[nrow(output.without.mortality),"H_UI"] + 
                                                                                                                     output.without.mortality[nrow(output.without.mortality),"H_TI"])),
                                           biting_rate_nymph_untreated_cattle=alpha_N * ((output.without.mortality[nrow(output.without.mortality),"NQ_UI"] + 
                                                                                            output.without.mortality[nrow(output.without.mortality),"NQ_US"])/(output.without.mortality[nrow(output.without.mortality),"H_US"] + 
                                                                                                                                                                 output.without.mortality[nrow(output.without.mortality),"H_TS"] +
                                                                                                                                                                 output.without.mortality[nrow(output.without.mortality),"H_UC"] + 
                                                                                                                                                                 output.without.mortality[nrow(output.without.mortality),"H_TC"] +
                                                                                                                                                                 output.without.mortality[nrow(output.without.mortality),"H_UI"] + 
                                                                                                                                                                 output.without.mortality[nrow(output.without.mortality),"H_TI"])) *
                                             ((output.without.mortality[nrow(output.without.mortality),"H_US"] + 
                                                 output.without.mortality[nrow(output.without.mortality),"H_UC"] + 
                                                 output.without.mortality[nrow(output.without.mortality),"H_UI"])/(output.without.mortality[nrow(output.without.mortality),"H_US"] + 
                                                                                                                     output.without.mortality[nrow(output.without.mortality),"H_TS"] +
                                                                                                                     output.without.mortality[nrow(output.without.mortality),"H_UC"] + 
                                                                                                                     output.without.mortality[nrow(output.without.mortality),"H_TC"] +
                                                                                                                     output.without.mortality[nrow(output.without.mortality),"H_UI"] + 
                                                                                                                     output.without.mortality[nrow(output.without.mortality),"H_TI"])),
                                           biting_rate_adult = alpha_A * ((output.without.mortality[nrow(output.without.mortality),"AQ_UI"] + 
                                                                             output.without.mortality[nrow(output.without.mortality),"AQ_US"])/(output.without.mortality[nrow(output.without.mortality),"H_US"] + 
                                                                                                                                                  output.without.mortality[nrow(output.without.mortality),"H_TS"] +
                                                                                                                                                  output.without.mortality[nrow(output.without.mortality),"H_UC"] + 
                                                                                                                                                  output.without.mortality[nrow(output.without.mortality),"H_TC"] +
                                                                                                                                                  output.without.mortality[nrow(output.without.mortality),"H_UI"] + 
                                                                                                                                                  output.without.mortality[nrow(output.without.mortality),"H_TI"])),
                                           biting_rate_infected_adult= alpha_A * ((output.without.mortality[nrow(output.without.mortality),"AQ_UI"] + 
                                                                                     output.without.mortality[nrow(output.without.mortality),"AQ_US"])/(output.without.mortality[nrow(output.without.mortality),"H_US"] + 
                                                                                                                                                          output.without.mortality[nrow(output.without.mortality),"H_TS"] +
                                                                                                                                                          output.without.mortality[nrow(output.without.mortality),"H_UC"] + 
                                                                                                                                                          output.without.mortality[nrow(output.without.mortality),"H_TC"] +
                                                                                                                                                          output.without.mortality[nrow(output.without.mortality),"H_UI"] + 
                                                                                                                                                          output.without.mortality[nrow(output.without.mortality),"H_TI"])) *
                                             ((output.without.mortality[nrow(output.without.mortality),"AQ_UI"])/(output.without.mortality[nrow(output.without.mortality),"AQ_UI"] + 
                                                                                                                    output.without.mortality[nrow(output.without.mortality),"AQ_US"])),
                                           biting_rate_adult_treated_cattle= alpha_A * ((output.without.mortality[nrow(output.without.mortality),"AQ_UI"] + 
                                                                                           output.without.mortality[nrow(output.without.mortality),"AQ_US"])/(output.without.mortality[nrow(output.without.mortality),"H_US"] + 
                                                                                                                                                                output.without.mortality[nrow(output.without.mortality),"H_TS"] +
                                                                                                                                                                output.without.mortality[nrow(output.without.mortality),"H_UC"] + 
                                                                                                                                                                output.without.mortality[nrow(output.without.mortality),"H_TC"] +
                                                                                                                                                                output.without.mortality[nrow(output.without.mortality),"H_UI"] + 
                                                                                                                                                                output.without.mortality[nrow(output.without.mortality),"H_TI"])) *
                                             ((output.without.mortality[nrow(output.without.mortality),"H_TS"] +
                                                 output.without.mortality[nrow(output.without.mortality),"H_TC"] +
                                                 output.without.mortality[nrow(output.without.mortality),"H_TI"])/(output.without.mortality[nrow(output.without.mortality),"H_US"] + 
                                                                                                                     output.without.mortality[nrow(output.without.mortality),"H_TS"] +
                                                                                                                     output.without.mortality[nrow(output.without.mortality),"H_UC"] + 
                                                                                                                     output.without.mortality[nrow(output.without.mortality),"H_TC"] +
                                                                                                                     output.without.mortality[nrow(output.without.mortality),"H_UI"] + 
                                                                                                                     output.without.mortality[nrow(output.without.mortality),"H_TI"])),
                                           biting_rate_adult_untreated_cattle= alpha_A * ((output.without.mortality[nrow(output.without.mortality),"AQ_UI"] + 
                                                                                             output.without.mortality[nrow(output.without.mortality),"AQ_US"])/(output.without.mortality[nrow(output.without.mortality),"H_US"] + 
                                                                                                                                                                  output.without.mortality[nrow(output.without.mortality),"H_TS"] +
                                                                                                                                                                  output.without.mortality[nrow(output.without.mortality),"H_UC"] + 
                                                                                                                                                                  output.without.mortality[nrow(output.without.mortality),"H_TC"] +
                                                                                                                                                                  output.without.mortality[nrow(output.without.mortality),"H_UI"] + 
                                                                                                                                                                  output.without.mortality[nrow(output.without.mortality),"H_TI"])) * 
                                             ((output.without.mortality[nrow(output.without.mortality),"H_US"] + 
                                                 output.without.mortality[nrow(output.without.mortality),"H_UC"] + 
                                                 output.without.mortality[nrow(output.without.mortality),"H_UI"])/(output.without.mortality[nrow(output.without.mortality),"H_US"] + 
                                                                                                                     output.without.mortality[nrow(output.without.mortality),"H_TS"] +
                                                                                                                     output.without.mortality[nrow(output.without.mortality),"H_UC"] + 
                                                                                                                     output.without.mortality[nrow(output.without.mortality),"H_TC"] +
                                                                                                                     output.without.mortality[nrow(output.without.mortality),"H_UI"] + 
                                                                                                                     output.without.mortality[nrow(output.without.mortality),"H_TI"])),
                                           adultick_host_ratio=(output.without.mortality[nrow(output.without.mortality),"AF_US"] +
                                                                  output.without.mortality[nrow(output.without.mortality),"AF_UI"])/(output.without.mortality[nrow(output.without.mortality),"H_US"] + 
                                                                                                                                       output.without.mortality[nrow(output.without.mortality),"H_TS"] +
                                                                                                                                       output.without.mortality[nrow(output.without.mortality),"H_UC"] + 
                                                                                                                                       output.without.mortality[nrow(output.without.mortality),"H_TC"] +
                                                                                                                                       output.without.mortality[nrow(output.without.mortality),"H_UI"] + 
                                                                                                                                       output.without.mortality[nrow(output.without.mortality),"H_TI"]),
                                           nymph_host_ratio=(output.without.mortality[nrow(output.without.mortality),"NF_US"] +
                                                               output.without.mortality[nrow(output.without.mortality),"NF_UI"])/(output.without.mortality[nrow(output.without.mortality),"H_US"] + 
                                                                                                                                    output.without.mortality[nrow(output.without.mortality),"H_TS"] +
                                                                                                                                    output.without.mortality[nrow(output.without.mortality),"H_UC"] + 
                                                                                                                                    output.without.mortality[nrow(output.without.mortality),"H_TC"] +
                                                                                                                                    output.without.mortality[nrow(output.without.mortality),"H_UI"] + 
                                                                                                                                    output.without.mortality[nrow(output.without.mortality),"H_TI"]),
                                           Egg.pop = output.without.mortality[nrow(output.without.mortality),"E_US"] + output.without.mortality[nrow(output.without.mortality),"E_TS"],
                                           Quest.pop = output.without.mortality[nrow(output.without.mortality),"AQ_UI"] + output.without.mortality[nrow(output.without.mortality),"AQ_US"] +
                                             output.without.mortality[nrow(output.without.mortality),"NQ_UI"] + output.without.mortality[nrow(output.without.mortality),"NQ_US"] +
                                             output.without.mortality[nrow(output.without.mortality),"LQ_US"],
                                           Feed.pop = output.without.mortality[nrow(output.without.mortality),"NF_UI"] + output.without.mortality[nrow(output.without.mortality),"NF_US"] +
                                             output.without.mortality[nrow(output.without.mortality),"AF_UI"] + output.without.mortality[nrow(output.without.mortality),"AF_US"] + 
                                             output.without.mortality[nrow(output.without.mortality),"LF_US"],
                                           Ovip.female.pop = output.without.mortality[nrow(output.without.mortality),"AO_U"] + output.without.mortality[nrow(output.without.mortality),"AO_T"],
                                           H_total = output.without.mortality[nrow(output.without.mortality),"H_UC"] + 
                                             output.without.mortality[nrow(output.without.mortality),"H_TC"] +
                                             output.without.mortality[nrow(output.without.mortality),"H_UI"] +
                                             output.without.mortality[nrow(output.without.mortality),"H_TI"] +
                                             output.without.mortality[nrow(output.without.mortality),"H_US"] +
                                             output.without.mortality[nrow(output.without.mortality),"H_TS"],
                                           
                                           H_susceptible = output.without.mortality[nrow(output.without.mortality),"H_US"] + output.without.mortality[nrow(output.without.mortality),"H_TS"],
                                           H_acute = output.without.mortality[nrow(output.without.mortality),"H_UI"] + output.without.mortality[nrow(output.without.mortality),"H_TI"],
                                           H_acute_prev = ((output.without.mortality[nrow(output.without.mortality),"H_UI"] + 
                                                              output.without.mortality[nrow(output.without.mortality),"H_TI"])/(output.without.mortality[nrow(output.without.mortality),"H_UC"] +
                                                                                                                                  output.without.mortality[nrow(output.without.mortality),"H_TC"] +
                                                                                                                                  output.without.mortality[nrow(output.without.mortality),"H_UI"] +
                                                                                                                                  output.without.mortality[nrow(output.without.mortality),"H_TI"] +
                                                                                                                                  output.without.mortality[nrow(output.without.mortality),"H_US"] +
                                                                                                                                  output.without.mortality[nrow(output.without.mortality),"H_TS"]))*100,
                                           H_carrier = output.without.mortality[nrow(output.without.mortality),"H_UC"] + output.without.mortality[nrow(output.without.mortality),"H_TC"],
                                           H_I = output.without.mortality[nrow(output.without.mortality),"H_UC"] + 
                                             output.without.mortality[nrow(output.without.mortality),"H_TC"] +
                                             output.without.mortality[nrow(output.without.mortality),"H_UI"] + 
                                             output.without.mortality[nrow(output.without.mortality),"H_TI"],
                                           
                                           H_I_prev = ((output.without.mortality[nrow(output.without.mortality),"H_UC"] +
                                                          output.without.mortality[nrow(output.without.mortality),"H_TC"] +
                                                          output.without.mortality[nrow(output.without.mortality),"H_UI"] +
                                                          output.without.mortality[nrow(output.without.mortality),"H_TI"])/(output.without.mortality[nrow(output.without.mortality),"H_UC"] +
                                                                                                                              output.without.mortality[nrow(output.without.mortality),"H_TC"] +
                                                                                                                              output.without.mortality[nrow(output.without.mortality),"H_UI"] +
                                                                                                                              output.without.mortality[nrow(output.without.mortality),"H_TI"] +
                                                                                                                              output.without.mortality[nrow(output.without.mortality),"H_US"] +
                                                                                                                              output.without.mortality[nrow(output.without.mortality),"H_TS"]))*100,
                                           Q_I_adult = output.without.mortality[nrow(output.without.mortality),"AQ_UI"],
                                           Q_I_nymph = output.without.mortality[nrow(output.without.mortality),"NQ_UI"],
                                           Q_I_adult_prev = ((output.without.mortality[nrow(output.without.mortality),"AQ_UI"])/(output.without.mortality[nrow(output.without.mortality),"AQ_UI"] +
                                                                                                                                   output.without.mortality[nrow(output.without.mortality),"AQ_US"]))*100,
                                           Q_I_nymph_prev = ((output.without.mortality[nrow(output.without.mortality),"NQ_UI"])/(output.without.mortality[nrow(output.without.mortality),"NQ_UI"] +
                                                                                                                                   output.without.mortality[nrow(output.without.mortality),"NQ_US"]))*100,
                                           F_I_adult = output.without.mortality[nrow(output.without.mortality),"AF_UI"],
                                           F_I_nymph = output.without.mortality[nrow(output.without.mortality),"NF_UI"],
                                           F_I_prev_adult = ((output.without.mortality[nrow(output.without.mortality),"AF_UI"])/(output.without.mortality[nrow(output.without.mortality),"AF_UI"] +
                                                                                                                                   output.without.mortality[nrow(output.without.mortality),"AF_US"]))*100,
                                           F_I_prev_nymph = ((output.without.mortality[nrow(output.without.mortality),"NF_UI"])/(output.without.mortality[nrow(output.without.mortality),"NF_UI"] + 
                                                                                                                                   output.without.mortality[nrow(output.without.mortality),"NF_US"]))*100,
                                           tick.pop = (output.without.mortality[nrow(output.without.mortality),"LQ_US"] +
                                                         output.without.mortality[nrow(output.without.mortality),"LF_US"] +
                                                         output.without.mortality[nrow(output.without.mortality),"LD_US"] +
                                                         output.without.mortality[nrow(output.without.mortality),"LD_TS"] +
                                                         output.without.mortality[nrow(output.without.mortality),"LD_UI"] +
                                                         output.without.mortality[nrow(output.without.mortality),"LD_TI"] +
                                                         output.without.mortality[nrow(output.without.mortality),"NQ_US"] +
                                                         output.without.mortality[nrow(output.without.mortality),"NQ_UI"] +
                                                         output.without.mortality[nrow(output.without.mortality),"NF_US"] +
                                                         output.without.mortality[nrow(output.without.mortality),"NF_UI"] +
                                                         output.without.mortality[nrow(output.without.mortality),"ND_US"] +
                                                         output.without.mortality[nrow(output.without.mortality),"ND_TS"] +
                                                         output.without.mortality[nrow(output.without.mortality),"ND_UI"] +
                                                         output.without.mortality[nrow(output.without.mortality),"ND_TI"] +
                                                         output.without.mortality[nrow(output.without.mortality),"AQ_US"] +
                                                         output.without.mortality[nrow(output.without.mortality),"AQ_UI"] +
                                                         output.without.mortality[nrow(output.without.mortality),"AF_US"] +
                                                         output.without.mortality[nrow(output.without.mortality),"AF_UI"] +
                                                         output.without.mortality[nrow(output.without.mortality),"AD_U"] +
                                                         output.without.mortality[nrow(output.without.mortality),"AD_T"] +
                                                         output.without.mortality[nrow(output.without.mortality),"AO_U"] +
                                                         output.without.mortality[nrow(output.without.mortality),"AO_T"]),
                                           T_H_ratio = (output.without.mortality[nrow(output.without.mortality),"LF_US"] +
                                                          output.without.mortality[nrow(output.without.mortality),"NF_US"] +
                                                          output.without.mortality[nrow(output.without.mortality),"NF_UI"] +
                                                          output.without.mortality[nrow(output.without.mortality),"AF_US"] +
                                                          output.without.mortality[nrow(output.without.mortality),"AF_UI"])/(output.without.mortality[nrow(output.without.mortality),"H_US"] +
                                                                                                                               output.without.mortality[nrow(output.without.mortality),"H_TS"] +
                                                                                                                               output.without.mortality[nrow(output.without.mortality),"H_UC"] + 
                                                                                                                               output.without.mortality[nrow(output.without.mortality),"H_TC"] +
                                                                                                                               output.without.mortality[nrow(output.without.mortality),"H_UI"] + 
                                                                                                                               output.without.mortality[nrow(output.without.mortality),"H_TI"]),
                                           T_H_ratio_2 = (output.without.mortality[nrow(output.without.mortality),"LQ_US"] +
                                                            output.without.mortality[nrow(output.without.mortality),"LF_US"] +
                                                            output.without.mortality[nrow(output.without.mortality),"LD_US"] +
                                                            output.without.mortality[nrow(output.without.mortality),"LD_TS"] +
                                                            output.without.mortality[nrow(output.without.mortality),"LD_UI"] +
                                                            output.without.mortality[nrow(output.without.mortality),"LD_TI"] +
                                                            output.without.mortality[nrow(output.without.mortality),"NQ_US"] +
                                                            output.without.mortality[nrow(output.without.mortality),"NQ_UI"] +
                                                            output.without.mortality[nrow(output.without.mortality),"NF_US"] +
                                                            output.without.mortality[nrow(output.without.mortality),"NF_UI"] +
                                                            output.without.mortality[nrow(output.without.mortality),"ND_US"] +
                                                            output.without.mortality[nrow(output.without.mortality),"ND_TS"] +
                                                            output.without.mortality[nrow(output.without.mortality),"ND_UI"] +
                                                            output.without.mortality[nrow(output.without.mortality),"ND_TI"] +
                                                            output.without.mortality[nrow(output.without.mortality),"AQ_US"] +
                                                            output.without.mortality[nrow(output.without.mortality),"AQ_UI"] +
                                                            output.without.mortality[nrow(output.without.mortality),"AF_US"] +
                                                            output.without.mortality[nrow(output.without.mortality),"AF_UI"] +
                                                            output.without.mortality[nrow(output.without.mortality),"AD_U"] +
                                                            output.without.mortality[nrow(output.without.mortality),"AD_T"] +
                                                            output.without.mortality[nrow(output.without.mortality),"AO_U"] +
                                                            output.without.mortality[nrow(output.without.mortality),"AO_T"])/(output.without.mortality[nrow(output.without.mortality),"H_US"] +
                                                                                                                                output.without.mortality[nrow(output.without.mortality),"H_TS"] +
                                                                                                                                output.without.mortality[nrow(output.without.mortality),"H_UC"] + 
                                                                                                                                output.without.mortality[nrow(output.without.mortality),"H_TC"] +
                                                                                                                                output.without.mortality[nrow(output.without.mortality),"H_UI"] + 
                                                                                                                                output.without.mortality[nrow(output.without.mortality),"H_TI"]),
                                           coverage = coverage_level,
                                           phi = treatment.interval)
    
    
    
    # Append the result row to the main results data frame
    
    results.without.mortality.df <- rbind(results.without.mortality.df, result.without.mortality)  
    
    
  }
}

## view the output data frame
#View(results.without.mortality.df) 




# (b). without reduced fecundity effect -----------------------------------

# Create an empty data frame with the necessary structure/columns ---------

results.without.fecundity.df <- data.frame(time=double(), # duration of simulation
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
                                           phi=factor()
)

# Nested loop for coverage levels and treatment intervals -----------------

for (coverage_level in coverage) {
  for (treatment.interval in phi) {
    print(coverage_level)
    print(treatment.interval)
    parameter <- parameter_fun(coverage_level, treatment.interval)
    parameter["TFR"] <- 0 # 36.9/100 ### updates parameter value
    
    
    # Run the ODE model function with the current coverage level and treatment interval
    output.without.fecundity <- run.model.by.treatment.interval.and.coverage(coverage_level, treatment.interval, time, initial.state, 
                                                                             param = parameter)
    
    output.without.fecundity <- as.data.frame(output.without.fecundity) # Convert model output to a data frame
    
    
    # Create a data frame row with the results
    
    alpha_N <- 1/20  # /day, nymph questing rate
    alpha_A <- 1/28  # /day, adult questing rate
    sigma = 1/15    #/day, rate of host recovery from disease #duration of infectiousness # the average time a cattle with the disease remains acutely infectious
    result.without.fecundity <- data.frame(time=time,
                                           incidence_in_cattle = ((output.without.fecundity[nrow(output.without.fecundity),"H_UI"] + 
                                                                     output.without.fecundity[nrow(output.without.fecundity),"H_TI"])/(output.without.fecundity[nrow(output.without.fecundity),"H_UC"] +
                                                                                                                                         output.without.fecundity[nrow(output.without.fecundity),"H_TC"] +
                                                                                                                                         output.without.fecundity[nrow(output.without.fecundity),"H_UI"] +
                                                                                                                                         output.without.fecundity[nrow(output.without.fecundity),"H_TI"] +
                                                                                                                                         output.without.fecundity[nrow(output.without.fecundity),"H_US"] +
                                                                                                                                         output.without.fecundity[nrow(output.without.fecundity),"H_TS"]))/sigma,
                                           biting_rate_nymph = alpha_N * ((output.without.fecundity[nrow(output.without.fecundity),"NQ_UI"] + 
                                                                             output.without.fecundity[nrow(output.without.fecundity),"NQ_US"])/(output.without.fecundity[nrow(output.without.fecundity),"H_US"] + 
                                                                                                                                                  output.without.fecundity[nrow(output.without.fecundity),"H_TS"] +
                                                                                                                                                  output.without.fecundity[nrow(output.without.fecundity),"H_UC"] + 
                                                                                                                                                  output.without.fecundity[nrow(output.without.fecundity),"H_TC"] +
                                                                                                                                                  output.without.fecundity[nrow(output.without.fecundity),"H_UI"] + 
                                                                                                                                                  output.without.fecundity[nrow(output.without.fecundity),"H_TI"])),
                                           biting_rate_infected_nymph= alpha_N * ((output.without.fecundity[nrow(output.without.fecundity),"NQ_UI"] + 
                                                                                     output.without.fecundity[nrow(output.without.fecundity),"NQ_US"])/(output.without.fecundity[nrow(output.without.fecundity),"H_US"] + 
                                                                                                                                                          output.without.fecundity[nrow(output.without.fecundity),"H_TS"] +
                                                                                                                                                          output.without.fecundity[nrow(output.without.fecundity),"H_UC"] + 
                                                                                                                                                          output.without.fecundity[nrow(output.without.fecundity),"H_TC"] +
                                                                                                                                                          output.without.fecundity[nrow(output.without.fecundity),"H_UI"] + 
                                                                                                                                                          output.without.fecundity[nrow(output.without.fecundity),"H_TI"])) *
                                             ((output.without.fecundity[nrow(output.without.fecundity),"NQ_UI"])/(output.without.fecundity[nrow(output.without.fecundity),"NQ_UI"] + 
                                                                                                                    output.without.fecundity[nrow(output.without.fecundity),"NQ_US"])),
                                           biting_rate_nymph_treated_cattle= alpha_N * ((output.without.fecundity[nrow(output.without.fecundity),"NQ_UI"] + 
                                                                                           output.without.fecundity[nrow(output.without.fecundity),"NQ_US"])/(output.without.fecundity[nrow(output.without.fecundity),"H_US"] + 
                                                                                                                                                                output.without.fecundity[nrow(output.without.fecundity),"H_TS"] +
                                                                                                                                                                output.without.fecundity[nrow(output.without.fecundity),"H_UC"] + 
                                                                                                                                                                output.without.fecundity[nrow(output.without.fecundity),"H_TC"] +
                                                                                                                                                                output.without.fecundity[nrow(output.without.fecundity),"H_UI"] + 
                                                                                                                                                                output.without.fecundity[nrow(output.without.fecundity),"H_TI"])) *
                                             ((output.without.fecundity[nrow(output.without.fecundity),"H_TS"] +
                                                 output.without.fecundity[nrow(output.without.fecundity),"H_TC"] +
                                                 output.without.fecundity[nrow(output.without.fecundity),"H_TI"])/(output.without.fecundity[nrow(output.without.fecundity),"H_US"] + 
                                                                                                                     output.without.fecundity[nrow(output.without.fecundity),"H_TS"] +
                                                                                                                     output.without.fecundity[nrow(output.without.fecundity),"H_UC"] + 
                                                                                                                     output.without.fecundity[nrow(output.without.fecundity),"H_TC"] +
                                                                                                                     output.without.fecundity[nrow(output.without.fecundity),"H_UI"] + 
                                                                                                                     output.without.fecundity[nrow(output.without.fecundity),"H_TI"])),
                                           biting_rate_nymph_untreated_cattle=alpha_N * ((output.without.fecundity[nrow(output.without.fecundity),"NQ_UI"] + 
                                                                                            output.without.fecundity[nrow(output.without.fecundity),"NQ_US"])/(output.without.fecundity[nrow(output.without.fecundity),"H_US"] + 
                                                                                                                                                                 output.without.fecundity[nrow(output.without.fecundity),"H_TS"] +
                                                                                                                                                                 output.without.fecundity[nrow(output.without.fecundity),"H_UC"] + 
                                                                                                                                                                 output.without.fecundity[nrow(output.without.fecundity),"H_TC"] +
                                                                                                                                                                 output.without.fecundity[nrow(output.without.fecundity),"H_UI"] + 
                                                                                                                                                                 output.without.fecundity[nrow(output.without.fecundity),"H_TI"])) *
                                             ((output.without.fecundity[nrow(output.without.fecundity),"H_US"] + 
                                                 output.without.fecundity[nrow(output.without.fecundity),"H_UC"] + 
                                                 output.without.fecundity[nrow(output.without.fecundity),"H_UI"])/(output.without.fecundity[nrow(output.without.fecundity),"H_US"] + 
                                                                                                                     output.without.fecundity[nrow(output.without.fecundity),"H_TS"] +
                                                                                                                     output.without.fecundity[nrow(output.without.fecundity),"H_UC"] + 
                                                                                                                     output.without.fecundity[nrow(output.without.fecundity),"H_TC"] +
                                                                                                                     output.without.fecundity[nrow(output.without.fecundity),"H_UI"] + 
                                                                                                                     output.without.fecundity[nrow(output.without.fecundity),"H_TI"])),
                                           biting_rate_adult = alpha_A * ((output.without.fecundity[nrow(output.without.fecundity),"AQ_UI"] + 
                                                                             output.without.fecundity[nrow(output.without.fecundity),"AQ_US"])/(output.without.fecundity[nrow(output.without.fecundity),"H_US"] + 
                                                                                                                                                  output.without.fecundity[nrow(output.without.fecundity),"H_TS"] +
                                                                                                                                                  output.without.fecundity[nrow(output.without.fecundity),"H_UC"] + 
                                                                                                                                                  output.without.fecundity[nrow(output.without.fecundity),"H_TC"] +
                                                                                                                                                  output.without.fecundity[nrow(output.without.fecundity),"H_UI"] + 
                                                                                                                                                  output.without.fecundity[nrow(output.without.fecundity),"H_TI"])),
                                           biting_rate_infected_adult= alpha_A * ((output.without.fecundity[nrow(output.without.fecundity),"AQ_UI"] + 
                                                                                     output.without.fecundity[nrow(output.without.fecundity),"AQ_US"])/(output.without.fecundity[nrow(output.without.fecundity),"H_US"] + 
                                                                                                                                                          output.without.fecundity[nrow(output.without.fecundity),"H_TS"] +
                                                                                                                                                          output.without.fecundity[nrow(output.without.fecundity),"H_UC"] + 
                                                                                                                                                          output.without.fecundity[nrow(output.without.fecundity),"H_TC"] +
                                                                                                                                                          output.without.fecundity[nrow(output.without.fecundity),"H_UI"] + 
                                                                                                                                                          output.without.fecundity[nrow(output.without.fecundity),"H_TI"])) *
                                             ((output.without.fecundity[nrow(output.without.fecundity),"AQ_UI"])/(output.without.fecundity[nrow(output.without.fecundity),"AQ_UI"] + 
                                                                                                                    output.without.fecundity[nrow(output.without.fecundity),"AQ_US"])),
                                           biting_rate_adult_treated_cattle= alpha_A * ((output.without.fecundity[nrow(output.without.fecundity),"AQ_UI"] + 
                                                                                           output.without.fecundity[nrow(output.without.fecundity),"AQ_US"])/(output.without.fecundity[nrow(output.without.fecundity),"H_US"] + 
                                                                                                                                                                output.without.fecundity[nrow(output.without.fecundity),"H_TS"] +
                                                                                                                                                                output.without.fecundity[nrow(output.without.fecundity),"H_UC"] + 
                                                                                                                                                                output.without.fecundity[nrow(output.without.fecundity),"H_TC"] +
                                                                                                                                                                output.without.fecundity[nrow(output.without.fecundity),"H_UI"] + 
                                                                                                                                                                output.without.fecundity[nrow(output.without.fecundity),"H_TI"])) *
                                             ((output.without.fecundity[nrow(output.without.fecundity),"H_TS"] +
                                                 output.without.fecundity[nrow(output.without.fecundity),"H_TC"] +
                                                 output.without.fecundity[nrow(output.without.fecundity),"H_TI"])/(output.without.fecundity[nrow(output.without.fecundity),"H_US"] + 
                                                                                                                     output.without.fecundity[nrow(output.without.fecundity),"H_TS"] +
                                                                                                                     output.without.fecundity[nrow(output.without.fecundity),"H_UC"] + 
                                                                                                                     output.without.fecundity[nrow(output.without.fecundity),"H_TC"] +
                                                                                                                     output.without.fecundity[nrow(output.without.fecundity),"H_UI"] + 
                                                                                                                     output.without.fecundity[nrow(output.without.fecundity),"H_TI"])),
                                           biting_rate_adult_untreated_cattle= alpha_A * ((output.without.fecundity[nrow(output.without.fecundity),"AQ_UI"] + 
                                                                                             output.without.fecundity[nrow(output.without.fecundity),"AQ_US"])/(output.without.fecundity[nrow(output.without.fecundity),"H_US"] + 
                                                                                                                                                                  output.without.fecundity[nrow(output.without.fecundity),"H_TS"] +
                                                                                                                                                                  output.without.fecundity[nrow(output.without.fecundity),"H_UC"] + 
                                                                                                                                                                  output.without.fecundity[nrow(output.without.fecundity),"H_TC"] +
                                                                                                                                                                  output.without.fecundity[nrow(output.without.fecundity),"H_UI"] + 
                                                                                                                                                                  output.without.fecundity[nrow(output.without.fecundity),"H_TI"])) * 
                                             ((output.without.fecundity[nrow(output.without.fecundity),"H_US"] + 
                                                 output.without.fecundity[nrow(output.without.fecundity),"H_UC"] + 
                                                 output.without.fecundity[nrow(output.without.fecundity),"H_UI"])/(output.without.fecundity[nrow(output.without.fecundity),"H_US"] + 
                                                                                                                     output.without.fecundity[nrow(output.without.fecundity),"H_TS"] +
                                                                                                                     output.without.fecundity[nrow(output.without.fecundity),"H_UC"] + 
                                                                                                                     output.without.fecundity[nrow(output.without.fecundity),"H_TC"] +
                                                                                                                     output.without.fecundity[nrow(output.without.fecundity),"H_UI"] + 
                                                                                                                     output.without.fecundity[nrow(output.without.fecundity),"H_TI"])),
                                           adultick_host_ratio=(output.without.fecundity[nrow(output.without.fecundity),"AF_US"] +
                                                                  output.without.fecundity[nrow(output.without.fecundity),"AF_UI"])/(output.without.fecundity[nrow(output.without.fecundity),"H_US"] + 
                                                                                                                                       output.without.fecundity[nrow(output.without.fecundity),"H_TS"] +
                                                                                                                                       output.without.fecundity[nrow(output.without.fecundity),"H_UC"] + 
                                                                                                                                       output.without.fecundity[nrow(output.without.fecundity),"H_TC"] +
                                                                                                                                       output.without.fecundity[nrow(output.without.fecundity),"H_UI"] + 
                                                                                                                                       output.without.fecundity[nrow(output.without.fecundity),"H_TI"]),
                                           nymph_host_ratio=(output.without.fecundity[nrow(output.without.fecundity),"NF_US"] +
                                                               output.without.fecundity[nrow(output.without.fecundity),"NF_UI"])/(output.without.fecundity[nrow(output.without.fecundity),"H_US"] + 
                                                                                                                                    output.without.fecundity[nrow(output.without.fecundity),"H_TS"] +
                                                                                                                                    output.without.fecundity[nrow(output.without.fecundity),"H_UC"] + 
                                                                                                                                    output.without.fecundity[nrow(output.without.fecundity),"H_TC"] +
                                                                                                                                    output.without.fecundity[nrow(output.without.fecundity),"H_UI"] + 
                                                                                                                                    output.without.fecundity[nrow(output.without.fecundity),"H_TI"]),
                                           Egg.pop = output.without.fecundity[nrow(output.without.fecundity),"E_US"] + output.without.fecundity[nrow(output.without.fecundity),"E_TS"],
                                           Quest.pop = output.without.fecundity[nrow(output.without.fecundity),"AQ_UI"] + output.without.fecundity[nrow(output.without.fecundity),"AQ_US"] +
                                             output.without.fecundity[nrow(output.without.fecundity),"NQ_UI"] + output.without.fecundity[nrow(output.without.fecundity),"NQ_US"] +
                                             output.without.fecundity[nrow(output.without.fecundity),"LQ_US"],
                                           Feed.pop = output.without.fecundity[nrow(output.without.fecundity),"NF_UI"] + output.without.fecundity[nrow(output.without.fecundity),"NF_US"] +
                                             output.without.fecundity[nrow(output.without.fecundity),"AF_UI"] + output.without.fecundity[nrow(output.without.fecundity),"AF_US"] + 
                                             output.without.fecundity[nrow(output.without.fecundity),"LF_US"],
                                           Ovip.female.pop = output.without.fecundity[nrow(output.without.fecundity),"AO_U"] + output.without.fecundity[nrow(output.without.fecundity),"AO_T"],
                                           H_total = output.without.fecundity[nrow(output.without.fecundity),"H_UC"] + 
                                             output.without.fecundity[nrow(output.without.fecundity),"H_TC"] +
                                             output.without.fecundity[nrow(output.without.fecundity),"H_UI"] +
                                             output.without.fecundity[nrow(output.without.fecundity),"H_TI"] +
                                             output.without.fecundity[nrow(output.without.fecundity),"H_US"] +
                                             output.without.fecundity[nrow(output.without.fecundity),"H_TS"],
                                           
                                           H_susceptible = output.without.fecundity[nrow(output.without.fecundity),"H_US"] + output.without.fecundity[nrow(output.without.fecundity),"H_TS"],
                                           H_acute = output.without.fecundity[nrow(output.without.fecundity),"H_UI"] + output.without.fecundity[nrow(output.without.fecundity),"H_TI"],
                                           H_acute_prev = ((output.without.fecundity[nrow(output.without.fecundity),"H_UI"] + 
                                                              output.without.fecundity[nrow(output.without.fecundity),"H_TI"])/(output.without.fecundity[nrow(output.without.fecundity),"H_UC"] +
                                                                                                                                  output.without.fecundity[nrow(output.without.fecundity),"H_TC"] +
                                                                                                                                  output.without.fecundity[nrow(output.without.fecundity),"H_UI"] +
                                                                                                                                  output.without.fecundity[nrow(output.without.fecundity),"H_TI"] +
                                                                                                                                  output.without.fecundity[nrow(output.without.fecundity),"H_US"] +
                                                                                                                                  output.without.fecundity[nrow(output.without.fecundity),"H_TS"]))*100,
                                           H_carrier = output.without.fecundity[nrow(output.without.fecundity),"H_UC"] + output.without.fecundity[nrow(output.without.fecundity),"H_TC"],
                                           H_I = output.without.fecundity[nrow(output.without.fecundity),"H_UC"] + 
                                             output.without.fecundity[nrow(output.without.fecundity),"H_TC"] +
                                             output.without.fecundity[nrow(output.without.fecundity),"H_UI"] + 
                                             output.without.fecundity[nrow(output.without.fecundity),"H_TI"],
                                           
                                           H_I_prev = ((output.without.fecundity[nrow(output.without.fecundity),"H_UC"] +
                                                          output.without.fecundity[nrow(output.without.fecundity),"H_TC"] +
                                                          output.without.fecundity[nrow(output.without.fecundity),"H_UI"] +
                                                          output.without.fecundity[nrow(output.without.fecundity),"H_TI"])/(output.without.fecundity[nrow(output.without.fecundity),"H_UC"] +
                                                                                                                              output.without.fecundity[nrow(output.without.fecundity),"H_TC"] +
                                                                                                                              output.without.fecundity[nrow(output.without.fecundity),"H_UI"] +
                                                                                                                              output.without.fecundity[nrow(output.without.fecundity),"H_TI"] +
                                                                                                                              output.without.fecundity[nrow(output.without.fecundity),"H_US"] +
                                                                                                                              output.without.fecundity[nrow(output.without.fecundity),"H_TS"]))*100,
                                           Q_I_adult = output.without.fecundity[nrow(output.without.fecundity),"AQ_UI"],
                                           Q_I_nymph = output.without.fecundity[nrow(output.without.fecundity),"NQ_UI"],
                                           Q_I_adult_prev = ((output.without.fecundity[nrow(output.without.fecundity),"AQ_UI"])/(output.without.fecundity[nrow(output.without.fecundity),"AQ_UI"] +
                                                                                                                                   output.without.fecundity[nrow(output.without.fecundity),"AQ_US"]))*100,
                                           Q_I_nymph_prev = ((output.without.fecundity[nrow(output.without.fecundity),"NQ_UI"])/(output.without.fecundity[nrow(output.without.fecundity),"NQ_UI"] +
                                                                                                                                   output.without.fecundity[nrow(output.without.fecundity),"NQ_US"]))*100,
                                           F_I_adult = output.without.fecundity[nrow(output.without.fecundity),"AF_UI"],
                                           F_I_nymph = output.without.fecundity[nrow(output.without.fecundity),"NF_UI"],
                                           F_I_prev_adult = ((output.without.fecundity[nrow(output.without.fecundity),"AF_UI"])/(output.without.fecundity[nrow(output.without.fecundity),"AF_UI"] +
                                                                                                                                   output.without.fecundity[nrow(output.without.fecundity),"AF_US"]))*100,
                                           F_I_prev_nymph = ((output.without.fecundity[nrow(output.without.fecundity),"NF_UI"])/(output.without.fecundity[nrow(output.without.fecundity),"NF_UI"] + 
                                                                                                                                   output.without.fecundity[nrow(output.without.fecundity),"NF_US"]))*100,
                                           tick.pop = (output.without.fecundity[nrow(output.without.fecundity),"LQ_US"] +
                                                         output.without.fecundity[nrow(output.without.fecundity),"LF_US"] +
                                                         output.without.fecundity[nrow(output.without.fecundity),"LD_US"] +
                                                         output.without.fecundity[nrow(output.without.fecundity),"LD_TS"] +
                                                         output.without.fecundity[nrow(output.without.fecundity),"LD_UI"] +
                                                         output.without.fecundity[nrow(output.without.fecundity),"LD_TI"] +
                                                         output.without.fecundity[nrow(output.without.fecundity),"NQ_US"] +
                                                         output.without.fecundity[nrow(output.without.fecundity),"NQ_UI"] +
                                                         output.without.fecundity[nrow(output.without.fecundity),"NF_US"] +
                                                         output.without.fecundity[nrow(output.without.fecundity),"NF_UI"] +
                                                         output.without.fecundity[nrow(output.without.fecundity),"ND_US"] +
                                                         output.without.fecundity[nrow(output.without.fecundity),"ND_TS"] +
                                                         output.without.fecundity[nrow(output.without.fecundity),"ND_UI"] +
                                                         output.without.fecundity[nrow(output.without.fecundity),"ND_TI"] +
                                                         output.without.fecundity[nrow(output.without.fecundity),"AQ_US"] +
                                                         output.without.fecundity[nrow(output.without.fecundity),"AQ_UI"] +
                                                         output.without.fecundity[nrow(output.without.fecundity),"AF_US"] +
                                                         output.without.fecundity[nrow(output.without.fecundity),"AF_UI"] +
                                                         output.without.fecundity[nrow(output.without.fecundity),"AD_U"] +
                                                         output.without.fecundity[nrow(output.without.fecundity),"AD_T"] +
                                                         output.without.fecundity[nrow(output.without.fecundity),"AO_U"] +
                                                         output.without.fecundity[nrow(output.without.fecundity),"AO_T"]),
                                           T_H_ratio = (output.without.fecundity[nrow(output.without.fecundity),"LF_US"] +
                                                          output.without.fecundity[nrow(output.without.fecundity),"NF_US"] +
                                                          output.without.fecundity[nrow(output.without.fecundity),"NF_UI"] +
                                                          output.without.fecundity[nrow(output.without.fecundity),"AF_US"] +
                                                          output.without.fecundity[nrow(output.without.fecundity),"AF_UI"])/(output.without.fecundity[nrow(output.without.fecundity),"H_US"] +
                                                                                                                               output.without.fecundity[nrow(output.without.fecundity),"H_TS"] +
                                                                                                                               output.without.fecundity[nrow(output.without.fecundity),"H_UC"] + 
                                                                                                                               output.without.fecundity[nrow(output.without.fecundity),"H_TC"] +
                                                                                                                               output.without.fecundity[nrow(output.without.fecundity),"H_UI"] + 
                                                                                                                               output.without.fecundity[nrow(output.without.fecundity),"H_TI"]),
                                           T_H_ratio_2 = (output.without.fecundity[nrow(output.without.fecundity),"LQ_US"] +
                                                            output.without.fecundity[nrow(output.without.fecundity),"LF_US"] +
                                                            output.without.fecundity[nrow(output.without.fecundity),"LD_US"] +
                                                            output.without.fecundity[nrow(output.without.fecundity),"LD_TS"] +
                                                            output.without.fecundity[nrow(output.without.fecundity),"LD_UI"] +
                                                            output.without.fecundity[nrow(output.without.fecundity),"LD_TI"] +
                                                            output.without.fecundity[nrow(output.without.fecundity),"NQ_US"] +
                                                            output.without.fecundity[nrow(output.without.fecundity),"NQ_UI"] +
                                                            output.without.fecundity[nrow(output.without.fecundity),"NF_US"] +
                                                            output.without.fecundity[nrow(output.without.fecundity),"NF_UI"] +
                                                            output.without.fecundity[nrow(output.without.fecundity),"ND_US"] +
                                                            output.without.fecundity[nrow(output.without.fecundity),"ND_TS"] +
                                                            output.without.fecundity[nrow(output.without.fecundity),"ND_UI"] +
                                                            output.without.fecundity[nrow(output.without.fecundity),"ND_TI"] +
                                                            output.without.fecundity[nrow(output.without.fecundity),"AQ_US"] +
                                                            output.without.fecundity[nrow(output.without.fecundity),"AQ_UI"] +
                                                            output.without.fecundity[nrow(output.without.fecundity),"AF_US"] +
                                                            output.without.fecundity[nrow(output.without.fecundity),"AF_UI"] +
                                                            output.without.fecundity[nrow(output.without.fecundity),"AD_U"] +
                                                            output.without.fecundity[nrow(output.without.fecundity),"AD_T"] +
                                                            output.without.fecundity[nrow(output.without.fecundity),"AO_U"] +
                                                            output.without.fecundity[nrow(output.without.fecundity),"AO_T"])/(output.without.fecundity[nrow(output.without.fecundity),"H_US"] +
                                                                                                                                output.without.fecundity[nrow(output.without.fecundity),"H_TS"] +
                                                                                                                                output.without.fecundity[nrow(output.without.fecundity),"H_UC"] + 
                                                                                                                                output.without.fecundity[nrow(output.without.fecundity),"H_TC"] +
                                                                                                                                output.without.fecundity[nrow(output.without.fecundity),"H_UI"] + 
                                                                                                                                output.without.fecundity[nrow(output.without.fecundity),"H_TI"]),
                                           coverage = coverage_level,
                                           phi = treatment.interval)
    
    
    
    # Append the result row to the main results data frame
    
    results.without.fecundity.df <- rbind(results.without.fecundity.df, result.without.fecundity)  
    
    
  }
}

## view the output data frame
#View(results.without.fecundity.df) 




# (c). without oviposition delay ------------------------------------------

# Create an empty data frame with the necessary structure/columns ---------

results.without.oviposition.delay.df <- data.frame(time=double(), # duration of simulation
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
                                                   phi=factor()
)

# Nested loop for coverage levels and treatment intervals -----------------

for (coverage_level in coverage) {
  for (treatment.interval in phi) {
    print(coverage_level)
    print(treatment.interval)
    parameter <- parameter_fun(coverage_level, treatment.interval)
    parameter["tau_o"] <- 0 # 6  ### updates parameter value
    parameter["k_tau"] <- 0 # 2.3
    
    
    # Run the ODE model function with the current coverage level and treatment interval
    output.without.oviposition.delay <- run.model.by.treatment.interval.and.coverage(coverage_level, treatment.interval, time, initial.state, 
                                                                                     param = parameter)
    
    output.without.oviposition.delay <- as.data.frame(output.without.oviposition.delay) # Convert model output to a data frame
    

    # Create a data frame row with the results
    
    alpha_N <- 1/20  # /day, nymph questing rate
    alpha_A <- 1/28  # /day, adult questing rate
    sigma = 1/15    #/day, rate of host recovery from disease #duration of infectiousness # the average time a cattle with the disease remains acutely infectious
    result.without.oviposition.delay <- data.frame(time=time,
                                                   incidence_in_cattle = ((output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"] + 
                                                                             output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"])/(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UC"] +
                                                                                                                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TC"] +
                                                                                                                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"] +
                                                                                                                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"] +
                                                                                                                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_US"] +
                                                                                                                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TS"]))/sigma,
                                                   biting_rate_nymph = alpha_N * ((output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NQ_UI"] + 
                                                                                     output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NQ_US"])/(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_US"] + 
                                                                                                                                                                          output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TS"] +
                                                                                                                                                                          output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UC"] + 
                                                                                                                                                                          output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TC"] +
                                                                                                                                                                          output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"] + 
                                                                                                                                                                          output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"])),
                                                   biting_rate_infected_nymph= alpha_N * ((output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NQ_UI"] + 
                                                                                             output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NQ_US"])/(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_US"] + 
                                                                                                                                                                                  output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TS"] +
                                                                                                                                                                                  output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UC"] + 
                                                                                                                                                                                  output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TC"] +
                                                                                                                                                                                  output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"] + 
                                                                                                                                                                                  output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"])) *
                                                     ((output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NQ_UI"])/(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NQ_UI"] + 
                                                                                                                                            output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NQ_US"])),
                                                   biting_rate_nymph_treated_cattle= alpha_N * ((output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NQ_UI"] + 
                                                                                                   output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NQ_US"])/(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_US"] + 
                                                                                                                                                                                        output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TS"] +
                                                                                                                                                                                        output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UC"] + 
                                                                                                                                                                                        output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TC"] +
                                                                                                                                                                                        output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"] + 
                                                                                                                                                                                        output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"])) *
                                                     ((output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TS"] +
                                                         output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TC"] +
                                                         output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"])/(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_US"] + 
                                                                                                                                             output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TS"] +
                                                                                                                                             output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UC"] + 
                                                                                                                                             output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TC"] +
                                                                                                                                             output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"] + 
                                                                                                                                             output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"])),
                                                   biting_rate_nymph_untreated_cattle=alpha_N * ((output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NQ_UI"] + 
                                                                                                    output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NQ_US"])/(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_US"] + 
                                                                                                                                                                                         output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TS"] +
                                                                                                                                                                                         output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UC"] + 
                                                                                                                                                                                         output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TC"] +
                                                                                                                                                                                         output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"] + 
                                                                                                                                                                                         output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"])) *
                                                     ((output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_US"] + 
                                                         output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UC"] + 
                                                         output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"])/(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_US"] + 
                                                                                                                                             output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TS"] +
                                                                                                                                             output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UC"] + 
                                                                                                                                             output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TC"] +
                                                                                                                                             output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"] + 
                                                                                                                                             output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"])),
                                                   biting_rate_adult = alpha_A * ((output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AQ_UI"] + 
                                                                                     output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AQ_US"])/(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_US"] + 
                                                                                                                                                                          output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TS"] +
                                                                                                                                                                          output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UC"] + 
                                                                                                                                                                          output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TC"] +
                                                                                                                                                                          output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"] + 
                                                                                                                                                                          output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"])),
                                                   biting_rate_infected_adult= alpha_A * ((output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AQ_UI"] + 
                                                                                             output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AQ_US"])/(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_US"] + 
                                                                                                                                                                                  output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TS"] +
                                                                                                                                                                                  output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UC"] + 
                                                                                                                                                                                  output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TC"] +
                                                                                                                                                                                  output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"] + 
                                                                                                                                                                                  output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"])) *
                                                     ((output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AQ_UI"])/(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AQ_UI"] + 
                                                                                                                                            output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AQ_US"])),
                                                   biting_rate_adult_treated_cattle= alpha_A * ((output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AQ_UI"] + 
                                                                                                   output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AQ_US"])/(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_US"] + 
                                                                                                                                                                                        output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TS"] +
                                                                                                                                                                                        output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UC"] + 
                                                                                                                                                                                        output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TC"] +
                                                                                                                                                                                        output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"] + 
                                                                                                                                                                                        output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"])) *
                                                     ((output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TS"] +
                                                         output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TC"] +
                                                         output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"])/(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_US"] + 
                                                                                                                                             output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TS"] +
                                                                                                                                             output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UC"] + 
                                                                                                                                             output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TC"] +
                                                                                                                                             output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"] + 
                                                                                                                                             output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"])),
                                                   biting_rate_adult_untreated_cattle= alpha_A * ((output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AQ_UI"] + 
                                                                                                     output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AQ_US"])/(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_US"] + 
                                                                                                                                                                                          output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TS"] +
                                                                                                                                                                                          output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UC"] + 
                                                                                                                                                                                          output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TC"] +
                                                                                                                                                                                          output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"] + 
                                                                                                                                                                                          output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"])) * 
                                                     ((output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_US"] + 
                                                         output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UC"] + 
                                                         output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"])/(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_US"] + 
                                                                                                                                             output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TS"] +
                                                                                                                                             output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UC"] + 
                                                                                                                                             output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TC"] +
                                                                                                                                             output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"] + 
                                                                                                                                             output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"])),
                                                   adultick_host_ratio=(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AF_US"] +
                                                                          output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AF_UI"])/(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_US"] + 
                                                                                                                                                               output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TS"] +
                                                                                                                                                               output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UC"] + 
                                                                                                                                                               output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TC"] +
                                                                                                                                                               output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"] + 
                                                                                                                                                               output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"]),
                                                   nymph_host_ratio=(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NF_US"] +
                                                                       output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NF_UI"])/(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_US"] + 
                                                                                                                                                            output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TS"] +
                                                                                                                                                            output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UC"] + 
                                                                                                                                                            output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TC"] +
                                                                                                                                                            output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"] + 
                                                                                                                                                            output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"]),
                                                   Egg.pop = output.without.oviposition.delay[nrow(output.without.oviposition.delay),"E_US"] + output.without.oviposition.delay[nrow(output.without.oviposition.delay),"E_TS"],
                                                   Quest.pop = output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AQ_UI"] + output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AQ_US"] +
                                                     output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NQ_UI"] + output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NQ_US"] +
                                                     output.without.oviposition.delay[nrow(output.without.oviposition.delay),"LQ_US"],
                                                   Feed.pop = output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NF_UI"] + output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NF_US"] +
                                                     output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AF_UI"] + output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AF_US"] + 
                                                     output.without.oviposition.delay[nrow(output.without.oviposition.delay),"LF_US"],
                                                   Ovip.female.pop = output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AO_U"] + output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AO_T"],
                                                   H_total = output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UC"] + 
                                                     output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TC"] +
                                                     output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"] +
                                                     output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"] +
                                                     output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_US"] +
                                                     output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TS"],
                                                   
                                                   H_susceptible = output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_US"] + output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TS"],
                                                   H_acute = output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"] + output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"],
                                                   H_acute_prev = ((output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"] + 
                                                                      output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"])/(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UC"] +
                                                                                                                                                          output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TC"] +
                                                                                                                                                          output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"] +
                                                                                                                                                          output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"] +
                                                                                                                                                          output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_US"] +
                                                                                                                                                          output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TS"]))*100,
                                                   H_carrier = output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UC"] + output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TC"],
                                                   H_I = output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UC"] + 
                                                     output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TC"] +
                                                     output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"] + 
                                                     output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"],
                                                   
                                                   H_I_prev = ((output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UC"] +
                                                                  output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TC"] +
                                                                  output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"] +
                                                                  output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"])/(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UC"] +
                                                                                                                                                      output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TC"] +
                                                                                                                                                      output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"] +
                                                                                                                                                      output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"] +
                                                                                                                                                      output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_US"] +
                                                                                                                                                      output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TS"]))*100,
                                                   Q_I_adult = output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AQ_UI"],
                                                   Q_I_nymph = output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NQ_UI"],
                                                   Q_I_adult_prev = ((output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AQ_UI"])/(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AQ_UI"] +
                                                                                                                                                           output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AQ_US"]))*100,
                                                   Q_I_nymph_prev = ((output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NQ_UI"])/(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NQ_UI"] +
                                                                                                                                                           output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NQ_US"]))*100,
                                                   F_I_adult = output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AF_UI"],
                                                   F_I_nymph = output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NF_UI"],
                                                   F_I_prev_adult = ((output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AF_UI"])/(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AF_UI"] +
                                                                                                                                                           output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AF_US"]))*100,
                                                   F_I_prev_nymph = ((output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NF_UI"])/(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NF_UI"] + 
                                                                                                                                                           output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NF_US"]))*100,
                                                   tick.pop = (output.without.oviposition.delay[nrow(output.without.oviposition.delay),"LQ_US"] +
                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"LF_US"] +
                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"LD_US"] +
                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"LD_TS"] +
                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"LD_UI"] +
                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"LD_TI"] +
                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NQ_US"] +
                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NQ_UI"] +
                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NF_US"] +
                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NF_UI"] +
                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"ND_US"] +
                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"ND_TS"] +
                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"ND_UI"] +
                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"ND_TI"] +
                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AQ_US"] +
                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AQ_UI"] +
                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AF_US"] +
                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AF_UI"] +
                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AD_U"] +
                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AD_T"] +
                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AO_U"] +
                                                                 output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AO_T"]),
                                                   T_H_ratio = (output.without.oviposition.delay[nrow(output.without.oviposition.delay),"LF_US"] +
                                                                  output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NF_US"] +
                                                                  output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NF_UI"] +
                                                                  output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AF_US"] +
                                                                  output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AF_UI"])/(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_US"] +
                                                                                                                                                       output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TS"] +
                                                                                                                                                       output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UC"] + 
                                                                                                                                                       output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TC"] +
                                                                                                                                                       output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"] + 
                                                                                                                                                       output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"]),
                                                   T_H_ratio_2 = (output.without.oviposition.delay[nrow(output.without.oviposition.delay),"LQ_US"] +
                                                                    output.without.oviposition.delay[nrow(output.without.oviposition.delay),"LF_US"] +
                                                                    output.without.oviposition.delay[nrow(output.without.oviposition.delay),"LD_US"] +
                                                                    output.without.oviposition.delay[nrow(output.without.oviposition.delay),"LD_TS"] +
                                                                    output.without.oviposition.delay[nrow(output.without.oviposition.delay),"LD_UI"] +
                                                                    output.without.oviposition.delay[nrow(output.without.oviposition.delay),"LD_TI"] +
                                                                    output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NQ_US"] +
                                                                    output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NQ_UI"] +
                                                                    output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NF_US"] +
                                                                    output.without.oviposition.delay[nrow(output.without.oviposition.delay),"NF_UI"] +
                                                                    output.without.oviposition.delay[nrow(output.without.oviposition.delay),"ND_US"] +
                                                                    output.without.oviposition.delay[nrow(output.without.oviposition.delay),"ND_TS"] +
                                                                    output.without.oviposition.delay[nrow(output.without.oviposition.delay),"ND_UI"] +
                                                                    output.without.oviposition.delay[nrow(output.without.oviposition.delay),"ND_TI"] +
                                                                    output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AQ_US"] +
                                                                    output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AQ_UI"] +
                                                                    output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AF_US"] +
                                                                    output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AF_UI"] +
                                                                    output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AD_U"] +
                                                                    output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AD_T"] +
                                                                    output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AO_U"] +
                                                                    output.without.oviposition.delay[nrow(output.without.oviposition.delay),"AO_T"])/(output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_US"] +
                                                                                                                                                        output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TS"] +
                                                                                                                                                        output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UC"] + 
                                                                                                                                                        output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TC"] +
                                                                                                                                                        output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_UI"] + 
                                                                                                                                                        output.without.oviposition.delay[nrow(output.without.oviposition.delay),"H_TI"]),
                                                   coverage = coverage_level,
                                                   phi = treatment.interval)
    
    
    
    # Append the result row to the main results data frame
    
    results.without.oviposition.delay.df <- rbind(results.without.oviposition.delay.df, result.without.oviposition.delay)  
    
    
  }
}

## view the output data frame
#View(results.without.oviposition.delay.df) 



# (d). without increased feeding duration ---------------------------------

# Create an empty data frame with the necessary structure/columns ---------

results.without.feeding.df <- data.frame(time=double(), # duration of simulation
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
                                         phi=factor()
)

# Nested loop for coverage levels and treatment intervals -----------------

for (coverage_level in coverage) {
  for (treatment.interval in phi) {
    print(coverage_level)
    print(treatment.interval)
    parameter <- parameter_fun(coverage_level, treatment.interval)
    parameter["tau_L"] <- 0 # 1.9
    parameter["tau_N"] <- 0 # 2.3
    parameter["tau_A"] <- 0 # 3
    
    
    
    # Run the ODE model function with the current coverage level and treatment interval
    output.without.feeding <- run.model.by.treatment.interval.and.coverage(coverage_level, treatment.interval, time, initial.state, 
                                                                           param = parameter)
    
    output.without.feeding <- as.data.frame(output.without.feeding) # Convert model output to a data frame
    
    
    # Create a data frame row with the results
    
    alpha_N <- 1/20  # /day, nymph questing rate
    alpha_A <- 1/28  # /day, adult questing rate
    sigma = 1/15    #/day, rate of host recovery from disease #duration of infectiousness # the average time a cattle with the disease remains acutely infectious
    result.without.feeding <- data.frame(time=time,
                                         incidence_in_cattle = ((output.without.feeding[nrow(output.without.feeding),"H_UI"] + 
                                                                   output.without.feeding[nrow(output.without.feeding),"H_TI"])/(output.without.feeding[nrow(output.without.feeding),"H_UC"] +
                                                                                                                                   output.without.feeding[nrow(output.without.feeding),"H_TC"] +
                                                                                                                                   output.without.feeding[nrow(output.without.feeding),"H_UI"] +
                                                                                                                                   output.without.feeding[nrow(output.without.feeding),"H_TI"] +
                                                                                                                                   output.without.feeding[nrow(output.without.feeding),"H_US"] +
                                                                                                                                   output.without.feeding[nrow(output.without.feeding),"H_TS"]))/sigma,
                                         biting_rate_nymph = alpha_N * ((output.without.feeding[nrow(output.without.feeding),"NQ_UI"] + 
                                                                           output.without.feeding[nrow(output.without.feeding),"NQ_US"])/(output.without.feeding[nrow(output.without.feeding),"H_US"] + 
                                                                                                                                            output.without.feeding[nrow(output.without.feeding),"H_TS"] +
                                                                                                                                            output.without.feeding[nrow(output.without.feeding),"H_UC"] + 
                                                                                                                                            output.without.feeding[nrow(output.without.feeding),"H_TC"] +
                                                                                                                                            output.without.feeding[nrow(output.without.feeding),"H_UI"] + 
                                                                                                                                            output.without.feeding[nrow(output.without.feeding),"H_TI"])),
                                         biting_rate_infected_nymph= alpha_N * ((output.without.feeding[nrow(output.without.feeding),"NQ_UI"] + 
                                                                                   output.without.feeding[nrow(output.without.feeding),"NQ_US"])/(output.without.feeding[nrow(output.without.feeding),"H_US"] + 
                                                                                                                                                    output.without.feeding[nrow(output.without.feeding),"H_TS"] +
                                                                                                                                                    output.without.feeding[nrow(output.without.feeding),"H_UC"] + 
                                                                                                                                                    output.without.feeding[nrow(output.without.feeding),"H_TC"] +
                                                                                                                                                    output.without.feeding[nrow(output.without.feeding),"H_UI"] + 
                                                                                                                                                    output.without.feeding[nrow(output.without.feeding),"H_TI"])) *
                                           ((output.without.feeding[nrow(output.without.feeding),"NQ_UI"])/(output.without.feeding[nrow(output.without.feeding),"NQ_UI"] + 
                                                                                                              output.without.feeding[nrow(output.without.feeding),"NQ_US"])),
                                         biting_rate_nymph_treated_cattle= alpha_N * ((output.without.feeding[nrow(output.without.feeding),"NQ_UI"] + 
                                                                                         output.without.feeding[nrow(output.without.feeding),"NQ_US"])/(output.without.feeding[nrow(output.without.feeding),"H_US"] + 
                                                                                                                                                          output.without.feeding[nrow(output.without.feeding),"H_TS"] +
                                                                                                                                                          output.without.feeding[nrow(output.without.feeding),"H_UC"] + 
                                                                                                                                                          output.without.feeding[nrow(output.without.feeding),"H_TC"] +
                                                                                                                                                          output.without.feeding[nrow(output.without.feeding),"H_UI"] + 
                                                                                                                                                          output.without.feeding[nrow(output.without.feeding),"H_TI"])) *
                                           ((output.without.feeding[nrow(output.without.feeding),"H_TS"] +
                                               output.without.feeding[nrow(output.without.feeding),"H_TC"] +
                                               output.without.feeding[nrow(output.without.feeding),"H_TI"])/(output.without.feeding[nrow(output.without.feeding),"H_US"] + 
                                                                                                               output.without.feeding[nrow(output.without.feeding),"H_TS"] +
                                                                                                               output.without.feeding[nrow(output.without.feeding),"H_UC"] + 
                                                                                                               output.without.feeding[nrow(output.without.feeding),"H_TC"] +
                                                                                                               output.without.feeding[nrow(output.without.feeding),"H_UI"] + 
                                                                                                               output.without.feeding[nrow(output.without.feeding),"H_TI"])),
                                         biting_rate_nymph_untreated_cattle=alpha_N * ((output.without.feeding[nrow(output.without.feeding),"NQ_UI"] + 
                                                                                          output.without.feeding[nrow(output.without.feeding),"NQ_US"])/(output.without.feeding[nrow(output.without.feeding),"H_US"] + 
                                                                                                                                                           output.without.feeding[nrow(output.without.feeding),"H_TS"] +
                                                                                                                                                           output.without.feeding[nrow(output.without.feeding),"H_UC"] + 
                                                                                                                                                           output.without.feeding[nrow(output.without.feeding),"H_TC"] +
                                                                                                                                                           output.without.feeding[nrow(output.without.feeding),"H_UI"] + 
                                                                                                                                                           output.without.feeding[nrow(output.without.feeding),"H_TI"])) *
                                           ((output.without.feeding[nrow(output.without.feeding),"H_US"] + 
                                               output.without.feeding[nrow(output.without.feeding),"H_UC"] + 
                                               output.without.feeding[nrow(output.without.feeding),"H_UI"])/(output.without.feeding[nrow(output.without.feeding),"H_US"] + 
                                                                                                               output.without.feeding[nrow(output.without.feeding),"H_TS"] +
                                                                                                               output.without.feeding[nrow(output.without.feeding),"H_UC"] + 
                                                                                                               output.without.feeding[nrow(output.without.feeding),"H_TC"] +
                                                                                                               output.without.feeding[nrow(output.without.feeding),"H_UI"] + 
                                                                                                               output.without.feeding[nrow(output.without.feeding),"H_TI"])),
                                         biting_rate_adult = alpha_A * ((output.without.feeding[nrow(output.without.feeding),"AQ_UI"] + 
                                                                           output.without.feeding[nrow(output.without.feeding),"AQ_US"])/(output.without.feeding[nrow(output.without.feeding),"H_US"] + 
                                                                                                                                            output.without.feeding[nrow(output.without.feeding),"H_TS"] +
                                                                                                                                            output.without.feeding[nrow(output.without.feeding),"H_UC"] + 
                                                                                                                                            output.without.feeding[nrow(output.without.feeding),"H_TC"] +
                                                                                                                                            output.without.feeding[nrow(output.without.feeding),"H_UI"] + 
                                                                                                                                            output.without.feeding[nrow(output.without.feeding),"H_TI"])),
                                         biting_rate_infected_adult= alpha_A * ((output.without.feeding[nrow(output.without.feeding),"AQ_UI"] + 
                                                                                   output.without.feeding[nrow(output.without.feeding),"AQ_US"])/(output.without.feeding[nrow(output.without.feeding),"H_US"] + 
                                                                                                                                                    output.without.feeding[nrow(output.without.feeding),"H_TS"] +
                                                                                                                                                    output.without.feeding[nrow(output.without.feeding),"H_UC"] + 
                                                                                                                                                    output.without.feeding[nrow(output.without.feeding),"H_TC"] +
                                                                                                                                                    output.without.feeding[nrow(output.without.feeding),"H_UI"] + 
                                                                                                                                                    output.without.feeding[nrow(output.without.feeding),"H_TI"])) *
                                           ((output.without.feeding[nrow(output.without.feeding),"AQ_UI"])/(output.without.feeding[nrow(output.without.feeding),"AQ_UI"] + 
                                                                                                              output.without.feeding[nrow(output.without.feeding),"AQ_US"])),
                                         biting_rate_adult_treated_cattle= alpha_A * ((output.without.feeding[nrow(output.without.feeding),"AQ_UI"] + 
                                                                                         output.without.feeding[nrow(output.without.feeding),"AQ_US"])/(output.without.feeding[nrow(output.without.feeding),"H_US"] + 
                                                                                                                                                          output.without.feeding[nrow(output.without.feeding),"H_TS"] +
                                                                                                                                                          output.without.feeding[nrow(output.without.feeding),"H_UC"] + 
                                                                                                                                                          output.without.feeding[nrow(output.without.feeding),"H_TC"] +
                                                                                                                                                          output.without.feeding[nrow(output.without.feeding),"H_UI"] + 
                                                                                                                                                          output.without.feeding[nrow(output.without.feeding),"H_TI"])) *
                                           ((output.without.feeding[nrow(output.without.feeding),"H_TS"] +
                                               output.without.feeding[nrow(output.without.feeding),"H_TC"] +
                                               output.without.feeding[nrow(output.without.feeding),"H_TI"])/(output.without.feeding[nrow(output.without.feeding),"H_US"] + 
                                                                                                               output.without.feeding[nrow(output.without.feeding),"H_TS"] +
                                                                                                               output.without.feeding[nrow(output.without.feeding),"H_UC"] + 
                                                                                                               output.without.feeding[nrow(output.without.feeding),"H_TC"] +
                                                                                                               output.without.feeding[nrow(output.without.feeding),"H_UI"] + 
                                                                                                               output.without.feeding[nrow(output.without.feeding),"H_TI"])),
                                         biting_rate_adult_untreated_cattle= alpha_A * ((output.without.feeding[nrow(output.without.feeding),"AQ_UI"] + 
                                                                                           output.without.feeding[nrow(output.without.feeding),"AQ_US"])/(output.without.feeding[nrow(output.without.feeding),"H_US"] + 
                                                                                                                                                            output.without.feeding[nrow(output.without.feeding),"H_TS"] +
                                                                                                                                                            output.without.feeding[nrow(output.without.feeding),"H_UC"] + 
                                                                                                                                                            output.without.feeding[nrow(output.without.feeding),"H_TC"] +
                                                                                                                                                            output.without.feeding[nrow(output.without.feeding),"H_UI"] + 
                                                                                                                                                            output.without.feeding[nrow(output.without.feeding),"H_TI"])) * 
                                           ((output.without.feeding[nrow(output.without.feeding),"H_US"] + 
                                               output.without.feeding[nrow(output.without.feeding),"H_UC"] + 
                                               output.without.feeding[nrow(output.without.feeding),"H_UI"])/(output.without.feeding[nrow(output.without.feeding),"H_US"] + 
                                                                                                               output.without.feeding[nrow(output.without.feeding),"H_TS"] +
                                                                                                               output.without.feeding[nrow(output.without.feeding),"H_UC"] + 
                                                                                                               output.without.feeding[nrow(output.without.feeding),"H_TC"] +
                                                                                                               output.without.feeding[nrow(output.without.feeding),"H_UI"] + 
                                                                                                               output.without.feeding[nrow(output.without.feeding),"H_TI"])),
                                         adultick_host_ratio=(output.without.feeding[nrow(output.without.feeding),"AF_US"] +
                                                                output.without.feeding[nrow(output.without.feeding),"AF_UI"])/(output.without.feeding[nrow(output.without.feeding),"H_US"] + 
                                                                                                                                 output.without.feeding[nrow(output.without.feeding),"H_TS"] +
                                                                                                                                 output.without.feeding[nrow(output.without.feeding),"H_UC"] + 
                                                                                                                                 output.without.feeding[nrow(output.without.feeding),"H_TC"] +
                                                                                                                                 output.without.feeding[nrow(output.without.feeding),"H_UI"] + 
                                                                                                                                 output.without.feeding[nrow(output.without.feeding),"H_TI"]),
                                         nymph_host_ratio=(output.without.feeding[nrow(output.without.feeding),"NF_US"] +
                                                             output.without.feeding[nrow(output.without.feeding),"NF_UI"])/(output.without.feeding[nrow(output.without.feeding),"H_US"] + 
                                                                                                                              output.without.feeding[nrow(output.without.feeding),"H_TS"] +
                                                                                                                              output.without.feeding[nrow(output.without.feeding),"H_UC"] + 
                                                                                                                              output.without.feeding[nrow(output.without.feeding),"H_TC"] +
                                                                                                                              output.without.feeding[nrow(output.without.feeding),"H_UI"] + 
                                                                                                                              output.without.feeding[nrow(output.without.feeding),"H_TI"]),
                                         Egg.pop = output.without.feeding[nrow(output.without.feeding),"E_US"] + output.without.feeding[nrow(output.without.feeding),"E_TS"],
                                         Quest.pop = output.without.feeding[nrow(output.without.feeding),"AQ_UI"] + output.without.feeding[nrow(output.without.feeding),"AQ_US"] +
                                           output.without.feeding[nrow(output.without.feeding),"NQ_UI"] + output.without.feeding[nrow(output.without.feeding),"NQ_US"] +
                                           output.without.feeding[nrow(output.without.feeding),"LQ_US"],
                                         Feed.pop = output.without.feeding[nrow(output.without.feeding),"NF_UI"] + output.without.feeding[nrow(output.without.feeding),"NF_US"] +
                                           output.without.feeding[nrow(output.without.feeding),"AF_UI"] + output.without.feeding[nrow(output.without.feeding),"AF_US"] + 
                                           output.without.feeding[nrow(output.without.feeding),"LF_US"],
                                         Ovip.female.pop = output.without.feeding[nrow(output.without.feeding),"AO_U"] + output.without.feeding[nrow(output.without.feeding),"AO_T"],
                                         H_total = output.without.feeding[nrow(output.without.feeding),"H_UC"] + 
                                           output.without.feeding[nrow(output.without.feeding),"H_TC"] +
                                           output.without.feeding[nrow(output.without.feeding),"H_UI"] +
                                           output.without.feeding[nrow(output.without.feeding),"H_TI"] +
                                           output.without.feeding[nrow(output.without.feeding),"H_US"] +
                                           output.without.feeding[nrow(output.without.feeding),"H_TS"],
                                         
                                         H_susceptible = output.without.feeding[nrow(output.without.feeding),"H_US"] + output.without.feeding[nrow(output.without.feeding),"H_TS"],
                                         H_acute = output.without.feeding[nrow(output.without.feeding),"H_UI"] + output.without.feeding[nrow(output.without.feeding),"H_TI"],
                                         H_acute_prev = ((output.without.feeding[nrow(output.without.feeding),"H_UI"] + 
                                                            output.without.feeding[nrow(output.without.feeding),"H_TI"])/(output.without.feeding[nrow(output.without.feeding),"H_UC"] +
                                                                                                                            output.without.feeding[nrow(output.without.feeding),"H_TC"] +
                                                                                                                            output.without.feeding[nrow(output.without.feeding),"H_UI"] +
                                                                                                                            output.without.feeding[nrow(output.without.feeding),"H_TI"] +
                                                                                                                            output.without.feeding[nrow(output.without.feeding),"H_US"] +
                                                                                                                            output.without.feeding[nrow(output.without.feeding),"H_TS"]))*100,
                                         H_carrier = output.without.feeding[nrow(output.without.feeding),"H_UC"] + output.without.feeding[nrow(output.without.feeding),"H_TC"],
                                         H_I = output.without.feeding[nrow(output.without.feeding),"H_UC"] + 
                                           output.without.feeding[nrow(output.without.feeding),"H_TC"] +
                                           output.without.feeding[nrow(output.without.feeding),"H_UI"] + 
                                           output.without.feeding[nrow(output.without.feeding),"H_TI"],
                                         
                                         H_I_prev = ((output.without.feeding[nrow(output.without.feeding),"H_UC"] +
                                                        output.without.feeding[nrow(output.without.feeding),"H_TC"] +
                                                        output.without.feeding[nrow(output.without.feeding),"H_UI"] +
                                                        output.without.feeding[nrow(output.without.feeding),"H_TI"])/(output.without.feeding[nrow(output.without.feeding),"H_UC"] +
                                                                                                                        output.without.feeding[nrow(output.without.feeding),"H_TC"] +
                                                                                                                        output.without.feeding[nrow(output.without.feeding),"H_UI"] +
                                                                                                                        output.without.feeding[nrow(output.without.feeding),"H_TI"] +
                                                                                                                        output.without.feeding[nrow(output.without.feeding),"H_US"] +
                                                                                                                        output.without.feeding[nrow(output.without.feeding),"H_TS"]))*100,
                                         Q_I_adult = output.without.feeding[nrow(output.without.feeding),"AQ_UI"],
                                         Q_I_nymph = output.without.feeding[nrow(output.without.feeding),"NQ_UI"],
                                         Q_I_adult_prev = ((output.without.feeding[nrow(output.without.feeding),"AQ_UI"])/(output.without.feeding[nrow(output.without.feeding),"AQ_UI"] +
                                                                                                                             output.without.feeding[nrow(output.without.feeding),"AQ_US"]))*100,
                                         Q_I_nymph_prev = ((output.without.feeding[nrow(output.without.feeding),"NQ_UI"])/(output.without.feeding[nrow(output.without.feeding),"NQ_UI"] +
                                                                                                                             output.without.feeding[nrow(output.without.feeding),"NQ_US"]))*100,
                                         F_I_adult = output.without.feeding[nrow(output.without.feeding),"AF_UI"],
                                         F_I_nymph = output.without.feeding[nrow(output.without.feeding),"NF_UI"],
                                         F_I_prev_adult = ((output.without.feeding[nrow(output.without.feeding),"AF_UI"])/(output.without.feeding[nrow(output.without.feeding),"AF_UI"] +
                                                                                                                             output.without.feeding[nrow(output.without.feeding),"AF_US"]))*100,
                                         F_I_prev_nymph = ((output.without.feeding[nrow(output.without.feeding),"NF_UI"])/(output.without.feeding[nrow(output.without.feeding),"NF_UI"] + 
                                                                                                                             output.without.feeding[nrow(output.without.feeding),"NF_US"]))*100,
                                         tick.pop = (output.without.feeding[nrow(output.without.feeding),"LQ_US"] +
                                                       output.without.feeding[nrow(output.without.feeding),"LF_US"] +
                                                       output.without.feeding[nrow(output.without.feeding),"LD_US"] +
                                                       output.without.feeding[nrow(output.without.feeding),"LD_TS"] +
                                                       output.without.feeding[nrow(output.without.feeding),"LD_UI"] +
                                                       output.without.feeding[nrow(output.without.feeding),"LD_TI"] +
                                                       output.without.feeding[nrow(output.without.feeding),"NQ_US"] +
                                                       output.without.feeding[nrow(output.without.feeding),"NQ_UI"] +
                                                       output.without.feeding[nrow(output.without.feeding),"NF_US"] +
                                                       output.without.feeding[nrow(output.without.feeding),"NF_UI"] +
                                                       output.without.feeding[nrow(output.without.feeding),"ND_US"] +
                                                       output.without.feeding[nrow(output.without.feeding),"ND_TS"] +
                                                       output.without.feeding[nrow(output.without.feeding),"ND_UI"] +
                                                       output.without.feeding[nrow(output.without.feeding),"ND_TI"] +
                                                       output.without.feeding[nrow(output.without.feeding),"AQ_US"] +
                                                       output.without.feeding[nrow(output.without.feeding),"AQ_UI"] +
                                                       output.without.feeding[nrow(output.without.feeding),"AF_US"] +
                                                       output.without.feeding[nrow(output.without.feeding),"AF_UI"] +
                                                       output.without.feeding[nrow(output.without.feeding),"AD_U"] +
                                                       output.without.feeding[nrow(output.without.feeding),"AD_T"] +
                                                       output.without.feeding[nrow(output.without.feeding),"AO_U"] +
                                                       output.without.feeding[nrow(output.without.feeding),"AO_T"]),
                                         T_H_ratio = (output.without.feeding[nrow(output.without.feeding),"LF_US"] +
                                                        output.without.feeding[nrow(output.without.feeding),"NF_US"] +
                                                        output.without.feeding[nrow(output.without.feeding),"NF_UI"] +
                                                        output.without.feeding[nrow(output.without.feeding),"AF_US"] +
                                                        output.without.feeding[nrow(output.without.feeding),"AF_UI"])/(output.without.feeding[nrow(output.without.feeding),"H_US"] +
                                                                                                                         output.without.feeding[nrow(output.without.feeding),"H_TS"] +
                                                                                                                         output.without.feeding[nrow(output.without.feeding),"H_UC"] + 
                                                                                                                         output.without.feeding[nrow(output.without.feeding),"H_TC"] +
                                                                                                                         output.without.feeding[nrow(output.without.feeding),"H_UI"] + 
                                                                                                                         output.without.feeding[nrow(output.without.feeding),"H_TI"]),
                                         T_H_ratio_2 = (output.without.feeding[nrow(output.without.feeding),"LQ_US"] +
                                                          output.without.feeding[nrow(output.without.feeding),"LF_US"] +
                                                          output.without.feeding[nrow(output.without.feeding),"LD_US"] +
                                                          output.without.feeding[nrow(output.without.feeding),"LD_TS"] +
                                                          output.without.feeding[nrow(output.without.feeding),"LD_UI"] +
                                                          output.without.feeding[nrow(output.without.feeding),"LD_TI"] +
                                                          output.without.feeding[nrow(output.without.feeding),"NQ_US"] +
                                                          output.without.feeding[nrow(output.without.feeding),"NQ_UI"] +
                                                          output.without.feeding[nrow(output.without.feeding),"NF_US"] +
                                                          output.without.feeding[nrow(output.without.feeding),"NF_UI"] +
                                                          output.without.feeding[nrow(output.without.feeding),"ND_US"] +
                                                          output.without.feeding[nrow(output.without.feeding),"ND_TS"] +
                                                          output.without.feeding[nrow(output.without.feeding),"ND_UI"] +
                                                          output.without.feeding[nrow(output.without.feeding),"ND_TI"] +
                                                          output.without.feeding[nrow(output.without.feeding),"AQ_US"] +
                                                          output.without.feeding[nrow(output.without.feeding),"AQ_UI"] +
                                                          output.without.feeding[nrow(output.without.feeding),"AF_US"] +
                                                          output.without.feeding[nrow(output.without.feeding),"AF_UI"] +
                                                          output.without.feeding[nrow(output.without.feeding),"AD_U"] +
                                                          output.without.feeding[nrow(output.without.feeding),"AD_T"] +
                                                          output.without.feeding[nrow(output.without.feeding),"AO_U"] +
                                                          output.without.feeding[nrow(output.without.feeding),"AO_T"])/(output.without.feeding[nrow(output.without.feeding),"H_US"] +
                                                                                                                          output.without.feeding[nrow(output.without.feeding),"H_TS"] +
                                                                                                                          output.without.feeding[nrow(output.without.feeding),"H_UC"] + 
                                                                                                                          output.without.feeding[nrow(output.without.feeding),"H_TC"] +
                                                                                                                          output.without.feeding[nrow(output.without.feeding),"H_UI"] + 
                                                                                                                          output.without.feeding[nrow(output.without.feeding),"H_TI"]),
                                         coverage = coverage_level,
                                         phi = treatment.interval)
    
    
    
    # Append the result row to the main results data frame
    
    results.without.feeding.df <- rbind(results.without.feeding.df, result.without.feeding)  
    
    
  }
}

## view the output data frame
#View(results.without.feeding.df) 



# Figure 5 in manuscript --------------------------------------------------

#### Figure 5(a) All effects  _____________________________________________

## Adjust the cases relative to the baseline and store it in a new column called "adjusted..." 
results.treatment.df$adjusted_H_acute <- ((63.09235 - results.treatment.df$H_acute)/63.09235)*100

# Reverse the Reduction values to show a declining trend
results.treatment.df$adjusted_H_acute <- -results.treatment.df$adjusted_H_acute


View(results.treatment.df)

cases.host.coverage.relative <- ggplot(data=results.treatment.df, 
                                       aes(x = coverage, y = adjusted_H_acute, group = factor(phi), color=factor(phi))) +
  geom_line() +
  geom_point() + 
  theme_bw() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1.05), breaks = seq(0, 1, 0.2)) +  
  scale_y_continuous(expand = c(0, 0), limits = c(-100, 1), breaks = seq(-100, 1, 20)) + 
  labs(title="",x = "", y = "") +
  theme(plot.title = element_text(hjust=0.5)) +  
  theme(legend.position="top") +
  scale_color_discrete(name = "Treatment interval",
                       breaks=c("28", "21", "14", "7"),
                       labels=c("Monthly", "Triweekly", "Biweekly", "Weekly")) 


cases.host.coverage.relative  


#### Figure 5(b) without mortality effect__________________________________  

## Adjust the cases relative to the baseline and store it in a new column called "adjusted..." 
results.without.mortality.df$adjusted_H_acute <- ((63.09235 - results.without.mortality.df$H_acute)/63.09235)*100

# Reverse the Reduction values to show a declining trend
results.without.mortality.df$adjusted_H_acute <- -results.without.mortality.df$adjusted_H_acute


View(results.without.mortality.df)


cases.without.mortality.effect.relative <- ggplot(data=results.without.mortality.df, 
                                                  aes(x = coverage, y = adjusted_H_acute, group = factor(phi), color=factor(phi))) +
  geom_line() +
  geom_point() + 
  theme_bw() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1.05), breaks = seq(0, 1, 0.2)) +  
  scale_y_continuous(expand = c(0, 0), limits = c(-100, 1), breaks = seq(-100, 1, 20)) + 
  labs(title="",x = "", y = "") +
  theme(plot.title = element_text(hjust=0.5),   
        axis.text.y=element_blank(),axis.ticks.y=element_blank()) +  
  #theme(legend.position="top") +
  theme(legend.position="none") +
  scale_color_discrete(name = "Treatment interval",
                       breaks=c("28", "21", "14", "7"),
                       labels=c("Monthly", "Triweekly", "Biweekly", "Weekly")) 


cases.without.mortality.effect.relative  



#### Figure 5(c) without fecundity reduction__________________________________  

## Adjust the cases relative to the baseline and store it in a new column called "adjusted..." 
results.without.fecundity.df$adjusted_H_acute <- ((63.09235 - results.without.fecundity.df$H_acute)/63.09235)*100

# Reverse the Reduction values to show a declining trend
results.without.fecundity.df$adjusted_H_acute <- -results.without.fecundity.df$adjusted_H_acute


View(results.without.fecundity.df)

cases.without.fecundity.effect.relative <- ggplot(data=results.without.fecundity.df, 
                                                  aes(x = coverage, y = adjusted_H_acute, group = factor(phi), color=factor(phi))) +
  geom_line() +
  geom_point() + 
  theme_bw() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1.05), breaks = seq(0, 1, 0.2)) +  
  scale_y_continuous(expand = c(0, 0), limits = c(-100, 1), breaks = seq(-100, 1, 20)) + 
  labs(title="",x = "", y = "") +
  theme(plot.title = element_text(hjust=0.5),   
        axis.text.y=element_blank(),axis.ticks.y=element_blank()) +  
  #theme(legend.position="top") +
  theme(legend.position="none") +
  scale_color_discrete(name = "Treatment interval",
                       breaks=c("28", "21", "14", "7"),
                       labels=c("Monthly", "Triweekly", "Biweekly", "Weekly")) 


cases.without.fecundity.effect.relative  


#### Figure 5(d) without oviposition delay________________________________  

## Adjust the cases relative to the baseline and store it in a new column called "adjusted..." 
results.without.oviposition.delay.df$adjusted_H_acute <- ((63.09235 - results.without.oviposition.delay.df$H_acute)/63.09235)*100

# Reverse the Reduction values to show a declining trend
results.without.oviposition.delay.df$adjusted_H_acute <- -results.without.oviposition.delay.df$adjusted_H_acute


View(results.without.oviposition.delay.df)


cases.without.oviposition.delay.relative <- ggplot(data=results.without.oviposition.delay.df, 
                                                   aes(x = coverage, y = adjusted_H_acute, group = factor(phi), color=factor(phi))) +
  geom_line() +
  geom_point() + 
  theme_bw() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1.05), breaks = seq(0, 1, 0.2)) +  
  scale_y_continuous(expand = c(0, 0), limits = c(-100, 1), breaks = seq(-100, 1, 20)) + 
  labs(title="",x = "", y = "") +
  theme(plot.title = element_text(hjust=0.5)) +  
  #theme(legend.position="top") +
  theme(legend.position="none") +
  scale_color_discrete(name = "Treatment interval",
                       breaks=c("28", "21", "14", "7"),
                       labels=c("Monthly", "Triweekly", "Biweekly", "Weekly")) 


cases.without.oviposition.delay.relative  



#### Figure 5(e) without feeding effect__________________________________  

## Adjust the cases relative to the baseline and store it in a new column called "adjusted..." 
results.without.feeding.df$adjusted_H_acute <- ((63.09235 - results.without.feeding.df$H_acute)/63.09235)*100

# Reverse the Reduction values to show a declining trend
results.without.feeding.df$adjusted_H_acute <- -results.without.feeding.df$adjusted_H_acute


View(results.without.feeding.df)


cases.without.feeding.effect.relative <- ggplot(data=results.without.feeding.df, 
                                                aes(x = coverage, y = adjusted_H_acute, group = factor(phi), color=factor(phi))) +
  geom_line() +
  geom_point() + 
  theme_bw() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1.05), breaks = seq(0, 1, 0.2)) +  
  scale_y_continuous(expand = c(0, 0), limits = c(-100, 1), breaks = seq(-100, 1, 20)) + 
  labs(title="",x = "", y = "") +
  theme(plot.title = element_text(hjust=0.5),   
        axis.text.y=element_blank(),axis.ticks.y=element_blank()) +  
  #theme(legend.position="top") +
  theme(legend.position="none") +
  scale_color_discrete(name = "Treatment interval",
                       breaks=c("28", "21", "14", "7"),
                       labels=c("Monthly", "Triweekly", "Biweekly", "Weekly")) 


cases.without.feeding.effect.relative  



# Combining the plots for composite effects using ggarrange ---------------

library(ggpubr) 

composite.effect.cases.relative <- ggarrange(cases.host.coverage.relative, 
                                             cases.without.mortality.effect.relative, 
                                             cases.without.fecundity.effect.relative, 
                                             cases.without.oviposition.delay.relative, 
                                             cases.without.feeding.effect.relative,
                                             labels = c("A", "B", "C", "D", "E"), 
                                             ncol = 3, nrow = 2,
                                             common.legend = TRUE, legend = "top")

composite.effect.cases.relative


## Annotate the arranged figure

annotate_figure(composite.effect.cases.relative, 
                left = text_grob("Change in acute infections (%)", color = "black", face = "bold", size = 14, rot = 90),
                bottom = text_grob("Coverage", color = "black", face = "bold", size = 14)) 


#Export image

#jpeg
ggsave("Figure_5.jpeg", plot = last_plot(), device = "jpeg", scale = 1,
       width = 7, height = 5, units = c("in", "cm", "mm", "px"), dpi = 300,
       limitsize = TRUE, bg = "white")

#TIFF
ggsave("Figure_5.tiff", plot = last_plot(), device = "tiff", scale = 1,
       width = 7, height = 5, units = c("in", "cm", "mm", "px"), dpi = 300,
       limitsize = TRUE, bg = "white")


