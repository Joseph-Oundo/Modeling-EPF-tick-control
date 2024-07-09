
#Source files for generating figures 2, 3 and 5 in the manuscript


# Function creating a treatment signal

times <- seq(from = 0, to = 365, by = 0.01) # duration of applying the treatment 

create.treatment.signal<-function(treatment.interval, times){  
  signal <- data.frame(times = times, phi = rep(0, length(times)))  
  signal$phi <- ifelse((trunc(signal$times) %% treatment.interval == 0), 1, 0)  
  View(signal)
  input <- approxfun(signal, rule = 2)    
  return(input)
}




# Function to solve the ODE model

run.model.by.treatment.interval.and.coverage <- function(coverage_level, treatment.interval, time, initial.state, param){
  input <- create.treatment.signal(treatment.interval, times)   
 
  tic()    #start the timer 
  
  cattle.tick.model.output <- ode(y = initial.state, times = time,
                                  func = cattle.tick.model, 
                                  parms = param, input = input) 
  
  toc()   #stops the timer 
  
  cattle.tick.model.output <- as.data.frame(cattle.tick.model.output) 
  
  return(cattle.tick.model.output) 
  
  
}




# Create a function for the tick model

cattle.tick.model <- function(time, state, parameters, input){  
  with(as.list(c(state, parameters)),{
    phi <- input(time)   
    
    H= H_US + H_TS + H_UI + H_TI + H_UC + H_TC
    
    H_U= H_US + H_UI + H_UC 
    
    H_T= H_TS + H_TI + H_TC
    
    H_S = H_US + H_TS
    
    H_I = H_UI + H_TI
    
    H_C = H_UC + H_TC
    
    NQ = NQ_US + NQ_UI
    
    AQ = AQ_US + AQ_UI
    
    N_T = E_US + E_TS + LQ_US + LF_US + LD_US + LD_TS + LD_UI + LD_TI + NQ_US + NQ_UI + NF_US +
      NF_UI + ND_US + ND_TS + ND_UI + ND_TI + AQ_US + AQ_UI + AF_US + AF_UI + 
      AD_U + AD_T + AO_U + AO_T
    
    dH_US = H*mu_H + H_I*mu_I + H_TS*delta - coverage*H_US*phi - H_US*mu_H - 
      H_US*alpha_N*(NQ/H)*(NF_UI/(NF_US+NF_UI))*p_HN - 
      H_US*alpha_A*(AQ/H)*(AF_UI/(AF_US+AF_UI))*p_HA
    
    dH_TS = coverage*H_US*phi - H_TS*delta - H_TS*mu_H - 
      H_TS*alpha_N*(NQ/H)*(NF_UI/(NF_US+NF_UI))*p_HN - 
      H_TS*alpha_A*(AQ/H)*(AF_UI/(AF_US+AF_UI))*p_HA
    
    dH_UI = H_US*alpha_N*(NQ/H)*(NF_UI/(NF_US+NF_UI))*p_HN + 
      H_US*alpha_A*(AQ/H)*(AF_UI/(AF_US+AF_UI))*p_HA + H_TI*delta - 
      coverage*H_UI*phi - H_UI*sigma - H_UI*mu_H - H_UI*mu_I
    
    dH_TI = H_TS*alpha_N*(NQ/H)*(NF_UI/(NF_US+NF_UI))*p_HN + 
      H_TS*alpha_A*(AQ/H)*(AF_UI/(AF_US+AF_UI))*p_HA + coverage*H_UI*phi - 
      H_TI*delta - H_TI*sigma - H_TI*mu_H - H_TI*mu_I
    
    dH_UC = H_UI*sigma + H_TC*delta - coverage*H_UC*phi - H_UC*mu_H
    
    dH_TC = H_TI*sigma + coverage*H_UC*phi - H_TC*delta - H_TC*mu_H
    
    dE_US = AO_U*(E_max/t_o)*(1 - N_T/k_T) - E_US*k_E - E_US*mu_E 
    
    dE_TS = AO_T*((E_max*(1-TFR))/(t_o + tau_o))*(1 - N_T/k_T) - E_TS*k_E - E_TS*mu_E 
    
    dLQ_US = E_US*k_E + E_TS*k_E - LQ_US*mu_LQ - LQ_US*alpha_L 
    
    dLF_US = LQ_US*alpha_L - LF_US*mu_LF - 
      LF_US*(H_U/H + H_T/H * (1-p_LT)) * (H_S/H + H_I/H*(1-p_LI) + H_C/H*(1-p_LC)) * (1/d_L) -
      LF_US*(H_T/H * p_LT) * (H_S/H + H_I/H*(1-p_LI) + H_C/H*(1-p_LC)) * 1/(d_L + tau_L) - 
      LF_US*(H_U/H + H_T/H * (1-p_LT)) * ((H_I/H * p_LI) + (H_C/H * p_LC)) * (1/d_L) -
      LF_US*(H_T/H * p_LT) * ((H_I/H * p_LI) + (H_C/H * p_LC)) * 1/(d_L + tau_L)
    
    dLD_US = LF_US*(H_U/H + H_T/H * (1-p_LT)) * (H_S/H + H_I/H*(1-p_LI) + H_C/H*(1-p_LC)) * (1/d_L) - 
      LD_US*mu_LD - LD_US*k_L
    
    dLD_TS = LF_US*(H_T/H * p_LT) * (H_S/H + H_I/H*(1-p_LI) + H_C/H*(1-p_LC)) * 1/(d_L + tau_L) - 
      LD_TS*(mu_LD + mu_LDF) - LD_TS*k_L
    
    dLD_UI = LF_US*(H_U/H + H_T/H * (1-p_LT)) * ((H_I/H * p_LI) + (H_C/H * p_LC)) * (1/d_L) - 
      LD_UI*mu_LD - LD_UI*k_L
    
    dLD_TI = LF_US*(H_T/H * p_LT) * ((H_I/H * p_LI) + (H_C/H * p_LC)) * 1/(d_L + tau_L) - 
      LD_TI*(mu_LD + mu_LDF) - LD_TI*k_L
    
    dNQ_US = LD_US*k_L + LD_TS*k_L - NQ_US*mu_NQ - NQ_US*alpha_N
    
    dNQ_UI = LD_UI*k_L + LD_TI*k_L - NQ_UI*mu_NQ - NQ_UI*alpha_N
    
    dNF_US = NQ_US*alpha_N - NF_US*mu_NF - 
      NF_US*(H_U/H + H_T/H * (1-p_NT)) * (H_S/H + H_I/H * (1-p_NI) + H_C/H * (1-p_NC)) * (1/d_N) -
      NF_US*(H_T/H * p_NT) * (H_S/H + H_I/H * (1-p_NI) + H_C/H * (1-p_NC)) * 1/(d_N + tau_N) - 
      NF_US*(H_U/H + H_T/H * (1-p_NT)) * ((H_I/H * p_NI) + (H_C/H * p_NC)) * (1/d_N) -
      NF_US*(H_T/H * p_NT) * ((H_I/H * p_NI) + (H_C/H * p_NC)) * 1/(d_N + tau_N)
    
    dNF_UI = NQ_UI*alpha_N - NF_UI*mu_NF - NF_UI*(H_U/H + H_T/H * (1-p_NT)) * (1/d_N) -
      NF_UI*(H_T/H * p_NT) * 1/(d_N + tau_N) 
    
    dND_US = NF_US*(H_U/H + H_T/H * (1-p_NT)) * (H_S/H + H_I/H * (1-p_NI) + H_C/H * (1-p_NC)) * (1/d_N) -
      ND_US*mu_ND - ND_US*k_N
    
    dND_TS = NF_US*(H_T/H * p_NT) * (H_S/H + H_I/H * (1-p_NI) + H_C/H * (1-p_NC)) * 1/(d_N + tau_N) -
      ND_TS*(mu_ND + mu_NDF) - ND_TS*k_N
    
    dND_UI = NF_US*(H_U/H + H_T/H * (1-p_NT)) * ((H_I/H * p_NI) + (H_C/H * p_NC)) * (1/d_N) +
      NF_UI*(H_U/H + H_T/H * (1-p_NT)) * (1/d_N) - ND_UI*mu_ND - ND_UI*k_N
    
    dND_TI = NF_US*(H_T/H * p_NT) * ((H_I/H * p_NI) + (H_C/H * p_NC)) * 1/(d_N + tau_N) +
      NF_UI*(H_T/H * p_NT) * 1/(d_N + tau_N) - ND_TI*(mu_ND + mu_NDF) - ND_TI*k_N
    
    dAQ_US = ND_US*k_N + ND_TS*k_N - AQ_US*mu_AQ - AQ_US*alpha_A
    
    dAQ_UI = ND_UI*k_N + ND_TI*k_N - AQ_UI*mu_AQ - AQ_UI*alpha_A
    
    dAF_US = AQ_US*alpha_A - AF_US*mu_AF - 
      AF_US*(H_U/H + H_T/H * (1-p_AT)) * (1/d_A) -
      AF_US*(H_T/H * p_AT) * 1/(d_A + tau_A)
    
    dAF_UI = AQ_UI*alpha_A - AF_UI*mu_AF - AF_UI*(H_U/H + H_T/H * (1-p_AT)) * (1/d_A) -
      AF_UI*(H_T/H * p_AT) * 1/(d_A + tau_A)  
    
    dAD_U = AF_US*(H_U/H + H_T/H * (1-p_AT)) * (1/d_A) - AD_U*mu_AD - AD_U*(1/k_o) + 
      AF_UI*(H_U/H + H_T/H * (1-p_AT)) * (1/d_A) 
    
    dAD_T = AF_US*(H_T/H * p_AT) * 1/(d_A + tau_A) - AD_T*(mu_AD + mu_ADF) - 
      AD_T*1/(k_o + k_tau) + AF_UI*(H_T/H * p_AT) * 1/(d_A + tau_A) 
    
    dAO_U = AD_U*zeta*(1/k_o) - AO_U*mu_AO - AO_U*(1/t_o)
    
    dAO_T = AD_T*zeta*1/(k_o + k_tau) - AO_T*mu_AO - AO_T*mu_AOF - AO_T*1/(t_o + tau_o)
    
    
    return(list(c(dH_US, dH_TS, dH_UI, dH_TI, dH_UC, dH_TC, dE_US, dE_TS, dLQ_US,
                  dLF_US, dLD_US, dLD_TS, dLD_UI, dLD_TI, dNQ_US, dNQ_UI, dNF_US, 
                  dNF_UI, dND_US, dND_TS, dND_UI, dND_TI, dAQ_US, dAQ_UI, dAF_US, 
                  dAF_UI, dAD_U, dAD_T, dAO_U, dAO_T), signal = phi))
  })
  
}



