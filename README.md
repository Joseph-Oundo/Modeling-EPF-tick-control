# Modeling-EPF-tick-control

This repository contains code used in the following paper.

Joseph W Oundo, Nienke Hartemink, Mart C.M. de Jong, Constantianus J.M. Koenraadt, Shewit Kalayou, Daniel Masiga, Quirine ten Bosch. (2025). Biological control of ticks in domestic environments: Modeling the potential impact of entomopathogenic fungi on the transmission of East Coast fever in cattle. Ticks and tick-borne diseases, 16(1), 102435. https://doi.org/10.1016/j.ttbdis.2024.102435 

 
## Description of the different R files and the figures generated


### "model_tick_output_1.R" and "model_tick_functions.R"

model_tick_functions.R contains functions that run model_tick_output_1.R. This code generates figure 2 in the manuscript - which shows that the default formulation of EPF, under the default treatment strategy, reduces East Coast fever (ECF) transmission in cattle population by:
	(a). Reducing the incidence in the cattle population
	(b). Reducing tick abundance and hence tick-to-host ratio
	(c). Reducing exposure rate of cattle to ticks 


### "model_tick_output_2.R" and "model_tick_functions.R"

model_tick_functions.R contains functions that run model_tick_output_2.R. This code generates figures 3 and 5 in the manuscript - which shows the potential impact of different implementation strategies (coverage and treatment frequency) and EPF properties/profiles on the incidence of ECF in the cattle population. 


### "decay_model.R" and "decay_model_functions.R"

decay_model_functions.R contains functions that run decay_model.R. This code generates figure 4 in the manuscript - which shows the potential effects of improving the persistence time of EPF on the incidence of ECF in the cattle population


### "contact_probability_model_functions.R" and "contact_probability_model.R"

contact_probability_model_functions.R contains functions that run contact_probability_model.R. This code generates figure 6 in the manuscript - which shows the potential effects of improving the treatment (delivery) efficiency of EPF on the incidence of ECF in the cattle population

