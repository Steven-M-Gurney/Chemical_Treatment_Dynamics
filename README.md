PAGE UNDER CONSTRUCTION

# Rotenone-based management of invasive fish at DTW: a feasibility analysis using chemical dynamics modeling

### [Steven Gurney](https://linktr.ee/stevenmgurney), [Selena Creed]()

### Manuscript: Airfield Operations Department–Wildlife Division, Special Publication No. 25-03. Wayne County Airport Authority, Detroit, MI, USA.

### Manuscript available here: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17842930.svg)](https://doi.org/10.5281/zenodo.17842930)

### Data: Some data are sensitive and housed with the Wildlife Division's special-publication repository.

#### Please contact the first author for questions about the code or data: Steven M. Gurney (stevenmgurney@gmail.com)
__________________________________________________________________________________________________________________________________________

## Summary

Invasive goldfish (Carassius auratus) in retention ponds at Detroit Metropolitan Wayne County Airport (DTW) pose a persistent wildlife-hazard concern by attracting fish-eating birds associated with damaging aircraft strikes. Rotenone is one potential management tool capable of achieving complete fish eradication and improving pond water quality, but its feasibility in highly managed stormwater systems depends on how rapidly it degrades under site-specific conditions. This project uses published rotenone degradation data, DTW pond temperature and pH measurements, and mass-balance dilution modeling to estimate rotenone half-life, simulate decay–dilution scenarios, and evaluate whether regulatory discharge thresholds can be met within operational constraints. By integrating exponential and log-linear chemical models and applying model averaging, this analysis provides reproducible, decision-relevant estimates of rotenone persistence to support structured evaluation of chemical treatment alongside alternative invasive fish management strategies at DTW.
__________________________________________________________________________________________________________________________________________

## Repository Directory


### [Cross_Study_Data.csv](./Cross_Study_Data.csv): Literature-derived cross-study dataset used for model fitting, including water temperature, pH, and rotenone half-life estimates.

### [Decay_Scenarios.R](./Decay_Scenarios.R): Code for running decay and dilution scenarios, summarizing associated results, and visualizing decay.

### [Half_Life_Modeling.R](./Half_Life_Modeling.R): Code to develop and apply exponential-decay and log–linear models for estimating rotenone half-life from cross-study degradation data, generate visualizations, and synthesize results using model averaging.

### [Site_Conditions.R](./Site_Conditions.R): Code to processes WCAA environmental sampling records and generates site-condition estimates for pond-level modeling.



