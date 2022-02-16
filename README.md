## Competing-risk length of stay analysis for COVID-19

Using the `flexsurv` package, fits length-of-stay distributions for COVID-19 clinical progression. 

### Paper

Full methodology, background and results are described in the paper, 'Hospital length of stay in a mixed Omicron and Delta epidemic in New South Wales, Australia', where this approach is implemented on hospital stay data from the NSW hospital system.



### Distribution fitting

This project uses `flexsurv` for all estimation steps, in particular using the multi-state mixture distribution approach as described in section 3 of [Flexible parametric multi-state modelling with flexsurv](https://cran.r-project.org/web/packages/flexsurv/vignettes/multistate.pdf). This is implemented in the `R/surv_ward_to_next.R`, `R/surv_ICU_to_next.R` and `R_surv_postICU_to_next.R` files. Estimated parameters and summary statistics are then extracted from these fits, as implemented in `R/get_fit_params.R`, `R/get_fit_means.R`, `R/get_fit_aj.R` and `R/extra/make_estimate_samples.R`. Code for production of figures as found in the paper is in the `results_*.R` files.

Code has been provided for fitting truncation-adjusted _symptom-onset-to-hospitalisation_ delays, but have not been verified or well-documented as these results were not used in the paper above.

### Fit pipeline

The model fitting pipeline is implemented with the `targets` package, see `_targets.R` for the full pipeline. You will need to re-implement the `read_NSW_linelist` such that it reads in your dataset as appropriate. The subsets for analysis ('epidemic periods') are defined by the `data_subsets` table in `R/fit_meta.R`. 

**If using this code for a new dataset, please review the assumptions made in each fitting step carefully (e.g. distribution of intra-ICU ward stay times, `optim` parameters, use of date-times rather than dates, etc.).**

