# CNPS.cycle Shiny App

This repository now contains a Shiny app scaffold for the CNPS.cycle workflow. The app
accepts a CSV input and parameters, then runs a placeholder `compute_cnps_cycle()`
function.

## Next steps

1. Replace `compute_cnps_cycle()` in `app.R` with the actual CNPS.cycle algorithm logic.
2. Extend the UI with any domain-specific parameters that the original repository
   requires.
3. Add plots or downloadable outputs based on the algorithm outputs.

## Running the app

```r
install.packages("shiny")
shiny::runApp()
```
