# CNPS.cycle Shiny App

This Shiny app runs the official `SampleData_AutomatedExecutionScript.Rmd` from the
CNPS.cycle v1.0.0 release. Upload the script and the sample data files, then render the
report directly in the browser.

## How to use

1. Download the release assets from:
   `https://github.com/yuezhengfu/CNPS.cycle/releases/tag/V1.0.0`
2. Start the app:

```r
install.packages(c("shiny", "rmarkdown"))
shiny::runApp()
```

3. In the app, upload `SampleData_AutomatedExecutionScript.Rmd` and all accompanying
   data files from the release.
4. Click **Run script** to generate the HTML report, then download it.

## Notes

- The app copies uploaded files into a temporary working directory before rendering.
- Any required R packages for the CNPS.cycle script must be installed locally.
