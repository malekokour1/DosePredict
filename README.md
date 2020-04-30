## *DosePredict*
In drug development, dose prediction is a routinely conducted quantitative data analysis. It involves combining available relevant preclinical and/or clinical data collectively to conduct modeling and simulation analyses where various dose prediction scenarios are considered. As further data emerges during drug development, dose prediction may undergo several rounds of refinement.  In this project, we present *DosePredict*, a Shiny-based graphical user interface software that can be used for the conduct of dose predictions.

#### Citations
Under Publcation 

## Installation
Install R package dependencies:
```r
install.packages(c("shiny","shinyjs","shinyBS","shinythemes","shinydashboard","mrgsolve","deSolve","magrittr","ggplot2","plotly","PKNCA","data.table","dplyr","RColorBrewer","rmarkdown","knitr","reshape2"))

```

Run the application from the R command line:
```r
shiny::runGitHub('malekokour1/DosePredict')
```
