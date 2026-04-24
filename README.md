# Insurance Cost Analysis

## Syfte

Analysera vilka faktorer som påverkar försäkringskostnader.

## Rapport

[Öppna rapport](report/report.html)

## Hur man kör projektet:

1.  Öppna projektet i RStudio (.Rproj)

2.  Installera paket: 
```r
install.packages("tidyverse")
```


3.  Kör analysen :

-   Öppna `report/report.Rmd`
-   Klicka på "Knit" för att generera rapporten

## Innehåll

-   Datastädning
-   Feature engineering (bmi_cat, age_group)
-   Visualiseringar
-   Regressionsanalys
-   Modelljämförelse


## Struktur

-   `data/` - dataset

-   `scripts/` - analyskod(Analysis.R)

-   `report/` - rapport (report.Rmd, report.html)

## Paket

-   tidyverse
-   ggplot2
-   dplyr
