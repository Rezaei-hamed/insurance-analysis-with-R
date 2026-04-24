---
title: "README"
output: html
editor_options: 
  markdown: 
    wrap: 72
---

# Insurance Cost Analysis


## Syftet
 Analysera vilka faktorer som påverkar försäkringskostnader.


## Hur man kör projektet:

1.  Öppna projektet i RStudio(.Rproj)

2.  Installera paket:
install.packages("tidyverse")

3.  Kör analysen :
- Öppna `report/report.Rmd`
- klicka på "Knit" för att generera rapporten


## Innehåll

-   Datastädning
-   Feature engineering (bmi_cat, age_group)
-   Visualiseringar
-   Regressionsanalys
-   Modelljämförelse



## Struktur

-   `data/`  > dataset

-   `scripts/` > analyskod(Analysis.R)

-   `report/` > rapport (report.Rmd)

## Paket

-   tidyverse
-   ggplot2
-   dplyr
