---
title: "README"
output: html_document
---

# CDSimX

## Climate Data Simulation and Forecasting Toolkit for R

CDSimX is an advanced R package for:

* Climate data simulation
* Weather variable generation
* Machine learning forecasting
* NetCDF export
* Climate visualization
* Seasonal analysis
* Copula-based dependence modeling

## Installation

```r
# install.packages("remotes")
remotes::install_github("ikemillar/CDSimX")
```

## Example

```r
library(CDSimX)

stations <- create_stations(n = 3)

time_index <- generate_time_index(
  start_date = "2000-01-01",
  end_date = "2020-12-31"
)

climate <- simulate_climate(
  stations = stations,
  time_index = time_index
)

head(climate)
```

## Features

* Temperature simulation
* Rainfall simulation
* Relative humidity simulation
* Wind speed simulation
* Solar radiation simulation
* Machine learning forecasting
* NetCDF export support
* Advanced climate visualization

## Author

Isaac Osei
