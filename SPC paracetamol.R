library(NHSRplotthedots)
library(tidyverse)

## tablets


para_combined %>% 
  filter(location == "Main Theatres [BRM]", item_merged == "PARACETAMOL 500 mg Tablets") %>% 
  ptd_spc(value_field = quant, date_field = Month, improvement_direction = "increase") %>% 
  plot(
    y_axis_label = "Number of Tablets",
    main_title = "SPC of chart of the number of paracetamol tablets ")


## 1g IV

para_combined %>% 
  filter(location == "Main Theatres [BRM]", item_merged == "PARACETAMOL 1 g in 100ml Intravenous Injection") %>% 
  ptd_spc(value_field = quant, date_field = Month, improvement_direction = "decrease") %>% 
  plot(
    y_axis_label = "Number",
    main_title = "SPC of chart of the 1g IV Paracetamol")

## 500mg IV

para_combined %>% 
  filter(location == "Main Theatres [BRM]", item_merged == "PARACETAMOL 500 mg in 50ml Intravenous Injection") %>% 
  ptd_spc(value_field = quant, date_field = Month, improvement_direction = "decrease") %>% 
  plot(
    y_axis_label = "Number",
    main_title = "SPC of chart of the 500mg IV Paracetamol ")



