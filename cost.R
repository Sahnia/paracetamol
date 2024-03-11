
library(here)
library(tidyverse)
library(janitor)

set_here()

#load datasets

value <- read_csv(here("Paracetamol IV and oral - Drug Usage Summary Report Dec 2023 value.csv"))

# f;atten
value <- clean_names(value)

value <-
  value %>% 
  mutate(location = factor(location)) %>% 
  mutate(item_1 = factor(item_1)) %>% 
  mutate(item_2 = factor(item_2)) %>% 
  mutate(item_merged = recode(item_2,
                              "PARACETAMOL (SUGAR FREE) 250 mg in 5ml Suspension. 100 mL Bottle" = "PARACETAMOL (SUGAR FREE) 250 mg in 5ml Suspension",
                              "PARACETAMOL (SUGAR FREE) 250 mg in 5ml Suspension. 200 mL Bottle" = "PARACETAMOL (SUGAR FREE) 250 mg in 5ml Suspension",
                              "PARACETAMOL (SUGAR FREE) 250 mg in 5ml Suspension. 500 mL Bottle" =  "PARACETAMOL (SUGAR FREE) 250 mg in 5ml Suspension", 
                              "PARACETAMOL 1 g in 100ml Intravenous Injection 10 x 100ml vial pack" ="PARACETAMOL 1 g in 100ml Intravenous Injection",
                              "PARACETAMOL 1 g in 100ml Intravenous Injection 12 x 100ml vial Pack" = "PARACETAMOL 1 g in 100ml Intravenous Injection",
                              "PARACETAMOL 500 mg in 50ml Intravenous Injection 1 x 50ml vial pack" = "PARACETAMOL 500 mg in 50ml Intravenous Injection",
                              "PARACETAMOL 500 mg in 50ml Intravenous Injection 10 x 50ml vial pack" ="PARACETAMOL 500 mg in 50ml Intravenous Injection",
                              "PARACETAMOL 500 mg Tablets 100 Tablet Bottle" = "PARACETAMOL 500 mg Tablets",                        
                              "PARACETAMOL 500 mg Tablets 1000 Tablet pack" = "PARACETAMOL 500 mg Tablets",                         
                              "PARACETAMOL 500 mg Tablets 32 Tablet Pack" = "PARACETAMOL 500 mg Tablets")) %>% 
  group_by( location, item_merged) %>%
  summarise(jan_23 = sum(jan_23),
            feb_23 = sum(feb_23),
            mar_23 = sum(mar_23),
            apr_23 = sum(apr_23),
            may_23 = sum(may_23),
            jun_23 = sum(jun_23),
            jul_23 = sum(jul_23),
            aug_23 = sum(aug_23),
            sep_23 = sum(sep_23),
            oct_23 = sum(oct_23),
            nov_23 = sum(nov_23),
            dec_23 = sum(dec_23))%>% 
  ungroup() %>% 
  pivot_longer(cols = !c(location, item_merged), 
               names_to = "Month",
               values_to = "cost") %>% 
  group_by(location,
           item_merged,
           Month) %>% 
  mutate(Month = as.Date(paste0("01_", Month), format = "%d_%b_%y")) 





wards_value <- value %>% 
  filter(location %in% c("Ward E323 John Ray [BRM]",
                         "Ward E322 Mayflower [BRM]",
                         "Ward E321 Billericay [BRM]",
                         "Ward E320 Stock [BRM]" ,
                         "Ward E226 General Intensive Care Unit [GICU]",
                         "Ward C452 Day Therapies [BRM]",
                         "Ward E122 Pegasus [BRM]",
                         "Ward A203 Gosfield [BRM]",                                
                         "Ward A211 General High Dependancy Unit [BRM]",            
                         "Ward A301 Tadsu [BRM]",                                   
                         "Ward A301 Wizard [BRM]",                                  
                         "Ward A304 Rayne [BRM]",
                         "Ward A303 Heybridge [BRM]",
                         "Ward A402 Delivery Suite (Labour ward) [BRM]",            
                         "Ward A404 Antenatal Ward [BRM]",                          
                         "Ward A405 Postnatal Ward [BRM]",
                         "Ward B448 Day Stay Theatre [BRM]",                        
                         "Ward B448 Day Surgical Recovery [BRM]",
                         "Main Theatre Recovery [BRM]",
                         "Ward A301 Tadsu [BRM]",
                         "Main Theatres [BRM]"
  ))

theatres_value <- value %>% 
  filter(location %in% c("Main Theatres [BRM]")) 


wards_Value_plot<-
  ggplot(wards_value, aes(x = Month, y = cost, group = item_merged, color = item_merged)) +
  geom_point() +
  #geom_line()+
  geom_line() +
  labs(x = "Month", y = "Cost (£)") +
  facet_wrap(~ location) +
  theme_minimal() +
  scale_color_discrete(name = "Drug") +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.key.size = unit(0.1, "cm"))




theatres_plot<-
  ggplot(theatres_value, aes(x = Month, y = cost, group = item_merged, color = item_merged)) +
  geom_point() +
  geom_line()+
 
  labs(x = "Month", y = "Cost (£)") +
  theme_minimal() +

  scale_color_discrete(name = "Drug") +
  theme(axis.text.x = element_text(angle = 90))
ggsave("theatres 2023.pdf", width = 397, height = 210, units = "mm",  dpi = 300)

