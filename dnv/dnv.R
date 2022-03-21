library("readxl")
library("xlsx")
library("tidyverse")
library("lubridate")

dnv_demand <- read_excel("C:/Users/rcepk/Google Drive/Tests/R/DNV-2021.xlsx",
                              sheet = "Demand by sectors")


rename(dnv_demand, "EnergyCarrier" = "Energy carrier")


write.xlsx(dnv_demand, file = "dnv_demand.xlsx")
dnv


dnv_demand <- as_tibble(dnv_demand)


dnv_demand$`2050` <- as.numeric(dnv_demand$`2050`)


dnv_demand_L <- pivot_longer(dnv_demand, 5:36, names_to = "Year")


dnv_demand_L_FinEnDem <- filter(dnv_demand_L, Variable == "Final Energy Demand") %>%
  select("Energy carrier", Year, value) %>%
  arrange(Year) %>%
  relocate(Year, .before = "Energy carrier")

# Grouping and summing by energy carrier
dnv_demand_L_FinEnDem_by_carrier <- dnv_demand_L_FinEnDem %>%
  group_by(Year, EnergyCarrier) %>%
  summarise(Value = sum(value))

# Make sure its a table/data frame
dnv_demand_L_FinEnDem_by_carrier <- as_tibble(dnv_demand_L_FinEnDem_by_carrier)

# Save the table
write.xlsx2(dnv_demand_L_FinEnDem_by_carrier, file="dnv_demand_L_FinEnDem_by_carrier.xlsx", sheetName = "Sheet1",
            col.names = TRUE, row.names = TRUE)

