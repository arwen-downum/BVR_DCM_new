#join phytos_

frame_weeks #has the dates that we have phyto data for plus water level, everything can be joined to this

final_RF_frame <- final_phytos #this is the frame after running 01_water_level and 02_Phytos_dataframe
#join
final_metals #from script 03
final_photic_thermo
final_buoyancy
final_chem #from 06_nutrients

full_weekly_data <- frame_weeks %>%
  left_join(final_phytos, by = c("Year", "Week")) %>%
  left_join(final_metals, by = c("Year", "Week")) %>%
  left_join(final_photic_thermo, by = c("Year", "Week")) %>%
  left_join(final_buoyancy, by = c("Year", "Week")) %>%
  left_join(final_chem, by = c("Year", "Week"))
