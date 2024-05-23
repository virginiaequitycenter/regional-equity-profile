# Datacode R scripts

## data_sources.R

* Pulls data from: ACS, County health rankings, CDC/NCHS, United Way/ALICE
  
* Saves downloaded files: health_rankings.xlsx, tract_expectancy.xlsx, alice_va.xlsx

* Exports processed data files for visualization: demographic_table.csv, sex_age.csv, ahdi_table.csv, tract_ahdi.csv, geographic_education.csv, tract_enroll.csv, student_data.csv, origins.Rda,  alice_alb_hhs.csv, alice_thresh.csv, med_inc_tract.csv, gini_index.csv, gini_index_all.csv, housing_costs.csv

## ahdi_visuals.R

* Reads: ahdi_table.csv, tract_names.csv, tract_ahdi.csv
  
* Creates: ahdi_table.html, ahdi_map.jpg, ahdi_dots_just_.jpg

## demographic_visuals.R

* Reads: demographic_table.csv, albco_profile_raceovertime.xlsx, origins.Rda
  
* Creates: race_1790_2010.jpg, race_poc_2000_2019.jpg, year_age.jpg, sex_age.jpg, citizen.jpg, origin.jpg

## health_visuals.R 

* Reads: tract_names.csv, tract_ahdi.csv, race_exp.csv, health_rankings.xlsx, food_insecurity.csv

* Creates: life_exp.jpg, life_exp_map.jpg, food_insecure.jpg, food_insecure_icon.jpg

## snap_data.R 

* Pulls data from: ACS, USDA

* Creates: snap_locations_map.jpg

## education_visuals.R

* Reads: education_distrbution.csv, geographic_education.csv, tract_enroll.csv, student_data.csv

* Creates: ed_race.jpg, ed_map.jpg, bach_deg_graph.jpg, enroll_map.jpg, suspended.jpg, absent.jpg, ap.jpg

## living_standards_visuals.R

* Reads: housing_costs.csv, alice_alb_hhs.csv, alice_thresh.csv, med_inc_tract.csv, gini_index.csv, gini_index_all.csv

* Creates: housing_costs.jpg, alice_hhs.jpg, alice_thresh.jpg, alice_thresh_overall.jpg, hhinc_map.jpg, gini_index.jpg, gini_index_2019.jpg

## tract_demo_table.R

* Pulls data from: ACS, reads tract_names.csv

* Creates: supplemental_tract_demo.pdf

## profile_visuals.R

* Compilation of above code (though some pieces are missing)
