# Install from GitHub
install.packages("remotes") \
remotes::install_github("UGA-IDD/vaccinetimeliness")

# Usage 
set DHS configuration using *rhds::set_rdhs_config()*

*getDHSdata("GH", survyear = 2022, vaccine = "MCV1", assessment = FALSE)*

*getDHSdata("GH", survyear = 2022, vaccine = "MCV1", assessment = FALSE) |> 
  survivalFit()*
