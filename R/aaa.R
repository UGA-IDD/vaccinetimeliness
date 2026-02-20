the <- new.env(parent = baseenv()) ## inherit from baseenv



utils::globalVariables(
  c("DHS_CountryCode", "FileFormat", "FileType", "Status", "SurveyType", "SurveyYear",
    "age_1", "age_1_mcv", "axis", "b3_01", "datedate", "followup", "h9_1", "h9a_1", "h9ad_1", "h9am_1",
    "h9ay_1", "h9d_1", "h9m_1", "h9y_1", "iso2", "lines", "mcv_na", "mutate", "n", "pdate", "prop", "v001",
    "v005", "v008", "vsum", "wgt")
)
