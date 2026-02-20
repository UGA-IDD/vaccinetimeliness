#' Get your survival data ready
#'
#' @param iso country specific iso code (eg. "GH")
#' @param survyear vector, year(s) of survey
#' @param vaccine character, options: MCV1, MCV2
#' @param DHS_cred_email character, provide the email used to register the DHS project
#' @param DHS_cred_project character, provide the project name for the email provided
#' @param vax_age integer, mininum vaccination age allowed
#' @param assessment boolean, TRUE to print assessment report of the data
#'
#' @return A nicely formatted dataframe for survival analysis
#' @export
#'


getDHSdata <- function(iso, survyear, vaccine, DHS_cred_email, DHS_cred_project,
                       vax_age = 0, assessment = FALSE) {

  avv <- rdhs::dhs_datasets() # loads all available data]


  the$country.iso1 <- iso
  cc_convert <- function(iso) {
    iso <- the$country.iso1

    clength <- stringr::str_length(iso)
    if (clength == 3) {
      iso <- countrycode::countrycode(iso, "iso3c", "iso2c")
    } else {
      iso
    }
    return(iso)
  }

  iso <- cc_convert(iso)
  the$country.iso2 <- iso

  ccodes <- dhs_country_codes
  if (iso == "NA") {
    dhscode <- ccodes |> dplyr::filter(is.na(iso2))
  } else {
    dhscode <- ccodes |> dplyr::filter(iso2 == iso)
  }
  iso <- dhscode[1, 1]

  cdata <- avv |> dplyr::filter(
    DHS_CountryCode == iso, FileType == "Individual Recode",
    SurveyType == "DHS",
    FileFormat == "Flat ASCII data (.dat)", SurveyYear %in% survyear
  )

  if (nrow(cdata) == 0) {
    stop("No data for survey year")
  }
  if (iso == "JO" & any(survyear) == 2009) stop("No data data on measles vaccination for chosen year")
  if (iso == "UA" & any(survyear) == 2007) stop("No data data on measles vaccination for chosen year")
  if (iso == "CO" & any(survyear) == 2015) stop("No data data on measles vaccination for chosen year")

  dwnload <- rdhs::get_datasets(cdata$FileName,
    download_option = "rds",
    clear_cache = T
  ) # download available data
  df <- purrr::map(dwnload, readRDS) # purr; reads downloads into a list
  df <- purrr::map(df, labelled::remove_labels)

  ### MCV1 -----------------------------------
  getDHSdata_mcv1 <- function(df) {
    df_select <- df |> dplyr::select(
      v005, v008, b3_01, h9_1, h9y_1, h9m_1, h9d_1
    )

    # afghanistan modifications ---------------------------
    if (iso == "AF") {
      df_afg1 <- df_select |> dplyr::mutate(
        h9y_1 = ifelse(h9y_1 %in% c(9997, 9998, 9999), NA, h9y_1),
        h9m_1 = ifelse(h9m_1 %in% c(97, 98, 99), NA, h9m_1),
        h9d_1 = ifelse(h9d_1 %in% c(97, 98, 99), NA, h9d_1)
      )
      df_afg2 <- df_afg1 |>
        dplyr::rowwise() |>
        dplyr::mutate(
          pdate = ifelse(!is.na(h9y_1) & !is.na(h9m_1) & !is.na(h9d_1),
            jalcal::jal2greg(h9y_1, h9m_1, h9d_1), NA
          ),
          pdate = lubridate::as_date(pdate),
          h9y_1 = lubridate::year(pdate),
          h9m_1 = lubridate::month(pdate),
          h9d_1 = lubridate::day(pdate)
        )
    }
    # ---------------------------------------------------------------------

    if (iso == "AF") {
      df_select <- df_afg1
    } else {
      df_select <- df_select
    }

    if (iso == "AF") {
      age_1_mcv <- ((df_select$h9y_1 - 1300) * 12 + df_select$h9m_1) - df_select$b3_01
    } else if (iso == "ET") {
      age_1_mcv <- ((df_select$h9y_1 - 1900) * 12 + df_select$h9m_1) - df_select$b3_01
    } else {
      age_1_mcv <- ((df_select$h9y_1 - 1900) * 12 + df_select$h9m_1) - df_select$b3_01
    }

    df_select <- df_select |> dplyr::mutate(
      wgt = v005 / 1000000, # weight variable
      age_1 = v008 - b3_01, # get age at survey
      age_1_mcv = age_1_mcv, # getting age at MCV
      Status = dplyr::case_when(
        h9_1 == 0 ~ 0,
        h9_1 == 1 ~ 1,
        h9_1 == 2 | h9_1 == 3 ~ 2
      )
    )

    if (iso == "AF") {
      df_select <- df_select |> dplyr::mutate(
        h9y_1 = df_afg2$h9y_1,
        h9m_1 = df_afg2$h9m_1,
        h9d_1 = df_afg2$h9d_1
      )
    } else {
      df_select <- df_select
    }

    df_select <- df_select |> dplyr::mutate(
      h9d_1 = ifelse(h9d_1 > 31, 15, h9d_1),
      h9d_1 = ifelse(is.na(h9d_1) & !is.na(h9m_1) & !is.na(h9y_1), 15, h9d_1),
      datedate = paste(h9y_1, h9m_1, h9d_1, sep = "-"),
      datedate = lubridate::ymd(datedate) # get date of vaccination
    )
    # Date adjustment for Nepal (iso=NP) -------------
    if (iso == "NP") {
      datenp <- df_select$datedate - months(681)
    } else if (iso == "ET") {
      datenp <- df_select$datedate + months(92)
    } else {
      datenp <- df_select$datedate
    }

    df_select <- df_select |> dplyr::mutate(
      datedate = datenp
    )

    df_final <- df_select |> dplyr::select(
      wgt, age_1, age_1_mcv, Status, datedate
    )

    # remove individuals who are not asked about vaccination
    df_final |>
      dplyr::arrange(age_1, Status) |>
      dplyr::pull(Status) -> status.out
    df_final |>
      dplyr::arrange(age_1, Status) |>
      dplyr::pull(age_1) -> age.out
    max.age <- age.out[max(which(status.out == 1 | status.out == 2 | status.out == 0))]

    df_final <- df_final |> dplyr::filter(age_1 <= max.age)

    df_final <- df_final |> dplyr::mutate(
      followup = ifelse(Status == 1, age_1_mcv, age_1)
    )
    return(df_final)
  }

  ### MCV2 ----------------------------------------------------
  getDHSdata_mcv2 <- function(df) {
    if (!"h9a_1" %in% names(df)) stop("No vaccination data for measles dose 2")
    df_select <- df |> dplyr::select(
      v005, v008, b3_01, h9a_1, h9ay_1, h9am_1, h9ad_1
    )

    # afghanistan modifications ---------------------------
    if (iso == "AF") {
      df_afg1 <- df_select |> dplyr::mutate(
        h9ay_1 = ifelse(h9ay_1 %in% c(9997, 9998, 9999), NA, h9ay_1),
        h9am_1 = ifelse(h9am_1 %in% c(97, 98, 99), NA, h9am_1),
        h9ad_1 = ifelse(h9ad_1 %in% c(97, 98, 99), NA, h9ad_1)
      )
      df_afg2 <- df_afg1 |>
        dplyr::rowwise() |>
        dplyr::mutate(
          pdate = ifelse(!is.na(h9ay_1) & !is.na(h9am_1) & !is.na(h9ad_1),
            jalcal::jal2greg(h9ay_1, h9am_1, h9ad_1), NA
          ),
          pdate = lubridate::as_date(pdate),
          h9ay_1 = lubridate::year(pdate),
          h9am_1 = lubridate::month(pdate),
          h9ad_1 = lubridate::day(pdate)
        )
    }
    # ---------------------------------------------------------------------

    if (iso == "AF") {
      df_select <- df_afg1
    } else {
      df_select <- df_select
    }

    if (iso == "AF") {
      age_1_mcv <- ((df_select$h9ay_1 - 1300) * 12 + df_select$h9am_1) - df_select$b3_01
    } else if (iso == "ET") {
      age_1_mcv <- ((df_select$h9ay_1 - 1900) * 12 + df_select$h9am_1) - df_select$b3_01
    } else {
      age_1_mcv <- ((df_select$h9ay_1 - 1900) * 12 + df_select$h9am_1) - df_select$b3_01
    }

    df_select <- df_select |> dplyr::mutate(
      age_1 = v008 - b3_01, # get age at survey
      age_1_mcv = age_1_mcv, # getting age at MCV
      Status = dplyr::case_when(
        h9a_1 == 0 ~ 0,
        h9a_1 == 1 ~ 1,
        h9a_1 == 2 | h9a_1 == 3 ~ 2
      )
    )


    if (iso == "AF") {
      df_select <- df_select |> mutate(
        h9ay_1 = df_afg2$h9ay_1,
        h9am_1 = df_afg2$h9am_1,
        h9ad_1 = df_afg2$h9ad_1
      )
    } else {
      df_select <- df_select
    }

    df_select <- df_select |> dplyr::mutate(
      h9ad_1 = ifelse(h9ad_1 > 31, 15, h9ad_1),
      h9ad_1 = ifelse(is.na(h9ad_1) & !is.na(h9am_1) & !is.na(h9ay_1), 15, h9ad_1),
      datedate = paste(h9ay_1, h9am_1, h9ad_1, sep = "-"),
      datedate = lubridate::ymd(datedate) # get date of vaccination
    )
    # Date adjustment for Nepal (iso=NP) -------------
    if (iso == "NP") {
      datenp <- df_select$datedate - months(681)
    } else if (iso == "ET") {
      datenp <- df_select$datedate + months(92)
    } else {
      datenp <- df_select$datedate
    }

    df_select <- df_select |> dplyr::mutate(
      datedate = datenp
    )

    df_final <- df_select |> dplyr::select(
      v001, wgt, age_1, age_1_mcv, Status, datedate
    )

    # remove individuals who are not asked about vaccination
    df_final |>
      dplyr::arrange(age_1, Status) |>
      dplyr::pull(Status) -> status.out
    df_final |>
      dplyr::arrange(age_1, Status) |>
      dplyr::pull(age_1) -> age.out
    max.age <- age.out[max(which(status.out == 1 | status.out == 2 | status.out == 0))]
    df_final <- df_final |> dplyr::filter(age_1 <= max.age)

    df_final <- df_final |> dplyr::mutate(
      followup = ifelse(Status == 1, age_1_mcv, age_1)
    )
  }


  if (vaccine == "MCV1") {
    out <- purrr::map(df, getDHSdata_mcv1)
  } else if (vaccine == "MCV2") {
    out <- purrr::map(df, getDHSdata_mcv2)
  }

  the$vax_age <- vax_age

  modifications <- function(dff) {
    df_final <- dff
    df_final <- df_final |> tidyr::drop_na(age_1)
    df_final <- df_final |> tidyr::drop_na(Status)
    df_final <- df_final |>
      dplyr::mutate(
        mcv_na = ifelse(is.na(age_1_mcv), -1, age_1_mcv)
      ) |>
      dplyr::filter(age_1 >= mcv_na) |>
      dplyr::select(-mcv_na) # fix for age1<=age1_mcv
    df_final <- df_final |> dplyr::filter(followup >= vax_age)
  }

  # Now return desired data after modifications
  outdisplay <- purrr::map(out, modifications)

  the$finalList <- out
  the$cdata <- cdata

  knit_reports <- function(cdata, df) {
    for (i in 1:nrow(cdata)) {
      data <- cdata[i, ]
      data1 <- df[[i]]
      report_file <- paste0("DHS_Report_", iso, survyear[i], ".html")
      render_file <- system.file("rmd", "rdhs_sample_report.Rmd", package = "timeliness")
      openweb <- rmarkdown::render(render_file,
        output_format = "html_document",
        envir = the,
        output_file = report_file, params = list(data = data, data1 = data1),
        quiet = TRUE,
        clean = TRUE
      )
      utils::browseURL(openweb)
    }
  }
  if (assessment == TRUE) {
    knit_reports(cdata, df = the$finalList)
  }

  return(outdisplay[[1]])
}
