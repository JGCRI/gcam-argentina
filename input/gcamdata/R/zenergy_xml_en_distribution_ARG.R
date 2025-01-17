# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_en_distribution_ARG_xml
#'
#' Construct XML data structure for \code{en_distribution_ARG.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_distribution_ARG.xml}. The corresponding file in the
#' original data system was \code{batch_en_distribution_ARG.xml.R} (energy XML).
module_energy_en_distribution_ARG_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L226.Supplysector_en",
              "L226.SubsectorLogit_en",
              "L226.SubsectorShrwt_en",
              "L226.SubsectorShrwtFllt_en",
              "L226.SubsectorInterp_en",
              "L226.SubsectorInterpTo_en",
              "L226.StubTech_en",
              "L226.GlobalTechEff_en",
              "L226.GlobalTechCost_en",
              "L226.GlobalTechTrackCapital_en",
              "L226.GlobalTechShrwt_en",
              "L226.StubTechCoef_elecownuse",
              "L226.StubTechCoef_electd",
              "L226.StubTechCoef_gaspipe"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_distribution_ARG.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L226.Supplysector_en <- get_data(all_data, "L226.Supplysector_en")
    L226.SubsectorLogit_en <- get_data(all_data, "L226.SubsectorLogit_en")
    L226.SubsectorShrwt_en <- get_data(all_data, "L226.SubsectorShrwt_en")
    L226.SubsectorShrwtFllt_en <- get_data(all_data, "L226.SubsectorShrwtFllt_en")
    L226.SubsectorInterp_en <- get_data(all_data, "L226.SubsectorInterp_en")
    L226.SubsectorInterpTo_en <- get_data(all_data, "L226.SubsectorInterpTo_en")
    L226.StubTech_en <- get_data(all_data, "L226.StubTech_en")
    L226.GlobalTechEff_en <- get_data(all_data, "L226.GlobalTechEff_en")
    L226.GlobalTechCost_en <- get_data(all_data, "L226.GlobalTechCost_en")
    L226.GlobalTechTrackCapital_en <- get_data(all_data, "L226.GlobalTechTrackCapital_en")
    L226.GlobalTechShrwt_en <- get_data(all_data, "L226.GlobalTechShrwt_en")
    L226.StubTechCoef_elecownuse <- get_data(all_data, "L226.StubTechCoef_elecownuse")
    L226.StubTechCoef_electd <- get_data(all_data, "L226.StubTechCoef_electd")
    L226.StubTechCoef_gaspipe <- get_data(all_data, "L226.StubTechCoef_gaspipe")

    # ===================================================

    L226.StubTech_en_arg <- L226.StubTech_en %>%
      filter(region == "Argentina" & supplysector == "backup_electricity") %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(CO2 = "CO2_elec")

    # Produce outputs
    create_xml("en_distribution_ARG.xml") %>%
      add_logit_tables_xml(L226.Supplysector_en, "Supplysector") %>%
      add_logit_tables_xml(L226.SubsectorLogit_en, "SubsectorLogit") ->
      en_distribution_ARG.xml

    # Some data inputs may not actually contain data. If so, do not add_xml_data.
    if(!is.null(L226.SubsectorShrwt_en)) {
      en_distribution_ARG.xml %>%
        add_xml_data(L226.SubsectorShrwt_en, "SubsectorShrwt") ->
        en_distribution_ARG.xml
    }

    if(!is.null(L226.SubsectorShrwtFllt_en)) {
      en_distribution_ARG.xml %>%
        add_xml_data(L226.SubsectorShrwtFllt_en, "SubsectorShrwtFllt") ->
        en_distribution_ARG.xml
    }

    if(!is.null(L226.SubsectorInterp_en)) {
      en_distribution_ARG.xml %>%
        add_xml_data(L226.SubsectorInterp_en, "SubsectorInterp") ->
        en_distribution_ARG.xml
    }

    if(!is.null(L226.SubsectorInterpTo_en)) {
      en_distribution_ARG.xml %>%
        add_xml_data(L226.SubsectorInterpTo_en, "SubsectorInterpTo") ->
        en_distribution_ARG.xml
    }

    en_distribution_ARG.xml %>%
      add_xml_data(L226.StubTech_en, "StubTech") %>%
      add_xml_data(L226.StubTech_en_arg, "StubTechCO2") %>%
      add_xml_data(L226.GlobalTechEff_en, "GlobalTechEff") %>%
      add_node_equiv_xml("input") %>%
      add_xml_data(L226.GlobalTechTrackCapital_en, "GlobalTechTrackCapital") %>%
      add_xml_data(L226.GlobalTechCost_en, "GlobalTechCost") %>%
      add_xml_data(L226.GlobalTechShrwt_en, "GlobalTechShrwt") %>%
      add_xml_data(L226.StubTechCoef_elecownuse, "StubTechCoef") %>%
      add_xml_data(L226.StubTechCoef_electd, "StubTechCoef") %>%
      add_xml_data(L226.StubTechCoef_gaspipe, "StubTechCoef") %>%
      add_precursors("L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.StubTech_en",
                     "L226.GlobalTechEff_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechTrackCapital_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_elecownuse",
                     "L226.StubTechCoef_electd",
                     "L226.StubTechCoef_gaspipe") ->
      en_distribution_ARG.xml

    return_data(en_distribution_ARG.xml)
  } else {
    stop("Unknown command")
  }
}
