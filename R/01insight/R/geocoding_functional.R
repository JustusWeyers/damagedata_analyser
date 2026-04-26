
### geocoding_functional.R

# This file serves somehow as the functional abstract from geocoding_insight.Rmd 
# where the functionality is explained more or less in detail.

addresses = function(dataset) {
  
  # Dictionary Bundeslaender
  bundeslaender = setNames(
    c(
      "Baden-Württemberg", "Bayern", "Berlin", "Brandenburg", "Bremen", 
      "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen", 
      "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen", 
      "Sachsen-Anhalt", "Schleswig-Holstein", "Thüringen"
    ),  c(
      "BW", "BY", "BE", "BB", "HB", "HH", "HE", "MV", "NI", "NW", "RP", "SL",
      "SN", "ST", "SH", "TH"
    ))
  
  # Create a working dataset with necessary columns
  working_dataset = dataset |>
    tidyr::unite("Bruecke", ort, baujahr, sep = "_", remove = FALSE) |> 
    dplyr::select(Bruecke, ort, lage_bl) # |> 
    # tidyr::drop_na()
  
  # Create api search string
  addresses = paste0(
      "'", working_dataset$Bruecke, "'", " in ", working_dataset$ort, " (", 
      unname(bundeslaender[working_dataset$lage_bl]), ", Deutschland)"
    )
  
  return(addresses)

}


geocoding = function(addresses, api_key) {
  if (!is.na(api_key)) {

    # Search
    res = data.frame(address = addresses) |>
      # Tinygeocoder
      tidygeocoder::geocode(
        address = address,
        method = "here",
        full_results = TRUE
      ) |>
      # Select only caracter and numerical columns
      dplyr::select(dplyr::where(~ is.character(.) | is.numeric(.)))

    return(res)

  } else {
    print("No api key supplied/no alternative api defined")
    return(NULL)
  }
}
