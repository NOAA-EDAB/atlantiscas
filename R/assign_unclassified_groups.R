#' assign unclassified groups to atlantis groups
#'
#' Reads species from dev branch of neus-atlantis. There are several codes in the landings data that
#' refer to unclassified species. These need to be assigned to functional groups
#' Only species remaining unclassified are unknown sharks
#'
#' @param allData Data frame
#'
#'
#' @family processData
#'
#'@export

assign_unclassified_groups <- function(allData,lbstotons = 2204.62){

  message("Assigning unclassified codes")
  landingsOfUnclassifieds <- allData |>
    dplyr::filter(is.na(Code)) |>
    dplyr::group_by(NESPP3,SPPNM) |>
    dplyr::summarise(lbs=sum(InsideLANDED),
                     .groups = "drop") |>
    dplyr::arrange(desc(lbs))

  print(landingsOfUnclassifieds)

  message("All Species with landings < 20000 lbs are removed")
  codesToRemove <- landingsOfUnclassifieds |>
    dplyr::filter(lbs<20000) |>
    dplyr::pull(NESPP3)

  allData |>
    dplyr::filter(!(NESPP3 %in% codesToRemove)) |>
    dplyr::filter(is.na(Code)) |>
    dplyr::group_by(Year,NESPP3,SPPNM) |>
    dplyr::summarise(landed = sum(InsideLANDED)/lbstotons,
                     .groups = "drop") |>
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=as.numeric(Year),y=landed,group=1)) +
    ggplot2::geom_point(ggplot2::aes(x=as.numeric(Year),y=landed)) +
    ggplot2::facet_wrap(~as.factor(SPPNM),scales = "free_y") +
    ggplot2::ggtitle("Species with unmatched Atlantis codes") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
    ggplot2::xlab("Metric Tons")

  message("Mapping: ...
          NK CLAM (764) -> SURF CLAM (769). Atlantis: CLA
          NK DOGFISH (350) -> DOGFISH SPINY (352). Atlantis: DOG
          NS SQUIDS (803) -> ILLEX SQUID (802). Atlantis: ISQ
          LITTLE/WINTER SKATE (373) -> LITTLE SKATE (366). Atlantis: LSK
          SKATES (365) -> ACROSS COMPLEX(365). Atlantis: SK
          OTHER FISH (526) -> Atlantis: FDF or BPF (Depending on gear used)")

  indclam <- allData$NESPP3 == 764
  inddog <- allData$NESPP3 == 350
  indsquid <- allData$NESPP3 == 803
  indlwskate <- allData$NESPP3 == 373
  indskate <- allData$NESPP3 == 365

  # RENAME RECORDS TO NEW CODES AND DESCRIPTIONS
  allData[indclam,"NESPP3"] <- 769
  allData[indclam,"SPPNM"] <- "SURF CLAM"
  allData[indclam,"Code"] <- "CLA"
  allData[indclam,"Functional_Group"] <- "Atlantic surf clam"
  allData[inddog,"NESPP3"] <- 352
  allData[inddog,"SPPNM"] <- "DOGFISH SPINY"
  allData[inddog,"Code"] <- "DOG"
  allData[inddog,"Functional_Group"] <- "Spiny dogfish"
  allData[indsquid,"NESPP3"] <- 802
  allData[indsquid,"SPPNM"] <- "ILLEX SQUID"
  allData[indsquid,"Code"] <- "ISQ"
  allData[indsquid,"Functional_Group"] <- "Illex squid"
  allData[indlwskate,"NESPP3"] <- 366
  allData[indlwskate,"SPPNM"] <- "LITTLE SKATE"
  allData[indlwskate,"Code"] <- "LSK"
  allData[indlwskate,"Functional_Group"] <- "Little skate"
  allData[indskate,"Code"] <- "SK"
  allData[indskate,"Functional_Group"] <- "Northeast skate complex"

  ## Other fish category
  # Based on the gear category, they will be assigned either FDF (miscellaneous demersal fish) or
  # BPF (Other Benthopelagic fish)
  message("OTHER FISH")
  benthicGears <- c("Bottom Trawl","Sink Gillnet","Lobster Pot","Other Pot","Bottom Longline","Scallop Gear")
  pelagicGears <- c("Drift Gillnet","Midwater Trawl","Hand Gear","Pelagic Longline","Other Gear","Shrimp Trawl","Separator & Ruhle Trawl")

  message(paste("Benthic gears:", paste(benthicGears, collapse = ", " )))
  message(paste("Pelagic gears:", paste(pelagicGears, collapse = ", " )))

  indothefishbenthic <- (allData$NESPP3 == 526) & (allData$GEARCAT %in% benthicGears)
  indothefishpelagic <- (allData$NESPP3 == 526) & (allData$GEARCAT %in% pelagicGears)

  allData[indothefishbenthic,"Code"] <- "FDF"
  allData[indothefishbenthic,"Functional_Group"] <- "Miscellaneous demersal fish"
  allData[indothefishpelagic,"Code"] <- "BPF"
  allData[indothefishpelagic,"Functional_Group"] <- "Other benthopelagic fish"

  return(allData)
}
