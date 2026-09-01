### PURPOSE: Create an interactive map of coal plants in the US in HTML widget form
### LAST EDITED: 7/30/2026

library(ggplot2)
library(viridis)
library(maps)
library(mapproj)
library(usmap)
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(usdata)
library(data.table)
library(purrr)
library(leaflet)
library(leaflegend)
library(htmlwidgets)
library(htmltools)
library(foreign)
library(base64enc)

### Setting directories
raw860wd <- "Data/Raw EIA 860"
cleanwd  <- "Data/Clean Data"
plotwd   <- "Plots"

################################################################################
### EIA 860
################################################################################
### Cleaning 860 generator data on generator fuel type (for identifying coal plants)
generator860 <- list()
years <- c(2001:2024)
for (i in 1:length(years)) {
  yr <- years[i]
  ## Setting spreadsheet filenames
  if (yr>=2013) {
    folderprefix <- ""
    fileprefix <- "3_1_Generator_Y"
    filesuffix <- yr
  } else if (yr==2012 | yr==2011) {
    folderprefix <- ""
    fileprefix <- "GeneratorY"
    filesuffix <- yr
  } else if (yr==2010) {
    folderprefix <- ""
    fileprefix <- "GeneratorsY"
    filesuffix <- yr
  } else if (yr==2009) {
    folderprefix <- ""
    fileprefix <- "GeneratorY"
    filesuffix <- substr(yr, 3, 4)
  } else if (yr<=2008 & yr>=2004) {
    folderprefix <- ""
    fileprefix <- "GenY"
    filesuffix <- substr(yr, 3, 4)
  } else if (yr<=2003 & yr>=2001) {
    folderprefix <- ""
    fileprefix <- "GENY"
    filesuffix <- substr(yr, 3, 4)
  }
  ## Setting spreadsheet file types
  if (yr>=2011) {
    filetype <- ".xlsx"
  } else if ((yr<=2010 & yr>=2004) | yr<=2000) {
    filetype <- ".xls"
  } else if (yr<=2003 & yr>=2001) {
    filetype <- ".dbf"
  }
  ## Setting spreadsheet sheet names
  if (yr>=2012) {
    sheetname <- "Operable"
  } else if (yr==2011) {
    sheetname <- "operable"
  } else if (yr<=2010 & yr>=2009) {
    sheetname <- "Exist"
  } else if (yr<=2008 & yr>=2004) {
    sheetname <- paste0("GenY", substr(yr, 3, 4))
  } else if (yr<=2003 & yr>=2001) {
    sheetname <- paste0("GENY", substr(yr, 3, 4))
  }
  ## Setting spreadsheet skip lengths
  if (yr>=2011) {
    skips <- 1
  } else if (yr<=2010) {
    skips <- 0
  }
  ## Setting variables to collect
  if (yr>=2012) {
    vars <- c(
      "plant_id" = "Plant Code",
      "generator_id" = "Generator ID",
      "status" = "Status",
      "energysource_1" = "Energy Source 1",
      "energysource_2" = "Energy Source 2",
      "energysource_3" = "Energy Source 3",
      "energysource_4" = "Energy Source 4",
      "energysource_5" = "Energy Source 5",
      "energysource_6" = "Energy Source 6"
    )
  } else if (yr<=2011 & yr>=2009) {
    vars <- c(
      "plant_id" = "PLANT_CODE",
      "generator_id" = "GENERATOR_ID",
      "status" = "STATUS",
      "energysource_1" = "ENERGY_SOURCE_1",
      "energysource_2" = "ENERGY_SOURCE_2",
      "energysource_3" = "ENERGY_SOURCE_3",
      "energysource_4" = "ENERGY_SOURCE_4",
      "energysource_5" = "ENERGY_SOURCE_5",
      "energysource_6" = "ENERGY_SOURCE_6"
    )
  } else if (yr<=2008 & yr>=2007) {
    vars <- c(
      "plant_id" = "PLNTCODE",
      "generator_id" = "GENCODE",
      "status" = "STATUS",
      "energysource_1" = "ENERGY_SOURCE_1",
      "energysource_2" = "ENERGY_SOURCE_2",
      "energysource_3" = "ENERGY_SOURCE_3",
      "energysource_4" = "ENERGY_SOURCE_4",
      "energysource_5" = "ENERGY_SOURCE_5",
      "energysource_6" = "ENERGY_SOURCE_6"
    )
  } else if (yr<=2006 & yr>=2004) {
    vars <- c(
      "plant_id" = "PLNTCODE",
      "generator_id" = "GENCODE",
      "status" = "STATUS",
      "energysource_1" = "ENERGY_SOURCE_1",
      "energysource_2" = "ENERGY_SOURCE_2",
      "energysource_3" = "ENERGY_SOURCE_3",
      "energysource_4" = "ENERGY_SOURCE_4",
      "energysource_5" = "ENERGY_SOURCE_5",
      "energysource_6" = "ENERGY_SOURCE_6"
    )
  } else if (yr<=2003) {
    vars <- c(
      "plant_id" = "PLNTCODE",
      "generator_id" = "GENCODE",
      "status" = "STATUS",
      "energysource_1" = "ENSOURCE1",
      "energysource_2" = "ENSOURCE2",
      "energysource_3" = "ENSOURCE3",
      "energysource_4" = "ENSOURCE4",
      "energysource_5" = "ENSOURCE5",
      "energysource_6" = "ENSOURCE6"
    )
  }
  ## Reading in data and cleaning it
  print(paste0("Working on ", yr))
  filepath <- file.path(raw860wd, yr, paste0(fileprefix, filesuffix, filetype))
  if (yr>=2004 | yr<=2000) {
    data <- read_excel(path = filepath, sheet = sheetname, skip = skips, col_names = T)
  } else if (yr<=2003 & yr>=2001) {
    # Check if the file physically exists on the runner to catch casing issues early
    if(!file.exists(filepath)) {
      stop(paste("CRITICAL ERROR: The runner cannot locate the file at:", filepath))
    }
    # Open using a normalized string connection to bypass spaces-in-folder bugs
    data <- read.dbf(file = normalizePath(filepath, mustWork = TRUE))
  }
  data <- data %>%
    select(all_of(vars)) %>%
    rename_with(
      ~ tolower(
        gsub(pattern = " ", replacement = "_", x = .x)
      )
    ) %>%
    pivot_longer(
      cols = starts_with("energysource"),
      names_to = c(".value", "sourcenum"),
      names_sep = "_"
    ) %>%
    filter(!is.na(energysource)) %>%
    mutate(energysource = as.character(energysource)) %>%
    mutate(
      energysource = if_else(
        energysource %in% c("ANT", "BIT", "LIG", "RC", "SUB", "WC"), "CO", energysource
      )
    ) %>%
    mutate(
      year = yr,
      across(
        .cols = c(plant_id, energysource),
        .fns = as.character
      )
    ) %>%
    filter(year>=2009 | (year<=2008 & status!="RE")) %>% # Kicking out retired units in pre-2009 data; no separate sheet for operable generators.
    select(!c(status)) %>%
    select(plant_id, year, everything())
  generator860[[i]] <- data
}
rm(data, filefolder, filename, filepath, fileprefix, filesuffix, filetype, folderprefix, i, skips, years, yr)
## Combining all years of data
generator860 <- list_rbind(generator860) %>%
  group_by(plant_id, generator_id, year) %>%
  summarise(
    generatorusescoal = any(energysource=="CO")
  ) %>%
  group_by(plant_id, year) %>%
  summarise(
    hascoalgenerator = any(generatorusescoal==T)
  ) %>%
  ungroup() %>%
  arrange(plant_id, year) %>%
  group_by(plant_id) %>%
  ungroup()

### Cleaning 860 plant data on plant locations and basic characteristics
plant860 <- list()
years <- c(2001:2024)
for (i in 1:length(years)) {
  yr <- years[i]
  ## Setting spreadsheet filenames
  if (yr>=2013) {
    folderprefix <- ""
    fileprefix <- "2___Plant_Y"
    filesuffix <- yr
  } else if (yr==2012 | yr==2010) {
    folderprefix <- ""
    fileprefix <- "PlantY"
    filesuffix <- yr
  } else if (yr==2011) {
    folderprefix <- ""
    fileprefix <- "Plant"
    filesuffix <- ""
  } else if (yr<=2009 & yr>=2004) {
    folderprefix <- ""
    fileprefix <- "PlantY"
    filesuffix <- substr(yr, 3, 4)
  } else if (yr<=2003 & yr>=2001) {
    folderprefix <- ""
    fileprefix <- "PLANTY"
    filesuffix <- substr(yr, 3, 4)
  }
  ## Setting spreadsheet file types
  if (yr>=2011) {
    filetype <- ".xlsx"
  } else if ((yr<=2010 & yr>=2004) | yr<=2000) {
    filetype <- ".xls"
  } else if (yr<=2003 & yr>=2001) {
    filetype <- ".DBF"
  }
  ## Setting spreadsheet skip lengths
  if (yr>=2011) {
    skips <- 1
  } else if (yr<=2010) {
    skips <- 0
  }
  ## Setting variables to collect
  if (yr>=2013) {
    vars <- c(
      "plant_id" = "Plant Code",
      "plant_name" = "Plant Name",
      "city" = "City",
      "state" = "State",
      "lat" = "Latitude",
      "long" = "Longitude",
      "bac" = "Balancing Authority Code",
      "sectorname" = "Sector Name"
    )
  } else if (yr==2012) {
    vars <- c(
      "plant_id" = "Plant Code",
      "plant_name" = "Plant Name",
      "city" = "City",
      "state" = "State",
      "lat" = "Latitude",
      "long" = "Longitude",
      "sectorname" = "Sector Name"
    )
  } else if (yr==2011) {
    vars <- c(
      "plant_id" = "PLANT_CODE",
      "plant_name" = "PLANT_NAME",
      "city" = "CITY",
      "state" = "STATE",
      "sectorname" = "SECTOR_NAME"
    )
  } else if (yr==2010 | yr==2009) {
    vars <- c(
      "plant_id" = "PLANT_CODE",
      "plant_name" = "PLANT_NAME",
      "city" = "CITY",
      "state" = "STATE",
      "sectorname" = "SECTOR_NAME"
    )
  } else if (yr<=2008 & yr>=2007) {
    vars <- c(
      "plant_id" = "PLNTCODE",
      "plant_name" = "PLNTNAME",
      "city" = "MAIL_CITY",
      "state" = "STATE"
    )
  } else if (yr<=2006 & yr>=2004) {
    vars <- c(
      "plant_id" = "PLNTCODE",
      "plant_name" = "PLNTNAME",
      "state" = "STATE"
    )
  } else if (yr<=2003 & yr>=2001) {
    vars <- c(
      "plant_id" = "PLNTCODE",
      "plant_name" = "PLNTNAME",
      "state" = "PLNTSTATE"
    )
  }
  ## Reading in data and cleaning it
  print(paste0("Working on ", yr))
  filepath <- file.path(raw860wd, yr, paste0(fileprefix, filesuffix, filetype))
  if (yr>=2004 | yr<=2000) {
    data <- read_excel(path = filepath, skip = skips, col_names = T)
  } else if (yr<=2003 & yr>=2001) {
    if(!file.exists(filepath)) {
      stop(paste("CRITICAL ERROR: The runner cannot locate the file at:", filepath))
    }
    data <- read.dbf(file = normalizePath(filepath, mustWork = TRUE))
  }
  data <- data %>%
    select(all_of(vars)) %>%
    rename_with(
      ~ tolower(
        gsub(pattern = " ", replacement = "_", x = .x)
      )
    ) %>%
    mutate(year = yr) %>%
    select(plant_id, year, everything())
  plant860[[i]] <- data
}
rm(data, filefolder, filename, filepath, fileprefix, filesuffix, filetype, folderprefix, i, skips, years, yr)
## Combining all years of data, keeping only coal plants, and backfilling data for years with missing data
## when a plant appears in later years for which the data is available
  # E.g., assigning a plant's city or BAC code in 2005 (when neither city or BAC is available in the raw 2005 
  # 860 data) with its city and BAC code reported in 2015 (or whatever its earliest available year of city/BAC 
  # data is)
plant860 <- list_rbind(plant860) %>%
  arrange(plant_id, year) %>%
  group_by(plant_id) %>%
  mutate(
    maxyear = max(year)
  ) %>%
  tidyr::fill(
    c(city, bac, lat, long, sectorname),
    .direction = "up"
  ) %>%
  ungroup() %>%
  mutate(
    across(
      .cols = c(lat, long),
      .fns = as.numeric
    ),
    across(
      .cols = c(plant_id),
      .fns = as.character
    )
  ) %>%
  left_join(
    generator860, 
    by = c("plant_id", "year")
  ) %>%
  filter(!is.na(hascoalgenerator) & !is.na(lat))
# NOTE: Plants that have missing hascoalgenerator are not in generator data despite being in plant data.
# NOTE: Plants that have missing coordinates only appear in 860 plant data prior to 2012 (which is the
# first year when coordinates were reported); they do not appear in any year from 2012 or later.

################################################################################
### Loading Charles' plant data
################################################################################
path <- paste(cleanwd, "cbs infographic data.xlsx", sep = "/")
### Active plants data
cbsactiveplants <- read_xlsx(
  path = path,
  sheet = "Active plants",
  col_names = T
) %>%
  rename_with(
    ~ tolower(
      gsub(pattern = " ", replacement = "_", x = .x)
    )
  ) %>%
  rename(
    plantname = plantname_gen, "coalprice" = "coal_price_($/mmbtu)", "coalquantity" = "coal_quantity_(mmbtu)",
    "electricityprice" = "electricty_price_($/mmbtu)", 
    "electricityquantity" = "quantity_electricity_produced_(mmbtu)", coalfuelefficiency = coal_fuel_efficiency,
    "ngprice" = "natural_gas_price_($/mmbtu)", "coalcapacity" = "coal_capacity_(mw)",
    totalswitchcost = total_switching_cost, switchcost_permw = switching_cost_per_mw,
    switchtogas = switch_to_gas, switchdecisionpolicy = switching_decision_sensitive_to_policy,
    "dontswitchtogas" = "don't_switch_to_gas", totalpredretirecosts = total_predicted_retirement_costs,
    "predtimetoretire" = "predicted_time_to_retirement_(years)"
  )
### Switched plants data
cbsswitchedplants <- read_xlsx(
  path = path,
  sheet = "Switched to natural gas ",
  col_names = T
) %>%
  rename_with(
    ~ tolower(
      gsub(pattern = " ", replacement = "_", x = .x)
    )
  ) %>%
  rename(
    plantname = plantname_gen, yearswitched = year_switched, "coalprice" = "coal_price_($/mmbtu)",
    "coalquantity" = "coal_quantity_(mmbtu)", "electricityprice" = "electricty_price_($/mmbtu)",
    "electricityquantity" = "quantity_electricity_produced_(mmbtu)", coalfuelefficiency = coal_fuel_efficiency,
    "ngprice" = "natural_gas_price_($/mmbtu)", "prevcoalcapacity" = "previous_coal_capacity_(mw)",
    totalswitchcost = total_switching_cost, switchcost_permw = switching_cost_per_mw
  )
### Retired plants data
cbsretiredplants <- read_xlsx(
  path = path,
  sheet = "Retired plants",
  col_names = T
) %>%
  rename_with(
    ~ tolower(
      gsub(pattern = " ", replacement = "_", x = .x)
    )
  ) %>%
  rename(
    plantname = plantname_gen, 
    "yearretiredrange" = "range_of_year_retired_-_range_is_based_on_min-max_year_generators_at_the_plant_retired",
    "coalprice" = "coal_price_($/mmbtu)_-_this_is_fbar_from_the_retirement_model",
    "electricityprice" = "electricty_price_($/mmbtu)_-_this_is_ebar_from_the_retirement_model",
    "plantcoalcapacity" = "plant_sum_coal_capacity_-_nameplate_capacity_(mw)",
    "planttotalretirecost" = "plant_total_retirement_cost_-_this_is_k_from_the_retirement_model_summed_at_the_plant_level"
  )

################################################################################
### Mapping
################################################################################
## Merging in additional plant information (including coordinates!) from 860 to each dataframe
newestplantdata <- plant860 %>%
  rename(plantcode = plant_id) %>%
  group_by(plantcode) %>%
  filter(year==max(year)) %>%
  ungroup() %>%
  mutate(
    plantcode = as.numeric(plantcode)
  ) %>%
  select(!c(year, plant_name, maxyear, hascoalgenerator))
activeplants <- left_join(cbsactiveplants, newestplantdata, by = "plantcode") %>%
  rename(capacity = coalcapacity)
switchedplants <- left_join(cbsswitchedplants, newestplantdata, by = "plantcode") %>%
  rename(capacity = prevcoalcapacity)
retiredplants <- left_join(cbsretiredplants, newestplantdata, by = "plantcode") %>%
  rename(capacity = plantcoalcapacity)

## Creating dataframes for mapping (accounting for overlaps between active and retired dataframes and switched and retired
## dataframes)
activemap <- activeplants %>%
  left_join(retiredplants %>% select(plantcode, yearretiredrange, planttotalretirecost), by = "plantcode") %>%
  filter(plantcode!=56786) # Removing Spiritwood Station plant, which is the only one that appears in both activeplants and
                           # switchedplants by virtue of having only one generator that switches from coal primary to gas
                           # primary (coal secondary).
switchedmap <- switchedplants %>%
  left_join(retiredplants %>% select(plantcode, yearretiredrange, planttotalretirecost), by = "plantcode")
retiredmap <- retiredplants %>%
  anti_join(activeplants, by = "plantcode") %>%
  anti_join(switchedplants, by = "plantcode")

## Forming custom dynamic marker border colors for active plants that vary with their switching status
min_pixel_radius <- 4
max_pixel_radius <- 25
activemap <- activemap %>%
  mutate(bordercolor = case_when(
    switchtogas==1 ~ "#00BFFF",
    switchdecisionpolicy==1 ~ "#800080",
    dontswitchtogas==1 ~ "#000000",
    T   ~ "#808080"
  ))

## Creating label generation function for plant map points
# Rounding and formatting function
format_num <- function(val) {
  if (is.null(val)) return("N/A")
  ifelse(is.na(val), "N/A", formatC(as.numeric(val), digits = 2, format = "f", big.mark = ","))
}
# Label generation function
generate_labels <- function(df, statustitle) {
  if (nrow(df) == 0) return(list(popup = character(0), hover = list()))
  # Variables present in all dataframes
  base_html <- paste0(
    "<strong>Plant Name:</strong> ", df$plantname, "<br/>",
    "<strong>Sector:</strong> ", df$sectorname, "<br/>",
    "<strong>Location:</strong> ", df$city, ", ", df$state, "<br/>",
    "<strong>Balancing Authority:</strong> ", df$bac, "<br/>",
    "<strong>Capacity:</strong> ", format_num(df$capacity), " MW<br/>"
  )
  # Dataframe-specific variables
  specific_html <- switch(statustitle,
                          "Active" = paste0(
                            "<strong>Status:</strong> Active<br/>",
                            "<strong>Coal Price:</strong> ", format_num(df$coalprice), " $/MMBtu<br/>",
                            "<strong>Coal Quantity:</strong> ", format_num(df$coalquantity), " MMBtu<br/>",
                            "<strong>Coal Fuel Efficiency:</strong> ", format_num(df$coalfuelefficiency), "<br/>",
                            "<strong>Natural Gas Price:</strong> ", format_num(df$ngprice), " $/MMBtu<br/>",
                            "<strong>Electricity Price:</strong> ", format_num(df$electricityprice), " $/MMBtu<br/>",
                            #"<strong>Electricity Quantity:</strong> ", format_num(df$electricityquantity), " MMBtu<br/>",
                            "<strong>Total Switching Cost:</strong> ", format_num(df$totalswitchcost), " $<br/>",
                            "<strong>Switching Cost per MW:</strong> ", format_num(df$switchcost_permw), " $<br/>",
                            "<strong>Predicted Retirement Costs:</strong> $", format_num(df$totalpredretirecosts), "<br/>",
                            "<strong>Predicted Time to Retirement:</strong> ", format_num(df$predtimetoretire)
                          ),
                          "Switched" = paste0(
                            "<strong>Status:</strong> Switched<br/>",
                            "<strong>Year Switched:</strong> ", df$yearswitched, "<br/>",
                            "<strong>Coal Price:</strong> ", format_num(df$coalprice), " $/MMBtu<br/>",
                            "<strong>Coal Quantity:</strong> ", format_num(df$coalquantity), " MMBtu<br/>",
                            "<strong>Coal Fuel Efficiency:</strong> ", format_num(df$coalfuelefficiency), "<br/>",
                            "<strong>Natural Gas Price:</strong> ", format_num(df$ngprice), " $/MMBtu<br/>",
                            "<strong>Electricity Price:</strong>" , format_num(df$electricityprice), " $/MMBtu<br/>",
                            #"<strong>Electricity Quantity:</strong> ", format_num(df$electricityquantity), "MMBtu<br/>",
                            "<strong>Total Switching Cost:</strong> ", format_num(df$totalswitchcost), " $<br/>",
                            "<strong>Switching Cost per MW:</strong> ", format_num(df$switchcost_permw), " $"
                          ),
                          "Retired" = paste0(
                            "<strong>Status:</strong> Retired<br/>",
                            "<strong>Retirement Year Range:</strong> ", df$yearretiredrange, "<br/>",
                            "<strong>Coal Price:</strong> ", format_num(df$coalprice), " $/MMBtu<br/>",
                            "<strong>Electricity Price:</strong> ", format_num(df$electricityprice), " $/MMBtu<br/>",
                            "<strong>Total Retirement Cost:</strong>", format_num(df$planttotalretirecost), " $"
                          ),
                          stop("Unknown dataframe type inside generate_labels function.")
  )
  # Handling overlaps between active/switched and retired dataframes
  overlap_html <- ""
  if (statustitle %in% c("Active", "Switched")) {
    if ("planttotalretirecost" %in% names(df)) {
      overlap_html <- paste0(
        overlap_html, 
        ifelse(!is.na(df$planttotalretirecost), 
               paste0("<br/><strong>Total Retirement Cost:</strong> $", df$planttotalretirecost), "")
      )
    }
    if ("yearretiredrange" %in% names(df)) {
      overlap_html <- paste0(
        overlap_html, 
        ifelse(!is.na(df$yearretiredrange), 
               paste0("<br/><strong>Retired Year Range:</strong> ", df$yearretiredrange), "")
      )
    }
  }
  # Combining everything together into one popup and hover label text block
  popup_text <- paste0(base_html, specific_html, overlap_html)
  hover_text <- lapply(popup_text, htmltools::HTML)
  return(list(popup = popup_text, hover = hover_text))
}
labelsactive   <- generate_labels(activemap, "Active")
labelsswitched <- generate_labels(switchedmap, "Switched")
labelsretired  <- generate_labels(retiredmap, "Retired")

## Mapping (separate layer buttons for the three types of active plants; leaflet requires separate dataframes for this)
# Capacity scaling function
all_capacities <- c(activemap$capacity, switchedmap$capacity, retiredmap$capacity)
capacity_domain <- range(all_capacities, na.rm = TRUE)
get_radius <- function(caps) {
  scales::rescale(caps, to = c(min_pixel_radius, max_pixel_radius), from = capacity_domain)
}
# HTML group labels for blending plant type legend and layer control buttons
lbl_gas <- paste0(
  "<div style='display:inline-block; width:12px; height:12px; background:#2E7D32; ",
  "border:3px solid #00BFFF; border-radius:50%; margin-right:5px;'></div>", 
  "Active Plants - Some Coal Generators Switch to Gas"
)
lbl_policy <- paste0(
  "<div style='display:inline-block; width:12px; height:12px; background:#2E7D32; ",
  "border:3px solid #800080; border-radius:50%; margin-right:5px;'></div>", 
  "Active Plants - Generator Coal to Gas Switch Depends on Policy"
)
lbl_dont <- paste0(
  "<div style='display:inline-block; width:12px; height:12px; background:#2E7D32; ",
  "border:3px solid #000000; border-radius:50%; margin-right:5px;'></div>", 
  "Active Plants - No Coal Generators Switch to Gas"
)
lbl_switched <- paste0(
  "<div style='display:inline-block; width:12px; height:12px; background:#FFA500; ",
  "border:1px solid #FFFFFF; border-radius:50%; margin-right:5px;'></div>", 
  "Switched Plants - All Coal Generators Switched to Gas"
)
lbl_retired <- paste0(
  "<div style='display:inline-block; width:12px; height:12px; background:#FF0000; ",
  "border:1px solid #FFFFFF; border-radius:50%; margin-right:5px;'></div>", 
  "Retired Plants - All Coal Generators Retired"
)
# Map
map <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  #addProviderTiles(providers$Esri.WorldStreetMap) %>%
  #addProviderTiles(providers$CartoDB.Positron) %>%
  #addProviderTiles(providers$CartoDB.Voyager) %>% # NOTE: This option has been confirmed to now require an API key (subscription-based)
  # Layer 1A: Active plants switch to gas
  addCircleMarkers(
    data = activemap %>% filter(switchtogas == 1),
    lng = ~long, lat = ~lat, 
    group = lbl_gas,
    radius = ~get_radius(capacity),
    fillColor = "#2E7D32", 
    fillOpacity = 0.8,
    color = "#00BFFF",                             
    weight = 3, opacity = 1,
    popup = labelsactive$popup[activemap$switchtogas==1],
    label = labelsactive$hover[activemap$switchtogas==1]
  ) %>%
  # Layer 1B: Active plants switching depends on policy
  addCircleMarkers(
    data = activemap %>% filter(switchdecisionpolicy == 1),
    lng = ~long, lat = ~lat, 
    group = lbl_policy,
    radius = ~get_radius(capacity),
    fillColor = "#2E7D32", 
    fillOpacity = 0.8,
    color = "#800080",
    weight = 3, 
    opacity = 1,
    popup = labelsactive$popup[activemap$switchdecisionpolicy == 1],
    label = labelsactive$hover[activemap$switchdecisionpolicy == 1]
  ) %>%
  # Layer 1C: Active plants don't switch to gas
  addCircleMarkers(
    data = activemap %>% filter(dontswitchtogas == 1),
    lng = ~long, lat = ~lat, 
    group = lbl_dont,
    radius = ~get_radius(capacity),
    fillColor = "#2E7D32", 
    fillOpacity = 0.8,
    color = "#000000",
    weight = 3, 
    opacity = 1,
    popup = labelsactive$popup[activemap$dontswitchtogas == 1],
    label = labelsactive$hover[activemap$dontswitchtogas == 1]
  ) %>%
  # Layer 2: Switched plants (orange circles, radius by capacity)
  addCircleMarkers(
    data = switchedmap, lng = ~long, lat = ~lat, 
    group = lbl_switched, # (Switched All Coal Generators to Gas)
    radius = ~get_radius(capacity),
    fillColor = "#FFA500", 
    fillOpacity = 0.8, 
    color = "#FFFFFF", 
    weight = 1,
    popup = labelsswitched$popup, 
    label = labelsswitched$hover
  ) %>%
  # Layer 3: Retired (only) plants (red circles, radius by capacity)
  addCircleMarkers(
    data = retiredmap, lng = ~long, lat = ~lat, 
    group = lbl_retired, # (Retired All Coal Generators)
    radius = ~get_radius(capacity),
    fillColor = "#FF0000", 
    fillOpacity = 0.8, 
    color = "#FFFFFF", 
    weight = 1,
    popup = labelsretired$popup, 
    label = labelsretired$hover
  ) %>%
  # Interactive layer control panel
  addLayersControl(
    overlayGroups = c(
      lbl_gas, 
      lbl_policy, 
      lbl_dont, 
      lbl_switched, 
      lbl_retired
    ),
    options = layersControlOptions(collapsed = FALSE),
    position = "bottomleft"
  ) %>%
  # Capacity legend (marker size)
  addLegendSize(
    values = all_capacities,
    shape = "circle",
    minSize = min_pixel_radius*2,
    maxSize = max_pixel_radius*2,  
    color = "#FFFFFF",
    fillColor = "#808080", 
    title = "Plant Capacity (MW)",
    position = "bottomleft",           
    breaks = 4
  )
map
saveWidget(map, file = paste(plotwd, "index.html", sep = "/"), selfcontained = TRUE)
