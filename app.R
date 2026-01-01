# For leaflet rendering
if (!require(shiny)) install.packages('shiny', repos='http://cran.us.r-project.org')
if (!require(leaflet)) install.packages('leaflet', repos='http://cran.us.r-project.org')
if (!require(tidyverse)) install.packages('tidyverse', repos='http://cran.us.r-project.org')
if (!require(sf)) install.packages('sf', repos='http://cran.us.r-project.org') 

library(tidyverse)
library(shiny)
library(sf)
library(leaflet)

df <- read.csv("/Users/shreya/Documents/ds-project/FlightDataAnalysis/data/delay2023.csv")
# Helper function to convert snake_case to camelCase
toCamelCase <- function(string) {
  # Split the string by underscores and convert to lowercase
  words <- tolower(unlist(strsplit(string, "_")))
  # Capitalize the first letter of each word except the first word
  words[-1] <- sapply(words[-1], function(word) {
    paste(toupper(substr(word, 1, 1)), substr(word, 2, nchar(word)), sep = "")
  })
  # Concatenate the words back together
  return(paste0(words, collapse = ""))
}

# Apply the function to all column names in the dataframe
if (exists("df") && is.data.frame(df)) {  # Check if 'df' exists and is a dataframe
  colnames(df) <- sapply(colnames(df), toCamelCase)
} else {
  cat("Dataframe 'df' does not exist or is not a dataframe.")
}

# Exclude canceled and diverted flights from the dataframe
df <- df %>%
  filter(cancelled == 0 & diverted == 0)

# Set Airline / Carrier data types as factor variables
df <- df %>%
  mutate(
    mktUniqueCarrier = factor(mktUniqueCarrier),
    opUniqueCarrier = factor(opUniqueCarrier),
    tailNum = factor(tailNum),
    mktCarrierFlNum = factor(mktCarrierFlNum)
  )

airlineIATA <- levels(df$mktUniqueCarrier)
# Define the mapping of IATA codes to airline names
#### Note that the variable mktUniqueCarrier in the data records the IATA code of the airline.
airlineNames <- c(
  "AA" = "American Airlines",
  "AS" = "Alaska Airlines",
  "B6" = "JetBlue Airways",
  "DL" = "Delta Air Lines",
  "F9" = "Frontier Airlines",
  "G4" = "Allegiant Air",
  "HA" = "Hawaiian Airlines",
  "NK" = "Spirit Airlines",
  "UA" = "United Airlines",
  "WN" = "Southwest Airlines"
)

# Define the top 4 airlines
top4Airlines <- c("AA", "DL", "WN", "UA")

# Add a new column 'Airline' to the dataframe and map the 'mktUniqueCarrier' to 'Airline'
df <- df %>%
  mutate(airline = airlineNames[mktUniqueCarrier],
         airlineName = if_else(mktUniqueCarrier %in% top4Airlines, airline, "Other Airlines"))

#Master list of airports with latitude and longitude information from 'https://www.transtats.bts.gov/Fields.asp?gnoyr_VQ=FLL'
airportNamesMaster <- read.csv("../data/Airports_Lat_Long.csv")
airportNamesMaster <- airportNamesMaster %>%
  select(AirportCode = AIRPORT,
         Airport_id = AIRPORT_ID,
         AirportName = DISPLAY_AIRPORT_NAME,
         Latitude = LATITUDE,
         Longitude = LONGITUDE,
         Airprot_is_latest = AIRPORT_IS_LATEST) %>%
  filter(Airprot_is_latest == 1)

all356AirportsNames <- df %>%
  filter(!is.na(dest)) %>%
  mutate(dest = factor(dest)) %>%
  group_by(dest) %>%
  summarise(
    totalFlights = n(),
    flightsArrivedLate = sum(arrDel15 == 1, na.rm = TRUE),
    percentArrivedLate = round((flightsArrivedLate / totalFlights) * 100, 1),
    .groups = 'drop'
  ) %>%
  arrange(desc(totalFlights)) %>%
  slice_max(totalFlights, n = 20) %>%
  select(dest, totalFlights, flightsArrivedLate, percentArrivedLate) %>%
  merge(airportNamesMaster, by.x = 'dest', by.y = 'AirportCode', 'left') %>%
  arrange(desc(totalFlights)) %>%
  mutate(airlineName = "Total") %>%
  select(dest, airlineName, totalFlights, flightsArrivedLate, percentArrivedLate, AirportName, Latitude, Longitude)

nrow(all356AirportsNames)
print(all356AirportsNames)
airportNames20 <- unique(all356AirportsNames$dest)


all356Airports <- df %>%
  filter(!is.na(dest)) %>%
  mutate(dest = factor(dest)) %>%
  group_by(dest, airlineName) %>%
  summarise(
    totalFlights = n(),
    flightsArrivedLate = sum(arrDel15 == 1, na.rm = TRUE),
    percentArrivedLate = round((flightsArrivedLate / totalFlights) * 100, 1),
    .groups = 'drop'
  ) %>%
  filter(dest %in% airportNames20) %>%
  select(dest, airlineName, totalFlights, flightsArrivedLate, percentArrivedLate) %>%
  merge(airportNamesMaster, by.x = 'dest', by.y = 'AirportCode', 'left')

all356Airports <- bind_rows(all356Airports, all356AirportsNames) 
all356Airports <- all356Airports %>%
                        group_by(dest) %>%
                        mutate(rank = if_else(airlineName == "Total", max(if_else(!is.na(totalFlights), row_number(desc(totalFlights)), -Inf), na.rm = TRUE) + 1,
                        if_else(!is.na(totalFlights), row_number(totalFlights), -Inf))) %>%
                        arrange(dest, rank) %>%
                        select(-rank)
print(all356Airports, n = 1000)

str(all356Airports)

ui = fluidPage(
  titlePanel("Airports Map"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(selectInput('Airline', 'Select Airline', choices = list(
      "American Airlines",
      "Delta Air Lines",
      "United Airlines",
      "Southwest Airlines",
      "Total"),
      selected = "Total"),
    helpText(HTML("The size of the circle represents the total flights arriving at the airport.<br>
             The color of the circle represents the percentage of delayed arrivals.<br>
             Red indicates a higher percentage of delayed arrivals, and yellow indicates a lower percent of delayed arrivals.<br>
             Roll over a circle with your mouse to see the details for that airport."))),
    mainPanel = mainPanel(
      h3("Flight Delays at top-20 US Busy Airports"),
      leafletOutput(outputId = 'map')))
)

server = function(input, output, session){
  
  # Set default value for selectInput
  observe({
    updateSelectInput(session, "Airline", selected = "Total")
  })
  
  airport_locations = reactive({
    filtered_airports <- filter(all356Airports, !is.na(Latitude) & !is.na(Longitude) & airlineName == input$Airline)

    
    if (nrow(filtered_airports) == 0) {
        return(NULL)  # Handle case where no valid airports are found
      }
      
    filtered_airports %>%
      st_as_sf(coords = c('Longitude', 'Latitude')) %>%
      st_set_crs(4326)
  })
  
  output$map = renderLeaflet({
    req(airport_locations())  # Ensure airport_locations is not NULL
    leaflet() %>%
      addTiles() %>%
      setView(lat = 39.8097343, lng =-98.5556199, zoom = 4) %>%
      addCircleMarkers(data = airport_locations(), radius = ~totalFlights / 400, 
                       color = ~colorNumeric("YlOrRd", percentArrivedLate)(percentArrivedLate),
                       fillOpacity = 0.7, # Adjust fill opacity if needed
                       stroke = TRUE,  # Enable stroke
                       weight = 1,  # Stroke width
                       label = ~paste("Airport:", AirportName, "<br>",
                                                     "Total Flights: ", totalFlights, "<br>",
                                                     "Percent Arrived Late: ", percentArrivedLate, "%")
                                                      %>% lapply(htmltools::HTML)
      )
  })
}
shinyApp(ui, server)
