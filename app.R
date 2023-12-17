library(shiny)
library(httr)
library(jsonlite)
library(leaflet)
library(shinydashboard)
library(shinythemes)
library(htmlwidgets)

# Function to get Wikipedia link for a given person
getWikipediaLink <- function(name) {
  tryCatch({
    # Search for the Wikipedia page using the person's name
    wikipedia_url <- paste0("https://en.wikipedia.org/wiki/", gsub(" ", "_", name))
    # Validate if the Wikipedia page exists
    if (httr::GET(wikipedia_url)$status_code == 200) {
      return(wikipedia_url)
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  })
}

# People in space right now
astro <- httr::GET("http://api.open-notify.org/astros.json")
data <- jsonlite::fromJSON(rawToChar(astro$content))

# Live ISS location
live <- httr::GET("http://api.open-notify.org/iss-now.json")
now <- jsonlite::fromJSON(rawToChar(live$content))

# Convert latitude and longitude to numeric
now$iss_position$latitude <- as.numeric(now$iss_position$latitude)
now$iss_position$longitude <- as.numeric(now$iss_position$longitude)

ui <- fluidPage(
  theme = shinytheme("flatly"),  # Use the flatly theme for a refreshing look
  dashboardPage(
    dashboardHeader(title = "People in Space Right Now"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("About", tabName = "about", icon = icon("info"),
                 menuSubItem("About the App", tabName = "about_app"),
                 menuSubItem("About Me", tabName = "about_me")
        )
      )
    ),
    dashboardBody(
      fluidRow(
        # Display total number of people in space in a square box
        valueBox(
          value = length(data$people$name),
          subtitle = "Total People in Space",
          icon = icon("users"),
          color = "blue"  # Change 'primary' to 'blue'
        )
      ),
      
      fluidRow(
        # Heading for the table
        h3("List of People in Space", style = "color: #2c3e50;"),
        # Display the table with hyperlinked names
        dataTableOutput('iss')
      ),
      
      fluidRow(
        # Heading for the map
        h3("Live Position of International Space Station", style = "color: #2c3e50;"),
        # Display live position of the International Space Station on a map
        leafletOutput("map"),
      ),
      
      tabItems(
        tabItem(
          tabName = "about_app",
          fluidRow(
            box(
              width = 12, solidHeader = TRUE,
              title = "About the App",
              status = "info",
              p("This Shiny app provides real-time information about the people currently in space."),
              p("Explore the list of astronauts and their positions on the International Space Station.")
            )
          )
        ),
        tabItem(
          tabName = "about_me",
          fluidRow(
            box(
              width = 12, solidHeader = TRUE,
              title = "About Me",
              status = "info",
              p("I am a space enthusiast and developer, passionate about bringing information about people in space to the public."),
              p("Connect with me on LinkedIn:"),
              a("LinkedIn Profile", href = "https://www.linkedin.com/in/arungopinat/", target="_blank")
              p("Check out my portfolio"),
              a("Arun Gopinathan", href = "https://arungopinathan.com", target="_blank")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  # Get Wikipedia links for each person
  wikipedia_links <- sapply(data$people$name, getWikipediaLink)
  
  # Create a new data frame with hyperlinked names
  hyperlinked_data <- data.frame(
    name = sapply(seq_along(data$people$name), function(i) {
      if (!is.null(wikipedia_links[i])) {
        sprintf('<a href="%s" target="_blank">%s</a>', wikipedia_links[i], data$people$name[i])
      } else {
        data$people$name[i]
      }
    }),
    stringsAsFactors = FALSE
  )
  
  output$iss <- renderDataTable(hyperlinked_data, options = list(pageLength = 5), escape = FALSE)
  
  # Render ISS live location on a leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = now$iss_position$longitude, lat = now$iss_position$latitude, zoom = 8) %>%
      addMarkers(
        lng = now$iss_position$longitude,
        lat = now$iss_position$latitude,
        popup = sprintf("Latitude: %s<br>Longitude: %s", now$iss_position$latitude, now$iss_position$longitude)
      )
  })
}

shinyApp(ui = ui, server = server)
