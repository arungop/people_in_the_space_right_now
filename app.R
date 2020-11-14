library(rjson)
library(httr)
library(jsonlite)


# People in space right now
astro = GET("http://api.open-notify.org/astros.json")
data = fromJSON(rawToChar(astro$content))
# Live ISS location
live = GET("http://api.open-notify.org/iss-now.json")
now = fromJSON(rawToChar(live$content))

server <- function(input, output) {
    output$iss <- renderDataTable(data$people)
    # Generate a summary of the dataset ----
    output$res <- renderPrint(now$iss_position)

}


ui <- basicPage(
    titlePanel("People in space right now"),
    dataTableOutput('iss'),
    # Main panel for displaying outputs ----
    mainPanel(

        # Output: Header + summary of distribution ----
        h4("Live position of INTERNATIONAL SPACE STATION"),
        verbatimTextOutput("res"))
)

shinyApp(ui = ui, server = server)
