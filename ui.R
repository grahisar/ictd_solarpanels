## ui.R ##
library(shinydashboard) #For the dashboard
library(magrittr) #To add pipe operator
library(leaflet) #To add a map

choices <- c("Select All", "Northern", "Southern", "Eastern", "Western")

ui <- dashboardPage(
  
  dashboardHeader(title = "Solar Maps"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Methodology", tabName = "methodology", icon = icon("th"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(".leaflet-container { background: #FFFDFA; }"))
    ),
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              h2("Data Visualization"),
              p("Disclaimer: The borders used in the map are from global sources, and do not reflect the creators' views on the borders of India."),
              fluidRow(
                column(9, leafletOutput("mymap", width = 1200, height = 1000)),
              column(3, box(width = "1000px",
                "Inputs for Allocation",
                #Solar panels
                numericInput("sp", "No of Solar Panels:", value = 0, min = 0),
                #equitable/efficiently
                selectInput("eqefchoice","Distribution",c("Equitable","Efficient")),
                #select regions
                selectInput("myselect", "Choose regions", choices = choices, multiple = TRUE, selected ="Select all"),
                #Action button
                actionButton("do", "Generate")
                )
              )
            )
              
      ),
      
      # Second tab content
      tabItem(tabName = "methodology",
              h2("Methodology"), 
              tags$h3("How do we decide what an efficient allocation of solar panels would look like?"),
              tags$h4("The aim of this project is to abstract away from a variety of concerns, and focus solely on the ability of solar energy to act as good substitute for coal-powered electricity. This is important because, in India, over the last decade, solar energy has primarily replaced installed hydro capacity. Our dependence on coal has grown over the years, and the installed capacity of thermal plants is set to rise, even as in the rest of the world, 90% of new energy installed in 2020 was green energy."),
              tags$h4("The right place to install a solar panel is the one that meets the following criteria:", tags$br(), tags$br(),
                "A) There should be some demand for solar energy. As it is tough to transport solar energy across place, we ideally require the demand for produced solar energy to be locally met.", tags$br(),
      "B) The solar generation profile - dependent on local seasons, position of sun, and demand profile - should be able to substitute for thermal generation profile. In other words, the correlation between thermal energy generation and potential solar energy generation should be high and positive.", tags$br(),
    "C) The costs of running the plant - operational and ancillary costs - should be high so as to substitute for the least efficient thermal power plants.", tags$br(),
  "Keeping these in mind, we compute the Marginal Benefit (MB) from the solar panel as the sum of avoided emissions, operating costs and ancillary costs.", tags$br(), tags$br(),
  "We then allay this Marginal Benefit onto an array of 1 degree by 1 degree cells spread across India. We install the first solar panel at the site with the highest Marginal Benefit. We then update the location's MB by accounting for displacement of electricity between coal and solar. The next batch of panels is allotted to the location with the new highest MB. This process keeps repeating till all solar panels have been allocated.")
  
      )
    )
  )
)
