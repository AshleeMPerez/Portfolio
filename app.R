# Loading Packages --------------------------------------------------------
# install.packages("MetBrewer")    
# install.packages("readxl")
# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("shinyWidgets")
# install.packages("rsconnect")
# install.packages("plotly")
# install.packages("DT")
# install.packages("data.table")
# install.packages("scales")
# install.packages("tidyverse")
library(MetBrewer)
library(readxl)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(rsconnect)
library(plotly)
library(DT)
library(data.table)
library(scales)
library(tidyverse)

# Import Data -------------------------------------------------------------

gapminder_created_df <- read_csv("data/gapminder_created.csv")

pandemicProject_df <- read_csv("data/data_CDC_final.csv")

pizzaSales_df <- read_csv("data/pizzaSales.csv")

# Functions ---------------------------------------------------------------

plot_bubble <- function(data, year) {
  ggplot(data) +
    theme(
      axis.line = element_line(colour = "grey90", linewidth = 0.5), 
      panel.grid = element_line(colour = "grey90", linewidth = 0.5), 
      legend.position = "top",
      legend.direction = "horizontal",
      legend.key = element_blank(),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      legend.background = element_blank(),
      legend.box.spacing = unit(0.2, "mm"),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(size = 12),
      plot.subtitle = element_text(size = 10, margin = margin(0, 0, 0, 0)),
      plot.caption = element_text(size = 8),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.text = element_text(size = 8),
    ) + 
    aes(
      x = gdp_per_capita,
      y = life_expectancy, 
      fill = continent, 
      size = population,
      frame = year,
      ids = country
    ) +
    scale_x_log10(
      name = "GDP per Capita (log scale)",
      # set limits of axis to min and max of data
      limits = c(
        min(data$gdp_per_capita),
        max(data$gdp_per_capita)
      ),
      breaks = c((1000 * 2 ^ (-3:8)))
    ) +
    scale_y_log10(
      name = "Life Expectancy in Years (log scale)",
      # set limits of axis to min and max of data
      limits = c(20, 100),
      expand = c(0, 0),
      breaks = (10 * (2:10))
    ) +
    scale_size_continuous(
      name = "Population",
      range = c(1, 30),
      guide = "none"
    ) +
    scale_fill_manual(
      name = "Continent", 
      values = c("turquoise3", "green3", "firebrick2", "yellow3", "plum3")
    ) +
    labs(
      title = "Life Expectancy Increases with GDP per Capita",
      subtitle = "As measured on national averages reported for 2013",
      caption = "Source: Gapminder; Year: 2022"
    )  +
    geom_point(shape = 21, alpha = 0.5)
  
}


plot_line <- function(data, year) { 
  
  ggplot(data = data) +
    theme(
      axis.line = element_line(colour = "black", linewidth = 0.25),
      panel.background = element_blank(),
      panel.grid.major = element_line(colour = "lightgrey", linewidth = 0.25),
      axis.ticks = element_blank(),
      legend.key = element_blank()
    ) +
    aes(
      x = year, 
      y = gdp_per_capita, 
      color = country
    ) +
    scale_x_continuous(
      name = "Year",
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      name = "GDP per Capita",
      limits = c(
        min(data$gdp_per_capita),
        max(data$gdp_per_capita) + 15
      ),
      breaks = (1000 * 2 ^ (1:10))
    ) +
    labs(
      title = "Evolving economic environment from 1800 to Present",
      color = "Country",
    ) +
    geom_line(linewidth = 2, lineend = "round")
  
}

plot_census <- function(data, color) {
  
  ggplot(data = data) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      axis.title.x = element_blank(),
    ) +
    aes(
      x = date_file, 
      y = Beds_Occupied_Total, 
      fill = County, 
      size = Beds_Available_Total
    ) +
    labs(
      title = "Examining trends in Hospital Census Data (Any Cause)",
      x = "Date of Admission",
      y = "Number of Beds Occupied (Patients Admitted)"
    ) +
    scale_x_date(
      date_breaks = "2 month", 
      date_labels = "%b - %Y",
      expand = c(0, 5)
    ) +
    scale_y_continuous(name = NULL) +
    scale_size_continuous(
      name = "Number of Available \n Hospital Beds", 
      breaks = c(1500 * (1:5)),
      guide = "none"
    ) +
    scale_fill_manual(values = color) +
    geom_point(
      shape = 21,
      alpha = 0.5,
    )
  
}

# Global Objects ----------------------------------------------------------

filtered_gapminder <- gapminder_created_df %>% 
  filter(
    if_all(
      c(
        continent, 
        country, 
        region, 
        gdp_per_capita, 
        population, 
        life_expectancy
      ),  
      ~ !is.na(.)
    )
  ) %>% 
  select(continent, country, everything())

countries <- unique(filtered_gapminder$country)
continents <- unique(filtered_gapminder$continent)
regions <- unique(filtered_gapminder$region)

factor_cols <- c(
  "order_details_id",
  "order_id",
  "pizza_id",
  "pizza_size",
  "pizza_category",
  "pizza_ingredients"
  )

countyColor <- c("turquoise3", "green3", "salmon2", "plum3")

pizzaCategory_ls <- unique(pizzaSales_df$pizza_category)

pizzaType_ls <- unique(pizzaSales_df$pizza_name)

pizzaSize_ls <- unique(pizzaSales_df$pizza_size)

pizzaSales <- pizzaSales_df %>% 
  mutate(
    across(.cols = all_of(factor_cols), .fns = factor),
    pizza_name = factor(pizza_name, levels = pizzaType_ls, labels = pizzaType_ls),
    # order_date = as.Date(order_date, units = "%m/%d/%Y"),
    round_date = floor_date(order_date, unit = "month"),
    datetime = strptime(paste(order_date, order_time), format="%Y-%m-%d %H:%M:%S"),
    # order_time = as.POSIXct(order_time, format = "%h:%m:%s"),
    round_time = floor_date(datetime, unit = "hour")
  ) %>% 
  select(-datetime) 


# UI ----------------------------------------------------------------------


# Header ------------------------------------------------------------------

header <- dashboardHeader(title = "Hi! I'm Ashlee.", titleWidth = "200px")

# Sidebar ----------------------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(" About Me", tabName = "aboutme", icon = icon("address-card")),
    menuItem(" Gapminder Exploration", tabName = "gapminder", icon = icon("magnifying-glass-chart")),
    menuItem(" COVID Pandemic Project", tabName = "pandemicproj", icon = icon("chart-pie")),
    menuItem(" KPI Dashboard", tabName = "kpi", icon = icon("ranking-star"))
  )
)

# Body --------------------------------------------------------------------

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "aboutme",
      fluidRow(
        box(
          title = "Introduction",
          solidHeader = TRUE,
          status = "primary",
          width = 10,
          collapsible = TRUE,
          h4(strong("Greetings! My name is Ashlee.")), 
          p(
            "Please explore the navigation bar for different coding projects I've collaborated on. I have an educational background in Public Health, Biostatistics, Data Analytics, and Veterinary medicine. I've worked with veterinary clinics, the Florida Department of Health, and the Crop Science department at Bayer Global to collect, manipulate, and visualize data to extract significant findings or actionable insights.",
            "My strengths are high-level, interactive visualizations, whether that would be for KPI tracking, Health Data Analysis, or simple personal tracking. I enjoy employing coding in my day to day life, whether that is as simple as fitness tracking or as complex as a surprise scenario generator for a dog sport competition. Please ask me anything and I'd be happy to help."
          ),
          br(),
          p(
            style = "padding:0px",
            h4(strong("Practiced Languages and Tools:")),
            "R/RStudio (Preferred), RShiny, tidymodels, SAS/SPSS, STATA, SQL, Python, Tableau, AWS, BigQuery",
          )
        ),
        box(
          title = "Connect with me",
          solidHeader = TRUE,
          status = "primary",
          width = 2,
          height = 2,
          align = "center",
          collapsible = TRUE,
          imageOutput("myqrcode")
        )
      ),
      fluidRow(
        box(
          title = "Education",
          solidHeader = TRUE,
          status = "primary",
          width = 4, 
          # height = "725px",
          collapsible = TRUE,
          p(
            style = "padding:-50px",
            "Florida International University",
            h4(strong("Master's in Public Health: Biostatistics")),
            strong("Selected Coursework:"),
            "Biostatistics I/II, Topics in Data Science, R for Health Data Science, SAS Computing Health Science, Longitudinal Health Data Analysis, Survival Data Analysis, Applied Statistical Methods for Discrete Data",
            br(),
          ),
          p(
            style = "text-align: right;",
            "Aug 2022 - Jul 2024"
          ),
          hr(),
          p(
            style = "padding:0px",
            "Florida International University",
            h4(strong("Bachelors' Degree: Pre-Veterinary Curriculum")),
            strong("Selected Coursework:"),
            "Calculus I, Calculus II, Physics with Calculus, Physics II with Calculus, Organic Chemistry I & II, Microbiology, Biochemistry",
            br(),
          ),
          p(
            style = "text-align: right;",
            "Aug 2014 - Dec 2019"
          ),
          hr(),
          p(
            style = "padding:0px",
            "Florida International University: Study Abroad in South Africa",
            h4(strong("One Health, Global Communicable Diseases: An African Exposure")),
            strong("Description:"),
            "A hands-on field course to learn of the history, geography, culture, flora and fauna of South Africa. The One Health perspective provided additional perspective of the modern political, social, and institutional systems which impact regional and national health disparities.",
            br(),
          ),
          p(
            style = "text-align: right;",
            "May 2016"
          ),
          hr(),
          p(
            style = "padding:0px",
            "Florida International University Honors College",
            h4(strong("Bachelors' Degree: Business Management")),
            "Graduated with Honors distinction and Magna Cum Laude with 3.92/4.00 GPA",
            br(),
          ),
          p(
            style = "text-align: right;",
            "Aug 2012 - Dec 2015"
          ),
          
        ),
        box(
          title = "Experience",
          solidHeader = TRUE,
          status = "primary",
          width = 8,
          # height = "725px",
          collapsible = TRUE,
          p(
            style = "padding:0px",
            tags$span(
              tags$div(strong(tags$a(href = "https://www.bayer.com/en/", "Bayer Global")), style = "font-size:18px; line-height: 0.75"),
              tags$span("May 2024 - Present", style = "float:right")
            ),
            h4(strong("Process Insights & Analytics Intern")),
            # strong("Description:"),
            p(
              tags$ul(
                tags$li("Collected and manipulated multiple genome sequencing data sets (over 900,000 rows) from BigQuery schemas, utilizing R/RStudio to develop summary files of pertinent dates, KPIs, and project details."), 
                tags$li("Designed novel product solutions to measure genome sequencing workflows and deployed interactive KPI dashboards and applications through RShiny. Deployed product through AWS and Posit Connect, enhancing accessibility and data-driven metrics for multiple teams."), 
                tags$li("Collaborated with cross functional teams, including lead researchers, laboratory technicians, and data scientists, to present data pipeline, product solutions, and application/dashboard demonstrations, as well as future feature integration.")
              )
            ),
            # br(),
            hr(),
          ),
          p(
            style = "padding:0px",
            tags$span(
              tags$div(strong(tags$a(href = "https://www.floridahealth.gov/", "Florida Department of Health")), style = "font-size:18px; line-height: 0.75"),
              tags$span("Jan 2024 - May 2024", style = "float:right")
            ),
            h4(strong("Applied Epidemiology Intern")),
            # strong("Description:"),
            p(
              tags$ul(
                tags$li(strong("Cleaned, manipulated, and managed epidemiological data sets with over 150,000 rows using statistical tools (e.g., R, SAS/SPSS, Excel) and created interactive visualizations to communicate public health trends and findings.")), 
                tags$li("Independently developed and applied statistical models to interpret health data and support decision-making processes for resource allocation."), 
                tags$li("Hosted presentations on Fatal and Non-fatal Drownings, Risk factors, and Prevention strategies through a community outreach program - Foster Grandparents of Miami."),
                tags$li("Developed and provided detailed presentation on statistical findings on poison exposure within Miami-Dade County, as well as actionable insights for public health initiatives.")
              )
            ),
            hr(),
            # br(),
          ),
          p(
            style = "padding:0px",
            tags$span(
              tags$div(strong(tags$a(href = "https://sevneurology.com/", "Southeast Veterinary Neurology")), style = "font-size:18px; line-height: 0.75"),
              tags$span("Apr 2023 - Jan 2024", style = "float:right")
            ),
            h4(strong("Patient Care Coordinator")),
            # strong("Description:"),
            p(
              tags$ul(
                tags$li(strong("Independently initiated and leading the development of a comprehensive data collection and analysis software to measure and optimize team performance metrics.")),
                tags$li("Employ data visualization tools like Excel and R to present call metric trends and patterns, aiding in data interpretation."),
                tags$li("Optimize data collection processes, resulting in improved efficiency and a more streamlined approach to gathering essential call metric information.")
              )
            ),
            # br(),
            hr(),
          ),
          p(
            style = "padding:0px",
            tags$span(
              tags$div(strong(tags$a(href = "https://doralvet.com/", "Doral Centre Animal Hospital")), style = "font-size:18px; line-height: 0.75"),
              tags$span("Nov 2022 - Apr 2023", style = "float:right")
            ),
            h4(strong("Client Service Representative")),
            # strong("Description:"),
            tags$ul(
              tags$li("Working alongside healthcare professionals to provide meaningful updates to clients regarding patient care, diagnostic reports, and coordinating continued care."),
              tags$li("Aide in the betterment of management and workflow processes. Streamlining hospitalization protocol through increased employee and client feedback."),
              tags$li("Managed master calendar and scheduled appointments for five providers based on optimal patient loads and clinician availability.")
            ),
            br(),
          ),
        )
      ),
    ),
    tabItem(
      tabName = "gapminder",
      fluidRow(
        box(
          title = "Gapminder",
          solidHeader = TRUE,
          status = "primary",
          width = 3,
          p(
            "The following visualizations were produced using publicly available data through",
            tags$a(href = "https://www.gapminder.org/", "Gapminder.")
          ),
          p("The bubble chart, for which Gapminder is famously known for, illustrates the trends of average Life Expectancy as it relates to GDP per capita over time. The size of each bubble corresponds to the population size of each country and color represents the country's respective continent."),
          p("If the line chart is selected, it demonstrates the trend of GDP per capita over time for selected countries (in unique colors). Please hover or click over the graph or legend for more precise insight on values."),
          h5("Use the following filters to subset the data:"),
          selectInput(
            "choice",
            "Choose a plot object:",
            choices = c("Bubble Chart" = "bubble", "Line Chart" = "line"),
            selected = "Bubble Chart"
          ),
          conditionalPanel(
            condition = "input.choice == 'bubble'",
            sliderInput(
              "year",
              "Select year of interest:",
              min = min(filtered_gapminder$year),
              max = max(filtered_gapminder$year),
              sep = "",
              step = 5,
              value = 2013,
              animate = TRUE
            ),
            p("Click the play button for an animation of gdp per capita changes over time."),
            pickerInput(
              "continent_choice",
              "Select continent(s) of interest:",
              choices = continents,
              multiple = TRUE
            ),
          ),
          conditionalPanel(
            condition = "input.choice == 'line'",
            sliderInput(
              "yearRange",
              label = "Select date range:",
              min = 1900,
              max = year(Sys.Date()),
              sep = "",
              step = 25,
              value = c(1940, 2013),
              # animate = TRUE
            ),
            pickerInput(
              "country_choice",
              "Select country or countries of interest:",
              choices = countries,
              multiple = TRUE,
              selected = c("China", "Russia", "India", "Germany", "USA")
            ),
          ),
          hr(),
          strong("Download tables below:"),
          br(),
          downloadButton(
            outputId = "downloadcsv",
            label = "Download CSV file",
            icon = icon("download")
          ),
          br(),
          downloadButton(
            outputId = "downloadxls",
            label = "Download XLS file",
            icon = icon("download")
          )
        ),
        box(
          title = "Gapminder plot", 
          solidHeader = TRUE,
          status = "primary",
          width = 9,
          height = "800px",
          plotlyOutput("gapminderPlot", height = "700px")
        )
      ),
      fluidRow(
        box(
          title = "Gapminder Table", 
          solidHeader = TRUE,
          status = "primary",
          width = 12,
          height = "90%",
          DTOutput("gapminderTable", height = "90%")
        )
      )
    ),
    tabItem(
      tabName = "pandemicproj",
      fluidRow(
      tabBox(
        title = "Hospital Census across four Florida Counties",
        id = "tabset",
        width = 8,
        height = "800px",
        tabPanel(
          "All Counties",
          plotlyOutput("allCounties_plot", height = "750px")
        ),
        tabPanel(
          "Miami-Dade",
          plotlyOutput("miamidade_plot", height = "750px")
        ),
        tabPanel(
          "Broward",
          plotlyOutput("broward_plot", height = "750px")
        ),
        tabPanel(
          "Palm Beach",
          plotlyOutput("palmbeach_plot", height = "750px")
        ),
        tabPanel(
          "Duval",
          plotlyOutput("duval_plot", height = "750px")
        ),
        # tabPanel(
        #   "Data Table",
        #   DTOutput("pandemicTable", height = "700px")
        # )
      ), 
        box(
          title = "About the Data",
          solidHeader = TRUE,
          status = "primary",
          width = 4,
          # height = "90&",
          collapsible = TRUE,
          p(
            "The following visualizations and data table were aggregated from approximately 386 COVID-19 Community Profile Reports from 2020 to 2022. More information about this data can be read",
            tags$a(href = "https://healthdata.gov/Health/COVID-19-Community-Profile-Report/gqxm-d9w9", "here."),
            "Wrangling steps are provided through my Github repository, which can be accessed",
            tags$a(href = "https://github.com/AshleeMPerez/COVIDProject", "here."),
            "Below the plot and data table, you may read an excerpt of the interpretations written explaining trends seen below."
          ),
          hr(),
          h5(strong("Interpretations:")),
          p("As seen in the adjacent graph, trends among number of occupied hospital beds over time can be visualized for four Florida counties of varying population size. The size of each circle represents the total number of available hospital beds and serves as a visual scale factor to more appropriately compare the differing population sizes between counties. For example, Miami-Dade County has a population of approximately 2.8 million, while Duval County is approximately 55,000."),
          # br(),
          p("Most notably, around February to April 2022, drastic drops can be seen in reporting hospitals across Florida, which presents a complex and multifaceted phenomenon, which may be influenced by CCN status, regulatory changes, socioeconomic conditions, and/or political tensions. While closures or mergers of hospitals may account for some of the observed decreases, the implications of these trends for public health are profound."), 
          p("Missing data can be seen within the table below toward the minimum and maximum date ranges available due to delays in reporting practices for COVID Positvity rates, Hospital Admissions, and resource allocation as it relates to the Hospital Census.")
        )
      ),
      fluidRow(
        box(
          title = "About the Data",
          solidHeader = TRUE,
          status = "primary",
          width = 12,
          # height = "90&",
          collapsible = TRUE,
            DTOutput("pandemicTable", height = "700px")
          
        )
      )
    ),
    tabItem(
      tabName = "kpi",
      fluidRow(
        valueBoxOutput("pizzaSold"),
        valueBoxOutput("pizzaProfits"),
        valueBoxOutput("highestsell"),
      ),
      fluidRow(
        box(
          title = "Monthly Pizza Sales",
          solidHeader = TRUE,
          status = "primary",
          width = 6,
          collapsible = TRUE,
          plotOutput("pizza_plot")
        ),
        box(
          title = "Highest Selling Menu Items",
          solidHeader = TRUE,
          status = "primary",
          width = 6,
          collapsible = TRUE,
          plotOutput("highestsell_plot")
        )
      ),
      fluidRow(
        box(
          title = "Explore the Data",
          solidHeader = TRUE,
          status = "primary",
          width = 4,
          collapsible = TRUE,
          dateRangeInput(
            inputId = "dateRange",
            label = "Select date range:",
            start = min(pizzaSales$order_date),
            end = max(pizzaSales$order_date),
            min = min(pizzaSales$order_date),
            max = max(pizzaSales$order_date),
            format = "mm-dd-yyyy",
            separator = " to "
          ),
          pickerInput(
            inputId = "pizza_category_choice",
            label = "Select pizza category:",
            choices = pizzaCategory_ls,
            selected = pizzaCategory_ls,
            multiple = TRUE
          ),
          pickerInput(
            inputId = "pizza_name_choice",
            label = "Select pizza name:",
            choices = pizzaType_ls,
            selected = pizzaType_ls[1],
            multiple = TRUE
          ),
          pickerInput(
            inputId = "pizza_size_choice",
            label = "Select pizza size:",
            choices = pizzaSize_ls,
            selected = pizzaSize_ls,
            multiple = TRUE
          ),
          sliderInput(
            inputId = "cost",
            label = "Use slider to display unit price:",
            min = 9,
            max = 36,
            value = c(16, 20),
            step = 5
          ),
          hr(),
          p(
            "The data utilized within this KPI dashboard was made publicly available by", 
            tags$a(href = "https://www.mavenanalytics.io/blog/maven-pizza-challenge", "Maven Analytics.")
          )
        ),
        box(
          title = "Data Table of All Sales",
          solidHeader = TRUE,
          status = "primary",
          width = 8,
          collapsible = TRUE,
          DTOutput("pizzaDT")
        )
      )
    )
  )
)

ui <- dashboardPage(header, sidebar, body)


# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  output$myqrcode <- renderImage({ 
    filename <- normalizePath(file.path("./www", "qrcode.png"))
    
    list(
      src = filename, 
      contentType = 'image/png', 
      width = 150,
      height = 150
      )
  }, 
  deleteFile = FALSE
  )
  
  
  # Gapminder --------------------------------------------------------------------
  
  gapminder_dateRange <- reactive({
    
    
    if (isTruthy(is.null(input$continent_choice))) {
      filtered_gapminder %>%
        filter(year == input$year) 
    }
    else {
      filtered_gapminder %>%
        filter(year == input$year) %>%
        filter(continent == input$continent_choice)
    }
    
  })
  
  
  gapminder_lineChart <-  reactive({
    
    if (isTruthy(is.null(input$country_choice))) {
      
      filtered_gapminder %>% 
        filter(country %in% c("China", "Russia", "India", "Germany", "USA"))
      
    }
    else {
      
      filtered_gapminder %>% 
        filter(country == input$country_choice)
    }
  })
  
  
  gapminder_tbl <- reactive({
    
    if (input$choice == "bubble") {
      gapminder_dateRange()
    }
    else if (input$choice == "line") {
      gapminder_lineChart()
    }
  })
  
  output$gapminderPlot <- renderPlotly({
    
    year <- input$year
    
    
    if (input$choice == "bubble") {
      
      p <- plot_bubble(gapminder_dateRange(), year)
      
      ggplotly(p) %>%
        layout(legend = list(orientation = "h"))
      
    }
    
    else if (input$choice == "line") {
      
      p <- plot_line(gapminder_lineChart(), input$year)
      
      ggplotly(p)
      
    }
    
    else {
      
      p <- plot_bubble(gapminder_dateRange(), year)
      
      ggplotly(p) %>%
        layout(legend = list(orientation = "h"))    }
    
    
    
  })
  
  
  output$gapminderTable <- renderDataTable({
    
    tbl_output <- gapminder_tbl() %>%
      mutate(
        population = format(population, scientific = TRUE, digits = 3),
        gdp = format(gdp, scientific = TRUE, digits = 3),
        gdp_per_capita = round(gdp_per_capita, digits = 3)
      )
    
    datatable(
      tbl_output,
      # gapminder_tbl(),
    # if (input$choice == "line"){
    #   gapminder_lineChart()
    # }
    # else {
    #   gapminder_dateRange()
    # },
    
      rownames = FALSE, 
      options = list(
        scrollX = TRUE, 
        scrollY = 300,
        columnDefs = list(
          list(className = 'dt-center', targets = 0:9)
        )
      )
    )
  })
  
  
  output$downloadcsv <- downloadHandler(
    filename = "gapminder_dataTable.csv",
    content = function(file) {
      write_csv(gapminder_tbl(), file)
    }
  )
  
  output$downloadxls <- downloadHandler(
    filename = "gapminder_dataTable.xls",
    content = function(file) {
      write_excel_csv(gapminder_tbl(), file)
    }
  )
  
  # Pandemic Project --------------------------------------------------------
  
  output$allCounties_plot <- renderPlotly({

    p <- plot_census(pandemicProject_df, countyColor)

    ggplotly(p) %>%
      layout(legend = list(orientation = "h"))

  })
  
  output$miamidade_plot <- renderPlotly({
    
    miami_dade <- pandemicProject_df %>% 
      filter(County == "Miami-Dade County, FL")
    
    p <- plot_census(miami_dade, countyColor[3])
    
    
    ggplotly(p) %>% 
      layout(legend = list(orientation = "h"))
    
  })
  
  output$broward_plot <- renderPlotly({
    
    broward <- pandemicProject_df %>% 
      filter(County == "Broward County, FL")
    
    p <- plot_census(broward, countyColor[1])
    
    ggplotly(p) %>% 
      layout(legend = list(orientation = "h"))
    
  })
  
  output$palmbeach_plot <- renderPlotly({
    
    palmbeach <- pandemicProject_df %>% 
      filter(County == "Palm Beach County, FL")
    
    p <- plot_census(palmbeach, countyColor[4])
    
    ggplotly(p) %>% 
      layout(legend = list(orientation = "h"))
    
  })
  
  output$duval_plot <- renderPlotly({
    
    duval <- pandemicProject_df %>% 
      filter(County == "Duval County, FL")
    
    p <- plot_census(duval, countyColor[2])
    
    ggplotly(p) %>% 
      layout(legend = list(orientation = "h"))
    
  })
  
  pandemicProject_tbl <- pandemicProject_df %>% 
    mutate(
      Population = format(Population, big.mark = ","),
      PCR_Positive_Tests = format(PCR_Positive_Tests, big.mark = ","),
      PCR_Total_Tests = format(PCR_Total_Tests, big.mark = ","),
      COVID_Total_Admissions = round(COVID_Total_Admissions, digits = 3),
      Beds_Occupied_Total = round(Beds_Occupied_Total, digits = 3),
    )
  
  output$pandemicTable <- renderDT(
    
    pandemicProject_tbl,
    rownames = FALSE, 
    options = list(scrollX = TRUE, scrollY = 650)
    
  )

# KPI Dashboard -----------------------------------------------------------


# Value Boxes -------------------------------------------------------------
  
  output$pizzaSold <- renderValueBox({
    
    totalnoSales <- pizzaSales %>% 
      group_by(max(month(order_date))) %>% 
      summarise(sum = sum(quantity))
    
    valueBox(
      value = format(totalnoSales$sum, big.mark = ","),
      subtitle = paste0("pizzas sold in ", max(year(pizzaSales$order_date))),
      icon = icon("pizza-slice"),
      color = "orange",
      width = 4
    )
  })
  
  output$pizzaProfits <- renderValueBox({
    
    totalSales <- pizzaSales %>% 
      summarise(sum = sum(total_price))
    
    valueBox(
      value = paste0("$", format(totalSales$sum, big.mark = ",")),
      subtitle = paste0("in Sales for ", max(year(pizzaSales$order_date))),
      icon = icon("comments-dollar"),
      color = "green",
      width = 4
    )
  })
  
  highestselling <- pizzaSales %>% 
    group_by(pizza_name) %>% 
    summarise(sum = sum(quantity)) %>% 
    arrange(desc(sum)) %>% 
    head(n = 6)
  
  output$highestsell <- renderValueBox({
    
    valueBox(
      value = paste0(format(highestselling$sum[1], big.mark = ","), " pizzas"),
      subtitle = paste0("Highest Selling: ", highestselling$pizza_name[1]),
      icon = icon("ranking-star"),
      color = "aqua",
      width = 4
    )
    
  })
  

# Visualizations ----------------------------------------------------------

  output$pizza_plot <- renderPlot({
    
    monthly_sales <- pizzaSales %>% 
      group_by(round_date) %>% 
      summarise(totalSales = sum(quantity))
    
    p <- ggplot(data = monthly_sales) +
      theme_minimal() +
      aes(x = round_date, y = totalSales, label = totalSales) +
      scale_x_date(
        name = NULL,
        date_breaks = "1 month",
        date_labels = "%b"
      ) +
      # coord_cartesian(ylim = c(2000, max(montxhly_sales$totalSales)))
      labs(
        title = "Monthly Sales across all Menu Items for ABC Pizzeria",
        y = "Number of Pizzas Sold"
      ) +
      geom_col(fill = "orangered3") +
      geom_label(nudge_y = 150)
    
    p
    
  })
  
  output$highestsell_plot <- renderPlot({

    
    p <- ggplot(data = highestselling) +
      theme_minimal() +
      theme(legend.position = "none") +
      aes(x = reorder(pizza_name, sum), y = sum, fill = pizza_name, label = sum) +
      coord_flip() +
      scale_x_discrete(name = NULL) +
      scale_y_continuous(name = "Number of Pizzas Sold") +
      scale_fill_manual(values = met.brewer("Homer2")) +
      labs(
        title = paste0("Highest Selling Menu Items for ", max(year(pizzaSales$order_date)))
      ) +
      geom_col() +
      geom_label(fill = "white") 
    
    p
    
  })
  

# Search Criteria and Data Table ------------------------------------------

  # output_tbl <- pizzaSales %>% 
  #   select(-round_date, -round_time, -order_details_id, -pizza_id) %>% 
  #   select(
  #     order_id, 
  #     pizza_name, 
  #     pizza_size, 
  #     pizza_category, 
  #     everything()
  #   )

  sales_tbl <- reactive({
    pizzaSales_df %>%
      filter(
        order_date >= input$dateRange[1] &
          order_date < input$dateRange[2]
      ) %>%
      filter(pizza_category %in% input$pizza_category_choice) %>%
      filter(pizza_name %in% input$pizza_name_choice) %>% 
      filter(pizza_size %in% input$pizza_size_choice) %>% 
      filter(
        unit_price >= input$cost[1] &
          unit_price <= input$cost[2]
      )

  })
  
  output$pizzaDT <- renderDT({
      datatable(
        sales_tbl(),
        rownames = FALSE,
        options = list(scrollX = TRUE, scrollY = 650)
      )
  })


}

# Launch App --------------------------------------------------------------

shinyApp(ui, server)


