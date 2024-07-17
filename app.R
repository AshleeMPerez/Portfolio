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
            "Please explore the navigation bar for different coding projects I've collaborated on. I have an educational background in Public Health, Biostatistics, Data Analytics, and Veterinary medicine. I've worked with veterinary clinics, the Florida Department of Health, and the Crop Science department at Bayer Global to collect, manipulate, and visualize data to extract significant findings or actionable insights."
          ), 
          br(), 
          p(
            "My strengths are high-level, interactive visualizations, whether that would be for KPI tracking, Health Data Analysis, or simple personal tracking. I enjoy employing coding in my day to day life, whether that is as simple as fitness tracking or as complex as a surprise scenario generator for a dog sport competition. Please ask me anything and I'd be happy to help."
          ),
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
            "Biostatistics I, Biostatistics II, SAS Computing Health Science, Epidemiology I, Longitudinal Health Data Analysis, Applied Statistical Methods for Discrete Data",
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
            h4(strong("Pre-Veterinary Curriculum")),
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
            "Florida International University",
            h4(strong("One Health, Global Communicable Diseases: An African Exposure")),
            strong("Description:"),
            "A hands-on field course to learn of the history, geography, culture, flora and fauna of South Africa. The One Health perspective provided additional perspective of the modern political, social, and institutional systems which impact regional and national health disparities.",
            br(),
          ),
          p(
            style = "text-align: right;",
            "Aug 2014 - Dec 2019"
          ),
          hr(),
          p(
            style = "padding:0px",
            h4(strong("Practiced Languages and Tools:")),
            "R/RStudio/RShiny (Preferred), SAS/SPSS, STATA, SQL",
            br(),
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
            "Bayer Global",
            h4(strong("Process Insights & Analytics Intern")),
            strong("Description:"),
            "Design novel product solutions for to measure genome sequencing workflows and deployed interactive KPI dashboard through RShiny. Collaborate with cross functional teams to integrate metrics of interest to optimize workflows.",
            br(),
          ),
          p(
            style = "text-align: right;",
            "May 2024 - Present"
          ),
          p(
            style = "padding:0px",
            "Florida Department of Health",
            h4(strong("Applied Epidemiology Intern")),
            strong("Description:"),
            "Cleaned, manipulated, and managed large epidemiological data sets using statistical tools (e.g., R, SAS/SPSS, Excel) and created interactive visualizations to communicate public health trends and findings. Independently developed and applied statistical models to interpret health data and support decision-making processes for resource allocation. Hosted presentations on Fatal and Non-fatal Drownings, Risk factors, and Prevention strategies through a community outreach program - Foster Grandparents of Miami. Developed and provided detailed presentation on statistical findings on poison exposure within Miami-Dade County, as well as actionable insights for public health initiatives.",
            br(),
          ),
          p(
            style = "text-align: right;",
            "Jan 2024 - May 2024"
          ),
          p(
            style = "padding:0px",
            "Southeast Veterinary Neurology",
            h4(strong("Patient Care Coordinator")),
            strong("Description:"),
            "Independently initiated and currently leading the development of a comprehensive data collection and analysis software to measure and optimize team performance metrics. Employ data visualization tools like Excel and R to present call metric trends and patterns, aiding in data interpretation. Optimize data collection processes, resulting in improved efficiency and a more streamlined approach to gathering essential call metric information.",
            br(),
          ),
          p(
            style = "text-align: right;",
            "Apr 2023 - Jan 2024"
          ),
          p(
            style = "padding:0px",
            "Doral Centre Animal Hospital",
            h4(strong("Client Service Representative")),
            strong("Description:"),
            "Working alongside healthcare professionals to provide meaningful updates to clients regarding patient care, diagnostic reports, and coordinating continued care. Aide in the betterment of management and workflow processes. Streamlining hospitalization protocol through increased employee and client feedback.  Managed master calendar and scheduled appointments for five providers based on optimal patient loads and clinician availability.",
            br(),
          ),
          p(
            style = "text-align: right;",
            "Nov 2022 - Apr 2023"
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
          width = 4,
          "The following visualizations were produced using publically available data through",
          tags$a(href = "https://www.gapminder.org/", "Gapminder."),
          "An independent Swedish foundation, which aims to make reliable data readily available to support data-driven interventions.",
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
          title = "Gapminder Table", 
          solidHeader = TRUE,
          status = "primary",
          width = 8,
          height = "500px",
          DTOutput("gapminderTable", height = "400px")
        )
      ),
      fluidRow(
        box(
          title = "Gapminder plot", 
          solidHeader = TRUE,
          status = "primary",
          width = 12,
          height = "800px",
          plotlyOutput("gapminderPlot", height = "700px")
        )
      )
    ),
    tabItem(
      tabName = "pandemicproj",
      fluidRow(
        box(
          title = "About the Data",
          solidHeader = TRUE,
          status = "primary",
          width = 12,
          collapsible = TRUE,
          # h4(strong("Greetings! My name is Ashlee.")),
          p(
            "The following visualizations and data tables are aggregated from approximately 386 COVID-19 Community Profile Reports from 2020 to 2022. More information about this data can be read",
            tags$a(href = "https://healthdata.gov/Health/COVID-19-Community-Profile-Report/gqxm-d9w9", "here."),
            "Wrangling steps are provided through my Github repository, which can be accessed",
            tags$a(href = "https://github.com/AshleeMPerez/COVIDProject", "here."),
            "Below the plot and data table, you may read an excerpt of the interpretations written explaining trends seen below."
          ),
        )
      ),
      fluidRow(
      tabBox(
        title = "Hospital Census across four Florida Counties",
        id = "tabset",
        width = 8,
        height = "900px",
        tabPanel(
          "All Counties",
          plotlyOutput("allCounties_plot", height = "800px")
        ),
        tabPanel(
          "Miami-Dade",
          plotlyOutput("miamidade_plot", height = "800px")
        ),
        tabPanel(
          "Broward",
          plotlyOutput("broward_plot", height = "800px")
        ),
        tabPanel(
          "Palm Beach",
          plotlyOutput("palmbeach_plot", height = "800px")
        ),
        tabPanel(
          "Duval",
          plotlyOutput("duval_plot", height = "800px")
        )
      ), 
        box(
          title = "Hospital Census table",
          solidHeader = TRUE,
          status = "primary",
          width = 4,
          height = "900px",
          collapsible = TRUE,
          DTOutput("pandemicTable", height = "700px")
        )
      ),
      fluidRow(
        box(
          title = "Interpretations",
          solidHeader = TRUE,
          status = "primary",
          width = 12,
          collapsible = TRUE,
          "The COVID-19 pandemic was an unprecedented challenge to global healthcare systems, emergency response infrastructure, and preparedness efforts. This underscored the critical importance of robust public health agendas in addressing emerging infectious diseases and safeguarding population health. The pandemic revealed vulnerabilities within healthcare systems, highlighted disparities in access to healthcare, and emphasized the need for proactive measures to mitigate the spread of infectious diseases and protect public health. During the pandemic, the Data Strategy and Execution Workgroup was included among a specialized COVID-19 response team to generate Community Profile Reports (CPR) to aggregate nation-wide data on COVID-19 mortality, transmission, and hospitalizations across varied geographical regions. From 386 Community Profile reports, information from Miami-Dade County, Broward County, Palm Beach County, and Duval County were extracted to analyze: 1) Daily Proportion of Positive COVID-19 tests, 2) Daily Census of Hospital Patients for any cause, 3) Daily Count of COVID-19 Hospital Admissions. RT-PCR COVID-19 Postivity rates are missing 4 observations from August 24, 2021, and 72 observations from ‘Daily Hospital Census’ and ‘Daily COVID-19 Hospital Admissions’ due to delays in reporting throughout the month of December 2020 and subsequent decreases in reporting in July 2022. A finalized data set is available to further analysis within this document. The primary objective of this analysis was to examine inflections on trends, discernible points of strain or relief on Hospital Patient Census for any cause, which in turn may identify periods of heightened demand, periods of relative stability or decline, or provide perspective on the dynamic environment of the COVID-19 pandemic.",
          br(),
          br(),
          "Many points of inflection in hospital patient census for any cause may be attributed to surges in COVID-19 spread. These surges lead to increased hospitalizations of individuals with severe COVID-19 symptoms, placing significant strain on healthcare systems and resulting in fluctuations. During periods of high transmission rates and surges in cases, hospitals experience a surge in admissions of COVID-19 patients requiring intensive care, mechanical ventilation, or other critical medical interventions. These influxes of COVID-19 patients can quickly overwhelm healthcare facilities, leading to capacity constraints, shortages of medical resources, and challenges in providing timely and adequate care to all patients. As discussed, drastic drops in reporting hospitals across Florida present a complex and multifaceted phenomenon influenced by changes in CCN status, regulatory changes, socioeconomic conditions, and political tensions. While closures or mergers of hospitals may account for some of the observed decreases, understanding nuanced rationale behind these changes is challenging amidst a backdrop of systemic strain, regulatory uncertainty and political conflict. The implications of these trends for public health are profound. Decreases in reporting hospitals may hinder the availability and accuracy of healthcare data, impacting the ability to monitor health outcomes, assess healthcare utilization, and inform policy decisions. This could compromise the effectiveness of public health interventions, resource allocation efforts, and pandemic response strategies. Additionally, disparities in healthcare access and service availability may exacerbate existing health inequities, disproportionately affecting vulnerable populations and under-served communities. Addressing these challenges requires a coordinated and multidisciplinary approach involving collaboration between healthcare providers, policymakers, public health agencies, and community stakeholders. Efforts to strengthen healthcare infrastructure, improve data collection and reporting mechanisms, and enhance transparency and accountability in healthcare delivery are essential. Moreover, initiatives to address underlying socioeconomic determinants of health, promote health equity, and foster resilience within the healthcare system are critical for building a more robust and responsive public health infrastructure to future disease outbreaks."
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
  
  
  output$gapminderTable <- renderDataTable(
    
    if (input$choice == "line"){
      gapminder_lineChart()
    }
    else {
      gapminder_dateRange()
    },
    
    rownames = FALSE, 
    options = list(scrollX = TRUE, scrollY = 300)
  )
  
  
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
  
  output$pandemicTable <- renderDT(
    pandemicProject_df,
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


