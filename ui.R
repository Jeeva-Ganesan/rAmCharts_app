shinyUI(dashboardPage(
  dashboardHeader(title = "rAmCharts editor"),
  dashboardSidebar(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    sidebarMenu(id = "sidebar",
      menuItem("Column/Bar", tabName = "column", icon = icon("bar-chart")),
      tags$li(conditionalPanel(condition = "input.sidebar == 'column'",
                               wellPanel(radioButtons("col_type", "Choose type", 
                                            choices = c("Column" = 1, "Clustered" = 2, "Stacked" = 3, 
                                                        "100% Stacked" = 4, "Floating column" = 5))))),
      menuItem("Line", tabName = "line", icon = icon("line-chart")),
      tags$li(conditionalPanel(condition = "input.sidebar == 'line'",
                               wellPanel(radioButtons("line_type", "Choose type", 
                                            choices = c("Line" = 1, "Smoothed line" = 2, "Step line" = 3, 
                                                        "Line & Points" = 4))))),
      menuItem("Area", tabName = "area", icon = icon("area-chart")),
      tags$li(conditionalPanel(condition = "input.sidebar == 'area'",
                               wellPanel(radioButtons("area_type", "Choose type", 
                                            choices = c("Area" = 1, "Stacked area" = 2, "100% stacked area" = 3))))),
      menuItem("Pie&Donut", tabName = "piedon", icon = icon("pie-chart")),
      tags$li(conditionalPanel(condition = "input.sidebar == 'piedon'",
                               wellPanel(radioButtons("piedon_type", "Choose type", 
                                            choices = c("Pie" = 1, "Donut" = 2))))),
      menuItem("Scatter/Bubble", tabName = "scatter", icon = icon("braille")),
      tags$li(conditionalPanel(condition = "input.sidebar == 'scatter'",
                               wellPanel(radioButtons("scat_type", "Choose type", 
                                            choices = c("Markers" = 1, "Lines" = 2, "Markers&line" = 3, 
                                                        "Trend lines" = 4))))),
      menuItem("Other types", tabName = "others", icon = icon("tachometer")),
      tags$li(conditionalPanel(condition = "input.sidebar == 'others'",
                               wellPanel(radioButtons("other_type", "Choose type", 
                                            choices = c("Gauge" = 1, "Funnel" = 2, "Pyramid" = 3, 
                                                        "Candlestick" = 4, "OHLC" = 5, "Radar" = 6,
                                                        "Polar" = 7)))))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      column(3,
             box(
               title = "Options", width = 12, height = "600px", solidHeader = TRUE, collapsible = FALSE,
               div(style = 'overflow-y: scroll; height: 540px;', fileInput("input_data", "Load data"),
                 uiOutput("uiOptions")))),
      column(9, 
             tabBox(id = "tabset", title = "Result", width = 12, side = "right", selected = "Chart",
                    height = "600px",
                    # solidHeader = TRUE, collapsible = FALSE,
                    tabPanel("Code", 
                             # includeMarkdown("test.md")
                           htmlOutput("code_chart")),
                    tabPanel("Data", 
                             dataTableOutput("table_chart")),
                    tabPanel("Chart",
                             amChartsOutput("amchart"))))
    )
  )
)
)
