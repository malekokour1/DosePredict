
library(shiny)
library(shinythemes)
library(plotly)
library(shinydashboard)


header <- dashboardHeader(
  title = "DosePredict 1.0"

  
  
)

#-------------------------------------------------------------------------------------------------------#
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Input Tab", tabName = "input", icon = icon("keyboard")),
    h6("Hint: Hit RUN after entering input values"),
    actionButton("go", "RUN",icon = icon("play"),style='margin:auto'),
    menuItem("Output Tab", tabName = "output", icon = icon("sign-out-alt")),
    menuItem("Analysis Comments", tabName = "comments", icon = icon("comment-dots")),
    menuItem("Download Datasets", tabName = "downloads", icon = icon("table")),
    h6("Hint: Generate report after finishing analysis"),
    downloadButton("report", "Generate report",class = "butt",style='margin:auto'),
    tags$head(tags$style(".butt{color: black !important;}")) #  font color
  )
  
)
#-------------------------------------------------------------------------------------------------------#
body <- dashboardBody(
  
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
           fluidRow(
               
              column(10,h1("DosePredict 1.0", align = "center")),
              column(10,h4("A One Stop Shop for PK-Based Dose Predictions", align = "center")),
              column(10,h4("Malek Okour, BDS, PhD", align = "center")),
              column(10, h4( actionButton(inputId = "email1", 
                                          icon = icon("envelope", lib = "font-awesome"), 
                                          a("Contact Admin", 
                                            href="mailto:malek.f.okour@gmail.com")),align = "right"))
              
           ),
              column(10, h4(tags$a(href='http://DosePredict.com',
                     tags$img(src='logo2.png'),width = "100px", height = "100px"),align = "center")),
              column(10, h4(tags$a(href='https://github.com/malekokour1/DosePredict/blob/master/README.md',
                                tags$img(src='userguide.jpg'),width = "100px", height = "100px"),align = "center")),
              column(10, h4(tags$a(href='https://github.com/malekokour1/DosePredict/blob/master/README.md',
                                tags$img(src='userguide.png'),width = "100px", height = "100px"),align = "center"))
           
            #column(10, h4(tags$img(src='userguide.jpg'),width = "100px", height = "100px",align = "center"))
              
              
    ),
    
    # Second tab content
    tabItem(tabName = "input",
            h2("Enter Input Values"),br(),
            fluidRow(
            box( title = "Project Options:", status = "primary", solidHeader = TRUE,width = 2,
                 collapsible = TRUE,
            textInput("Author", "Author Name:", "Malek"),
            textInput("Compound", "Compound ID:", "55544432")),
            
            box( title ="Display Options:",status = "primary", solidHeader = TRUE,width = 2,
                 collapsible = TRUE,
               
                checkboxInput("logPlot","Log Scale",value=T),
                checkboxInput("PredInterval","90% PI",value=T),
                checkboxInput("IndLines","Individual Subject Lines",value=T),
                checkboxInput("LimitsLines","Safety & Efficacy Lines",value=T),
                numericInput("Xaxis", "Tick on X axis every:", 6, min = 1, max = 10000,step = 1)
                        
            ),
            
                     
            #checkboxInput("logConc","Concentrations in Log Scale",value=T),
            #checkboxInput("IndLines","Show Individual Lines",value=T),
            #checkboxInput("PredInterval","Show 90% Prediction Intervals",value=T),
            #checkboxInput("LimitsLines","Show Safety and Efficacy Limits",value=T),
            
            box( title = "Simulation Options:", status = "primary", solidHeader = TRUE,width = 2,
                 collapsible = TRUE,
                 numericInput("IDmax", "# of Subjects:", 4, min = 1, max = 10000,step = 1),
                 numericInput("SimTime", "Length of Sim:", 72, min = 1, max = 10000,step = 1)
            ),
            
            box( title = "Dosing Input:", status = "primary", solidHeader = TRUE,width = 3,
                 collapsible = TRUE,
                 textInput('DOSE', 'Enter a Dose Vector (Comma Delimited):', "10,50"),
                 numericInput("II", "II:", 0, min = 0, max = 500,step = 1),
                 numericInput("ADDL", "ADDL:", 0, min = 0, max = 1000,step = 1),
                 h6("Hint: In the current software version,
                   \n - Only regular dosing is possible. 
                   \n - Only PK-based analysis can be performed.")
            )),
    
            fluidRow(
            box( title = "PK Options:", status = "primary", solidHeader = TRUE,width = 4,
                 collapsible = TRUE,
                 selectInput("PKmodel",
                             "Select the PK Model:",
                             choices = list("Oral 1 Compartment PK" = 1,
                                            "Oral 2 Compartment PK" = 2,
                                            "Oral 3 Compartment PK" = 3,
                                            "IV 1 Compartment PK" = 4,
                                            "IV 2 Compartment PK" = 5,
                                            "IV 3 Compartment PK" = 6),
                             selected = 1),
                 h5("PK Parameters:"),   
                 # oral PK 1-compartment
                 conditionalPanel(
                   condition = "input.PKmodel == 1",
                   splitLayout(
                     numericInput("CL1", "CL:", 5, min = 0.000001, max = 10000,step = .01),
                     numericInput("VC1", "V:", 50, min = 0.0000001, max = 20,step = .01),
                     numericInput("KA1", "KA:", 0.5, min = 0, max = 20,step = .01)
                   ),
                   splitLayout(
                     numericInput("FBIO1", "F (%):", 75, min = 0, max = 100,step = .01),
                     numericInput("SCALE1", "Scaling :", 1, min = 1, max = 10000000,step = 1),
                     numericInput("BP1", "BP Ratio (fraction):", 0.7, min = 0.001, max = 20,step = .01)
                   ),
                   
                   h5("Between Subject Variability:"),
                   splitLayout(
                     numericInput("ECL1", "CL BSV (%) :", 20, min = 0, max = 200,step = 1),
                     numericInput("EVC1", "VC BSV (%):", 20, min = 0, max = 200,step = 1),  
                     numericInput("EKA1", "KA BSV (%):", 30, min = 0, max = 200,step = 1),  
                     numericInput("EBV1", "F BSV (%):", 20, min = 0, max = 200,step = 1)
                   )
                   
                 ),
                 
                 # oral PK 2- compartment
                 conditionalPanel(
                   condition = "input.PKmodel == 2",
                   splitLayout(
                     numericInput("CL2", "CL:", 10, min = 0.000001, max = 10000,step = .01),
                     numericInput("VC2", "V:", 2, min = 0.0000001, max = 20,step = .01),
                     numericInput("KA2", "KA:", 0.5, min = 0, max = 20,step = .01)
                   ),
                   splitLayout(
                     numericInput("FBIO2", "F (%):", 75, min = 0, max = 100,step = .01),
                     numericInput("SCALE2", "Scaling :", 1, min = 1, max = 10000000,step = 1),
                     numericInput("BP2", "BP Ratio (fraction):", 0.7, min = 0.001, max = 20,step = .01)
                   ),
                   splitLayout(
                     numericInput("Q12", "Q1:", 1.5, min = 0.000001, max = 10000,step = .01),
                     numericInput("V22", "V2:", 120, min = 0.0000001, max = 10000,step = .01)
                   ),
                   
                   h5("Between Subject Variability:"),
                   splitLayout(
                     numericInput("ECL2", "CL BSV (%) :", 20, min = 0, max = 200,step = 1),
                     numericInput("EVC2", "VC BSV (%):", 20, min = 0, max = 200,step = 1),  
                     numericInput("EKA2", "KA BSV (%):", 30, min = 0, max = 200,step = 1),  
                     numericInput("EBV2", "F BSV (%):", 20, min = 0, max = 200,step = 1)
                   )
                   
                 ),
                 
                 
                 #  oral PK 3-compartment
                 conditionalPanel(
                   condition = "input.PKmodel == 3",
                   splitLayout(
                     numericInput("CL3", "CL:", 20, min = 0.000001, max = 10000,step = .01),
                     numericInput("VC3", "V:", 7, min = 0.0000001, max = 20,step = .01),
                     numericInput("KA3", "KA:", 0.5, min = 0, max = 20,step = .01)
                   ),
                   splitLayout(
                     numericInput("FBIO3", "F (%):", 65, min = 0, max = 100,step = .01),
                     numericInput("SCALE3", "Scaling :", 1, min = 1, max = 10000000,step = 1),
                     numericInput("BP3", "BP Ratio (fraction):", 0.6, min = 0.001, max = 20,step = .01)
                   ),
                   splitLayout(
                     numericInput("Q13", "Q1:", 4, min = 0.000001, max = 10000,step = .01),
                     numericInput("V23", "V2:", 85, min = 0.0000001, max = 10000,step = .01),
                     numericInput("Q23", "Q2:", 2, min = 0.000001, max = 10000,step = .01),
                     numericInput("V33", "V3:", 14, min = 0.0000001, max = 10000,step = .01)
                   ),
                   
                   h5("Between Subject Variability:"),
                   splitLayout(
                     numericInput("ECL3", "CL BSV (%) :", 20, min = 0, max = 200,step = 1),
                     numericInput("EVC3", "VC BSV (%):", 20, min = 0, max = 200,step = 1),  
                     numericInput("EKA3", "KA BSV (%):", 30, min = 0, max = 200,step = 1),  
                     numericInput("EBV3", "F BSV (%):", 20, min = 0, max = 200,step = 1)
                   )
                   
                 ), 
                 #IV PK 1- compartment
                 conditionalPanel(
                   condition = "input.PKmodel == 4",
                   splitLayout(
                     numericInput("CL4", "CL:", 8, min = 0.000001, max = 10000,step = .01),
                     numericInput("VC4", "V:", 70, min = 0.0000001, max = 20,step = .01)#,
                     #numericInput("KA1", "KA:", 0.15, min = 0, max = 20,step = .01)
                   ),
                   splitLayout(
                     #numericInput("FBIO1", "F (%):", 85, min = 0, max = 100,step = .01),
                     numericInput("SCALE4", "Scaling :", 1, min = 1, max = 10000000,step = 1),
                     numericInput("BP4", "BP Ratio (fraction):", 1, min = 0.001, max = 20,step = .01)
                   ),
                   
                   h5("Between Subject Variability:"),
                   splitLayout(
                     numericInput("ECL4", "CL BSV (%) :", 20, min = 0, max = 200,step = 1),
                     numericInput("EVC4", "VC BSV (%):", 20, min = 0, max = 200,step = 1)#,  
                     #numericInput("EKA1", "KA BSV (%):", 0, min = 0, max = 200,step = 1),  
                     #numericInput("EBV1", "F BSV (%):", 20, min = 0, max = 200,step = 1)
                   )
                   
                 ),
                 
                 # IV PK 2- compartment
                 conditionalPanel(
                   condition = "input.PKmodel == 5",
                   splitLayout(
                     numericInput("CL5", "CL:", 15, min = 0.000001, max = 10000,step = .01),
                     numericInput("VC5", "V:", 3.2, min = 0.0000001, max = 20,step = .01)#,
                     #numericInput("KA2", "KA:", 0.5, min = 0, max = 20,step = .01)
                   ),
                   splitLayout(
                     #numericInput("FBIO2", "F (%):", 85, min = 0, max = 100,step = .01),
                     numericInput("SCALE5", "Scaling :", 1, min = 1, max = 10000000,step = 1),
                     numericInput("BP5", "BP Ratio (fraction):", 1, min = 0.001, max = 20,step = .01)
                   ),
                   splitLayout(
                     numericInput("Q15", "Q1:", 3.1, min = 0.000001, max = 10000,step = .01),
                     numericInput("V25", "V2:", 43.8, min = 0.0000001, max = 10000,step = .01)
                   ),
                   
                   h5("Between Subject Variability:"),
                   splitLayout(
                     numericInput("ECL5", "CL BSV (%) :", 20, min = 0, max = 200,step = 1),
                     numericInput("EVC5", "VC BSV (%):", 20, min = 0, max = 200,step = 1)#,  
                     # numericInput("EKA2", "KA BSV (%):", 0, min = 0, max = 200,step = 1)#,  
                     #numericInput("EBV2", "F BSV (%):", 20, min = 0, max = 200,step = 1)
                   )
                   
                 ),
                 
                 #  IV PK 3-compartment
                 conditionalPanel(
                   condition = "input.PKmodel == 6",
                   splitLayout(
                     numericInput("CL6", "CL:", 13, min = 0.000001, max = 10000,step = .01),
                     numericInput("VC6", "V:", 8.2, min = 0.0000001, max = 20,step = .01)#,
                     #numericInput("KA3", "KA:", 0.5, min = 0, max = 20,step = .01)
                   ),
                   splitLayout(
                     #numericInput("FBIO3", "F (%):", 85, min = 0, max = 100,step = .01),
                     numericInput("SCALE6", "Scaling :", 1, min = 1, max = 10000000,step = 1),
                     numericInput("BP6", "BP Ratio (fraction):", 1, min = 0.001, max = 20,step = .01)
                   ),
                   splitLayout(
                     numericInput("Q16", "Q1:", 5, min = 0.000001, max = 10000,step = .01),
                     numericInput("V26", "V2:", 77.4, min = 0.0000001, max = 10000,step = .01),
                     numericInput("Q26", "Q2:", 2.7, min = 0.000001, max = 10000,step = .01),
                     numericInput("V36", "V3:", 7.9, min = 0.0000001, max = 10000,step = .01)
                   ),
                   
                   h5("Between Subject Variability:"),
                   splitLayout(
                     numericInput("ECL6", "CL BSV (%) :", 20, min = 0, max = 200,step = 1),
                     numericInput("EVC6", "VC BSV (%):", 30, min = 0, max = 200,step = 1)#,  
                     #numericInput("EKA3", "KA BSV (%):", 0, min = 0, max = 200,step = 1),  
                     #numericInput("EBV3", "F BSV (%):", 20, min = 0, max = 200,step = 1)
                   )
                   
                 )),
                 
            box( title = "Safety Data:", status = "primary", solidHeader = TRUE,width = 2,
                 collapsible = TRUE,
                 numericInput("AUCinfSaf", "AUC:", 52, min = 0.0001, max = 100,step = .001),
                 numericInput("cmaxSafinput", "Cmax:", 0.5, min = 0.0001, max = 250,step = .001)
            ),
                 
            box( title = "Efficacy Data:", status = "primary", solidHeader = TRUE,width = 2,
                 collapsible = TRUE,
                 numericInput("effConcLim", "Efficacy Conc Limit :", 0.06, min = 0.0001, max = 100,step = .001),
                 numericInput("effTimeLim", "Efficacy Time Limit :", 24, min = 0, max = 5000,step = 1)
            ),     
                 
            box( title = "NCA Calculation Time-Limits:", status = "primary", solidHeader = TRUE,width = 2,
                 collapsible = TRUE, 
            
                 numericInput("NCAfrom", "Time to start the NCA calc:", 0, min = 0, max = 1000000000,step = .001),
                 numericInput("NCAto", "Time to finish the NCA calc:", 24, min = 0.0001, max = 100,step = .001)
            )),
            
            
           h4("Helpful Hint: refresh browser to reset values!"),
           
           fluidRow(
             box(width = "100%",
           column(10,h1("DosePredict 1.0", align = "center")),
           column(10,h4("A One Stop Shop for PK-Based Dose Predictions", align = "center")),
           column(10,h4("Malek Okour, BDS, PhD", align = "center"))
             )),
           column(10, h4(tags$a(href='http://DosePredict.com',
                                tags$img(src='logo2.png'),width = "100px", height = "100px"),align = "center"))
            
    ),
    
    
    
    # Third tab content
    tabItem(tabName = "output",
            fluidRow(
              h2("Toggle Between Tabs for Output"),br(),  
              tabBox(
                id = "tabset1",width = "100%",
                
                    tabPanel("PK Plot",
                         tags$style(type='text/css', 
                                    ".nav-tabs {font-size: 20px} "),
                         plotOutput("PLOTConc2", height = 800),
                         plotlyOutput("trendPlot", width = "100%", height = "800px")
                    ),
                tabPanel("NCA Histogram ",
                         tags$style(type='text/css', 
                                    ".nav-tabs {font-size: 20px} "),
                         plotOutput("plotallNCA", height = 800)),
                tabPanel("NCA Table",
                         tags$style(type='text/css', 
                                    ".nav-tabs {font-size: 20px} "),
                         tableOutput("tb")),
                tabPanel("Safety Results",
                         tags$style(type='text/css', 
                                    ".nav-tabs {font-size: 20px} "),
                         plotOutput("plotSAFNCA", height = 800),
                         tags$hr(style="border-color: blue;"),
                         tableOutput("SAFtb"),width = "100%"),
                tabPanel("Efficacy  Results ",
                         tags$style(type='text/css', 
                                    ".nav-tabs {font-size: 20px} "),
                         plotOutput("plotTimeAbove", height = 800),
                         tags$hr(style="border-color: blue;"),
                         plotOutput("plotConcEff", height = 800),
                         tags$hr(style="border-color: blue;"),
                         tableOutput("Efftb")),
                
              
              
              
              column(10,h1("DosePredict 1.0", align = "center")),
              column(10,h4("A One Stop Shop for PK-Based Dose Predictions", align = "center")),
              column(10,h4("Malek Okour, BDS, PhD", align = "center"))
            )),
            column(10, h4(tags$a(href='http://DosePredict.com',
                                 tags$img(src='logo2.png'),width = "100px", height = "100px"),align = "center"))
            
            
    ),
    
    # 4th tab
    tabItem(tabName = "comments",
            fluidRow(
              h2("Enter comments related to the current analysis in the box below"),br(),
              #tags$hr(style="border-color: blue;"),
              HTML('<textarea id="foo" rows="10" cols="200">  </textarea>'),
              box(width = "100%",
                column(10,h1("DosePredict 1.0", align = "center")),
                column(10,h4("A One Stop Shop for PK-Based Dose Predictions", align = "center")),
                column(10,h4("Malek Okour, BDS, PhD", align = "center"))
              )),
            column(10, h4(tags$a(href='http://DosePredict.com',
                                 tags$img(src='logo2.png'),width = "100px", height = "100px"),align = "center"))
              
            
    ),
    
    
    # 5th tab
    tabItem(tabName = "downloads",
            fluidRow(
              infoBox("Download Simulated Dataset", 
                      downloadButton('downloadDataset', 'Simulated Dataset'),
                      icon = icon("credit-card"), fill = TRUE),
              infoBox("Download NCA Dataset", 
                     
                      downloadButton('downloadNCA', 'NCA Data'),
                      
                      icon = icon("credit-card"), fill = TRUE),
              infoBox("Download Conc Efficacy Dataset", 
                     
                      downloadButton('downloadConcEfficacy', 'Conc Efficacy Data'),
                     
                      icon = icon("credit-card"), fill = TRUE),
              infoBox("Download Time Efficacy Dataset", 
                      
                      downloadButton('downloadTimeEfficacy', 'Time Efficacy Data'),
                      icon = icon("credit-card"), fill = TRUE)#,
              # box(width = "100%",
              #     column(10,h1("DosePredict 1.0", align = "center")),
              #     column(10,h4("A One Stop Shop for PK-Based Dose Predictions", align = "center")),
              #     column(10,h4("Malek Okour, BDS, PhD", align = "center"))
              # )
              ),
            
            column(10, h4(tags$a(href='http://DosePredict.com',
                                 tags$img(src='logo2.png'),width = "100px", height = "100px"),align = "center"))
    )
  
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    )
  
  
  
  
)
#-------------------------------------------------------------------------------------------------------#
dashboardPage(header, sidebar, body,skin = "red")
#-------------------------------------------------------------------------------------------------------#

