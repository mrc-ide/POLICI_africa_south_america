source("load_data.R")

shinyUI(fluidPage(
  tags$head(HTML('<link rel="icon", href="YF_map.png",
                 type="image/png" />')),
  headerPanel(list(HTML('<img src="Imperial.png"/>'),"Yellow fever Immunization coverage across Africa and South America"),
              windowTitle = "POLICI: Yellow fever immunization coverage"),
  sidebarPanel(width = 4,
               conditionalPanel(condition = "input.conditionedPanels == '1'", 
                                selectInput("country",
                                            label = "Choose a country to display",
                                            choices = country_vec,
                                            selected = "Angola", selectize = FALSE),
                                sliderInput("year1", label = "Year of interest",
                                            min = 1940, max = 2050, value = 2019, sep = "", step = 1,
                                            bsPopover("year", "Information", content = "", placement = "right", trigger = "hover", option = NA)),
                                sliderInput("age", label = "Age range of interest",
                                            min = 0, max = 100, value = c(0, 100),
                                            sep = "", step = 1),
                                actionButton("resetSelection1", label = "Reset row selection"),
                                actionButton("update_range", label = "Update age range"),
                                DT::dataTableOutput("country_df"),
                                downloadButton("country_map_download", label = "Download map"),
                                downloadButton("country_df_download", label = "Download table")),
               
               conditionalPanel(condition = "input.conditionedPanels == '2'", 
                                selectInput("country2",
                                            label = "Choose a country to display",
                                            choices = country_vec,
                                            selected = "Angola", selectize = FALSE),
                                sliderInput("year2", label = "Year of interest",
                                            min = 1940, max = 2050, value = 2019, sep = "", step = 1,
                                            bsPopover("year", "Information", content = "", placement = "right", trigger = "hover", option = NA)),
                                actionButton("resetSelection2", label = "Reset row selection"),
                                DT::dataTableOutput("country_df2")),
               
               conditionalPanel(condition = "input.conditionedPanels == '3'",
                                sliderInput("year3", label = "Year of interest", min = 1940, max = 2050, value = 2019, sep = "", step = 1,
                                            bsPopover("year", "Information", content = "", placement = "right", trigger = "hover", option = NA)),
                                sliderInput("age2", label = "Age range of interest",
                                            min = 0, max = 100, value = c(0, 100), sep = "", step = 1),
                                actionButton("update_range2", label = "Update age range"),
                                actionButton("resetSelection3", label = "Reset row selection"),
                                DT::dataTableOutput("endemic_df"),
                                downloadButton("endemic_map_download", label = "Download map"),
                                downloadButton("endemic_df_download", label = "Download table"))),
  
  br(),br(),
  
  mainPanel(width=8,
            
            tabsetPanel(id="conditionedPanels",
                        
                        tabPanel("Country maps", value=1, leafletOutput("country_map", height = 850, width = 1000)),
                         
                        tabPanel("Age distribution", value = 2, plotlyOutput("barplot", height = 400, width = 1000),
                                 
                                 bsPopover("barplot", "Information", content = paste0("This graph shows the total population of each 5 year", " age band by vaccination status.","</p><p> Choose from the table to display values."," By default the whole countries values are shown"), placement = "right", trigger = "hover", options = NULL),
                                 plotlyOutput("linegraph", height = 450, width = 800),
                                 
                                 bsPopover("linegraph", "Information", content=paste0("This graph shows the vaccination coverage in different", " provinces across ages."," Scroll to see all selected provinces","</p><p> Choose from the table to display values."," By default the country average is shown."), placement = "right", trigger = "hover", options = NULL)),
                        
                        tabPanel("Endemic zone", tabName = "endemic", value = 3, leafletOutput("endemic_map", height = 850, width = 1000),
                                 tagList(
                                   tags$head(
                                     tags$script(type = "text/javascript", src = "busy.js"))),
                                 div(class = "busy", p('Loading'), img(src = "spinner.gif"))),
                        
                        tabPanel("Methodology", value = 4, id = "conditionedPanels", strong(h2("Summary of methods")),
                                 
                                 p("Vaccination coverage was estimated using data from large-scale mass vaccinations in French West Africa during the 1940s to 1960s, outbreak response campaigns since 1970 as reported in the",
                                   a("Weekly Epidemiological Record", href=  "http://www.who.int/wer/en/", target = "_blank"), "(WER) or the", a("WHO disease outbreak news", href = "http://www.who.int/csr/don/en/", target = "_blank"),"(DON),",
                                   a("WHO-UNICEF estimates of routine infant immunization coverage", href = "http://www.who.int/immunization/monitoring_surveillance/routine/coverage/en/index4.html", target = "_blank"),
                                   "for yellow fever as part of the", a("Enhanced Programme for Immunisation", href = "http://www.wpro.who.int/immunization/en/", target = "_blank"), "(EPI), and mass vaccination campaigns in 11 West African countries under the Yellow Fever Initiative and in the Central African Republic from 2006 to 2012."),
                                 p("This data was compiled into a dataset of age-specific vaccination coverage which took into account the year and location of campaigns as well as the demographics of the targeted populations."), 
                                 p("We assumed that vaccination is 100% efficacious and confers lifelong immunity. Furthermore, we assumed no future response or preventive vaccination campaigns, but routine infant immunization coverage to continue at level of the last year provided.",
                                   "Population movements or displacements are not taken into account. In addition to these caveats, accuracy of the vaccination coverage estimates relies on completeness of the records of vaccination activities, as well as accurate estimates of demography, which is uncertain in many African countries.",
                                   "All vaccination activities are implemented at the end of each year, so that the corresponding changes in vaccination coverage will only be visible the following year. For example recent reactive vaccination campaigns conducted in Angola in 2016 are only reflected in the coverage in 2017."),
                                 br(),p("For further reading and a detailed explanation on the methodology, see", strong(a("Hamlet et al., 2018 Vaccine", href = "https://www.sciencedirect.com/science/article/pii/S0264410X19301598?via%3Dihub" ,target = "_blank"))),
                                 br(),p("Vaccination data last updated - November 2019"),
                                 
                                 p("If the application appears too zoomed in, hold down the control (command on macs) and - key")),
                        
                        tabPanel("About", value = 5, strong(h2("About")), p("POLICI stands for", strong("PO"),"pulation ", strong("L"),"evel ", strong("I"),"mmunization ", strong("C"), "overage ", strong("I"), "mperial", br(), br(), "This tool was developed in order to visualise yellow fever vaccination coverage across the yellow fever endemic zone in Africa.",
                                                                            br(), p("We thank the World Health Organization and countries for providing data on vaccination activities, in particular Sergio Yactayo and Olivier Ronveaux"),
                                                                            br(), p("For age disaggregated vaccination coverage, and further information at a higher spatial resolution (2nd administrative division) please contact Arran Hamlet")), br(), strong(h2("Contact Information")),
                                 p(a("Mr Arran Hamlet", href = "http://www.imperial.ac.uk/people/arran.hamlet14", target = "_blank"), br(), ("arran.hamlet14@imperial.ac.uk")),
                                 p(a("Dr Kevin Jean", href = "http://www.imperial.ac.uk/people/k.jean", target = "_blank"), br(), ("k.jean@imperial.ac.uk")),
                                 p(a("Dr Tini Garske", href = "http://www.imperial.ac.uk/people/t.garske", target = "_blank"), br(), ("t.garske@imperial.ac.uk")),
                                 p(strong("MRC Centre for Outbreak Analysis and Modelling", br(),"Department of Infectious Disease Epidemiology", br(), "Imperial College London")))
            )
  )
))