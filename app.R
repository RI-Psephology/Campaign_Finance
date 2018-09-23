# https://www.rand.org/content/dam/rand/pubs/research_reports/RR700/RR791/RAND_RR791.pdf

## app.R ##
# suppressWarnings(suppressMessages(library(dplyr)))
# library(tidyr)
# library(lubridate)
# library(scales)
# library(stringr)
# library(ggplot2)
# library(ggrepel)
# library(viridis)
# library(shinydashboard)
# library(shiny)
# library(shinythemes)
# library(shinyjs)
# library(DT)
# library(qgraph)

suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(tidyr)))
suppressWarnings(suppressMessages(library(lubridate)))
suppressWarnings(suppressMessages(library(scales)))
suppressWarnings(suppressMessages(library(stringr)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(ggrepel)))
suppressWarnings(suppressMessages(library(viridis)))
suppressWarnings(suppressMessages(library(shinydashboard)))
suppressWarnings(suppressMessages(library(shiny)))
suppressWarnings(suppressMessages(library(shinythemes)))
suppressWarnings(suppressMessages(library(shinyjs)))
suppressWarnings(suppressMessages(library(DT)))
suppressWarnings(suppressMessages(library(qgraph)))


# Multiplot 
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
     library(grid); plots <- c(list(...), plotlist) # Make a list from the ... arguments and plotlist
     numPlots = length(plots) # If layout is NULL, then use 'cols' to determine layout
     if (is.null(layout)) {
          layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),# Make the panel
                           ncol = cols, nrow = ceiling(numPlots/cols))# ncol: Number of columns of plots, nrow: Number of rows needed, calculated from # of cols
     }
     if (numPlots==1) {
          print(plots[[1]])
     } else {
          grid.newpage(); pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout)))) # Set up the page
          for (i in 1:numPlots) { # Make each plot, in the correct location
               matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))  # Get the i,j matrix positions of the regions that contain this subplot
               print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, layout.pos.col = matchidx$col))
          }}}

# monify 
monify <- function(x) {
     library(scales)
     x <- as.numeric(x) # Make sure class of x is numeric
     suffix <- ifelse(x >= 1000000000, "B", # Assign x to appropriate suffix group
                      ifelse(x >= 1000000, "M", ifelse (x <= 10000, "NA", "k")))
     x <- ifelse(suffix == "B", round(x / 1000000000, digits = 1), # Divide if necessary
                 ifelse(suffix == "M", round(x / 1000000, digits = 1),
                        ifelse(suffix == "k", comma(round(x / 1000, digits = 0)), comma(round(x, digits = 0)))))
     x <- ifelse(suffix == "B", paste0(x, "B"), #Add letter to the end
                 ifelse(suffix == "M", paste0(x, "M"), ifelse(suffix == "k", paste0(x, "k"), paste0(x, ""))))
     x <- sub("^", "$", x) #Add $ to the beginning
     return(x) # Return the formatted object
}


# Function to identify duplicates
dupf <- function(dat, var, ...) {
     library(dplyr); x <- dat; v <- var; grp <- list(...)
     g <- unlist(grp)
     if(length(grp) == 0) {
          v2 <- var
     } else {
          v2 <- c(v,g)
     }
     dup <<- x %>% group_by_at(v2) %>% tally() %>% ungroup() %>%
          filter(n>1) %>% 
          arrange(desc(n)) %>%
          select(-n) %>% left_join(x, by = v2)
     dupN <<- x %>% group_by_at(v2) %>% tally() %>% ungroup() %>%
          filter(n>1) %>% arrange(desc(n))
     if(nrow(dup) + nrow(dupN) == 0) {
          print("No Duplicates")
     } else {
          print(head(dupN));print(head(dup))
          return(invisible(list(dupN, dup)))
     }
}

# Function to adjust size of text size of geom_text(), geom_label()
textLabelFun <- function(dat) {
     sliderVal <- nrow(dat)
     case_when(sliderVal < 10 ~ 6,
               sliderVal >= 10 & sliderVal < 15 ~ 5,
               sliderVal >= 15 & sliderVal < 20 ~ 4.25,
               sliderVal >= 20 & sliderVal < 25 ~ 3.5,
               sliderVal >= 30 ~ 3)
}

# Function to adjust size of element_text(size)
axisLabelFun <- function(dat) {
     sliderVal <- nrow(dat)
     case_when(sliderVal < 10 ~ 17,
               sliderVal >= 10 & sliderVal < 15 ~ 16,
               sliderVal >= 15 & sliderVal < 20 ~ 15,
               sliderVal >= 20 & sliderVal < 25 ~ 12,
               sliderVal >= 25 & sliderVal < 30 ~ 10,
               sliderVal >= 30 ~ 8.5)
}

# dateAxisFun <- function(dat) {
#      numLabs <- n_distinct(dat$Month_Yr))
#      case_when(numLabs )
# }
# Read in formatted finance data
#dfx <- readRDS("//cdsfsp06/InterSite/Integra/analytics/Misc_Projects/Misc/Providence_Open_Data/campaign_finance_2018-07-30.rds")
#dfx <- readRDS("//cdsfsp06/InterSite/Integra/analytics/Misc_Projects/Misc/Providence_Open_Data/campaign_finance_2018-08-01.rds")

# dir <- "/Users/jeffreyrichardson/Documents/Campaign_Finance/Psephology_App/"
# 
# # Loans by Year
# yearlyDonorLoans <- readRDS(paste(dir, "Yearly_Donor_Loans.rds", sep = ""))
# 
# # Donations by Year
# yearlyDonorOrgs <- readRDS(paste(dir, "Yearly_Donor_Orgs.rds", sep = ""))
# 
# # Donations
# yearlyDonorDonations <- readRDS(paste(dir, "yearlyDonorDonations.rds", sep = ""))
# 
# # Top Donors
# yearlyTopDonations <- readRDS(paste(dir, "yearlyTopDonations.rds", sep = ""))
# 
# # Donations by Donor City, State
# yearlyLoc <- readRDS(paste(dir, "yearlyLoc.rds", sep = ""))
# 
# # Donations by Employer City, State
# yearlyLocEmp <- readRDS(paste(dir, "yearlyLocEmp.rds", sep = ""))
# yearlyStateEmp <- readRDS(paste(dir, "yearlyStateEmp.rds", sep = ""))
# 
# # PAC Donations
# yearlyPAC <- readRDS(paste(dir, "yearlyPAC.rds", sep = ""))
# 
# # Orgizations
# yearlyOrg <- readRDS(paste(dir, "yearlyOrg.rds", sep = ""))
# 
# # By Employer
# yearlyEmp <- readRDS(paste(dir, "yearlyEmp.rds", sep = ""))
# 
# # Employer to Organization
# yearlyEmpOrg <- readRDS(paste(dir, "yearlyEmpOrg.rds", sep = ""))
# 
# # Donor to Pac
# yearlyDonorPAC <- readRDS(paste(dir, "yearlyDonorPAC.rds", sep = ""))
# 
# # Donor & Donor Info to Organization
# yearlyDonationSummary <- readRDS(paste(dir, "yearlyDonationSummary.rds", sep = ""))
# 
# # Top Donors
# topDonors <- readRDS(paste(dir, "topDonors.rds", sep = ""))
# 
# # All Donors
# allDonors <- readRDS(paste(dir, "allDonors.rds", sep = ""))
# 
# # All Donors sans loans
# allDonors_noloans <- readRDS(paste(dir, "allDonors_noloans.rds", sep = ""))
# 
# # Time Series Data
# timeSeries <- readRDS(paste(dir, "timeSeries.rds", sep = ""))
# 
# # Industry Data
# yearlyInd <- readRDS(paste(dir, "yearlyInd.rds", sep = ""))
# yearlyIndEmp <- readRDS(paste(dir, "yearlyIndEmp.rds", sep = ""))
# 
# # Gina vs the field
# ginaVs <- readRDS(paste(dir, "ginaVs.rds", sep = ""))
# 
# # Dynasties - Grouped by First & Last Name...John, Robert, Smith, Jones, etc
# dynasties <- readRDS(paste(dir, "dynasties.rds", sep = ""))
# 
# # Network Visualizations of Employers to Orgs
# donateVis <- readRDS(paste(dir, "donateVis.rds", sep = ""))
# donateVisInd <- readRDS(paste(dir, "donateVisInd.rds", sep = ""))
# 
# vizFull <- readRDS(paste(dir, "vizFull.rds", sep = ""))
# viz_emp <- readRDS(paste(dir, "viz_emp.rds", sep = ""))
# 
# # Read in donor to org dataframe
# donateVisOrg <- readRDS(paste(dir, "donateVisOrg.rds", sep = ""))
# 
# # Expenditure Descriptions
# expDesc <- readRDS(paste(dir, "expenditure_desc.rds", sep = ""))
# 
# # Read in company expenditures
# expComp <- readRDS(paste(dir, "company_expenditures.rds", sep = ""))
# 
# # Expenditure Networks
# expNet <- readRDS(paste(dir, "expNet.rds", sep = ""))
# 
# # Top Companies
# topComp <- readRDS(paste(dir, "top_company_expenditures.rds", sep = ""))
# 
# # Expenditures from Organizations to companies
# expOrgComp <- readRDS(paste(dir, "comp_org_expenditures.rds", sep = ""))
# expOrg <- readRDS(paste(dir, "org_expenditures.rds", sep = ""))
# 
# # Campaign Finance Contributions
# dfx <- readRDS(paste(dir, "campaign_finance_2018-09-22.rds", sep = "")) %>% 
#      transform(Donor_Name_Unformatted = str_to_title(trimws(Donor_Name_Unformatted))) %>% 
#      transform(OrganizationName = str_to_title(trimws(OrganizationName))) %>% 
#      transform(OrganizationName = gsub("^Ri ", "RI ", OrganizationName)) %>% 
#      transform(OrganizationName = gsub(" Ri$", " RI", OrganizationName)) %>% 
#      transform(OrganizationName = gsub(" Ri ", " RI ", OrganizationName),
#                Employer = str_to_title(trimws(Employer)),
#                donor_st = str_to_upper(donor_st), emp_st = str_to_upper(emp_st),
#                donor_state_name = str_to_title(donor_state_name),emp_state_name = str_to_title(emp_state_name),
#                donor_city = str_to_title(donor_city), emp_city = str_to_title(emp_city),
#                donor_region = ifelse(is.na(donor_region), "Other", donor_region),
#                emp_region = ifelse(is.na(emp_region), "Other", emp_region)) %>% 
#      transform(Employer = gsub(" Ri$", " RI", Employer)) %>% 
#      transform(Employer = gsub("^Ri ", "RI ", Employer)) %>% 
#      transform(Employer = gsub(" Ri ", " RI ", Employer)) %>% 
#      mutate(Month_Yr = format(ReceiptDate, "%Y %m")) %>% 
#      mutate(City = paste(donor_city, donor_st, sep = ", "),
#             Emp_City = paste(emp_city, emp_st, sep = ", ")); head(dfx)




dir <- "/Users/jeffreyrichardson/Documents/Campaign_Finance/Psephology_App/"

# Loans by Year
yearlyDonorLoans <- readRDS("Yearly_Donor_Loans.rds")

# Donations by Year
yearlyDonorOrgs <- readRDS("Yearly_Donor_Orgs.rds")

# Donations
yearlyDonorDonations <- readRDS("yearlyDonorDonations.rds")

# Top Donors
yearlyTopDonations <- readRDS("yearlyTopDonations.rds")

# Donations by Donor City, State
yearlyLoc <- readRDS("yearlyLoc.rds")

# Donations by Employer City, State
yearlyLocEmp <- readRDS("yearlyLocEmp.rds")
yearlyStateEmp <- readRDS("yearlyStateEmp.rds")

# PAC Donations
yearlyPAC <- readRDS("yearlyPAC.rds")

# Orgizations
yearlyOrg <- readRDS("yearlyOrg.rds")

# By Employer
yearlyEmp <- readRDS("yearlyEmp.rds")

# Employer to Organization
yearlyEmpOrg <- readRDS("yearlyEmpOrg.rds")

# Donor to Pac
yearlyDonorPAC <- readRDS("yearlyDonorPAC.rds")

# Donor & Donor Info to Organization
yearlyDonationSummary <- readRDS("yearlyDonationSummary.rds")

# Top Donors
topDonors <- readRDS("topDonors.rds")

# All Donors
allDonors <- readRDS("allDonors.rds")

# All Donors sans loans
allDonors_noloans <- readRDS("allDonors_noloans.rds")

# Time Series Data
timeSeries <- readRDS("timeSeries.rds")

# Industry Data
yearlyInd <- readRDS("yearlyInd.rds")
yearlyIndEmp <- readRDS("yearlyIndEmp.rds")

# Gina vs the field
ginaVs <- readRDS("ginaVs.rds")

# Dynasties - Grouped by First & Last Name...John, Robert, Smith, Jones, etc
dynasties <- readRDS("dynasties.rds")

# Network Visualizations of Employers to Orgs
donateVis <- readRDS("donateVis.rds")
donateVisInd <- readRDS("donateVisInd.rds")

vizFull <- readRDS("vizFull.rds")
viz_emp <- readRDS("viz_emp.rds")

# Read in donor to org dataframe
donateVisOrg <- readRDS("donateVisOrg.rds")

# Expenditure Descriptions
expDesc <- readRDS("expenditure_desc.rds")

# Read in company expenditures
expComp <- readRDS("company_expenditures.rds")

# Expenditure Networks
expNet <- readRDS("expNet.rds")

# Top Companies
topComp <- readRDS("top_company_expenditures.rds")

# Expenditures from Organizations to companies
expOrgComp <- readRDS("comp_org_expenditures.rds")
expOrg <- readRDS("org_expenditures.rds")

# Campaign Finance Contributions
dfx <- readRDS("campaign_finance_2018-09-22.rds") %>% 
     transform(Donor_Name_Unformatted = str_to_title(trimws(Donor_Name_Unformatted))) %>% 
     transform(OrganizationName = str_to_title(trimws(OrganizationName))) %>% 
     transform(OrganizationName = gsub("^Ri ", "RI ", OrganizationName)) %>% 
     transform(OrganizationName = gsub(" Ri$", " RI", OrganizationName)) %>% 
     transform(OrganizationName = gsub(" Ri ", " RI ", OrganizationName),
               Employer = str_to_title(trimws(Employer)),
               donor_st = str_to_upper(donor_st), emp_st = str_to_upper(emp_st),
               donor_state_name = str_to_title(donor_state_name),emp_state_name = str_to_title(emp_state_name),
               donor_city = str_to_title(donor_city), emp_city = str_to_title(emp_city),
               donor_region = ifelse(is.na(donor_region), "Other", donor_region),
               emp_region = ifelse(is.na(emp_region), "Other", emp_region)) %>% 
     transform(Employer = gsub(" Ri$", " RI", Employer)) %>% 
     transform(Employer = gsub("^Ri ", "RI ", Employer)) %>% 
     transform(Employer = gsub(" Ri ", " RI ", Employer)) %>% 
     mutate(Month_Yr = format(ReceiptDate, "%Y %m")) %>% 
     mutate(City = paste(donor_city, donor_st, sep = ", "),
            Emp_City = paste(emp_city, emp_st, sep = ", ")); head(dfx)



srce <- "http://www.ricampaignfinance.com/RIPublic/Filings.aspx"

subIndustries <- sort(unique(dfx$Industry))
industries <- sort(unique(dfx$Industry2))
#visIndustries <- sort(unique(donateVisInd$Industry[donateVisInd$Ind == "Sub Industry"]))
donors <- sort(unique(topDonors$FullName))
employers <- sort(unique(dfx$Employer))
organizations <- sort(unique(dfx$OrganizationName))
donor_cities <- sort(unique(dfx$donor_city))
donor_states <- sort(unique(grep("^[a-zA-Z]{2}$", unique(yearlyLoc$donor_st), value = TRUE)))
emp_cities <- sort(unique(dfx$emp_city))
exp_descriptions <- sort(unique(expComp$ExpDesc))
exp_organization <- sort(unique(expOrg$OrganizationName))
exp_companies <- sort(unique(topComp$FullName))
exp_dex <- sort(unique(expNet$ExpDesc))
# Latest Data Extraction
#upload_date <- format(ymd("2018-08-01"), "%b %d, %Y")
upload_date <- "Dec 31, "
upload_date18 <- "Sep 19, "
#rm(ui, server)
# Run App

# Run App
ui <- fluidPage(useShinyjs(),#tweaks, 
                theme = shinytheme("journal"), # shinythemes::themeSelector(),#theme = shinytheme("cerulean"), # #theme = shinytheme("yeti"), # From shinythemes package  to pick a theme, use: shinythemes::themeSelector(), 
                tags$head(tags$style(HTML(" .selectize-input, .selectize-dropdown { font-size: 90%; }"))), # Decrease Dropdown text size in sidebar panel
                tags$style(type = "text/css", "label { font-size: 13px; }"),  # Decrease other text size in sidebar panel
                # tagList(
                #      singleton(tags$head(tags$script(src='//cdn.datatables.net/fixedheader/2.1.2/js/dataTables.fixedHeader.min.js',type='text/javascript'))),
                #      singleton(tags$head(tags$link(href='//cdn.datatables.net/fixedheader/2.1.2/css/dataTables.fixedHeader.css',rel='stylesheet',type='text/css')))
                # ),
                # titlePanel(h2("RI Campaign Finance Data", align = "center")), # Title text size h2() and centered
                sidebarLayout(
                     sidebarPanel(width = 3,
                                  fluidRow(tags$img(src='HopeFlag.png', align = "left", height = "90%", width="90%")),
                                  br(),
                                  # fluidRow(sliderInput(inputId = "slider1",
                                  #                      label = "Number of Observations",
                                  #                      value = 15,
                                  #                      min = 5, max = 35)),
                                  # br(),
                                  helpText("Change the slider range to refresh data"),
                                  fluidRow(sliderInput("sliderRange", "Select Date Range:", min = 2002, max = 2018, value = c(2014,2018), sep = "")),
                                  # br(),
                                  #br(),
                                  #helpText(paste("All data downloaded from\n ", srce, sep = "")),
                                  # fluidRow(actionButton("go", "Update")),
                                  br(),
                                  fluidRow(tags$img(src='StateRI.png', align = "left", height = "90%", width="90%"))),
                     #br(),
                     #helpText(paste("All data extracted from ", srce, sep = ""))),
                     #br()),
                     #imageOutput("Flag")),
                     #tags$img(src='/Users/jeffreyrichardson/Documents/Campaign_Finance/www/HopeFlag.png', align = "left", height = "90%", width="100%")),
                     mainPanel(width = 9,#h5("Main Panel Description"),
                               navbarPage("",#srce, #inverse = TRUE,
                                          tabPanel("Donors",
                                                   tabsetPanel(
                                                        tabPanel("Top Donors",
                                                                 br(),
                                                                 fluidRow(sliderInput(inputId = "DonorSlider",
                                                                                      label = "Number of Observations",
                                                                                      value = 15,
                                                                                      min = 5, max = 35)),
                                                                 br(),
                                                                 fluidRow(plotOutput("donor_plot")),
                                                                 fluidRow(div(DT::dataTableOutput("donor_tbl"), style = "font-size:90%"))),
                                                        tabPanel("Loan Proceeds",
                                                                 br(),
                                                                 fluidRow(sliderInput(inputId = "sliderLoan",
                                                                                      label = "Number of Observations",
                                                                                      value = 15,
                                                                                      min = 5, max = 35)),
                                                                 br(),
                                                                 fluidRow(plotOutput("loan_plot")),
                                                                 fluidRow(div(DT::dataTableOutput("loan_tbl"), style = "font-size:90%"))),
                                                        tabPanel("Donations",
                                                                 br(),
                                                                 fluidRow(column(width = 6, 
                                                                                 selectInput('donor_select', 'Choose a Donor', c(Choose='', donors), selectize=TRUE, selected = "Mattiello, Nicholas")),
                                                                          column(width = 6,
                                                                                 br(),
                                                                                 h4("Donor Summary All Time (since 2002)"))),
                                                                 fluidRow(#column(width = 4),
                                                                      column(width = 12,
                                                                             fluidRow(tableOutput("donorSummary_tbl")))),
                                                                 br(),
                                                                 fluidRow(column(width = 6,
                                                                                 h4("Other Donor Info")),
                                                                          column(width = 6,
                                                                                 sliderInput(inputId = "sliderDonorSummary",
                                                                                             label = "Number of Observations",
                                                                                             value = 15,
                                                                                             min = 5, max = 35))),
                                                                 br(),
                                                                 fluidRow(plotOutput("donations_plot", height = "400px")),#, width = "90%")),
                                                                 br(),
                                                                 fluidRow(column(width = 6,
                                                                                 plotOutput("donate_daily_plot", height = "300px")),
                                                                          column(width = 6,
                                                                                 plotOutput("donate_monthly_plot", height = "300px"))),
                                                                 br(),
                                                                 h4("Summary of Donations", align = "center"),
                                                                 br(),
                                                                 fluidRow(div(DT::dataTableOutput("donations_tbl"), style = "font-size:90%"))),
                                                        tabPanel("Donor Info",
                                                                 fluidRow(selectInput('donor_select', 'Donor', c(Choose='', donors), selectize=TRUE, selected = "Mattiello, Nicholas")),
                                                                 br(),
                                                                 fluidRow(div(DT::dataTableOutput("donor_info_tbl"), style = "font-size:90%"))))),
                                          tabPanel("Organizations",
                                                   tabsetPanel(
                                                        tabPanel("Top Organizations", 
                                                                 br(),
                                                                 fluidRow(column(width = 6,
                                                                                 checkboxInput("oneInd", "Filter to Specific Industry"),
                                                                                 conditionalPanel(
                                                                                      condition = "input.oneInd == true",
                                                                                      selectInput("selectIndustry", "Select Industry", choices = industries, selected = "Healthcare"))),
                                                                          column(width = 6,
                                                                                 sliderInput(inputId = "sliderOrg",
                                                                                             label = "Number of Observations",
                                                                                             value = 15,
                                                                                             min = 5, max = 35))),
                                                                 
                                                                 fluidRow(width = NULL, plotOutput("org_plot", height = "700px")),
                                                                 fluidRow(div(DT::dataTableOutput("org_tbl"), style = "font-size:90%"))),
                                                        tabPanel("Next Tab", 
                                                                 fluidRow(plotOutput("next_org_plot"))))),
                                          tabPanel("Employers",
                                                   tabsetPanel(
                                                        tabPanel("Top Employers",
                                                                 br(),
                                                                 fluidRow(sliderInput(inputId = "sliderEmp",
                                                                                      label = "Number of Observations",
                                                                                      value = 15,
                                                                                      min = 5, max = 35)),
                                                                 fluidRow(plotOutput("emp_plot", height = "700px")),
                                                                 fluidRow(div(DT::dataTableOutput("emp_tbl"), style = "font-size:90%"))),
                                                        tabPanel("Next Tab", 
                                                                 fluidRow(plotOutput("next_emp_plot"))))),
                                          tabPanel("PACS",
                                                   tabsetPanel(
                                                        tabPanel("Top PACs",
                                                                 br(),
                                                                 fluidRow(sliderInput(inputId = "sliderPac",
                                                                                      label = "Number of Observations",
                                                                                      value = 15,
                                                                                      min = 5, max = 35)),
                                                                 br(),
                                                                 #fluidRow(plotOutput("pac_plot", height = "700px")),
                                                                 #fluidRow(plotOutput("pac_plot", height = "700px")),
                                                                 fluidRow(div(DT::dataTableOutput("pac_tbl"), style = "font-size:90%"))),
                                                        tabPanel("Next Tab", 
                                                                 fluidRow(plotOutput("next_pac_plot"))))),
                                          tabPanel("Industries",
                                                   tabsetPanel(
                                                        tabPanel("Top Industries",
                                                                 br(),
                                                                 fluidRow(column(width = 6,
                                                                                 radioButtons("subInd", label = h5("Industry or Sub-Indsutry"),inline = TRUE,
                                                                                              choices = c("Industry","Sub Industry"),
                                                                                              selected = "Industry")),
                                                                          column(width = 6,
                                                                                 sliderInput(inputId = "sliderInd",
                                                                                             label = "Number of Observations",
                                                                                             value = 15,
                                                                                             min = 5, max = 35))),
                                                                 br(),
                                                                 fluidRow(plotOutput("ind_plot", height = "700px")),
                                                                 fluidRow(div(DT::dataTableOutput("ind_tbl"), style = "font-size:90%"))),
                                                        tabPanel("Next Tab",
                                                                 fluidRow(div(DT::dataTableOutput("next_ind_tbl"), style = "font-size:90%"))))),
                                          tabPanel("Regional",
                                                   tabsetPanel(
                                                        tabPanel("By City",
                                                                 br(),
                                                                 fluidRow(column(width = 6,
                                                                                 checkboxInput("oneState", "Filter to Specific State"),
                                                                                 conditionalPanel(
                                                                                      condition = "input.oneState == true",
                                                                                      selectInput("donorState", "Select State", choices = donor_states, selected = "RI"))),
                                                                          column(width = 6,
                                                                                 sliderInput(inputId = "sliderCity",
                                                                                             label = "Number of Observations",
                                                                                             value = 15,
                                                                                             min = 5, max = 35))),
                                                                 
                                                                 # fluidRow(selectInput("checkDonorState","Choose State(s):",donor_states, selected = donor_states,
                                                                 #                      multipe = TRUE)),
                                                                 #actionLink("selectall","Select All")),
                                                                 fluidRow(plotOutput("city_plot")),
                                                                 fluidRow(div(DT::dataTableOutput("city_tbl"), style = "font-size:90%"))),
                                                        tabPanel("By State", 
                                                                 br(),
                                                                 fluidRow(sliderInput(inputId = "sliderState",
                                                                                      label = "Number of Observations",
                                                                                      value = 15,
                                                                                      min = 5, max = 35)),
                                                                 fluidRow(plotOutput("state_plot")),
                                                                 br(),
                                                                 fluidRow(column(width = 6, plotOutput("statePolar_plot")),
                                                                          column(width = 6, plotOutput("statePolar_plot2"))),
                                                                 #br(),
                                                                 fluidRow(div(DT::dataTableOutput("state_tbl"), style = "font-size:90%"))),
                                                        tabPanel("Governer Raimondo vs The Field",
                                                                 br(),
                                                                 fluidRow(plotOutput("gina_plot", height = "700px")),
                                                                 fluidRow(plotOutput("gina_plot_overall2")),
                                                                 fluidRow(div(DT::dataTableOutput("gina__overall_tbl"), style = "font-size:90%"))))),
                                          tabPanel("Expenditures",
                                                   tabsetPanel(
                                                        tabPanel("Types", 
                                                                 br(),
                                                                 fluidRow(column(width = 3,
                                                                                 checkboxInput("oneType", h4("Filter to Specific Expenditure Type"))),
                                                                          column(width = 3,
                                                                                 conditionalPanel(
                                                                                      condition = "input.oneType == true",
                                                                                      selectInput("exp_type", "Expenditure Type", choices = exp_descriptions, selected = "Advertising"))),
                                                                          column(width = 3,
                                                                                 conditionalPanel(
                                                                                      condition = "input.oneType == true",
                                                                                      radioButtons("exp_view", "View By",
                                                                                                   choices = c("Organization (Candidate)","Expenditure Recipient"),
                                                                                                   selected = "Organization (Candidate)"))),
                                                                          column(width = 3,
                                                                                 conditionalPanel(
                                                                                      condition = "input.oneType == true",
                                                                                      sliderInput(inputId = "exp_type_slider",
                                                                                                  label = "Number of Observations",
                                                                                                  value = 25,
                                                                                                  min = 5, max = 45)))),
                                                                 br(),
                                                                 fluidRow(plotOutput("expDesc_plot", height = "700px")),
                                                                 fluidRow(div(DT::dataTableOutput("expDesc_tbl"), style = "font-size:90%"))))),
                                          # tabPanel("Expenditures",
                                          #          tabsetPanel(
                                          #               tabPanel("Types", 
                                          #                        fluidRow(plotOutput("expDesc_plot", height = "700px")),
                                          #                        fluidRow(div(DT::dataTableOutput("expDesc_tbl"), style = "font-size:90%"))),
                                          #               tabPanel("Companies", 
                                          #                        br(),
                                          #                        fluidRow(column(width = 4,
                                          #                                        checkboxInput("oneType", "Filter to Specific Expenditure Type"),
                                          #                                        conditionalPanel(
                                          #                                             condition = "input.oneType == true",
                                          #                                             selectInput("exp_type", "Expenditure Type", choices = exp_descriptions, selected = "Advertising"))),
                                          #                                 column(width = 4,
                                          #                                        checkboxInput("oneComp", "Filter to Specific Company"),
                                          #                                        conditionalPanel(
                                          #                                             condition = "input.oneComp == true",
                                          #                                             selectInput("exp_comps", "Full Name", choices = exp_companies, selected = "Blue West Media"))),
                                          #                                 column(width = 4,
                                          #                                        sliderInput(inputId = "expCo_slider",
                                          #                                                    label = "Number of Observations",
                                          #                                                    value = 25,
                                          #                                                    min = 5, max = 45))),
                                          #                        fluidRow(plotOutput("expCo_plot", height = "700px")),
                                          #                        fluidRow(div(DT::dataTableOutput("expCo_tbl"), style = "font-size:90%"))),
                                          # tabPanel("Candidates", 
                                          #          br(),
                                          #          fluidRow(column(width = 6,
                                          #                          checkboxInput("oneOrg", "Filter to Specific Organization"),
                                          #                          conditionalPanel(
                                          #                               condition = "input.oneOrg == true",
                                          #                               selectInput("exp_org", "Organizations", choices = exp_organization, selected = "Allan W Fung"))),
                                          #                   column(width = 6,
                                          #                          sliderInput(inputId = "expSliderOrg",
                                          #                                      label = "Number of Observations",
                                          #                                      value = 15,
                                          #                                      min = 5, max = 45))),
                                          #          br(),
                                          #          fluidRow(plotOutput("expOrg_plot", height = "700px")),
                                          #          br(),
                                          #          fluidRow(div(DT::dataTableOutput("expOrg_tbl"), style = "font-size:90%"))))),
                                          # tabPanel("Candidates",
                                          #          tabsetPanel(
                                          #               tabPanel("Candidates", 
                                          #                        fluidRow(plotOutput("cand_plot")),
                                          #                        fluidRow(plotOutput("cand_plot_run")),
                                          #                        fluidRow(div(DT::dataTableOutput("cand_tbl"), style = "font-size:90%"))),
                                          #               tabPanel("By State", 
                                          #                        fluidRow(plotOutput("cand_st_plot")),
                                          #                        fluidRow(plotOutput("cand_st_plot2")),
                                          #                        fluidRow(div(DT::dataTableOutput("cand_st_tbl"), style = "font-size:90%"))))),
                                          # tabPanel("Race for Gov",
                                          #          tabsetPanel(
                                          #               tabPanel("Candidates", 
                                          #                        fluidRow(plotOutput("gov18_plot")),
                                          #                        fluidRow(plotOutput("gov18_plot_run")),
                                          #                        fluidRow(div(DT::dataTableOutput("gov18_tbl"), style = "font-size:90%"))),
                                          #               tabPanel("By State", 
                                          #                        fluidRow(plotOutput("gov18_plot2")),
                                          #                        fluidRow(plotOutput("gov18_plot_run2")),
                                          #                        fluidRow(div(DT::dataTableOutput("gov18_donorSt_tbl"), style = "font-size:90%"))))),
                                          tabPanel("Network Vis",
                                                   tabsetPanel(
                                                        # tabPanel("by Employer", 
                                                        #          #br(),
                                                        #          br(),
                                                        #          fluidRow(column(width = 3,
                                                        #                          fluidRow(sliderInput("sliderVis", "Observations", min = 5, max = 1000, value = 25)),
                                                        #                          fluidRow(sliderInput("vertexSlide", "Vertex Size", min = 0.01, max = 10, value = 2))),
                                                        #                   column(width = 1),
                                                        #                   column(width = 3,
                                                        #                          fluidRow(radioButtons("layout", label = "Type of Layout",inline = FALSE,
                                                        #                                                choices = c("spring","circle"),
                                                        #                                                selected = "spring")),#,
                                                        #                          fluidRow(sliderInput("labelPos", label = "Edge Label Position",min = 0.01, max = 0.99, value = 0.5))),
                                                        #                   column(width = 1),
                                                        #                   column(width = 3,
                                                        #                          fluidRow(sliderInput("nodeSlide", "Node Label Size", min = 0.01, max = 20, value = 5)),
                                                        #                          fluidRow(sliderInput("edgeLabSlide", "Edge Label Size", min = 0.01, max = 3, value = 0.5)))),
                                                        #          fluidRow(checkboxInput("visInd", h4("Filter to Specific Industry")),
                                                        #                   conditionalPanel(
                                                        #                        condition = "input.visInd == true",
                                                        #                        selectInput("visIndustry", "Select Industry", choices = industries, selected = "Healthcare",
                                                        #                                    multiple = FALSE, selectize = TRUE))),
                                                        #          #                                                                 br(),
                                                        #          fluidRow(plotOutput("empVis_plot", height = "600px")),
                                                        #          #fluidRow(plotOutput("net_plot2")),
                                                        #          fluidRow(div(DT::dataTableOutput("net_tbl"), style = "font-size:90%"))),
                                                        tabPanel("by Donor", 
                                                                 br(),
                                                                 fluidRow(column(width = 3,
                                                                                 fluidRow(sliderInput("sliderVisOrg", "Observations", min = 5, max = 1000, value = 40)),
                                                                                 fluidRow(sliderInput("vertexSlideOrg", "Vertex Size", min = 0.01, max = 10, value = 2))),
                                                                          column(width = 1),
                                                                          column(width = 3,
                                                                                 fluidRow(radioButtons("layoutOrg", label = "Type of Layout",inline = FALSE,
                                                                                                       choices = c("spring","circle"),
                                                                                                       selected = "spring")),#,
                                                                                 fluidRow(sliderInput("labelPosOrg", label = "Edge Label Position",min = 0.01, max = 0.95, value = 0.5))),
                                                                          column(width = 1),
                                                                          column(width = 3,
                                                                                 fluidRow(sliderInput("nodeSlideOrg", "Node Label Size", min = 0.01, max = 20, value = 5)),
                                                                                 fluidRow(sliderInput("edgeLabSlideOrg", "Edge Label Size", min = 0.01, max = 3, value = 0.45)))),
                                                                 fluidRow(column(width = 4,
                                                                                 checkboxInput("visOrg", h4("Filter to Specific Industry")),
                                                                                 conditionalPanel(
                                                                                      condition = "input.visOrg == true",
                                                                                      selectInput("visIndustryOrg", "Select Industry", choices = industries, selected = "Healthcare",
                                                                                                  multiple = FALSE, selectize = TRUE))),
                                                                          column(width = 8,
                                                                                 br(),
                                                                                 h4("Visualization of Connections between Donors & Organizations"))),
                                                                 #                                                                 br(),
                                                                 fluidRow(plotOutput("visOrg_plot", height = "600px")),
                                                                 fluidRow(div(DT::dataTableOutput("visOrg_tbl"), style = "font-size:90%"))),
                                                        tabPanel("by Employer",
                                                                 fluidRow(column(width = 4, sliderInput("slider_viz_emp", "Observations", min = 5, max = 1000, value = 40)),
                                                                          column(width = 4, radioButtons("layout_viz_emp", label = "Type of Layout",inline = TRUE,
                                                                                                         choices = c("spring","circle"),
                                                                                                         selected = "spring")),
                                                                          column(width = 4, sliderInput("vertexSize_viz_emp", "Node Size", min = 0.01, max = 10, value = 2))),
                                                                 fluidRow(column(width = 4, sliderInput("edgeLabSlide_viz_emp", "Edge Label Size", min = 0.01, max = 3, value = 0.45)),
                                                                          column(width = 4, checkboxInput("viz_emp_fade", "Fade", value = FALSE)),
                                                                          column(width = 4, sliderInput("nodeSlide_viz_emp", "Node Label Size", min = 0.01, max = 20, value = 5))),
                                                                 #        fluidRow(sliderInput("slider_viz_emp", "Observations", min = 5, max = 1000, value = 40)),
                                                                 #        fluidRow(sliderInput("vertexSize_viz_emp", "Node Size", min = 0.01, max = 10, value = 2))),
                                                                 # #column(width = 1),
                                                                 # column(width = 4,
                                                                 #        fluidRow(radioButtons("layout_viz_emp", label = "Type of Layout",inline = FALSE,
                                                                 #                              choices = c("spring","circle"),
                                                                 #                              selected = "spring")),#,
                                                                 #        fluidRow(sliderInput("labelPos_viz_emp", label = "Edge Label Position",min = 0.01, max = 0.95, value = 0.5))),
                                                                 # #column(width = 1),
                                                                 # column(width = 4,
                                                                 #        fluidRow(sliderInput("nodeSlide_viz_emp", "Node Label Size", min = 0.01, max = 20, value = 5)),
                                                                 #        fluidRow(sliderInput("edgeLabSlide_viz_emp", "Edge Label Size", min = 0.01, max = 3, value = 0.45)))),
                                                                 fluidRow(column(width = 4,
                                                                                 checkboxInput("viz_emp_box", h5("Filter to Specific Industry")),
                                                                                 conditionalPanel(
                                                                                      condition = "input.viz_emp_box == true",
                                                                                      selectInput("viz_emp_inds", "Select Industry", choices = ""))),# choices = industries, selected = "Healthcare",multiple = FALSE, selectize = TRUE))),
                                                                          column(width = 4,
                                                                                 conditionalPanel(
                                                                                      condition = "input.viz_emp_box == true",
                                                                                      radioButtons("viz_emp_ind", "Industry or Sub-Industry", choices = c("Industry","SubIndustry"), selected = "Industry"))),
                                                                          column(width = 4,
                                                                                 radioButtons("viz_loans", "Include Loans?", choices = c("Include Loans", "Remove Loans"), selected = "Remove Loans"))),
                                                                 fluidRow(plotOutput("viz_emp_plot", height = "600px")),
                                                                 fluidRow(div(DT::dataTableOutput("viz_emp_tbl"), style = "font-size:90%"))),
                                                        tabPanel("Expenditure Networks", 
                                                                 fluidRow(column(width = 4, sliderInput("slider_exp_net", "Observations", min = 5, max = 1000, value = 40)),
                                                                          column(width = 4, radioButtons("layout_exp_net", label = "Type of Layout",inline = TRUE,
                                                                                                         choices = c("spring","circle"),
                                                                                                         selected = "spring")),
                                                                          column(width = 4, sliderInput("vertexSize_exp_net", "Node Size", min = 0.01, max = 10, value = 2))),
                                                                 fluidRow(column(width = 4, sliderInput("edgeLabSlide_exp_net", "Edge Label Size", min = 0.01, max = 3, value = 0.45)),
                                                                          column(width = 4, checkboxInput("exp_net_fade", "Fade", value = FALSE)),
                                                                          column(width = 4, sliderInput("nodeSlide_exp_net", "Node Label Size", min = 0.01, max = 20, value = 5))),
                                                                 fluidRow(column(width = 4,
                                                                                 radioButtons("exp_net_view", h5("Color By"), choices = c("Candidate", "Company"), selected = "Candidate")),
                                                                          column(width = 4,
                                                                                 checkboxInput("viz_exp_net_box", h4("Filter to Type of Expenditure"))),
                                                                          column(width = 4, 
                                                                                 conditionalPanel(
                                                                                      condition = "input.viz_exp_net_box == true",
                                                                                      selectInput("viz_exp_net", "Expenditure Description", choices = exp_dex)))),
                                                        fluidRow(plotOutput("exp_net_plot", height = "700px")),
                                                        fluidRow(div(DT::dataTableOutput("exp_net_tbl"), style = "font-size:90%"))))),
                               tabPanel("Misc",
                                        tabsetPanel(
                                             tabPanel("Time Series",
                                                      fluidRow(radioButtons("timeSeriesDF", label = h3("Choose an Option"),
                                                                            choices = unique(timeSeries$Loans), inline = TRUE,
                                                                            selected = "Loans Included")),
                                                      br(),
                                                      fluidRow(#column(width = 6, 
                                                           plotOutput("time_plot", height = "400px")),
                                                      #column(width = 6,
                                                      fluidRow(
                                                           plotOutput("time_plot_cum", height = "400px")),
                                                      fluidRow(div(DT::dataTableOutput("time_tbl"), style = "font-size:90%"))),
                                             tabPanel("N Percent", 
                                                      br(),
                                                      fluidRow(h3("Compare the top N to the rest of the population")),
                                                      br(),
                                                      fluidRow(column(width = 3,
                                                                      fluidRow(sliderInput("top_n", "Choose Number of Donors (N)", min = 10, max = 50000, value = 100, step = 10)),
                                                                      fluidRow(checkboxInput("top_n_loan", h5("Include Loans?")))),
                                                               column(width = 6,
                                                                      fluidRow(helpText("There are likely many duplicates in the 110,000+ names below.  A computer interprets \'Doe, Jane\' differently than \'Doe, Ms Jane\'.
                                                                                                   Estimate the avg variations in the slider on the right to approximate the real number of people making contributions"))),
                                                               column(width = 3,
                                                                      sliderInput("avg_names", "Avg Name Variations", min = 1, max = 5, value = 2.5, step = 0.1))),
                                                      br(),
                                                      fluidRow(div(tableOutput("top_n_tbl"), style = "font-size:90%")),
                                                      fluidRow(plotOutput("top_n_plot", height = "400px", width = "75%")),
                                                      fluidRow(div(DT::dataTableOutput("top_n_tbl2"), style = "font-size:90%"))),
                                             tabPanel("First & Last Names",
                                                      br(),
                                                      fluidRow(column(width = 6,
                                                                      fluidRow(sliderInput(inputId = "sliderDynasty",
                                                                                           label = "Number of Observations",
                                                                                           value = 15,
                                                                                           min = 5, max = 35))),
                                                               column(width = 6,
                                                                      fluidRow(radioButtons("radio_dyn", label = h3("First or Last Name"),
                                                                                            choices = c("First","Last"),
                                                                                            selected = "Last")))),
                                                      br(),
                                                      fluidRow(plotOutput("dynasty_plot")),
                                                      fluidRow(div(DT::dataTableOutput("dynasty_tbl"), style = "font-size:90%"))))),
                               tabPanel("About",
                                        #br(),
                                        #h4("Gitbub repository located here, https://github.com/RI-Psephology/Campaign_Finance"),
                                        br(),
                                        h4("Data publicly available at RI Board of Elections website, http://www.elections.state.ri.us/"),
                                        br(),
                                        h5(paste("Last updated ", upload_date18, "2018", sep = "")),
                                        br(),
                                        h5("Gitbub repository found here, https://github.com/RI-Psephology/Campaign_Finance"),
                                        br())
                     )), position = "left"))

server <- function(input, output, session) {
     
     # output$Flag <- renderImage({
     #      # A temp file to save the output.
     #      # This file will be removed later by renderImage
     #      #outfile <- tempfile(fileext = '.png')
     #      
     #      # Generate the PNG
     #      tags$img(src = "HopeFlag.png", width = "95%", height = "80%")
     # })
     # 
     # output$Flag <- renderImage({
     #      list(src = "http://data-informed.com/wp-content/uploads/2013/11/R-language-logo-224x136.png",
     #           contentType = 'image/png',
     #           width = 224,
     #           height = 136#,
     #           #alt = "This is image alternate text")
     # })
     # 
     # observe({
     #      if(input$selectall == 0) return(NULL) 
     #      else if (input$selectall %% 2 == 0)
     #      {
     #           updateCheckboxGroupInput(session,"checkDonorState","Choose State(s):", choices = donor_states)
     #      }
     #      else
     #      {
     #           updateCheckboxGroupInput(session,"checkDonorState","Choose State(s):", choices = donor_states,selected=donor_states)
     #      }
     # })
     #observe(toggle(id = "donorSelect", condition = ifelse(input$Donors == 'Donations', TRUE, FALSE)))
     
     # In case you need to hide them for some reason
     #   observeEvent(input$tabs != "Donations", {
     #   shinyjs::hide(id = "donorSelect")
     # })
     
     # *********************************************************************** Donors
     
     # Top Donors
     dx <- reactive({
          yearlyDonorOrgs %>%
               filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
               mutate(Range = paste("Jan 1, ", min(CY), " to ", ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>% 
               group_by(FullName,Range) %>%
               summarise(Number_of_Donations = sum(Number_of_Donations, na.rm = TRUE),
                         Donations_Under_100 = sum(Donations_Under_100, na.rm = TRUE),
                         Donations_Over_100 = sum(Donations_Over_100, na.rm = TRUE),
                         Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
                         Total_Donation_Amount = sum(Total_Donation_Amount, na.rm = TRUE),
                         Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
                         Organizations_Donated_To = sum(Organizations_Donated_To, na.rm = TRUE),
                         Org_Donations = sum(Org_Donations, na.rm = TRUE),
                         Org_Donation_Amount = sum(Org_Donation_Amount, na.rm = TRUE),
                         PACs_Donated_To = sum(PACs_Donated_To, na.rm = TRUE),
                         PAC_Donations = sum(PAC_Donations, na.rm = TRUE),
                         PAC_Donation_Amount = sum(PAC_Donation_Amount, na.rm = TRUE)) %>%
               ungroup() %>%
               arrange(desc(Total_Donation_Amount))
     })
     
     # Donor Plot
     output$donor_plot <- renderPlot({
          
          dx2 <- dx() %>% head(input$DonorSlider)
          
          sliderVal <- nrow(dx2)
          
          ggplot(dx2, aes(reorder(FullName, Total_Donation_Amount), Total_Donation_Amount)) +
               geom_bar(stat = "identity", color = "gray50", width = 0.5, fill = "dodgerblue", alpha = 0.8) +
               geom_label(size = textLabelFun(dx2),#3.9, 
                          fontface = "bold", aes(label = monify(Total_Donation_Amount)), hjust = -0.25) + #, hjust = "outward"
               scale_y_continuous(label = dollar, limits = c(0, round(max(dx2$Total_Donation_Amount) * 1.1, 0))) +
               labs(x = "", y = "Total", title = "Top Donors",
                    subtitle = unique(dx2$Range)) +
               coord_flip() +
               theme(axis.text.x = element_text(size = 14,
                                                #axisLabelFun(dx2),
                                                face = "bold"),
                     plot.subtitle = element_text(size = 14),
                     plot.title = element_text(size = 17),
                     axis.text.y = element_text(size = axisLabelFun(dx2), face = "bold"))
          
     })
     
     # Donor Table
     output$donor_tbl <- DT::renderDataTable({
          x <- dx() %>% select(-Range)
          names(x) <- str_wrap(gsub("_", " ", names(x)), 8)
          x
     })
     
     
     # Donations To
     don <- reactive({
          yearlyDonorDonations %>%
               filter(FullName == input$donor_select) %>%
               filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
               mutate(Range = paste("Jan 1, ", min(CY), " to ", ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>% 
               group_by(FullName,OrganizationName,PAC,ContDesc,Range) %>%
               summarise(Number_of_Donations = sum(Number_of_Donations, na.rm = TRUE),
                         Donations_Under_100 = sum(Donations_Under_100, na.rm = TRUE),
                         Donations_Over_100 = sum(Donations_Over_100, na.rm = TRUE),
                         Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
                         Total_Donation_Amount = sum(Total_Donation_Amount, na.rm = TRUE),
                         Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
                         Max_Donation = max(Max_Donation, na.rm = TRUE)) %>%
               ungroup() %>%
               group_by(OrganizationName) %>% 
               mutate(GrandTot = sum(Total_Donation_Amount)) %>% ungroup() %>% 
               transform(ContDesc = factor(ContDesc)) %>% 
               arrange(desc(GrandTot)) %>% select(-GrandTot)
          #arrange(desc(Total_Donation_Amount))
          
     })
     
     donDt <- reactive({
          dfx %>%
               #mutate(Loan_Proceeds = ifelse(ContDesc == "Loan Proceeds", "Loan Proceeds", "Non Loan")) %>%
               filter(FullName == input$donor_select) %>%
               filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
               mutate(Range = paste("Jan 1, ", min(CY), " to ",  ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>% 
               group_by(FullName,ReceiptDate,ContDesc,Range) %>%
               summarise(Total = sum(Amount, na.rm = TRUE)) %>%
               ungroup() %>%
               arrange(ReceiptDate) %>% 
               group_by(ContDesc) %>% 
               mutate(Run = cumsum(Total)) %>% ungroup()
          
          
     })
     
     
     donGrob <- reactive({
          filter(topDonors, FullName == input$donor_select) %>%
               transform(Max_Donation = monify(Max_Donation),
                         Avg_Donation = monify(Avg_Donation),
                         Median_Donation = monify(Median_Donation)) %>% 
               select(-Total_Donated) %>%
               rename(Total = Dollars)
     })
     
     
     # Donor Table
     # output$donorSummary_tbl <- DT::renderDataTable({
     #      x <- donGrob() 
     #      names(x) <- str_wrap(gsub("_", " ", names(x)), 8)
     #      x
     #      
     # })
     
     # Donor Table
     output$donorSummary_tbl <- renderTable({
          x <- donGrob() %>% transform(First_Donation = as.character(First_Donation),
                                       Most_Recent_Donation = as.character(Most_Recent_Donation))
          names(x) <- str_wrap(gsub("_", " ", names(x)), 8)
          x
          
     })
     
     
     donDt2 <- reactive({
          
          dfx %>%
               filter(FullName == input$donor_select) %>%
               filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
               mutate(Range = paste("Jan 1, ", min(CY), " to ",  ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>% 
               #mutate(Mon = format(ReceiptDate, "%Y %m")) %>% 
               #mutate(Month_Yr = as.Date(ReceiptDate))
               group_by(FullName,Month_Yr,ContDesc,Range) %>%
               summarise(Total = sum(Amount, na.rm = TRUE)) %>%
               ungroup() %>%
               mutate(Mo = ymd(gsub("$", "-01", Month_Yr))) 
          
          
     })
     
     
     output$donate_monthly_plot <- renderPlot({
          x <- donDt2()
          
          ggplot(x, aes(Mo, Total)) +
               geom_point(size = 2, aes(color = ContDesc), alpha = 0.7) +
               geom_line(stat = "identity", lwd = 1, aes(group = ContDesc, color = ContDesc)) +
               # geom_bar(stat = "identity", width = 0.9, color = "gray50", aes(fill = ContDesc), position = "stack") +
               scale_y_continuous(label = dollar) +
               #scale_x_date(labels = date_format("%m-%Y"))
               scale_x_date(breaks=pretty_breaks(), date_minor_breaks="1 month") + 
               labs(x = "Donation Month", y = "Monthly Total",
                    title = paste("Monthly Donations from ", unique(x$FullName), sep = ""),
                    subtitle = unique(x$Range)) +
               theme(#legend.position = "bottom",
                    legend.background = element_rect(color = "gray50"),
                    axis.text.x = element_text(size = 14),#angle = 45, hjust = 1, vjust = 1, size = axisLabelFun(x)),
                    plot.subtitle = element_text(size = 14),
                    plot.title = element_text(size = 17),
                    legend.title = element_blank(),
                    legend.position = c(0.25,0.75), 
                    axis.title.x = element_text(size = 11), axis.title.y = element_text(size = 11),
                    axis.text.y = element_text(size = 13))
     })
     
     
     # Donations Plot
     output$donations_plot <- renderPlot({
          
          donx <- don() %>% head(input$sliderDonorSummary)
          #xlabels <- unique()
          
          ggplot(donx, aes(reorder(OrganizationName, Total_Donation_Amount), Total_Donation_Amount, fill = ContDesc)) +
               geom_bar(stat = "identity", color = "gray50", width = 0.5, position = "stack",#fill = "yellowgreen", 
                        #aes(fill = ContDesc),
                        alpha = 0.8) +
               geom_text(size = textLabelFun(donx), fontface = "bold", position = position_stack(vjust = 0.5),
                         aes(label = monify(Total_Donation_Amount)), hjust = -0.25) +
               # geom_label(size = textLabelFun(donx),alpha = 0.5, fontface = "bold", position = position_stack(vjust = 0.5),
               #           aes(label = monify(Total_Donation_Amount), color = ContDesc), hjust = -0.5) +
               scale_y_continuous(label = dollar, limits = c(0, round(max(donx$Total_Donation_Amount) * 1.3, 0))) +
               labs(x = "", y = "Total", title = paste("Orgs Receiving Contributions from ", unique(donx$FullName), sep = ""),
                    subtitle = paste("Jan 1, ", input$sliderRange[1], " to ",  ifelse(input$sliderRange[2] == 2018, upload_date18, upload_date), input$sliderRange[2], sep = "")) +
               coord_flip() +
               theme(axis.text.x = element_text(size = 12, face = "bold"),
                     legend.position = c(0.75,0.25), legend.background = element_rect(color = "gray50"),
                     axis.title.x = element_text(size = 11), axis.title.y = element_text(size = 11),
                     plot.subtitle = element_text(size = 14),
                     plot.title = element_text(size = 17),
                     legend.title = element_blank(),
                     axis.text.y = element_text(size = axisLabelFun(donx), face = "bold")) +
               guides(color = FALSE)
          
     })
     
     # Donations Table
     output$donations_tbl <- DT::renderDataTable({
          x <- don() %>% select(-Range)
          names(x) <- str_wrap(gsub("_", " ", names(x)), 6)
          x
     })
     
     # Donor Info Table
     output$donor_info_tbl <- DT::renderDataTable({
          x <- dfx %>%
               filter(FullName == input$donor_select) %>%
               transform(Donor_Name_Unformatted = str_to_title(Donor_Name_Unformatted)) %>% #filter(FullName == "Mattiello, Nicholas") %>%
               filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
               select(FullName,Donor_Name_Unformatted,Address,CityStZip,donor_city,donor_st,donor_zip,EmployerName,Employer,ReceiptDate,Amount,OrganizationName,PAC,ContDesc) %>%
               distinct() %>% arrange(ReceiptDate)
          
          names(x) <- str_wrap(gsub("_", " ", names(x)), 6)
          x
     })
     # 
     output$donate_daily_plot <- renderPlot({
          x <- donDt()
          dtDiff <- as.numeric(difftime(max(x$ReceiptDate), min(x$ReceiptDate), units = "days"))
          
          #Xlab <- sort(unique(x$))
          
          x %>%
               ggplot(aes(ReceiptDate, Run)) +
               geom_point(size = 2, aes(color = ContDesc), alpha = 0.7) +
               geom_line(stat = "identity", lwd = 1, aes(group = ContDesc, color = ContDesc)) +
               #geom_label_repel(data = filter(x, Total > quantile(x$Total, probs = 0.9)), size = 3, min.segment.length = 0.01, aes(label = monify(Total), color = ContDesc)) +
               labs(x = "Contribution Month", y = "Running Total",
                    title = paste(unique(x$FullName), " Running Total", sep = ""),
                    subtitle = unique(x$Range)) +
               scale_y_continuous(label = dollar) +
               scale_x_date(breaks=pretty_breaks(), date_minor_breaks="1 month") + 
               #scale_x_date(breaks = ifelse(dtDiff < 365, "1 month", ifelse(dtDiff < 1000, "3 months","6 months"))) +
               #theme_bw() +
               theme(axis.text.x = element_text(size = 14),#, angle = 45, hjust = 1, vjust = 1
                     axis.title.x = element_text(size = 11), axis.title.y = element_text(size = 11),
                     plot.subtitle = element_text(size = 14),
                     plot.title = element_text(size = 17),
                     axis.text.y = element_text(size = 14),
                     legend.title = element_blank(),
                     legend.position = c(0.25,0.75), legend.background = element_rect(color = "gray50")) +
               #facet_wrap(~ContDesc, scales = "free_y", ncol = 1) +
               NULL
     })
     
     # *********************************************************************** Loans
     
     # Top Loans
     lp <- reactive({
          
          yearlyDonorLoans %>%
               filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
               mutate(Range = paste("Jan 1, ", min(CY), " to ",  ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>% 
               group_by(FullName,Range) %>%
               summarise(Number_of_Loans = sum(Number_of_Loans, na.rm = TRUE),
                         Total_Loan_Amount = sum(Total_Loan_Amount, na.rm = TRUE),
                         Dollars = monify(Total_Loan_Amount),
                         Avg_Loan_Amount = round(Total_Loan_Amount / Number_of_Loans, 2),
                         Max_Loan_Amount = max(Max_Loan_Amount, na.rm = TRUE),
                         Orgs_Receiving_Loans = paste(unique(Orgs_Receiving_Loans), collapse = " | ")) %>% 
               ungroup() %>%
               arrange(desc(Total_Loan_Amount))
          
     })
     
     # Loan Plot
     output$loan_plot <- renderPlot({
          
          lx <- lp() %>% head(input$sliderLoan)
          
          ggplot(lx, aes(reorder(FullName, Total_Loan_Amount), Total_Loan_Amount)) +
               geom_bar(stat = "identity", color = "gray50", width = 0.5, fill = "darkorange", alpha = 0.8) +
               geom_text(size = textLabelFun(lx), fontface = "bold", aes(label = monify(Total_Loan_Amount)), hjust = -0.25) +
               scale_y_continuous(label = dollar, limits = c(0, round(max(lx$Total_Loan_Amount) * 1.1, 0))) +
               labs(x = "", y = "Total", title = "Top Loan Proceeds",
                    subtitle = unique(lx$Range)) +
               coord_flip() +
               theme(axis.text.x = element_text(size = 14, face = "bold"),
                     plot.subtitle = element_text(size = 14),
                     plot.title = element_text(size = 17),
                     axis.title.x = element_text(size = 11), axis.title.y = element_text(size = 11),
                     axis.text.y = element_text(size = axisLabelFun(lx), face = "bold"))
          
     })
     
     # Loan Table
     output$loan_tbl <- DT::renderDataTable({
          x <- lp() %>% select(-Range)
          names(x) <- str_wrap(gsub("_", " ", names(x)), 8)
          x
     })
     
     # *********************************************************************** organizations
     
     orgx <- reactive({
          
          if (input$oneInd == TRUE) {
               
               yearlyEmpOrg %>% 
                    filter(Industry2 == input$selectIndustry) %>%      
                    filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
                    mutate(Range = paste("Jan 1, ", min(CY), " to ",  ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>% 
                    group_by(OrganizationName,Industry2,Range) %>% 
                    summarise(Number_of_Donations = sum(Number_of_Donations, na.rm = TRUE),
                              Donations_Under_100 = sum(Donations_Under_100, na.rm = TRUE),
                              Donations_Over_100 = sum(Donations_Over_100, na.rm = TRUE),
                              Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
                              Total_Donation_Amount = sum(Total_Donation_Amount, na.rm = TRUE),
                              Dollars = monify(Total_Donation_Amount),
                              Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
                              Max_Donation = max(Max_Donation, na.rm = TRUE)) %>% 
                    ungroup() %>% 
                    arrange(desc(Total_Donation_Amount))
               
          } else {
               
               yearlyOrg %>% 
                    # filter(Industry2 == input$selectIndustry) %>%      
                    filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
                    mutate(Range = paste("Jan 1, ", min(CY), " to ",  ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>% 
                    group_by(OrganizationName,Range) %>% 
                    summarise(Number_of_Donations = sum(Number_of_Donations, na.rm = TRUE),
                              Donations_Under_100 = sum(Donations_Under_100, na.rm = TRUE),
                              Donations_Over_100 = sum(Donations_Over_100, na.rm = TRUE),
                              Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
                              Total_Donation_Amount = sum(Total_Donation_Amount, na.rm = TRUE),
                              Dollars = monify(Total_Donation_Amount),
                              Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
                              Max_Donation = max(Max_Donation, na.rm = TRUE)) %>% 
                    ungroup() %>% 
                    arrange(desc(Total_Donation_Amount))
          }
     })
     
     
     # Organization Plot
     output$org_plot <- renderPlot({
          
          df_org <- orgx() %>% head(input$sliderOrg)
          #var <- unique(df_org$Industry2)
          if ("Industry2" %in% names(df_org)) {
               orgTitle <- paste("Orgs Receiving Donations from ", unique(df_org$Industry2), " Industry", sep = "")
          } else {
               orgTitle <- "Top Earning Organizations"
          }
          
          ggplot(df_org, aes(reorder(OrganizationName, Total_Donation_Amount), Total_Donation_Amount)) +
               geom_bar(stat = "identity", color = "gray50", width = 0.5, fill = "turquoise", alpha = 0.8) +
               geom_text(size = textLabelFun(df_org), fontface = "bold", aes(label = monify(Total_Donation_Amount)), hjust = -0.25) +
               scale_y_continuous(label = dollar, limits = c(0, round(max(df_org$Total_Donation_Amount) * 1.1, 0))) +
               labs(x = "", y = "Total", title = orgTitle,#"Top Earning Organizations",
                    subtitle = unique(df_org$Range)) +
               coord_flip() +
               theme(axis.text.x = element_text(size = 12, face = "bold"),
                     plot.subtitle = element_text(size = 14),
                     plot.title = element_text(size = 17),
                     axis.text.y = element_text(size = axisLabelFun(df_org), face = "bold"))
          
     })
     
     # Organization Table
     output$org_tbl <- DT::renderDataTable({
          x <- orgx() %>% select(-Range)
          names(x) <- str_wrap(gsub("_", " ", names(x)), 6)
          x
     })
     
     
     # *********************************************************************** Employers
     
     empx <- reactive({
          
          yearlyEmp %>% 
               filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
               mutate(Range = paste("Jan 1, ", min(CY), " to ",  ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>% 
               group_by(Employer,Industry,Industry2,Range) %>% 
               summarise(Number_of_Donations = sum(Number_of_Donations, na.rm = TRUE),
                         Donations_Under_100 = sum(Donations_Under_100, na.rm = TRUE),
                         Donations_Over_100 = sum(Donations_Over_100, na.rm = TRUE),
                         Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
                         Total_Donation_Amount = sum(Total_Donation_Amount, na.rm = TRUE),
                         Dollars = monify(Total_Donation_Amount),
                         Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
                         Max_Donation = max(Max_Donation, na.rm = TRUE)) %>% 
               ungroup() %>% 
               arrange(desc(Total_Donation_Amount))
          
     })
     
     
     # Employers Plot
     output$emp_plot <- renderPlot({
          
          df_emp <- empx() %>% head(input$sliderEmp)
          #xlabels <- unique()
          
          ggplot(df_emp, aes(reorder(Employer, Total_Donation_Amount), Total_Donation_Amount)) +
               geom_bar(stat = "identity", color = "gray50", width = 0.5, fill = "blueviolet", alpha = 0.8) +
               geom_text(size = textLabelFun(df_emp), fontface = "bold", aes(label = monify(Total_Donation_Amount)), hjust = -0.25) +
               scale_y_continuous(label = dollar, limits = c(0, round(max(df_emp$Total_Donation_Amount) * 1.1, 0))) +
               labs(x = "", y = "Total", title = "Top Employers Making Donations",
                    subtitle = unique(df_emp$Range)) +
               coord_flip() +
               theme(axis.text.x = element_text(size = 14, face = "bold"),
                     plot.title = element_text(size = 20),
                     plot.subtitle = element_text(size = 14),
                     axis.text.y = element_text(size = axisLabelFun(df_emp), face = "bold"))
          
     })
     
     # Employers Table
     output$emp_tbl <- DT::renderDataTable({
          x <- empx() %>% select(-Range)
          names(x) <- str_wrap(gsub("_", " ", names(x)), 6)
          x
     })
     
     
     # *********************************************************************** PACs
     
     pacx <- reactive({
          
          yearlyPAC %>% 
               filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
               mutate(Range = paste("Jan 1, ", min(CY), " to ",  ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>% 
               group_by(OrganizationName,Range) %>% 
               summarise(Number_of_Donations = sum(Number_of_Donations, na.rm = TRUE),
                         Donations_Under_100 = sum(Donations_Under_100, na.rm = TRUE),
                         Donations_Over_100 = sum(Donations_Over_100, na.rm = TRUE),
                         Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
                         Total_Donation_Amount = sum(Total_Donation_Amount, na.rm = TRUE),
                         Dollars = monify(Total_Donation_Amount),
                         Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
                         Max_Donation = max(Max_Donation, na.rm = TRUE)) %>% 
               ungroup() %>% 
               arrange(desc(Total_Donation_Amount))
          
     })
     
     # PAC Plot
     output$pac_plot <- renderPlot({
          
          df_pac <- pacx() %>% head(input$sliderPac)
          
          ggplot(df_pac, aes(reorder(str_trim(str_wrap(OrganizationName,50), 80), Total_Donation_Amount), Total_Donation_Amount)) +
               geom_bar(stat = "identity", color = "gray50", width = 0.5, fill = "green2", alpha = 0.8) +
               #geom_label(size = textLabelFun(df_pac), fontface = "bold", aes(label = monify(Total_Donation_Amount)), hjust = -0.25) +
               #scale_y_continuous(label = dollar, limits = c(0, round(max(df_pac$Total_Donation_Amount) * 1.1, 0))) +
               # labs(x = "", y = "Total", title = "Top Earning Political Action Committee's",
               #      subtitle = unique(df_pac$Range)) +
               coord_flip() +
               # theme(axis.text.x = element_text(size = 12, face = "bold"),
               #       plot.subtitle = element_text(size = 14),
               #       plot.title = element_text(size = 17),
               #       axis.text.y = element_text(size = axisLabelFun(df_pac), face = "bold")) + 
               NULL
          
     })
     
     # PAC Table
     output$pac_tbl <- DT::renderDataTable({
          x <- pacx() %>% select(-Range)
          names(x) <- str_wrap(gsub("_", " ", names(x)), 6)
          x
     })
     
     
     
     # *********************************************************************** Industries
     
     # 
     # indx <- reactive({
     #      
     #      ind_df1 <- yearlyInd %>% 
     #           
     #           filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
     #           mutate(Range = paste("Jan 1, ", min(CY), " to Dec 31, ", max(CY), sep = "")) %>% 
     #           group_by(Industry,Range) %>% 
     #           summarise(Number_of_Donations = sum(Number_of_Donations, na.rm = TRUE),
     #                     Donations_Under_100 = sum(Donations_Under_100, na.rm = TRUE),
     #                     Donations_Over_100 = sum(Donations_Over_100, na.rm = TRUE),
     #                     Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
     #                     Total_Donation_Amount = sum(Total_Donation_Amount, na.rm = TRUE),
     #                     Dollars = monify(Total_Donation_Amount),
     #                     Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
     #                     Max_Donation = max(Max_Donation, na.rm = TRUE)) %>% 
     #           ungroup() %>% 
     #           arrange(desc(Total_Donation_Amount)) %>% mutate(SubIndustry = "Sub Industry")
     #      
     #      
     #      ind_df2 <- yearlyInd %>% 
     #           filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
     #           mutate(Range = paste("Jan 1, ", min(CY), " to Dec 31, ", max(CY), sep = "")) %>% 
     #           group_by(Industry2,Range) %>% 
     #           summarise(Number_of_Donations = sum(Number_of_Donations, na.rm = TRUE),
     #                     Donations_Under_100 = sum(Donations_Under_100, na.rm = TRUE),
     #                     Donations_Over_100 = sum(Donations_Over_100, na.rm = TRUE),
     #                     Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
     #                     Total_Donation_Amount = sum(Total_Donation_Amount, na.rm = TRUE),
     #                     Dollars = monify(Total_Donation_Amount),
     #                     Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
     #                     Max_Donation = max(Max_Donation, na.rm = TRUE)) %>% 
     #           ungroup() %>% 
     #           arrange(desc(Total_Donation_Amount)) %>% 
     #           rename(Industry = Industry2) %>% mutate(SubIndustry = "Industry")
     #      
     #      switch(input$subInd,
     #             # "Loans Included" = filter(timeSeries, Loans == "Loans Included"),
     #             # "Loans Excluded" = filter(timeSeries, Loans == "Loans Excluded"),
     #             # "Loans Only" = filter(timeSeries, Loans == "Loans Only"))
     #             "Sub Industry" = ind_df1,
     #             "Industry" = ind_df)
     # })
     
     
     indx <- reactive({
          
          if (input$subInd == "Sub Industry") {
               
               yearlyInd %>%
                    filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
                    mutate(Range = paste("Jan 1, ", min(CY), " to ",  ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>%
                    group_by(Industry,Range) %>%
                    summarise(Number_of_Donations = sum(Number_of_Donations, na.rm = TRUE),
                              Donations_Under_100 = sum(Donations_Under_100, na.rm = TRUE),
                              Donations_Over_100 = sum(Donations_Over_100, na.rm = TRUE),
                              Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
                              Total_Donation_Amount = sum(Total_Donation_Amount, na.rm = TRUE),
                              Dollars = monify(Total_Donation_Amount),
                              Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
                              Max_Donation = max(Max_Donation, na.rm = TRUE)) %>%
                    ungroup() %>%
                    arrange(desc(Total_Donation_Amount)) %>% mutate(SubIndustry = "Sub Industry")
               
          } else {
               
               yearlyInd %>%
                    # filter(Industry2 == input$selectIndustry) %>%
                    filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
                    mutate(Range = paste("Jan 1, ", min(CY), " to ",  ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>%
                    group_by(Industry2,Range) %>%
                    summarise(Number_of_Donations = sum(Number_of_Donations, na.rm = TRUE),
                              Donations_Under_100 = sum(Donations_Under_100, na.rm = TRUE),
                              Donations_Over_100 = sum(Donations_Over_100, na.rm = TRUE),
                              Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
                              Total_Donation_Amount = sum(Total_Donation_Amount, na.rm = TRUE),
                              Dollars = monify(Total_Donation_Amount),
                              Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
                              Max_Donation = max(Max_Donation, na.rm = TRUE)) %>%
                    ungroup() %>%
                    arrange(desc(Total_Donation_Amount)) %>%
                    rename(Industry = Industry2) %>% mutate(SubIndustry = "Industry")
               
          }
     })
     
     
     # Industry Plot
     output$ind_plot <- renderPlot({
          
          df_ind <- indx() %>% head(input$sliderInd)
          
          if (df_ind$SubIndustry == "Industry") {
               indTitle <- "Top Donations by Industry"
          } else {
               indTitle <- "Top Donations by Sub-Industry"
          }
          
          ggplot(df_ind, aes(reorder(Industry, Total_Donation_Amount), Total_Donation_Amount)) +
               geom_bar(stat = "identity", color = "gray50", width = 0.5, fill = "steelblue", alpha = 0.8) +
               geom_text(size = textLabelFun(df_ind), fontface = "bold", 
                         aes(label = monify(Total_Donation_Amount)), hjust = -0.25) +
               scale_y_continuous(label = dollar, limits = c(0, round(max(df_ind$Total_Donation_Amount) * 1.1, 0))) +
               labs(x = "", y = "Total", title = indTitle,#"Top Earning Organizations",
                    subtitle = unique(df_ind$Range)) +
               coord_flip() + theme_bw() +
               theme(axis.text.x = element_text(size = 12, face = "bold"),
                     plot.subtitle = element_text(size = 14),
                     plot.title = element_text(size = 17),
                     axis.text.y = element_text(size = axisLabelFun(df_ind), face = "bold"))
          
     })
     
     # Industry Table
     output$ind_tbl <- DT::renderDataTable({
          x <- indx() %>% select(-Range)
          names(x) <- str_wrap(gsub("_", " ", names(x)), 6)
          x
     })
     
     # *********************************************************************** Viz Employers
     
     observe({
          updateSelectInput(session,
                            "viz_emp_inds",
                            choices = viz_emp %>% filter(View == input$viz_emp_ind) %>%
                                 select(Industry) %>% distinct() %>% arrange(Industry) %>% 
                                 .[[1]]
          )
     })
     
     loanView <- reactive({
          if(input$viz_emp_ind == "Industry") {
               if(input$viz_loans == "Not Loan") {
                    viz_emp %>% filter(Loan == "Not Loan" & View == "Industry")
               } else {
                    viz_emp %>% filter(View == "Industry")
               }
          } else {
               if(input$viz_loans == "Not Loan") {
                    viz_emp %>% filter(Loan == "Not Loan" & View == "SubIndustry")
               } else {
                    viz_emp %>% filter(View == "SubIndustry")
               }
          }
     })
     
     viz_empx <- reactive({
          
          loanView() %>% 
               filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
               mutate(Range = paste("Jan 1, ", min(CY), " to ", ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>%
               group_by(from,to,Industry,Loans,Range) %>% 
               summarise(weight = sum(weight, na.rm = TRUE)) %>% 
               ungroup() %>% 
               arrange(desc(weight))
     })
     
     
     output$viz_emp_plot <- renderPlot({
          set.seed(91)
          if (input$viz_emp_inds == "") {
               return()
          }
          
          if(input$viz_emp_box == TRUE) {
               
               ve <- viz_empx() %>% 
                    filter(Industry == input$viz_emp_inds) %>% 
                    head(input$slider_viz_emp) %>%
                    transform(from = str_wrap(from, 12),
                              to = str_wrap(str_to_title(to), 12)) %>%
                    group_by(to) %>%
                    mutate(edgeCol = sample(colors()[!colors() %in% grep("^grey[1-2]|^gray[1-2]|^grey[4-6]|^gray[4-6]|^grey[8-9]|7^gray[8-9]", colors(), value = TRUE)], 1)) %>%
                    ungroup()
          } else {
               
               ve <- viz_empx() %>% 
                    head(input$slider_viz_emp) %>%
                    transform(from = str_wrap(from, 12),
                              to = str_wrap(str_to_title(to), 12)) %>%
                    group_by(to) %>%
                    mutate(edgeCol = sample(colors()[!colors() %in% grep("^grey[1-2]|^gray[1-2]|^grey[4-6]|^gray[4-6]|^grey[8-9]|7^gray[8-9]", colors(), value = TRUE)], 1)) %>%
                    ungroup()
          }
          
          eCol <- ve$edgeCol; dfRange <- unique(ve$Range)
          
          d <- data.frame(label = c(ve$from, ve$to), stringsAsFactors = FALSE) %>% distinct() %>%
               mutate(col = ifelse(label %in% ve$from, "greenyellow","turquoise"),
                      shp = ifelse(label %in% ve$from, "circle","diamond"))
          
          ve <- ve %>% select(from,to,weight)# select(-c(Range,edgeCol))
          
          xLabs <- ifelse(ve$weight >= quantile(ve$weight, probs = 0.1), monify(ve$weight), "")
          
          elabPos <- ifelse(input$layout_viz_emp == "spring", 0.5, 0.8)
          
          suppressWarnings(
               qgraph(ve,
                      layout = input$layout_viz_emp,
                      vsize = input$vertexSize_viz_emp, # Size of the nodes
                      label.cex = input$nodeSlide_viz_emp, # Size of Node Labels
                      mar = c(1,1,1,1),
                      title = paste("Political Connections\n", dfRange, sep = ""),
                      #lcolor = "ghostwhite", bg = "gray0", edge.label.bg = "lightyellow1",
                      lcolor = "black", bg = "white", edge.label.bg = "gray90",
                      #fade = TRUE,
                      fade = input$viz_emp_fade,
                      vTrans = 100, asize = 3,
                      color = d$col, shape = d$shp,
                      color.main = "whitesmoke",
                      label.scale.equal = TRUE,
                      edge.label.color = "navyblue", # Color of the numeric labels
                      edge.color = eCol,#x2$edgeCol,#"gray50", # Line color
                      edge.width = 0.75,
                      edge.label.cex = input$edgeLabSlide_viz_emp,
                      edge.label.position = elabPos,#input$labelPos_viz_emp,
                      edge.labels= xLabs))
     })
     
     
     
     # *********************************************************************** Time Series
     
     var <- reactive({
          switch(input$timeSeriesDF,
                 "Loans Included" = timeSeries[timeSeries$Loans == "Loans Included",],
                 "Loans Excluded" = timeSeries[timeSeries$Loans == "Loans Excluded",],
                 "Loans Only" = timeSeries[timeSeries$Loans == "Loans Only",])
     })
     
     timex <- reactive({
          var() %>% 
               filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
               mutate(Range = paste("Jan 1, ", min(CY), " to ",  ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>% 
               group_by(Month_Yr,Range) %>% 
               summarise(Total_Amount = sum(Amount, na.rm = TRUE)) %>%
               ungroup() %>% 
               mutate(Mo = ymd(gsub("$", "-01", Month_Yr))) %>% 
               mutate(Running = cumsum(Total_Amount))
          
     })
     
     output$time_plot <- renderPlot({
          tx <- timex()
          
          textVals <- tx %>% arrange(desc(Total_Amount)) %>% head(5)
          
          ggplot(tx, aes(Mo, Total_Amount)) +
               geom_point(size = 1, alpha = 0.5) +
               geom_line(stat = "identity", aes(group = 1)) +
               geom_label_repel(data = textVals, size = 4.5, min.segment.length = 0.01, aes(label = monify(Total_Amount))) +
               #geom_vline(xintercept = emo$Loc, lty = 3, lwd = 0.8, color = "violetred") +
               scale_x_date(breaks=pretty_breaks(), date_minor_breaks="1 month") + 
               scale_y_continuous(label = dollar) +
               labs(x = "Donation Month", y = "Total Donations",
                    title = "Monthly Campaign Finance Donations",
                    subtitle = unique(tx$Range)) +
               theme(axis.text.x = element_text(size = 14),#axisLabelFun(tx), angle = 45, hjust = 1, vjust = 1),
                     plot.subtitle = element_text(size = 14),
                     plot.title = element_text(size = 17),
                     axis.text.y = element_text(size = 14))
     })
     
     output$time_plot_cum <- renderPlot({
          tx <- timex()
          
          textVal1 <- tx %>% arrange(Month_Yr) %>% head(1); textVal2 <- tx %>% arrange(Month_Yr) %>% tail(1)
          textVal <- bind_rows(textVal1, textVal2)
          
          ggplot(tx, aes(Mo, Running)) +
               geom_point(size = 1, alpha = 0.5) +
               geom_line(stat = "identity", aes(group = 1)) +
               geom_label_repel(data = textVal, size = 5, min.segment.length = 0.01, aes(label = monify(Running))) +
               #size = textLabelFun(textVal))
               #geom_vline(xintercept = emo$Loc, lty = 3, lwd = 0.8, color = "violetred") +
               scale_x_date(breaks=pretty_breaks(), date_minor_breaks="1 month") + 
               scale_y_continuous(label = dollar) +
               labs(x = "Donation Month", y = "Total Donations",
                    title = "Running       Monthly Campaign Finance Donations",
                    subtitle = unique(tx$Range)) +
               theme(axis.text.x = element_text(size = 14),#axisLabelFun(tx), angle = 45, hjust = 1, vjust = 1),
                     plot.subtitle = element_text(size = 14),
                     plot.title = element_text(size = 17),
                     axis.text.y = element_text(size = 14))
     })
     
     # *********************************************************************** One Percent
     top_nx <- reactive({
          
          if(input$top_n_loan == TRUE) {
               
               theTop <- allDonors %>% arrange(desc(Total_Donated)) %>% head(input$top_n)
               
               allDonors %>% 
                    mutate(TopN = ifelse(FullName %in% theTop$FullName, "Top", "Bottom")) %>% 
                    group_by(TopN) %>% 
                    summarise(Donors = n_distinct(FullName),
                              Likely_Donors = round(Donors / input$avg_names, 0),
                              Contributions = sum(Donations),
                              Total = sum(Total_Donated),
                              Avg_Contribution = round(Total / Contributions, 2)) %>% 
                    ungroup() %>% 
                    transform(Likely_Donors = ifelse(TopN == "Top", Donors, Likely_Donors)) %>% 
                    mutate(Donations_per_Donor = round(Contributions / Likely_Donors, 1),
                           Tot = sum(Total), Don = sum(Contributions), Cont = sum(Likely_Donors)) %>% 
                    mutate(Pct_of_Donors = round(Likely_Donors / Cont, 5),
                           Pct_of_Donations = round(Contributions / Don, 5),
                           Pct_of_Total = round(Total / Tot, 5)) %>% 
                    select(-c(Tot,Don,Cont))
          } else {
               theTop <- allDonors_noloans %>% arrange(desc(Total_Donated)) %>% head(input$top_n)
               
               allDonors_noloans %>% 
                    mutate(TopN = ifelse(FullName %in% theTop$FullName, "Top", "Bottom")) %>% 
                    group_by(TopN) %>% 
                    summarise(Donors = n_distinct(FullName),
                              Likely_Donors = round(Donors / input$avg_names, 0),
                              Contributions = sum(Donations),
                              Total = sum(Total_Donated),
                              Avg_Contribution = round(Total / Contributions, 2)) %>% 
                    ungroup() %>% 
                    transform(Likely_Donors = ifelse(TopN == "Top", Donors, Likely_Donors)) %>% 
                    mutate(Donations_per_Donor = round(Contributions / Likely_Donors, 1),
                           Tot = sum(Total), Don = sum(Contributions), Cont = sum(Likely_Donors)) %>% 
                    mutate(Pct_of_Donors = round(Likely_Donors / Cont, 5),
                           Pct_of_Donations = round(Contributions / Don, 5),
                           Pct_of_Total = round(Total / Tot, 5)) %>% 
                    select(-c(Tot,Don,Cont))
               
               
          }
     })
     
     output$top_n_plot <- renderPlot({
          v <- top_nx()
          
          v %>% #select(-c(Tot,Don,Cont)) %>% 
               gather(Measure, Var, -TopN) %>% 
               transform(Measure = gsub("_", " ", Measure)) %>% 
               filter(Measure %in% c("Pct of Donors","Pct of Donations","Pct of Total")) %>% 
               transform(Measure = factor(Measure, levels = c("Pct of Donors","Pct of Donations","Pct of Total"))) %>% 
               # transform(Measure = factor(Measure, levels = c("Total","Contributions","Avg Contribution","Donors","Likely Donors",
               #                                                "Donations per Donor","Pct of Donors","Pct of Donations","Pct of Total"))) %>% 
               filter(TopN == "Top") %>% 
               ggplot(aes(TopN, Var)) +
               geom_bar(stat = "identity", width = 0.5, color = "gray50", alpha = 0.9, size = 1, aes(fill = Measure)) +
               geom_label(alpha = 0.5, size = 5, fontface = "bold", aes(label = percent(Var))) +
               scale_y_continuous(label = percent, limits = c(0,0.6), breaks = seq(0,1,0.1)) +
               #scale_fill_manual(values = c("navyblue","dodgerblue")) +
               #scale_y_continuous(#limits = c(0,1), breaks = seq(0,1,0.1), label = percent),
               labs(x = "", y = "Value",
                    title = "Balance of Influence") +
               theme_bw() +
               theme(strip.text.x = element_text(size = 14),
                     legend.position = "none",#c(0.8,0.15), 
                     plot.title = element_text(size = 18, hjust = 0.5),
                     axis.text.x = element_blank(),#element_text(size = 12),
                     axis.text.y = element_text(size = 15),
                     legend.title = element_blank()) +
               facet_wrap(~Measure, #scales = "free",
                          ncol = 3)
     })
     
     # Table
     output$top_n_tbl <- renderTable({
          x <- top_nx() %>% 
               transform(Donors = comma(Donors),
                         Likely_Donors = comma(Likely_Donors),
                         Contributions = comma(Contributions),
                         Total = monify(Total),
                         Avg_Contribution = monify(Avg_Contribution),
                         Pct_of_Total = percent(Pct_of_Total),
                         Pct_of_Donations = percent(Pct_of_Donations),
                         Pct_of_Donors = percent(Pct_of_Donors)) %>% 
               rename(Tier = TopN) 
          names(x) <- str_wrap(gsub("_", " ", names(x)), 6)
          x
     })
     
     n_tbl <- reactive({
          if(input$top_n_loan == TRUE) {
               x <- allDonors
               names(x) <- str_wrap(gsub("_", " ", names(x)), 6)
               x
          } else {
               x <- allDonors_noloans
               names(x) <- str_wrap(gsub("_", " ", names(x)), 6)
               x
          }
     })
     
     # Table
     output$top_n_tbl2 <- DT::renderDataTable({
          x <- n_tbl()
          names(x) <- str_wrap(gsub("_", " ", names(x)), 6)
          x
          
     })
     
     
     
     
     # *********************************************************************** Dynasties
     
     dynx <- reactive({
          if(input$radio_dyn == "First") {
               dynasties %>% filter(Title == "First")
          } else {
               dynasties %>% filter(Title == "Last")
          }
     })
     
     dyn_fill <- reactive({
          if(input$radio_dyn ==  "First") {
               "mediumvioletred"
          } else {
               "mediumblue"
          }
     })
     
     dyn_title <- reactive({
          if(input$radio_dyn ==  "First") {
               "Contributions by First Name"
          } else {
               "Contributions by Last Name"
          }
     })
     
     # Llamos Plot
     output$dynasty_plot <- renderPlot({
          df_dyn <- dynx() %>% head(input$sliderDynasty)
          ggplot(df_dyn, aes(reorder(Llamo, Total), Total)) +
               geom_bar(stat = "identity", color = "gray50", width = 0.5, fill = dyn_fill(),#"mediumaquamarine", 
                        alpha = 0.8) +
               geom_label(size = textLabelFun(df_dyn), fontface = "bold", aes(label = monify(Total)), hjust = -0.25) +
               scale_y_continuous(label = dollar, limits = c(0, round(max(df_dyn$Total) * 1.1, 0))) +
               labs(x = "", y = "Total", title = dyn_title(),#"Contributions by Name",
                    subtitle = paste("2002 thru", " 2018", sep = "")) +
               coord_flip() +
               theme(axis.text.x = element_text(size = 12, face = "bold"),
                     plot.subtitle = element_text(size = 14),
                     plot.title = element_text(size = 17),
                     axis.text.y = element_text(size = axisLabelFun(df_dyn), face = "bold"))
     })
     
     # PAC Table
     output$dynasty_tbl <- DT::renderDataTable({
          x <- dynx() %>% 
               select(-c(Mean_Donation,Title))
          names(x) <- str_wrap(gsub("_", " ", names(x)), 6)
          x
     })
     
     
     # *********************************************************************** Geography
     
     locx <- reactive({
          
          if (input$oneState == TRUE) {
               
               yearlyLoc %>% 
                    filter(donor_st == input$donorState) %>% 
                    filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
                    mutate(Range = paste("Jan 1, ", min(CY), " to ",  ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>% 
                    group_by(City, donor_city, donor_st, donor_region,Range) %>%
                    summarise(Donors = sum(Donors, na.rm = TRUE),
                              Number_of_Donations = sum(Number_of_Donations, na.rm = TRUE),
                              Donations_Under_100 = sum(Donations_Under_100, na.rm = TRUE),
                              Donations_Over_100 = sum(Donations_Over_100, na.rm = TRUE),
                              Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
                              Num_PAC_Donations = sum(Num_PAC_Donations, na.rm = TRUE),
                              Num_Org_Donations = sum(Num_Org_Donations, na.rm = TRUE),
                              Total_Donation_Amount = sum(Total_Donation_Amount, na.rm = TRUE),
                              Dollars = monify(Total_Donation_Amount),
                              PAC_Donation_Amount = sum(PAC_Donation_Amount, na.rm = TRUE),
                              Org_Donation_Amount = sum(Org_Donation_Amount, na.rm = TRUE),
                              Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
                              Avg_PAC_Donation_Amt = round(PAC_Donation_Amount / Num_PAC_Donations, 2),
                              Avg_Org_Donation_Amt = round(Org_Donation_Amount / Num_Org_Donations, 2)) %>%
                    ungroup() %>% 
                    arrange(desc(Total_Donation_Amount)) 
          } else {
               
               yearlyLoc %>% 
                    #filter(donor_st %in% input$donorState) %>% 
                    filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
                    mutate(Range = paste("Jan 1, ", min(CY), " to ",  ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>% 
                    group_by(City,donor_city, donor_st, donor_region,Range) %>%
                    summarise(Donors = sum(Donors, na.rm = TRUE),
                              Number_of_Donations = sum(Number_of_Donations, na.rm = TRUE),
                              Donations_Under_100 = sum(Donations_Under_100, na.rm = TRUE),
                              Donations_Over_100 = sum(Donations_Over_100, na.rm = TRUE),
                              Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
                              Num_PAC_Donations = sum(Num_PAC_Donations, na.rm = TRUE),
                              Num_Org_Donations = sum(Num_Org_Donations, na.rm = TRUE),
                              Total_Donation_Amount = sum(Total_Donation_Amount, na.rm = TRUE),
                              Dollars = monify(Total_Donation_Amount),
                              PAC_Donation_Amount = sum(PAC_Donation_Amount, na.rm = TRUE),
                              Org_Donation_Amount = sum(Org_Donation_Amount, na.rm = TRUE),
                              Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
                              Avg_PAC_Donation_Amt = round(PAC_Donation_Amount / Num_PAC_Donations, 2),
                              Avg_Org_Donation_Amt = round(Org_Donation_Amount / Num_Org_Donations, 2)) %>%
                    ungroup() %>% 
                    arrange(desc(Total_Donation_Amount)) 
          }
     })
     
     
     output$city_plot <- renderPlot({
          
          cityx <- locx() %>% head(input$sliderCity)
          
          ggplot(cityx, aes(reorder(City, Total_Donation_Amount), Total_Donation_Amount)) +
               geom_bar(stat = "identity", color = "gray50", width = 0.5, aes(fill = donor_st), alpha = 0.8) +
               geom_text(size = textLabelFun(cityx), fontface = "bold", aes(label = monify(Total_Donation_Amount)), hjust = -0.25) +
               scale_y_continuous(label = dollar, limits = c(0, round(max(cityx$Total_Donation_Amount) * 1.1, 0))) +
               labs(x = "", y = "Total", title = "Campaign Donations by City",
                    subtitle = unique(cityx$Range)) +
               coord_flip() + theme_bw() +
               theme(axis.text.x = element_text(size = 12, face = "bold"), legend.title = element_blank(),
                     plot.subtitle = element_text(size = 14),
                     plot.title = element_text(size = 17),
                     legend.position = c(0.8,0.25), legend.background = element_rect(color = "gray50"),
                     axis.text.y = element_text(size = axisLabelFun(cityx), face = "bold"))
          
     })
     
     # City Table
     output$city_tbl <- DT::renderDataTable({
          x <- locx() %>% select(-Range)
          names(x) <- str_wrap(gsub("_", " ", names(x)), 6)
          x
     })
     
     statex <- reactive({
          
          yearlyLoc %>% 
               filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
               mutate(Range = paste("Jan 1, ", min(CY), " to ",  ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>% 
               group_by(donor_state_name, donor_st, donor_region,Range) %>%
               summarise(Donors = sum(Donors, na.rm = TRUE),
                         Number_of_Donations = sum(Number_of_Donations, na.rm = TRUE),
                         Donations_Under_100 = sum(Donations_Under_100, na.rm = TRUE),
                         Donations_Over_100 = sum(Donations_Over_100, na.rm = TRUE),
                         Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
                         Num_PAC_Donations = sum(Num_PAC_Donations, na.rm = TRUE),
                         Num_Org_Donations = sum(Num_Org_Donations, na.rm = TRUE),
                         Total_Donation_Amount = sum(Total_Donation_Amount, na.rm = TRUE),
                         Dollars = monify(Total_Donation_Amount),
                         PAC_Donation_Amount = sum(PAC_Donation_Amount, na.rm = TRUE),
                         Org_Donation_Amount = sum(Org_Donation_Amount, na.rm = TRUE),
                         Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
                         Avg_PAC_Donation_Amt = round(PAC_Donation_Amount / Num_PAC_Donations, 2),
                         Avg_Org_Donation_Amt = round(Org_Donation_Amount / Num_Org_Donations, 2)) %>%
               ungroup() %>% 
               arrange(desc(Total_Donation_Amount)) 
     })
     
     output$state_plot <- renderPlot({
          
          stx <- statex() %>% head(input$sliderState)
          
          ggplot(filter(stx, !is.na(donor_state_name)), aes(reorder(donor_state_name, Total_Donation_Amount), Total_Donation_Amount)) +
               geom_bar(stat = "identity", color = "gray50", width = 0.5, aes(fill = donor_region), alpha = 0.8) + #coord_polar() +
               geom_text(size = textLabelFun(stx), fontface = "bold", aes(label = monify(Total_Donation_Amount)), hjust = -0.25) +
               scale_y_continuous(label = dollar, limits = c(0, round(max(stx$Total_Donation_Amount) * 1.1, 0))) +
               labs(x = "", y = "Total", title = "Campaign Donations by State",
                    subtitle = unique(stx$Range)) +
               coord_flip() + 
               theme_bw() +
               theme(axis.text.x = element_text(size = 12, face = "bold"), legend.title = element_blank(),
                     plot.subtitle = element_text(size = 14),
                     plot.title = element_text(size = 17),
                     legend.position = "none", legend.background = element_rect(color = "gray50"),
                     axis.text.y = element_text(size = axisLabelFun(stx), face = "bold"))
          
     })
     
     output$statePolar_plot <- renderPlot({
          
          stx <- statex() %>% filter(donor_st %in% c("NY","MA","CA","CT"))# %>%  head(input$slider1)
          
          ggplot(filter(stx, !is.na(donor_state_name)), aes(reorder(donor_state_name, Total_Donation_Amount), Total_Donation_Amount)) +
               geom_bar(stat = "identity", color = "gray50", width = 0.5, aes(fill = donor_region), alpha = 0.8) + coord_polar() +
               geom_text(size = 4, color = "violetred", fontface = "bold", aes(label = monify(Total_Donation_Amount)), hjust = -0.25) +
               scale_y_continuous(label = dollar, limits = c(0, round(max(stx$Total_Donation_Amount) * 1.1, 0))) +
               labs(x = "", y = "Total", title = "Large Spenders in Rhode Island",
                    subtitle = unique(stx$Range)) +
               #coord_flip() + 
               theme_bw() +
               theme(axis.text.y = element_text(size = 12, face = "bold"), legend.title = element_blank(),
                     plot.subtitle = element_text(size = 14),
                     plot.title = element_text(size = 17),
                     legend.position = "none", legend.background = element_rect(color = "gray50"),
                     axis.text.x = element_text(size = 10, face = "bold"))
          
     })
     
     output$statePolar_plot2 <- renderPlot({
          
          stx <- statex() %>% filter(!donor_st %in% c("RI","MA","CT","CA","NY")) %>%  head(input$sliderState)
          
          ggplot(filter(stx, !is.na(donor_state_name)), aes(reorder(donor_st, Total_Donation_Amount), Total_Donation_Amount)) +
               geom_bar(stat = "identity", color = "gray50", width = 0.5, aes(fill = donor_region), alpha = 0.8) + coord_polar() +
               geom_text(data = head(stx, 2), color = "violetred", size = 4, fontface = "bold", aes(label = monify(Total_Donation_Amount)), hjust = -0.25) +
               scale_y_continuous(label = dollar, limits = c(0, round(max(stx$Total_Donation_Amount) * 1.1, 0))) +
               labs(x = "", y = "Total", title = "Donations from Other States",
                    subtitle = unique(stx$Range)) +
               #coord_flip() + 
               theme_bw() +
               theme(axis.text.y = element_text(size = 12, face = "bold"), legend.title = element_blank(),
                     plot.subtitle = element_text(size = 14),
                     plot.title = element_text(size = 17),
                     legend.position = "none", legend.background = element_rect(color = "gray50"),
                     axis.text.x = element_text(size = axisLabelFun(stx), face = "bold"))
          
     })
     
     # State Table
     output$state_tbl <- DT::renderDataTable({
          x <- statex() %>% select(-c(Range,donor_st)) %>% rename(Donor_State = donor_state_name)
          names(x) <- str_wrap(gsub("_", " ", names(x)), 6)
          x
     })
     
     # **********************************************************************************  Gina vs the field
     ginax <- reactive({
          ginaVs %>%
               filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
               mutate(Range = paste("Jan 1, ", min(CY), " to ",  ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>%
               group_by(Gina,Range,donor_region) %>% 
               summarise(Total_Amount = sum(Total, na.rm = TRUE)) %>% 
               ungroup()
     })
     
     output$gina_plot <- renderPlot({
          
          df_gina <- ginax() %>% transform(donor_region = str_wrap(donor_region, 19)) %>% 
               transform(donor_region = ifelse(donor_region == "", "Undetermined", donor_region))
          
          ggplot(df_gina, aes(Gina, Total_Amount)) + 
               geom_bar(stat = "identity", color = "gray50", width = 0.5, position = "dodge", aes(fill = Gina)) + 
               scale_fill_manual(values = c("dodgerblue","navyblue")) +
               scale_y_continuous(label = dollar) +
               labs(x = "", y = "Donations Received", 
                    title = "Political Donations Received by State of Donor",
                    subtitle = unique(df_gina$Range)) +
               #caption = paste(srce, "downloaded on Feb 9, 2017.  Donation 'ReceiptDate' between Jan 1, 2017 & Dec 31, 2017", sep = "\n")) +
               theme_bw() +
               theme(legend.position = "bottom", legend.background = element_rect(color = "gray50"),
                     legend.title = element_blank(), legend.text = element_text(size = 14),
                     plot.subtitle = element_text(size = 14),
                     plot.title = element_text(size = 17),
                     strip.text.x = element_text(size = 11, margin = margin(0.05,0.05,0.05,0.05, "cm")),
                     axis.text.x = element_blank(),axis.text.y = element_text(size = 12)) +
               facet_wrap(~donor_region, scales = "free") +
               guides(fill = guide_legend(override.aes = list(size=10))) +
               NULL
          
     })
     
     # gina_indx <- reactive({
     # 
     # genaVis %>%
     #      filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
     #      mutate(Range = paste("Jan 1, ", min(CY), " to Dec 31, ", max(CY), sep = "")) %>%
     #      group_by(Industry2) %>% 
     #      mutate(indTot = sum(Amount, na.rm = TRUE)) %>%
     #      ungroup() %>% 
     #      filter(indTot >= 5000) %>% 
     #      mutate(Gina = ifelse(OrganizationName == "Gina M. Raimondo", "Governor\nRaimondo", "Everybody\nElse")) %>% 
     #      group_by(Industry2,Gina,Range) %>% 
     #      summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     #      ungroup() %>% 
     #      arrange(desc(Total)); head(ginaVs)
     # 
     # })
     # # *********************************************************************** Expenditures
     
     
     
     
     
     # By Description
     exp_x <- reactive({
          if (input$oneType == TRUE) {
               expOrgComp %>% 
                    filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
                    mutate(Range = paste("Jan 1, ", min(CY), " to ",  ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>%
                    filter(ExpDesc == input$exp_type) %>% 
                    group_by(ExpDesc,FullName,OrganizationName,#col,#City,State,
                             Range) %>% 
                    summarise(Expenditures = sum(Expenditures), Total = sum(Total),
                              avg = round(Total / Expenditures, 2), max = max(Total, na.rm = TRUE),
                              First = min(First, na.rm = TRUE), Most_Recent = max(Most_Recent, na.rm = TRUE)) %>% 
                    ungroup() %>%
                    group_by(ExpDesc) %>% mutate(ExpType_GrandTot = sum(Total)) %>% 
                    mutate(Pct_Total = round(Total / ExpType_GrandTot, 5)) %>% 
                    mutate(Dollars = monify(Total),Avg = monify(avg), Grand_Total_by_Type = monify(ExpType_GrandTot), Max = monify(max),Pct_of_Total = percent(Pct_Total)) %>% 
                    select(FullName,#City,State,
                           OrganizationName,
                           ExpDesc,Expenditures,Dollars,Avg,Max,Grand_Total_by_Type,Pct_of_Total,First,Most_Recent,everything()) %>% 
                    arrange(desc(Total))# %>% head(input$expCo_slider) 
          } else {
               expDesc %>% 
                    filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
                    mutate(Range = paste("Jan 1, ", min(CY), " to ",  ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>%
                    group_by(ExpDesc,Range) %>% 
                    summarise(Expenditures = sum(Expenditures), Total = sum(Total),
                              avg = round(Total / Expenditures, 2), max = max(Total, na.rm = TRUE),
                              First = min(First, na.rm = TRUE), Most_Recent = max(Most_Recent, na.rm = TRUE)) %>% 
                    ungroup() %>%
                    mutate(GrandTot = sum(Total)) %>% mutate(Pct_Total = round(Total / GrandTot, 5)) %>% 
                    mutate(Dollars = monify(Total),Avg = monify(avg), Grand_Total = monify(GrandTot), Max = monify(max),Pct_of_Total = percent(Pct_Total)) %>% 
                    select(ExpDesc,Range,Expenditures,Dollars,Avg,Max,Grand_Total,Pct_of_Total,First,Most_Recent,everything()) %>% 
                    arrange(desc(Total))          
          }
     })
     
     output$expDesc_plot <- renderPlot({
          
          if (input$oneType == TRUE) {
               if(input$exp_view == "Organization (Candidate)") {
                    
                    e_x <- exp_x() %>% 
                         group_by(OrganizationName, ExpDesc,Range) %>% 
                         summarise(Total = sum(Total)) %>% 
                         ungroup() %>% 
                         arrange(desc(Total)) %>% 
                         head(input$exp_type_slider)
               } else {
                    e_x <- exp_x() %>% 
                         group_by(FullName, ExpDesc,Range) %>% 
                         summarise(Total = sum(Total)) %>% 
                         ungroup() %>% 
                         arrange(desc(Total)) %>% 
                         head(input$exp_type_slider)
               }
          } else {
               e_x <- exp_x() 
          }
          
          eTitle <- reactive({
               if(input$exp_view == "Organization (Candidate)") {
                    paste(unique(e_x$ExpDesc), " Expenditures by Organization (Candidate)", sep = "")
               } else {
                    paste("Recipients of ", unique(e_x$ExpDesc), " Expenditures", sep = "")
               }
          })
          
          if (input$oneType == TRUE) {
               
               if(input$exp_view == "Organization (Candidate)") {
                    
                    ggplot(e_x, aes(reorder(str_wrap(OrganizationName, 50), Total), Total)) +
                         geom_bar(stat = "identity", width = 0.6, color = "gray50", size = 0.1, fill = "yellowgreen") +
                         geom_text(size = textLabelFun(e_x), fontface = "bold", aes(label = monify(Total)), hjust = -0.25) +
                         #scale_y_continuous(label = dollar) +
                         scale_y_continuous(label = dollar, limits = c(0, round(max(e_x$Total) * 1.2, 0))) +
                         labs(x = "Organization", y = "Total",
                              title = eTitle(),
                              subtitle = unique(e_x$Range)) + theme_bw() +
                         theme(axis.text.y = element_text(size = 16),
                               axis.text.x = element_text(size = 16),# axisLabelFun(e_x),
                               legend.position = c(0.8,0.2),
                               legend.background = element_rect(color = "gray50"),
                               plot.subtitle = element_text(size = 17),
                               plot.title = element_text(size = 21)) +
                         coord_flip() + 
                         NULL
               } else {
                    ggplot(e_x, aes(reorder(strtrim(str_wrap(FullName,50), 90), Total), Total)) +
                         geom_bar(stat = "identity", width = 0.6, color = "gray50", size = 0.1, aes(fill = ExpDesc)) +
                         geom_text(size = textLabelFun(e_x), fontface = "bold", aes(label = monify(Total)), hjust = -0.25) +
                         #scale_y_continuous(label = dollar) +
                         scale_y_continuous(label = dollar, limits = c(0, round(max(e_x$Total) * 1.2, 0))) +
                         labs(x = "", y = "Total",
                              title = eTitle(),
                              subtitle = e_x$Range) + theme_bw() +
                         theme(axis.text.y = element_text(size = 16),
                               axis.text.x = element_text(size = 16),# axisLabelFun(e_x),
                               legend.position = c(0.8,0.2),
                               legend.background = element_rect(color = "gray50"),
                               plot.subtitle = element_text(size = 17),
                               plot.title = element_text(size = 21)) +
                         coord_flip() +
                         NULL
               }
               
          } else {
               
               ggplot(e_x, aes(reorder(ExpDesc,Total), Total)) +
                    geom_bar(stat = "identity", width = 0.6, color = "gray50", fill = "navyblue") +
                    scale_y_continuous(label = dollar, limits = c(0, round(max(e_x$Total) * 1.1, 0))) +
                    geom_text(size = 5,#textLabelFun(e_x),#4,
                              fontface = "bold", 
                              aes(label = Dollars), hjust = -0.25) +
                    labs(x = "Expenditure Description", y = "Total",
                         title = "Expenditure Types",
                         subtitle = paste(e_x$Range, sep = "")) +
                    theme(axis.text = element_text(size = 16),
                          plot.subtitle = element_text(size = 17),
                          plot.title = element_text(size = 21)) +
                    coord_flip() +
                    NULL
          }
     })
     
     
     #  Table
     output$expDesc_tbl <- DT::renderDataTable({
          x <- exp_x() #%>% arrange(desc(Total))
          names(x) <- str_wrap(gsub("_", " ", names(x)), 6)
          x
     })
     
     
     
     
     
     
     # 
     # # By Description
     # exp_x <- reactive({
     #      expDesc %>% 
     #           filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
     #           mutate(Range = paste("Jan 1, ", min(CY), " to ",  ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>%
     #           group_by(ExpDesc,Range) %>% 
     #           summarise(Expenditures = sum(Expenditures),
     #                     Total = sum(Total),
     #                     avg = round(Total / Expenditures, 2),
     #                     max = max(Total, na.rm = TRUE),
     #                     First = min(First, na.rm = TRUE),
     #                     Most_Recent = max(Most_Recent, na.rm = TRUE)) %>% 
     #           ungroup() %>%
     #           mutate(GrandTot = sum(Total)) %>% 
     #           mutate(Pct_Total = round(Total / GrandTot, 5)) %>% 
     #           mutate(Dollars = monify(Total),Avg = monify(avg), Grand_Total = monify(GrandTot), Max = monify(max),Pct_of_Total = percent(Pct_Total)) %>% 
     #           select(ExpDesc,Range,Expenditures,Dollars,Avg,Max,Grand_Total,Pct_of_Total,First,Most_Recent,everything()) %>% 
     #           arrange(desc(Total))                
     # })
     # 
     # output$expDesc_plot <- renderPlot({
     #      e_x <- exp_x() 
     #      
     #      ggplot(e_x, aes(reorder(ExpDesc,Total), Total)) +
     #           geom_bar(stat = "identity", width = 0.6, color = "gray50", fill = "navyblue") +
     #           scale_y_continuous(label = dollar, limits = c(0, round(max(e_x$Total) * 1.1, 0))) +
     #           geom_text(size = 5,#textLabelFun(e_x),#4,
     #                     fontface = "bold", 
     #                     aes(label = Dollars), hjust = -0.25) +
     #           labs(x = "Expenditure Description", y = "Total",
     #                title = "Expenditure Types",
     #                subtitle = paste(e_x$Range, sep = "")) +
     #           theme(axis.text = element_text(size = 16),
     #                 plot.subtitle = element_text(size = 17),
     #                 plot.title = element_text(size = 21)) +
     #           coord_flip() +
     #           NULL
     # })
     # 
     # 
     # #  Table
     # output$expDesc_tbl <- DT::renderDataTable({
     #      x <- exp_x() #%>% arrange(desc(Total))
     #      names(x) <- str_wrap(gsub("_", " ", names(x)), 6)
     #      x
     # })
     # 
     # # By Company
     # expCo_x <- reactive({
     #      set.seed(235813)
     #      if (input$oneType == TRUE) {
     #           
     #           if(input$oneComp == TRUE) {
     #                
     #                expOrgComp %>% 
     #                     filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
     #                     mutate(Range = paste("Jan 1, ", min(CY), " to ",  ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>%
     #                     filter(ExpDesc == input$exp_type) %>% 
     #                     filter(FullName == input$exp_comps) %>% 
     #                     group_by(ExpDesc,FullName,OrganizationName,#col,#City,State,
     #                              Range) %>% 
     #                     summarise(Expenditures = sum(Expenditures),
     #                               Total = sum(Total),
     #                               avg = round(Total / Expenditures, 2),
     #                               max = max(Total, na.rm = TRUE),
     #                               First = min(First, na.rm = TRUE),
     #                               Most_Recent = max(Most_Recent, na.rm = TRUE)) %>% 
     #                     ungroup() %>%
     #                     group_by(ExpDesc) %>% 
     #                     mutate(ExpType_GrandTot = sum(Total)) %>% 
     #                     mutate(Pct_Total = round(Total / ExpType_GrandTot, 5)) %>% 
     #                     mutate(Dollars = monify(Total),Avg = monify(avg), Grand_Total_by_Type = monify(ExpType_GrandTot), Max = monify(max),Pct_of_Total = percent(Pct_Total)) %>% 
     #                     select(FullName,#City,State,
     #                            OrganizationName,
     #                            ExpDesc,Expenditures,Dollars,Avg,Max,Grand_Total_by_Type,Pct_of_Total,First,Most_Recent,everything()) %>% 
     #                     arrange(desc(Total))# %>% head(input$expCo_slider) 
     #           } else {
     #                expComp %>% 
     #                     filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
     #                     mutate(Range = paste("Jan 1, ", min(CY), " to ",  ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>%
     #                     filter(ExpDesc == input$exp_type) %>% 
     #                     group_by(ExpDesc,FullName,#col,#City,State,
     #                              Range) %>% 
     #                     summarise(Expenditures = sum(Expenditures),
     #                               Total = sum(Total),
     #                               avg = round(Total / Expenditures, 2),
     #                               max = max(Total, na.rm = TRUE),
     #                               First = min(First, na.rm = TRUE),
     #                               Most_Recent = max(Most_Recent, na.rm = TRUE)) %>% 
     #                     ungroup() %>%
     #                     group_by(ExpDesc) %>% 
     #                     mutate(ExpType_GrandTot = sum(Total)) %>% 
     #                     mutate(Pct_Total = round(Total / ExpType_GrandTot, 5)) %>% 
     #                     mutate(Dollars = monify(Total),Avg = monify(avg), Grand_Total_by_Type = monify(ExpType_GrandTot), Max = monify(max),Pct_of_Total = percent(Pct_Total)) %>% 
     #                     select(FullName,#City,State,
     #                            ExpDesc,Expenditures,Dollars,Avg,Max,Grand_Total_by_Type,Pct_of_Total,First,Most_Recent,everything()) %>% 
     #                     arrange(desc(Total))# %>% head(input$expCo_slider) 
     #                
     #           }
     #      } else {
     #           
     #           if(input$oneComp == TRUE) {
     #                
     #                expOrgComp %>% 
     #                     filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
     #                     mutate(Range = paste("Jan 1, ", min(CY), " to ",  ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>%
     #                     filter(FullName == input$exp_comps) %>% 
     #                     # filter(ExpDesc == input$exp_type) %>% 
     #                     group_by(ExpDesc,FullName,OrganizationName,#col,#City,State,
     #                              Range) %>% 
     #                     summarise(Expenditures = sum(Expenditures),
     #                               Total = sum(Total),
     #                               avg = round(Total / Expenditures, 2),
     #                               max = max(Total, na.rm = TRUE),
     #                               First = min(First, na.rm = TRUE),
     #                               Most_Recent = max(Most_Recent, na.rm = TRUE)) %>% 
     #                     ungroup() %>%
     #                     group_by(ExpDesc) %>% 
     #                     mutate(ExpType_GrandTot = sum(Total)) %>% 
     #                     mutate(Pct_Total = round(Total / ExpType_GrandTot, 5)) %>% 
     #                     mutate(Dollars = monify(Total),Avg = monify(avg), Grand_Total_by_Type = monify(ExpType_GrandTot), Max = monify(max),Pct_of_Total = percent(Pct_Total)) %>% 
     #                     select(FullName,
     #                            OrganizationName,#City,State,
     #                            ExpDesc,Expenditures,Dollars,Avg,Max,Grand_Total_by_Type,Pct_of_Total,First,Most_Recent,everything()) %>% 
     #                     arrange(desc(Total))# %>% head(input$expCo_slider) 
     #           } else {
     #                
     #                expComp %>% 
     #                     filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
     #                     mutate(Range = paste("Jan 1, ", min(CY), " to ",  ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>%
     #                     # filter(ExpDesc == input$exp_type) %>% 
     #                     group_by(ExpDesc,FullName,#col,#City,State,
     #                              Range) %>% 
     #                     summarise(Expenditures = sum(Expenditures),
     #                               Total = sum(Total),
     #                               avg = round(Total / Expenditures, 2),
     #                               max = max(Total, na.rm = TRUE),
     #                               First = min(First, na.rm = TRUE),
     #                               Most_Recent = max(Most_Recent, na.rm = TRUE)) %>% 
     #                     ungroup() %>%
     #                     group_by(ExpDesc) %>% 
     #                     mutate(ExpType_GrandTot = sum(Total)) %>% 
     #                     mutate(Pct_Total = round(Total / ExpType_GrandTot, 5)) %>% 
     #                     mutate(Dollars = monify(Total),Avg = monify(avg), Grand_Total_by_Type = monify(ExpType_GrandTot), Max = monify(max),Pct_of_Total = percent(Pct_Total)) %>% 
     #                     select(FullName,#City,State,
     #                            ExpDesc,Expenditures,Dollars,Avg,Max,Grand_Total_by_Type,Pct_of_Total,First,Most_Recent,everything()) %>% 
     #                     arrange(desc(Total))# %>% head(input$expCo_slider) 
     #                
     #                
     #           }
     #      }
     # })
     # 
     # 
     # output$expCo_plot <- renderPlot({
     #      
     #      #set.seed(83)
     #      #expCols <- sample(colors()[!colors() %in% grep("^grey1|^gray1|^grey4|^gray4|^grey|7^gray7", colors(), value = TRUE)], length(unique(expComp$ExpDesc)))
     #      
     #      
     #      exp_comp <- expCo_x() %>% head(input$expCo_slider) %>% droplevels() #%>% head(input$expCo_slider) 
     #      colVals_comp <- sample(colors()[!colors() %in% grep("^grey[1-2]|^gray[1-2|^grey[4-5]|^gray[4-5]|^grey[7-9]|^gray[7-9]", colors(), value = TRUE)], 17) 
     #      
     #      if(input$oneComp == TRUE) {
     #           
     #           ggplot(exp_comp, aes(reorder(str_wrap(OrganizationName,50), Total), Total)) +
     #                geom_bar(stat = "identity", #width = 0.6, 
     #                         color = "gray50",position = "dodge", #fill = "mintcream"
     #                         aes(fill = ExpDesc)) +
     #                scale_y_continuous(labels = dollar, limits = c(0, round(max(exp_comp$Total) * 1.4, 0))) +
     #                #scale_y_continuous(label = dollar) +
     #                scale_fill_manual(values = colVals_comp, limits = levels(exp_comp$ExpDesc)) +
     #                #scale_fill_discrete(limits = levels(expComp$ExpDesc)) +
     #                # geom_text(size = textLabelFun(exp_comp), position = position_dodge(width = 1), fontface = "bold", aes(label = paste(str_wrap(ExpDesc,50), "\n", Dollars, sep = ""), fill = ExpDesc)#, hjust = -0.25
     #                #           ) +
     #                geom_text(size = textLabelFun(exp_comp),#4,
     #                          fontface = "bold", position = position_dodge(width = 1), aes(label = Dollars, fill = ExpDesc)#, hjust = -0.25
     #                ) +
     #                labs(x = "Organization", y = "Total",
     #                     title = paste("Donations Spent with ", unique(exp_comp$FullName), sep = ""),
     #                     subtitle = paste(exp_comp$Range, sep = "")) + theme_bw() +
     #                theme(axis.text.x = element_text(size = 14),
     #                      plot.title = element_text(size = 18),
     #                      legend.position = "bottom",#c(0.75,0.25),
     #                      legend.text = element_text(size = 15),
     #                      strip.text.x = element_text(size = 14, margin = margin(0.05,0.05,0.05,0.05, "cm")), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
     #                      legend.title = element_blank(),legend.background = element_rect(color = "gray50"),
     #                      plot.subtitle = element_text(size = 15)) +
     #                #axis.text.y = element_text(size = axisLabelFun(exp_comp), face = "bold")) +
     #                #coord_flip() +
     #                facet_wrap(~OrganizationName, scales = "free_y") +
     #                guides(fill = guide_legend(override.aes = list(size=15))) +
     #                NULL
     #      } else {
     #           
     #           ggplot(exp_comp, aes(reorder(str_wrap(FullName,50), Total), Total)) +
     #                geom_bar(stat = "identity", width = 0.6, color = "gray50", #fill = "mintcream"
     #                         #aes(fill = ExpDesc)) +
     #                         aes(fill=ExpDesc)) +
     #                scale_y_continuous(labels = dollar, limits = c(0, round(max(exp_comp$Total) * 1.4, 0))) +
     #                #scale_fill_manual(values = colVals_comp, limits = levels(exp_comp$ExpDesc)) +
     #                #scale_fill_discrete(limits = levels(expComp$ExpDesc)) +
     #                #                    geom_text(size = textLabelFun(exp_comp), position = position_dodge(width = 1), fontface = "bold", aes(label = paste(str_wrap(ExpDesc,30), " - ", Dollars, sep = ""), fill = ExpDesc), hjust = -0.25) +
     #                geom_text(size = textLabelFun(exp_comp),#4,
     #                          fontface = "bold", aes(label = Dollars), hjust = -0.25) +
     #                labs(x = "Company Name", y = "Total",
     #                     title = "Companies Receiving Campaign Expenditures",
     #                     subtitle = paste(exp_comp$Range, sep = "")) + theme_bw() +
     #                theme(axis.text.x = element_text(size = 14),
     #                      plot.title = element_text(size = 17),
     #                      legend.position = c(0.75,0.33),legend.text = element_text(size = 15),
     #                      legend.title = element_blank(),legend.background = element_rect(color = "gray50"),
     #                      plot.subtitle = element_text(size = 14),
     #                      axis.text.y = element_text(size = axisLabelFun(exp_comp), face = "bold")) +
     #                coord_flip() +
     #                guides(fill = guide_legend(override.aes = list(size=15))) +
     #                NULL
     #           
     #           
     #      }
     # })
     # 
     # #  Table
     # output$expCo_tbl <- DT::renderDataTable({
     #      x <- expCo_x() #%>% arrange(desc(Total))
     #      names(x) <- str_wrap(gsub("_", " ", names(x)), 6)
     #      x
     # })
     # 
     # # By Candidate
     # 
     # expOrg_x <- reactive({
     #      set.seed(235813)
     #      if (input$oneOrg == FALSE) {
     #           
     #           expOrg %>% 
     #                filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
     #                mutate(Range = paste("Jan 1, ", min(CY), " to ",  ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>%
     #                #filter(OrganizationName == input$exp_org) %>% 
     #                group_by(#ExpDesc,
     #                     OrganizationName,#col,#City,State,
     #                     Range) %>% 
     #                summarise(Expenditures = sum(Expenditures),
     #                          Total = sum(Total),
     #                          avg = round(Total / Expenditures, 2),
     #                          #max = max(Total, na.rm = TRUE),
     #                          First = min(First, na.rm = TRUE),
     #                          Most_Recent = max(Most_Recent, na.rm = TRUE)) %>% 
     #                ungroup() %>%
     #                #group_by(ExpDesc,OrganizationName) %>% 
     #                mutate(OrgTotal = sum(Total)) %>% ungroup() %>% 
     #                mutate(Pct_Total = round(Total / OrgTotal, 5)) %>% 
     #                mutate(Dollars = monify(Total),Avg = monify(avg), Org_Total = monify(OrgTotal),#Max = monify(max),
     #                       Pct_of_Total = percent(Pct_Total)) %>% 
     #                select(OrganizationName,#City,State,
     #                       #ExpDesc,
     #                       Expenditures,Dollars,Avg,#Max,
     #                       Org_Total,Pct_of_Total,First,Most_Recent,everything()) %>% 
     #                arrange(desc(Total))
     #           
     #      } else {
     #           
     #           expOrg %>% 
     #                filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
     #                mutate(Range = paste("Jan 1, ", min(CY), " to ", ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>%
     #                filter(OrganizationName == input$exp_org) %>% 
     #                # filter(ExpDesc == input$exp_type) %>% 
     #                group_by(ExpDesc,OrganizationName,col,#City,State,
     #                         Range) %>% 
     #                summarise(Expenditures = sum(Expenditures),
     #                          Total = sum(Total),
     #                          avg = round(Total / Expenditures, 2),
     #                          #max = max(Total, na.rm = TRUE),
     #                          First = min(First, na.rm = TRUE),
     #                          Most_Recent = max(Most_Recent, na.rm = TRUE)) %>% 
     #                ungroup() %>%
     #                group_by(ExpDesc,OrganizationName) %>% 
     #                mutate(OrgTotal = sum(Total)) %>% ungroup() %>% 
     #                mutate(Pct_Total = round(Total / OrgTotal, 5)) %>% 
     #                mutate(Dollars = monify(Total),Avg = monify(avg), Org_Total = monify(OrgTotal),#Max = monify(max),
     #                       Pct_of_Total = percent(Pct_Total)) %>% 
     #                select(OrganizationName,#City,State,
     #                       ExpDesc,Expenditures,Dollars,Avg,#Max,
     #                       Org_Total,Pct_of_Total,First,Most_Recent,everything()) %>% 
     #                arrange(desc(Total))
     #      }
     # })
     # 
     # 
     # output$expOrg_plot <- renderPlot({
     #      
     #      exp_org <- expOrg_x() %>% head(input$expSliderOrg) %>% droplevels() #%>% head(input$expCo_slider) 
     #      colVals <- sample(colors()[!colors() %in% grep("^grey[1-2]|^gray[1-2|^grey[4-5]|^gray[4-5]|^grey[7-9]|^gray[7-9]", colors(), value = TRUE)], 17); colVals
     #      
     #      if(nrow(exp_org < 1)) {
     #           p("Try selecting a different date range")
     #      } 
     #      #if(n_distinct(exp_org$OrganizationName) > 1) {
     #      if (input$oneOrg == FALSE) {
     #           
     #           ggplot(exp_org, aes(reorder(str_wrap(OrganizationName,50), Total), Total)) +
     #                geom_bar(stat = "identity", width = 0.6, 
     #                         color = "gray50", position = "dodge", fill = "cadetblue") +
     #                #aes(fill = ExpDesc)) +
     #                scale_y_continuous(labels = dollar, limits = c(0, round(max(exp_org$Total) * 1.5, 0))) +
     #                #scale_fill_discrete(limits = levels(expOrg$ExpDesc)) +
     #                #scale_fill_manual(values = colVals, limits = levels(exp_org$ExpDesc)) +
     #                coord_flip() +
     #                #geom_text(size = textLabelFun(exp_org), position = position_dodge(width = 1), fontface = "bold", aes(label = paste(str_wrap(ExpDesc,30), " - ", Dollars, sep = ""), fill = ExpDesc), hjust = -0.25) +
     #                labs(x = "Organization Name", y = "Total",
     #                     title = "Campaign Expenditures by Organization Name",
     #                     subtitle = paste(exp_org$Range, sep = "")) + theme_bw() +
     #                theme(axis.text.x = element_text(size = 16),
     #                      plot.title = element_text(size = 20),
     #                      legend.position = c(0.75,0.25),legend.text = element_text(size = 15),
     #                      legend.title = element_blank(),legend.background = element_rect(color = "gray50"),
     #                      plot.subtitle = element_text(size = 16),
     #                      axis.text.y = element_text(size = axisLabelFun(exp_org), face = "bold")) +# guides(fill = FALSE) +
     #                guides(fill = guide_legend(override.aes = list(size=10))) +
     #                #               coord_flip() +
     #                NULL
     #      } else {
     #           ggplot(exp_org, aes(reorder(str_wrap(ExpDesc,50), Total), Total)) +
     #                geom_bar(stat = "identity", width = 0.6, color = "gray50", #fill = "mintcream"
     #                         aes(fill = ExpDesc)) +
     #                scale_y_continuous(labels = dollar, limits = c(0, round(max(exp_org$Total) * 1.5, 0))) +
     #                #                    scale_fill_discrete(limits = levels(exp_org$ExpDesc)) +
     #                scale_fill_manual(values = colVals, limits = levels(exp_org$ExpDesc)) +
     #                geom_text(size = textLabelFun(exp_org), fontface = "bold", aes(label = Dollars), hjust = -0.25) +
     #                labs(x = "Organization Name", y = "Total",
     #                     title = paste("Campaign Expenditures by ", unique(exp_org$OrganizationName), sep = ""),
     #                     #                         title = "Campaign Expenditures by Organization Name",
     #                     subtitle = paste(exp_org$Range, sep = "")) + theme_bw() +
     #                theme(axis.text.y = element_text(size = 14),
     #                      plot.subtitle = element_text(size = 14),
     #                      plot.title = element_text(size = 17),
     #                      legend.position = "none",# c(0.75,0.25),legend.text = element_text(size = 15),
     #                      legend.title = element_blank(),legend.background = element_rect(color = "gray50"),
     #                      axis.text.x = element_text(size = axisLabelFun(exp_org), face = "bold")) +
     #                guides(fill = guide_legend(override.aes = list(size=10))) +
     #                coord_flip() + #guides(fill = FALSE) +
     #                #coord_polar() +
     #                NULL
     #      }
     #      #}
     #      
     # })
     # 
     # #  Table
     # output$expOrg_tbl <- DT::renderDataTable({
     #      x <- expOrg_x() #%>% arrange(desc(Total))
     #      names(x) <- str_wrap(gsub("_", " ", names(x)), 6)
     #      x
     # })
     # 
     # 
     # ***********************************************************************  Network Visualizations
     
     # visEmpx <- reactive({
     #      
     #      if(input$visInd == TRUE) {
     #           
     #           donateVisInd %>%
     #                filter(Ind == "Industry") %>% 
     #                select(-Ind) %>% 
     #                filter(Industry == input$visIndustry) %>% 
     #                filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
     #                mutate(Range = paste("Jan 1, ", min(CY), " to ", ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>%
     #                group_by(Employer,OrganizationName,Range) %>% 
     #                summarise(Total = round(sum(Total, na.rm = TRUE),0)) %>% 
     #                ungroup() %>% 
     #                arrange(desc(Total))
     #           
     #      } else {
     #           
     #           donateVis %>% 
     #                filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
     #                mutate(Range = paste("Jan 1, ", min(CY), " to ", ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>%
     #                group_by(Employer,OrganizationName,Range) %>% 
     #                summarise(Total = round(sum(Total, na.rm = TRUE),0)) %>% 
     #                ungroup() %>% 
     #                arrange(desc(Total))
     #           #}
     #      }
     # })
     # 
     # 
     # output$empVis_plot <- renderPlot({
     #      set.seed(1881)
     #      
     #      df_vis <- visEmpx() %>% 
     #           head(input$sliderVis) %>% 
     #           transform(Employer = str_wrap(str_to_title(Employer), 12),
     #                     OrganizationName = str_wrap(str_to_title(OrganizationName), 14)) %>% 
     #           group_by(OrganizationName) %>% 
     #           mutate(edgeCol = sample(colors()[!colors() %in% grep("^grey1|^gray1|^grey4|^gray4|^grey|7^gray7", colors(), value = TRUE)], 1)) %>% 
     #           ungroup()
     #      
     #      eCol <- df_vis$edgeCol; dfRange <- unique(df_vis$Range)
     #      
     #      
     #      d <- data.frame(label = c(df_vis$Employer, df_vis$OrganizationName), stringsAsFactors = FALSE) %>% distinct() %>% 
     #           mutate(col = ifelse(label %in% df_vis$Employer, "dodgerblue","lightgoldenrod"),
     #                  shp = ifelse(label %in% df_vis$Employer, "circle","diamond"))
     #      
     #      
     #      df_vis <- df_vis %>% select(-c(Range,edgeCol))
     #      
     #      #xLabs <- monify(df_vis$Total)
     #      xLabs <- ifelse(df_vis$Total >= quantile(df_vis$Total, probs = 0.25), monify(df_vis$Total), "")
     #      
     #      suppressWarnings(
     #           qgraph(df_vis, 
     #                  layout = input$layout,
     #                  vsize = input$vertexSlide, # Size of the nodes
     #                  label.cex = input$nodeSlide, # Size of Node Labels
     #                  mar = c(1,1,1,1),
     #                  title = paste("Political Connections\n", dfRange, sep = ""), 
     #                  #fade = input$fade,
     #                  bg = "white",
     #                  fade = TRUE,
     #                  vTrans = 100,
     #                  color = d$col, 
     #                  shape = d$shp,
     #                  asize = 3,
     #                  label.scale.equal = TRUE,
     #                  edge.label.color = "black", # Color of the numeric labels
     #                  edge.color = eCol,#x2$edgeCol,#"gray50", # Line color
     #                  edge.width = 0.75, 
     #                  edge.label.cex = input$edgeLabSlide,
     #                  edge.label.position = input$labelPos,
     #                  edge.label.bg = "gray90",
     #                  edge.labels= xLabs))
     #      
     #      
     # })
     # 
     # 
     
     # ***********************************************************************  Donors to Organization Network Vis
     
     
     visOrgx <- reactive({
          
          if(input$visOrg == TRUE) {
               
               donateVisOrg %>%
                    filter(Ind == "Industry") %>%
                    select(-Ind) %>%
                    filter(Industry == input$visIndustryOrg) %>%
                    filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
                    mutate(Range = paste("Jan 1, ", min(CY), " to ", ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>%
                    group_by(FullName,OrganizationName,Range) %>%
                    summarise(Total = round(sum(Total, na.rm = TRUE),0)) %>%
                    ungroup() %>%
                    arrange(desc(Total))
               
          } else {
               
               donateVisOrg %>%
                    filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
                    mutate(Range = paste("Jan 1, ", min(CY), " to  ", ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>%
                    group_by(FullName,OrganizationName,Range) %>%
                    summarise(Total = round(sum(Total, na.rm = TRUE),0)) %>%
                    ungroup() %>%
                    arrange(desc(Total))
               #}
          }
     })
     
     
     output$visOrg_plot <- renderPlot({
          set.seed(87)
          
          df_vis_org <- visOrgx() %>%
               head(input$sliderVisOrg) %>%
               transform(FullName = str_wrap(FullName, 12),
                         OrganizationName = str_wrap(str_to_title(OrganizationName), 14)) %>%
               group_by(OrganizationName) %>%
               mutate(edgeCol = sample(colors()[!colors() %in% grep("^grey1|^gray1|^grey4|^gray4|^grey|7^gray7", colors(), value = TRUE)], 1)) %>%
               ungroup()
          
          eCol <- df_vis_org$edgeCol; dfRange <- unique(df_vis_org$Range)
          
          
          d <- data.frame(label = c(df_vis_org$FullName, df_vis_org$OrganizationName), stringsAsFactors = FALSE) %>% distinct() %>%
               mutate(col = ifelse(label %in% df_vis_org$FullName, "greenyellow","turquoise"),
                      shp = ifelse(label %in% df_vis_org$FullName, "circle","diamond"))
          
          
          df_vis_org <- df_vis_org %>% select(-c(Range,edgeCol))
          
          #xLabs <- monify(df_vis$Total)
          xLabs <- ifelse(df_vis_org$Total >= quantile(df_vis_org$Total, probs = 0.25), monify(df_vis_org$Total), "")
          
          suppressWarnings(
               qgraph(df_vis_org,
                      layout = input$layoutOrg,
                      vsize = input$vertexSlideOrg, # Size of the nodes
                      label.cex = input$nodeSlideOrg, # Size of Node Labels
                      mar = c(1,1,1,1),
                      title = paste("Political Connections\n", dfRange, sep = ""),
                      #fade = input$fade,
                      lcolor = "ghostwhite",
                      bg = "gray0",
                      fade = TRUE,
                      vTrans = 100,
                      asize = 3,
                      color = d$col,
                      shape = d$shp,
                      color.main = "white",
                      label.scale.equal = TRUE,
                      edge.label.color = "navyblue", # Color of the numeric labels
                      edge.color = eCol,#x2$edgeCol,#"gray50", # Line color
                      edge.width = 0.75,
                      edge.label.cex = input$edgeLabSlideOrg,
                      edge.label.position = input$labelPosOrg,
                      edge.label.bg = "lightyellow1",
                      edge.labels= xLabs))
          
          
     })
     
     
     
     
     exp_vizx <- reactive({
          
          if(input$viz_exp_net_box == TRUE) {
               
               expNet %>%
                    filter(ExpDesc == input$viz_exp_net) %>%
                    filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
                    mutate(Range = paste("Jan 1, ", min(CY), " to ", ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>%
                    group_by(OrganizationName,FullName,Range,ExpDesc) %>%
                    summarise(Total = round(sum(Total, na.rm = TRUE),0)) %>%
                    ungroup() %>%
                    arrange(desc(Total))
               
          } else {
               
               expNet %>%
                    filter(CY >= input$sliderRange[1] & CY <= input$sliderRange[2]) %>%
                    mutate(Range = paste("Jan 1, ", min(CY), " to ", ifelse(max(CY) == 2018, upload_date18, upload_date), max(CY), sep = "")) %>%
                    group_by(OrganizationName,FullName,Range,ExpDesc) %>%
                    summarise(Total = round(sum(Total, na.rm = TRUE),0)) %>%
                    ungroup() %>%
                    arrange(desc(Total))
               #}
          }
     })
     
     
     output$exp_net_plot <- renderPlot({
          
          if(input$exp_net_view == "Candidate") {
               
               df_exp_viz <- exp_vizx() %>%
                    head(input$slider_exp_net) %>%
                    transform(FullName = str_wrap(FullName, 12),
                              OrganizationName = str_wrap(str_to_title(OrganizationName), 14)) %>%
                    group_by(OrganizationName) %>%
                    mutate(edgeCol = sample(colors()[!colors() %in% grep("^grey[1-2]|^gray[1-2]|^grey[4-6]|^gray[4-6]|^grey[8-9]|^gray[8-9]", colors(), value = TRUE)], 1)) %>%
                    ungroup()
               
          } else {
               
               df_exp_viz <- exp_vizx() %>%
                    head(input$slider_exp_net) %>%
                    transform(FullName = str_wrap(FullName, 12),
                              OrganizationName = str_wrap(str_to_title(OrganizationName), 14)) %>%
                    group_by(FullName) %>%
                    mutate(edgeCol = sample(colors()[!colors() %in% grep("^grey[1-2]|^gray[1-2]|^grey[4-6]|^gray[4-6]|^grey[8-9]|^gray[8-9]", colors(), value = TRUE)], 1)) %>%
                    ungroup()
               
          }
          
          eCol <- df_exp_viz$edgeCol; dfRange <- unique(df_exp_viz$Range)
          
          
          d <- data.frame(label = c(df_exp_viz$OrganizationName, df_exp_viz$FullName), stringsAsFactors = FALSE) %>% distinct() %>%
               mutate(col = ifelse(label %in% df_exp_viz$FullName, "greenyellow","turquoise"),
                      shp = ifelse(label %in% df_exp_viz$FullName, "circle","diamond"))
          
          
          df_exp_viz <- df_exp_viz %>% select(OrganizationName,FullName,Total)
          
          #xLabs <- monify(df_vis$Total)
          xLabs <- ifelse(df_exp_viz$Total >= quantile(df_exp_viz$Total, probs = 0.1), monify(df_exp_viz$Total), "")
          
          suppressWarnings(
               qgraph(df_exp_viz,
                      layout = input$layout_exp_net,
                      vsize = input$vertexSize_exp_net, # Size of the nodes
                      label.cex = input$nodeSlide_exp_net, # Size of Node Labels
                      mar = c(1.5,1.5,1.5,1.5),
                      title = paste("Campaign Expenditures\n", dfRange, sep = ""),
                      #fade = input$fade,
                      lcolor = "black", bg = "white", edge.label.bg = "gray90",
                      fade = input$exp_net_fade,
                      vTrans = 100,
                      asize = 3,
                      color = d$col,
                      shape = d$shp,
                      color.main = "white",
                      label.scale.equal = TRUE,
                      edge.label.color = "navyblue", # Color of the numeric labels
                      edge.color = eCol,#x2$edgeCol,#"gray50", # Line color
                      edge.width = 0.75,
                      edge.label.cex = input$edgeLabSlide_exp_net,
                      edge.label.position = input$labelPosOrg,
                      edge.labels= xLabs))
          
          
     })
     
     
     
}        

shinyApp(ui = ui, server = server)




#












