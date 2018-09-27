
## app.R ##
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(stringr)
library(ggplot2)
library(ggrepel)
library(viridis)


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

# Read in formatted finance data
#dfx <- readRDS("//cdsfsp06/InterSite/Integra/analytics/Misc_Projects/Misc/Providence_Open_Data/campaign_finance_2018-07-30.rds")
#dfx <- readRDS("//cdsfsp06/InterSite/Integra/analytics/Misc_Projects/Misc/Providence_Open_Data/campaign_finance_2018-08-01.rds")


dir <- "/Users/jeffreyrichardson/Documents/Campaign_Finance/Psephology_App/"

dfx <- readRDS(paste(dir, "campaign_finance_2018-09-27.rds", sep = "")) %>% 
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

# subIndustry <- sort(unique(dfx$Industry))
# industry <- sort(unique(dfx$Industry2))
# donors <- sort(unique(dfx$FullName))
# employers <- sort(unique(dfx$Employer))
# organizations <- sort(unique(dfx$OrganizationName))
# donor_cities <- sort(unique(dfx$donor_city))
# emp_cities <- sort(unique(dfx$emp_city))
aliases <- dfx %>% 
     group_by(FullName) %>% 
     summarise(Aliases = paste(sort(unique(str_to_title(Donor_Name_Unformatted))), collapse = "| "),
               Employers = paste(sort(unique(str_to_title(EmployerName))), collapse = "| "),
               Industries = paste(sort(unique(Industry)), collapse = "| ")) %>% 
     ungroup() %>% 
     arrange(FullName); head(aliases)


# Save
saveRDS(aliases, paste(dir, "aliases.rds", sep = ""))


# All Donors w/ Over $1000
topDonors <- dfx %>% group_by(FullName) %>% 
     summarise(Donations = n_distinct(ContributionID),
               First_Donation = min(ReceiptDate),
               Most_Recent_Donation = max(ReceiptDate),
               Total_Donated = sum(Amount, na.rm = TRUE),
               Dollars = monify(Total_Donated),
               Max_Donation = max(Amount, na.rm = TRUE),
               Avg_Donation = round(Total_Donated / Donations, 2),
               Median_Donation = median(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     filter(Total_Donated > 2500) %>% 
     arrange(desc(Total_Donated)); head(topDonors)

# Save
saveRDS(topDonors, paste(dir, "topDonors.rds", sep = ""))

# All Donors 
allDonors <- dfx %>% group_by(FullName) %>% 
     summarise(Donations = n_distinct(ContributionID),
               First_Donation = min(ReceiptDate),
               Most_Recent_Donation = max(ReceiptDate),
               Total_Donated = sum(Amount, na.rm = TRUE),
               Dollars = monify(Total_Donated),
               Max_Donation = max(Amount, na.rm = TRUE),
               Avg_Donation = round(Total_Donated / Donations, 2),
               Median_Donation = median(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total_Donated)); head(allDonors)

# Save
saveRDS(allDonors, paste(dir, "allDonors.rds", sep = ""))

# All Donors Loans Excluded
allDonors_noloans <- dfx %>% group_by(FullName) %>% 
     filter(ContDesc != "Loan Proceeds") %>% 
     summarise(Donations = n_distinct(ContributionID),
               First_Donation = min(ReceiptDate),
               Most_Recent_Donation = max(ReceiptDate),
               Total_Donated = sum(Amount, na.rm = TRUE),
               Dollars = monify(Total_Donated),
               Max_Donation = max(Amount, na.rm = TRUE),
               Avg_Donation = round(Total_Donated / Donations, 2),
               Median_Donation = median(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total_Donated)); head(allDonors_noloans)

# Save
saveRDS(allDonors_noloans, paste(dir, "allDonors_noloans.rds", sep = ""))


# By Last Name & First Name
lasts <- dfx %>% #filter(CY == 2018) %>% 
     filter(ContDesc != "Loan Proceeds") %>% 
     #group_by(FirstName,LastName) %>% 
     transform(LastName = str_to_title(LastName)) %>% 
     group_by(LastName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE),
               Individuals = n_distinct(FullName),
               Orgs = n_distinct(OrganizationName),
               Employers = n_distinct(Employer),
               Donations = n_distinct(ContributionID),
               Avg_per_Donation = round(Total / Donations, 2),
               Avg_per_Donor = round(Total / Individuals, 2),
               Mean_Donation = mean(Amount, na.rm = TRUE),
               Median_Donation = median(Amount, na.rm = TRUE),
               Max_Donation = max(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     mutate(Dollars = monify(Total), Title = "Last") %>% 
     select(LastName,Total,Dollars,everything()) %>% 
     rename(Llamo = LastName); head(lasts, 20)

firsts <- dfx %>% #filter(CY == 2018) %>% 
     filter(ContDesc != "Loan Proceeds") %>% 
     #group_by(FirstName,LastName) %>% 
     transform(FirstName = str_to_title(FirstName)) %>% 
     group_by(FirstName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE),
               Individuals = n_distinct(FullName),
               Orgs = n_distinct(OrganizationName),
               Employers = n_distinct(Employer),
               Donations = n_distinct(ContributionID),
               Avg_per_Donation = round(Total / Donations, 2),
               Avg_per_Donor = round(Total / Individuals, 2),
               Mean_Donation = mean(Amount, na.rm = TRUE),
               Median_Donation = median(Amount, na.rm = TRUE),
               Max_Donation = max(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     mutate(Dollars = monify(Total), Title = "First") %>% 
     select(FirstName,Total,Dollars,everything()) %>% 
     rename(Llamo = FirstName); head(firsts, 20)

dynasties <- bind_rows(lasts,firsts); rm(lasts,firsts); head(dynasties)

# Save
saveRDS(dynasties, paste(dir, "dynasties.rds", sep = ""))

# Clear
rm(firsts,lasts)

# Loans
yearlyDonorLoans <- dfx %>% #transform(FullName = gsub("\\s{2,}", " ", trimws(FullName))) %>%
     group_by(FullName,CY) %>%
     mutate(Aliases = paste(unique(Donor_Name_Unformatted), collapse = " | "),
            Employers = paste(unique(Employer), collapse = " | ")) %>% ungroup() %>%
     filter(ContDesc == "Loan Proceeds") %>%
     group_by(FullName,CY) %>%
     summarise(Number_of_Loans = n_distinct(ContributionID),
               Total_Loan_Amount = sum(Amount, na.rm = TRUE),
               Avg_Loan_Amount = round(Total_Loan_Amount / Number_of_Loans, 2),
               Median_Loan_Amount = median(Amount, na.rm = TRUE),
               Max_Loan_Amount = max(Amount, na.rm = TRUE),
               #Min_Loan_Amount = min(Amount, na.rm = TRUE),
               #Number_of_Organization_Loans = n_distinct(ContributionID[PAC == "Not PAC"]),
               #Number_of_PAC_Loans = n_distinct(ContributionID[PAC == "PAC"]),
               Organizations_Loaned_To = n_distinct(OrganizationName),
               Orgs_Receiving_Loans = paste(unique(sort(OrganizationName)), collapse = " | ")) %>% 
     #Orgs_Receiving_Loans = paste(unique(OrganizationName[PAC == "Not PAC"]), collapse = " | "),
     #Org_Loan_Total = sum(Amount[PAC == "Not PAC"], na.rm = TRUE),
     #PACs_Loaned_To = n_distinct(OrganizationName[PAC == "PAC"]),
     #PACs_Receiving_Loans = paste(unique(PAC[PAC == "PAC"]), collapse = " | "),
     #PAC_Loan_Total = sum(Amount[PAC == "PAC"])) %>%
     ungroup() %>% filter(Total_Loan_Amount > 0) %>% 
     transform(Orgs_Receiving_Loans = gsub(" \\| $", "", Orgs_Receiving_Loans)) %>% 
     arrange(desc(Total_Loan_Amount)); head(yearlyDonorLoans)


# Save
saveRDS(yearlyDonorLoans, paste(dir, "Yearly_Donor_Loans.rds", sep = ""))

# Donor Donations To
yearlyDonorDonations <- dfx %>% filter(FullName != "") %>%
     group_by(FullName,CY) %>%
     mutate(Aliases = paste(unique(Donor_Name_Unformatted), collapse = " | "),
            Employers = paste(unique(Employer), collapse = " | ")) %>% ungroup() %>%
     filter(ContDesc != "Loan Proceeds") %>%
     group_by(FullName,Aliases,Employers,OrganizationName,PAC,ContDesc,CY) %>%
     summarise(Number_of_Donations = n_distinct(ContributionID),
               Donations_Under_100 = n_distinct(ContributionID[Amount < 100]),
               Donations_Over_100 = n_distinct(ContributionID[Amount >= 100]),
               Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
               Total_Donation_Amount = sum(Amount, na.rm = TRUE),
               Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
               Median_Donation_Amount = round(median(Amount, na.rm= TRUE), 2),
               Max_Donation = max(Amount, na.rm = TRUE)) %>%
     ungroup() %>% 
     arrange(FullName,CY,desc(Total_Donation_Amount)) %>% 
     filter(Total_Donation_Amount > 0); head(yearlyDonorDonations)

# Save
saveRDS(yearlyDonorDonations, paste(dir, "yearlyDonorDonations.rds", sep = ""))

# Top Donors each year
yearlyTopDonations <- yearlyDonorDonations %>%
     group_by(CY) %>%
     top_n(Total_Donation_Amount, n = 250) %>%
     ungroup() %>%
     arrange(desc(CY), desc(Total_Donation_Amount)); head(yearlyTopDonations)

# Save
saveRDS(yearlyTopDonations, paste(dir, "yearlyTopDonations.rds", sep = ""))

# Donor Summary
yearlyDonorOrgs <- dfx  %>% #filter(FullName != "") %>%
     group_by(FullName) %>%
     mutate(Aliases = paste(unique(Donor_Name_Unformatted), collapse = " | "),
            Employers = paste(unique(Employer), collapse = " | ")) %>% ungroup() %>%
     filter(ContDesc != "Loan Proceeds") %>%
     group_by(FullName,Aliases,Employers,CY) %>%
     summarise(Number_of_Donations = n_distinct(ContributionID),
               Donations_Under_100 = n_distinct(ContributionID[Amount < 100]),
               Donations_Over_100 = n_distinct(ContributionID[Amount >= 100]),
               Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
               Total_Donation_Amount = sum(Amount, na.rm = TRUE),
               Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
               Organizations_Donated_To = n_distinct(OrganizationName[PAC == "Not PAC"]),
               Org_Donations = n_distinct(ContributionID[PAC == "Not PAC"]),
               Org_Donation_Amount = sum(Amount[PAC == "Not PAC"], na.rm = TRUE),
               PACs_Donated_To = n_distinct(OrganizationName[PAC == "PAC"]),
               PAC_Donations = n_distinct(ContributionID[PAC == "PAC"]),
               PAC_Donation_Amount = sum(Amount[PAC == "PAC"]),
               Max_Donation = max(Amount, na.rm = TRUE),
               Orgs_Receiving_Donation = paste(unique(OrganizationName[PAC == "Not PAC"]), collapse = " | "),
               PACs_Receiving_Donation = ifelse(PACs_Donated_To == 0, "None",
                                                paste(unique(OrganizationName[PAC == "PAC"]), collapse = " | "))) %>%
     ungroup() %>% filter(Total_Donation_Amount > 0) %>% 
     arrange(desc(Total_Donation_Amount)); head(yearlyDonorOrgs)

# Save
saveRDS(yearlyDonorOrgs, paste(dir, "Yearly_Donor_Orgs.rds", sep = ""))

# PACs
yearlyPAC <- dfx %>%
     filter(PAC == "PAC") %>% 
     group_by(OrganizationName,CY) %>%
     summarise(Number_of_Donations = n_distinct(ContributionID),
               Donations_Under_100 = n_distinct(ContributionID[Amount < 100]),
               Donations_Over_100 = n_distinct(ContributionID[Amount >= 100]),
               Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
               Total_Donation_Amount = sum(Amount, na.rm = TRUE),
               Dollars = monify(Total_Donation_Amount),
               Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
               Max_Donation = max(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% filter(Total_Donation_Amount > 0) %>% 
     #group_by(FullName, CY) %>% mutate()
     arrange(desc(CY), desc(Total_Donation_Amount)); head(yearlyPAC)

# Save
saveRDS(yearlyPAC, paste(dir, "yearlyPAC.rds", sep = ""))

# Donations to PACs
yearlyDonorPAC <- dfx %>%
     #filter(ContDesc != "Loan Proceeds") %>%
     filter(PAC == "PAC") %>% 
     group_by(FullName,OrganizationName,CY) %>%
     summarise(Number_of_Donations = n_distinct(ContributionID),
               Total_Donation_Amount = sum(Amount, na.rm = TRUE),
               Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
               Max_Donation = max(Amount, na.rm = TRUE)) %>%
     ungroup() %>% filter(Total_Donation_Amount > 0) %>% 
     arrange(#desc(CY), 
          desc(Total_Donation_Amount)); head(yearlyDonorPAC)

# Save
saveRDS(yearlyDonorPAC, paste(dir, "yearlyDonorPAC.rds", sep = ""))

# Employer donations to PACs
yearlyEmp_to_PAC <- dfx %>%
     filter(ContDesc != "Loan Proceeds") %>%
     filter(PAC == "PAC") %>% 
     group_by(Employer,Industry2,Industry,OrganizationName,CY) %>% 
     summarise(Number_of_Donations = n_distinct(ContributionID),
               Donations_Under_100 = n_distinct(ContributionID[Amount < 100]),
               Donations_Over_100 = n_distinct(ContributionID[Amount >= 100]),
               Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
               Total_Donation_Amount = sum(Amount, na.rm = TRUE),
               Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
               Max_Donation = max(Amount, na.rm = TRUE)) %>%
     ungroup() %>% filter(Total_Donation_Amount > 0) %>% 
     arrange(desc(CY), desc(Total_Donation_Amount)); head(yearlyEmp_to_PAC)

#filter(yearlyEmp_to_PAC, Employer == "seiu local 1199") %>% View("x")
# Save
saveRDS(yearlyEmp_to_PAC, paste(dir, "yearlyEmp_to_PAC.rds", sep = ""))


# Employer to Organization
yearlyEmp_to_Org <- dfx %>%
     filter(ContDesc != "Loan Proceeds") %>%
     filter(PAC != "PAC") %>% 
     group_by(Employer,Industry2,Industry,OrganizationName,CY) %>% #,CY) %>%
     summarise(Number_of_Donations = n_distinct(ContributionID),
               Donations_Under_100 = n_distinct(ContributionID[Amount < 100]),
               Donations_Over_100 = n_distinct(ContributionID[Amount >= 100]),
               Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
               Total_Donation_Amount = sum(Amount, na.rm = TRUE),
               Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
               Max_Donation = max(Amount, na.rm = TRUE)) %>%
     ungroup() %>% filter(Total_Donation_Amount > 0) %>% 
     arrange(desc(CY), 
             desc(Total_Donation_Amount)); head(yearlyEmp_to_Org)

saveRDS(yearlyEmp_to_Org, paste(dir, "yearlyEmp_to_Org.rds", sep = ""))

# By Donor City & State
yearlyLoc <- dfx  %>% 
     filter(ContDesc != "Loan Proceeds") %>% 
     group_by(City, donor_city, donor_st,donor_state_name, donor_region,CY) %>%
     summarise(Donors = n_distinct(FullName),
               Number_of_Donations = n_distinct(ContributionID),
               Donations_Under_100 = n_distinct(ContributionID[Amount < 100]),
               Donations_Over_100 = n_distinct(ContributionID[Amount >= 100]),
               Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
               Num_PAC_Donations = n_distinct(ContributionID[PAC == "PAC"]),
               Num_Org_Donations = n_distinct(ContributionID[PAC == "Not PAC"]),
               Total_Donation_Amount = sum(Amount, na.rm = TRUE),
               PAC_Donation_Amount = sum(Amount[PAC == "PAC"], na.rm = TRUE),
               Org_Donation_Amount = sum(Amount[PAC == "Not PAC"], na.rm = TRUE),
               Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
               Avg_PAC_Donation_Amt = round(PAC_Donation_Amount / Num_PAC_Donations, 2),
               Avg_Org_Donation_Amt = round(Org_Donation_Amount / Num_Org_Donations, 2)) %>%
     ungroup() %>% 
     arrange(desc(CY), desc(Total_Donation_Amount)); head(yearlyLoc)

# Save
saveRDS(yearlyLoc, paste(dir, "yearlyLoc.rds", sep = ""))


yearlyState <- dfx %>% 
     #filter(ContDesc != "Loan Proceeds") %>% 
     #mutate(City = paste(donor_city, donor_st, sep = ", ")) %>% 
     group_by(donor_state_name, donor_st, donor_region,CY) %>%
     summarise(Donors = n_distinct(FullName),
               Number_of_Donations = n_distinct(ContributionID),
               Donations_Under_100 = n_distinct(ContributionID[Amount < 100]),
               Donations_Over_100 = n_distinct(ContributionID[Amount >= 100]),
               Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
               Num_PAC_Donations = n_distinct(ContributionID[PAC == "PAC"]),
               Num_Org_Donations = n_distinct(ContributionID[PAC == "Not PAC"]),
               Total_Donation_Amount = sum(Amount, na.rm = TRUE),
               PAC_Donation_Amount = sum(Amount[PAC == "PAC"], na.rm = TRUE),
               Org_Donation_Amount = sum(Amount[PAC == "Not PAC"], na.rm = TRUE),
               Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
               Avg_PAC_Donation_Amt = round(PAC_Donation_Amount / Num_PAC_Donations, 2),
               Avg_Org_Donation_Amt = round(Org_Donation_Amount / Num_Org_Donations, 2)) %>%
     ungroup() %>% 
     arrange(desc(CY), desc(Total_Donation_Amount)); head(yearlyState)

# By Employer City & State
yearlyLocEmp <- dfx  %>% 
     filter(ContDesc != "Loan Proceeds") %>% 
     group_by(City, donor_city, donor_st,donor_state_name, donor_region,CY) %>%
     summarise(Donors = n_distinct(FullName),
               Number_of_Donations = n_distinct(ContributionID),
               Donations_Under_100 = n_distinct(ContributionID[Amount < 100]),
               Donations_Over_100 = n_distinct(ContributionID[Amount >= 100]),
               Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
               Num_PAC_Donations = n_distinct(ContributionID[PAC == "PAC"]),
               Num_Org_Donations = n_distinct(ContributionID[PAC == "Not PAC"]),
               Total_Donation_Amount = sum(Amount, na.rm = TRUE),
               PAC_Donation_Amount = sum(Amount[PAC == "PAC"], na.rm = TRUE),
               Org_Donation_Amount = sum(Amount[PAC == "Not PAC"], na.rm = TRUE),
               Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
               Avg_PAC_Donation_Amt = round(PAC_Donation_Amount / Num_PAC_Donations, 2),
               Avg_Org_Donation_Amt = round(Org_Donation_Amount / Num_Org_Donations, 2)) %>%
     ungroup() %>% 
     arrange(desc(CY), desc(Total_Donation_Amount)); head(yearlyLoc)

# Save
saveRDS(yearlyLocEmp, paste(dir, "yearlyLocEmp.rds", sep = ""))



yearlyStateEmp <- dfx %>% 
     #filter(ContDesc != "Loan Proceeds") %>% 
     #mutate(City = paste(donor_city, donor_st, sep = ", ")) %>% 
     group_by(emp_state_name, emp_st, emp_region) %>%
     summarise(Donors = n_distinct(FullName),
               Number_of_Donations = n_distinct(ContributionID),
               Donations_Under_100 = n_distinct(ContributionID[Amount < 100]),
               Donations_Over_100 = n_distinct(ContributionID[Amount >= 100]),
               Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
               Num_PAC_Donations = n_distinct(ContributionID[PAC == "PAC"]),
               Num_Org_Donations = n_distinct(ContributionID[PAC == "Not PAC"]),
               Total_Donation_Amount = sum(Amount, na.rm = TRUE),
               PAC_Donation_Amount = sum(Amount[PAC == "PAC"], na.rm = TRUE),
               Org_Donation_Amount = sum(Amount[PAC == "Not PAC"], na.rm = TRUE),
               Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
               Avg_PAC_Donation_Amt = round(PAC_Donation_Amount / Num_PAC_Donations, 2),
               Avg_Org_Donation_Amt = round(Org_Donation_Amount / Num_Org_Donations, 2)) %>%
     ungroup() %>% 
     arrange(#desc(CY), 
          desc(Total_Donation_Amount)); head(yearlyStateEmp)

# Save
saveRDS(yearlyStateEmp, paste(dir, "yearlyStateEmp.rds", sep = ""))

# Organization Summary
yearlyOrg <- dfx %>% 
     filter(ContDesc != "Loan Proceeds") %>% 
     group_by(OrganizationName,CY) %>%
     summarise(Number_of_Donations = n_distinct(ContributionID),
               Donations_Under_100 = n_distinct(ContributionID[Amount < 100]),
               Donations_Over_100 = n_distinct(ContributionID[Amount >= 100]),
               Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
               Total_Donation_Amount = sum(Amount, na.rm = TRUE),
               Dollars = monify(Total_Donation_Amount),
               Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
               Max_Donation = max(Amount, na.rm = TRUE)) %>% 
     ungroup() %>%
     arrange(desc(CY), desc(Total_Donation_Amount)); head(yearlyOrg)

# Save
saveRDS(yearlyOrg, paste(dir, "yearlyOrg.rds", sep = ""))

# Employer Summary
yearlyEmp <- dfx %>% 
     filter(ContDesc != "Loan Proceeds") %>% 
     group_by(#EmployerName,
          Employer,Industry,Industry2,CY) %>%
     summarise(Number_of_Donations = n_distinct(ContributionID),
               Donations_Under_100 = n_distinct(ContributionID[Amount < 100]),
               Donations_Over_100 = n_distinct(ContributionID[Amount >= 100]),
               Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
               Total_Donation_Amount = sum(Amount, na.rm = TRUE),
               Dollars = monify(Total_Donation_Amount),
               Num_PAC_Donations = n_distinct(ContributionID[PAC == "PAC"]),
               Num_Org_Donations = n_distinct(ContributionID[PAC == "Not PAC"]),
               Total_Donation_Amount = sum(Amount, na.rm = TRUE),
               PAC_Donation_Amount = sum(Amount[PAC == "PAC"], na.rm = TRUE),
               Org_Donation_Amount = sum(Amount[PAC == "Not PAC"], na.rm = TRUE),
               Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
               Avg_PAC_Donation_Amt = round(PAC_Donation_Amount / Num_PAC_Donations, 2),
               Avg_Org_Donation_Amt = round(Org_Donation_Amount / Num_Org_Donations, 2),
               Max_Donation = max(Amount, na.rm = TRUE)) %>%
     ungroup() %>%
     arrange(desc(CY), desc(Total_Donation_Amount)); head(yearlyEmp)

# Save
saveRDS(yearlyEmp, paste(dir, "yearlyEmp.rds", sep = ""))

# Employer to Organization
yearlyEmpOrg <- dfx  %>% 
     filter(ContDesc != "Loan Proceeds") %>% 
     group_by(Employer,#Industry,
              Industry2,OrganizationName,CY) %>%
     summarise(Number_of_Donations = n_distinct(ContributionID),
               Donations_Under_100 = n_distinct(ContributionID[Amount < 100]),
               Donations_Over_100 = n_distinct(ContributionID[Amount >= 100]),
               Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
               Total_Donation_Amount = sum(Amount, na.rm = TRUE),
               Dollars = monify(Total_Donation_Amount),
               Num_PAC_Donations = n_distinct(ContributionID[PAC == "PAC"]),
               Num_Org_Donations = n_distinct(ContributionID[PAC == "Not PAC"]),
               Total_Donation_Amount = sum(Amount, na.rm = TRUE),
               PAC_Donation_Amount = sum(Amount[PAC == "PAC"], na.rm = TRUE),
               Org_Donation_Amount = sum(Amount[PAC == "Not PAC"], na.rm = TRUE),
               Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
               Avg_PAC_Donation_Amt = round(PAC_Donation_Amount / Num_PAC_Donations, 2),
               Avg_Org_Donation_Amt = round(Org_Donation_Amount / Num_Org_Donations, 2),
               Max_Donation =  max(Amount, na.rm = TRUE)) %>%
     ungroup() %>%
     arrange(#desc(CY), 
          desc(Total_Donation_Amount)); head(yearlyEmpOrg)

# Save
saveRDS(yearlyEmpOrg, paste(dir, "yearlyEmpOrg.rds", sep = ""))



# Industry Summary
yearlyInd <- dfx %>% 
     filter(ContDesc != "Loan Proceeds") %>% 
     group_by(Industry,Industry2,CY) %>%
     summarise(Number_of_Donations = n_distinct(ContributionID),
               Donations_Under_100 = n_distinct(ContributionID[Amount < 100]),
               Donations_Over_100 = n_distinct(ContributionID[Amount >= 100]),
               Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
               Total_Donation_Amount = sum(Amount, na.rm = TRUE),
               Dollars = monify(Total_Donation_Amount),
               Num_PAC_Donations = n_distinct(ContributionID[PAC == "PAC"]),
               Num_Org_Donations = n_distinct(ContributionID[PAC == "Not PAC"]),
               Total_Donation_Amount = sum(Amount, na.rm = TRUE),
               PAC_Donation_Amount = sum(Amount[PAC == "PAC"], na.rm = TRUE),
               Org_Donation_Amount = sum(Amount[PAC == "Not PAC"], na.rm = TRUE),
               Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
               Avg_PAC_Donation_Amt = round(PAC_Donation_Amount / Num_PAC_Donations, 2),
               Avg_Org_Donation_Amt = round(Org_Donation_Amount / Num_Org_Donations, 2),
               Max_Donation = max(Amount, na.rm = TRUE)) %>%
     ungroup() %>%
     arrange(desc(CY), desc(Total_Donation_Amount)); head(yearlyInd)

# Save
saveRDS(yearlyInd, paste(dir, "yearlyInd.rds", sep = ""))



# Industry to Employer Summary
yearlyIndEmp <- dfx %>% 
     filter(ContDesc != "Loan Proceeds") %>% 
     group_by(Employer,Industry,Industry2,CY) %>%
     summarise(Number_of_Donations = n_distinct(ContributionID),
               Donations_Under_100 = n_distinct(ContributionID[Amount < 100]),
               Donations_Over_100 = n_distinct(ContributionID[Amount >= 100]),
               Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
               Total_Donation_Amount = sum(Amount, na.rm = TRUE),
               Dollars = monify(Total_Donation_Amount),
               Num_PAC_Donations = n_distinct(ContributionID[PAC == "PAC"]),
               Num_Org_Donations = n_distinct(ContributionID[PAC == "Not PAC"]),
               Total_Donation_Amount = sum(Amount, na.rm = TRUE),
               PAC_Donation_Amount = sum(Amount[PAC == "PAC"], na.rm = TRUE),
               Org_Donation_Amount = sum(Amount[PAC == "Not PAC"], na.rm = TRUE),
               Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
               Avg_PAC_Donation_Amt = round(PAC_Donation_Amount / Num_PAC_Donations, 2),
               Avg_Org_Donation_Amt = round(Org_Donation_Amount / Num_Org_Donations, 2),
               Max_Donation = max(Amount, na.rm = TRUE)) %>%
     ungroup() %>%
     arrange(desc(CY), desc(Total_Donation_Amount)); head(yearlyIndEmp)

# Save
saveRDS(yearlyIndEmp, paste(dir, "yearlyIndEmp.rds", sep = ""))

# Overall Summary
yearlyDonationSummary <- dfx  %>% 
     #filter(ContDesc != "Loan Proceeds") %>% 
     #transform(Employer = str_to_title(Employer)) %>% 
     group_by(FullName,Employer,Industry,Industry2,OrganizationName,CY) %>%
     summarise(Number_of_Donations = n_distinct(ContributionID),
               Donations_Under_100 = n_distinct(ContributionID[Amount < 100]),
               Donations_Over_100 = n_distinct(ContributionID[Amount >= 100]),
               Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
               Total_Donation_Amount = sum(Amount, na.rm = TRUE),
               Dollars = monify(Total_Donation_Amount),
               Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
               Max_Donation = max(Amount, na.rm = TRUE)) %>% 
     ungroup() %>%
     arrange(desc(CY), 
             desc(Total_Donation_Amount)); head(yearlyDonationSummary)

# Save
saveRDS(yearlyDonationSummary, paste(dir, "yearlyDonationSummary.rds", sep = ""))


# Organization Summary
gov18Org <- dfx %>% 
     filter(OrganizationName %in% c("Allan W Fung","Gina M. Raimondo","Matt Brown","Matthew A Brown","Joseph A Trillo","Patricia L. Morgan","Giovanni Feroce","Spencer Dickinson")) %>% 
     filter(ContDesc != "Loan Proceeds") %>% 
     group_by(OrganizationName,CY) %>%
     summarise(Number_of_Donations = n_distinct(ContributionID),
               Donations_Under_100 = n_distinct(ContributionID[Amount < 100]),
               Donations_Over_100 = n_distinct(ContributionID[Amount >= 100]),
               Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
               Total_Donation_Amount = sum(Amount, na.rm = TRUE),
               Dollars = monify(Total_Donation_Amount),
               Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
               Max_Donation = max(Amount, na.rm = TRUE)) %>% 
     ungroup() %>%
     group_by(OrganizationName) %>% 
     mutate(Running = cumsum(Total_Donation_Amount)) %>% ungroup() %>% 
     arrange(desc(CY), desc(Total_Donation_Amount)); head(gov18Org)

# Save
saveRDS(gov18Org, paste(dir, "gov18.rds", sep = ""))


ginaVs <- dfx %>% 
     #filter(CY >= 2014 & CY <= 2018) %>% 
     mutate(Gina = ifelse(OrganizationName == "Gina M. Raimondo", "Governor\nRaimondo", "Everybody\nElse")) %>% 
     group_by(donor_region,Gina,CY) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)); head(ginaVs)

# Save
saveRDS(ginaVs, paste(dir, "ginaVs.rds", sep = ""))


# Time Series Data
timeSeries1 <- dfx %>% 
     filter(ContDesc != "Loan Proceeds") %>% 
     group_by(Month_Yr,CY) %>% 
     summarise(Amount = sum(Amount, na.rm= TRUE)) %>% 
     ungroup() %>% 
     mutate(Cumulative_Total = cumsum(Amount)) %>% 
     mutate(Loans = "Loans Excluded"); head(timeSeries1)

timeSeries2 <- dfx %>% 
     group_by(Month_Yr,CY) %>% 
     summarise(Amount = sum(Amount, na.rm= TRUE)) %>% 
     ungroup() %>% 
     mutate(Cumulative_Total = cumsum(Amount)) %>% 
     mutate(Loans = "Loans Included"); head(timeSeries2)

timeSeries3 <- dfx %>% 
     filter(ContDesc == "Loan Proceeds") %>% 
     group_by(Month_Yr,CY) %>% 
     summarise(Amount = sum(Amount, na.rm= TRUE)) %>% 
     ungroup() %>% 
     mutate(Cumulative_Total = cumsum(Amount)) %>% 
     mutate(Loans = "Loans Only"); head(timeSeries3)

timeSeries <- bind_rows(timeSeries1, timeSeries2, timeSeries3); rm(timeSeries1, timeSeries2, timeSeries3)

# # Time Series Data
# timeSeries <- dfx %>% 
#      group_by(Month_Yr,CY) %>% 
#      summarise(Amount = sum(Amount, na.rm= TRUE)) %>% 
#      ungroup() %>% 
#      mutate(Cumulative_Total = cumsum(Amount)); head(timeSeries)

# Save
saveRDS(timeSeries, paste(dir, "timeSeries.rds", sep = ""))

# ***************************************************************************************
# ***************************************************************************************

ggplot(gov18Org, aes(CY,Total_Donation_Amount)) +
     geom_point(size = 2, aes(color = OrganizationName), alpha = 0.5) +
     geom_line(stat = "identity", size = 1,  aes(group = OrganizationName, color = OrganizationName)) +
     scale_y_continuous(label = dollar) +
     labs(x = "", y = "Total Contributions Received",
          title = "Fundraising by Year",
          caption = "*Note the different y-axis for each candidate",
          subtitle = "2018 RI Candidates for Governor") +
     theme_bw() +
     theme(legend.position = "none",
           strip.text.x = element_text(margin = margin(0.05,0.05,0.05,0.05, "cm")),
           plot.caption = element_text(hjust = 0.5)) +
     facet_wrap(~OrganizationName, scales = "free_y", ncol = 1)



ggplot(gov18Org, aes(CY,Running)) +
     geom_point(size = 2, aes(color = OrganizationName), alpha = 0.5) +
     geom_line(stat = "identity", size = 1,  aes(group = OrganizationName, color = OrganizationName)) +
     scale_y_continuous(label = dollar) +
     labs(x = "", y = "Total Contributions Received",
          title = "Running Fundraising Total",
          caption = "*Note the different y-axis for each candidate",
          subtitle = "2018 RI Candidates for Governor") +
     theme_bw() +
     theme(legend.position = "none",
           strip.text.x = element_text(margin = margin(0.05,0.05,0.05,0.05, "cm")),
           plot.caption = element_text(hjust = 0.5)) +
     facet_wrap(~OrganizationName, scales = "free_y", ncol = 1)

ggplot(gov18Org, aes(CY,Running)) +
     geom_point(size = 2, aes(color = OrganizationName), alpha = 0.5) +
     geom_line(stat = "identity", size = 1,  aes(group = OrganizationName, color = OrganizationName)) +
     scale_y_continuous(label = dollar) +
     labs(x = "", y = "Total Contributions Received",
          title = "Running Fundraising Total",
          caption = "*Note the different y-axis for each candidate",
          subtitle = "2018 RI Candidates for Governor") +
     theme_bw() +
     theme(legend.position = "none",
           strip.text.x = element_text(margin = margin(0.05,0.05,0.05,0.05, "cm")),
           plot.caption = element_text(hjust = 0.5)) +
     facet_wrap(~OrganizationName, #scales = "free_y", 
                ncol = 1)



# Organization Summary
gov18_donorSt <- dfx %>% 
     filter(OrganizationName %in% c("Allan W Fung","Gina M. Raimondo","Matt Brown","Matthew A Brown","Joseph A Trillo","Patricia L. Morgan","Giovanni Feroce")) %>% 
     filter(ContDesc != "Loan Proceeds") %>% 
     filter(donor_region != "Rhode Island") %>% 
     group_by(OrganizationName,#City,donor_city,donor_st, donor_state_name,
              donor_region#,CY
     ) %>%
     summarise(Number_of_Donations = n_distinct(ContributionID),
               Donations_Under_100 = n_distinct(ContributionID[Amount < 100]),
               Donations_Over_100 = n_distinct(ContributionID[Amount >= 100]),
               Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
               Total_Donation_Amount = sum(Amount, na.rm = TRUE),
               Dollars = monify(Total_Donation_Amount),
               Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
               Max_Donation = max(Amount, na.rm = TRUE)) %>% 
     ungroup() %>%
     group_by(OrganizationName) %>% 
     mutate(GrandTotal = sum(Total_Donation_Amount)) %>% 
     ungroup() %>% 
     mutate(Pct_of_Total = round(Total_Donation_Amount / GrandTotal, 5)) %>% 
     group_by(OrganizationName,donor_region) %>% 
     mutate(Running_Donor_Region = cumsum(Total_Donation_Amount)) %>% ungroup() %>% 
     arrange(desc(Total_Donation_Amount)); head(gov18_donorSt)


gov18_donorSt %>% select(OrganizationName,#donor_state_name,
                         donor_region,Total_Donation_Amount,GrandTotal,Pct_of_Total) %>% 
     distinct()

ggplot(gov18_donorSt, aes(str_wrap(donor_region,13), Pct_of_Total)) +
     geom_bar(stat = "identity", aes(fill = donor_region)) +
     #geom_text(size = 3, aes(label = percent(Pct_of_Total))) +
     scale_y_continuous(label = percent) +
     theme_bw() +
     labs(x = "", y = "Percent of Total", 
          title = "Percent of Non-RI Donations",
          subtitle = "By State of Donor") +
     theme(strip.text.x = element_text(margin = margin(0.05,0.05,0.05,0.05, "cm")),
           legend.position = "none") +
     coord_polar() +
     facet_wrap(~OrganizationName)


ginaVs <- dfx %>% 
     filter(CY >= 2014 & CY <= 2018) %>% 
     mutate(Range = paste("Jan 1, ", min(CY), " to Dec 31, ", max(CY), sep = ""),
            Gina = ifelse(OrganizationName == "Gina M. Raimondo", "Governor\nRaimondo", "Everybody\nElse")) %>% 
     group_by(donor_region,Gina,Range) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)); head(ginaVs)

ginaVs <- dfx %>% 
     #filter(CY >= 2014 & CY <= 2018) %>% 
     mutate(Gina = ifelse(OrganizationName == "Gina M. Raimondo", "Governor\nRaimondo", "Everybody\nElse")) %>% 
     group_by(donor_region,Gina,CY) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)); head(ginaVs)

# Save
saveRDS(ginaVs, paste(dir, "ginaVs.rds", sep = ""))

# Gina vs the field
ginaVs <- readRDS(paste(dir, "ginaVs.rds", sep = ""))
ggplot(ginaVs, aes(Gina, Total)) + 
     geom_bar(stat = "identity", color = "gray50", width = 0.5, position = "dodge", aes(fill = Gina)) +
     scale_fill_manual(values = c("dodgerblue","navyblue")) +
     scale_y_continuous(label = dollar) +
     labs(x = "", y = "Donations Received", 
          title = "Rhode Island Political Donations Received by State of Donor",
          subtitle = unique(ginaVs$Range)) +
     #caption = paste(srce, "downloaded on Feb 9, 2017.  Donation 'ReceiptDate' between Jan 1, 2017 & Dec 31, 2017", sep = "\n")) +
     theme_bw() +
     theme(legend.position = "none", legend.background = element_rect(color = "gray50"),
           legend.title = element_blank(),
           plot.caption = element_text(hjust = 0.5)) +
     facet_wrap(~donor_region, scales = "free")


ginaVsInd <- dfx %>% 
     filter(CY >= 2014 & CY <= 2018) %>% 
     group_by(Industry) %>% mutate(indTot = sum(Amount, na.rm = TRUE)) %>% ungroup() %>% filter(indTot > 1000) %>% 
     mutate(Gina = ifelse(OrganizationName == "Gina M. Raimondo", "Governor\nRaimondo", "Everybody\nElse")) %>% 
     group_by(Industry,Gina) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)); head(ginaVs)

ggplot(ginaVsInd, aes(Gina, Total)) + 
     geom_bar(stat = "identity", color = "gray50", width = 0.5, position = "dodge", aes(fill = Gina)) +
     scale_fill_manual(values = c("dodgerblue","navyblue")) +
     scale_y_continuous(label = dollar) +
     labs(x = "", y = "Donations Received", 
          title = "Rhode Island Political Donations Received by State of Donor",
          subtitle = unique(ginaVs$Range)) +
     #caption = paste(srce, "downloaded on Feb 9, 2017.  Donation 'ReceiptDate' between Jan 1, 2017 & Dec 31, 2017", sep = "\n")) +
     theme_bw() +
     theme(legend.position = "bottom", legend.background = element_rect(color = "gray50"),
           legend.title = element_blank(),
           strip.text.x = element_text(margin = margin(0.05,0.05,0.05,0.05, "cm"), size = 8),
           axis.text.x = element_blank(),
           plot.caption = element_text(hjust = 0.5)) +
     facet_wrap(~Industry#, scales = "free"
     )



# summarise(Number_of_Donations = sum(Number_of_Donations, na.rm = TRUE),
#           Donations_Under_100 = sum(Donations_Under_100, na.rm = TRUE),
#           Donations_Over_100 = sum(Donations_Over_100, na.rm = TRUE),
#           Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
#           Total_Donation_Amount = sum(Total_Donation_Amount, na.rm = TRUE),
#           Dollars = monify(Total_Donation_Amount),
#           Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
#           Max_Donation = max(Max_Donation, na.rm = TRUE)) %>% 
#      ungroup() %>% 




# Save
saveRDS(yearlyOrg, paste(dir, "yearlyOrg.rds", sep = ""))


# timeSeries <- dfx %>% 
#      group_by(Month_Yr,CY) %>% 
#      summarise(Amount = sum(Amount, na.rm= TRUE)) %>% 
#      ungroup() 
# 
# timeSeries %>% filter(donor_state_name == "New York") %>% 
#      ggplot(aes(Month_Yr, Amount)) +
#      geom_line(stat = "identity", aes(group = 1)) +
#      #geom_vline(xintercept = emo$Loc, lty = 3, lwd = 0.8, color = "violetred") +
#      scale_y_continuous(label = dollar, breaks = seq(0,5000000,500000)) +
#      labs(x = "Donation Month", y = "Total Donations",
#           title = "Rhode Island Monthly Campaign Finance Donations",
#           subtitle = srce) +
#      #scale_x_discrete(breaks = xlabs) +
#      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

timeSeries <- readRDS(paste(dir, "timeSeries.rds", sep = ""))

# Time Series by Donor Location
timeSeriesLoc <- dfx %>% filter(!is.na(donor_city) & donor_city != "") %>% 
     group_by(Month_Yr,CY,City,donor_city,donor_st,donor_state_name) %>% 
     summarise(Amount = sum(Amount, na.rm= TRUE)) %>% 
     ungroup() %>% 
     group_by(City,donor_city,donor_st,donor_state_name) %>% 
     mutate(Cumulative_Total = cumsum(Amount)) %>% 
     ungroup(); head(timeSeriesLoc)

# Save
saveRDS(timeSeriesLoc, paste(dir, "timeSeriesLoc.rds", sep = ""))
timeSeriesLoc <- readRDS(paste(dir, "timeSeriesLoc.rds", sep = ""))

# Time Series by Employer Location
timeSeriesLocEmp <- dfx %>% filter(!is.na(emp_city) & !is.na(emp_st) & emp_city != "" & emp_state_name != "") %>% 
     group_by(Month_Yr,CY,Emp_City,emp_city,emp_st,emp_state_name) %>% 
     summarise(Amount = sum(Amount, na.rm= TRUE)) %>% 
     ungroup() %>% 
     group_by(Emp_City,emp_city,emp_st,emp_state_name) %>% 
     mutate(Cumulative_Total = cumsum(Amount)) %>% 
     ungroup(); head(timeSeriesLocEmp)

# Save
saveRDS(timeSeriesLocEmp, paste(dir, "timeSeriesLocEmp.rds", sep = ""))
timeSeriesLocEmp <- readRDS(paste(dir, "timeSeriesLocEmp.rds", sep = ""))


x <- yearlyLoc %>% transform(donor_city = str_to_title(donor_city), donor_st = str_to_upper(donor_st)) %>% 
     filter(CY >= 2017 & CY <= 2018) %>% 
     mutate(Range = paste("Jan 1, ", min(CY), " to Dec 31, ", max(CY), sep = ""),
            City = paste(donor_city, donor_st, sep = ", ")) %>% 
     group_by(donor_city, donor_st, City, donor_region,Range) %>%
     summarise(Donors = sum(Donors, na.rm = TRUE),
               Number_of_Donations = sum(Number_of_Donations, na.rm = TRUE),
               Num_PAC_Donations = sum(Num_PAC_Donations, na.rm = TRUE),
               Num_Org_Donations = sum(Num_Org_Donations, na.rm = TRUE),
               Total_Donation_Amount = sum(Total_Donation_Amount, na.rm = TRUE),
               PAC_Donation_Amount = sum(PAC_Donation_Amount, na.rm = TRUE),
               Org_Donation_Amount = sum(Org_Donation_Amount, na.rm = TRUE),
               Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
               Avg_PAC_Donation_Amt = round(PAC_Donation_Amount / Num_PAC_Donations, 2),
               Avg_Org_Donation_Amt = round(Org_Donation_Amount / Num_Org_Donations, 2)) %>%
     ungroup() %>% 
     arrange(desc(Total_Donation_Amount)) %>% 
     head(50); head(x)


ggplot(x, aes(reorder(City, Total_Donation_Amount), Total_Donation_Amount)) +
     geom_bar(stat = "identity", color = "gray50", width = 0.5, aes(fill = donor_st), alpha = 0.8) +
     geom_text(size = 3.1, fontface = "bold", aes(label = monify(Total_Donation_Amount)), hjust = -0.25) +
     scale_y_continuous(label = dollar, limits = c(0, round(max(x$Total_Donation_Amount) * 1.1, 0))) +
     labs(x = "", y = "Total", title = "Campaign Donations by City",
          subtitle = unique(x$Range)) +
     coord_flip() + theme_bw() +
     theme(axis.text.x = element_text(size = 12, face = "bold"), legend.title = element_blank(),
           legend.position = c(0.8,0.2), legend.background = element_rect(color = "gray50"),
           axis.text.y = element_text(size = 11, face = "bold"))

# Loans by Year
yearlyDonorLoans <- readRDS(paste(dir, "Yearly_Donor_Loans.rds", sep = ""))

# Donations by Year
yearlyDonorOrgs <- readRDS(paste(dir, "Yearly_Donor_Orgs.rds", sep = ""))

# Donations
yearlyDonorDonations <- readRDS(paste(dir, "yearlyDonorDonations.rds", sep = ""))

# Top Donors
yearlyTopDonations <- readRDS(paste(dir, "yearlyTopDonations.rds", sep = ""))

# Donations by Donor City, State
yearlyLoc <- readRDS(paste(dir, "yearlyLoc.rds", sep = ""))

# Donations by Employer City, State
yearlyLocEmp <- readRDS(paste(dir, "yearlyLocEmp.rds", sep = ""))
yearlyStateEmp <- readRDS(paste(dir, "yearlyStateEmp.rds", sep = ""))

# PAC Donations
yearlyPAC <- readRDS(paste(dir, "yearlyPAC.rds", sep = ""))

# Orgizations
yearlyOrg <- readRDS(paste(dir, "yearlyOrg.rds", sep = ""))

# By Employer
yearlyEmp <- readRDS(paste(dir, "yearlyEmp.rds", sep = ""))

# Employer to Organization
yearlyEmpOrg <- readRDS(paste(dir, "yearlyEmpOrg.rds", sep = ""))

# Donor to Pac
yearlyDonorPAC <- readRDS(paste(dir, "yearlyDonorPAC.rds", sep = ""))

# Donor & Donor Info to Organization
yearlyDonationSummary <- readRDS(paste(dir, "yearlyDonationSummary.rds", sep = ""))

# Top Donors
topDonors <- readRDS(paste(dir, "topDonors.rds", sep = ""))

# Time Series
timeSeries <- readRDS(paste(dir, "timeSeries.rds", sep = ""))

# Industry
yearlyInd <- readRDS(paste(dir, "yearlyInd.rds", sep = ""))
yearlyIndEmp <- readRDS(paste(dir, "yearlyIndEmp.rds", sep = ""))

# Gina vs the field
ginaVs <- readRDS(paste(dir, "ginaVs.rds", sep = ""))


# Shiny summarise donations
yearlyEmp %>% 
     filter(CY >= 2014 & CY <= 2015) %>%
     group_by(Employer,Industry,Industry2) %>% 
     summarise(Number_of_Donations = sum(Number_of_Donations, na.rm = TRUE),
               Donations_Under_100 = sum(Donations_Under_100, na.rm = TRUE),
               Donations_Over_100 = sum(Donations_Over_100, na.rm = TRUE),
               Pct_Under_100 = round(Donations_Under_100 / Number_of_Donations, 4),
               Total_Donation_Amount = sum(Total_Donation_Amount, na.rm = TRUE),
               Dollars = monify(Total_Donation_Amount),
               Num_PAC_Donations = sum(Num_PAC_Donations, na.rm = TRUE),
               Num_Org_Donations = sum(Num_Org_Donations, na.rm = TRUE),
               PAC_Donation_Amount = sum(PAC_Donation_Amount, na.rm = TRUE),
               Org_Donation_Amount = sum(Org_Donation_Amount, na.rm = TRUE),
               Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
               Avg_PAC_Donation_Amt = round(PAC_Donation_Amount / Num_PAC_Donations, 2),
               Avg_Org_Donation_Amt = round(Org_Donation_Amount / Num_Org_Donations, 2),
               Max_Donation = max(Max_Donation, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total_Donation_Amount))




# x <- yearlyLoc %>% 
#      #filter(ContDesc != "Loan Proceeds") %>% 
#      filter(CY >= 2014 & CY <= 2018) %>% 
#      group_by(donor_city, donor_st, donor_region,CY) %>%
#      summarise(Donors = sum(Donors, na.rm = TRUE),
#                Number_of_Donations = sum(Number_of_Donations, na.rm = TRUE),
#                Num_PAC_Donations = sum(Num_PAC_Donations, na.rm = TRUE),
#                Num_Org_Donations = sum(Num_Org_Donations, na.rm = TRUE),
#                Total_Donation_Amount = sum(Total_Donation_Amount, na.rm = TRUE),
#                PAC_Donation_Amount = sum(PAC_Donation_Amount, na.rm = TRUE),
#                Org_Donation_Amount = sum(Org_Donation_Amount, na.rm = TRUE),
#                Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
#                Avg_PAC_Donation_Amt = round(PAC_Donation_Amount / Num_PAC_Donations, 2),
#                Avg_Org_Donation_Amt = round(Org_Donation_Amount / Num_Org_Donations, 2)) %>%
#      ungroup() %>% 
#      arrange(desc(CY), desc(Total_Donation_Amount)) %>% 
#      mutate(rnum = row_number()); head(x)
# 

tx <- timeSeries %>%
     filter(CY >= 2016 & CY <= 2018) %>% 
     mutate(Range = paste("Jan 1, ", min(CY), " to Dec 31, ", max(CY), sep = "")) %>% 
     group_by(Month_Yr, Range) %>% 
     summarise(Total_Amount = sum(Amount, na.rm = TRUE)) %>%
     ungroup() %>% 
     mutate(Mo = ymd(gsub("$", "-01", Month_Yr))) %>% 
     mutate(Running = cumsum(Total_Amount)); head(tx); str(tx)

#numMos <- n_distinct(tx$Month_Yr); numMos

ggplot(tx, aes(Mo, Total_Amount)) +
     geom_point(size = 1, alpha = 0.5) +
     geom_line(stat = "identity", aes(group = 1)) +
     scale_y_continuous(label = dollar) +
     scale_x_date(breaks=pretty_breaks(), date_minor_breaks="1 month") + 
     theme(axis.text.x = element_text(size = 14),
           axis.text.y = element_text(size = 14))


yearlyDonorDonations %>% 
     filter(FullName == "Ruggerio, Dominick") %>% 
     filter(CY >= 2015 & CY <= 2018) %>% 
     group_by(FullName,OrganizationName,PAC,ContDesc) %>%
     summarise(Number_of_Donations = sum(Number_of_Donations, na.rm = TRUE),
               Total_Donation_Amount = sum(Total_Donation_Amount, na.rm = TRUE),
               Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
               Max_Donation = max(Max_Donation, na.rm = TRUE)) %>%
     ungroup() %>%
     arrange(desc(Total_Donation_Amount)) 

donDt <- dfx %>%
     filter(FullName == "Mattiello, Nicholas") %>%
     filter(CY >= 2002 & CY <= 2018) %>%
     mutate(Range = paste("Jan 1, ", min(CY), " to Dec 31, ", max(CY), sep = "")) %>%
     mutate(Mo = format(ReceiptDate, "%Y %m")) %>%
     group_by(FullName,Mo,ContDesc,Range) %>%
     summarise(Total = sum(Amount, na.rm = TRUE)) %>%
     ungroup() %>%
     arrange(Mo) %>% 
     mutate(Run = cumsum(Total)); head(donDt)


yearlyDonorOrgs %>% 
     filter(CY >= 2015 & CY <= 2018) %>% 
     group_by(FullName) %>%
     summarise(Number_of_Donations = sum(Number_of_Donations, na.rm = TRUE),
               Total_Donation_Amount = sum(Total_Donation_Amount, na.rm = TRUE),
               #Loan_Total_Amount = sum(Amount[ContDesc == "Loan Proceeds"], na.rm = TRUE),
               Avg_Donation_Amount = round(Total_Donation_Amount / Number_of_Donations, 2),
               Organizations_Donated_To = sum(Organizations_Donated_To, na.rm = TRUE),
               Org_Donations = sum(Org_Donations, na.rm = TRUE),
               Org_Donation_Amount = sum(Org_Donation_Amount, na.rm = TRUE),
               PACs_Donated_To = sum(PACs_Donated_To, na.rm = TRUE),
               PAC_Donations = sum(PAC_Donations, na.rm = TRUE),
               PAC_Donation_Amount = sum(PAC_Donation_Amount, na.rm = TRUE),
               Max_Donation = max(Max_Donation, na.rm = TRUE)) %>%
     ungroup() %>%
     arrange(desc(Total_Donation_Amount)) 


yearlyDonorLoans %>% 
     filter(CY >= 2002 & CY <= 2018) %>% 
     group_by(FullName) %>%
     summarise(Number_of_Loans = sum(Number_of_Loans, na.rm = TRUE),
               Total_Loan_Amount = sum(Total_Loan_Amount, na.rm = TRUE),
               Dollars = monify(Total_Loan_Amount),
               Avg_Loan_Amount = round(Total_Loan_Amount / Number_of_Loans, 2),
               Max_Loan_Amount = max(Max_Loan_Amount, na.rm = TRUE),
               Orgs_Receiving_Loans = paste(unique(sort(Orgs_Receiving_Loans)), collapse = " | ")) %>% 
     ungroup() %>%
     arrange(desc(Total_Loan_Amount)) 
# ggplot(yearlyDonorOrgs, aes(factor(CY), log(Total_Donation_Amount))) +
#      geom_jitter(size = 1, alpha = 0.7, aes(color = factor(CY))) +
#      geom_boxplot(aes(fill = factor(CY)), alpha = 0.5, color = "black") + theme_bw() +
#      labs(x = "Donation Year", y = "Total Donated", 
#           title = "Yearly Donations Boxplot") +
#      theme(legend.position = "none")


# x <- yearlyTopDonations %>% filter(CY == 2017) %>% 
#      select(FullName,OrganizationName,Total_Donation_Amount) %>% 
#      distinct(); head(x)
# 
# 
# xLabs <- monify(x$Total_Donation_Amount)
# 
# 
# qgraph(x, 
#        layout = "spring",
#        vsize = 5, # Size of the nodes
#        label.cex = 6, # Size of Node Labels
#        mar = c(1,1,1,1),
#        title = "Top Campaign Contributions 2017", 
#        edge.color = "gray50", # Line color
#        edge.width = 0.75, 
#        fade = FALSE,
#        edge.label.color = "black", # Color of the numeric labels
#        bg = "whitesmoke",
#        fade = TRUE,
#        vTrans = 100,
#        color = d$col, 
#        shape = d$shp,
#        #color = nodeLists$col,
#        #color = nList$col,
#        #shape = nodeLists$shp,
#        label.scale.equal = TRUE,
#        edge.label.cex = 0.6,
#        edge.label.position = 0.5,
#        edge.label.bg = "gray90",
#        edge.labels= xLabs)






# This will handle the ordering
d <- d %>% 
     ungroup() %>%   # As a precaution / handle in a separate .grouped_df method
     arrange(f, y) %>%   # arrange by facet variables and continuous values
     mutate(rnum = row_number()) # Add a row number variable

ggplot(d, aes(.r, y)) +  # Use .r instead of x
     geom_col() +
     facet_wrap(~ f, scales = "free") +  # Should free scales (though could be left to user)
     scale_x_continuous(  # This handles replacement of .r for x
          breaks = d$.r,     # notice need to reuse data frame
          labels = d$x
     )

yearlyLoc %>% 
     group_by(CY) %>% 
     top_n(Total_Donation_Amount, n = 7) %>% 
     ungroup() %>% 
     group_by(CY) %>% 
     mutate(Rnk = rank(desc(Total_Donation_Amount), ties.method = "first")) %>% ungroup() %>% 
     #ggplot(aes(reorder(paste(donor_city, donor_st, sep = ", "), Total_Donation_Amount), Total_Donation_Amount)) +
     ggplot(aes(paste(donor_city, donor_st, sep = ", "), Total_Donation_Amount)) +
     geom_bar(stat = "identity", width = 0.4, aes(fill = factor(CY))) +
     geom_text(size = 6, aes(label = paste("#", Rnk, sep = ""))) +
     scale_y_continuous(label = dollar) +
     coord_flip() +
     facet_wrap(~CY, scales = "free_y") +
     #scale_x_discrete(breaks = yearlyLoc$rnum) +
     theme_bw() +
     theme(legend.position = "none",
           axis.text.y = element_text(size = 14),
           axis.text.x = element_text(size = 14),
           strip.text.x = element_text(margin = margin(0.04,0.04,0.04,0.04, "cm"))) +
     NULL

yearlyLoc %>% 
     group_by(CY) %>% 
     top_n(Total_Donation_Amount, n = 10) %>% 
     ungroup() %>% 
     group_by(CY) %>% 
     mutate(Rnk = rank(desc(Total_Donation_Amount), ties.method = "first")) %>% ungroup() %>% 
     #ggplot(aes(reorder(paste(donor_city, donor_st, sep = ", "), Total_Donation_Amount), Total_Donation_Amount)) +
     ggplot(aes(factor(CY), Total_Donation_Amount)) +
     geom_bar(stat = "identity", width = 0.4, aes(fill = factor(CY))) +
     #geom_text(size = 3, aes(label = paste("#", Rnk, sep = ""))) +
     scale_y_continuous(label = dollar) +
     coord_flip() +
     facet_wrap(~donor_city, scales = "free_y") +
     #scale_x_discrete(breaks = yearlyLoc$rnum) +
     theme_bw() +
     theme(legend.position = "none",
           axis.text.y = element_text(size = 7),
           axis.text.x = element_text(size = 10),
           strip.text.x = element_text(margin = margin(0.04,0.04,0.04,0.04, "cm"))) +
     NULL


ggplot(x, aes(Pct_Under_100, log(Total_Donation_Amount))) +
     geom_point() + 
     geom_smooth()
