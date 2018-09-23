



library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(stringr)
library(ggplot2)
library(ggrepel)
library(viridis)
library(qgraph)


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



dir <- "/Users/jeffreyrichardson/Documents/Campaign_Finance/Psephology_App/"


dfx <- readRDS(paste(dir, "campaign_finance_2018-09-22.rds", sep = "")) %>% 
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

# Read in expenditures
exp <- readRDS(paste(dir, "Exp_Clean.rds", sep = ""))


donateVis <- dfx %>% #filter(dfx, OrganizationName %in% healthPACs) %>%
     #filter(ReceiptDate > "2016-12-31") %>%
     filter(ContDesc != "Loan Proceeds") %>%
     group_by(Employer,OrganizationName,CY) %>%
     summarise(Total = round(sum(Amount, na.rm = TRUE),0)) %>%
     ungroup() %>%
     arrange(desc(Total)) %>%
     filter(Total > 99); head(donateVis, 10)

# Save
saveRDS(donateVis, paste(dir, "donateVis.rds", sep = ""))
# #donateVis <- readRDS(paste(dir, "donateVis.rds", sep = ""))



# By Employer & Industry
viz_sub <- dfx %>%
     mutate(Loans = ifelse(ContDesc == "Loan Proceeds", "Loan", "Not Loan")) %>% 
     group_by(Employer,Industry,Loans,OrganizationName,CY) %>% 
     summarise(Total = round(sum(Amount, na.rm = TRUE),0)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     rename(from = Employer, to = OrganizationName, weight = Total) %>% 
     select(from,to,weight,everything()) %>% 
     mutate(View = "SubIndustry"); head(viz_sub, 10)

viz_ind <- dfx %>%
     mutate(Loans = ifelse(ContDesc == "Loan Proceeds", "Loan", "Not Loan")) %>% 
     group_by(Employer,Industry2,Loans,OrganizationName,CY) %>% 
     summarise(Total = round(sum(Amount, na.rm = TRUE),0)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     rename(from = Employer, to = OrganizationName, weight = Total, Industry = Industry2) %>% 
     select(from,to,weight,everything()) %>% 
     mutate(View = "Industry"); head(viz_ind, 10)

viz_emp <- bind_rows(viz_sub,viz_ind); head(viz_emp)

# Save
saveRDS(viz_emp, paste(dir, "viz_emp.rds", sep = ""))

# Clear
rm(viz_sub,viz_ind)



#donateVis <- readRDS(paste(dir, "donateVis.rds", sep = ""))

donateVisInd <- dfx %>% #filter(dfx, OrganizationName %in% healthPACs) %>% 
     #filter(ReceiptDate > "2016-12-31") %>% 
     filter(ContDesc != "Loan Proceeds") %>% 
     group_by(Employer,Industry2,OrganizationName,CY) %>% 
     summarise(Total = round(sum(Amount, na.rm = TRUE),0)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     group_by(Employer) %>% 
     mutate(Tot = sum(Total)) %>% 
     ungroup() %>% 
     filter(Tot > 999) %>% select(-Tot) %>% 
     rename(Industry = Industry2) %>% 
     mutate(Ind = "Industry"); head(donateVisInd, 10)

# Save
#saveRDS(donateVisInd, paste(dir, "donateVisInd.rds", sep = ""))
#donateVisInd <- readRDS(paste(dir, "donateVisInd.rds", sep = ""))

donateVisSubInd <- dfx %>% #filter(dfx, OrganizationName %in% healthPACs) %>% 
     #filter(ReceiptDate > "2016-12-31") %>% 
     filter(ContDesc != "Loan Proceeds") %>% 
     group_by(Employer,Industry,OrganizationName,CY) %>% 
     summarise(Total = round(sum(Amount, na.rm = TRUE),0)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     group_by(Employer) %>% 
     mutate(Tot = sum(Total)) %>% 
     ungroup() %>% 
     filter(Tot > 999) %>% select(-Tot) %>% 
     #rename(Industry = Industry2) %>% 
     mutate(Ind = "Sub Industry"); head(donateVisSubInd, 10)


donateVisInd <- bind_rows(donateVisInd, donateVisSubInd); rm(donateVisSubInd)
# Save
saveRDS(donateVisInd, paste(dir, "donateVisInd.rds", sep = ""))



donateVisOrg <- dfx %>% #filter(dfx, OrganizationName %in% healthPACs) %>% 
     #filter(ReceiptDate > "2016-12-31") %>% 
     filter(ContDesc != "Loan Proceeds") %>% 
     group_by(FullName,Industry2,OrganizationName,CY) %>% 
     summarise(Total = round(sum(Amount, na.rm = TRUE),0)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     group_by(FullName) %>% 
     mutate(Tot = sum(Total)) %>% 
     ungroup() %>% 
     filter(Tot > 99) %>% 
     select(-Tot) %>% 
     rename(Industry = Industry2) %>% 
     mutate(Ind = "Industry"); head(donateVisOrg, 10)

# Save
#saveRDS(donateVisOrg, paste(dir, "donateVisOrg.rds", sep = ""))
#donateVisOrg <- readRDS(paste(dir, "donateVisOrg.rds", sep = ""))

donateVisSubOrg <- dfx %>% #filter(dfx, OrganizationName %in% healthPACs) %>% 
     #filter(ReceiptDate > "2016-12-31") %>% 
     filter(ContDesc != "Loan Proceeds") %>% 
     group_by(FullName,Industry,OrganizationName,CY) %>% 
     summarise(Total = round(sum(Amount, na.rm = TRUE),0)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     group_by(FullName) %>% 
     mutate(Tot = sum(Total)) %>% 
     ungroup() %>% 
     filter(Tot > 99) %>% select(-Tot) %>% 
     #rename(Industry = Industry2) %>% 
     mutate(Ind = "Sub Industry"); head(donateVisSubOrg, 10)


donateVisOrg <- bind_rows(donateVisOrg, donateVisSubOrg) %>% arrange(Industry,FullName,CY); rm(donateVisSubOrg); head(donateVisOrg, 20)
# Save
saveRDS(donateVisOrg, paste(dir, "donateVisOrg.rds", sep = ""))

# *********************************************************************

fullInd <- dfx %>% filter(!Industry %in% c("Unavailable","Uncoded","Unemployed","Retired","Self Employed")) %>% 
     group_by(FullName,Industry) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(FullName,desc(Total)) %>% 
     group_by(FullName) %>% 
     mutate(Rnk = rank(desc(Total), ties.method = "first")) %>% 
     ungroup() %>% 
     filter(Rnk == 1) %>% 
     select(-c(Total,Rnk)); head(fullInd)

fullInd2 <- dfx %>% filter(!Industry %in% c("Unavailable","Uncoded")) %>% 
     filter(FullName != "") %>% 
     filter(!FullName %in% fullInd$FullName) %>% 
     group_by(FullName,Industry) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(FullName,desc(Total)) %>% 
     group_by(FullName) %>% 
     mutate(Rnk = rank(desc(Total), ties.method = "first")) %>% 
     ungroup() %>% 
     filter(Rnk == 1) %>% 
     select(-c(Total,Rnk)); head(fullInd2)

fullInd3 <- dfx %>% #filter(!Industry %in% c("Unavailable","Uncoded")) %>% 
     filter(FullName != "") %>% 
     filter(!FullName %in% fullInd$FullName & !FullName %in% fullInd2$FullName) %>% 
     group_by(FullName,Industry) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(FullName,desc(Total)) %>% 
     group_by(FullName) %>% 
     mutate(Rnk = rank(desc(Total), ties.method = "first")) %>% 
     ungroup() %>% 
     filter(Rnk == 1) %>% 
     select(-c(Total,Rnk)); head(fullInd3)

nameInd <- bind_rows(fullInd,fullInd2,fullInd3); rm(fullInd,fullInd2,fullInd3)


fullInd <- dfx %>% filter(!Industry2 %in% c("Unavailable","Uncoded","Unemployed","Retired","Self Employed")) %>% 
     group_by(FullName,Industry2) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(FullName,desc(Total)) %>% 
     group_by(FullName) %>% 
     mutate(Rnk = rank(desc(Total), ties.method = "first")) %>% 
     ungroup() %>% 
     filter(Rnk == 1) %>% 
     select(-c(Total,Rnk)); head(fullInd)

fullInd2 <- dfx %>% filter(!Industry2 %in% c("Unavailable","Uncoded")) %>% 
     filter(FullName != "") %>% 
     filter(!FullName %in% fullInd$FullName) %>% 
     group_by(FullName,Industry2) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(FullName,desc(Total)) %>% 
     group_by(FullName) %>% 
     mutate(Rnk = rank(desc(Total), ties.method = "first")) %>% 
     ungroup() %>% 
     filter(Rnk == 1) %>% 
     select(-c(Total,Rnk)); head(fullInd2)

fullInd3 <- dfx %>% #filter(!Industry %in% c("Unavailable","Uncoded")) %>% 
     filter(FullName != "") %>% 
     filter(!FullName %in% fullInd$FullName & !FullName %in% fullInd2$FullName) %>% 
     group_by(FullName,Industry2) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(FullName,desc(Total)) %>% 
     group_by(FullName) %>% 
     mutate(Rnk = rank(desc(Total), ties.method = "first")) %>% 
     ungroup() %>% 
     filter(Rnk == 1) %>% 
     select(-c(Total,Rnk)); head(fullInd3)

nameInd2 <- bind_rows(fullInd,fullInd2,fullInd3); rm(fullInd,fullInd2,fullInd3)

ind_lookup <- full_join(nameInd,nameInd2, by = "FullName")

# Clear
rm(nameInd,nameInd2)


vizFull <- dfx %>% #filter(dfx, OrganizationName %in% healthPACs) %>% 
     #filter(ReceiptDate > "2016-12-31") %>% 
     filter(ContDesc != "Loan Proceeds") %>% 
     group_by(FullName,OrganizationName,CY) %>% 
     summarise(Total = round(sum(Amount, na.rm = TRUE),0)) %>% 
     ungroup() %>% 
     left_join(ind_lookup, by = "FullName") %>% 
     arrange(desc(Total)); head(vizFull, 10)


# Save
saveRDS(vizFull, paste(dir, "vizFull.rds", sep = ""))


# Network Visualizations of Employers to Orgs
donateVis <- readRDS(paste(dir, "donateVis.rds", sep = ""))
donateVisInd <- readRDS(paste(dir, "donateVisInd.rds", sep = ""))

# Read in donor to org dataframe
donateVisOrg <- readRDS(paste(dir, "donateVisOrg.rds", sep = ""))





fullEmp <- dfx %>% filter(Employer != "") %>% 
     group_by(FullName,Employer) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(FullName,desc(Total)) %>% 
     group_by(FullName) %>% 
     mutate(Rnk = rank(desc(Total), ties.method = "first")) %>% 
     ungroup() %>% 
     filter(Rnk == 1) %>% 
     select(-c(Total,Rnk)); head(fullEmp)


empInd <- dfx %>% 
     group_by(Employer,Industry) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(Employer,desc(Total)) %>% 
     group_by(E); head(empInd)


fullInd2 <- dfx %>% filter(!Industry2 %in% c("Unavailable","Uncoded")) %>% 
     filter(FullName != "") %>% 
     filter(!FullName %in% fullInd$FullName) %>% 
     group_by(FullName,Industry2) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(FullName,desc(Total)) %>% 
     group_by(FullName) %>% 
     mutate(Rnk = rank(desc(Total), ties.method = "first")) %>% 
     ungroup() %>% 
     filter(Rnk == 1) %>% 
     select(-c(Total,Rnk)); head(fullInd2)

fullInd3 <- dfx %>% #filter(!Industry %in% c("Unavailable","Uncoded")) %>% 
     filter(FullName != "") %>% 
     filter(!FullName %in% fullInd$FullName & !FullName %in% fullInd2$FullName) %>% 
     group_by(FullName,Industry2) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(FullName,desc(Total)) %>% 
     group_by(FullName) %>% 
     mutate(Rnk = rank(desc(Total), ties.method = "first")) %>% 
     ungroup() %>% 
     filter(Rnk == 1) %>% 
     select(-c(Total,Rnk)); head(fullInd3)

#donateVisInd <- readRDS(paste(dir, "donateVisInd.rds", sep = ""))
# 
# x2 <- x %>% 
#      head(50) %>% 
#      transform(Employer = str_wrap(str_to_title(Employer), 12),
#                OrganizationName = str_wrap(str_to_title(OrganizationName), 14)) %>% 
#      group_by(OrganizationName) %>% 
#      mutate(edgeCol = sample(colors(), 1)) %>% 
#      ungroup(); head(x2)
#  eCol <- x2$edgeCol
# 
# x2 <- x2 %>% select(-edgeCol)
#  
#  d <- data.frame(c(unique(x2$Employer), unique(x2$OrganizationName)), stringsAsFactors = FALSE) %>% 
#      setNames("label") %>% 
#      mutate(col = ifelse(label %in% x2$Employer, "dodgerblue","darkorange"),
#             shp = ifelse(label %in% x2$Employer, "circle","triangle")); head(d)
# 
# d <- data.frame(label = c(x2$Employer, x2$OrganizationName), stringsAsFactors = FALSE) %>% distinct() %>% 
#      #setNames("label") %>% 
#      mutate(col = ifelse(label %in% x2$Employer, "dodgerblue","darkorange"),
#             shp = ifelse(label %in% x2$Employer, "circle","triangle")) %>% 
#      group_by(label) %>% 
#      mutate(edgeCol = sample(colors(), 1)) %>% 
#      ungroup(); head(d,20)
# 
# xLabs <- monify(x2$Total)
# 
# 
# qgraph(x2, 
#        layout = "spring",
#        vsize = 6, # Size of the nodes
#        label.cex = 6, # Size of Node Labels
#        mar = c(1,1,1,1),
#        title = "Political Alliances", 
#        edge.color = eCol,#x2$edgeCol,#"gray50", # Line color
#        edge.width = 0.75, 
#        fade = FALSE,
#        edge.label.color = "black", # Color of the numeric labels
#        bg = "smoke",
#        fade = TRUE,
#        vTrans = 100,
#        color = d$col, 
#        shape = d$shp,
#        #color = nodeLists$col,
#        #color = nList$col,
#        #shape = nodeLists$shp,
#        label.scale.equal = TRUE,
#        edge.label.cex = 0.9,
#        edge.label.position = 0.5,
#        edge.label.bg = "gray90",
#        edge.labels= xLabs)






