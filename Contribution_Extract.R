library(dplyr)
library(tidyr)
library(stringr)
library(scales)
library(ggplot2)
library(lubridate)
library(viridis)
library(qgraph)
library(ggrepel)


# Multiplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
     library(grid)
     plots <- c(list(...), plotlist) # Make a list from the ... arguments and plotlist
     numPlots = length(plots) # If layout is NULL, then use 'cols' to determine layout
     if (is.null(layout)) {
          layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),# Make the panel
                           ncol = cols, nrow = ceiling(numPlots/cols))# ncol: Number of columns of plots, nrow: Number of rows needed, calculated from # of cols
     }
     if (numPlots==1) {
          print(plots[[1]])
     } else {
          grid.newpage() # Set up the page
          pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
          for (i in 1:numPlots) { # Make each plot, in the correct location
               matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))  # Get the i,j matrix positions of the regions that contain this subplot
               print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                               layout.pos.col = matchidx$col))
          }}}

# monify
monify <- function(x) {
     library(scales)
     x <- as.numeric(x) # Make sure class of x is numeric
     suffix <- ifelse(x >= 1000000000, "B", # Assign x to appropriate suffix group
                      ifelse(x >= 1000000, "M",
                             ifelse (x <= 10000, "NA", "k")))
     x <- ifelse(suffix == "B", round(x / 1000000000, digits = 1), # Divide if necessary
                 ifelse(suffix == "M", round(x / 1000000, digits = 1),
                        ifelse(suffix == "k", comma(round(x / 1000, digits = 0)),
                               comma(round(x, digits = 0)))))
     x <- ifelse(suffix == "B", paste0(x, "B"), #Add letter to the end
                 ifelse(suffix == "M", paste0(x, "M"),
                        ifelse(suffix == "k", paste0(x, "k"),
                               paste0(x, ""))))
     x <- sub("^", "$", x) #Add $ to the beginning
     return(x) # Return the formatted object
}

# Read in saved data around line 3570

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

campaignStart <- Sys.time() %>% print

# # Websites with good industry info
# 
# # https://www.worldatlas.com/articles/which-are-the-biggest-industries-in-the-united-states.html
# # Top 19 Industries: 
# # Real estate, renting, leasing - contributes to the economy in two fronts; the first being through consumer spending through rent and payment of household utilities, and the other being through residential investment which encompasses the construction of new housing units, broker fees, and residential remodeling
# # state & local govt - Government spending is classified into two components government investment and government final consumption expenditure. Government investment is defined as the government spending used to finance projects with future or long-term benefits such as spending on research as well as spending on infrastructure. Government final consumption, on the other hand, is the government's spending on items for direct consumption
# # Finance & Insurance -  The Finance and Insurance industry is made up of four distinct sectors which include 
# #                  insurance carriers, credit intermediation and Federal Reserve banks, commodity contracts
# #                  and securities, and trusts and funds and other financial vehicles
# # Health/Social Care
# # Durable manufacturing -  characterized by long durations between purchases -  durable products such as computers, automobiles, firearms, sports equipment, house appliances, and aircraft
# # Retail Trade -  the final stage in the distribution of commodities to the final consumer
# # Wholesale Trade -  involves the bulk distribution of commodities from producers to retailers 
# # Non-durable manufacturing - all products with a lifespan of less than three years and include gasoline, 
# #                             electricity, and clothing among others
# # Federal govt
# # Information - companies and institutions which engage in the production, transmission, processing, storing, and selling of information which includes media companies, data processing companies, law firms, and telephone companies among others. 
# # Arts & Entertainment | Construction | Waste Services | Other Services | Utilities | Mining | Corporate Management | Education Services | Agriculture
# 
# # Bureau of Labor & Statistics
# 
# # https://www.bls.gov/emp/ep_table_201.htm
# # Industry Sectors
# # Sector
# # Nonagriculture Wage & Salary - Current Employment Statistics survey, except private households, which is from the Current Populations Survey. Logging workers are excluded.
# #    Goods Producing, Excluding Agriculture
# #         - Mining
# #         - Construction
# #         - Manufacturing
# #    Services-providing Excluding Special Industries
# #         - Utilities
# #         - Wholesale Trade
# #         - Retail Trade
# #         - Transportation & Warehousing
# #         - Information
# #         - Financial Activities
# #         - Professional Services & Business Services
# #         - Educational Services
# #         - Health care & social assistance
# #         - Leisure & hospitality
# #         - Other Services
# #         - Federal Government
# #         - State & Local Government
# 
# # Agriculture, forestry, fishing and hunting
# #         - Agriculture wage & salary
# #         - Agriculture self employed
# 
# # Nonagriculture self-employed
# 
# # https://www.bls.gov/emp/ep_table_201.htm
# # Sectors
# # 1. Nonagriculture Wage & Salary
# #     a) Goods Producing, Excluding Agriculture
# #     b) Services Producing, Excluding Special Industries
# # 2. Agriculture, Forestry, Fishing & Hunting
# # 3. Nonagriculture Self-Employed
# 
# # Industries
# # 1a. Mining, Construction, Manufacturing
# # 1b. Utilities, Wholesale trade, Retail trade, Transportation & Warehousing, Information,
# #    Financial Activities, Professional & Business Services, Educational Services, 
# #    Health Care & Social Assistance, Leisure & Hospitality, Other Services, Federal Govt, State & Local Govt
# # 2. Agriculture wage & salary, Agriculture Self-Employed
# 
# # https://en.wikipedia.org/wiki/Outline_of_industry#Industry_sectors
# # Aerospace, Agriculture (fishing, timber, tobacco), Chemical (Pharmaceutical), Computer (software),
# # Construction, Defense (Arms), Education, Energy (Electrical power & petroleum), Entertainment,
# # Financial Services (Insurance), Food (fruit), Health Care, Hospitality, Information, 
# # Manufacturing (Auto, electronic, pulp & paper, steel, shipbuilding), 
# # Mass Media (Broadcasting, film, jusic, news, publishing, www), Mining, Telecomm (Internet),
# # Transport, Water, Direct Selling
# 
# 
# # Also good https://www.bls.gov/opub/ted/2014/ted_20140728.htm
# 
# 
# # https://en.wikipedia.org/wiki/Outline_of_industry#Industry_sectors
# # Major Industries:
# # Aerospace
# # Agriculture - fishing, timber, tobacco
# # chemical  - Pharmaceutical
# # Computer - Software
# # Construction
# # Defense - Arms
# # Education
# # Energy - Electical power industry & petroleum industry
# # Entertainment
# # Financial Services - Insurance
# # Food - Fruit
# # Health care
# # Hospitality
# # Information
# # Manufacturing - Automotive | Electronics | Pulp & paper | Steel | Shipbuilding
# # Mass Media - Broadcasting | Film | Music | News | Publishing | World wide web
# # Mining
# # Telecommunications  - Internet
# # Transport
# # Water
# # Direct Selling
# 
# # https://en.wikipedia.org/wiki/Outline_of_industry#Industry_sectors
# # Aerospace, Agriculture (fishing, timber, tobacco), Chemical (Pharmaceutical), Computer (software),
# # Construction, Defense (Arms), Education, Energy (Electrical power & petroleum), Entertainment,
# # Financial Services (Insurance), Food (fruit), Health Care, Hospitality, Information, 
# # Manufacturing (Auto, electronic, pulp & paper, steel, shipbuilding), 
# # Mass Media (Broadcasting, film, jusic, news, publishing, www), Mining, Telecomm (Internet),
# # Transport, Water, Direct Selling
# 
# ***********************************************************************************
dir <- "//cdsfsp06/InterSite/Integra/analytics/Misc_Projects/Misc/Providence_Open_Data/2018-09-03/Contributions/"

a <- read.csv(paste(dir, "a.csv", sep = ""), colClasses = "character")
b <- read.csv(paste(dir, "b.csv", sep = ""), colClasses = "character")
c <- read.csv(paste(dir, "c.csv", sep = ""), colClasses = "character")
d <- read.csv(paste(dir, "d.csv", sep = ""), colClasses = "character")
e <- read.csv(paste(dir, "e.csv", sep = ""), colClasses = "character")
f <- read.csv(paste(dir, "f.csv", sep = ""), colClasses = "character")
g <- read.csv(paste(dir, "g.csv", sep = ""), colClasses = "character")
h <- read.csv(paste(dir, "h.csv", sep = ""), colClasses = "character")
i <- read.csv(paste(dir, "i.csv", sep = ""), colClasses = "character")
j <- read.csv(paste(dir, "j.csv", sep = ""), colClasses = "character")
k <- read.csv(paste(dir, "k.csv", sep = ""), colClasses = "character")
l <- read.csv(paste(dir, "l.csv", sep = ""), colClasses = "character")
m <- read.csv(paste(dir, "m.csv", sep = ""), colClasses = "character")
n <- read.csv(paste(dir, "n.csv", sep = ""), colClasses = "character")
o <- read.csv(paste(dir, "o.csv", sep = ""), colClasses = "character")
p <- read.csv(paste(dir, "p1.csv", sep = ""), colClasses = "character")
q <- read.csv(paste(dir, "q.csv", sep = ""), colClasses = "character")
r <- read.csv(paste(dir, "r.csv", sep = ""), colClasses = "character")
s <- read.csv(paste(dir, "s.csv", sep = ""), colClasses = "character")
t <- read.csv(paste(dir, "t.csv", sep = ""), colClasses = "character")
u <- read.csv(paste(dir, "u.csv", sep = ""), colClasses = "character")
v <- read.csv(paste(dir, "v.csv", sep = ""), colClasses = "character")
w <- read.csv(paste(dir, "w.csv", sep = ""), colClasses = "character")
x <- read.csv(paste(dir, "x.csv", sep = ""), colClasses = "character")
y <- read.csv(paste(dir, "y.csv", sep = ""), colClasses = "character")
z <- read.csv(paste(dir, "z.csv", sep = ""), colClasses = "character")


dat <- bind_rows(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) %>%
     select(-c(BeginDate,EndDate)) %>% distinct()

# Clear
rm(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z)

dir <- "//cdsfsp06/InterSite/Integra/analytics/Misc_Projects/Misc/Providence_Open_Data/"

# Read in the last data frame
df <- readRDS(paste(dir, "Donations_thru_Aug18.rds", sep = ""))

# Remove contributions
datminus <- dat %>%
     filter(!ContributionID %in% df$ContributionID)

# Combine
z <- df %>% bind_rows(datminus) %>% 
     transform(ReceiptDate = mdy(ReceiptDate),
               Amount = as.numeric(Amount),
               EmployerName = ifelse(is.na(EmployerName), "", EmployerName),
               MPFMatchAmount = as.numeric(MPFMatchAmount),
               DepositDate = mdy(DepositDate)) %>%
     mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>% 
     group_by(ContributionID) %>% 
     mutate(ContribTot = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% filter(ContribTot > 0) %>% select(-ContribTot) %>% 
     distinct(); dupf(z, "ContributionID")

# Save
saveRDS(z, paste(dir, "Donations_thru_Sep18.rds", sep = ""))


# # a <- read.csv(paste(dir, "a18.csv", sep = ""), colClasses = "character")
# b <- read.csv(paste(dir, "b18.csv", sep = ""), colClasses = "character")
# c <- read.csv(paste(dir, "c18.csv", sep = ""), colClasses = "character")
# d <- read.csv(paste(dir, "d18.csv", sep = ""), colClasses = "character")
# e <- read.csv(paste(dir, "e18.csv", sep = ""), colClasses = "character")
# f <- read.csv(paste(dir, "f18.csv", sep = ""), colClasses = "character")
# g <- read.csv(paste(dir, "g18.csv", sep = ""), colClasses = "character")
# h <- read.csv(paste(dir, "h18.csv", sep = ""), colClasses = "character")
# i <- read.csv(paste(dir, "i18.csv", sep = ""), colClasses = "character")
# j <- read.csv(paste(dir, "j18.csv", sep = ""), colClasses = "character")
# k <- read.csv(paste(dir, "k18a.csv", sep = ""), colClasses = "character")#KING RICHARD'S SUBARU Spelled Richard"s with a quote instead of apostrophe
# l <- read.csv(paste(dir, "l18.csv", sep = ""), colClasses = "character")
# m <- read.csv(paste(dir, "m18.csv", sep = ""), colClasses = "character")
# n <- read.csv(paste(dir, "n18.csv", sep = ""), colClasses = "character")
# o <- read.csv(paste(dir, "o18.csv", sep = ""), colClasses = "character")
# p1 <- read.csv(paste(dir, "p18a.csv", sep = ""), colClasses = "character") # O'Gara (Pannone Lopes Devereaux & O'Gara) was spelled O"Gara.  The quotes f'd everything up
# q <- read.csv(paste(dir, "q18.csv", sep = ""), colClasses = "character")
# r <- read.csv(paste(dir, "r18.csv", sep = ""), colClasses = "character")
# s <- read.csv(paste(dir, "s18.csv", sep = ""), colClasses = "character")
# t <- read.csv(paste(dir, "t18.csv", sep = ""), colClasses = "character")
# u <- read.csv(paste(dir, "u18.csv", sep = ""), colClasses = "character")
# v <- read.csv(paste(dir, "v18.csv", sep = ""), colClasses = "character")
# w <- read.csv(paste(dir, "w18.csv", sep = ""), colClasses = "character")
# x <- read.csv(paste(dir, "x18.csv", sep = ""), colClasses = "character")
# y <- read.csv(paste(dir, "y18.csv", sep = ""), colClasses = "character")
# z <- read.csv(paste(dir, "z18.csv", sep = ""), colClasses = "character")





# # Some files needed to be slightly altered (fixed) to allow import
# a <- read.csv(paste(dir, "a13a.csv", sep = ""), colClasses = "character")
# b <- read.csv(paste(dir, "b13.csv", sep = ""), colClasses = "character")
# #c <- read.csv(paste(dir, "c13.csv", sep = ""), colClasses = "character", comment.char="#")# Warning message: In scan(file = file, what = what, sep = sep, quote = quote, dec = dec,  :EOF within quoted string
# c <- read.csv(paste(dir, "c13a.csv", sep = ""), colClasses = "character")
# d <- read.csv(paste(dir, "d13.csv", sep = ""), colClasses = "character")
# e <- read.csv(paste(dir, "e13.csv", sep = ""), colClasses = "character")
# f <- read.csv(paste(dir, "f13.csv", sep = ""), colClasses = "character")
# g <- read.csv(paste(dir, "g13.csv", sep = ""), colClasses = "character")
# h <- read.csv(paste(dir, "h13.csv", sep = ""), colClasses = "character")
# i <- read.csv(paste(dir, "i13a.csv", sep = ""), colClasses = "character")# Warning message: In scan(file = file, what = what, sep = sep, quote = quote, dec = dec,  :EOF within quoted string
# j <- read.csv(paste(dir, "j13a.csv", sep = ""), colClasses = "character")# Warning message: In scan(file = file, what = what, sep = sep, quote = quote, dec = dec,  :EOF within quoted string
# k <- read.csv(paste(dir, "k13a.csv", sep = ""), colClasses = "character")# Warning message: In scan(file = file, what = what, sep = sep, quote = quote, dec = dec,  :EOF within quoted string
# l <- read.csv(paste(dir, "l13.csv", sep = ""), colClasses = "character")
# m <- read.csv(paste(dir, "m13.csv", sep = ""), colClasses = "character")
# n <- read.csv(paste(dir, "n13.csv", sep = ""), colClasses = "character")
# o <- read.csv(paste(dir, "o13.csv", sep = ""), colClasses = "character")
# p <- read.csv(paste(dir, "p13a.csv", sep = ""), colClasses = "character")# Warning message: In scan(file = file, what = what, sep = sep, quote = quote, dec = dec,  :EOF within quoted string
# q <- read.csv(paste(dir, "q13.csv", sep = ""), colClasses = "character")
# r <- read.csv(paste(dir, "r13a.csv", sep = ""), colClasses = "character")# Warning message: In scan(file = file, what = what, sep = sep, quote = quote, dec = dec,  :EOF within quoted string
# s <- read.csv(paste(dir, "s13.csv", sep = ""), colClasses = "character")
# t <- read.csv(paste(dir, "t13a.csv", sep = ""), colClasses = "character")# Warning message: In scan(file = file, what = what, sep = sep, quote = quote, dec = dec,  :EOF within quoted string
# u <- read.csv(paste(dir, "u13.csv", sep = ""), colClasses = "character")
# v <- read.csv(paste(dir, "v13.csv", sep = ""), colClasses = "character")
# w <- read.csv(paste(dir, "w13.csv", sep = ""), colClasses = "character")
# x <- read.csv(paste(dir, "x13.csv", sep = ""), colClasses = "character")
# y <- read.csv(paste(dir, "y13.csv", sep = ""), colClasses = "character")
# z <- read.csv(paste(dir, "z13.csv", sep = ""), colClasses = "character")
# 
# df13 <- bind_rows(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) %>% select(-X)
# 
# 
# dir <- "//cdsfsp06/InterSite/Integra/analytics/Misc_Projects/Misc/Providence_Open_Data/"
# files <- paste(letters, "18.csv", sep = ""); files
# 
# files[files == "k18.csv"] <- "k18a.csv"
# files[files == "p18.csv"] <- "p18a.csv"
# 
# dfList <- list()
# 
# for(i in 1:length(files)) {
# 
#      dat <- read.csv(paste(dir, files[i], sep = ""), colClasses = "character")
# 
#      dfList[[i]] <- dat
# 
# }
# 
# df0 <- bind_rows(dfList) %>% select(-c(BeginDate,EndDate)); str(df0)



# a0 <- read.csv(paste(dir, "a0.csv", sep = ""), colClasses = "character")
# b0 <- read.csv(paste(dir, "b0.csv", sep = ""), colClasses = "character")
# c0 <- read.csv(paste(dir, "c0a.csv", sep = ""), colClasses = "character")# Warning message: In scan(file = file, what = what, sep = sep, quote = quote, dec = dec,  :EOF within quoted string
# d0 <- read.csv(paste(dir, "d0.csv", sep = ""), colClasses = "character")
# e0 <- read.csv(paste(dir, "e0.csv", sep = ""), colClasses = "character")
# f0 <- read.csv(paste(dir, "f0.csv", sep = ""), colClasses = "character")
# g0 <- read.csv(paste(dir, "g0.csv", sep = ""), colClasses = "character")
# h0 <- read.csv(paste(dir, "h0.csv", sep = ""), colClasses = "character")
# i0 <- read.csv(paste(dir, "i0.csv", sep = ""), colClasses = "character")
# j0 <- read.csv(paste(dir, "j0.csv", sep = ""), colClasses = "character")
# k0 <- read.csv(paste(dir, "k0.csv", sep = ""), colClasses = "character")
# l0 <- read.csv(paste(dir, "l0.csv", sep = ""), colClasses = "character")
# m0 <- read.csv(paste(dir, "m0a.csv", sep = ""), colClasses = "character")# Warning message: In scan(file = file, what = what, sep = sep, quote = quote, dec = dec,  :EOF within quoted string
# n0 <- read.csv(paste(dir, "n0.csv", sep = ""), colClasses = "character")
# o0 <- read.csv(paste(dir, "o0.csv", sep = ""), colClasses = "character")
# p0 <- read.csv(paste(dir, "p0.csv", sep = ""), colClasses = "character")
# q0 <- read.csv(paste(dir, "q0.csv", sep = ""), colClasses = "character")
# r0 <- read.csv(paste(dir, "r0.csv", sep = ""), colClasses = "character")
# s0 <- read.csv(paste(dir, "s0.csv", sep = ""), colClasses = "character")
# t0 <- read.csv(paste(dir, "t0.csv", sep = ""), colClasses = "character")
# u0 <- read.csv(paste(dir, "u0.csv", sep = ""), colClasses = "character")
# v0 <- read.csv(paste(dir, "v0.csv", sep = ""), colClasses = "character")
# w0 <- read.csv(paste(dir, "w0.csv", sep = ""), colClasses = "character")
# x0 <- read.csv(paste(dir, "x0.csv", sep = ""), colClasses = "character")
# y0 <- read.csv(paste(dir, "y0.csv", sep = ""), colClasses = "character")
# z0 <- read.csv(paste(dir, "z0.csv", sep = ""), colClasses = "character")
# 
# 
# df0 <- bind_rows(a0,b0,c0,d0,e0,f0,g0,h0,i0,j0,k0,l0,m0,n0,o0,p0,q0,r0,s0,t0,u0,v0,w0,x0,y0,z0)
# 
# 
# df <- bind_rows(df0, df13)
# 
# # Save
# write.csv(df, paste(dir, "Donations_2018-06-06.csv", sep = ""), na = "", row.names = FALSE)
# 
# files <- paste(letters, "0.csv", sep = ""); files
# 
# 
# dfList <- list()
# 
# for(i in 1:length(files)) {
#      
#      df <- files[i]
#      
#      dfList[[i]] <- df
#      
# }
# 
# df0_2 <- bind_rows(dfList)
# 
# 
# for(i in 1:length(files)) {
#      
#      df <- read.csv(paste(dir, files[i], sep = ""), colClasses = "character")
#      
#      if(i == 1) {
#           
#           dfNew <- df
#      } else {
#           
#           dfNew <- bind_rows(dfNew, df)
#      }
#      
# }
# 
# df1 <- read.csv(paste(dir, files[1], sep = ""), quote = "", colClasses = "character")
# df1 <- read.csv(paste(dir, files[1], sep = ""))

# 
# wd <- "/Users/jeffreyrichardson/Documents/Campaign_Finance/"
# 
# 
# a <- read.csv(paste(dir, "a.csv", sep = ""), colClasses = "character")
# b <- read.csv(paste(dir, "b.csv", sep = ""), colClasses = "character")
# c <- read.csv(paste(dir, "c.csv", sep = ""), colClasses = "character")
# d <- read.csv(paste(dir, "d.csv", sep = ""), colClasses = "character")
# e <- read.csv(paste(dir, "e.csv", sep = ""), colClasses = "character")
# f <- read.csv(paste(dir, "f.csv", sep = ""), colClasses = "character")
# g <- read.csv(paste(dir, "g.csv", sep = ""), colClasses = "character")
# h <- read.csv(paste(dir, "h.csv", sep = ""), colClasses = "character")
# i <- read.csv(paste(dir, "i.csv", sep = ""), colClasses = "character")
# j <- read.csv(paste(dir, "j.csv", sep = ""), colClasses = "character")
# k <- read.csv(paste(dir, "k.csv", sep = ""), colClasses = "character")
# l <- read.csv(paste(dir, "l.csv", sep = ""), colClasses = "character")
# m <- read.csv(paste(dir, "m.csv", sep = ""), colClasses = "character")
# n <- read.csv(paste(dir, "n.csv", sep = ""), colClasses = "character")
# o <- read.csv(paste(dir, "o.csv", sep = ""), colClasses = "character")
# p <- read.csv(paste(dir, "p.csv", sep = ""), colClasses = "character")
# q <- read.csv(paste(dir, "q.csv", sep = ""), colClasses = "character")
# r <- read.csv(paste(dir, "r.csv", sep = ""), colClasses = "character")
# s <- read.csv(paste(dir, "s.csv", sep = ""), colClasses = "character")
# t <- read.csv(paste(dir, "t.csv", sep = ""), colClasses = "character")
# u <- read.csv(paste(dir, "u.csv", sep = ""), colClasses = "character")
# v <- read.csv(paste(dir, "v.csv", sep = ""), colClasses = "character")
# w <- read.csv(paste(dir, "w.csv", sep = ""), colClasses = "character")
# x <- read.csv(paste(dir, "x.csv", sep = ""), colClasses = "character")
# y <- read.csv(paste(dir, "y.csv", sep = ""), colClasses = "character")
# z <- read.csv(paste(dir, "z.csv", sep = ""), colClasses = "character")
# 
# 
# 
# df <- bind_rows(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) 
# 
# # Clear
# rm(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z)


# ri_senators <- c("jack reed","sheldon whitehouse")
# ri_congress <- c("james langevin", "david cicilline")
# 
# ri_gov <- "gina raimondo"
# ri_ltGov <- "dan mckee"
# ri_secState <- "nellie gorbea"
# ri_genTreasure <- "seth magaziner"
# ri_AG <- "peter kilmartin"


# srce <- "http://www.ricampaignfinance.com/RIPublic/Filings.aspx"
# 
# 
# dir <- "//cdsfsp06/InterSite/Integra/analytics/Misc_Projects/Misc/Providence_Open_Data/"
#file1 <- "Donations_2018-02-08.csv"
# file1 <- "Donations_2018-06-06.csv"
#file2 <- "RI_PoliticalAffiliation.csv"
# 
# # Read in the political donations file
#pAff <- read.csv(paste(dir, file2, sep = ""), colClasses = "character")

# file1 <- "Donations_2018-06-06_Clean.csv"
# 
# 
# # Read in the election.ri data
# df <- read.csv(paste(dir, file1, sep = ""), colClasses = "character") %>%
#      distinct() %>%
#      select(-c(BeginDate,EndDate))
# 
# aug <- read.csv(paste(dir, "August2018_Export_Cleaned.csv", sep = ""), colClasses = "character") %>% 
#      distinct()
# 
# #df <- bind_rows(df, df0) %>% distinct()
# donations <- bind_rows(df, aug) %>% distinct()
# 
# dupf(donations, "ContributionID")
# 
# 
# dup1 <- dup %>% group_by(ContributionID) %>% slice(which.min(nchar(IncompleteDesc))) %>% ungroup()
# 
# donate <- donations %>% 
#      filter(!ContributionID %in% dup1$ContributionID) %>% 
#      bind_rows(dup1)
# 
# # Save
# saveRDS(donate, paste(dir, "Donations_thru_Aug18.rds", sep = ""))
# 
dir <- "//cdsfsp06/InterSite/Integra/analytics/Misc_Projects/Misc/Providence_Open_Data/"

#srce <- "http://www.ricampaignfinance.com/RIPublic/Filings.aspx"

#file2 <- "RI_PoliticalAffiliation.csv"
 
# Read in the political donations file
#pAff <- read.csv(paste(dir, file2, sep = ""), colClasses = "character")

df <- readRDS(paste(dir, "Donations_thru_Aug18.rds", sep = ""))


#dat <- readRDS(paste(dir, "August2018_Export.rds", sep = ""))

# Save
#saveRDS(df, paste(dir, "campaign_finance_2018-06.rds", sep = ""))
#saveRDS(df, paste(dir, "campaign_finance_2018-08.rds", sep = ""))

# Clear
#rm(df0)


# Read in saved data
#df <- readRDS(paste(dir, "campaign_finance_2018-06.rds", sep = ""))
df <- readRDS(paste(dir, "campaign_finance_2018-08.rds", sep = "")) %>% distinct()



# badEmp <- grep("bad employer", df$IncompleteDesc, value = TRUE, ignore.case = TRUE) %>% unique(); badEmp
# 
# filter(df, IncompleteDesc %in% badEmp) %>% 
#      group_by(EmployerName) %>% tally() %>% ungroup() %>% arrange(desc(n)) %>% View("qq")
# 
# filter(df, IncompleteDesc %in% badEmp) %>% 
#      group_by(FullName) %>% tally() %>% ungroup() %>% arrange(desc(n)) %>% View("qq")

# Format
dfx <- df %>% 
     #filter(ViewIncomplete == "Complete") %>% 
     mutate(Donor_Name_Unformatted = FullName) %>% 
     transform(Amount = as.numeric(Amount),
               ReceiptDate = mdy(ReceiptDate),
               DepositDate = mdy(DepositDate),
               Address = str_to_lower(Address),
               EmpAddress = str_to_lower(EmpAddress),
               EmployerName = str_to_lower(EmployerName),
               FirstName = str_to_lower(FirstName),
               LastName = str_to_lower(LastName),
               CityStZip = str_to_lower(CityStZip),
               EmpCityStZip = str_to_lower(EmpCityStZip),
               FullName = str_to_lower(FullName),
               OrganizationName = str_to_lower(OrganizationName)) %>% 
     transform(Address = gsub("\\.", "", Address),
               EmpAddress = gsub("\\.", "", EmpAddress)) %>% 
     #mutate(Donor = paste(FirstName, LastName, sep = " ")) %>%  
     # Format Employer City
     transform(EmpCityStZip = gsub("^, |^,", "", EmpCityStZip)) %>% 
     transform(EmpCityStZip = gsub("^cf, ri", "central falls, ri", EmpCityStZip)) %>% 
     transform(EmpCityStZip = gsub("^ns, ri", "north smithfield, ri", EmpCityStZip)) %>% 
     transform(EmpCityStZip = gsub("^bi, ri", "block island, ri", EmpCityStZip)) %>% 
     transform(EmpCityStZip = gsub("^ny, ny", "new york, ny", EmpCityStZip)) %>% 
     transform(EmpCityStZip = gsub("^np, ri", "north providence, ri", EmpCityStZip)) %>% 
     transform(EmpCityStZip = gsub("^ep, ri", "east providence, ri", EmpCityStZip)) %>% 
     transform(EmpCityStZip = gsub("^ww, ri", "west warwick, ri", EmpCityStZip)) %>% 
     transform(EmpCityStZip = gsub("^eg, ri", "east greenwich, ri", EmpCityStZip)) %>% 
     transform(EmpCityStZip = gsub("^wg, ri", "west greenwich, ri", EmpCityStZip)) %>% 
     transform(EmpCityStZip = gsub("^nk, ri", "north kingstown, ri", EmpCityStZip)) %>% 
     transform(EmpCityStZip = gsub("^wk, ri", "west kingstown, ri", EmpCityStZip)) %>% 
     transform(EmpCityStZip = gsub("^sk, ri", "south kingston, ri", EmpCityStZip)) %>% 
     transform(EmpCityStZip = gsub("^jo, ri", "johnston, ri", EmpCityStZip)) %>% 
     transform(EmpCityStZip = gsub("^ww, wi", "west warwick, ri", EmpCityStZip)) %>% 
     transform(EmpCityStZip = gsub("^nh, ct", "new haven, ct", EmpCityStZip)) %>% 
     transform(EmpCityStZip = gsub("^nl, ct", "new london, ct", EmpCityStZip)) %>% 
     transform(EmpCityStZip = gsub("^la, ca", "los angeles, ca", EmpCityStZip)) %>% 
     transform(EmpCityStZip = gsub(",[a-zA-Z0-9]", ", ", EmpCityStZip)) %>% 
     transform(EmpCityStZip = gsub("\\s{2,}", " ", EmpCityStZip)) %>% 
     
     # Format Donor City
     transform(CityStZip = gsub("^, |^,", "", CityStZip)) %>% 
     transform(CityStZip = gsub("^cf, ri", "central falls, ri", CityStZip)) %>% 
     transform(CityStZip = gsub("^ns, ri", "north smithfield, ri", CityStZip)) %>% 
     transform(CityStZip = gsub("^bi, ri", "block island, ri", CityStZip)) %>% 
     transform(CityStZip = gsub("^ny, ny", "new york, ny", CityStZip)) %>% 
     transform(CityStZip = gsub("^np, ri", "north providence, ri", CityStZip)) %>% 
     transform(CityStZip = gsub("^ep, ri", "east providence, ri", CityStZip)) %>% 
     transform(CityStZip = gsub("^ww, ri", "west warwick, ri", CityStZip)) %>% 
     transform(CityStZip = gsub("^eg, ri", "east greenwich, ri", CityStZip)) %>% 
     transform(CityStZip = gsub("^wg, ri", "west greenwich, ri", CityStZip)) %>% 
     transform(CityStZip = gsub("^nk, ri", "north kingstown, ri", CityStZip)) %>% 
     transform(CityStZip = gsub("^wk, ri", "west kingstown, ri", CityStZip)) %>% 
     transform(CityStZip = gsub("^sk, ri", "south kingston, ri", CityStZip)) %>% 
     transform(CityStZip = gsub("^jo, ri", "johnston, ri", CityStZip)) %>% 
     transform(CityStZip = gsub("^ww, wi", "west warwick, ri", CityStZip)) %>% 
     transform(CityStZip = gsub("^nh, ct", "new haven, ct", CityStZip)) %>% 
     transform(CityStZip = gsub("^nl, ct", "new london, ct", CityStZip)) %>% 
     transform(CityStZip = gsub("^la, ca", "los angeles, ca", CityStZip)) %>% 
     transform(CityStZip = gsub("^ma, ma 02142", "cambridge, ma 02142", CityStZip)) %>% 
     transform(CityStZip = gsub(",[a-zA-Z0-9]", ", ", CityStZip)) %>% 
     transform(CityStZip = gsub("\\s{2,}", " ", CityStZip)) %>% 
     separate(CityStZip, c("donor_city","statezip"), sep = ", ", extra = "merge", remove = FALSE) %>% 
     separate(statezip, c("donor_st", "donor_zip"), sep = "\\s", extra = "merge") %>% transform(donor_st = gsub(",","",donor_st)) %>% 
     separate(donor_zip, c("donor_zip","d_zip2"), sep = "-") %>% 
     separate(EmpCityStZip, c("emp_city","statezip"), sep = ", ", extra = "merge", remove = FALSE) %>% 
     separate(statezip, c("emp_st", "emp_zip"), sep = "\\s", extra = "merge") %>% transform(emp_st = gsub(",","", emp_st)) %>% 
     separate(emp_zip, c("emp_zip","e_zip2"), sep = "-") %>% 
     transform(emp_st = gsub("02886|greenwich|^r$|^rh$|^pl$|^37|^np$|^71|^nk$|^xx|^ff|^of$|^dr", "ri", emp_st)) %>% 
     transform(emp_st = gsub("122001", "india", emp_st)) %>% 
     transform(emp_st = gsub("^bm$", "bermuda", emp_st)) %>% 
     transform(emp_st = gsub("^mq$", "ma", emp_st)) %>% 
     transform(emp_st = gsub("^77|^nt", "ny", emp_st)) %>% 
     transform(emp_st = gsub("^nb$", "ne", emp_st)) %>% 
     transform(emp_st = gsub("^qc$|^on$", "canada", emp_st)) %>% 
     #transform(emp_st = gsub("", "", emp_st)) %>% 
     #transform(emp_st = gsub("", "", emp_st)) %>% 
     transform(donor_st = gsub("02906|providence", "ri", donor_st)) %>% 
     transform(donor_st = gsub("81620", "co", donor_st)) %>% 
     transform(donor_st = gsub("ii", "il", donor_st)) %>% 
     transform(donor_st = gsub("^r$|^ei$|erland|^ro$", "ri", donor_st)) %>% 
     transform(donor_st = gsub("bm", "bermuda", donor_st)) %>% 
     transform(donor_st = gsub("ew|queens", "ny", donor_st)) %>% 
     #transform(donor_st = gsub("", "", donor_st)) %>% 
     transform(FullName = gsub("\\.", "", FullName)) %>% 
     transform(FullName = gsub(" md,", ",", FullName)) %>% 
     transform(FullName = gsub(",[a-zA-Z0-9]", ", ", FullName)) %>% 
     transform(FullName = gsub("\\s{2,}", " ", FullName)) %>% 
     transform(FullName = gsub(",{2,}", ",", FullName)) %>% 
     transform(EmployerName = gsub("\\(|\\)", "", EmployerName)) %>% 
     transform(EmployerName = gsub("\\,|\\.", "", EmployerName)) %>% 
     transform(EmployerName = gsub("\\&", "and", EmployerName)) %>% 
     transform(EmployerName = gsub(",[a-zA-Z0-9]", ", ", EmployerName)) %>% 
     transform(EmployerName = gsub("\\s{2,}", " ", EmployerName)) %>% 
     transform(EmployerName = gsub(",{2,}", ",", EmployerName)) %>% 
     transform(EmployerName = gsub("self-employed", "self employed", EmployerName)) %>%      
     transform(EmployerName = gsub("not-employed; retired", "retired", EmployerName)) %>% 
     transform(EmployerName = gsub("not-employed", "not employed", EmployerName)) %>% 
     #transform(EmployerName = gsub("\\s-[[:alpha:]]", " ", EmployerName)) %>% 
     #transform(EmployerName = gsub(" - ", " ", EmployerName)) %>% 
     transform(EmployerName = gsub("\\'", "", EmployerName)) %>% 
     transform(EmployerName = gsub(" / ", " ", EmployerName)) %>% 
     transform(EmployerName = gsub(" - ", " ", EmployerName)) %>% 
     transform(OrganizationName = gsub("/", "", OrganizationName),
               EmployerName = gsub("rhode island|rhode islasnd", "ri", EmployerName)) %>% 
     transform(EmployerName = gsub("\\s\\s", " ", EmployerName)) %>% 
     left_join(pAff, by = "OrganizationName") %>% 
     transform(Party = ifelse(Party == "" & PAC == "Y", "P",
                              ifelse(Party == "" & PAC != "Y", "U", Party))) %>% 
     mutate(Employer = "",
            Industry = "", 
            Industry2 = "",
            CY = year(ReceiptDate), Mo = strtrim(months(ReceiptDate), 3)) %>% 
     transform(Mo = factor(Mo, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))) %>% 
     mutate(Month_Yr =  format(as.Date(ReceiptDate), "%Y-%m")) %>% 
     mutate(Election_Yr = ifelse(CY %in% c(2002,2006,2010,2014,2018), "Election Year", "Non-Election Year"),
            Election_Mo = ifelse(Month_Yr %in% c("2002-11","2006-11","2010-11","2014-11","2018-11"), "Election Month", "Non-Election Month")) %>% 
     filter(!is.na(Amount)); table(dfx$Party)





# *************************************************************************************************************************
# ******************************************                      *********************************************************
# ******************************************   Format Addresses   *********************************************************
# ******************************************                      *********************************************************
# *************************************************************************************************************************


dfx$Address <- gsub(" street$", " st", dfx$Address)
dfx$Address <- gsub(" drive$", " dr", dfx$Address)
dfx$Address <- gsub(" avenue$", " ave", dfx$Address)
dfx$Address <- gsub(" av$", " ave", dfx$Address)
dfx$Address <- gsub(" road$", " rd", dfx$Address)
dfx$Address <- gsub(" court$", " ct", dfx$Address)
dfx$Address <- gsub(" circle$", " cir", dfx$Address)
dfx$Address <- gsub(" trail$", " trl", dfx$Address)
dfx$Address <- gsub(" lane$", " ln", dfx$Address)
dfx$Address <- gsub(" boulevard$", " blvd", dfx$Address)
dfx$Address <- gsub(" terrace$", " terr", dfx$Address)
dfx$Address <- gsub(" ter$", " terr", dfx$Address)
dfx$Address <- gsub(" parkway$", " pkwy", dfx$Address)
dfx$Address <- gsub(" place$", " pl", dfx$Address)
dfx$Address <- gsub(" highway$", " hwy", dfx$Address)
dfx$Address <- gsub(" turnpike$", " tpke", dfx$Address)

dfx$EmpAddress <- gsub(" street$", " st", dfx$EmpAddress)
dfx$EmpAddress <- gsub(" drive$", " dr", dfx$EmpAddress)
dfx$EmpAddress <- gsub(" avenue$", " ave", dfx$EmpAddress)
dfx$EmpAddress <- gsub(" av$", " ave", dfx$EmpAddress)
dfx$EmpAddress <- gsub(" road$", " rd", dfx$EmpAddress)
dfx$EmpAddress <- gsub(" court$", " ct", dfx$EmpAddress)
dfx$EmpAddress <- gsub(" circle$", " cir", dfx$EmpAddress)
dfx$EmpAddress <- gsub(" trail$", " trl", dfx$EmpAddress)
dfx$EmpAddress <- gsub(" lane$", " ln", dfx$EmpAddress)
dfx$EmpAddress <- gsub(" boulevard$", " blvd", dfx$EmpAddress)
dfx$EmpAddress <- gsub(" terrace$", " terr", dfx$EmpAddress)
dfx$EmpAddress <- gsub(" ter$", " terr", dfx$EmpAddress)
dfx$EmpAddress <- gsub(" parkway$", " pkwy", dfx$EmpAddress)
dfx$EmpAddress <- gsub(" place$", " pl", dfx$EmpAddress)
dfx$EmpAddress <- gsub(" highway$", " hwy", dfx$EmpAddress)
dfx$EmpAddress <- gsub(" turnpike$", " tpke", dfx$EmpAddress)




# *************************************************************************************************************************
# ******************************************                            ***************************************************
# ******************************************   Format Cities & States   ***************************************************
# ******************************************                            ***************************************************
# *************************************************************************************************************************


# Format RI Employer Cities
dfx$emp_city <- gsub("\\-[0-9]{1,}", "", dfx$emp_city); head(dfx$emp_city)
dfx$emp_city <- gsub(", [a-zA-Z]{2}$", "", dfx$emp_city); head(dfx$emp_city)
dfx$emp_city <- gsub(",|\\.|\\/|\\;|'|`|\\:|_|\\?|\\[|\\*|\\\t|\\|\\^|\\-{1,}", " ", dfx$emp_city); head(dfx$emp_city)
dfx$emp_city <- gsub("\\", "", dfx$emp_city, fixed = TRUE); head(dfx$emp_city)
dfx$emp_city <- gsub("\\s{2,}", " ", dfx$emp_city)
dfx$emp_city <- gsub(" ri$", "", dfx$emp_city)
dfx$emp_city <- trimws(dfx$emp_city)
dfx$emp_city <- gsub("^n a$", "na", dfx$emp_city)
dfx$emp_city <- gsub("^e |^eat |^easty |^easr |^esat |^est |^eart |^eas |^easst |^eeast |^eats |^eaat |^eaast |^rast ", "east ", dfx$emp_city)
dfx$emp_city <- gsub("^w |^w est |^werst |^wet |^wwae |^wast |^wesk |^westw ", "west ", dfx$emp_city)
dfx$emp_city <- gsub("^s |^so |^ssouth |^sout ", "south ", dfx$emp_city)
dfx$emp_city <- gsub("^n |^no |^norht |^norh |^norrth |^nothr |^nor |^noerh |^niorth |^noirth |^norrh |^nort |^noth |^nroth ", "north ", dfx$emp_city)
dfx$emp_city <- gsub("^ashway$|^ahaway$|^asaway$|^shaway$", "ashaway", dfx$emp_city)
dfx$emp_city <- gsub("barriington|brrington|barringtonq|barringon$|barringtons|barringron$|barringtron|barringston|barrngton$|barringto$|barrrington|barringtom|barrignton$|^barr$|bwrrington|barringtonton|barringtontonton|west barrington|barringtn|barringotn|^barring$|barington|barrintong|barrinton|bafrrington", "barrington", dfx$emp_city)
dfx$emp_city <- gsub("^bi$|blcok island|new shoreham|^bock island", "block island", dfx$emp_city)
dfx$emp_city <- gsub("^bristo$|bristool|bristrol|briston|pristol$|^britol$|brsitol|brisdtol|brsitol|^brisol$|brtistol", "bristol", dfx$emp_city)
dfx$emp_city <- gsub("buriville|burriville|burillville", "burrillville", dfx$emp_city)
dfx$emp_city <- gsub("centerdale", "centredale", dfx$emp_city)
dfx$emp_city <- gsub("^ccranston$|crancton|carnton|crnanston|edgewood|crnston|cranst0n|ctranston|cranstin|cranstonn|cranstonr|craston|^cran$|canston|^ransron$|carnston|cxranston|cvranston|cranson|cranstonq|^crnt$|crnaston|cranstone|cranaton|craanston|cranston ri|crasnton|crantson|^cra$|cransto$|cranton|cranstron|cransrton", "cranston", dfx$emp_city)
dfx$emp_city <- gsub("centra falls|centrals fall|crntral falls|central fall$|central fallss|centrall falls|cental falls|^cf$", "central falls", dfx$emp_city)
dfx$emp_city <- gsub("charlsetown|chaerlestown|charlestpwn|chaleston|chalestown|charlesown|charlesrtown|charlstown|chalrestown|charleston|charletown|charlestwon", "charlestown", dfx$emp_city)
dfx$emp_city <- gsub("chepatchet|chepachrt|chepachwt|cheachet|cheepachet|chapachet|chepachat|chepcahet", "chepachet", dfx$emp_city)
dfx$emp_city <- gsub("covertry|converty|convetry|coventy|coverntry|covnetry|cobentry|conventry|coventr$", "coventry", dfx$emp_city)
dfx$emp_city <- gsub("cumberlad|cuberland|cumberalnd|cumbrland|cumberl and|cumebrland|cumberlan$|cumerland|cu berland|cumberlandr|cumbland|cumbeland|cumberlandq|cumberlane|cumberlnad|cumberlnd|cumblerland|cumbderland|cumbreland", "cumberland", dfx$emp_city)
dfx$emp_city <- gsub("^eg$|east green$|eastgreenwich|^greenwich$|^149 overfield road$", "east greenwich", dfx$emp_city)
dfx$emp_city <- gsub("^ep$|^eprovidence|^02916$|^eprov$", "east providence", dfx$emp_city)
dfx$emp_city <- gsub("^exter$|^exteter$|^exiter$|^exet$|^exeer$|^exdter$", "exeter", dfx$emp_city)
dfx$emp_city <- gsub("fiskville", "fiskeville", dfx$emp_city)
dfx$emp_city <- gsub("fosrter$|foster city|^fister$|^forster$|^faster$", "foster", dfx$emp_city)
dfx$emp_city <- gsub("forsetdale", "forestdale", dfx$emp_city)
dfx$emp_city <- gsub("greendwich|greenwwich|ngreenwich|greenwihc|greenwichq|greenwih$|greemwich|grennwich|geeenwich|greewnich|greenwixh$|greenwiclle|greenwish$|greenwick|greebwich|gfreenwich|grenwich|geenwich|greenwhich|greewnwich|greewich|greeniwch|greenwiich|greeenwich|greenwichri|grrenwich", "greenwich", dfx$emp_city)
dfx$emp_city <- gsub("^green$", "greene", dfx$emp_city)
dfx$emp_city <- gsub("grennville|greenwille|cgreenville|greenwiclle|greenvkille|greenvill$|grenville|greenvile|^geenville$|greemville", "greenville", dfx$emp_city)
dfx$emp_city <- gsub("glocester|gloster|^clocester$|glouster|golcester|glocetste|glochester", "gloucester", dfx$emp_city)
dfx$emp_city <- gsub("^greenwich$", "east greenwich", dfx$emp_city)
dfx$emp_city <- gsub("^harmon$|^harmoney$", "harmony", dfx$emp_city)
dfx$emp_city <- gsub("hopkington", "hopkinton", dfx$emp_city)
dfx$emp_city <- gsub("hooe|hope valley", "hope", dfx$emp_city)
dfx$emp_city <- gsub("harrissville|harrrisville", "harrisville", dfx$emp_city)
dfx$emp_city <- gsub("jamestowrn|jamestworn|jametown|jamesown|^james$|jamestrown|jamestwon|jamesetown|jamesstown|janestown|^jameston$|jamestonw|jamestownq", "jamestown", dfx$emp_city)
dfx$emp_city <- gsub("jct$|junchion|junctions$|junstion", "junction", dfx$emp_city)
dfx$emp_city <- gsub("johsnston|^jstn$|josnton|johynston|johnstonb|jonnston|johnstron|hohnston|johnson|johsnton|johston|johnsotn|johnstoin|jonston|jphnston|johnstonq|jpohnston", "johnston", dfx$emp_city)
dfx$emp_city <- gsub("kignston|kinsgstown|kingsiown|kingstownq|kinsgtown|kingsown|kingstowon|kungstown|kingsrown|kingstpwm|kingswtown|kingstaown|kingstoen|kinkstown|ki8ngstown|kingstowm|kinngstown|kingstwon|kinsgston|kignstown|kingstonw|kinstown|kingstgown|dingstown|kingtown|kings$|kinghstown|kingstow$|kingdtown", "kingstown", dfx$emp_city)
dfx$emp_city <- gsub("linccoln|^20 steeple lane$|^incoln$|kincoln|linciln|lincoiln|lincolnc|lincolne|lincolnq|linconl|loncoln|linocln|lincioln|linoln|linclon|^lincol$|llincoln|licoln|liincoln|^lincon$|lincolnn", "lincoln", dfx$emp_city)
dfx$emp_city <- gsub("little comption|little comptom|02837|little comton|little comtpon|little copton", "little compton", dfx$emp_city)
dfx$emp_city[dfx$emp_city == "compton" & dfx$emp_st == "ri"] <- "little compton"
dfx$emp_city <- gsub("mmanville", "manville", dfx$emp_city)
dfx$emp_city <- gsub("mapoleville|mappleville", "mapleville", dfx$emp_city)
dfx$emp_city <- gsub("matunick", "matunuck", dfx$emp_city)
dfx$emp_city <- gsub("midddletown|middetown|middfletown|middleown|middletowh|middlettown|middletwon|middeltown|middleotwn|middltown|midletown|middlet0wn|middleton", "middletown", dfx$emp_city)
dfx$emp_city <- gsub("^narragansettt|^narraganasett$|narragasett$|narrasgansett$|narraganestt$|narragasnsett$|narrragansett$|narraganett$|narragensett$|narragnasett$|narragansettt$|narragnsett$|narragansettte|narrangensett$|nargansett$|narraganstt$|narrangnsett$|narraganseet$|narrangsett$|narransett$|narraganset$|naragansett|narrasansett|narrgansett|narraganssett$|^narr$|^narra$|narrangansett$|narragannsett$|narrangasett$", "narragansett", dfx$emp_city)
dfx$emp_city <- gsub("neweport|newpart|^new port$|wewport|west newport|newporft|newportq|newpot|newprot|newprt|newpoer", "newport", dfx$emp_city)
dfx$emp_city <- gsub("north kingstown north kingstown|02852|north k$|north kingston|^nk$|^kingaton$|30 pojac point rd", "north kingstown", dfx$emp_city)
dfx$emp_city <- gsub("northscituate", "north scituate", dfx$emp_city)
dfx$emp_city <- gsub("pqwtucket|pawtcket|pawtucketq|patwucket|pawtucklet|pawtcuet|pawtucdket|pawtuckeet|pwtucket|pawtcuket|pawtuckett|pawutcket|pawticket|pawtuckt|pawtudcket|pawtucet|patucket|paqwtucket|pawtucke$|pawtuket|pawtuvcket|pawtucker|pawrucket|^pawt$|pawucket", "pawtucket", dfx$emp_city)
dfx$emp_city <- gsub("porthmouth|postsmouth|postsmouthc|porthsmouth|portmouth|prortsmouth|portsmith|posrtsmouth|porstmouth|porstsmouth|portsmoth|portsouth|poutsmouth|protsmouth", "portsmouth", dfx$emp_city)
dfx$emp_city <- gsub("quonset point", "quonset", dfx$emp_city)
dfx$emp_city <- gsub("^pacoag$|pascaog$|^pascog$|^pasqoag$", "pascoag", dfx$emp_city)
dfx$emp_city <- gsub("peace dale", "peacedale", dfx$emp_city)
dfx$emp_city[dfx$emp_city == "pr" & dfx$emp_st == "ri"] <- "providence"
dfx$emp_city <- gsub("provvidence|^smith st providence$|68 kennedy plaza providence|259 benefit street|235 promenade st providence|perovidence|providencew|02903|rpovidence|providnece|provdience|prov$|providenc$|providenece|ptovidence|provicence|providene$|provisdence|proviidence|provicdence|proividence|procidence|prfovidence|prob$", "providence", dfx$emp_city)
dfx$emp_city <- gsub("pprovidence|parovidence|pr0vidence|^rovidence|provcidence|p0rovidence|prviddence$|providwence$|providennce$|providencre$|providence02903$|providencce$|provfidende$|providnce$|providenfce$|providenceri$|providenc e$|provdince$|proivdence$|prividence$|pprovidenc$|provivdence$|providenec$|providencer$|providence ri 2903$|providecne|provdence|proidence$|providenxe$|providende$|providenceq$|providence ri$|providece$|provd$|^prof$|povidence$|priovidence$|provience$|providenve$|providendce$|providenceence$|^provide$|procvidence$|prinvidence$|prvidence$|proviedence$|providencve$|providenced$|providencde$|^provi$|prov idence$|^proc$|porvidence$|porovidence$", "providence", dfx$emp_city)
dfx$emp_city <- gsub("probvidence|235 promenade st providence|south main st providence|15 humboldt|management corp|1165 n main|94 meeting st|8 blackstone blvd|1 citizens|68 kennedy plaza providence|^pro$|providencece|^ovidence$|providencw|providencw|providnence$|providenceprovidence$|proviednce$|provifence$|providcence$|provvidenc$|providencxe|providencd$|providcence$|ph providence$|providenxce$|p rovidence$| peov$|providernce|providewnce$| proiv$| prov1417$|providenvce$|orovidence$|providenxwe$|providence r$|provience$|provience|proviedence$|providencde$|providece$|providnce$|providenec$|proivdence$|providende$|proidence$", "providence", dfx$emp_city)
dfx$emp_city <- gsub("^np$|^northprovidence$|^nprovidence$|^noprovidence$|north prov senate|also 26 countryside dr", "north providence", dfx$emp_city)
dfx$emp_city <- gsub("riveside|riverisde|riversice|riversidse|riversie$", "riverside", dfx$emp_city)
dfx$emp_city <- gsub("rehobeth", "rehoboth", dfx$emp_city)
dfx$emp_city <- gsub("rimford|rumfod|02916|runford|^ruford$|rumfoed|^rumord$", "rumford", dfx$emp_city)
dfx$emp_city <- gsub("sanfrancisco", "san francisco", dfx$emp_city)
dfx$emp_city <- gsub("saunderstoen|^saunderstwon$|^sunderstown$|^sndstwn$|suanderstown|shanderstown|sounderstown|saundestown|sauponstown|saunderstonw|saunderstorn|saunderstow$|saunderstown rd|saudnerstown|saundersown|saunderatown|sauderstown|saundertown", "saunderstown", dfx$emp_city)
dfx$emp_city <- gsub("^scituae|^scitute|^sctituate|^sctuate|^scituae|^scituater|scit$|situate|scituite|schituate|sciutate|scituateq|scitaue|scitaute", "scituate", dfx$emp_city)
dfx$emp_city <- gsub("slaterville", "slatersville", dfx$emp_city)
dfx$emp_city <- gsub("smiithfield|^02917$|smthfield|smithfileld|smirthfield|smitfield|smithgfield|smithfild|smighfield|smighfiels|smith field$|smithfied|smithield|smithfiel$|smithfiled|smihfield|smithifeld|sithfiel|smithfeild|smtihfield", "smithfield", dfx$emp_city)
dfx$emp_city <- gsub("south kingstown|^sk$|^wk$|^kignston|^kingsto$|^kinston$|^kingstown$|^kingston$|^kngston$", "south kingston", dfx$emp_city)
dfx$emp_city <- gsub("tivertson|02878", "tiverton", dfx$emp_city)
dfx$emp_city <- gsub("unk$|searching|not applicable|anywhere|^tbd$|unknown at this time|^co$|north office location|27 cornell rd$|various|requested|unkown|don t know|info requested|information requested|investigating|^x$|^xx$|^x{1,} unknown$|obtaining info|^na$|^none$|^n$|awaiting info", "unknown", dfx$emp_city)
dfx$emp_city <- gsub("^warrren$|^warrent$|^waren$|warrein|^warrem$|^warrenn$|^warrne$|^wareen$", "warren", dfx$emp_city)
dfx$emp_city <- gsub("wakefieldddddd|^wakefileld$|wakefeild|wakrefield|wakwfield|wakefiels|wakefiield|wakefiled|wakefiedl|wakefirld|wakfield|wakefieeld", "wakefield", dfx$emp_city)
dfx$emp_city <- gsub("waewick|^war$|warwixck|wqartwick|warwickl$|warick$|warwickk|wariwkc|warwcick|^warrick$|warwcik|wrwick|wrawick|waerwick|warwic$|wrwick|wrawick|warwuck|warwck|warweick|warwivk|wawick|watwick|warwik|warkwick|wariwick|warrwick|warwiick|warwicvk|watrwick|warwich|wariwck|^warick$|^warwic$|warwickj|was 144 bignall st war|wawrick|warwickd", "warwick", dfx$emp_city)
dfx$emp_city <- gsub("west kingstown|west kngston|west kingston", "south kingston", dfx$emp_city)
dfx$emp_city <- gsub("westerley|^westerl$|westrerly|wester y$|westerlt$|westery$|weterly$|weserly$|wwesterly$|^westrerly$|westerly ri \\(state", "westerly", dfx$emp_city)
dfx$emp_city <- gsub("^wg$|westgreenwich", "west greenwich", dfx$emp_city)
dfx$emp_city <- gsub("^ww$|^wwarwick$|westwarwick|west w$|02893|east warwick|^wwae warwick$", "west warwick", dfx$emp_city)
dfx$emp_city <- gsub("woming|wynoming|^wyoning$", "wyoming", dfx$emp_city)
dfx$emp_city <- gsub("woodriver", "wood river", dfx$emp_city)
dfx$emp_city <- gsub("wonnsocket|^woonsoket$|120 count st woon|^woonsockt$|woonsokcet|soonsocket|woomsocket|woonsocketq|woonsockrt|^woon$|wonsocket|woobsocket|woondocket|woosocket|woonscket|woodsocket|wioonsocket|woonsocke$|woonsockett|wooksocket", "woonsocket", dfx$emp_city)
dfx$emp_city <- gsub("ttt", "tt", dfx$emp_city)



# Massachusetts Employer Cities
dfx$emp_city <- gsub("^allston", "alston", dfx$emp_city)
dfx$emp_city <- gsub("atteboro|attlerboro|attelboro|attleboro falls|attleborough|attleoboro|attlelboro|attloboro", "attleboro", dfx$emp_city)
dfx$emp_city <- gsub("auburndale", "auburn", dfx$emp_city)
dfx$emp_city <- gsub("barintree", "braintree", dfx$emp_city)
dfx$emp_city <- gsub("boxborough", "boxboro", dfx$emp_city)
dfx$emp_city <- gsub("belingham", "bellingham", dfx$emp_city)
dfx$emp_city <- gsub("bostaon|bostom|bsoton|bosten|bolton", "boston", dfx$emp_city)
dfx$emp_city <- gsub("bridgewaer", "bridgewater", dfx$emp_city)
dfx$emp_city <- gsub("brochton|brockon|brocton", "brockton", dfx$emp_city)
dfx$emp_city <- gsub("buzzard bay", "buzzards bay", dfx$emp_city)
dfx$emp_city <- gsub("cabridge|cambrdige|^harvard|carmbidge", "cambridge", dfx$emp_city)
dfx$emp_city <- gsub("clelmsford|chelmsfor$|chelmsfor$", "chelmsford", dfx$emp_city)
dfx$emp_city <- gsub("chestnut hil$|chestnut hills", "chestnut hill", dfx$emp_city)
dfx$emp_city <- gsub("^davers$", "danvers", dfx$emp_city)
dfx$emp_city <- gsub("dedeham", "dedham", dfx$emp_city)
dfx$emp_city <- gsub("darthmouth| darmtmout$|darmouth", "dartmouth", dfx$emp_city)
dfx$emp_city <- gsub("dorchester center|dorchestter", "dorchester", dfx$emp_city)
dfx$emp_city <- gsub("fakl river|^fal river", "fall river", dfx$emp_city)
dfx$emp_city <- gsub("foxborough|foxoboro", "foxboro", dfx$emp_city)
dfx$emp_city <- gsub("franlin", "franklin", dfx$emp_city)
dfx$emp_city <- gsub("freetwon", "freetown", dfx$emp_city)
dfx$emp_city <- gsub("farmingham|frayham", "framingham", dfx$emp_city)
dfx$emp_city <- gsub("havenhill", "haverhill", dfx$emp_city)
dfx$emp_city <- gsub("highhan", "hingham", dfx$emp_city)
dfx$emp_city <- gsub("hopington", "hopkinton", dfx$emp_city)
dfx$emp_city <- gsub("hyannic", "hyannis", dfx$emp_city)
dfx$emp_city <- gsub("malborough|marlborough|marlbrough", "marlboro", dfx$emp_city)
dfx$emp_city <- gsub("masfield|manfield|mansfiled", "mansfield", dfx$emp_city)
dfx$emp_city <- gsub("miilton", "milton", dfx$emp_city)
dfx$emp_city <- gsub("nantucket island|nantuck$", "nantucket", dfx$emp_city)
dfx$emp_city <- gsub("new bedord|^bedford$|new beford", "new bedford", dfx$emp_city)
dfx$emp_city <- gsub("newberryport|newburry port", "newburyport", dfx$emp_city)
dfx$emp_city <- gsub("needham heights|needdham|^needam|weedham", "needham", dfx$emp_city)
dfx$emp_city <- gsub("norfalk", "norfolk", dfx$emp_city)
dfx$emp_city <- gsub("northboroudh|northborough|nortborough", "northboro", dfx$emp_city)
dfx$emp_city <- gsub("northhampton", "northampton", dfx$emp_city)
dfx$emp_city <- gsub("pocassett", "pocasset", dfx$emp_city)
dfx$emp_city <- gsub("^auincy|quncy", "quincy", dfx$emp_city)
dfx$emp_city <- gsub("rohoboth|rehodoth|reboboth", "rehoboth", dfx$emp_city)
dfx$emp_city <- gsub("seekon$|seeknok$|^seekon$|seakonk$|seekink$|seekonkk$|seekoonk$", "seekonk", dfx$emp_city)
dfx$emp_city <- gsub("sommerville", "somerville", dfx$emp_city)
dfx$emp_city <- gsub("sawnsea", "swansea", dfx$emp_city)
dfx$emp_city <- gsub("southdartmouth", "south dartmouth", dfx$emp_city)
dfx$emp_city <- gsub("stouighton", "stoughton", dfx$emp_city)
dfx$emp_city <- gsub("southborough", "southboro", dfx$emp_city)
dfx$emp_city <- gsub("tauton", "taunton", dfx$emp_city)
dfx$emp_city <- gsub("tewksberry|tewsbury|tewskbury", "tewksbury", dfx$emp_city)
dfx$emp_city <- gsub("warehan|warwham", "wareham", dfx$emp_city)
dfx$emp_city <- gsub("walthan", "waltham", dfx$emp_city)
dfx$emp_city <- gsub("welster", "webster", dfx$emp_city)
dfx$emp_city <- gsub("westburo|westborough|westbourough|westburo|west borough", "westboro", dfx$emp_city)
dfx$emp_city <- gsub("wellsley hills|wellesley hills|wellesly|wellesly hill|wellsley hills", "wellesley", dfx$emp_city)
dfx$emp_city <- gsub("woercester|wocester|worchester", "worcester", dfx$emp_city)
dfx$emp_city <- gsub("wobum", "woburn", dfx$emp_city)
dfx$emp_city <- gsub("witinsville|whitensville", "whitinsville", dfx$emp_city)
dfx$emp_city <- gsub("^westward$|^westwod$", "westwood", dfx$emp_city)
dfx$emp_city <- gsub("westport point|westprot|^west port$", "westport", dfx$emp_city)
# dfx$emp_city <- gsub("", "", dfx$emp_city)
# dfx$emp_city <- gsub("", "", dfx$emp_city)
# dfx$emp_city <- gsub("", "", dfx$emp_city)
# dfx$emp_city <- gsub("", "", dfx$emp_city)
# dfx$emp_city <- gsub("", "", dfx$emp_city)
# dfx$emp_city <- gsub("", "", dfx$emp_city)

sort(unique(dfx$emp_city[dfx$emp_st == "ct"]))

# Connecticut Employer Cities
dfx$emp_city <- gsub("brandford|bradford", "branford", dfx$emp_city)
dfx$emp_city <- gsub("cheshire court|^chesire$", "cheshire", dfx$emp_city)
dfx$emp_city <- gsub("^croton$", "groton", dfx$emp_city)
dfx$emp_city <- gsub("hardford", "hartford", dfx$emp_city)
dfx$emp_city <- gsub("farfield", "fairfield", dfx$emp_city)
dfx$emp_city <- gsub("glastonberry", "glastonbury", dfx$emp_city)
dfx$emp_city <- gsub("haddam", "hamden", dfx$emp_city)
dfx$emp_city <- gsub("mashantuckut|meshantucket", "mashantucket", dfx$emp_city)
dfx$emp_city <- gsub("^mstic$", "mystic", dfx$emp_city)
dfx$emp_city <- gsub("new cannan", "new canaan", dfx$emp_city)
dfx$emp_city <- gsub("new hanven", "new haven", dfx$emp_city)
dfx$emp_city <- gsub("^ns$", "north stonington", dfx$emp_city)
dfx$emp_city <- gsub("^putnan$", "putnam", dfx$emp_city)
dfx$emp_city <- gsub("redding ridge|reeding", "redding", dfx$emp_city)
dfx$emp_city <- gsub("sstamford", "stamford", dfx$emp_city)
dfx$emp_city <- gsub("waterfoed", "waterford", dfx$emp_city)
dfx$emp_city <- gsub("wilton", "wilson", dfx$emp_city)
dfx$emp_city <- gsub("windsor locks", "windsor", dfx$emp_city)
dfx$emp_city <- gsub("wethesfield", "wethersfield", dfx$emp_city)
dfx$emp_city <- gsub("storrs mansfield", "storrs", dfx$emp_city)
dfx$emp_city <- gsub("trumbill|trumsoll", "trumbull", dfx$emp_city)
dfx$emp_city <- gsub("vernon rockville", "vernon", dfx$emp_city)
dfx$emp_city <- gsub("waterbury center", "waterbury", dfx$emp_city)

# Format Rhode Island Cities
dfx$donor_city <- gsub("\\-[0-9]{1,}", "", dfx$donor_city); head(dfx$donor_city)
dfx$donor_city <- gsub(", [a-zA-Z]{2}$", "", dfx$donor_city); head(dfx$donor_city)
dfx$donor_city <- gsub(",|\\.|\\/|\\;|'|`|\\:|_|\\?|\\[|\\*|\\\t|\\|\\^|\\-{1,}", " ", dfx$donor_city); head(dfx$donor_city)
dfx$donor_city <- gsub("\\", "", dfx$donor_city, fixed = TRUE); head(dfx$donor_city)
dfx$donor_city <- gsub("\\s{2,}", " ", dfx$donor_city)
dfx$donor_city <- gsub(" ri$", "", dfx$donor_city)
dfx$donor_city <- trimws(dfx$donor_city)
dfx$donor_city <- gsub("^n a$", "na", dfx$donor_city)
dfx$donor_city <- gsub("^e |^eat |^easty |^easr |^esat |^est |^eart |^eas |^easst |^eeast |^eats |^eaat |^eaast |^rast ", "east ", dfx$donor_city)
dfx$donor_city <- gsub("^w |^w est |^werst |^wet |^wwae |^wast |^wesk |^westw ", "west ", dfx$donor_city)
dfx$donor_city <- gsub("^s |^so |^ssouth |^sout |^soth ", "south ", dfx$donor_city)
dfx$donor_city <- gsub("^n |^no |^norht |^norh |^norrth |^nothr |^nor |^noerh |^niorth |^noirth |^norrh |^nort |^noth |^nroth ", "north ", dfx$donor_city)
dfx$donor_city <- gsub("^ashway$|^ahaway$|^asaway$|^shaway$", "ashaway", dfx$donor_city)
dfx$donor_city <- gsub("barriington|^carrington|brrington|barringtonq|barringon$|barringtons|barringron$|barringtron|barringston|barrngton$|barringto$|barrrington|barringtom|barrignton$|^barr$|bwrrington|barringtonton|barringtontonton|west barrington|barringtn|barringotn|^barring$|barington|barrintong|barrinton|bafrrington", "barrington", dfx$donor_city)
dfx$donor_city <- gsub("^bi$|blcok island|new shoreham|^bock island", "block island", dfx$donor_city)
dfx$donor_city <- gsub("^bristo$|^bistol$|bristool|bristrol|briston|pristol$|^britol$|brsitol|brisdtol|brsitol|^brisol$|brtistol", "bristol", dfx$donor_city)
dfx$donor_city <- gsub("buriville|burriville|burillville", "burrillville", dfx$donor_city)
dfx$donor_city <- gsub("centerdale", "centredale", dfx$donor_city)
dfx$donor_city <- gsub("^ccranston$|crancton|carnton|crnanston|crnston|cranst0n|ctranston|cranstin|cranstonn|cranstonr|craston|^cran$|canston|^ransron$|carnston|cxranston|cvranston|cranson|cranstonq|^crnt$|crnaston|cranstone|cranaton|craanston|cranston ri|crasnton|crantson|^cra$|cransto$|cranton|cranstron|cransrton", "cranston", dfx$donor_city)
dfx$donor_city <- gsub("centra falls|centrals fall|crntral falls|central fall$|central fallss|centrall falls|cental falls|^cf$", "central falls", dfx$donor_city)
dfx$donor_city <- gsub("charlsetown|charlestownw|chaerlestown|charlestpwn|chaleston|chalestown|charlesown|charlesrtown|charlstown|chalrestown|charleston|charletown|charlestwon", "charlestown", dfx$donor_city)
dfx$donor_city <- gsub("chepatchet|chepachrt|chepachwt|cheachet|cheepachet|chapachet|chepachat|chepcahet", "chepachet", dfx$donor_city)
dfx$donor_city <- gsub("claville|clayvile|claville", "clayville", dfx$donor_city)
dfx$donor_city <- gsub("covertry|converty|convetry|coventy|coverntry|covnetry|cobentry|conventry|coventr$", "coventry", dfx$donor_city)
dfx$donor_city <- gsub("cumberlad|cuberland|^cu$|cumberalnd|cumbrland|cumberl and|cumebrland|cumberlan$|cumerland|cu berland|cumberlandr|cumbland|cumbeland|cumberlandq|cumberlane|cumberlnad|cumberlnd|cumblerland|cumbderland|cumbreland", "cumberland", dfx$donor_city)
dfx$donor_city <- gsub("^eg$|east green$|eastgreenwich|^greenwich$|^149 overfield road$", "east greenwich", dfx$donor_city)
dfx$donor_city <- gsub("^ep$|^eprovidence$|^02916$|^eprov$", "east providence", dfx$donor_city)
dfx$donor_city <- gsub("^exter$|^exteter$|^exiter$|^exet$|^exeer$|^exdter$", "exeter", dfx$donor_city)
dfx$donor_city <- gsub("fiskville", "fiskeville", dfx$donor_city)
dfx$donor_city <- gsub("fosrter$|foster city|^fister$|^forster$|^faster$", "foster", dfx$donor_city)
dfx$donor_city <- gsub("forsetdale", "forestdale", dfx$donor_city)
dfx$donor_city <- gsub("greendwich|greenwwich|ngreenwich|greenwihc|greenwichq|greenwih$|greemwich|grennwich|geeenwich|greewnich|greenwixh$|greenwiclle|greenwish$|greenwick|greebwich|gfreenwich|grenwich|geenwich|greenwhich|greewnwich|greewich|greeniwch|greenwiich|greeenwich|greenwichri|grrenwich", "greenwich", dfx$donor_city)
dfx$donor_city <- gsub("^green$", "greene", dfx$donor_city)
dfx$donor_city <- gsub("grennville|greenwille|cgreenville|greenwiclle|greenvkille|greenvill$|grenville|greenvile|^geenville$|greemville", "greenville", dfx$donor_city)
dfx$donor_city <- gsub("glocester|gloster|^clocester$|glouster|golcester|glocetste|glochester", "gloucester", dfx$donor_city)
dfx$donor_city <- gsub("^greenwich$", "east greenwich", dfx$donor_city)
dfx$donor_city <- gsub("^harmon$|^harmoney$", "harmony", dfx$donor_city)
dfx$donor_city <- gsub("hopkington", "hopkinton", dfx$donor_city)
dfx$donor_city <- gsub("hooe|hope valley", "hope", dfx$donor_city)
dfx$donor_city <- gsub("harrissville|harrrisville", "harrisville", dfx$donor_city)
dfx$donor_city <- gsub("jamestowrn|jamestworn|jametown|jamesown|^james$|jamestrown|jamestwon|jamesetown|jamesstown|janestown|^jameston$|jamestonw|jamestownq", "jamestown", dfx$donor_city)
dfx$donor_city <- gsub("jct$|junchion|junctions$|junstion", "junction", dfx$donor_city)
dfx$donor_city <- gsub("johsnston|josnton|johynston|johnstonb|jonnston|johnstron|hohnston|johnson|johsnton|johston|johnsotn|johnstoin|jonston|jphnston|johnstonq|jpohnston", "johnston", dfx$donor_city)
dfx$donor_city <- gsub("kignston|kinsgstown|kingsiown|kingstownq|kinsgtown|kingsown|kingstowon|kungstown|kingsrown|kingstpwm|kingswtown|kingstaown|kingstoen|kinkstown|ki8ngstown|kingstowm|kinngstown|kingstwon|kinsgston|kignstown|kingstonw|kinstown|kingstgown|dingstown|kingtown|kings$|kinghstown|kingstow$|kingdtown", "kingstown", dfx$donor_city)
dfx$donor_city <- gsub("linccoln|^20 steeple lane$|^incoln$|kincoln|linciln|lincoiln|lincolnc|lincolne|lincolnq|linconl|loncoln|linocln|lincioln|linoln|linclon|^lincol$|llincoln|licoln|liincoln|^lincon$|lincolnn", "lincoln", dfx$donor_city)
dfx$donor_city <- gsub("little comption|little comptom|02837|little comton|little comtpon|little copton", "little compton", dfx$donor_city)
dfx$donor_city <- gsub("mmanville", "manville", dfx$donor_city)
dfx$donor_city <- gsub("mapoleville|mappleville", "mapleville", dfx$donor_city)
dfx$donor_city <- gsub("matunick|natunick", "matunuck", dfx$donor_city)
dfx$donor_city <- gsub("midddletown|^m$|middetown|middfletown|middleown|middletowh|middlettown|middletwon|middeltown|middleotwn|middltown|midletown|middlet0wn|middleton", "middletown", dfx$donor_city)
#dfx$donor_city <- gsub("^narragansettt$|^narraganasett$|narragasett|narrasgansett|narraganestt|narragasnsett|narrragansett|narraganett|narragensett|narragnasett|narragansettt|narragnsett|narragansettte|narrangensett|nargansett|narraganstt|narrangnsett|narraganseet|narrangsett|narransett|narraganset|naragansett|narrasansett|narrgansett|narraganssett|^narr$|^narra$|narrangansett|narragannsett|narrangasett", "narragansett", dfx$donor_city)
dfx$donor_city <- gsub("^narragansettte$|narragansette$|^narrangensett$|^nargansett$|^narraganstt$|^narrangnsett$|^narraganseet$|^narrangsett$|^narransett|^narraganset$|naragansett$|^narrasansett$|^narrgansett$|^narraganssett$|^narr$|^narra$|^narrangansett$|^narragannsett$|^narrangasett$", "narragansett", dfx$donor_city)
dfx$donor_city <- gsub("^narragansettt$|^narraganasett$|^narragasett$|^narrasgansett$|^narraganestt$|^narragasnsett$|^narrragansett$|^narraganett|^narragensett$|^narragnasett$|^narragansettt$|^narragnsett", "narragansett", dfx$donor_city)
dfx$donor_city <- gsub("neweport|newpart|^new port$|wewport|west newport|newporft|newportq|newpot|newprot|newprt|newpoer", "newport", dfx$donor_city)
dfx$donor_city <- gsub("north kingstown north kingstown|02852|north k$|north kingston|^nk$|^kingaton$|30 pojac point rd", "north kingstown", dfx$donor_city)
dfx$donor_city <- gsub("northscituate", "north scituate", dfx$donor_city)
dfx$donor_city <- gsub("pqwtucket|pawtcket|pawtucketq|patwucket|pawtucklet|pawtcuet|pawtucdket|pawtuckeet|pwtucket|pawtcuket|pawtuckett|pawutcket|pawticket|pawtuckt|pawtudcket|pawtucet|patucket|paqwtucket|pawtucke$|pawtuket|pawtuvcket|pawtucker|pawrucket|^pawt$|pawucket", "pawtucket", dfx$donor_city)
dfx$donor_city <- gsub("porthmouth|postsmouth|postsmouthc|porthsmouth|portmouth|prortsmouth|portsmith|posrtsmouth|porstmouth|porstsmouth|portsmoth|portsouth|poutsmouth|protsmouth", "portsmouth", dfx$donor_city)
dfx$donor_city <- gsub("quonset point", "quonset", dfx$donor_city)
dfx$donor_city <- gsub("^pacoag$|pascaog$|^pascog$|^pasqoag$", "pascoag", dfx$donor_city)
dfx$donor_city <- gsub("peace dale|peace pike", "peacedale", dfx$donor_city)
dfx$donor_city <- gsub("provvidence|perovidence|providencew|02903|rpovidence|providnece|provdience|prov$|providenc$|providenece|ptovidence|provicence|providene$|provisdence|proviidence|provicdence|proividence|procidence|prfovidence|prob$", "providence", dfx$donor_city)
dfx$donor_city <- gsub("parovidence|pr0vidence|^rovidence|provcidence|p0rovidence|prviddence$|providwence$|providennce$|providencre$|providence02903$|providencce$|provfidende$|providnce$|providenfce$|providenceri$|providenc e$|provdince$|proivdence$|prividence$|pprovidenc$|provivdence$|providenec$|providencer$|providence ri 2903$|providecne|provdence|proidence$|providenxe$|providende$|providenceq$|providence ri$|providece$|provd$|^prof$|povidence$|priovidence$|provience$|providenve$|providendce$|providenceence$|^provide$|procvidence$|prinvidence$|prvidence$|proviedence$|providencve$|providenced$|providencde$|^provi$|prov idence$|^proc$|porvidence$|porovidence$", "providence", dfx$donor_city)
dfx$donor_city <- gsub("probvidence|15 humboldt|management corp|1165 n main|94 meeting st|8 blackstone blvd|1 citizens|68 kennedy plaza providence|^pro$|providencece|^ovidence$|providencw|providencw|providnence$|providenceprovidence$|proviednce$|provifence$|providcence$|provvidenc$|providencxe|providencd$|providcence$|ph providence$|providenxce$|p rovidence$| peov$|providernce|providewnce$| proiv$| prov1417$|providenvce$|orovidence$|providenxwe$|providence r$|provience$|provience|proviedence$|providencde$|providece$|providnce$|providenec$|proivdence$|providende$|proidence$", "providence", dfx$donor_city)
dfx$donor_city <- gsub("^np$|^northprovidence$|^nprovidence$|^noprovidence$|also 26 countryside dr", "north providence", dfx$donor_city)
dfx$donor_city <- gsub("riveside|riverisde|riversice|riversidse|riversie$", "riverside", dfx$donor_city)
dfx$donor_city <- gsub("rehobeth", "rehoboth", dfx$donor_city)
dfx$donor_city <- gsub("rimford|rumfod|02916|runford|^ruford$|rumfoed|^rumord$", "rumford", dfx$donor_city)
dfx$donor_city <- gsub("sanfrancisco", "san francisco", dfx$donor_city)
dfx$donor_city <- gsub("shanrock", "shannock", dfx$donor_city)
dfx$donor_city <- gsub("saunderstoen|^saunderstwon$|^sunderstown$|^sndstwn$|suanderstown|shanderstown|sounderstown|saundestown|sauponstown|saunderstonw|saunderstorn|saunderstow$|saunderstown rd|saudnerstown|saundersown|saunderatown|sauderstown|saundertown", "saunderstown", dfx$donor_city)
dfx$donor_city <- gsub("^scituae|scidtuate|^scitute|^sctituate|^sctuate|^scituae|^scituater|scit$|situate|scituite|schituate|sciutate|scituateq|scitaue|scitaute", "scituate", dfx$donor_city)
dfx$donor_city <- gsub("slaterville", "slatersville", dfx$donor_city)
dfx$donor_city <- gsub("smiithfield|^02917$|smthfield|smithfileld|smirthfield|smitfield|smithgfield|smithfild|smighfield|smighfiels|smith field$|smithfied|smithield|smithfiel$|smithfiled|smihfield|smithifeld|sithfiel|smithfeild|smtihfield", "smithfield", dfx$donor_city)
dfx$donor_city <- gsub("south kingstown|^sk$|^wk$|^kignston|^kingsto$|^kinston$|^kingstown$|^kingston$|^kngston$|552 kingstown ro", "south kingston", dfx$donor_city)
dfx$donor_city <- gsub("tivertson|02878", "tiverton", dfx$donor_city)
dfx$donor_city <- gsub("unk$|^tbd$|anywhere|unknown at this time|^co$|27 cornell rd$|various|requested|unkown|don t know|info requested|information requested|investigating|^x$|^xx$|^x{1,} unknown$|obtaining info|^na$|^none$|^n$|awaiting info", "unknown", dfx$donor_city)
dfx$donor_city <- gsub("^warrren$|^warr$|^warrent$|^waren$|warrein|^warrem$|^warrenn$|^warrne$|^wareen$", "warren", dfx$donor_city)
dfx$donor_city <- gsub("wakefieldddddd|^wakefileld$|wakefeild|wakrefield|wakwfield|wakefiels|wakefiield|wakefiled|wakefiedl|wakefirld|wakfield|wakefieeld", "wakefield", dfx$donor_city)
dfx$donor_city <- gsub("waewick|^war$|warwixck|wqartwick|warwickl$|warick$|warwickk|wariwkc|warwcick|^warrick$|warwcik|wrwick|wrawick|waerwick|warwic$|wrwick|wrawick|warwuck|warwck|warweick|warwivk|wawick|watwick|warwik|warkwick|wariwick|warrwick|warwiick|warwicvk|watrwick|warwich|wariwck|^warick$|^warwic$|warwickj|was 144 bignall st war|wawrick|warwickd", "warwick", dfx$donor_city)
dfx$donor_city <- gsub("west kingstown|west kngston|west kingston", "south kingston", dfx$donor_city)
dfx$donor_city <- gsub("westerley|^westerl$|westrerly|wester y$|westerlt$|westery$|weterly$|weserly$|wwesterly$|^westrerly$|westerly ri \\(state", "westerly", dfx$donor_city)
dfx$donor_city <- gsub("^wg$|westgreenwich", "west greenwich", dfx$donor_city)
dfx$donor_city <- gsub("^ww$|^wes$|^wwarwick$|westwarwick|west w$|east warwick|^wwae warwick$|02893", "west warwick", dfx$donor_city)
dfx$donor_city <- gsub("woming|wynoming|^wyoning$", "wyoming", dfx$donor_city)
dfx$donor_city <- gsub("woodriver", "wood river", dfx$donor_city)
dfx$donor_city <- gsub("wonnsocket|^woonsoket$|^woonsockt$|woonsokcet|soonsocket|woomsocket|woonsocketq|woonsockrt|^woon$|wonsocket|woobsocket|woondocket|woosocket|woonscket|woodsocket|wioonsocket|woonsocke$|woonsockett|wooksocket", "woonsocket", dfx$donor_city)

# Massachusetts donor cities
dfx$donor_city <- gsub("^allston", "alston", dfx$donor_city)
dfx$donor_city <- gsub("^ashlandq", "ashland", dfx$donor_city)
dfx$donor_city <- gsub("atteboro|attlerboro|attelboro|attleboro falls|attleborough|attleoboro|attlelboro|attloboro", "attleboro", dfx$donor_city)
dfx$donor_city <- gsub("auburndale", "auburn", dfx$donor_city)
dfx$donor_city <- gsub("brantree|barintree", "braintree", dfx$donor_city)
dfx$donor_city <- gsub("boxborough", "boxboro", dfx$donor_city)
dfx$donor_city <- gsub("belingham|bellinghamn", "bellingham", dfx$donor_city)
dfx$donor_city <- gsub("bostaon|bostom|bsoton|bosten|bolton|bostin|boston ma", "boston", dfx$donor_city)
dfx$donor_city <- gsub("bridgewaer", "bridgewater", dfx$donor_city)
dfx$donor_city <- gsub("brochton|brockon|brocton", "brockton", dfx$donor_city)
dfx$donor_city <- gsub("brooklin$|broookline", "brookline", dfx$donor_city)
dfx$donor_city <- gsub("buzzard bay", "buzzards bay", dfx$donor_city)
dfx$donor_city <- gsub("cabridge|cambrdige|^harvard|carmbidge", "cambridge", dfx$donor_city)
dfx$donor_city <- gsub("clelmsford|chelmsfor$|chelmsfor$|chelmsfordd", "chelmsford", dfx$donor_city)
dfx$donor_city <- gsub("chestnut hil$|chestnut hills|chestut hill|^chestnut$", "chestnut hill", dfx$donor_city)
dfx$donor_city <- gsub("^davers$", "danvers", dfx$donor_city)
dfx$donor_city <- gsub("dedeham", "dedham", dfx$donor_city)
dfx$donor_city <- gsub("darthmouth| darmtmout$|darmouth", "dartmouth", dfx$donor_city)
dfx$donor_city <- gsub("dorchester center|dorchestter", "dorchester", dfx$donor_city)
dfx$donor_city <- gsub("fall rvier|fakl river|^fal river", "fall river", dfx$donor_city)
dfx$donor_city <- gsub("foxborough|foxoboro", "foxboro", dfx$donor_city)
dfx$donor_city <- gsub("franlin", "franklin", dfx$donor_city)
dfx$donor_city <- gsub("freetwon", "freetown", dfx$donor_city)
dfx$donor_city <- gsub("farmingham|frayham|framinghan|fraimgham", "framingham", dfx$donor_city)
dfx$donor_city <- gsub("haifax", "halifax", dfx$donor_city)
dfx$donor_city <- gsub("havenhill", "haverhill", dfx$donor_city)
dfx$donor_city <- gsub("highhan", "hingham", dfx$donor_city)
dfx$donor_city <- gsub("hopington", "hopkinton", dfx$donor_city)
dfx$donor_city <- gsub("hyannic", "hyannis", dfx$donor_city)
dfx$donor_city <- gsub("malborough|marlborough|marlbrough", "marlboro", dfx$donor_city)
dfx$donor_city <- gsub("masfield|manfield|mansfiled", "mansfield", dfx$donor_city)
dfx$donor_city <- gsub("miilton", "milton", dfx$donor_city)
dfx$donor_city <- gsub("nantucket island|^nantuck$|nantucketet", "nantucket", dfx$donor_city)
dfx$donor_city <- gsub("new bedord|^bedford$|new beford", "new bedford", dfx$donor_city)
dfx$donor_city <- gsub("newberryport|newburry port", "newburyport", dfx$donor_city)
dfx$donor_city <- gsub("needham heights|needdham|^needam|weedham", "needham", dfx$donor_city)
dfx$donor_city <- gsub("norfalk", "norfolk", dfx$donor_city)
dfx$donor_city <- gsub("newton square", "newton", dfx$donor_city)
dfx$donor_city <- gsub("northboroudh|northborough|nortborough", "northboro", dfx$donor_city)
dfx$donor_city <- gsub("northhampton", "northampton", dfx$donor_city)
dfx$donor_city <- gsub("pocassett", "pocasset", dfx$donor_city)
dfx$donor_city <- gsub("qunicy|^auincy|quncy", "quincy", dfx$donor_city)
dfx$donor_city <- gsub("rohoboth|rehodoth|reboboth|reholboth", "rehoboth", dfx$donor_city)
dfx$donor_city <- gsub("seeknok|^seekon$|^seakonk$|^seekink$|seekonkk$|seekoonk$", "seekonk", dfx$donor_city)
dfx$donor_city <- gsub("sommerville|somervillw", "somerville", dfx$donor_city)
dfx$donor_city <- gsub("southdartmouth", "south dartmouth", dfx$donor_city)
dfx$donor_city <- gsub("stouighton", "stoughton", dfx$donor_city)
dfx$donor_city <- gsub("southborough", "southboro", dfx$donor_city)
dfx$donor_city <- gsub("sswansea|swanswa$|sawnsea$", "swansea", dfx$donor_city)
dfx$donor_city <- gsub("tauton", "taunton", dfx$donor_city)
dfx$donor_city <- gsub("tewksberry|tewsbury|tewskbury", "tewksbury", dfx$donor_city)
dfx$donor_city <- gsub("warehan|warwham", "wareham", dfx$donor_city)
dfx$donor_city <- gsub("walthan", "waltham", dfx$donor_city)
dfx$donor_city <- gsub("welster", "webster", dfx$donor_city)
dfx$donor_city <- gsub("westburo|westborough|westbourough|westburo|west borough", "westboro", dfx$donor_city)
dfx$donor_city <- gsub("wellsley hills|wellesley hills|wellesly|wellesly hill|wellsley hills", "wellesley", dfx$donor_city)
dfx$donor_city <- gsub("woercester|wocester|worchester", "worcester", dfx$donor_city)
dfx$donor_city <- gsub("wobum|worburn", "woburn", dfx$donor_city)
dfx$donor_city <- gsub("witinsville|whitensville", "whitinsville", dfx$donor_city)
dfx$donor_city <- gsub("^westward$|^westwod$", "westwood", dfx$donor_city)
dfx$donor_city <- gsub("westport point", "westport", dfx$donor_city)
dfx$donor_city <- gsub("wrenthem", "wrentham", dfx$donor_city)
# dfx$donor_city <- gsub("", "", dfx$donor_city)
# dfx$donor_city <- gsub("", "", dfx$donor_city)
# dfx$donor_city <- gsub("studbury", "sudbury", dfx$donor_city)
# dfx$donor_city <- gsub("stoenham", "stoneham", dfx$donor_city)


# Connecticut donor cities
dfx$donor_city <- gsub("brandford|bradford", "branford", dfx$donor_city)
dfx$donor_city <- gsub("cheshire court|^chesire$", "cheshire", dfx$donor_city)
dfx$donor_city <- gsub("^croton$", "groton", dfx$donor_city)
dfx$donor_city <- gsub("hardford", "hartford", dfx$donor_city)
dfx$donor_city <- gsub("farfield", "fairfield", dfx$donor_city)
dfx$donor_city <- gsub("glastonberry", "glastonbury", dfx$donor_city)
dfx$donor_city <- gsub("haddam", "hamden", dfx$donor_city)
dfx$donor_city <- gsub("mashantuckut|meshantucket", "mashantucket", dfx$donor_city)
dfx$donor_city <- gsub("^mstic$", "mystic", dfx$donor_city)
dfx$donor_city <- gsub("new cannan", "new canaan", dfx$donor_city)
dfx$donor_city <- gsub("new hanven", "new haven", dfx$donor_city)
dfx$donor_city <- gsub("^ns$", "north stonington", dfx$donor_city)
dfx$donor_city <- gsub("^putnan$", "putnam", dfx$donor_city)
dfx$donor_city <- gsub("redding ridge|reeding", "redding", dfx$donor_city)
dfx$donor_city <- gsub("sstamford", "stamford", dfx$donor_city)
dfx$donor_city <- gsub("waterfoed", "waterford", dfx$donor_city)
dfx$donor_city <- gsub("wilton", "wilson", dfx$donor_city)
dfx$donor_city <- gsub("windsor locks", "windsor", dfx$donor_city)
dfx$donor_city <- gsub("wethesfield", "wethersfield", dfx$donor_city)
dfx$donor_city <- gsub("storrs mansfield", "storrs", dfx$donor_city)
dfx$donor_city <- gsub("trumbill|trumsoll", "trumbull", dfx$donor_city)
dfx$donor_city <- gsub("vernon rockville", "vernon", dfx$donor_city)
dfx$donor_city <- gsub("waterbury center", "waterbury", dfx$donor_city)


# Change Massachusettes states to ma
dfx$donor_st[dfx$donor_city %in% c("attleboro","south attleboro","north attleboro","boston","cambridge","somerville","somerset",
                                   "east boston","lunenburg","walpole","wellesley","bellingham","deerfield","new bedford","nantucket","winchester",
                                   "worcester","east taunton","provincetown","newton","seekonk","fall river","westport","swansea",
                                   "weymouth","north dartmouth","needham") &
                  dfx$donor_st == "ri"] <- "ma"

dfx$emp_st[dfx$emp_city %in% c("attleboro","south attleboro","north attleboro","boston","cambridge","somerville","somerset",
                               "east boston","lunenburg","walpole","wellesley","bellingham","deerfield","new bedford","nantucket","winchester",
                               "worcester","east taunton","provincetown","newton","seekonk","fall river","westport","swansea",
                               "weymouth","north dartmouth","needham") &
                dfx$emp_st == "ri"] <- "ma"


# change rhode island states to ri
dfx$donor_st[dfx$donor_city %in% c("east greenwich","newport","tiverton","warwick") &
                  dfx$donor_st == "ma"] <- "ri"

dfx$emp_st[dfx$emp_city %in% c("east greenwich","newport","tiverton","warwick") &
                dfx$emp_st == "ma"] <- "ri"


# Change connecticut states to ct
dfx$donor_st[dfx$donor_city %in% c("bloomfield","new london","hartford","superior ct",
                                   "west haven") &
                  dfx$donor_st == "ri"] <- "ct"

dfx$emp_st[dfx$emp_city %in% c("bloomfield","new london","hartford","superior ct",
                               "west haven") &
                dfx$emp_st == "ri"] <- "ct"

# Change ri states to co
dfx$donor_st[dfx$donor_city %in% c("denver") &
                  dfx$donor_st == "ri"] <- "co"

dfx$emp_st[dfx$emp_city %in% c("denver") &
                dfx$emp_st == "ri"] <- "co"


# Change california states to ca
dfx$donor_st[dfx$donor_city %in% c("san francisco","san diego") &
                  dfx$donor_st == "ri"] <- "ca"

dfx$emp_st[dfx$emp_city %in% c("san francisco","san diego") &
                dfx$emp_st == "ri"] <- "ca"


# Change New York states to ny
dfx$donor_st[dfx$donor_city %in% c("new york","rochester","saratoga springs") &
                  dfx$donor_st == "ri"] <- "ny"

dfx$emp_st[dfx$emp_city %in% c("new york","rochester","saratoga springs") &
                dfx$emp_st == "ri"] <- "ny"

# Change Texas states to tx
dfx$donor_st[dfx$donor_city %in% c("houston") &
                  dfx$donor_st == "ri"] <- "tx"

dfx$emp_st[dfx$emp_city %in% c("houston") &
                dfx$emp_st == "ri"] <- "tx"


# Change Florida states to fl
dfx$donor_st[dfx$donor_city %in% c("boca raton","orlando") &
                  dfx$donor_st == "ri"] <- "fl"

dfx$emp_st[dfx$emp_city %in% c("boca raton","orlando") &
                dfx$emp_st == "ri"] <- "fl"



# Read in state lookup codes
stateLook <- read.csv(paste(dir, "state_codes.csv", sep = ""), colClasses = "character"); str(stateLook)

# Join state names
dfx <- dfx %>% left_join(rename(stateLook, emp_state_name = state_name), by = c("emp_st" = "st_code")) %>% 
     left_join(rename(stateLook, donor_state_name = state_name), by = c("donor_st" = "st_code"))

# Clear
rm(stateLook)

sort(unique(dfx$donor_city[dfx$donor_st == "ri"]))
sort(unique(dfx$emp_city[dfx$emp_st == "ri"]))

sort(unique(dfx$emp_city[dfx$emp_st == "ma"]))
sort(unique(dfx$donor_city[dfx$donor_st == "ma"]))
# 
# 
# 
# x <- dfx %>% 
#      filter(emp_st == "ri") %>% 
#      group_by(emp_city,emp_st) %>% 
#      summarise(Amount = sum(Amount)) %>% 
#      ungroup() %>% mutate(TotAmount = sum(Amount)) %>% mutate(Pct_Amount = round(Amount / TotAmount, 5)) %>% 
#      arrange(desc(Amount)) %>% mutate(Dol = monify(Amount))


# Add region of country
dfx$donor_region <- case_when(dfx$donor_st %in% c("nh","me","vt") ~ "Other New England",
                              dfx$donor_st %in% c("mi","nd","sd","ne","ka","mo","mn","ia","wi","il","in","oh","ks") ~ "Midwest",
                              dfx$donor_st %in% c("wa","or","id","mt","wy") ~ "Northwest",
                              dfx$donor_st %in% c("tx","ok","ar","la","ms","al","tn","ky","wv","md","ka","de","va","nc","sc","ga","fl") ~ "The South",
                              dfx$donor_st %in% c("hi","ak","gu","vi","pr") ~ "US Territories & Non-Continental States",
                              dfx$donor_st %in% c("az","nm","co","ut","nv") ~ "Southwest",
                              is.na(dfx$donor_st) | dfx$donor_st == "" ~ "Other",
                              TRUE ~ str_to_title(dfx$donor_state_name)); table(dfx$donor_region)
# Add region of country
dfx$emp_region <- case_when(dfx$emp_st %in% c("nh","me","vt") ~ "Other New England",
                              dfx$emp_st %in% c("mi","nd","sd","ne","ka","mo","mn","ia","wi","il","in","oh","ks") ~ "Midwest",
                              dfx$emp_st %in% c("wa","or","id","mt","wy") ~ "Northwest",
                              dfx$emp_st %in% c("tx","ok","ar","la","ms","al","tn","ky","wv","md","ka","de","va","nc","sc","ga","fl") ~ "The South",
                              dfx$emp_st %in% c("hi","ak","gu","vi","pr") ~ "US Territories & Non-Continental States",
                              dfx$emp_st %in% c("az","nm","co","ut","nv") ~ "Southwest",
                              is.na(dfx$emp_st) | dfx$emp_st == "" ~ "Other",
                              TRUE ~ str_to_title(dfx$emp_state_name)); table(dfx$emp_region)

# x <- dfx %>% 
#      group_by(emp_city, emp_st, emp_zip) %>% 
#      summarise(Donors = n_distinct(FullName),
#                Employers = n_distinct(EmployerName),
#                Orgs = n_distinct(OrganizationName),
#                Total = sum(Amount, na.rm = TRUE),
#                Dollars = monify(Total),
#                Avg = mean(Amount, na.rm = TRUE)) %>% 
#      ungroup() %>% 
#      arrange(emp_st, emp_city, desc(Total))#; View(x)


# ****************************************************************************************************************************
# ******************************************            Format              **************************************************
# ******************************************   Political Action Committees  **************************************************
# ******************************************       & Organizations          **************************************************
# ****************************************************************************************************************************



dfx$OrganizationName <- gsub("\\s{2,}", " ", dfx$OrganizationName)
dfx$OrganizationName <- gsub("rhode island", "ri", dfx$OrganizationName)
dfx$OrganizationName <- gsub("amicari pac", "amica ri pac", dfx$OrganizationName)
dfx$OrganizationName <- gsub("ciri pac", "ciri pac (construction industries of ri)", dfx$OrganizationName)
dfx$OrganizationName <- gsub("coca-cola refreshments us ri nonpartisan pac for good govt", "coca-cola refreshments usa ri nonpartisan pac for good govt", dfx$OrganizationName)
dfx$OrganizationName <- gsub("cvs health ri pac (frmly cvscaremark corporation ri pac)|cvscaremark corporation ri pac|cvs caremark corporation ri pac|cvs ri pac", "cvs health pac (frmly cvscaremark corporation ri pac)", dfx$OrganizationName)
dfx$OrganizationName <- gsub("eds ri pac", "eds ri pac (electronic data system)", dfx$OrganizationName)
dfx$OrganizationName <- gsub("federal express ri state pac cmte", "federal express ri state pac", dfx$OrganizationName)
dfx$OrganizationName <- gsub("^dd provider pac$", "dd provider pac (democracy data)", dfx$OrganizationName)
dfx$OrganizationName <- gsub("hari pac$", "hari pac (hospital association of ri)", dfx$OrganizationName)
dfx$OrganizationName <- gsub("ri n.a.g.e.i.b.p.o. pac", "ri nageibpo pac (national assoc of gov employinternational brotherhood)", dfx$OrganizationName)
dfx$OrganizationName <- gsub("rioa pac$", "rioa pac (ri optometric association)", dfx$OrganizationName)
dfx$OrganizationName <- gsub("risa pac$", "risa pac (ri society anesthesiologist)", dfx$OrganizationName)
dfx$OrganizationName <- gsub("roberts,carrol,peirce pac", "roberts carrol feldstein peirce pac", dfx$OrganizationName)
dfx$OrganizationName <- gsub("twin river pac (formerly lincoln park pac)|^lincoln park pac", "twin river pac", dfx$OrganizationName)
dfx$OrganizationName <- gsub("ydri pac", "young democrats of ri pac", dfx$OrganizationName)
dfx$OrganizationName <- gsub("^hope now pac", "gina pac (frmly hope now pac)", dfx$OrganizationName)
dfx$OrganizationName <- gsub("rigoa pac", "rigoa pac (ri greyhound owners association)", dfx$OrganizationName)
dfx$OrganizationName <- gsub("hinckley allen & snyder pac|hinckley allen snyder ri pac|hinckley,allen,snyder pac", "hinckley, allen & snyder pac", dfx$OrganizationName)
#dfx$OrganizationName <- gsub("", "", dfx$OrganizationName)
#dfx$OrganizationName <- gsub("", "", dfx$OrganizationName)
#dfx$OrganizationName <- gsub("", "", dfx$OrganizationName)
#dfx$OrganizationName <- gsub("", "", dfx$OrganizationName)
#dfx$OrganizationName <- gsub("", "", dfx$OrganizationName)
#dfx$OrganizationName <- gsub("", "", dfx$OrganizationName)
#dfx$OrganizationName <- gsub("", "", dfx$OrganizationName)


pacs <- sort(grep(" pac$| pac |political action|pac$", dfx$OrganizationName, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print

dfx$PAC <- ifelse(dfx$OrganizationName %in% pacs, "PAC", "Not PAC")



# *************************************************************************************************************************
# ******************************************                          *****************************************************
# ******************************************   Format Employer Names  *****************************************************
# ******************************************                          *****************************************************
# *************************************************************************************************************************



# Format
dfx$EmployerName <- gsub("advovacy","advocacy", dfx$EmployerName)
dfx$EmployerName <- gsub(" corp$", " corporation", dfx$EmployerName)
dfx$EmployerName <- gsub(" inc$", "", dfx$EmployerName)
dfx$EmployerName <- gsub("assocation", "association", dfx$EmployerName)
dfx$EmployerName <- gsub(" llc$", "", dfx$EmployerName)
dfx$EmployerName <- gsub(" ltd$", "", dfx$EmployerName)
dfx$EmployerName <- gsub("\\s\\s", " ", dfx$EmployerName)
dfx$EmployerName <- gsub("\\/", " ", dfx$EmployerName)
dfx$EmployerName <- gsub(" \\* ", " ", dfx$EmployerName)
dfx$EmployerName <- gsub("\\-", " ", dfx$EmployerName)
dfx$EmployerName <- gsub("\\+", "and", dfx$EmployerName)
dfx$EmployerName <- gsub(" dept$", " department", dfx$EmployerName)
dfx$EmployerName <- gsub("dept of", "department of", dfx$EmployerName)
#dfx$EmployerName <- gsub("", "", dfx$EmployerName)


# Consolidate employer names   
blues <- grep("blue cross|bcbs|bleu cross|bule cross|belu cross|blue cors", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% blues] <- "blue cross blue shield"; dfx$Industry[dfx$EmployerName %in% blues] <- "Health Insurance"; rm(blues)
women <- grep("^Women and|^Woman and|^Womens a|wih|^w and i|^wi hosp|^w i ", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% women] <- "women and infants hospital"; dfx$Industry[dfx$EmployerName %in% women] <- "Hospital"; rm(women)
care <- grep("^care ne$|^care new|care ne home|^c n e|^care nwe|^cne ", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% care] <- "care new england"; dfx$Industry[dfx$EmployerName %in% care] <- "Hospital System"; rm(care)
kent <- grep("kent hospital|kent county hosp|kent county mem hosp|kent conty memorial hosp|kent country hosp|kent count hosp|kent county memorial hsopital", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% kent] <- "kent county hospital"; dfx$Industry[dfx$EmployerName %in% kent] <- "Hospital"; rm(kent)
butler <- grep("butler hosp", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% butler] <- "butler hospital"; dfx$Industry[dfx$EmployerName %in% butler] <- "Hospital"; rm(butler)
mem <- grep("^memorial hosp|^memorial hoapital|^memorial hosspital|sturdy memorial|pawtucket memorial|strudy memorial", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mem] <- "memorial hospital"; dfx$Industry[dfx$EmployerName %in% mem] <- "Hospital"; rm(mem)
tpc <- grep("the providence cent|providence center", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% tpc] <- "the providence center"; dfx$Industry[dfx$EmployerName %in% tpc] <- "Mental Health"; rm(tpc)
cne_vna <- grep("vna care new|vna of care new", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% cne_vna] <- "vna of care new england"; dfx$Industry[dfx$EmployerName %in% cne_vna] <- "Visiting Nurse"; rm(cne_vna)
codac <- grep("codac", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% codac] <- "codac behavioral health"; dfx$Industry[dfx$EmployerName %in% codac] <- "Healthcare"; rm(codac)
hasbrChild <- grep("^hasbro childrens hospital|hasbro hospital|ri hospital hasbro", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% hasbrChild] <- "hasbro childrens hospital"; dfx$Industry[dfx$EmployerName %in% hasbrChild] <- "Hospital"; rm(hasbrChild)
brad <- grep("bradley hosp|bradely hosp", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% brad] <- "bradley hospital"; dfx$Industry[dfx$EmployerName %in% brad] <- "Hospital"; rm(brad)
miriam <- grep("miriam hosp|miriam hiosp|miriam anesth|miriam immun|miriam anes inc phys|miriam cardio", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% miriam] <- "miriam hospital"; dfx$Industry[dfx$EmployerName %in% miriam] <- "Hospital"; rm(miriam)
rihosp <- grep("ri hospital|neurologist ri hospital|lifespan ri hospital|uemf ri hospital", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
hasbro_wih <- grep("women and infants ri hospital|ri hospital hasbro|ri hospitality", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% rihosp & !dfx$EmployerName %in% hasbro_wih] <- "ri hospital"; dfx$Industry[dfx$EmployerName %in% rihosp] <- "Hospital"; rm(rihosp,hasbro_wih)
newhosp <- grep("newport hospital$|newport hospital lifespan", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% newhosp] <- "newport hospital"; dfx$Industry[dfx$EmployerName %in% newhosp] <- "Hospital"; rm(newhosp)
lifeSpan <- grep("gateway health|gateway mental|ateway partners|^lifespan&|^lifespan corp|^lifespan practice|^lifespan risk", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% lifeSpan] <- "lifespan"; dfx$Industry[dfx$EmployerName %in% lifeSpan] <- "Hospital System"; rm(lifeSpan)
partners <- sort(grep("partners health|^mass gen|brigham and", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
dfx$Employer[dfx$EmployerName %in% partners] <- "partners healthcare"; dfx$Industry[dfx$EmployerName %in% partners] <- "Healthcare"; rm(partners)
# x <- sort(grep("^mental| mental|behavioral", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
# dfx$Employer[dfx$EmployerName %in% x] <- "x"; dfx$Industry[dfx$EmployerName %in% x] <- "x"; rm(x)
# commMental <- sort(grep("com mental health|community mental health", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
# dfx$Employer[dfx$EmployerName %in% x] <- "x"; dfx$Industry[dfx$EmployerName %in% x] <- "x"; rm(x)
# x <- sort(grep("treatment", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
# dfx$Employer[dfx$EmployerName %in% x] <- "x"; dfx$Industry[dfx$EmployerName %in% x] <- "x"; rm(x)
stateRI <- grep("^state of r", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% stateRI] <- "state of ri"; dfx$Industry[dfx$EmployerName %in% stateRI] <- "State Government"; rm(stateRI)
cityProv <- grep("^city of pr|providence city|town of providence", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% cityProv] <- "city of providence"; dfx$Industry[dfx$EmployerName %in% cityProv] <- "Local Government"; rm(cityProv)
cityCran <- grep("^city of cra|cranston city", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% cityCran] <- "city of cranston"; dfx$Industry[dfx$EmployerName %in% cityCran] <- "Local Government"; rm(cityCran)
cityWar <- grep("^city of war|warwick city|town of warw|warwick town", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% cityWar] <- "city of warwick"; dfx$Industry[dfx$EmployerName %in% cityWar] <- "Local Government"; rm(cityWar)
cityPaw <- grep("^city of pawt|pawtucket city|pawt city|pawt town|pawtucket town|town of pawtuc", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% cityPaw] <- "city of pawtucket"; dfx$Industry[dfx$EmployerName %in% cityPaw] <- "Local Government"; rm(cityPaw)
townNP  <- sort(grep("town of north prov|town of no prov$|town of n prov|town ofnorth prov|north providence city|north providence town", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
jb  <- sort(grep("jaybee industries", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% townNP & !dfx$EmployerName %in% jb] <- "town of north providence"; dfx$Industry[dfx$EmployerName %in% townNP & !dfx$EmployerName %in% jb] <- "Local Government"; rm(townNP,jb)
townScit  <- sort(grep("town of scit|scituate town", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% townScit] <- "town of scituate"; dfx$Industry[dfx$EmployerName %in% townScit] <- "Local Government"; rm(townScit)
townJohn  <- sort(grep("town of johnson|town of johnston$|town of johstown|town ofjohnston|johnston town|city of johnston|johnston city", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% townJohn] <- "town of johnston"; dfx$Industry[dfx$EmployerName %in% townJohn] <- "Local Government"; rm(townJohn)
townEG <- sort(grep("^town of east greenwich|town of e greenwich|^eg town|^e greenwich town|east greenwich town|town of eg|city of eg", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
dfx$Employer[dfx$EmployerName %in% townEG] <- "town of east greenwich"; dfx$Industry[dfx$EmployerName %in% townEG] <- "Local Government"; rm(townEG)
townNK <- sort(grep("^town of north king|^town of no king|^nk town|north kingstown town", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
dfx$Employer[dfx$EmployerName %in% townNK] <- "town of north kingstown"; dfx$Industry[dfx$EmployerName %in% townNK] <- "Local Government"; rm(townNK)
townLinc <- sort(grep("^town of lincoln|^town of lincolnri|lincoln town|city of lincoln|lincoln city", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
dfx$Employer[dfx$EmployerName %in% townLinc] <- "town of lincoln"; dfx$Industry[dfx$EmployerName %in% townLinc] <- "Local Government"; rm(townLinc)
townSmith <- sort(grep("^town of smithfield|^smithfield town", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
dfx$Employer[dfx$EmployerName %in% townSmith] <- "town of smithfield"; dfx$Industry[dfx$EmployerName %in% townSmith] <- "Local Government"; rm(townSmith)
townSK <- grep("^town of south kings|town of sk$|^sk town|south kingstown high|^south kingstown public|^s kingstown school|^south kingstown$", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% townSK] <- "town of south kingston"; dfx$Industry[dfx$EmployerName %in% townSK] <- "Local Government"; rm(townSK)
townNS <- grep("^town of north smithfield|town of north simthfield|north smithfield town|n smithfield town", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% townNS] <- "town of north smithfield"; dfx$Industry[dfx$EmployerName %in% townNS] <- "Local Government"; rm(townNS)
# 
townBurr <- sort(grep("^town of burr|burrilville town|burrillville town", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
dfx$Employer[dfx$EmployerName %in% townBurr] <- "town of burrillville"; dfx$Industry[dfx$EmployerName %in% townBurr] <- "Local Government"; rm(townBurr)
townBar <- grep("^town of bar|barrington town|barrington city|city of barrington", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% townBar] <- "town of barrington"; dfx$Industry[dfx$EmployerName %in% townBar] <- "Local Government"; rm(townBar)
# townEP <- sort(grep("^e provid|east provid|eproviden", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
# dfx$Employer[dfx$EmployerName %in% townEP] <- "town of east providence"; dfx$Industry[dfx$EmployerName %in% townEP] <- "Local Government"; rm(townEP)
# townCov <- sort(grep("^town of | town|^twon of| twon", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
# dfx$Employer[dfx$EmployerName %in% townCov] <- "town of coventry"; dfx$Industry[dfx$EmployerName %in% townCov] <- "Local Government"; rm(townCov)
# townCum <- sort(grep("^town of ", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
# dfx$Employer[dfx$EmployerName %in% townCov] <- "town of cumberland"; dfx$Industry[dfx$EmployerName %in% townCov] <- "Local Government"; rm(townCov)
# townWoon <- sort(grep("^town of ", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
# dfx$Employer[dfx$EmployerName %in% townWoon] <- "town of woonsocket"; dfx$Industry[dfx$EmployerName %in% townWoon] <- "Local Government"; rm(townWoon)
# townPort <- sort(grep("^town of ", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
# dfx$Employer[dfx$EmployerName %in% townPort] <- "town of portsmouth"; dfx$Industry[dfx$EmployerName %in% townPort] <- "Local Government"; rm(townPort)
# townMid <- sort(grep("^town of ", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
# dfx$Employer[dfx$EmployerName %in% townMid] <- "town of middletown"; dfx$Industry[dfx$EmployerName %in% townMid] <- "Local Government"; rm(townMid)
# townNewp <- sort(grep("^town of ", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
# dfx$Employer[dfx$EmployerName %in% townNewp] <- "city of newport"; dfx$Industry[dfx$EmployerName %in% townNewp] <- "Local Government"; rm(townNewp)
# townBri <- sort(grep("^town of ", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
# dfx$Employer[dfx$EmployerName %in% townBri] <- "town of bristol"; dfx$Industry[dfx$EmployerName %in% townBri] <- "Local Government"; rm(townBri)
# townWar <- sort(grep("^town of ", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
# dfx$Employer[dfx$EmployerName %in% townWar] <- "town of warren"; dfx$Industry[dfx$EmployerName %in% townWar] <- "Local Government"; rm(townWar)
# townWW <- sort(grep("^town of ww|town of west warwick", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
# dfx$Employer[dfx$EmployerName %in% townWW] <- "town of west warwick"; dfx$Industry[dfx$EmployerName %in% townWW] <- "Local Government"; rm(townWW)
# town <- sort(grep("town", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
# dfx$Employer[dfx$EmployerName %in% town] <- "town of "; dfx$Industry[dfx$EmployerName %in% town] <- "Local Government"; rm(town)
# 
# 


notEmployed <- grep("not employed|not-employed|unemployed|unemplyed|no employer|^none$", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% notEmployed] <- "unemployed"; dfx$Industry[dfx$EmployerName %in% notEmployed] <- "Unemployed"; rm(notEmployed)
self <- sort(grep("^self|self employed|self-employed", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% self] <- "self employed"; dfx$Industry[dfx$EmployerName %in% self] <- "Self Employed"; rm(self)
twin <- sort(grep("^twin river$|^twin river m|^twin river w|twin river casino|^twin rivers", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% twin] <- "twin river"; dfx$Industry[dfx$EmployerName %in% twin] <- "Gambling"; rm(twin)
cox <- sort(grep("^cox", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% cox] <- "cox communications"; dfx$Industry[dfx$EmployerName %in% cox] <- "Communications"; rm(cox)
land <- sort(grep("landmark med", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% land] <- "landmark medical"; dfx$Industry[dfx$EmployerName %in% land] <- "Hospital"; rm(land)
coast <- sort(grep("coastal me", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% coast] <- "coastal medical"; dfx$Industry[dfx$EmployerName %in% coast] <- "Primary Care"; rm(coast)
prospect <- sort(grep("charteracre|chartercare|prospect med", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% prospect] <- "prospect medical"; dfx$Industry[dfx$EmployerName %in% prospect] <- "Hospital"; rm(prospect)
ric <- sort(grep("^ri college| ri college$", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% ric] <- "ri college"; dfx$Industry[dfx$EmployerName %in% ric] <- "Higher Education"; rm(ric)
brown <- sort(grep("^Brown u|^brown$", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% brown] <- "brown university"; dfx$Industry[dfx$EmployerName %in% brown] <- "Higher Education"; rm(brown)
retired <- sort(grep("retired", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% retired] <- "retired"; dfx$Industry[dfx$EmployerName %in% retired] <- "Retired"; rm(retired)
uri <- sort(grep("university of ri|^uri$|^uri ", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% uri] <- "university of ri"; dfx$Industry[dfx$EmployerName %in% uri] <- "Higher Education"; rm(uri)
adler <- sort(grep("adler p|alder p|lock and she|ap and s|^aps$", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% adler] <- "adler pollock and sheehan"; dfx$Industry[dfx$EmployerName %in% adler] <- "Attorneys & Lawyers"; rm(adler)
citizens <- sort(grep("^citizens b|^citizens fi|^citizens$|rbs citizens", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% citizens] <- "citizens bank"; dfx$Industry[dfx$EmployerName %in% citizens] <- "Finance"; rm(citizens)
cvs <- sort(grep("cvs headquarters|cvs attorney|cvs health|cvs pharm|cvs corp|cvs  cfo|cvs cfo|cvs care|cvscare|^cvs$| cvs$", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% cvs] <- "CVS"; dfx$Industry[dfx$EmployerName %in% cvs] <- "Pharmacy"; rm(cvs)
yale <- sort(grep("yale|yake", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% yale] <- "yale"; dfx$Industry[dfx$EmployerName %in% yale] <- "Higher Education"; rm(yale)
kent <- sort(grep("^kent county hosp|^kent county mem|^kent hosp|^kent county mental", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% kent] <- "kent hospital"; dfx$Industry[dfx$EmployerName %in% kent] <- "Hospital"; rm(kent)
memorial <- sort(grep("^memorial h|sturdy|lawrence and memorial", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% memorial] <- "memorial hospital"; dfx$Industry[dfx$EmployerName %in% memorial] <- "Hospital"; rm(memorial)
verizon <- sort(grep("verizon", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% verizon] <- "verizon"; dfx$Industry[dfx$EmployerName %in% verizon] <- "Communications"; rm(verizon)
uOrth <- sort(grep("^university ortho$|^university orthopedic|^university orthropedics|^university orthopae", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% uOrth] <- "university orthopedics"; dfx$Industry[dfx$EmployerName %in% uOrth] <- "Orthopedics"; rm(uOrth)
rwu <- sort(grep("Roger Williams u", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% rwu] <- "roger williams university"; dfx$Industry[dfx$EmployerName %in% rwu] <- "Higher Education"; rm(rwu)
pfizer <- sort(grep("pfizer", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% pfizer] <- "pfizer"; dfx$Industry[dfx$EmployerName %in% pfizer] <- "Pharmaceutical"; rm(pfizer)
petrarca <- sort(grep("petrarca and p|petrarca l", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% petrarca] <- "petrarca law offices"; dfx$Industry[dfx$EmployerName %in% petrarca] <- "Attorneys & Lawyers"; rm(petrarca)
peregrine <- sort(grep("peregrine|pereigrine", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% peregrine] <- "peregrine group"; dfx$Industry[dfx$EmployerName %in% peregrine] <- "Real Estate"; rm(peregrine)
paulWeiss <- sort(grep("paul weiss|paul wess", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% paulWeiss] <- "paul weiss rifkind wharton and garrison"; dfx$Industry[dfx$EmployerName %in% paulWeiss] <- "Attorneys & Lawyers"; rm(paulWeiss)
patti <- sort(grep("pattie doyle|patti doyle", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% patti] <- "patti doyle communications"; dfx$Industry[dfx$EmployerName %in% patti] <- "Communications"; rm(patti)
parSnow <- sort(grep("partridge snow|partridge show|patridge snow|show and hahn|snow and hahn", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% parSnow] <- "partridge snow and hahn"; dfx$Industry[dfx$EmployerName %in% parSnow] <- "Attorneys & Lawyers"; rm(parSnow)
pannone <- sort(grep("pannone lopes|panone|panonne|pennone|lopes and dever", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% pannone] <- "pannone lopes and devereaux"; dfx$Industry[dfx$EmployerName %in% pannone] <- "Attorneys & Lawyers"; rm(pannone)
pacGas <- sort(grep("pacific gas|pac gas", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% pacGas] <- "pacific gas and electric"; dfx$Industry[dfx$EmployerName %in% pacGas] <- "Fossil Fuel"; rm(pacGas)
nick <- sort(grep("mattiello|matiello|mattielo", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% nick] <- "nicholas matiello"; dfx$Industry[dfx$EmployerName %in% nick] <- "Attorneys & Lawyers"; rm(nick)
nhp <- sort(grep("neighborhood heal|neighborhood h p|nhp", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% nhp] <- "neighborhood health plan"; dfx$Industry[dfx$EmployerName %in% nhp] <- "Health Insurance"; rm(nhp)
mandell <- sort(grep("mandell schwartz", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mandell] <- "mandell schwartz and boisclair"; dfx$Industry[dfx$EmployerName %in% mandell] <- "Attorneys & Lawyers"; mandell
gtech <- sort(grep("^igt|gtech|^g tech", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% gtech] <- "igt gtech"; dfx$Industry[dfx$EmployerName %in% gtech] <- "Technology"; rm(gtech)
darEverett <- sort(grep("darrowever|darrowand|darrow |darren everett|darrow and ever|darowever", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% darEverett] <- "darrow everett llp"; dfx$Industry[dfx$EmployerName %in% darEverett] <- "Attorneys & Lawyers"; rm(darEverett)
lynchPine <- sort(grep("lynch and pine", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% lynchPine] <- "lynch and pine"; dfx$Industry[dfx$EmployerName %in% lynchPine] <- "Attorneys & Lawyers"; rm(lynchPine)
jonesKelleher <- sort(grep("jones kelleher", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% jonesKelleher] <- "jones and kelleher"; dfx$Industry[dfx$EmployerName %in% jonesKelleher] <- "Attorneys & Lawyers"; rm(jonesKelleher)
delta <- sort(grep("delta dent", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% delta] <- "delta dental"; dfx$Industry[dfx$EmployerName %in% delta] <- "Dental"; rm(delta)
aaa <- sort(grep("aaa$|aaa-sne|aaa southern|aaa northeast$|aaa southeast|aaa new england", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% aaa] <- "aaa"; dfx$Industry[dfx$EmployerName %in% aaa] <- "Transportation"; rm(aaa)
feld <- sort(grep("feldstein|^roberts ca|robets carrol|roll feld|stein and pierce$", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% feld] <- "roberts carroll feldstein and pierce"; dfx$Industry[dfx$EmployerName %in% feld] <- "Attorneys & Lawyers"; rm(feld)
shan <- sort(grep("^duffy shan|^duffy and shan|^duffy and shanl|duffyshan|duffeyshan|^duffey shan|duffy and shanley", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% shan] <- "duffy shanley"; dfx$Industry[dfx$EmployerName %in% shan] <- "Attorneys & Lawyers"; rm(shan)
iacoi <- sort(grep("iacoi", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% iacoi] <- "calenda and iacoi"; dfx$Industry[dfx$EmployerName %in% iacoi] <- "Attorneys & Lawyers"; rm(iacoi)
macandrew <- sort(grep("macandrews", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% macandrew] <- "macandrews and forbes"; dfx$Industry[dfx$EmployerName %in% macandrew] <- "Finance"; rm(macandrew)
proct <- sort(grep("goodwin proctor|goodwin procter|^goodwin p|^goodwin and pr", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% proct] <- "goodwin proctor"; dfx$Industry[dfx$EmployerName %in% proct] <- "Attorneys & Lawyers"; rm(proct)
sullCrom <- sort(grep("sullivan and cromwell", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% sullCrom] <- "sullivan and cromwell"; dfx$Industry[dfx$EmployerName %in% sullCrom] <- "Attorneys & Lawyers"; rm(sullCrom)
hahn <- sort(grep("^partridge sw|partridge snow|partidge snow|snow and hahn", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% hahn] <- "patridge snow and hahn"; dfx$Industry[dfx$EmployerName %in% hahn] <- "Attorneys & Lawyers"; rm(hahn)
boies <- sort(grep("boies sch", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% boies] <- "boies schiller and flexner"; dfx$Industry[dfx$EmployerName %in% boies] <- "Attorneys & Lawyers"; rm(boies)
moses <- sort(grep("^moses a", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% moses] <- "moses afonso ryan"; dfx$Industry[dfx$EmployerName %in% moses] <- "Attorneys & Lawyers"; rm(moses)
marasco <- sort(grep("marasco|nesselbush", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% marasco] <- "marasco and nesselbush"; dfx$Industry[dfx$EmployerName %in% marasco] <- "Attorneys & Lawyers"; rm(marasco)
zarella <- sort(grep("^zarrella dev|zarella dev", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% zarella] <- "zarella development"; dfx$Industry[dfx$EmployerName %in% zarella] <- "Real Estate"; rm(zarella)
wistow <- sort(grep("wistow", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% wistow] <- "wistow barylick sheehan and loveley"; dfx$Industry[dfx$EmployerName %in% wistow] <- "Attorneys & Lawyers"; rm(wistow)
wilmer <- sort(grep("^wilmer|cutler picker|hale and door", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print 
dfx$Employer[dfx$EmployerName %in% wilmer] <- "wilmerhale"; dfx$Industry[dfx$EmployerName %in% wilmer] <- "Attorneys & Lawyers"; rm(wilmer)
wilStew <- sort(grep("williams and stuart", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% wilStew] <- "williams and stuart real estate"; dfx$Industry[dfx$EmployerName %in% wilStew] <- "Real Estate"; rm(wilStew)
wilFarrel <- sort(grep("william a farrell|william farrell|farrell assoc|farrell and assoc", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% wilFarrel] <- "william a farrell and associates"; dfx$Industry[dfx$EmployerName %in% wilFarrel] <- "Attorneys & Lawyers"; rm(wilFarrel)
whelan <- sort(grep("whelan corrente|whelan and corr", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% whelan] <- "whelan corrente flanders kinder and siket"; dfx$Industry[dfx$EmployerName %in% whelan] <- "Attorneys & Lawyers"; rm(whelan)
wellone <- sort(grep("wellone", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% wellone] <- "wellone primary medical and dental care"; dfx$Industry[dfx$EmployerName %in% wellone] <- "Healthcare"; rm(wellone)
weill <- sort(grep("weill cornell", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% weill] <- "weill cornell medicine"; dfx$Industry[dfx$EmployerName %in% weill] <- "Higher Education"; rm(weill)
washTrust <- sort(grep("washington trust|wash trust", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% washTrust] <- "washington trust"; dfx$Industry[dfx$EmployerName %in% washTrust] <- "Finance"; rm(washTrust)
wachtell <- sort(grep("wachtel", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% wachtell] <- "wachtell lipton rosen and kratz"; dfx$Industry[dfx$EmployerName %in% wachtell] <- "Attorneys & Lawyers"; rm(wachtell)
volante <- sort(grep("volante", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% volante] <- "volantes service station"; dfx$Industry[dfx$EmployerName %in% volante] <- "Autobody"; rm(volante)
boulderHill <- sort(grep("the preserve|boulder hill", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% boulderHill] <- "the preserve at boulder hills"; dfx$Industry[dfx$EmployerName %in% boulderHill] <- "Recreation"; rm(boulderHill)
overlook <- sort(grep("overlook", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% overlook] <- "overlook nursing home"; dfx$Industry[dfx$EmployerName %in% overlook] <- "Nursing Home"; rm(overlook)
eastgate <- sort(grep("eastgate|east gate", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% eastgate] <- "eastgate nursing home"; dfx$Industry[dfx$EmployerName %in% eastgate] <- "Nursing Home"; rm(eastgate)
westview <- sort(grep("west view|westview", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% westview] <- "west view nursing home"; dfx$Industry[dfx$EmployerName %in% westview] <- "Nursing Home"; rm(westview)
elmhurst <- sort(grep("elmhurst extend|elmhurst nurs|at elmhurst", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% elmhurst] <- "elmhurst nursing and rehab"; dfx$Industry[dfx$EmployerName %in% elmhurst] <- "Healthcare"; rm(elmhurst)
scallop <- sort(grep("^scallop shell|elderwood at|elderwood of", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% scallop] <- "scallop shell nursing and rehab"; dfx$Industry[dfx$EmployerName %in% scallop] <- "Nursing Home"; rm(scallop)
scNursing <- sort(grep("south county nurs", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% scNursing] <- "south county nursing and rehab"; dfx$Industry[dfx$EmployerName %in% scNursing] <- "Nursing Home"; rm(scNursing)
bucci <- sort(grep("bucci insurance|bucci ins", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% bucci] <- "bucci insurance associates"; dfx$Industry[dfx$EmployerName %in% bucci] <- "Insurance"; rm(bucci)
ricci <- sort(grep("thomas r ricci|thomas ricci| ricci esq|tom ricci", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% ricci] <- "thomas ricci esq"; dfx$Industry[dfx$EmployerName %in% ricci] <- "Attorneys & Lawyers"; rm(ricci)
abal <- sort(grep("abal check|^abal ", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% abal] <- "abal check cashing"; dfx$Industry[dfx$EmployerName %in% abal] <- "Payday Loan"; rm(abal)
adjM <- sort(grep("adj m", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% adjM] <- "Dunkin Donuts"; dfx$Industry[dfx$EmployerName %in% adjM] <- "Food & Beverage"; rm(adjM)
lusi <- sort(grep(" lusi|^lusi", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% lusi] <- "a f lusi construction"; dfx$Industry[dfx$EmployerName %in% lusi] <- "Construction"; rm(lusi)
gump <- sort(grep(" gump$|^akin gump|strauss hauer|hauer and feld", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% gump] <- "akin gump strauss hauer and feld"; dfx$Industry[dfx$EmployerName %in% gump] <- "Attorneys & Lawyers"; rm(gump)
albert <- sort(grep("^albert real|^albert relator", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% albert] <- "albert realty"; dfx$Industry[dfx$EmployerName %in% albert] <- "Realty"; rm(albert)
annaldo <- sort(grep("andrew annaldo|andrew j annaldo|annaldo and assoc|annaldo assoc|annaldo lobby|annaldo consult", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% annaldo] <- "andrew annaldo lobbyist/consultant"; dfx$Industry[dfx$EmployerName %in% annaldo] <- "Lobby"; rm(annaldo)
corvese <- sort(grep("arthur corvese|^arthur j c|^art corvese|corvese opt", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% corvese] <- "arthur corvese od"; dfx$Industry[dfx$EmployerName %in% corvese] <- "Optometry"; rm(corvese)
athenaSol <- sort(grep("^athena sol|athenasol", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% athenaSol] <- "athena solutions group"; dfx$Industry[dfx$EmployerName %in% athenaSol] <- "Lobby"; rm(athenaSol)
athenaHealth <- sort(grep("athena health|athenahealth", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% athenaHealth] <- "athenahealth"; dfx$Industry[dfx$EmployerName %in% athenaHealth] <- "Health Records"; rm(athenaHealth)
atom <- sort(grep("atom m", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% atom] <- "atom media group"; dfx$Industry[dfx$EmployerName %in% atom] <- "Media"; rm(atom)
autobodyCon <- sort(grep("auto body concepts|autobody concepts", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% autobodyCon] <- "autobody concepts"; dfx$Industry[dfx$EmployerName %in% autobodyCon] <- "Autobody"; rm(autobodyCon)
avalon <- sort(grep("avalon n", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% avalon] <- "avalon nursing home"; dfx$Industry[dfx$EmployerName %in% avalon] <- "Nursing Home"; rm(avalon)
baccala <- sort(grep("baccala|^bacala", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% baccala] <- "baccala concrete"; dfx$Industry[dfx$EmployerName %in% baccala] <- "Construction"; rm(baccala)
ballantine <- sort(grep("ballentine|ballantine", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% ballantine] <- "ballentine partners"; dfx$Industry[dfx$EmployerName %in% ballantine] <- "Finance"; rm(ballantine)
bayCrane <- sort(grep("bay crane|baycrane", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% bayCrane] <- "bay crane"; dfx$Industry[dfx$EmployerName %in% bayCrane] <- "Construction"; rm(bayCrane)
beaconMutual <- sort(grep("beacon mutual|beaconmutual|beacon ins", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% beaconMutual] <- "beacon mutual insurance"; dfx$Industry[dfx$EmployerName %in% beaconMutual] <- "Workers Comp Insurance"; rm(beaconMutual)
beretta <- sort(grep("^beretta", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% beretta] <- "beretta realty"; dfx$Industry[dfx$EmployerName %in% beretta] <- "Realty"; rm(beretta)
berkshirePlace <- sort(grep("berkshire place|berkshire nursing", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% berkshirePlace] <- "berkshire place nursing home"; dfx$Industry[dfx$EmployerName %in% berkshirePlace] <- "Nursing Home"; rm(berkshirePlace)
bessemer <- sort(grep("bessemer", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% bessemer] <- "bessemer venture partners"; dfx$Industry[dfx$EmployerName %in% bessemer] <- "Finance"; rm(bessemer)
betaGrp <- sort(grep("beta group|beta engin|beta grp", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% betaGrp] <- "beta group engineering"; dfx$Industry[dfx$EmployerName %in% betaGrp] <- "Engineering"; rm(betaGrp)
agniel <- sort(grep("agniel", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% agniel] <- "agniel commodities"; dfx$Industry[dfx$EmployerName %in% agniel] <- "Finance"; rm(agniel)
oralAssoc <- sort(grep("associates in oral", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% oralAssoc] <- "associates in oral surgery"; dfx$Industry[dfx$EmployerName %in% oralAssoc] <- "Oral Surgery"; rm(oralAssoc)
bianchi <- sort(grep("bianchi brouillard|bianchi and broullard|bianchi and brouillard", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% bianchi] <- "bianchi and brouillard"; dfx$Industry[dfx$EmployerName %in% bianchi] <- "Attorneys & Lawyers"; rm(bianchi)
bloomberg <- sort(grep("bloomberg", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% bloomberg] <- "bloomberg foundation"; dfx$Industry[dfx$EmployerName %in% bloomberg] <- "Finance"; rm(bloomberg)
bluedog <- sort(grep("bluedog|blue dog", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% bluedog] <- "bluedog capital partners"; dfx$Industry[dfx$EmployerName %in% bluedog] <- "Finance"; rm(bluedog)
boyleFogarty <- sort(grep("boyle and fogarty|boyle fogarty", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% boyleFogarty] <- "boyle and fogarty construction"; dfx$Industry[dfx$EmployerName %in% boyleFogarty] <- "Construction"; rm(boyleFogarty)
montefusco <- sort(grep("montefusco", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% montefusco] <- "delgrande and montefusco"; dfx$Industry[dfx$EmployerName %in% montefusco] <- "Accounting"; rm(montefusco)
bridgewater <- sort(grep("bridgewater|bridgwater", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # It's University
dfx$Employer[dfx$EmployerName %in% bridgewater] <- "bridgewater state university"; dfx$Industry[dfx$EmployerName %in% bridgewater] <- "Higher Education"; rm(bridgewater)
bristolMyers <- sort(grep("bristol-meyers|bristol mey|bristol my|yers squib", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% bristolMyers] <- "bristol myers squibb"; dfx$Industry[dfx$EmployerName %in% bristolMyers] <- "Pharmaceutical"; rm(bristolMyers)
broadrock <- sort(grep("broadrock|broad rock", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Renew
dfx$Employer[dfx$EmployerName %in% broadrock] <- "broadrock renewables"; dfx$Industry[dfx$EmployerName %in% broadrock] <- "Clean Energy"; rm(broadrock)
rudnick <- sort(grep("brown rudnick|brown and rudnick|brownrudnick|brwn rudnick|brwnrudnick|brodn rudnick", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% rudnick] <- "brown rudnick"; dfx$Industry[dfx$EmployerName %in% rudnick] <- "Attorneys & Lawyers"; rm(rudnick)
brownHarris  <- sort(grep("brown harris|harris steve", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Real estate
dfx$Employer[dfx$EmployerName %in% brownHarris] <- "brown harris stevens residential sales"; dfx$Industry[dfx$EmployerName %in% brownHarris] <- "Real Estate"; rm(brownHarris)
buildri <- sort(grep("buildri|build ri", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% buildri] <- "build ri"; dfx$Industry[dfx$EmployerName %in% buildri] <- "Construction"; rm(buildri)
burn <- sort(grep("^burn and lev|^burns and lev|^burn lev|^burns lev", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% burn] <- "burns and levinson"; dfx$Industry[dfx$EmployerName %in% burn] <- "Attorneys & Lawyers"; rm(burn)
butlerMessier <- sort(grep("butler and messier|butler messier|butlermess", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Real estate
dfx$Employer[dfx$EmployerName %in% butlerMessier] <- "butler and messier"; dfx$Industry[dfx$EmployerName %in% butlerMessier] <- "Insurance"; rm(butlerMessier)
kellyMancini <- sort(grep("^kelly and man|^kelly man|kelly and macini", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% kellyMancini] <- "kelly and mancini"; dfx$Industry[dfx$EmployerName %in% kellyMancini] <- "Attorneys & Lawyers"; rm(kellyMancini)
calvino <- sort(grep("john calvino|calvino law|calvino esq|calvino and assoc|calvino assoc", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Even john calvino
dfx$Employer[dfx$EmployerName %in% calvino] <- "calvino law associates"; dfx$Industry[dfx$EmployerName %in% calvino] <- "Attorneys & Lawyers"; rm(calvino)
mittleman <- sort(grep("mittleman|^cameron and|^cameron mitt", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mittleman] <- "cameron and mittleman"; dfx$Industry[dfx$EmployerName %in% mittleman] <- "Attorneys & Lawyers"; rm(mittleman)
capBenefit <- sort(grep("capital benefit|capitol benefit", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% capBenefit] <- "capital benefit partners"; dfx$Industry[dfx$EmployerName %in% capBenefit] <- "Consulting"; rm(capBenefit)
travelers <- sort(grep("capital city ins|traveler", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Travelers Insurance
dfx$Employer[dfx$EmployerName %in% travelers] <- "travelers insurance"; dfx$Industry[dfx$EmployerName %in% travelers] <- "Insurance"; rm(travelers)
capitalGood <- sort(grep("capital good|capitol good", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% capitalGood] <- "capital good fund"; dfx$Industry[dfx$EmployerName %in% capitalGood] <- "Finance"; rm(capitalGood)
capitolCity <- sort(grep("capital city g|capitol city g|capitalcity|capitolcity", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Beware the capital city vs capitol city rabbithole
dfx$Employer[dfx$EmployerName %in% capitolCity] <- "capitol city group"; dfx$Industry[dfx$EmployerName %in% capitolCity] <- "Lobby"; rm(capitolCity)
capriccio <- sort(grep("capriccio", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% capriccio] <- "capriccio bakery"; dfx$Industry[dfx$EmployerName %in% capriccio] <- "Food & Beverage"; rm(capriccio)
tomRicci <- sort(grep("r ricci|thomas ricci", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% tomRicci] <- "thomas ricci esq"; dfx$Industry[dfx$EmployerName %in% tomRicci] <- "Attorneys & Lawyers"; rm(tomRicci)
# Cardi construction vs Cardi foundation vs cardi furniture?
cardiCo <- sort(grep("cardi c|^cardi f", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% cardiCo] <- "cardi corporation"; dfx$Industry[dfx$EmployerName %in% cardiCo] <- "Construction"; rm(cardiCo)

carlWein <- sort(grep("carl weinberg|carlwein", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Accountants
dfx$Employer[dfx$EmployerName %in% carlWein] <- "carl weinberg and co"; dfx$Industry[dfx$EmployerName %in% carlWein] <- "Accounting"; rm(carlWein)
carlson <- sort(grep("^carlson", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Retirement planning - Finance
dfx$Employer[dfx$EmployerName %in% carlson] <- "carlson financial advisors"; dfx$Industry[dfx$EmployerName %in% carlson] <- "Finance"; rm(carlson)
carpionato <- sort(grep("carpionato|carpianato", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Carpionato group - real estate
dfx$Employer[dfx$EmployerName %in% carpionato] <- "carpionato group"; dfx$Industry[dfx$EmployerName %in% carpionato] <- "Real Estate"; rm(carpionato)
casaleAuto <- sort(grep("casale auto|casales auto|casaleauto|casalesauto", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% casaleAuto] <- "casale auto body"; dfx$Industry[dfx$EmployerName %in% casaleAuto] <- "Autobody"; rm(casaleAuto)
casalePaint <- sort(grep("casale paint|casales paint", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% casalePaint] <- "casale painting company"; dfx$Industry[dfx$EmployerName %in% casalePaint] <- "Construction"; rm(casalePaint)
cathedral <- sort(grep("property advisory g|cathedral dev", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Real estate
dfx$Employer[dfx$EmployerName %in% cathedral] <- "property advisory group"; dfx$Industry[dfx$EmployerName %in% cathedral] <- "Real Estate"; rm(cathedral)
cbre <- sort(grep("richard ellis|cbre", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Real estate
dfx$Employer[dfx$EmployerName %in% cbre] <- "cb richard ellis"; dfx$Industry[dfx$EmployerName %in% cbre] <- "Real Estate"; rm(cbre)
cbUtility <- sort(grep("cb util", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Construction
dfx$Employer[dfx$EmployerName %in% cbUtility] <- "cb utility company"; dfx$Industry[dfx$EmployerName %in% cbUtility] <- "Construction"; rm(cbUtility)
cedarCrest <- sort(grep("cedar crest|cedarcrest", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Nursing home
dfx$Employer[dfx$EmployerName %in% cedarCrest] <- "cedar crest nursing home"; dfx$Industry[dfx$EmployerName %in% cedarCrest] <- "Nursing Home"; rm(cedarCrest)
cerrone <- sort(grep("cerrone", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Cerrone Auto
dfx$Employer[dfx$EmployerName %in% cerrone] <- "cerrone auto sales"; dfx$Industry[dfx$EmployerName %in% cerrone] <- "Auto Sales"; rm(cerrone)

cervenka <- sort(grep("cervenka g", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Law
dfx$Employer[dfx$EmployerName %in% cervenka] <- "cervenka green ducharme antonelli"; dfx$Industry[dfx$EmployerName %in% cervenka] <- "Attorneys & Lawyers"; rm(cervenka)
cfoCon <- sort(grep("cfo con", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Rabbit hole - brett smiley
dfx$Employer[dfx$EmployerName %in% cfoCon] <- "cfo consulting"; dfx$Industry[dfx$EmployerName %in% cfoCon] <- "Consulting"; rm(cfoCon)
chaceRut <- sort(grep("chace ruttenberg|ruttenberg and|ruttenberg free", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Law firm
dfx$Employer[dfx$EmployerName %in% chaceRut] <- "chace ruttenberg and freedman"; dfx$Industry[dfx$EmployerName %in% chaceRut] <- "Attorneys & Lawyers"; rm(chaceRut)
charland <- sort(grep("^charland mar", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% charland] <- "charland marciano and co"; dfx$Industry[dfx$EmployerName %in% charland] <- "Accounting"; rm(charland)
charLynn <- sort(grep("lynn schusterman|schusterman foundation", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% charLynn] <- "charles and lynn schusterman foundation"; dfx$Industry[dfx$EmployerName %in% charLynn] <- "Philanthropy"; rm(charLynn)
charlesgate <- sort(grep("^charlesgate|charlesgate$|^charles gate", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% charlesgate] <- "charlesgate nursing center"; dfx$Industry[dfx$EmployerName %in% charlesgate] <- "Nursing Home"; rm(charlesgate)
checkMate <- sort(grep("checkmate|check mate", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% checkMate] <- "checkmate consulting group"; dfx$Industry[dfx$EmployerName %in% checkMate] <- "Consulting"; rm(checkMate)
chelseaP <- sort(grep("chelsea piers", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% chelseaP] <- "chelsea piers management"; dfx$Industry[dfx$EmployerName %in% chelseaP] <- "Recreation"; rm(chelseaP)
chisholm <- sort(grep("chishold chisholm|chisholm chisholm|chisholm and kirk|chisholm and kil|chisholm and chisholm|chisholm kirk|chisholm kil", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% chisholm] <- "chisholm chisholm and kilpatrick"; dfx$Industry[dfx$EmployerName %in% chisholm] <- "Attorneys & Lawyers"; rm(chisholm)
choate <- sort(grep("choate hall|hall and stewart", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% choate] <- "choate hall and stewart"; dfx$Industry[dfx$EmployerName %in% choate] <- "Attorneys & Lawyers"; rm(choate)
cBoyle <- sort(grep("christopher boyle|chris boyle|boyle esq|boyle law", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% cBoyle] <- "christopher boyle esq"; dfx$Industry[dfx$EmployerName %in% cBoyle] <- "Attorneys & Lawyers"; rm(cBoyle)
cMill <- sort(grep("millea", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% cMill] <- "christopher millea esq"; dfx$Industry[dfx$EmployerName %in% cMill] <- "Attorneys & Lawyers"; rm(cMill)
citadel <- sort(grep("citadel con|citadel$", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% citadel] <- "citadel consumer litigation"; dfx$Industry[dfx$EmployerName %in% citadel] <- "Attorneys & Lawyers"; rm(citadel)
cityKitty <- sort(grep("city kitty", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% cityKitty] <- "city kitty veterinary care"; dfx$Industry[dfx$EmployerName %in% cityKitty] <- "Veterinarian"; rm(cityKitty)
cuny <- sort(grep("cuny|university of new york", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% cuny] <- "city university of new york"; dfx$Industry[dfx$EmployerName %in% cuny] <- "Higher Education"; rm(cuny)
clearyGot <- sort(grep("steen", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% clearyGot] <- "cleary gottlieb steen and hamilton"; dfx$Industry[dfx$EmployerName %in% clearyGot] <- "Attorneys & Lawyers"; rm(clearyGot)
clifford <- sort(grep("clifford law", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% clifford] <- "clifford law offices"; dfx$Industry[dfx$EmployerName %in% clifford] <- "Attorneys & Lawyers"; rm(clifford)
clubDesireOwnership <- sort(grep("desire|beats", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% clubDesireOwnership] <- "club desire / equity beats"; dfx$Industry[dfx$EmployerName %in% clubDesireOwnership] <- "Nightlife"; rm(clubDesireOwnership)
coastReal <- sort(grep("coast real", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% coastReal] <- "coast realty management"; dfx$Industry[dfx$EmployerName %in% coastReal] <- "Real Estate"; rm(coastReal)

colardo <- sort(grep("colardo", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Is Chris & RJ one person or two?
dfx$Employer[dfx$EmployerName %in% colardo] <- "colardo law offices"; dfx$Industry[dfx$EmployerName %in% colardo] <- "Attorneys & Lawyers"; rm(colardo)

coloian <- sort(grep("coloian", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% coloian] <- "coloian law offices"; dfx$Industry[dfx$EmployerName %in% coloian] <- "Attorneys & Lawyers"; rm(coloian)
columbusDoor <- sort(grep("columbus door", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% columbusDoor] <- "columbus door company"; dfx$Industry[dfx$EmployerName %in% columbusDoor] <- "Construction"; rm(columbusDoor)
commonEngine <- sort(grep("commonwealth engineers", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% commonEngine] <- "commonwealth engineers and consulting"; dfx$Industry[dfx$EmployerName %in% commonEngine] <- "Engineering"; rm(commonEngine)
ccri <- sort(grep("ccri|college of ri|cc ri", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% ccri] <- "community college of ri"; dfx$Industry[dfx$EmployerName %in% ccri] <- "Higher Education"; rm(ccri)
consolidCol <- sort(grep("consolidated concrete|consolodated concrete", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% consolidCol] <- "consolidated concrete"; dfx$Industry[dfx$EmployerName %in% consolidCol] <- "Construction"; rm(consolidCol)
crmMod <- sort(grep("modular home|^crm", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # construction
dfx$Employer[dfx$EmployerName %in% crmMod] <- "construction and rehab modular homes"; dfx$Industry[dfx$EmployerName %in% crmMod] <- "Construction"; rm(crmMod)
cornishAssoc <- sort(grep("cornish a|cornish l", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Real estate
dfx$Employer[dfx$EmployerName %in% cornishAssoc] <- "cornish associates"; dfx$Industry[dfx$EmployerName %in% cornishAssoc] <- "Real Estate"; rm(cornishAssoc)
corvias <- sort(grep("corvias", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Real estate
dfx$Employer[dfx$EmployerName %in% corvias] <- "corvias group"; dfx$Industry[dfx$EmployerName %in% corvias] <- "Real Estate"; rm(corvias)
cosco <- sort(grep("cosco", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% cosco] <- "cosco corporation"; dfx$Industry[dfx$EmployerName %in% cosco] <- "Shipping"; rm(cosco)
covLumb <- sort(grep("coventry lumb", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% covLumb] <- "coventry lumber"; dfx$Industry[dfx$EmployerName %in% covLumb] <- "Construction"; rm(covLumb)
craMar <- sort(grep("cra mar ", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% craMar] <- "cra mar meadows nursing home"; dfx$Industry[dfx$EmployerName %in% craMar] <- "Nursing Home"; rm(craMar)
cranemere <- sort(grep("cranemere", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Capital market company
dfx$Employer[dfx$EmployerName %in% cranemere] <- "cranemere group"; dfx$Industry[dfx$EmployerName %in% cranemere] <- "Finance"; rm(cranemere)
cullion <- sort(grep("cullion", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # construction
dfx$Employer[dfx$EmployerName %in% cullion] <- "cullion construction"; dfx$Industry[dfx$EmployerName %in% cullion] <- "Construction"; rm(cullion)
curreri <- sort(grep("curreri|currieri", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # constructionval <- sort(grep("swaine", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% curreri] <- "curreri collision"; dfx$Industry[dfx$EmployerName %in% curreri] <- "Autobody"; rm(curreri)
customDes <- sort(grep("custom des|custum des", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% customDes] <- "custom design"; dfx$Industry[dfx$EmployerName %in% customDes] <- "Manufacturing"; rm(customDes)
daisyBas <- sort(grep("daisy bassen", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% daisyBas] <- "daisy bassen md psychiatrist"; dfx$Industry[dfx$EmployerName %in% daisyBas] <- "Psychiatry"; rm(daisyBas)
daley <- sort(grep(" orton", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% daley] <- "daley and orton"; dfx$Industry[dfx$EmployerName %in% daley] <- "Attorneys & Lawyers"; rm(daley)
dambra <- sort(grep("dambra", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% dambra] <- "dambra construction"; dfx$Industry[dfx$EmployerName %in% dambra] <- "Construction"; rm(dambra)
damianoIns <- sort(grep("damiano a|damiano ins", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% damianoIns] <- "damiano insurance agency"; dfx$Industry[dfx$EmployerName %in% damianoIns] <- "Insurance"; rm(damianoIns)
damianoCPA <- sort(grep("damiano steve|steven damiano", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% damianoCPA] <- "steven damiano cpa"; dfx$Industry[dfx$EmployerName %in% damianoCPA] <- "Accounting"; rm(damianoCPA)
damico <- sort(grep("damico eng", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% damico] <- "damico engineering technology"; dfx$Industry[dfx$EmployerName %in% damico] <- "Engineering"; rm(damico)
dAndD <- sort(grep("dnd|d and d con", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% dAndD] <- "d and d construction"; dfx$Industry[dfx$EmployerName %in% dAndD] <- "Construction"; rm(dAndD)
dAndH <- sort(grep("d and h|dandh", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% dAndH] <- "d and h collision"; dfx$Industry[dfx$EmployerName %in% dAndH] <- "Autobody"; rm(dAndH)
danHarrop <- sort(grep("harrop", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% danHarrop] <- "daniel harrop md"; dfx$Industry[dfx$EmployerName %in% danHarrop] <- "Psychiatry"; rm(danHarrop)
dansMgmt <- sort(grep("dans mana", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% dansMgmt] <- "Dunkin Donuts"; dfx$Industry[dfx$EmployerName %in% dansMgmt] <- "Food & Beverage"; rm(dansMgmt)
davenports <- sort(grep("davenports", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% davenports] <- "davenports restaurant"; dfx$Industry[dfx$EmployerName %in% davenports] <- "Food & Beverage"; rm(davenports)
weiz <- sort(grep("weizenbaum", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% weiz] <- "deluca and weizenbaum"; dfx$Industry[dfx$EmployerName %in% weiz] <- "Attorneys & Lawyers"; rm(weiz)
deanWare <- sort(grep("dean wa", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% deanWare] <- "dean warehousing services"; dfx$Industry[dfx$EmployerName %in% deanWare] <- "Warehousing"; rm(deanWare)
debMc <- sort(grep("mcinteer", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% debMc] <- "debbi mcinteer md"; dfx$Industry[dfx$EmployerName %in% debMc] <- "Psychiatry"; rm(debMc)
decof <- sort(grep("decof", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% decof] <- "decof decof and barry"; dfx$Industry[dfx$EmployerName %in% decof] <- "Attorneys & Lawyers"; rm(decof)
decotis <- sort(grep("decotis", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% decotis] <- "decotis insurance"; dfx$Industry[dfx$EmployerName %in% decotis] <- "Insurance"; rm(decotis)
delGrand <- sort(grep("montefusco", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% delGrand] <- "delgrande and montefusco"; dfx$Industry[dfx$EmployerName %in% delGrand] <- "Accounting"; rm(delGrand)
deltaMech <- sort(grep("delta mech", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% deltaMech] <- "delta mechanical contractors"; dfx$Industry[dfx$EmployerName %in% deltaMech] <- "HVAC"; rm(deltaMech)
dentons <- sort(grep("detnons|denton", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% dentons] <- "dentons"; dfx$Industry[dfx$EmployerName %in% dentons] <- "Attorneys & Lawyers"; rm(dentons)
detroitColl <- sort(grep("detroit", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% detroitColl] <- "detroit collision center"; dfx$Industry[dfx$EmployerName %in% detroitColl] <- "Autobody"; rm(detroitColl)
dipreteEng <- sort(grep("deprete eng|diprete eng", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% dipreteEng] <- "diprete engineering"; dfx$Industry[dfx$EmployerName %in% dipreteEng] <- "Engineering"; rm(dipreteEng)
dimeo <- sort(grep("dimeo co", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% dimeo] <- "dimeo construction"; dfx$Industry[dfx$EmployerName %in% dimeo] <- "Construction"; rm(dimeo)
disantoPriest <- sort(grep("disanto", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% disantoPriest] <- "disanto priest and company"; dfx$Industry[dfx$EmployerName %in% disantoPriest] <- "Accounting"; rm(disantoPriest)
dominion <- sort(grep("dominion", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% dominion] <- "dominion energy"; dfx$Industry[dfx$EmployerName %in% dominion] <- "Fossil Fuel"; rm(dominion)
donoghue <- sort(grep("donoghue", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% donoghue] <- "donoghue barrett and singal"; dfx$Industry[dfx$EmployerName %in% donoghue] <- "Attorneys & Lawyers"; rm(donoghue)
dorrance <- sort(grep("dorrance", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% dorrance] <- "dorrance street financial"; dfx$Industry[dfx$EmployerName %in% dorrance] <- "Finance"; rm(dorrance)
dougLumb <- sort(grep("douglas lum", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% dougLumb] <- "douglas lumber"; dfx$Industry[dfx$EmployerName %in% dougLumb] <- "Construction"; rm(dougLumb)
drDay <- sort(grep("dr day", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% drDay] <- "dr daycare"; dfx$Industry[dfx$EmployerName %in% drDay] <- "Early Education"; rm(drDay)
drb <- sort(grep("drb", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% drb] <- "drb investments"; dfx$Industry[dfx$EmployerName %in% drb] <- "Finance"; rm(drb)
drk <- sort(grep("drk", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% drk] <- "drk foundation"; dfx$Industry[dfx$EmployerName %in% drk] <- "Philanthropy"; rm(drk)
dufSween <- sort(grep("duffy and sween|duffy sween", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% dufSween] <- "duffy sweeney and scott"; dfx$Industry[dfx$EmployerName %in% dufSween] <- "Attorneys & Lawyers"; rm(dufSween)
dulgarian <- sort(grep("dulgarian", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% dulgarian] <- "dulgarian properties"; dfx$Industry[dfx$EmployerName %in% dulgarian] <- "Real Estate"; rm(dulgarian)
ebOral <- sort(grep("bay oral", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% ebOral] <- "east bay oral surgery"; dfx$Industry[dfx$EmployerName %in% ebOral] <- "Oral Surgery"; rm(ebOral)
eSide <- sort(grep("east side ent", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% eSide] <- "east side enterprises"; dfx$Industry[dfx$EmployerName %in% eSide] <- "Retail"; rm(eSide)
ebProp <- sort(grep("eb properties|^eb$", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% ebProp] <- "eb properties"; dfx$Industry[dfx$EmployerName %in% ebProp] <- "Real Estate"; rm(ebProp)
edWild <- sort(grep("wildman|edwards angell", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% edWild] <- "edwards wildman palmer"; dfx$Industry[dfx$EmployerName %in% edWild] <- "Attorneys & Lawyers"; rm(edWild)
efBishop <- sort(grep("ef bishop", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% efBishop] <- "ef bishop insurance agency"; dfx$Industry[dfx$EmployerName %in%efBishop] <- "Insurance"; rm(efBishop)
eBoat <- sort(grep("electdric boat|electric board|electric boat|general dynamics", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% eBoat] <- "general dynamics electric boat"; dfx$Industry[dfx$EmployerName %in% eBoat] <- "Military Industrial"; rm(eBoat)
elmHealth <- sort(grep("elmwood nurs|elmwood health", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% elmHealth] <- "elmwood nursing rehab center"; dfx$Industry[dfx$EmployerName %in% elmHealth] <- "Nursing"; rm(elmHealth)
employ2k <- sort(grep("^employment", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% employ2k] <- "employment 2000"; dfx$Industry[dfx$EmployerName %in% employ2k] <- "Employment Agency"; rm(employ2k)
espey <- sort(grep("espey", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% espey] <- "espey electronics and mfg"; dfx$Industry[dfx$EmployerName %in% espey] <- "Manufacturing"; rm(espey)
essexNew <- sort(grep("essex new|essex north", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% essexNew] <- "essex newbury north"; dfx$Industry[dfx$EmployerName %in% essexNew] <- "Construction"; rm(essexNew)
fsCap <- sort(grep("^f s", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% fsCap] <- "f s capitol consulting"; dfx$Industry[dfx$EmployerName %in% fsCap] <- "Consulting"; rm(fsCap)
fabiani <- sort(grep("fabiani", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% fabiani] <- "fabiani and co"; dfx$Industry[dfx$EmployerName %in% fabiani] <- "Lobby"; rm(fabiani)
fcg <- sort(grep("fcg", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% fcg] <- "fcg associates - triggs golf course"; dfx$Industry[dfx$EmployerName %in% fcg] <- "Recreation"; rm(fcg)
ferland <- sort(grep("ferland", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% ferland] <- "ferland corporation"; dfx$Industry[dfx$EmployerName %in% ferland] <- "Property Management"; rm(ferland)
ferrucci <- sort(grep("ferrucci", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in%ferrucci] <- "ferrucci russo law"; dfx$Industry[dfx$EmployerName %in% ferrucci] <- "Attorneys & Lawyers"; rm(ferrucci)
fidelity <- sort(grep("fidelity inv|fidelity$|fidelty", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% fidelity] <- "fidelity investments"; dfx$Industry[dfx$EmployerName %in% fidelity] <- "Finance"; rm(fidelity)
fmGlobal <- sort(grep("fm global", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% fmGlobal] <- "fm global foundation"; dfx$Industry[dfx$EmployerName %in% fmGlobal] <- "Insurance"; rm(fmGlobal)
foLard <- sort(grep("lardne", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% foLard] <- "foley and lardner"; dfx$Industry[dfx$EmployerName %in% foLard] <- "Attorneys & Lawyers"; rm(foLard)
folCer <- sort(grep("foley cer", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% folCer] <- "foley cerilli"; dfx$Industry[dfx$EmployerName %in% folCer] <- "Attorneys & Lawyers"; rm(folCer)
foleyHoag <- sort(grep("hoag", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% foleyHoag] <- "foley hoag"; dfx$Industry[dfx$EmployerName %in% foleyHoag] <- "Attorneys & Lawyers"; rm(foleyHoag)
fonBell <- sort(grep("fontaine bel", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% fonBell] <- "fontaine bell trial attorneys"; dfx$Industry[dfx$EmployerName %in% fonBell] <- "Attorneys & Lawyers"; rm(fonBell)
fracassaLaw <- sort(grep("fracassa la", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% fracassaLaw] <- "fracassa law and consulting"; dfx$Industry[dfx$EmployerName %in% fracassaLaw] <- "Attorneys & Lawyers"; rm(fracassaLaw)
fracassaT <- sort(grep("terence frac|terence m", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% fracassaT] <- "terence fracassa esq"; dfx$Industry[dfx$EmployerName %in% fracassaT] <- "Attorneys & Lawyers"; rm(fracassaT)
friendly <- sort(grep("friendly", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% friendly] <- "the friendly nursing home"; dfx$Industry[dfx$EmployerName %in%friendly] <- "Nursing Home"; rm(friendly)
fureyRoof <- sort(grep("furey roof", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% fureyRoof] <- "furey roofing and construction"; dfx$Industry[dfx$EmployerName %in% fureyRoof] <- "Construction"; rm(fureyRoof)
galvinAssoc <- sort(grep("^galvin", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in%galvinAssoc] <- "galvin and associates"; dfx$Industry[dfx$EmployerName %in% galvinAssoc] <- "Accounting"; rm(galvinAssoc)
garofalo <- sort(grep("garofalo|garofolo", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% garofalo] <- "garofalo and associates"; dfx$Industry[dfx$EmployerName %in% garofalo] <- "Engineering"; rm(garofalo)
garrity <- sort(grep("garrity", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% garrity] <- "garrity asphalt"; dfx$Industry[dfx$EmployerName %in% garrity] <- "Construction"; rm(garrity)
gemPlumb <- sort(grep("gem plumb", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% gemPlumb] <- "gem plumbing and heating"; dfx$Industry[dfx$EmployerName %in% gemPlumb] <- "HVAC"; rm(gemPlumb)
gemma <- sort(grep("gemma", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% gemma] <- "gemma law offices"; dfx$Industry[dfx$EmployerName %in% gemma] <- "Attorneys & Lawyers"; rm(gemma)
gencorp <- sort(grep("gencorp|gen corp|hilb", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Gencorp Insurance Group - ins broker
dfx$Employer[dfx$EmployerName %in% gencorp] <- "gencorp insurance group"; dfx$Industry[dfx$EmployerName %in% gencorp] <- "Insurance"; rm(gencorp)
genesis <- sort(grep("^genesis$|^genesis hc|genesis health care|genesis healthcare|genesis physician", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Genesis healthcare
dfx$Employer[dfx$EmployerName %in% genesis] <- "genesis healthcare"; dfx$Industry[dfx$EmployerName %in% genesis] <- "Nursing Home"; rm(genesis)
grandIsle <- sort(grep("grand isl", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Grand Islander - Genesis 
dfx$Employer[dfx$EmployerName %in% grandIsle] <- "grand islander nursing home genesis"; dfx$Industry[dfx$EmployerName %in% grandIsle] <- "Nursing Home"; rm(grandIsle)
kentReg <- sort(grep("kent r", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Kent Regency - Genesis
dfx$Employer[dfx$EmployerName %in% kentReg] <- "kent regency center genesis"; dfx$Industry[dfx$EmployerName %in% kentReg] <- "Nursing Home"; rm(kentReg)
scNurse <- sort(grep("south county nursing", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% scNurse] <- "south county nursing home"; dfx$Industry[dfx$EmployerName %in% scNurse] <- "Nursing Home"; rm(scNurse)
georgeCar <- sort(grep("caruolo", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% georgeCar] <- "law offices of george caruolo"; dfx$Industry[dfx$EmployerName %in% georgeCar] <- "Attorneys & Lawyers"; rm(georgeCar)
giarrusso <- sort(grep("giarrusso", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% giarrusso] <- "giarrusso norton gooley and mcglone"; dfx$Industry[dfx$EmployerName %in% giarrusso] <- "Attorneys & Lawyers"; rm(giarrusso)
gibsonDunn <- sort(grep("gibson dunn", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% gibsonDunn] <- "gibson dunn and crutcher"; dfx$Industry[dfx$EmployerName %in% gibsonDunn] <- "Attorneys & Lawyers"; rm(gibsonDunn)
gilbane <- sort(grep("gilbane", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in%gilbane] <- "gilbane bulding company"; dfx$Industry[dfx$EmployerName %in% gilbane] <- "Construction"; rm(gilbane)
glennAnd <- sort(grep("glenn andreoni|glenn j and", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% glennAnd] <- "glenn andreoni law offices"; dfx$Industry[dfx$EmployerName %in% glennAnd] <- "Attorneys & Lawyers"; rm(glennAnd)
orLand <- sort(grep("orlando a", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% orLand] <- "law offices of orlando andreoni"; dfx$Industry[dfx$EmployerName %in% orLand] <- "Attorneys & Lawyers"; rm(orLand)
goldberg <- sort(grep("^goldberg|goldberg$|goldberg esq|goldberg law", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Goldberg Law offices
dfx$Employer[dfx$EmployerName %in% goldberg] <- "goldberg law offices"; dfx$Industry[dfx$EmployerName %in% goldberg] <- "Attorneys & Lawyers"; rm(goldberg)
goldstein <- sort(grep("goldstein", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% goldstein] <- "goldstein and associates"; dfx$Industry[dfx$EmployerName %in% goldstein] <- "Real Estate"; rm(goldstein)
gonzalez <- sort(grep("gonzalez", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Gonzalez law
dfx$Employer[dfx$EmployerName %in% gonzalez] <- "gonzalez law offices"; dfx$Industry[dfx$EmployerName %in% gonzalez] <- "Attorneys & Lawyers"; rm(gonzalez)
govStrat <- sort(grep("govt strat|government strat", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% govStrat] <- "government strateries"; dfx$Industry[dfx$EmployerName %in% govStrat] <- "Lobby"; rm(govStrat)
grace <- sort(grep("^grace|grace bark|grace barb", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% grace] <- "grace barker nursing home"; dfx$Industry[dfx$EmployerName %in% grace] <- "Nursing Home"; rm(grace)
provChamber <- sort(grep("dence chamber", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% provChamber] <- "greater providence chamber of commerce"; dfx$Industry[dfx$EmployerName %in% provChamber] <- "Chamber of Commerce"; rm(provChamber)
greenberg <- sort(grep("^greenberg tr", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% greenberg] <- "greenberg traurig"; dfx$Industry[dfx$EmployerName %in% greenberg] <- "Attorneys & Lawyers"; rm(greenberg)
gregDias <- sort(grep("greg dias|gregory dias|gregory s dias", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% gregDias] <- "gregory dias esq"; dfx$Industry[dfx$EmployerName %in% gregDias] <- "Attorneys & Lawyers"; rm(gregDias)
groovPin <- sort(grep("groov pin", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% groovPin] <- "groov pin corporation"; dfx$Industry[dfx$EmployerName %in% groovPin] <- "Manufacturing"; rm(groovPin)
gunning <- sort(grep("gunning", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% gunning] <- "gunning and lafazia"; dfx$Industry[dfx$EmployerName %in% gunning] <- "Attorneys & Lawyers"; rm(gunning)
gursky <- sort(grep("gursky", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% gursky] <- "gursky wiens attorneys at law"; dfx$Industry[dfx$EmployerName %in% gursky] <- "Attorneys & Lawyers"; rm(gursky)
gusWhite <- sort(grep("gustave", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Realty
dfx$Employer[dfx$EmployerName %in% gusWhite] <- "gustave white sothebys realty"; dfx$Industry[dfx$EmployerName %in% gusWhite] <- "Realty"; rm(gusWhite)
gza <- sort(grep("gza", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% gza] <- "gza geoenvironmental"; dfx$Industry[dfx$EmployerName %in% gza] <- "Consulting"; rm(gza)
hCarr <- sort(grep("h carr|hcarr", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% hCarr] <- "h carr and sons"; dfx$Industry[dfx$EmployerName %in% hCarr] <- "Construction"; rm(hCarr)
hamelWax <- sort(grep("waxl", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% hamelWax] <- "hammel waxler allen and collins"; dfx$Industry[dfx$EmployerName %in% hamelWax] <- "Attorneys & Lawyers"; rm(hamelWax)
hartEngine <- sort(grep("hart engine", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% hartEngine] <- "hart engineering corporation"; dfx$Industry[dfx$EmployerName %in% hartEngine] <- "Engineering"; rm(hartEngine)
harvard <- sort(grep("harvard", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% harvard] <- "harvard university"; dfx$Industry[dfx$EmployerName %in% harvard] <- "Higher Education"; rm(harvard)
idaChaf <- sort(grep("chaffe", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% idaChaf] <- "hattie ide chaffee home"; dfx$Industry[dfx$EmployerName %in% idaChaf] <- "Nursing Home"; rm(idaChaf)
henCrown <- sort(grep("henry crown", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% henCrown] <- "henry crown and company"; dfx$Industry[dfx$EmployerName %in% henCrown] <- "Finance"; rm(henCrown)
heroica <- sort(grep("heroica", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% heroica] <- "heroica construction"; dfx$Industry[dfx$EmployerName %in% heroica] <- "Construction"; rm(heroica)
highRock <- sort(grep("high rock", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in%highRock] <- "high rock development"; dfx$Industry[dfx$EmployerName %in% highRock] <- "Real Estate"; rm(highRock)
higherEd <- sort(grep("higher ed", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% higherEd] <- "higher education partners"; dfx$Industry[dfx$EmployerName %in% higherEd] <- "Consulting"; rm(higherEd)
hinkly <- sort(grep("^hinc|^hink|^hnick|kinckley", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% hinkly] <- "hinkley allen and snyder"; dfx$Industry[dfx$EmployerName %in% hinkly] <- "Attorneys & Lawyers"; rm(hinkly)
holKnight <- sort(grep("^holland and", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% holKnight] <- "holland and knight"; dfx$Industry[dfx$EmployerName %in% holKnight] <- "Attorneys & Lawyers"; rm(holKnight)
homeLoan <- sort(grep("^home loan", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% homeLoan] <- "home loan investment bank"; dfx$Industry[dfx$EmployerName %in% homeLoan] <- "Finance"; rm(homeLoan)
hopeNurse <- sort(grep("hope nursing", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% hopeNurse] <- "hope nursing home care"; dfx$Industry[dfx$EmployerName %in% hopeNurse] <- "Healthcare"; rm(hopeNurse)
hopeMain <- sort(grep("hope and main|hope on main", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% hopeMain] <- "hope on main"; dfx$Industry[dfx$EmployerName %in% hopeMain] <- "Nonprofit"; rm(hopeMain)
horanLaw <- sort(grep("horan law", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% horanLaw] <- "horan law offices"; dfx$Industry[dfx$EmployerName %in% horanLaw] <- "Attorneys & Lawyers"; rm(horanLaw)
horizon <- sort(grep("^horizon bev", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% horizon] <- "horizon beverage company"; dfx$Industry[dfx$EmployerName %in% horizon] <- "Food & Beverage"; rm(horizon)
hari <- sort(grep("^hari$|hospital assoc", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% hari] <- "hospital association of ri"; dfx$Industry[dfx$EmployerName %in% hari] <- "Healthcare"; rm(hari)
hoJo <- sort(grep("howard john", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% hoJo] <- "howard johnson inn"; dfx$Industry[dfx$EmployerName %in% hoJo] <- "Hotel"; rm(hoJo)
howland <- sort(grep("howland", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% howland] <- "howland evangelista kohlenberg burnett"; dfx$Industry[dfx$EmployerName %in% howland] <- "Attorneys & Lawyers"; rm(howland)
huntonWill <- sort(grep("hunton and will", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% huntonWill] <- "hunton and williams"; dfx$Industry[dfx$EmployerName %in% huntonWill] <- "Attorneys & Lawyers"; rm(huntonWill)
hvCollins <- sort(grep("hv collins|h v collins", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% hvCollins] <- "hv collins company"; dfx$Industry[dfx$EmployerName %in% hvCollins] <- "Construction"; rm(hvCollins)
ibew <- sort(grep("^ibew|99", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% ibew] <- "ibew local 99"; dfx$Industry[dfx$EmployerName %in% ibew] <- "Labor Union"; rm(ibew)
seiu <- sort(grep("1199", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% seiu] <- "seiu local 1199"; dfx$Industry[dfx$EmployerName %in% seiu] <- "Labor Union"; rm(seiu)
jacksonO <- sort(grep("jackson oneil|jackson and oneil", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% jacksonO] <- "jackson and oneill law associates"; dfx$Industry[dfx$EmployerName %in% jacksonO] <- "Attorneys & Lawyers"; rm(jacksonO)
jjElectric <- sort(grep("jandj elec|j and j", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% jjElectric] <- "j and j electric"; dfx$Industry[dfx$EmployerName %in% jjElectric] <- "Construction"; rm(jjElectric)
jhLynch <- sort(grep("j h |jh lynch|lynch and sons", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% jhLynch] <- "j h lynch and sons"; dfx$Industry[dfx$EmployerName %in% jhLynch] <- "Construction"; rm(jhLynch)
jmPaint <- sort(grep("jm paint", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% jmPaint] <- "jm painting and plastering"; dfx$Industry[dfx$EmployerName %in% jmPaint] <- "Construction"; rm(jmPaint)
lombardiLaw <- sort(grep("john j lombardi|john lombardi|john jlombardi|frank s lombardi|frank lombardi|lombardi and ferrari|lombardi law", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% lombardiLaw] <- "lombardi law offices"; dfx$Industry[dfx$EmployerName %in% lombardiLaw] <- "Attorneys & Lawyers"; rm(lombardiLaw)
joeShekarchi <- sort(grep("^shekarchi|shekarchi$|shekarchi law|mary b shekarchi|k joseph shekarchi|joseph shekarchi", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% joeShekarchi] <- "shekarchi law offices"; dfx$Industry[dfx$EmployerName %in% joeShekarchi] <- "Attorneys & Lawyers"; rm(joeShekarchi)
jwu <- sort(grep("jwu|johnson and wales", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% jwu] <- "johnson and wales university"; dfx$Industry[dfx$EmployerName %in% jwu] <- "Higher Education"; rm(jwu)
joeWalsh <- sort(grep("joseph walsh|joe walsh", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% joeWalsh] <- "joseph walsh esq"; dfx$Industry[dfx$EmployerName %in% joeWalsh] <- "Attorneys & Lawyers"; rm(joeWalsh)
jpMorgan <- sort(grep("jp morgan", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% jpMorgan] <- "jp morgan"; dfx$Industry[dfx$EmployerName %in% jpMorgan] <- "Finance"; rm(jpMorgan)
jrVinagro <- sort(grep("jr vinagro", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% jrVinagro] <- "jr vinagro corporation"; dfx$Industry[dfx$EmployerName %in% jrVinagro] <- "Waste Management"; rm(jrVinagro)
kahnLitwin <- sort(grep("kahn litwin", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% kahnLitwin] <- "kahn litwin renza"; dfx$Industry[dfx$EmployerName %in% kahnLitwin] <- "Accounting"; rm(kahnLitwin)
kapFox <- sort(grep("kaplan f", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% kapFox] <- "kaplan fox and kilsheimer"; dfx$Industry[dfx$EmployerName %in% kapFox] <- "Attorneys & Lawyers"; rm(kapFox)
kartabar <- sort(grep("kartabar", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% kartabar] <- "kartabar restaurant"; dfx$Industry[dfx$EmployerName %in% kartabar] <- "Food & Beverage"; rm(kartabar)
kays <- sort(grep("^kays", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% kays] <- "kays restaurant"; dfx$Industry[dfx$EmployerName %in% kays] <- "Food & Beverage"; rm(kays)
keegan <- sort(grep("keegan", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% keegan] <- "keegan werlin"; dfx$Industry[dfx$EmployerName %in% keegan] <- "Attorneys & Lawyers"; rm(keegan)
kenyon <- sort(grep("kenyon law", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% kenyon] <- "kenyon law offices"; dfx$Industry[dfx$EmployerName %in% kenyon] <- "Attorneys & Lawyers"; rm(kenyon)
kessler <- sort(grep("kessler", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% kessler] <- "kessler topaz meltzer and check"; dfx$Industry[dfx$EmployerName %in% kessler] <- "Attorneys & Lawyers"; rm(kessler)
kiernan <- sort(grep("^kiernan", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% kiernan] <- "kiernan plunkett and redihan"; dfx$Industry[dfx$EmployerName %in% kiernan] <- "Attorneys & Lawyers"; rm(kiernan)
kirkland <- sort(grep("kirkland", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% kirkland] <- "kirkland and ellis"; dfx$Industry[dfx$EmployerName %in% kirkland] <- "Attorneys & Lawyers"; rm(kirkland)
kirshenbaum <- sort(grep("kirshenbaum", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% kirshenbaum] <- "kirshenbaum law office"; dfx$Industry[dfx$EmployerName %in% kirshenbaum] <- "Attorneys & Lawyers"; rm(kirshenbaum)
lakeview <- sort(grep("lakeview|lake view", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% lakeview] <- "lake view development"; dfx$Industry[dfx$EmployerName %in% lakeview] <- "Construction"; rm(lakeview)
lamar <- sort(grep("lamar", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% lamar] <- "lamar advertising co"; dfx$Industry[dfx$EmployerName %in% lamar] <- "Advertising"; rm(lamar)
georgeBauerle <- sort(grep("bauerle", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% georgeBauerle] <- "george bauerle esq"; dfx$Industry[dfx$EmployerName %in% georgeBauerle] <- "Attorneys & Lawyers"; rm(georgeBauerle)
johnMac <- sort(grep("john macdonald|john e macdonald", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% johnMac] <- "law office of john macdonald"; dfx$Industry[dfx$EmployerName %in% johnMac] <- "Attorneys & Lawyers"; rm(johnMac)
johnLanni <- sort(grep(" lanni", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% johnLanni] <- "john j lanni esq"; dfx$Industry[dfx$EmployerName %in%johnLanni] <- "Attorneys & Lawyers"; rm(johnLanni)
johnVerd <- sort(grep("verdecchia", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% johnVerd] <- "john m verdecchia esq"; dfx$Industry[dfx$EmployerName %in% johnVerd] <- "Attorneys & Lawyers"; rm(johnVerd)
markWelch <- sort(grep("mark welch|mark p welch|welch law", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% markWelch] <- "mark welch esq"; dfx$Industry[dfx$EmployerName %in% markWelch] <- "Attorneys & Lawyers"; rm(markWelch)
horanLaw <- sort(grep("horan", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% horanLaw] <- "horan law offices"; dfx$Industry[dfx$EmployerName %in% horanLaw] <- "Attorneys & Lawyers"; rm(horanLaw)
mikeDichiro <- sort(grep("dichiro", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mikeDichiro] <- "michael dichiro jr esq"; dfx$Industry[dfx$EmployerName %in% mikeDichiro] <- "Attorneys & Lawyers"; rm(mikeDichiro)
mikeNap <- sort(grep("michael t napolitano|michael napolitano|^napolitano", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mikeNap] <- "michael napolitano esq"; dfx$Industry[dfx$EmployerName %in% mikeNap] <- "Attorneys & Lawyers"; rm(mikeNap)
steveNap <- sort(grep("stephen napolitano", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% steveNap] <- "stephen napolitano esq"; dfx$Industry[dfx$EmployerName %in% steveNap] <- "Attorneys & Lawyers"; rm(steveNap)
patCon <- sort(grep("conley law office|patrick conley|patrick t conley", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% patCon] <- "patrick conley esq"; dfx$Industry[dfx$EmployerName %in% patCon] <- "Attorneys & Lawyers"; rm(patCon)
willCon <- sort(grep("^conley law$|william conley|william j conley", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% willCon] <- "law office of william conley"; dfx$Industry[dfx$EmployerName %in% willCon] <- "Attorneys & Lawyers"; rm(willCon)
patQuinlan <- sort(grep("patrick j quinlan|patrick quinlan|quinlan and durant|quinlan law", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% patQuinlan] <- "patrick quinlan law offices"; dfx$Industry[dfx$EmployerName %in% patQuinlan] <- "Attorneys & Lawyers"; rm(patQuinlan)
paulRag <- sort(grep("paul d ragosta|paul ragosta", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% paulRag] <- "paul d ragosta esq"; dfx$Industry[dfx$EmployerName %in% paulRag] <- "Attorneys & Lawyers"; rm(paulRag)
vinRag <- sort(grep("vincent ragosta|vincent f ragosta", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% vinRag] <- "vincent ragosta jr esq"; dfx$Industry[dfx$EmployerName %in% vinRag] <- "Attorneys & Lawyers"; rm(vinRag)
dimaio <- sort(grep("dimaio", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% dimaio] <- "grilli and dimaio"; dfx$Industry[dfx$EmployerName %in% dimaio] <- "Attorneys & Lawyers"; rm(dimaio)
richGal <- sort(grep("richard gallone", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% richGal] <- "law office of richard gallone"; dfx$Industry[dfx$EmployerName %in% richGal] <- "Attorneys & Lawyers"; rm(richGal)
robFlaherty <- sort(grep("robert e flaherty|robert flaherty", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% robFlaherty] <- "robert flaherty esq"; dfx$Industry[dfx$EmployerName %in% robFlaherty] <- "Attorneys & Lawyers"; rm(robFlaherty)
sueChi <- sort(grep("susan chiariello", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% sueChi] <- "susan chiariello esq"; dfx$Industry[dfx$EmployerName %in% sueChi] <- "Attorneys & Lawyers"; rm(sueChi)
suePerk <- sort(grep("susan perkins", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% suePerk] <- "susan perkins law"; dfx$Industry[dfx$EmployerName %in% suePerk] <- "Attorneys & Lawyers"; rm(suePerk)
terenceLiv <- sort(grep("terence livingston", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% terenceLiv] <- "terence livingston law"; dfx$Industry[dfx$EmployerName %in% terenceLiv] <- "Attorneys & Lawyers"; rm(terenceLiv)
tomBadway <- sort(grep("badway", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% tomBadway] <- "thomas badway and assoc"; dfx$Industry[dfx$EmployerName %in% tomBadway] <- "Attorneys & Lawyers"; rm(tomBadway)
timDodd <- sort(grep("timothy j dodd|timothy dodd|tim dodd|tim j dodd", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% timDodd] <- "timothy j dodd esq"; dfx$Industry[dfx$EmployerName %in% timDodd] <- "Attorneys & Lawyers"; rm(timDodd)
vitoSciolto <- sort(grep("vito sciolto|vito l sciolto", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% vitoSciolto] <- "vito sciolto attorney"; dfx$Industry[dfx$EmployerName %in% vitoSciolto] <- "Attorneys & Lawyers"; rm(vitoSciolto)
willGas <- sort(grep("william gasbarro|william p gasbarro", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% willGas] <- "william p gasbarro esq"; dfx$Industry[dfx$EmployerName %in% willGas] <- "Attorneys & Lawyers"; rm(willGas)
chrisMul <- sort(grep("christopher mulhearn", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% chrisMul] <- "law offices of christopher mulhearn"; dfx$Industry[dfx$EmployerName %in% chrisMul] <- "Attorneys & Lawyers"; rm(chrisMul)
dennishRob <- sort(grep("dennis j roberts", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% dennishRob] <- "law offices of dennis j roberts"; dfx$Industry[dfx$EmployerName %in% dennishRob] <- "Attorneys & Lawyers"; rm(dennishRob)
schadone <- sort(grep("schadone", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% schadone] <- "law office gregory schadone"; dfx$Industry[dfx$EmployerName %in% schadone] <- "Attorneys & Lawyers"; rm(schadone)
malSal <- sort(grep("salvadore", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% malSal] <- "law office of mal salvadore"; dfx$Industry[dfx$EmployerName %in% malSal] <- "Attorneys & Lawyers"; rm(malSal)
mikeKelly <- sort(grep("michael a kelly|michael kelly", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mikeKelly] <- "law offices of michael a kelly"; dfx$Industry[dfx$EmployerName %in% mikeKelly] <- "Attorneys & Lawyers"; rm(mikeKelly)
mccaffreyLaw <- sort(grep("mccaffrey", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mccaffreyLaw] <- "mccaffrey and mccaffrey law offices"; dfx$Industry[dfx$EmployerName %in% mccaffreyLaw] <- "Attorneys & Lawyers"; rm(mccaffreyLaw)
mikeRiley <- sort(grep("michael riley|michael j riley", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mikeRiley] <- "michael j riley esq"; dfx$Industry[dfx$EmployerName %in% mikeRiley] <- "Attorneys & Lawyers"; rm(mikeRiley)
steveBreggia <- sort(grep("breggia", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% steveBreggia] <- "law offices of stephen e breggia"; dfx$Industry[dfx$EmployerName %in% steveBreggia] <- "Attorneys & Lawyers"; rm(steveBreggia)
marsellaDev <- sort(grep("marsella dev", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% marsellaDev] <- "marsella development corp"; dfx$Industry[dfx$EmployerName %in% marsellaDev] <- "Real Estate"; rm(marsellaDev)
paulJab <- sort(grep("paul jabour", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% paulJab] <- "law offices paul jabour"; dfx$Industry[dfx$EmployerName %in% paulJab] <- "Attorneys & Lawyers"; rm(paulJab)
leadingage <- sort(grep("leadingage|leading age", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% leadingage] <- "leading age ri"; dfx$Industry[dfx$EmployerName %in% leadingage] <- "Nonprofit"; rm(leadingage)
lenette <- sort(grep("lenette", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% lenette] <- "lenette boisselle esq"; dfx$Industry[dfx$EmployerName %in% lenette] <- "Attorneys & Lawyers"; rm(lenette)
lepizzera <- sort(grep("lepizzera|lepizzwerra|laprocina", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% lepizzera] <- "lepizzera and laprocina esq"; dfx$Industry[dfx$EmployerName %in% lepizzera] <- "Attorneys & Lawyers"; rm(lepizzera)
lgcandd <- sort(grep("lgcandd|lgc", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% lgcandd] <- "lgc and d"; dfx$Industry[dfx$EmployerName %in% lgcandd] <- "Accounting"; rm(lgcandd)
libertyMut <- sort(grep("liberty mutual", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% libertyMut] <- "liberty mutual insurance group"; dfx$Industry[dfx$EmployerName %in% libertyMut] <- "Insurance"; rm(libertyMut)
lilaDelman <- sort(grep("delman", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% lilaDelman] <- "lila delman real estate"; dfx$Industry[dfx$EmployerName %in% lilaDelman] <- "Real Estate"; rm(lilaDelman)
liscoDev <- sort(grep("lisco", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% liscoDev] <- "lisco developments"; dfx$Industry[dfx$EmployerName %in% liscoDev] <- "Real Estate"; rm(liscoDev)
lockLord <- sort(grep("locke|lock lorde", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% lockLord] <- "locke lord edwards"; dfx$Industry[dfx$EmployerName %in% lockLord] <- "Attorneys & Lawyers"; rm(lockLord)
lockheed <- sort(grep("lockheed martin|lockhead martin", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% lockheed] <- "lockheed martin"; dfx$Industry[dfx$EmployerName %in% lockheed] <- "Military Industrial"; rm(lockheed)
loews <- sort(grep("loews", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Real estate
dfx$Employer[dfx$EmployerName %in% loews] <- "loews corporation"; dfx$Industry[dfx$EmployerName %in% loews] <- "Real Estate"; rm(loews)
louDesimone <- sort(grep("louis a desimone", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% louDesimone] <- "louis a desimone esq"; dfx$Industry[dfx$EmployerName %in% louDesimone] <- "Attorneys & Lawyers"; rm(louDesimone)
lubMeyer <- sort(grep("^lubin|^lublin", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% lubMeyer] <- "lublin and meyer"; dfx$Industry[dfx$EmployerName %in% lubMeyer] <- "Attorneys & Lawyers"; rm(lubMeyer)
daveLucier <- sort(grep("lucier", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% daveLucier] <- "david lucier cpa"; dfx$Industry[dfx$EmployerName %in% daveLucier] <- "Accounting"; rm(daveLucier)
lynchGreen <- sort(grep("lynch and green", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% lynchGreen] <- "lynch and greenfield"; dfx$Industry[dfx$EmployerName %in%lynchGreen] <- "Attorneys & Lawyers"; rm(lynchGreen)
lynchPine <- sort(grep("lynch and pine", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% lynchPine] <- "lynch and pine attorneys at law"; dfx$Industry[dfx$EmployerName %in% lynchPine] <- "Attorneys & Lawyers"; rm(lynchPine)
paivaWeed <- sort(grep("paiva weed", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% paivaWeed] <- "theresa paiva weed"; dfx$Industry[dfx$EmployerName %in% paivaWeed] <- "Attorneys & Lawyers"; rm(paivaWeed)
madDear <- sort(grep("madison dear", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% madDear] <- "madison dearborn capital partners"; dfx$Industry[dfx$EmployerName %in% madDear] <- "Finance"; rm(madDear)
magellan <- sort(grep("magellan", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% magellan] <- "cdmi magellan health"; dfx$Industry[dfx$EmployerName %in% magellan] <- "Healthcare"; rm(magellan)
mainSt <- sort(grep("main street", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mainSt] <- "main street resources advisors"; dfx$Industry[dfx$EmployerName %in% mainSt] <- "Finance"; rm(mainSt)
manafortBro <- sort(grep("manafort", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% manafortBro] <- "manafort brothers"; dfx$Industry[dfx$EmployerName %in% manafortBro] <- "Construction"; rm(manafortBro)
manatt <- sort(grep("manatt", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% manatt] <- "manatt phelps and phillips"; dfx$Industry[dfx$EmployerName %in% manatt] <- "Attorneys & Lawyers"; rm(manatt)
mandell <- sort(grep("mandell", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mandell] <- "mandell schwartz and boisclair"; dfx$Industry[dfx$EmployerName %in% mandell] <- "Attorneys & Lawyers"; rm(mandell)
manion <- sort(grep("gaynor|manion", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% manion] <- "manion gaynor and manning"; dfx$Industry[dfx$EmployerName %in% manion] <- "Attorneys & Lawyers"; rm(manion)
manMgmt <- sort(grep("mansfield man", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% manMgmt] <- "mansfield management co"; dfx$Industry[dfx$EmployerName %in% manMgmt] <- "Property Management"; rm(manMgmt)
mansion <- sort(grep("mansion", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mansion] <- "the mansion nursing home"; dfx$Industry[dfx$EmployerName %in% mansion] <- "Nursing Home"; rm(mansion)
marchettis <- sort(grep("marchettis", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% marchettis] <- "marchettis restaurant"; dfx$Industry[dfx$EmployerName %in% marchettis] <- "Food & Beverage"; rm(marchettis)
marcum <- sort(grep("marcum", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% marcum] <- "marcum"; dfx$Industry[dfx$EmployerName %in% marcum] <- "Accounting"; rm(marcum)
martiesian <- sort(grep("martiesian", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% martiesian] <- "martiesian law offices"; dfx$Industry[dfx$EmployerName %in% martiesian] <- "Attorneys & Lawyers"; rm(martiesian)
fredMarz <- sort(grep("marzilli", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% fredMarz] <- "frederic marzilli esq"; dfx$Industry[dfx$EmployerName %in% fredMarz] <- "Attorneys & Lawyers"; rm(fredMarz)
maselloBro <- sort(grep("masello|masiello", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% maselloBro] <- "masello brothers"; dfx$Industry[dfx$EmployerName %in% maselloBro] <- "Salon"; rm(maselloBro)
matouk <- sort(grep("matouk", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% matouk] <- "matouk textiles"; dfx$Industry[dfx$EmployerName %in% matouk] <- "Manufacturing"; rm(matouk)
matunuck <- sort(grep("matunuck|matunick", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% matunuck] <- "matunuck oyster bar"; dfx$Industry[dfx$EmployerName %in% matunuck] <- "Food & Beverage"; rm(matunuck)
mayforth <- sort(grep("mayforth", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mayforth] <- "the mayforth group"; dfx$Industry[dfx$EmployerName %in% mayforth] <- "Lobby"; rm(mayforth)
mcRen <- sort(grep("^mc ren", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mcRen] <- "mc renovations"; dfx$Industry[dfx$EmployerName %in% mcRen] <- "Construction"; rm(mcRen)
mcadams <- sort(grep("mcadams", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mcadams] <- "mcadams charitable foundation"; dfx$Industry[dfx$EmployerName %in% mcadams] <- "Philanthropy"; rm(mcadams)
mcentee <- sort(grep("mcentee", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mcentee] <- "mcentee and mcentee law offices"; dfx$Industry[dfx$EmployerName %in% mcentee] <- "Attorneys & Lawyers"; rm(mcentee)
mcintyre <- sort(grep("mcintyre", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mcintyre] <- "mcintyre tate and lynch"; dfx$Industry[dfx$EmployerName %in% mcintyre] <- "Attorneys & Lawyers"; rm(mcintyre)
mckennaRoof <- sort(grep("mckenna", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mckennaRoof] <- "mckenna roofing and construction"; dfx$Industry[dfx$EmployerName %in% mckennaRoof] <- "Construction"; rm(mckennaRoof)
mckenney <- sort(grep("mckenney", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mckenney] <- "mckenney quigley izzo and clarkin"; dfx$Industry[dfx$EmployerName %in% mckenney] <- "Attorneys & Lawyers"; rm(mckenney)
mckinsey <- sort(grep("mckinsey", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mckinsey] <- "mckinsey and company"; dfx$Industry[dfx$EmployerName %in% mckinsey] <- "Consulting"; rm(mckinsey)
mckool <- sort(grep("^mckool", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mckool] <- "mckool smith"; dfx$Industry[dfx$EmployerName %in% mckool] <- "Attorneys & Lawyers"; rm(mckool)
viacom <- sort(grep("viacom", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% viacom] <- "viacom"; dfx$Industry[dfx$EmployerName %in% viacom] <- "Media"; rm(viacom)
mclarty <- sort(grep("mclarty", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mclarty] <- "the mclarty companies"; dfx$Industry[dfx$EmployerName %in% mclarty] <- "Finance"; rm(mclarty)
mclaughlin <- sort(grep("mclaughlin and mor", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mclaughlin] <- "mclaughlin and moran distributors"; dfx$Industry[dfx$EmployerName %in% mclaughlin] <- "Food & Beverage"; rm(mclaughlin)
mcmahon <- sort(grep("mcmahon", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mcmahon] <- "mcmahon and associates"; dfx$Industry[dfx$EmployerName %in% mcmahon] <- "Engineering"; rm(mcmahon)
mdfx <- sort(grep("mdf|mdx", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mdfx] <- "mdf capital"; dfx$Industry[dfx$EmployerName %in% mdfx] <- "Finance"; rm(mdfx)
medici <- sort(grep("^medici|al medici ", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% medici] <- "medici and sciacca law"; dfx$Industry[dfx$EmployerName %in% medici] <- "Attorneys & Lawyers"; rm(medici)
chrisBoyle <- sort(grep("boyle law|chris boyle|christopher boyle", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% chrisBoyle] <- "law office of christopher boyle"; dfx$Industry[dfx$EmployerName %in% chrisBoyle] <- "Attorneys & Lawyers"; rm(chrisBoyle)
meehanBoyle <- sort(grep("^meehan", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% meehanBoyle] <- "meehan boyle black and bogdanow"; dfx$Industry[dfx$EmployerName %in% meehanBoyle] <- "Attorneys & Lawyers"; rm(meehanBoyle)
megaTrans <- sort(grep("mega trans", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% megaTrans] <- "mega transportation group"; dfx$Industry[dfx$EmployerName %in% megaTrans] <- "Transportation"; rm(megaTrans)
meredith <- sort(grep("meredith man", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% meredith] <- "meredith management corporation"; dfx$Industry[dfx$EmployerName %in% meredith] <- "Real Estate"; rm(meredith)
meridianReal <- sort(grep("meridian real|meridian custom", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% meridianReal] <- "meridian real estate"; dfx$Industry[dfx$EmployerName %in% meridianReal] <- "Real Estate"; rm(meridianReal)
merLynch <- sort(grep("merrill l|merill l", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% merLynch] <- "merrill lynch"; dfx$Industry[dfx$EmployerName %in% merLynch] <- "Finance"; rm(merLynch)
metLife <- sort(grep("metlife|met life", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% metLife] <- "metlife auto and home"; dfx$Industry[dfx$EmployerName %in% metLife] <- "Insurance"; rm(metLife)
mikeInteglia <- sort(grep("integlia", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mikeInteglia] <- "michael integlia and company"; dfx$Industry[dfx$EmployerName %in% mikeInteglia] <- "Real Estate"; rm(mikeInteglia)
mikeMul <- sort(grep("mulcahy", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mikeMul] <- "michael mulcahy law"; dfx$Industry[dfx$EmployerName %in% mikeMul] <- "Attorneys & Lawyers"; rm(mikeMul)
mikeEgan <- sort(grep(" egan", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mikeEgan] <- "michael r egan esq"; dfx$Industry[dfx$EmployerName %in% mikeEgan] <- "Attorneys & Lawyers"; rm(mikeEgan)
mikeSepe <- sort(grep(" sepe|^sepe", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mikeSepe] <- "michael sepe and company"; dfx$Industry[dfx$EmployerName %in% mikeSepe] <- "Accounting"; rm(mikeSepe)
dunkin <- sort(grep("dunkin|mineral enterprise", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% dunkin] <- "dunkin donuts"; dfx$Industry[dfx$EmployerName %in% dunkin] <- "Food & Beverage"; rm(dunkin)
drBanki <- sort(grep("banki |^moham", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% drBanki] <- "mohammad banki dmd"; dfx$Industry[dfx$EmployerName %in% drBanki] <- "Oral Surgery"; rm(drBanki)
mongo <- sort(grep("mongo", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mongo] <- "mongo db"; dfx$Industry[dfx$EmployerName %in% mongo] <- "Technology"; rm(mongo)
moranMgmt <- sort(grep("moran man|moran prop", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% moranMgmt] <- "moran properties"; dfx$Industry[dfx$EmployerName %in% moranMgmt] <- "Real Estate"; rm(moranMgmt)
motleyRice <- sort(grep("motely|motley", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% motleyRice] <- "motley rice law firm"; dfx$Industry[dfx$EmployerName %in% motleyRice] <- "Attorneys & Lawyers"; rm(motleyRice)
faceSurg <- sort(grep("msf facial|msl facial", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% faceSurg] <- "msl facial and oral surgery"; dfx$Industry[dfx$EmployerName %in% faceSurg] <- "Oral Surgery"; rm(faceSurg)
murphFay <- sort(grep("murphy and fay", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% murphFay] <- "murphy and fay"; dfx$Industry[dfx$EmployerName %in% murphFay] <- "Attorneys & Lawyers"; rm(murphFay)
murrayFam <- sort(grep("murray family", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% murrayFam] <- "murray family charitable foundation"; dfx$Industry[dfx$EmployerName %in% murrayFam] <- "Philanthropy"; rm(murrayFam)
mutualProp <- sort(grep("mutual prop", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% mutualProp] <- "mutual property associates"; dfx$Industry[dfx$EmployerName %in% mutualProp] <- "Real Estate"; rm(mutualProp)
nandd <- sort(grep("^n and d|nandd", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% nandd] <- "n and d transportation"; dfx$Industry[dfx$EmployerName %in% nandd] <- "Transportation"; rm(nandd)
nabsys <- sort(grep("nabsys", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% nabsys] <- "nabsys"; dfx$Industry[dfx$EmployerName %in% nabsys] <- "Biotech"; rm(nabsys)
nagelMach <- sort(grep("nagel machine", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in%nagelMach] <- "nagel machine company"; dfx$Industry[dfx$EmployerName %in% nagelMach] <- "Manufacturing"; rm(nagelMach)
gansettImp <- sort(grep("narragansett imp", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% gansettImp] <- "narragansett improvement co"; dfx$Industry[dfx$EmployerName %in% gansettImp] <- "Manufacturing"; rm(gansettImp)
neari <- sort(grep("neari|nea ri|^national education", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% neari] <- "national education association ri"; dfx$Industry[dfx$EmployerName %in% neari] <- "Education"; rm(neari)
natGrid <- sort(grep("national grid|nationl grid", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% natGrid] <- "national grid"; dfx$Industry[dfx$EmployerName %in% natGrid] <- "Energy"; rm(natGrid)
nuwc <- sort(grep("nuwc|naval under|navel under", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% nuwc] <- "naval undersea warfare center"; dfx$Industry[dfx$EmployerName %in% nuwc] <- "Military Industrial"; rm(nuwc)
neTech <- sort(grep("^ne inst|^ne tech|england tech|neit|england institute", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% neTech] <- "new england tech"; dfx$Industry[dfx$EmployerName %in% neTech] <- "Higher Education"; rm(neTech)
nellmct <- sort(grep("nellmct", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% nellmct] <- "new england laborers (nellmct) media"; dfx$Industry[dfx$EmployerName %in% nellmct] <- "Media"; rm(nellmct) # What is this?  What do they do?
nephAssoc <- sort(grep("nephrology", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% nephAssoc] <- "nephrology associates"; dfx$Industry[dfx$EmployerName %in% nephAssoc] <- "Nephrology"; rm(nephAssoc)
neLabor <- sort(grep("england labor|ne labor", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% neLabor] <- "new england laborers"; dfx$Industry[dfx$EmployerName %in% neLabor] <- "Labor Union"; rm(neLabor)
nptGrand <- sort(grep("newport grand", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% nptGrand] <- "newport grand casino"; dfx$Industry[dfx$EmployerName %in% nptGrand] <- "Gambling"; rm(nptGrand)
newtonOrth <- sort(grep("newton wellesley", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% newtonOrth] <- "newton wellesley orthopedics"; dfx$Industry[dfx$EmployerName %in% newtonOrth] <- "Orthopedics"; rm(newtonOrth)
nextHome <- sort(grep("next home", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% nextHome] <- "next home real estate"; dfx$Industry[dfx$EmployerName %in% nextHome] <- "Real Estate"; rm(nextHome)
nixon <- sort(grep("nixon pea|nison pea", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% nixon] <- "nixon peabody"; dfx$Industry[dfx$EmployerName %in% nixon] <- "Attorneys & Lawyers"; rm(nixon)
neTree <- sort(grep("eastern tree|n e tree|east tree", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% neTree] <- "northeast tree service"; dfx$Industry[dfx$EmployerName %in% neTree] <- "Landscaping"; rm(neTree)
nriChamber <- sort(grep("northern ri chamber|nri chamber", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% nriChamber] <- "northern ri chamber of commerce"; dfx$Industry[dfx$EmployerName %in% nriChamber] <- "Chamber of Commerce"; rm(nriChamber)
nuLux <- sort(grep("nu lux|nulux", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% nuLux] <- "nu lux cleaners"; dfx$Industry[dfx$EmployerName %in% nuLux] <- "Cleaning"; rm(nuLux)
nutter <- sort(grep("nutter", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% nutter] <- "nutter mcclennen and fish"; dfx$Industry[dfx$EmployerName %in% nutter] <- "Attorneys & Lawyers"; rm(nutter)
osCap <- sort(grep("ocean state cap|^os cap|oceanstate cap", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% osCap] <- "ocean state capital consulting"; dfx$Industry[dfx$EmployerName %in% osCap] <- "Lobby"; rm(osCap)
oceanPt <- sort(grep("oceanpoint|ocean point", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% oceanPt] <- "oceanpoint insurance"; dfx$Industry[dfx$EmployerName %in% oceanPt] <- "Insurance"; rm(oceanPt)
osFin <- sort(grep("ocean state fin|^os cap|oceanstate fin", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% osFin] <- "ocean state financial services"; dfx$Industry[dfx$EmployerName %in% osFin] <- "Finance"; rm(osFin)
olennPenza <- sort(grep("olenn|penza", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% olennPenza] <- "olenn and penza"; dfx$Industry[dfx$EmployerName %in% olennPenza] <- "Attorneys & Lawyers"; rm(olennPenza)
oralSurg <- sort(grep("^oral surgery and|^oral surgery dental", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% oralSurg] <- "oral surgery and dental implant ctr"; dfx$Industry[dfx$EmployerName %in% oralSurg] <- "Oral Surgery"; rm(oralSurg)
orPorc <- sort(grep("porcaro", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% orPorc] <- "orlando porcaro and associates"; dfx$Industry[dfx$EmployerName %in% orPorc] <- "Attorneys & Lawyers"; rm(orPorc)
orrick <- sort(grep("orrick", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% orrick] <- "orrick herrington and sutcliffe"; dfx$Industry[dfx$EmployerName %in% orrick] <- "Attorneys & Lawyers"; rm(orrick)
optum <- sort(grep("optum|united health|^uhc|unitedhealth", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% optum] <- "united health care"; dfx$Industry[dfx$EmployerName %in% optum] <- "Health Insurance"; rm(optum)
fatima <- sort(grep("fatima", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% fatima] <- "fatima hospital"; dfx$Industry[dfx$EmployerName %in% fatima] <- "Hospital"; rm(fatima)
overhead <- sort(grep("overhead|over head", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% overhead] <- "overhead door company"; dfx$Industry[dfx$EmployerName %in% overhead] <- "Construction"; rm(overhead)
overlook <- sort(grep("overlook", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% overlook] <- "overlook nursing home"; dfx$Industry[dfx$EmployerName %in% overlook] <- "Nursing Home"; rm(overlook)
paolinoProp <- sort(grep("paolino prop|paolino part", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% paolinoProp] <- "paolino properties"; dfx$Industry[dfx$EmployerName %in% paolinoProp] <- "Real Estate"; rm(paolinoProp)
patLynch <- sort(grep("patrick lynch", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% patLynch] <- "the patrick lynch group"; dfx$Industry[dfx$EmployerName %in% patLynch] <- "Lobby"; rm(patLynch)
pawtHot <- sort(grep("pawtucket hot mix", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% pawtHot] <- "pawtucket hot mix asphalt"; dfx$Industry[dfx$EmployerName %in% pawtHot] <- "Construction"; rm(pawtHot)
phoenixHouse <- sort(grep("phoenix houses", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% phoenixHouse] <- "phoenix houses of new england"; dfx$Industry[dfx$EmployerName %in% phoenixHouse] <- "Rehab"; rm(phoenixHouse)
piccerelli <- sort(grep("piccerelli", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% piccerelli] <- "piccerelli and gilstein"; dfx$Industry[dfx$EmployerName %in% piccerelli] <- "Accounting"; rm(piccerelli)
picerne <- sort(grep("^picerne", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% picerne] <- "picerne properties"; dfx$Industry[dfx$EmployerName %in% picerne] <- "Real Estate"; rm(picerne)
pierceAt <- sort(grep("pierce atwood", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% pierceAt] <- "pierce atwood"; dfx$Industry[dfx$EmployerName %in% pierceAt] <- "Attorneys & Lawyers"; rm(pierceAt)
pilTit <- sort(grep("pilgrim title", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% pilTit] <- "pilgrim title insurance"; dfx$Industry[dfx$EmployerName %in% pilTit] <- "Insurance"; rm(pilTit)
plannedP <- sort(grep("^planned|parenthood", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% plannedP] <- "planned parenthood"; dfx$Industry[dfx$EmployerName %in% plannedP] <- "Healthcare"; rm(plannedP)
polsinelli <- sort(grep("polsinelli", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% polsinelli] <- "polsinelli"; dfx$Industry[dfx$EmployerName %in% polsinelli] <- "Attorneys & Lawyers"; rm(polsinelli)
pontefract <- sort(grep("pontefract", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% pontefract] <- "pontefract global strategies"; dfx$Industry[dfx$EmployerName %in% pontefract] <- "Consulting"; rm(pontefract)
postAcute <- sort(grep("post acute", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% postAcute] <- "post acute care partners"; dfx$Industry[dfx$EmployerName %in% postAcute] <- "Healthcare"; rm(postAcute)
ppac <- sort(grep("dence performing arts|ppac", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% ppac] <- "providence performing arts center"; dfx$Industry[dfx$EmployerName %in% ppac] <- "Arts"; rm(ppac)
premierLand <- sort(grep("premier land|premier dev", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% premierLand] <- "premier land development"; dfx$Industry[dfx$EmployerName %in% premierLand] <- "Real Estate"; rm(premierLand)
primacare <- sort(grep("prima care|primacare", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% primacare] <- "primacare pc"; dfx$Industry[dfx$EmployerName %in% primacare] <- "Healthcare"; rm(primacare)
procaccianti <- sort(grep("procaccianti", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Real estate
dfx$Employer[dfx$EmployerName %in% procaccianti] <- "the procaccianti group"; dfx$Industry[dfx$EmployerName %in% procaccianti] <- "Real Estate"; rm(procaccianti)
propAdvise <- sort(grep("property advisory", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% propAdvise] <- "property advisory group"; dfx$Industry[dfx$EmployerName %in% propAdvise] <- "Real Estate"; rm(propAdvise)
propCasualty <- sort(grep("property cas", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% propCasualty] <- "property casualty insurers"; dfx$Industry[dfx$EmployerName %in% propCasualty] <- "Insurance"; rm(propCasualty)
provAnesth <- sort(grep("providence anesth", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% provAnesth] <- "providence anesthesiologists"; dfx$Industry[dfx$EmployerName %in% provAnesth] <- "Anesthesiology"; rm(provAnesth)
provAuto <- sort(grep("providence auto|prov auto", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% provAuto] <- "providence auto body"; dfx$Industry[dfx$EmployerName %in% provAuto] <- "Autobody"; rm(provAuto)
provCol <- sort(grep("providence college|^pc$", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% provCol] <- "providence college"; dfx$Industry[dfx$EmployerName %in% provCol] <- "Higher Education"; rm(provCol)
pchc <- sort(grep("providence community health|pchc", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% pchc] <- "providence community health centers"; dfx$Industry[dfx$EmployerName %in% pchc] <- "Healthcare"; rm(pchc)
provVA <- sort(grep("providence va", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% provVA] <- "providence va medical center"; dfx$Industry[dfx$EmployerName %in% provVA] <- "Healthcare"; rm(provVA)
rpIannuccillo <- sort(grep("iannuccillo", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% rpIannuccillo] <- "r p iannuccillo and sons construction"; dfx$Industry[dfx$EmployerName %in% rpIannuccillo] <- "Construction"; rm(rpIannuccillo)
reMax <- sort(grep("remax|re max", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% reMax] <- "re max professionals"; dfx$Industry[dfx$EmployerName %in% reMax] <- "Real Estate"; rm(reMax)
reedSmith <- sort(grep("reed smith", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% reedSmith] <- "reed smith"; dfx$Industry[dfx$EmployerName %in% reedSmith] <- "Attorneys & Lawyers"; rm(reedSmith)
reliable <- sort(grep("reliable coll|reliage coll", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% reliable] <- "reliable collision repair"; dfx$Industry[dfx$EmployerName %in% reliable] <- "Autobody"; rm(reliable)
realAssoc <- sort(grep("assoc of realtors|ri association of realtors", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% realAssoc] <- "ri association of realtors"; dfx$Industry[dfx$EmployerName %in% realAssoc] <- "Realty"; rm(realAssoc)
riFinAssoc <- sort(grep("association of financ", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% riFinAssoc] <- "ri association of financial services"; dfx$Industry[dfx$EmployerName %in% riFinAssoc] <- "Finance"; rm(riFinAssoc)
riDistrib <- sort(grep("ri distrib", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% riDistrib] <- "ri distributing co"; dfx$Industry[dfx$EmployerName %in% riDistrib] <- "Food & Beverage"; rm(riDistrib)
riFinance <- sort(grep("ri finan", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% riFinance] <- "ri financial services"; dfx$Industry[dfx$EmployerName %in% riFinance] <- "Finance"; rm(riFinance)
rihc <- sort(grep("ri health care", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% rihc] <- "ri health care association"; dfx$Industry[dfx$EmployerName %in% rihc] <- "Healthcare"; rm(rihc)
riHealthCtr <- sort(grep("ri health cen|ri health ctr|rihc", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% riHealthCtr] <- "ri health center association"; dfx$Industry[dfx$EmployerName %in% riHealthCtr] <- "Healthcare"; rm(riHealthCtr)
rimi <- sort(grep("^rimi|ri medical imag|island medical imag", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% rimi] <- "ri medical imaging"; dfx$Industry[dfx$EmployerName %in% rimi] <- "Medical Imaging"; rm(rimi)
robAudet <- sort(grep("robert f audet|robert audet", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% robAudet] <- "robert f audet"; dfx$Industry[dfx$EmployerName %in% robAudet] <- "Construction"; rm(robAudet)
robPeretti <- sort(grep("peretti", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% robPeretti] <- "robert peretti esq"; dfx$Industry[dfx$EmployerName %in% robPeretti] <- "Attorneys & Lawyers"; rm(robPeretti)
robKap <- sort(grep("robins kaplan", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% robKap] <- "robins kaplan"; dfx$Industry[dfx$EmployerName %in% robKap] <- "Attorneys & Lawyers"; rm(robKap)
robinCole <- sort(grep("robinson and cole|robinson cole", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% robinCole] <- "robinson and cole"; dfx$Industry[dfx$EmployerName %in% robinCole] <- "Attorneys & Lawyers"; rm(robinCole)
rodio <- sort(grep("rodio", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% rodio] <- "rodio and ursillo"; dfx$Industry[dfx$EmployerName %in% rodio] <- "Attorneys & Lawyers"; rm(rodio)
ronLab <- sort(grep("labinger", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% ronLab] <- "rooney and labinger"; dfx$Industry[dfx$EmployerName %in% ronLab] <- "Attorneys & Lawyers"; rm(ronLab)
rosewood <- sort(grep("rosewood cons", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% rosewood] <- "rosewood consulting firm"; dfx$Industry[dfx$EmployerName %in% rosewood] <- "Lobby"; rm(rosewood)
rueDel <- sort(grep("^rue de", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% rueDel] <- "rue de l'espoir"; dfx$Industry[dfx$EmployerName %in% rueDel] <- "Restaurant"; rm(rueDel)
richSah <- sort(grep("sahagian", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% richSah] <- "sahagian law office"; dfx$Industry[dfx$EmployerName %in% richSah] <- "Attorneys & Lawyers"; rm(richSah)
saks <- sort(grep("saks", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% saks] <- "saks centredale liquors"; dfx$Industry[dfx$EmployerName %in% saks] <- "Food & Beverage"; rm(saks)
samBerg <- sort(grep("sammartino", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% samBerg] <- "sammartino and berg law office"; dfx$Industry[dfx$EmployerName %in% samBerg] <- "Attorneys & Lawyers"; rm(samBerg)
santander <- sort(grep("santander", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% santander] <- "santander bank"; dfx$Industry[dfx$EmployerName %in% santander] <- "Finance"; rm(santander)
santoro <- sort(grep("santoro", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% santoro] <- "santoro oil company"; dfx$Industry[dfx$EmployerName %in% santoro] <- "Fossil Fuel"; rm(santoro)
sardellas <- sort(grep("sardellas", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% sardellas] <- "sardellas restaurant"; dfx$Industry[dfx$EmployerName %in% sardellas] <- "Restaurant"; rm(sardellas)
sayerReg <- sort(grep("sayer regan", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% sayerReg] <- "sayer regan thayer"; dfx$Industry[dfx$EmployerName %in% sayerReg] <- "Attorneys & Lawyers"; rm(sayerReg)
schect <- sort(grep("^schetman|^schectman|^schechtman|halperin|sheckmanhalpernsavage|happerin", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% schect] <- "shechtman halperin savage"; dfx$Industry[dfx$EmployerName %in% schect] <- "Attorneys & Lawyers"; rm(schect)
seaChurch <- sort(grep("^seamans|^seamens", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% seaChurch] <- "seamans church institute of newport"; dfx$Industry[dfx$EmployerName %in% seaChurch] <- "Nonprofit"; rm(seaChurch)
shamrock <- sort(grep("shamrock fin", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% shamrock] <- "shamrock financial corporation"; dfx$Industry[dfx$EmployerName %in% shamrock] <- "Finance"; rm(shamrock)
shawmut <- sort(grep("shawmut", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% shawmut] <- "shawmut design and construction"; dfx$Industry[dfx$EmployerName %in% shawmut] <- "Construction"; rm(shawmut)
sheLog <- sort(grep("sherin", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% sheLog] <- "sherin and lodgen"; dfx$Industry[dfx$EmployerName %in% sheLog] <- "Attorneys & Lawyers"; rm(sheLog)
sherleWag <- sort(grep("sherle", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% sherleWag] <- "sherle wagner international"; dfx$Industry[dfx$EmployerName %in% sherleWag] <- "Manufacturing"; rm(sherleWag)
sienna <- sort(grep("siena|sienna", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% sienna] <- "sienna restaurant"; dfx$Industry[dfx$EmployerName %in% sienna] <- "Restaurant"; rm(sienna)
silvaTom <- sort(grep("silva thomas", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% silvaTom] <- "silva thomas martland and offenberg"; dfx$Industry[dfx$EmployerName %in% silvaTom] <- "Attorneys & Lawyers"; rm(silvaTom)
simpsonThat  <- sort(grep("^simpson|^simson", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% simpsonThat] <- "simpson thacher and bartlett"; dfx$Industry[dfx$EmployerName %in% simpsonThat] <- "Attorneys & Lawyers"; rm(simpsonThat)
simsMetal <- sort(grep("^sims", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% simsMetal] <- "sims metal management"; dfx$Industry[dfx$EmployerName %in% simsMetal] <- "Waste Management"; rm(simsMetal)
sinapiLaw  <- sort(grep("sinapi law", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% sinapiLaw] <- "sinapi law associates"; dfx$Industry[dfx$EmployerName %in% sinapiLaw] <- "Attorneys & Lawyers"; rm(sinapiLaw)
singleSource  <- sort(grep("single source|single cource", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% singleSource] <- "single source disaster and recovery"; dfx$Industry[dfx$EmployerName %in% singleSource] <- "Construction"; rm(singleSource)
skadArp  <- sort(grep("skadden", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% skadArp] <- "skadden arps slate meagher and flom"; dfx$Industry[dfx$EmployerName %in% skadArp] <- "Attorneys & Lawyers"; rm(skadArp)
seGreen  <- sort(grep("se greenhouse|enterprise green", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% seGreen] <- "social enterprise greenhouse"; dfx$Industry[dfx$EmployerName %in% seGreen] <- "Nonprofit"; rm(seGreen)
soucy  <- sort(grep("soucy", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% soucy] <- "soucy insurance agency"; dfx$Industry[dfx$EmployerName %in% soucy] <- "Insurance"; rm(soucy)
southCoast  <- sort(grep("^southcoast", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% southCoast] <- "southcoast hospitals group"; dfx$Industry[dfx$EmployerName %in% southCoast] <- "Hospital"; rm(southCoast)
soSky  <- sort(grep("southern sky|south sky|sky renew", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% soSky] <- "southern sky renewable energy ri"; dfx$Industry[dfx$EmployerName %in% soSky] <- "Clean Energy"; rm(soSky)
sparrow  <- sort(grep("sparrow", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% sparrow] <- "sparrow johnson and ursillo"; dfx$Industry[dfx$EmployerName %in% sparrow] <- "Accounting"; rm(sparrow)
stLuke  <- sort(grep("st lukes$|st lukes hosp", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% stLuke] <- "st lukes hospital"; dfx$Industry[dfx$EmployerName %in% stLuke] <- "Hospital"; rm(stLuke)
stMartin  <- sort(grep("st martins", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% stMartin] <- "st martins church"; dfx$Industry[dfx$EmployerName %in% stMartin] <- "Religious"; rm(stMartin)
stanford  <- sort(grep("stanford", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% stanford] <- "stanford university"; dfx$Industry[dfx$EmployerName %in% stanford] <- "Higher Education"; rm(stanford)
starkweather  <- sort(grep("starkweather|stark weather", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% starkweather] <- "starkweather and shepley insurance"; dfx$Industry[dfx$EmployerName %in% starkweather] <- "Insurance"; rm(starkweather)
stasiunas  <- sort(grep("stasiunas", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% stasiunas] <- "stasiunas construction"; dfx$Industry[dfx$EmployerName %in% stasiunas] <- "Construction"; rm(stasiunas)
steward  <- sort(grep("steward", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% steward] <- "steward health system"; dfx$Industry[dfx$EmployerName %in% steward] <- "Healthcare"; rm(steward)
stormtite  <- sort(grep("stormtite", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% stormtite] <- "stormtite home improvement"; dfx$Industry[dfx$EmployerName %in% stormtite] <- "Construction"; rm(stormtite)
stratCon  <- sort(grep("^strategic con", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% stratCon] <- "strategic consulting service"; dfx$Industry[dfx$EmployerName %in% stratCon] <- "Consulting"; rm(stratCon)
suffolkCon  <- sort(grep("suffolk", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% suffolkCon] <- "suffolk construction company"; dfx$Industry[dfx$EmployerName %in% suffolkCon] <- "Construction"; rm(suffolkCon)
sulWhite  <- sort(grep("whitehead", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% sulWhite] <- "sullivan whitehead and deluca"; dfx$Industry[dfx$EmployerName %in% sulWhite] <- "Attorneys & Lawyers"; rm(sulWhite)
taco  <- sort(grep("^taco$|taco comfort|taco inc", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% taco] <- "taco incorporated"; dfx$Industry[dfx$EmployerName %in% taco] <- "Manufacturing"; rm(taco)
taftMc  <- sort(grep("^taft", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% taftMc] <- "taft and mcsally"; dfx$Industry[dfx$EmployerName %in% taftMc] <- "Attorneys & Lawyers"; rm(taftMc)
taio  <- sort(grep("^tai o", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% taio] <- "tai o associates"; dfx$Industry[dfx$EmployerName %in% taio] <- "Attorneys & Lawyers"; rm(taio)
tasca  <- sort(grep("tasca", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% tasca] <- "tasca automotive group"; dfx$Industry[dfx$EmployerName %in% tasca] <- "Auto Sales"; rm(tasca)
tarbox  <- sort(grep("tarbox", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% tarbox] <- "tarbox toyota"; dfx$Industry[dfx$EmployerName %in% tarbox] <- "Auto Sales"; rm(tarbox)
taxFin  <- sort(grep("^tax cred|^tax inc", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% taxFin] <- "tax incentive finance"; dfx$Industry[dfx$EmployerName %in% taxFin] <- "Finance"; rm(taxFin)
taylorBox  <- sort(grep("taylor box", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% taylorBox] <- "taylor box company"; dfx$Industry[dfx$EmployerName %in% taylorBox] <- "Manufacturing"; rm(taylorBox)
team251  <- sort(grep("251", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% team251] <- "teamsters local 251"; dfx$Industry[dfx$EmployerName %in% team251] <- "Labor Union"; rm(team251)
technic  <- sort(grep("technic$|^technic", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% technic] <- "technic corporation"; dfx$Industry[dfx$EmployerName %in% technic] <- "Technology"; rm(technic)
theArmory  <- sort(grep("armory", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% theArmory] <- "the armory revival co"; dfx$Industry[dfx$EmployerName %in% theArmory] <- "Real Estate"; rm(theArmory)
beaconMut  <- sort(grep("beacon mut", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% beaconMut] <- "beacon mutual insurance co"; dfx$Industry[dfx$EmployerName %in% beaconMut] <- "Insurance"; rm(beaconMut)
theFound  <- sort(grep("the foundry", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% theFound] <- "the foundry associates"; dfx$Industry[dfx$EmployerName %in% theFound] <- "Real Estate"; rm(theFound)
greenbrier  <- sort(grep("greenbrier|greenbier", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% greenbrier] <- "the greenbrier companies"; dfx$Industry[dfx$EmployerName %in% greenbrier] <- "Manufacturing"; rm(greenbrier)
tlc  <- sort(grep("learning comm", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% tlc] <- "the learning community charter school"; dfx$Industry[dfx$EmployerName %in% tlc] <- "Education"; rm(tlc)
tomSlate  <- sort(grep("slater compassion", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% tomSlate] <- "thomas c slater compassion center"; dfx$Industry[dfx$EmployerName %in% tomSlate] <- "Marijuana"; rm(tomSlate)
trigg  <- sort(grep("triggs", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% trigg] <- "triggs golf course"; dfx$Industry[dfx$EmployerName %in% trigg] <- "Recreation"; rm(trigg)
unap  <- sort(grep("united nurse|unap", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% unap] <- "united nurses and allied professionals"; dfx$Industry[dfx$EmployerName %in% unap] <- "Labor Union"; rm(unap)
univMed  <- sort(grep("^university med|^university emer", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% univMed] <- "university medicine"; dfx$Industry[dfx$EmployerName %in% univMed] <- "Healthcare"; rm(univMed)
upStream  <- sort(grep("upstream", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% upStream] <- "upstream policy"; dfx$Industry[dfx$EmployerName %in% upStream] <- "Lobby"; rm(upStream)
urbanCon  <- sort(grep("urbane|^urban con", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print # Which one is it?
dfx$Employer[dfx$EmployerName %in% urbanCon] <- "urbane construction"; dfx$Industry[dfx$EmployerName %in% urbanCon] <- "Construction"; rm(urbanCon)
usArmy  <- sort(grep("army", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% usArmy] <- "united states army"; dfx$Industry[dfx$EmployerName %in% usArmy] <- "Military Industrial"; rm(usArmy)
usNavy  <- sort(grep("navy$", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% usNavy] <- "united states navy"; dfx$Industry[dfx$EmployerName %in% usNavy] <- "Military Industrial"; rm(usNavy)
vanasse  <- sort(grep("vanasse", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% vanasse] <- "vanasse hangen brustlin"; dfx$Industry[dfx$EmployerName %in% vanasse] <- "Engineering"; rm(vanasse)
vareika  <- sort(grep("vareika|vereika", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% vareika] <- "william vereika fine arts"; dfx$Industry[dfx$EmployerName %in% vareika] <- "Arts"; rm(vareika)
venda  <- sort(grep("venda", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% venda] <- "venda ravioli"; dfx$Industry[dfx$EmployerName %in% venda] <- "Restaurant"; rm(venda)
vensys  <- sort(grep("vensys", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% vensys] <- "vensys energy"; dfx$Industry[dfx$EmployerName %in% vensys] <- "Clean Energy"; rm(vensys)
venus  <- sort(grep("venus de", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% venus] <- "venus de milo"; dfx$Industry[dfx$EmployerName %in% venus] <- "Restaurant"; rm(venus)
walco  <- sort(grep("walco", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% walco] <- "walco electric"; dfx$Industry[dfx$EmployerName %in% walco] <- "Manufacturing"; rm(walco)
wardFish  <- sort(grep("ward fish", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% wardFish] <- "ward fisher and company"; dfx$Industry[dfx$EmployerName %in% wardFish] <- "Accounting"; rm(wardFish)
wesRib  <- sort(grep("^wes rib|^wess rib", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% wesRib] <- "wess rib house"; dfx$Industry[dfx$EmployerName %in% wesRib] <- "Restaurant"; rm(wesRib)
wWarwickAuto  <- sort(grep("west warwick auto", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% wWarwickAuto] <- "west warwick autobody"; dfx$Industry[dfx$EmployerName %in% wWarwickAuto] <- "Autobody"; rm(wWarwickAuto)
whalerock  <- sort(grep("whalerock|whale rock", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% whalerock] <- "whalerock point partners"; dfx$Industry[dfx$EmployerName %in% whalerock] <- "Finance"; rm(whalerock)
wickfordApp  <- sort(grep("wickford app", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% wickfordApp] <- "wickford appliances"; dfx$Industry[dfx$EmployerName %in% wickfordApp] <- "Retail"; rm(wickfordApp)
wilfreds  <- sort(grep("wilfreds", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% wilfreds] <- "wilfreds seafood"; dfx$Industry[dfx$EmployerName %in% wilfreds] <- "Restaurant"; rm(wilfreds)
dimitri  <- sort(grep("dimitri", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% dimitri] <- "william dimitri law"; dfx$Industry[dfx$EmployerName %in% dimitri] <- "Attorneys & Lawyers"; rm(dimitri)
winChest  <- sort(grep("winchester", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% winChest] <- "winchester investments"; dfx$Industry[dfx$EmployerName %in% winChest] <- "Finance"; rm(winChest)
trill  <- sort(grep("trillium$|trillium asset|trillium asseet|trillium assset", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% trill] <- "trillium asset mgmt"; dfx$Industry[dfx$EmployerName %in% trill] <- "Finance"; rm(trill)
york  <- sort(grep("york resource", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% york] <- "york resources"; dfx$Industry[dfx$EmployerName %in% york] <- "Finance"; rm(york)
rdw  <- sort(grep("rdw ", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% rdw] <- "rdw group"; dfx$Industry[dfx$EmployerName %in% rdw] <- "Media / Advertising"; rm(rdw)
nortek  <- sort(grep("nortek", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% nortek] <- "nortek"; dfx$Industry[dfx$EmployerName %in% nortek] <- "Manufacturing"; rm(nortek)
cand  <- sort(grep("candidate$|candidate for|candidate united", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% cand] <- "Candidate for Office"; dfx$Industry[dfx$EmployerName %in% cand] <- "State Government"; rm(cand)
lilMed  <- sort(grep("little medeiros|medeiros kinder|kinder bulman|kinder whitney|bulman and whitney|bulman whitney", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% lilMed] <- "little mederious kinder bulman and whitney"; dfx$Industry[dfx$EmployerName %in% lilMed] <- "Attorneys & Lawyers"; rm(lilMed)
simpat  <- sort(grep("simpatico soft", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% simpat] <- "simpatico software"; dfx$Industry[dfx$EmployerName %in% simpat] <- "Technology"; rm(simpat)
tlps  <- sort(grep("tlps and c|tillinghast licht|tillinghast and licht|licht perkins|licht and perkins|perkins smith|perkins and smith|smith and cohen|smith cohen|smith collins", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% tlps] <- "tillinghast licht perkins smith and collins"; dfx$Industry[dfx$EmployerName %in% tlps] <- "Attorneys & Lawyers"; rm(tlps)
troyPire  <- sort(grep("troy pires|troy and pires|pires and allen|pires allen", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Employer[dfx$EmployerName %in% troyPire] <- "troy pires allen ins"; dfx$Industry[dfx$EmployerName %in% troyPire] <- "Insurance"; rm(troyPire)
# val  <- sort(grep("", dfx$EmployerName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
# dfx$Employer[dfx$EmployerName %in% val] <- ""; dfx$Industry[dfx$EmployerName %in% val] <- ""; rm()

# Format
dfx$Employer <- ifelse(dfx$Employer == "", dfx$EmployerName, dfx$Employer)



# Groups of Employers
constructionCo <- c("a z corporation","caldwell and johnson","jl marshall and sons","millwork one","clean care of new england",
                    "digregorio","john rocchio corporation","stand corporation","h v collins company")
mkting <- c("anne holland ventures","the bradford group","data mail","bradford group")
wasteCo <- c("hw management")
finco <- c("lazard","neuberger berman","azrack and company","jane street")
techCos <- c("utilidata","google","dell emc","facebook","twitter","alphabet")
manuFax <- c("hasbro","tpr2","doranco","the chemical company")
edCo <- c("year up","astor childrens services")
lawFirms <- c("coia and lepore","orson and brusini","moses afonso ryan","goulston and storrs","igliozzi and reis","edwards and angell",
              "revens revens and st pierre","smith costello and crawford","lasalle and kelleher pc","moses and afonso")
tobaccoReps <- c("berman devalerio") # Lawyers
military <- c("north atlantic distribution") # NORAD
consultCo <- c("the glover park group","prisere","szostak partners","samdec","blackstone river group","york resources")
selfEmp <- c("eugene lee")
miscCo <- c("","info requested","unknown","waiting on info","information requested")

# Group together similar industries
autoBody <- sort(grep("autobody|auto body|automotive|auto repair|collision", dfx$Employer, value = TRUE) %>% unique()) %>% print
#dfx$Industry[dfx$Employer %in% autoBody] <- "Auto Body"
dfx$Industry[dfx$Employer %in% autoBody & dfx$Industry == ""] <- "Auto Body"; rm(autoBody)
lawSchools <- sort(grep("law school|school of law", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% lawSchools & dfx$Industry == ""] <- "Law Schools"; rm(lawSchools)
teleComs <- sort(grep("tele |tele-|Verizon|^Cox |Comcast|COMMUNICATIONS|broadcast|sinclair|telecom|t mobile|time warner|wireless|time warner", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
dfx$Industry[dfx$Employer %in% teleComs & dfx$Industry == ""] <- "Communications"; rm(teleComs)
energy <- sort(grep("coal |oil |gas |petrol|fossil|fuel|crude|liquified|lng|natural gas|gas$| oil$", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
pizza <- sort(grep("pizza", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
dfx$Industry[dfx$Employer %in% energy & !dfx$Employer %in% pizza & dfx$Industry == ""] <- "Fossil Fuel"; rm(energy,pizza)
cityGovt <- sort(sort(grep("^city of|^town of|city of |town of |cityof|townof|the city |the town |mayor of", dfx$Employer, value = TRUE, ignore.case = TRUE)) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% cityGovt & dfx$Industry == ""] <- "Local Government"; rm(cityGovt)
natGovt <- sort(grep("^us house of|us department of|^department of|federal dept|federal bureau|federal depa|fbi|central intel|us rep|^cia$", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% natGovt & dfx$Industry == ""] <- "Federal Government"; rm(natGovt)
stateGovt <- sort(grep("^state of|state house|state representative|ri dept|ri department|ma dept|ma department|state of |state rep|state treasurer|lt governor", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% stateGovt & dfx$Industry == ""] <- "State Government"; rm(stateGovt)
uni <- sort(grep("ccri|university|^uri|college|graduate school|business school|school of", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% uni & dfx$Industry == ""] <- "Higher Education"; rm(uni)
lobby <- sort(grep("victor group|capitol strategies|lobbyist|lobbying|government strategies|advocacy solutions", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% lobby & dfx$Industry == ""] <- "Lobby"; rm(lobby)
bankers <- sort(grep("invest|BANKERS|banks | bank|^bank|credit u|bank |GOLDMAN SACHS|finance|financial|WASHINGTON MUTUAL|home loan| loan$|^loan | venture |ventures$", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
capital <- sort(sort(grep("capital| wealth |capitol$", dfx$Employer, value = TRUE, ignore.case = TRUE)) %>% unique()) %>% print
consult <- sort(grep("consult|public relation", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
finCon <- sort(grep("financial consultant|tax consult|ophthalmolog|dental consult", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% c(consult, consultCo) & !dfx$Employer %in% c(bankers,capital,finCon) & dfx$Industry == ""] <- "Consulting"; rm(capital,consult,finCon,consultCo)
defense <- sort(grep("textron|Raytheon|navy|nuwc|war college|warfare|naval academy|weapon|missile|army|armed force", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
dfx$Industry[dfx$Employer %in% defense | dfx$Employer %in% military & dfx$Industry == ""] <- "Military Industrial"; rm(defense,military)
unions <- sort(grep("usaw|union|brotherhood|AFL-CIO|DEMOCRAT, REPUBLICAN, INDEP|iupat|teacher|school department", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
bankers <- sort(grep("invest|BANKERS|banks | bank|^bank|credit u|bank |GOLDMAN SACHS|finance|financial|WASHINGTON MUTUAL|home loan| loan$|^loan | venture |ventures$", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
dfx$Industry[dfx$Employer %in% unions & !dfx$Employer %in% bankers & dfx$Industry == ""] <- "Labor Union"; rm(unions)
dems <- sort(grep("DEMOCRATS|democratic|democrat ", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
dfx$Industry[dfx$Employer %in% dems & dfx$Industry == ""] <- "Democrats"; rm(dems)
bankers <- sort(grep("invest|BANKERS|banks | bank|^bank|credit u|bank |GOLDMAN SACHS|finance|financial|WASHINGTON MUTUAL|home loan| loan$|^loan | venture |ventures$", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
capital <- sort(sort(grep("capital| wealth |capitol$", dfx$Employer, value = TRUE, ignore.case = TRUE)) %>% unique()) %>% print
notCapital <- sort(sort(grep("capital tv", dfx$Employer, value = TRUE, ignore.case = TRUE)) %>% unique()) %>% print
dfx$Industry[dfx$Employer %in% bankers | dfx$Employer %in% capital & !dfx$Employer %in% notCapital & dfx$Industry == ""] <- "Finance"; rm(bankers,capital,notCapital)
ins <- sort(grep("ins agency| ins co|delta dental|metlife|travelers|ALLSTATE INSURANCE|AMERICAN INSURANCE|MUTUAL INSURANCE|LIFE INSURANCE|INDEPENDENT INSURANCE|MORTGAGE INSURANCE|AMERICA INSURANCE|REINSURANCE|Insurance Agents|insurance|blue cross|united health$", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
dfx$Industry[dfx$Employer %in% ins & dfx$Industry == ""] <- "Insurance"; rm(ins)
lawyers <- sort(grep("law group|associates$| llp$|digennaro|and ass| law$|capitol city|matiello|esq|justice|attorney|law office| law ", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
dfx$Industry[dfx$Employer %in% c(lawyers,lawFirms,tobaccoReps) & dfx$Industry == ""] <- "Attorneys & Lawyers"; rm(lawyers,lawFirms,tobaccoReps)
police <- sort(grep("police| pd$", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% police & dfx$Industry == ""] <- "Police Dept"; rm(police)
fire <- sort(grep("fire dep|firef|fire dis|providence fire$|firem|fire fig|fire marsh|fire chief", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% fire & dfx$Industry == ""] <- "Fire Dept"; rm(fire)
casino <- sort(grep("newport g|twin riv|foxw|casino|slots|gambling|gambler", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% casino & dfx$Industry == ""] <- "Gambling"; rm(casino)
education <- sort(grep("education|educate", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% education | dfx$Employer %in% edCo & dfx$Industry == ""] <- "Education"; rm(education,edCo)
school <- sort(grep("school|academy$|st mary academy|academy charter school|stem | stem$", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% school & dfx$Industry == ""] <- "School"; rm(school)
hosp <- sort(grep("hospital", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% hosp & dfx$Industry == ""] <- "Hospital"; rm(hosp)
medicine <- sort(grep("medicine|medical|homeop", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% medicine & dfx$Industry == ""] <- "Medicine"; rm(medicine)
healthcare <- sort(grep("healthcare|health", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
ins <- sort(grep("ins agency| ins co|delta dental|metlife|travelers|ALLSTATE INSURANCE|AMERICAN INSURANCE|MUTUAL INSURANCE|LIFE INSURANCE|INDEPENDENT INSURANCE|MORTGAGE INSURANCE|AMERICA INSURANCE|REINSURANCE|Insurance Agents|insurance|blue cross|united health$", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
dfx$Industry[dfx$Employer %in% healthcare & !dfx$Employer %in% ins & dfx$Industry == ""] <- "Healthcare"; rm(healthcare,ins)
nursing <- sort(grep("nurse|nursing|assisted|grand islander|manor|genesis health|genesis hc|center genesis|care at home|convalescent", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% nursing & dfx$Industry == ""] <- "Nursing Home"; rm(nursing)
neurology <- sort(grep("neurology|neuro", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% neurology & dfx$Industry == ""] <- "Neurology"; rm(neurology)
derma <- sort(grep("derma", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% derma & dfx$Industry == ""] <- "Dermatology"; rm(derma)
gastro <- sort(grep("gastr", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% gastro & dfx$Industry == ""] <- "Gastroenterology"; rm(gastro)
ortho <- sort(grep("orthop", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% ortho & dfx$Industry == ""] <- "Orthopedics"; rm(ortho)
radiology <- sort(grep("radiol", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% radiology & dfx$Industry == ""] <- "Radiology"; rm(radiology)
ophth <- sort(grep("Opht| eye |^eye ", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% ophth & dfx$Industry == ""] <- "Ophthalmology"; rm(ophth)
pedi <- sort(grep("pediatric", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% pedi & dfx$Industry == ""] <- "Pediatrics"; rm(pedi)
internalMed <- sort(grep("internal medicine|primary care", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% internalMed & dfx$Industry == ""] <- "Internal Medicine"; rm(internalMed)
maxillo <- sort(grep("orthodon|dentist|maxillo|oral surg", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% maxillo & dfx$Industry == ""] <- "Maxillofacial"; rm(maxillo)
fakeDocs <- sort(grep("dr communications|dr daycare|dr day care", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
physicians <- sort(grep("physician| md| df$|^dr | dmd$| do$", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% physicians & !dfx$Employer %in% fakeDocs & dfx$Industry == ""] <- "Physician"; rm(physicians, fakeDocs)
obgyn <- sort(grep("gyn|obstetrics|gynecology", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% obgyn & dfx$Industry == ""] <- "OBGYN"; rm(obgyn)

# life <- sort(grep("ri hospital$|newport hospital$|newport hospital life|hasbro childrens hospital|miriam|bradley hosp|^gateway health|^gateway mental|gateway partner|lifespan corp|lifespan ri|lifespan practice|^lifespan$", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
# dfx$Industry[dfx$Employer %in% life & dfx$Industry == ""] <- "Hospital"; rm(life)
# cne <- sort(grep("care new e|butler h|kent h|^memorial h|providence center|infants hosp|wih", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
# dfx$Industry[dfx$Employer %in% cne & dfx$Industry == ""] <- "Hospital"; rm(cne)

ambulance <- sort(grep("Ambulance", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% ambulance & dfx$Industry == ""] <- "Ambulance"; rm(ambulance)
bioTech <- sort(grep(" bio$|biot|bios|biom|^bio", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% bioTech & dfx$Industry == ""] <- "Biotech"; rm(bioTech)
pharm <- sort(grep("epivax|PHARMACEUTICAL|PHARMATHENE|PHARMAVITE|PHARMERICA|PHARMA |pfizer|Abbvie|Depuy|Sanofi|Teva|Allergan|bristol meyers|meyers squibb|Merck|Medtronic|Amgen|eli lilly|genentech", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
dfx$Industry[dfx$Employer %in% pharm & dfx$Industry == ""] <- "Pharmaceutical"; rm(pharm)
student <- sort(grep("student", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% student & dfx$Industry == ""] <- "Student"; rm(student)
self <- sort(grep("self employ|self-employ", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
lawyers <- sort(grep("law group|associates$| llp$|digennaro|and ass| law$|capitol city|matiello|esq|justice|attorney|law office| law ", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
lawFirms <- c("coia and lepore","orson and brusini","moses afonso ryan","goulston and storrs",
              "revens revens and st pierre","smith costello and crawford","lasalle and kelleher pc","moses and afonso")
dfx$Industry[dfx$Employer %in% self & !dfx$Employer %in% c(lawyers,lawFirms) & dfx$Industry == ""] <- "Self Employed"; rm(self,lawyers,lawFirms)
home <- sort(grep("homemaker|home maker|stay at home|works from home|^at home$|housewife", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% home & dfx$Industry == ""] <- "Home Maker"; rm(home)
realEstate <- sort(grep("development|real estate|properties|property|estate|architect", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
realEstateAgencies <- c("caldwell and johnson","first bristol corporation","peterson management","cushman and wakefield","executive affiliates","omni new york","zds")
dfx$Industry[dfx$Employer %in% realEstate | dfx$Employer %in% realEstateAgencies & dfx$Industry == ""] <- "Real Estate"; rm(realEstate,realEstateAgencies)
rx <- sort(grep("cvs$|pharmacy|prescript|drug$|drug store", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% rx & dfx$Industry == ""] <- "Pharmacy"; rm(rx)
cleanEnergy <- sort(grep("renewable|deepwater wind|energy wind|wind power|wind energy|solar|green development llc| wind | wind$| solar|^solar |^wind |geotherm|hydro", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% cleanEnergy & dfx$Industry == ""] <- "Clean Energy"; rm(cleanEnergy)
realty <- sort(grep("realty|realtor|homes$", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
dfx$Industry[dfx$Employer %in% realty & dfx$Industry == ""] <- "Realty"; rm(realty)
construct <- sort(grep("concrete|construction|building|bldg|builder|drywall|ceiling|floor|window| roof|carpentry|carpenter|excavat", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
mktSales <- sort(grep("marketing|social media", dfx$Employer, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Industry[dfx$Employer %in% construct | dfx$Employer %in% constructionCo & dfx$Industry == ""] <- "Construction"; rm(construct)
chamber <- sort(grep("commerce ri|chamber of commerce|commerce corp", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% chamber & dfx$Industry == ""] <- "Chamber of Commerce"; rm(chamber)
newTech <- sort(grep("drupal|igt gtech|data", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% newTech & dfx$Industry == ""] <- "New Technology"; rm(newTech)
provPort <- sort(grep("prov port|providence port|waterson terminal", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% provPort & dfx$Industry == ""] <- "Port of Providence"; rm(provPort)
maryJane <- sort(grep("compassion|marij", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% maryJane & dfx$Industry == ""] <- "Marijuana"; rm(maryJane)
engine <- sort(grep("engineering|engineer", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
engineCo <- c("pare corporation","aecom","haks","cdm smith")
dfx$Industry[dfx$Employer %in% engine | dfx$Employer %in% engineCo & dfx$Industry == ""] <- "Engineering"; rm(engine,engineCo)
#tech <- sort(grep("", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
utils <- sort(grep("national grid|nationalgrid|natonal grid", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% utils & dfx$Industry == ""] <- "Utilities"; rm(utils)
natGovt <- sort(grep("^us |usps|usda", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% natGovt & dfx$Industry == ""] <- "Federal Government"; rm(natGovt)
transp <- sort(grep("trucking|shipping|transport|transit$|transit |railroad", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% transp & dfx$Industry == ""] <- "Trucking / Shipping"; rm(transp)
mediAd <- sort(grep("media$|media |advert", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% mediAd | dfx$Employer %in% mkting & dfx$Industry == ""] <- "Media / Advertising"; rm(mediAd,mkting)
payDay <- sort(grep("check cash| loan |gold loan|gold machine|gold$| advance$|cash |cash$|cashing$|pawn", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% payDay & dfx$Industry == ""] <- "Payday Loan"; rm(payDay)
retire <- sort(grep("retirement home|retirement$|retirement com|home care|senior", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% retire & dfx$Industry == ""] <- "Senior Living"; rm(retire)
funHome <- sort(grep("funeral home|burial|cremation|cemetary", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% funHome & dfx$Industry == ""] <- "Funeral Home"; rm(funHome)
waste <- sort(grep("disposal|recycling|trash|waste", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% waste | dfx$Employer %in% wasteCo & dfx$Industry == ""] <- "Waste Management"; rm(waste,wasteCo)
electric <- sort(grep("electric", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% electric & dfx$Industry == ""] <- "Utilities"; rm(electric)
energy <- sort(grep("energy", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% energy & dfx$Industry == ""] <- "Energy"; rm(energy)
eats <- sort(grep("grille|restaurant$|restaurant group| diner |diner$|pizza | pizza$|taco bell|burger|bakery", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% eats & dfx$Industry == ""] <- "Restaurant"; rm(eats)
lumber <- sort(grep("supply|lumber|sheetrock|plumber|plumbing|waterproof", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% lumber & dfx$Industry == ""] <- "Construction"; rm(lumber)
water <- sort(grep("water |water$", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% water & dfx$Industry == ""] <- "Water"; rm(water)
booze <- sort(grep("beer|wine | wine$|spirits|liquor", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% booze & dfx$Industry == ""] <- "Alcohol"; rm(booze)
capPart <- sort(grep("capitol partner|capital partner", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% capPart | dfx$Employer %in% finco & dfx$Industry == ""] <- "Finance"; rm(capPart,finco)
yard <- sort(grep("landscaping|^lawn | lawn |lawn$|garden design|garden shop| yard|tree service|sprinkler|pest", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% yard & dfx$Industry == ""] <- "Landscaping"; rm(yard)
foodBev <- sort(grep("coffee", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% foodBev & dfx$Industry == ""] <- "Food & Beverage"; rm(foodBev)
masonry <- sort(grep("gravel|asphalt|paving|concrete|masonry|wrecking", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% masonry & dfx$Industry == ""] <- "Masonry"; rm(masonry)
sci <- sort(grep("science", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% sci & dfx$Industry == ""] <- "Science"; rm(sci)
techno <- sort(grep("tech|software$", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% techno | dfx$Employer %in% techCos & dfx$Industry == ""] <- "Technology"; rm(techno,techCos)
vidz <- sort(grep("production|film|video|movie", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% vidz & dfx$Industry == ""] <- "Film"; rm(vidz)
accounts <- sort(grep("cpa|account", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% accounts & dfx$Industry == ""] <- "Accounting"; rm(accounts)
art <- sort(grep("^art | art |gallery", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% art & dfx$Industry == ""] <- "Art"; rm(art)
mortgage <- sort(grep("mortgage", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% mortgage & dfx$Industry == ""] <- "Mortgage"; rm(mortgage)
manufact <- sort(grep("manufact|chemical|plastic|packag|products", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
dfx$Industry[dfx$Employer %in% manufact | dfx$Employer %in% manuFax & !dfx$Employer %in% constructionCo & dfx$Industry == ""] <- "Manufacturing"; rm(manufact,manuFax)
farming <- sort(grep(" farm | farm$|nurser|agric|cattle|growers", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
dfx$Industry[dfx$Employer %in% farming & dfx$Industry == ""] <- "Farming"; rm(farming)
marina <- sort(grep("ocean link|marine|marina|boater|boating|sailing|yachting|maritime|boatwork", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% marina & dfx$Industry == ""] <- "Marine"; rm(marina)
hotels <- sort(grep("vacation|getaway|hotel|motel|marriot|hilton|biltmore|laundry|freeway enterprises", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
dfx$Industry[dfx$Employer %in% hotels & dfx$Industry == ""] <- "Hotel"; rm(hotels)
jewels <- sort(grep("jewel|ldc|agandg|ag + g", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
dfx$Industry[dfx$Employer %in% jewels & dfx$Industry == ""] <- "Arts & Entertainment"; rm(jewels)
hvac <- sort(grep("refridge|vacuum|heating|refrig", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
dfx$Industry[dfx$Employer %in% hvac & dfx$Industry == ""] <- "HVAC"; rm(hvac)
design <- sort(grep("^ash n|designs|interior design|design$| design ", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print; construct <- c("Construction","Masonry","Landscaping","Electric","HVAC")
construct <- sort(grep("concrete|construction|building|bldg|builder|drywall|ceiling|floor|window| roof|carpentry|carpenter|excavat", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>%  print
dfx$Industry[dfx$Employer %in% design & !dfx$Employer %in% c(construct,constructionCo) & dfx$Industry == ""] <- "Arts & Entertainment"; rm(design,construct,constructionCo)
enviro <- sort(grep("environment|save the bay", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
dfx$Industry[dfx$Employer %in% enviro & dfx$Industry == ""] <- "Environmental"; rm(enviro)
cleaning <- sort(grep("carpet|cleaning|cleaner|wash | wash$|sweeping", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
dfx$Industry[dfx$Employer %in% cleaning & dfx$Industry == ""] <- "Other Services"; rm(cleaning)
strategyAdvise <- sort(grep("advisor|strateg", dfx$Employer, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
bankers <- sort(grep("invest|BANKERS|banks | bank|^bank|credit u|bank |GOLDMAN SACHS|finance|financial|WASHINGTON MUTUAL|home loan| loan$|^loan | venture |ventures$", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
capital <- sort(sort(grep("capital| wealth |capitol$", dfx$Employer, value = TRUE, ignore.case = TRUE)) %>% unique()) %>% print
dfx$Industry[dfx$Employer %in% strategyAdvise & !dfx$Employer %in% c(bankers,capital) & dfx$Industry == ""] <- "Consulting"; rm(strategyAdvise,bankers,capital)
travel <- sort(grep("airline|aviation|flight|travel$|vacation|cruise|limousine|limo ", dfx$Employer, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Industry[dfx$Employer %in% travel & dfx$Industry == ""] <- "Travel"; rm(travel)
partners <- sort(grep("partners$| partners ", dfx$Employer, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Industry[dfx$Employer %in% partners & dfx$Industry == ""] <- "Finance"; rm(partners)
philanth <- sort(grep("foundation$|charitable", dfx$Employer, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
dfx$Industry[dfx$Employer %in% philanth & dfx$Industry == ""] <- "Philanthropy"; rm(philanth)
dfx$Industry[dfx$Employer %in% mktSales & dfx$Industry == ""] <- "Media / Advertising"; rm(mktSales)
dfx$Industry[dfx$Employer %in% selfEmp] <- "Self Employed"; rm(selfEmp)
dfx$Industry[dfx$Employer %in% miscCo] <- "Misc"; rm(miscCo)
# x <- sort(grep("", dfx$Employer, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
# dfx$Industry[dfx$Employer %in% ] <- ""
# x <- sort(grep("studios$|studio$", dfx$Employer, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
# x <- sort(grep("group$", dfx$Employer, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
# x <- sort(grep("social work|reponsibility|assistance|social justice|and justice", dfx$Employer, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
# x <- sort(grep("center for|ctr for|cft", dfx$Employer, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print
# x <- sort(grep("women|woman|parenthood", dfx$Employer, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print


dfx$Industry2 <- dfx$Industry


# sort(unique(dfx$Industry))
# Sectors
# 1. Nonagriculture Wage & Salary
#     a) Goods Producing, Excluding Agriculture
#     b) Services Producing, Excluding Special Industries
# 2. Agriculture, Forestry, Fishing & Hunting
# 3. Nonagriculture Self-Employed

# Industries
# 1a. Mining, Construction, Manufacturing
# 1b. Utilities, Wholesale trade, Retail trade, Transportation & Warehousing, Information,
#    Financial Activities, Professional & Business Services, Educational Services, 
#    Health Care & Social Assistance, Leisure & Hospitality, Other Services, Federal Govt, State & Local Govt
# 2. Agriculture wage & salary, Agriculture Self-Employed

# https://en.wikipedia.org/wiki/Outline_of_industry#Industry_sectors
# Aerospace, Agriculture (fishing, timber, tobacco), Chemical (Pharmaceutical), Computer (software),
# Construction, Defense (Arms), Education, Energy (Electrical power & petroleum), Entertainment,
# Financial Services (Insurance), Food (fruit), Health Care, Hospitality, Information, 
# Manufacturing (Auto, electronic, pulp & paper, steel, shipbuilding), 
# Mass Media (Broadcasting, film, jusic, news, publishing, www), Mining, Telecomm (Internet),
# Transport, Water, Direct Selling


# Also good https://www.bls.gov/opub/ted/2014/ted_20140728.htm


# d <- filter(dfx, Industry != "")
# dy <- filter(dfx, Industry == "")
# 
# sort(unique(d$Industry))


# Industry groups
hc <- c("Psychiatry","Primary Care","Orthopedics","Oral Surgery","Optometry","Nursing Home",
        "Nursing","Nephrology","Medical Imaging","Hospital","Home Healthcare","Healthcare","lifespan",
        "Health Records","Dental","Anesthesiology","Rehab","Biotech","Ambulance","CNE","Dermatology","Internal Medicine","Lifespan","Hospital",
        "Maxillofacial","Medicine","Neurology","Nursing Home","OBGYN","Gastroenterology",
        "Ophthalmology","Pediatrics","Physician","Radiology","Senior Living")
real <- c("Realty","Real Estate","Property Management")
govt <- c("Government","Federal Government","Local Government","State Government","Local Government")
auto <- c("Autobody","Auto Sales")
lobbyCon <- c("Lobby","Consulting")
media <- c("Advertising", "Media")
ins <- c("Health Insurance","Insurance")
construct <- c("Construction","Masonry","Landscaping","Electric","HVAC")
edu <- c("Early Education","Education","Higher Education","School")
tech <- c("Biotech","Technology","Tech","New Technology","Science")
energy <- c("Clean Energy","Energy","Fossil Fuel","Utilities")
food <- c("Alcohol","Restaurant")
ent <- c("Art","Arts","Arts & Entertainment","Gambling","Nightlife","Recreation")

dfx$Industry2[dfx$Industry %in% real] <- "Real Estate"
dfx$Industry2[dfx$Industry %in% govt] <- "Government"
dfx$Industry2[dfx$Industry %in% tech] <- "Technology"
dfx$Industry2[dfx$Industry %in% lobbyCon] <- "Lobbying / Consulting"
dfx$Industry2[dfx$Industry %in% hc] <- "Healthcare"
dfx$Industry2[dfx$Industry %in% construct] <- "Construction"
dfx$Industry2[dfx$Industry %in% food] <- "Food & Beverage"
dfx$Industry2[dfx$Industry %in% edu] <- "Education Services"
dfx$Industry2[dfx$Industry == "Mortgage"] <- "Finance"
dfx$Industry2[dfx$Industry %in% energy] <- "Energy"
dfx$Industry2[dfx$Industry %in% ent] <- "Arts & Entertainment"


# Clear
rm(auto,construct,govt,hc,ins,lobbyCon,media,real,edu,tech,energy,food,ent)


# ********************************************                                 ******************************************************
# ********************************************                                 ******************************************************
# ********************************************  Finish Formatting Employer Names  ***************************************************
# ********************************************                                 ******************************************************
# ***********************************************************************************************************************************
# ***********************************************************************************************************************************
# ***********************************************************************************************************************************
# ***********************************************************************************************************************************
# ***********************************************************************************************************************************
# ***********************************************************************************************************************************
# ********************************************                                 ******************************************************
# ********************************************  Begin Formatting Donor Names   ******************************************************
# ********************************************                                 ******************************************************
# ********************************************                                 ******************************************************

# Format donors
matiello <- sort(grep("mattiello esq, nicholas|mattiello, n|matiello, nick|mattielo, nick|mattiello, f|nick mattiello|mattiello, re|matielo|matiello|mattiello, rep nicholas", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% matiello] <- "Mattiello, Nicholas"; rm(matiello)
fung <- grep("fung, a|fung, mr a", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique %>% print
dfx$FullName[dfx$FullName %in% fung] <- "Fung, Allan"; rm(fung)
shekarchi <- grep("shekarchi, k j|shekarchi esq, hon k|shekarchi esq, k jos|shekarchi, f|shekarchi, joseph|shekarchi, joe", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique %>% print
dfx$FullName[dfx$FullName %in% shekarchi] <- "Shekarchi, Joe"; rm(shekarchi)
johnPetrarca <- grep("petrarca, john|petrarca, mr john|petarca, john", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique %>% print
dfx$FullName[dfx$FullName %in% johnPetrarca] <- "Petrarca, John"; rm(johnPetrarca)
joePaoline <- grep("paolino, joseph|paolino jr, joseph|paolino jr, mr|paolino, mr joseph|paolino jr, honorable", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique %>% print
dfx$FullName[dfx$FullName %in% joePaoline] <- "Paolino, Joseph"; rm(joePaoline)
DomRuggerio <- grep("ruggerio, domenic|dominick j ruggerio|ruggerio, senator dom|ruggerio, sen domnick|ruggerio, domenick|dominick j ruggerio|ruggerio, senator dom|ruggierio, domenick|ruggerio, sen dom|ruggiero, dominick|ruggerio, dominic|ruggerio, friends of dom|ruggiero, the hon dom|ruggerio, dominick|ruggerio, sen dom|ruggerio, hon dom|ruggiero, senator dom|ruggiero, dominick|ruggerio, dominick", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique %>% print
dfx$FullName[dfx$FullName %in% DomRuggerio] <- "Ruggerio, Dominick"; rm(DomRuggerio)
tomCasale <- grep("casale, mr thomas|casale, thomas", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique %>% print
dfx$FullName[dfx$FullName %in% tomCasale] <- "Casale, Thomas"; rm(tomCasale)
lisaMinsky <- grep("minsky-primus", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique %>% print
dfx$FullName[dfx$FullName %in% lisaMinsky] <- "Minsky-Primus, Lisa"; rm(lisaMinsky)
patLynch <- sort(grep("lynch, friends of patrick|lynch, hon patrick|lynch esquire, honorable patrick|lynch, patrick|lynch, atty patrick|patrick lynch, friends of|lynch  (friends of), patrick", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% patLynch] <- "Lynch, Patrick"; rm(patLynch)
sandPetrarca <- sort(grep("petrarca, sandra|petrarca, ms sandra", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% sandPetrarca] <- "Petrarca, Sandra"; rm(sandPetrarca)
petePetrarca <- sort(grep("petrarca, pet|petrarca, mr p|petrarca, hon peter|petrarca, pietro", dfx$FullName, value = TRUE, ignore.case = TRUE)) %>% unique %>% print
dfx$FullName[dfx$FullName %in% petePetrarca] <- "Petrarca, Peter"; rm(petePetrarca)
antPaolino <- sort(grep("paolino jr, anthony|paolino, anthony", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% antPaolino] <- "Paolino, Anthony"; rm(antPaolino)
louPaolino <- sort(grep("paolino, lou|paolino, mr lou", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% louPaolino] <- "Paolino, Louis"; rm(louPaolino)
tomBadway <- sort(grep("badway, thomas|badway, mr thomas", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% tomBadway] <- "Badway, Thomas"; rm(tomBadway)
ralphPal <- sort(grep("palumbo, ralph|palumbo jr, mr ralph|palumbo jr, ralph", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% ralphPal] <- "Palumbo, Ralph"; rm(ralphPal)
chrisVitale <- sort(grep("vitale, chris|vitale, mr chris", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% chrisVitale] <- "Vitale, Christopher"; rm(chrisVitale)
tomDepetrillo <- sort(grep("depetrillo, thom|de petrillo, thom|depetrillo, mr thom", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% tomDepetrillo] <- "Depetrillo, Thomas"; rm(tomDepetrillo)
rebeccaKislak <- sort(grep("kislak, rebecca|kislak brown|kislak, ms rebecca|brown, rebecca kislak", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% rebeccaKislak] <- "Kislak, Rebecca"; rm(rebeccaKislak)
patMorgan <- sort(grep("morgan, 4018623040|morgan, pat|morgan mrs pat", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% patMorgan] <- "Morgan, Patricia"; rm(patMorgan)
yarMinsky <- sort(grep("minsky, yaron", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% yarMinsky] <- "Minsky, Yaron"; rm(yarMinsky)
terryCortvriend <- sort(grep("cortvriend, terri", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% terryCortvriend] <- "Cortvriend, Terri"; rm(terryCortvriend)
johnPesce <- sort(grep("pesce, john|pesce, mr john|pesce, j robert|pesce, mr j robert|pesce, mr robert|pesce, robert|pesce, jrobert", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% johnPesce] <- "Pesce, John R"; rm(johnPesce)
andAnnaldo <- sort(grep("annaldo, andrew|annaldo, mr andrew", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% andAnnaldo] <- "Annaldo, Andrew"; rm(andAnnaldo)
zackMandell <- sort(grep("mandell, z|mandell esquire, zack", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% zackMandell] <- "Mandell, Zach"; rm(zackMandell)
joeRodio <- sort(grep("rodio jr|rodio esq, jo|rodio, joseph|rodio, mr joseph", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% joeRodio] <- "Rodio Jr, Joseph"; rm(joeRodio)
johnSavage <- sort(grep("savage esq|savage, john|savage, jon|savage, mr jon", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% johnSavage] <- "Savage, Jonathan"; rm(johnSavage)
terenceFacassa <- sort(grep("fracassa, ter|fracassa, mr ter|fracassa esq, ter", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% terenceFacassa] <- "Fracassa, Terence"; rm(terenceFacassa)
willMurphy <- sort(grep("murphy, william|murphy, mr will|murphy, willliam|murphy, wiliam|william murphy, friends of|murphy esq, will|murphy, atty will", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% willMurphy] <- "Murphy, William"; rm(willMurphy)
gragGabel <- sort(grep("gabel, greg|gabel, grogory|gabel, mr greg|greg, gabel", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% gragGabel] <- "Gabel, Gregory"; rm(gragGabel)
lizPerik <- sort(grep("perik, eliz|perik, ms eliz|beretta-perik, eliz|beretta perik, eliz", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% lizPerik] <- "Beretta-Perik, Elizabeth"; rm(lizPerik)
mikeDonegan <- sort(grep("donegan, michael", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% mikeDonegan] <- "Donegan, Michael"; rm(mikeDonegan)
jeffGrybowski <- sort(grep("grybowski, jeff|grybowski esq, jeff|grybowski, mr jeff", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% jeffGrybowski] <- "Grybowski, Jeffrey"; rm(jeffGrybowski)
steveIssa <- sort(grep("issa, steve|issa, mr stev", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% steveIssa] <- "Issa, Steven"; rm(steveIssa)
mikePerik <- sort(grep("perik jr, michael|perik, michael|perik, mike|perik, mr michael|perik, mr mike|perik jr, mike|perik jr, mr mi", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% mikePerik] <- "Perik, Michael"; rm(mikePerik)
marissaPesce <- sort(grep("pesce, mar", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% marissaPesce] <- "Pesce, Marissa"; rm(marissaPesce)
jonDuffy <- sort(grep("duffy, jon|duffy, john|duffy, mr jon|duffy, mr john", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% jonDuffy] <- "Duffy, Jonathan"; rm(jonDuffy)
mattLopes <- sort(grep("lopes, matt|lopes, jr, mr matthew|lopes, mr matt|lopes jr, mathew|lopes esq, mathew|lopes jr, atty matt|lopes, mr matthew|lopes, matt$|lopes jr, matt|lopes jr, mr matt|lopes, mathew", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% mattLopes] <- "Lopes Jr, Matthew A"; rm(mattLopes)
joeWalsh <- sort(grep("walsh, joseph$|walsh, joseph w|walsh, mr jos", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% joeWalsh] <- "Walsh, Joseph"; rm(joeWalsh)
ernieBaptista <- sort(grep("baptista jr, ern|baptista, ern|baptista, mr e", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% ernieBaptista] <- "Baptista, Ernest"; rm(ernieBaptista)
bradDimeo <- sort(grep("dimeo, brad|dimeo, mr brad", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% bradDimeo] <- "Dimeo, Bradford"; rm(bradDimeo)
jeffGuimond <- sort(grep("guimond, jeff|guimond, mr j", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% jeffGuimond] <- "Guimond, Jefferson"; rm(jeffGuimond)
carolOdonnell <- sort(grep("o'donnell, carol|o'donnell, ms car", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% carolOdonnell] <- "O'Donnell, Carol"; rm(carolOdonnell)
edGalvin <- sort(grep("galvin, ed|galvin, mr ed", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% edGalvin] <- "Galvin, Edward"; rm(edGalvin)
kenMancini <- sort(grep("mancini, ken|mancini, mr ken", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% kenMancini] <- "Mancini, Kenneth"; rm(kenMancini)
paivaWeed <- sort(grep("paiva-weed|paiva weed", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% paivaWeed] <- "Paiva Weed, Teresa"; rm(paivaWeed)
georgeZainyeh <- sort(grep("zainyeh, george|zainyeh, mr george", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% georgeZainyeh] <- "Zainyeh, George"; rm(georgeZainyeh)
carolMurray <- sort(grep("murray, carol|murray, f/s carol|murray, ms car", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% carolMurray] <- "Murray, Carolyn"; rm(carolMurray)
johnBentz <- sort(grep("bentz, john|bentz, mr j", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% johnBentz] <- "Bentz, John"; rm(johnBentz)
mikeDambra <- sort(grep("d'ambra, michael$|d'ambra, michael v|dambra, michael|d'ambra, mr mi|d'ambra, mike|dambra, mike", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% mikeDambra] <- "D'Ambra, Michael"; rm(mikeDambra)
willFisher <- sort(grep("fischer, will|fischer, mr wil|fischer, bill", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% willFisher] <- "Fischer, William"; rm(willFisher)
karenGrande <- sort(grep("grande, karen|grande, ms kar", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% karenGrande] <- "Grande, Karen"; rm(karenGrande)
geraldHar <- sort(grep("harrington, ger|harrington, mr ger", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% geraldHar] <- "Harrington, Gerald"; rm(geraldHar)
ressHryzan <- sort(grep("hryzan, russ|hryzan, mr ru", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% ressHryzan] <- "Hryzan, Russell"; rm(ressHryzan)
johnJusto <- sort(grep("justo", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% johnJusto] <- "Justo, John"; rm(johnJusto)
joeMcnamara <- sort(grep("mcnamara, jos|mcnamara, mr j|mcnamara, joe", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% joeMcnamara] <- "Mcnamara, Joseph"; rm(joeMcnamara)
ryanPearson <- sort(grep("pearson, friends|pearson, ryan|pearson, mr ry", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% ryanPearson] <- "Pearson, Ryan"; rm(ryanPearson)
donSweitzer <- sort(grep("sweitzer, don|sweitzer, mr d", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% donSweitzer] <- "Sweitzer, Donald"; rm(donSweitzer)
allTown <- sort(grep("townsend, alis|townsend, mrs alis|townsend, al|townsend, ms al", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% allTown] <- "Townsend, Alison"; rm(allTown)
daveUrsillo <- sort(grep("ursillo, dav|ursillo, mr dav", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% daveUrsillo] <- "Ursillo, David"; rm(daveUrsillo)
danHarrop <- sort(grep("harrop, dan|harrop, dr dan|harrop, md dan|harrop, md dr dan|harrop 3rd, d|harrop 3rd, d|harrop iii, d|harrop, mr d|harrop, md m|harrop, mba, dr", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% danHarrop] <- "Harrop MD, Daniel"; rm(danHarrop)
artCorvese <- sort(grep("corvese, art|corvese od, art|corvese, dr art|corvese od, dr art|corvese, mr a|corvese od, mr|corvese, rep arthur|corvese, representative|for representative, corvese", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% artCorvese] <- "Corvese OD, Arthur"; rm(artCorvese)
johnHoliver <- sort(grep("holiver, john|holiver, mr jo|holiver, jon", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% johnHoliver] <- "Holiver, John"; rm(johnHoliver)
ashTal <- sort(grep("akshay|talwar", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% ashTal] <- "Talwar, Akshay"; rm(ashTal)
richMiga <- sort(grep("miga jr, rich|miga, rich|miga, mr rich|miga jr, mr ri|miga, dick|miga jr, dick|miga, mr dick|miga jr, mr dick", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% richMiga] <- "Miga Jr, Richard"; rm(richMiga)
markLescault <- sort(grep("lescault, mark|lescault, mr mark", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% markLescault] <- "Lescault, Mark"; rm(markLescault)
jamesBen <- sort(grep("bennett, james|bennett, mr jam|bennett, jim|bennett, mr jim", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% jamesBen] <- "Bennett, James"; rm(jamesBen)
lisAbb <- sort(grep("abbott, lisa|abbott, ms lisa|abbott, mrs lisa", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% lisAbb] <- "Abbott, Lisa"; rm(lisAbb)
donAm <- sort(grep("amaral, donna|amaral, ms donna|amaral, mrs donna", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% donAm] <- "Amaral, Donna"; rm(donAm)
jacBag <- sort(grep("baginski, jac", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% jacBag] <- "Baginski, Jacquelyn"; rm(jacBag)
steveBake <- sort(grep("bakios, stephen|bakios, dr ste|bakios, mr ste", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% steveBake] <- "Bakios, Steven"; rm(steveBake)
daveBal <- sort(grep("balasco, dav|balasco, mr dav", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% daveBal] <- "Balasco, David"; rm(daveBal)
bankiMoh <- sort(grep("banki dmd, dr mo|banki, dr mo|banki, mo|banki dmd, mo", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% bankiMoh] <- "Banki DMD, Mohammad"; rm(bankiMoh)
peteBaz <- sort(grep("baziotis, dr pet|baziotis, pet|baziotis, mr pet|baziotis md", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% peteBaz] <- "Baziotis MD, Peter"; rm(peteBaz)
mikeBig <- sort(grep("bigney, mi|bigney, mr mi", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% mikeBig] <- "Bigney, Michael"; rm(mikeBig)
bouchLin <- sort(grep("bouchard, linds", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% bouchLin] <- "Bouchard, Lindsey"; rm(bouchLin)
richCerz <- sort(grep("cervone, rich|cervone, mr ri|cervone, dick|cervone dr, richard|cervone, dr richard", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% richCerz] <- "Cervone, Richard"; rm(richCerz)
jasChap <- sort(grep("chopoorian, jason|choporian, jason|choporian, mr j|chopoorian, mr j", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% jasChap] <- "Chopoorian, Jason"; rm(jasChap)
johnChoop <- sort(grep("chopoorian, john|chopoorian, mr jo", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% johnChoop] <- "Chopoorian, John"; rm(johnChoop)
kimCio <- sort(grep("ciociola, kim", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% kimCio] <- "Ciociola, Kimb"; rm(kimCio)
carlCor <- sort(grep("corrow, dr carl|corrow, carl", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% carlCor] <- "Corrow MD, Carl"; rm(carlCor)
shawnCorn <- sort(grep("cournoyer, sh|cournoyer, mr sh", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% shawnCorn] <- "Cournoyer, Shaun"; rm(shawnCorn)
joeDowl <- sort(grep("dowling jr, jo|dowling, jo|dowling, mr jo|dowling jr, dr jos|dowling jr,, jos", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% joeDowl] <- "Dowling, Joseph"; rm(joeDowl)
mikEarl <- sort(grep("ehrlich, michael|ehrlich, mike|ehrlich, mr michael|ehrlich, mr mik", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% mikEarl] <- "Ehrlich, Michael"; rm(mikEarl)
kathEpp <- sort(grep("^epp, kath", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% kathEpp] <- "Epp, Katharine"; rm(kathEpp)
jenFair <- sort(grep("fairbank, jen|fairbank rn, jen", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% jenFair] <- "Fairbank, Jennifer"; rm(jenFair)
bruceFish <- sort(grep("fischer, bruce|fischer, mr bruce", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% bruceFish] <- "Fischer, Bruce"; rm(bruceFish)
louGian <- sort(grep("giancola, lou|giancola, mr lou", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% louGian] <- "Giancola, Louis"; rm(louGian)
debGriffin <- sort(grep("griffin, deb", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% debGriffin] <- "Griffin, Deborah"; rm(debGriffin)
grzych <- sort(grep("grzych, scott|grzych, mr scott", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% grzych] <- "Grzych, Scott"; rm(grzych)
huHall <- sort(grep("hall, hugh|hall, mr hug", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% huHall] <- "Hall, Hugh"; rm(huHall)
hartman <- sort(grep("hartman, fred|hartman, mr fred", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% hartman] <- "Hartman, Frederick"; rm(hartman)
janeHay <- sort(grep("hayward, jane|hayward, ms jan", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% janeHay] <- "Hayward, Jane"; rm(janeHay)
briHo <- sort(grep("hogan, brian|hogan, dr brian|hogan dmd, brian|hogan, mr bri|hogan dmd, mr bri", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% briHo] <- "Hogan, Brian"; rm(briHo)
connieHow <- sort(grep("howes, constance|howes esq, constance|howes, mr constance", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% connieHow] <- "Howes, Constance"; rm(connieHow)
frankIac <- sort(grep("iacono, frank|iacono jr, frank|iacono, mr frank", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% frankIac] <- "Iacono, Frank"; rm(frankIac)
jonKap <- sort(grep("kaplan dr, jon|kaplan, jon|kaplan, mr jo", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% jonKap] <- "Kaplan, Jonathan"; rm(jonKap)
adibKa <- sort(grep("karam, adib", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% adibKa] <- "Karam, Adib"; rm(adibKa)
dennis <- sort(grep("keefe, den|keefe, mr den", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% dennis] <- "Keefe, Dennis"; rm(dennis)
charlKen <- sort(grep("kenoian, charles|kenoian, mr ch", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% charlKen] <- "Kenoian, Charles"; rm(charlKen)
trevKin <- sort(grep("kinney, trevor|kinney, mr tre", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% trevKin] <- "Kinney, Trevor"; rm(trevKin)
franKow <- sort(grep("kowalik, francis|kowalik, frank|kowalik, mr fran", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% franKow] <- "Kowalik, Francis"; rm(franKow)
kLal <- sort(grep("lally, karen|lally, ms kar", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% kLal] <- "Lally, Karen"; rm(kLal)
lJames <- sort(grep("lehane, james|lehane iii, james|lehane, mr jam|lehane, jim", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% lJames] <- "Lehane, James"; rm(lJames)
loisLy <- sort(grep("lyman, lois|lyman, ms lo", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% loisLy] <- "Lyman, Lois"; rm(loisLy)
keithMac <- sort(grep("macksoud, keith|macksoud, friends of, keith|macksoud, mr keith", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% keithMac] <- "Macksoud, Keith"; rm(keithMac)
debMag <- sort(grep("maguire, deb|maguire, ms deb", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% debMag] <- "Maguire, Debra"; rm(debMag)
pamMark <- sort(grep("marchetti, pam|marchetti, ms pam", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% pamMark] <- "Marchetti, Pamela"; rm(pamMark)
debbieMc <- sort(grep("mcinteer, debbi|mcinteer, ms deb", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% debbieMc] <- "Mcinteer, Debbi"; rm(debbieMc)
stellaMor <- sort(grep("moran, stella|moran, ms stella", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% stellaMor] <- "Moran, Stella"; rm(stellaMor)
normCat <- sort(grep("norman, kat|norman, ms kat", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% normCat] <- "Norman, Katherine"; rm(normCat)
demOwl <- sort(grep("ouellette, demetra|oulette, demetra|ouellette, ms demetra|oulette, ms demetra", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% demOwl] <- "Ouellette, Demetra"; rm(demOwl)
hernPad <- sort(grep("padilla jr, hernan|padilla, hernan|padilla jr, mr hernan|padilla, mr hernan", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% hernPad] <- "Padilla, Hernan"; rm(hernPad)
frankPal <- sort(grep("paletta, frank|paletta, dr frank|paletta, mr frank", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% frankPal] <- "Paletta, Frank"; rm(frankPal)
chePer <- sort(grep("perry, cheryl|perry, ms cher", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% chePer] <- "Perry, Cheryl"; rm(chePer)
rotAng <- sort(grep("rotella, angelo|rotella esq, mr angelo", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% rotAng] <- "Rotella, Angelo"; rm(rotAng)
mattRot <- sort(grep("rotella, matt|rotella, mr matt", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% mattRot] <- "Rotella, Matthew"; rm(mattRot)
daveRyan <- sort(grep("^ryan, dav|^ryan, mr dav", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% daveRyan] <- "Ryan, David"; rm(daveRyan)
kevRyan <- sort(grep("^ryan, kevin|^ryan, mr kev", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% kevRyan] <- "Ryan, Kevin"; rm(kevRyan)
artSamp <- sort(grep("sampson, art|sampson, mr art", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% artSamp] <- "Sampson, Arthur"; rm(artSamp)
leonSam <- sort(grep("samuelian, 64|samuelian, leon|samuelian, mr leon", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% leonSam] <- "Samuelian, Leon"; rm(leonSam)
steveSantos <- sort(grep("santos jr, ste|santos, dr ste|santos, steven|santos, stephen", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% steveSantos] <- "Santos, Steven"; rm(steveSantos)
johnSepe <- sort(grep("sepe, john s|sepe, mr jo", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% johnSepe] <- "Sepe, John"; rm(johnSepe)
katShark <- sort(grep("sharkey, dr kat|sharkey, kat", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% katShark] <- "Sharkey, Kat"; rm(katShark)
steveSkol <- sort(grep("skoly jr, dr ste|skoly, dr stephen|skoly, dr steven|skoly, stephen|skoly, stephen", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% steveSkol] <- "Skoly DMD, Stephen"; rm(steveSkol)
patSos <- sort(grep("soscia, patricia", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% patSos] <- "Soscia, Patricia"; rm(patSos)
willStan <- sort(grep("stanley, will|stanley, bill|stanley, mr wil|stanley, mr bil", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% willStan] <- "stanley, william"; rm(willStan)
sheSweitz <- sort(grep("sweitzer, sheri|sweitzer, sherry", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% sheSweitz] <- "Sweitzer, Sheri"; rm(sheSweitz)
vinTromb <- sort(grep("trombetti, vincent|trombetti, mr vincent", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% vinTromb] <- "Trombetti, Vincent"; rm(vinTromb)
margVan <- sort(grep("van bree, margaret", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% margVan] <- "Van Bree, Margaret"; rm(margVan)
joanWoods <- sort(grep("woods, joan|woods, ms jo|woods, mrs jo", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% joanWoods] <- "Woods, Joan"; rm(joanWoods)
clayPell <- sort(grep("pell iv, herbert|pell, clay|pell, herbert claiborne|pell, h clay", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% clayPell] <- "Pell, Herbert Claiborne"; rm(clayPell)
senPell <- sort(grep("pell, the honorable|pell, sen claiborne|pell, claiborne", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% senPell] <- "Pell, Sen Claiborne"; rm(senPell)
york <- sort(grep("^york, hon myr|^york, ms myr|^york, myr", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% york] <- "York, Myrth"; rm(york)
kBlock <- sort(grep("^block, k|block, mr k", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% kBlock] <- "Block, Kenneth"; rm(kBlock)
sethMag <- sort(grep("magaziner, seth|magaziner, mr seth", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% sethMag] <- "Magaziner, Seth"; rm(sethMag)
shelWhite <- sort(grep("whitehouse, hon sheldon|whitehouse, sheldon|whitehouse, mr sheldon", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% shelWhite] <- "Whitehouse, Sheldon"; rm(shelWhite)
frankCap <- sort(grep("caprio, mr frank|caprio, frank|caprio jr, frank", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% frankCap] <- "Caprio, Frank"; rm(frankCap)
daveCic <- sort(grep("cicilline, mayor david|cicilline, mr david|cicilline, hon david|cicilline, david|cicilline esq, the hon david", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% daveCic] <- "Cicilline, David"; rm(daveCic)
deRamel <- sort(grep("deramel, guillaume h|de ramel, mr guillaime|de ramel, guillame|de ramel, guillaume|de remel, guillaume|deramel, guillaume", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% deRamel] <- "De Ramel, Guillaume"; rm(deRamel)
mikeSol <- sort(grep("solomon, michael|solomon, michale a|solomon, mr michael", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% mikeSol] <- "Solomon, Michael"; rm(mikeSol)
gFox <- sort(grep("fox, friends of gordon|fox esq, gordon|fox, gorton|fox, gordon|fox, gord$|fox, rep gordon|gordon fox, friends of|fox, rep gordon|fox, hon gordon|fox, mr gordon", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% gFox] <- "Fox, Gordon"; rm(gFox)
pKil <- sort(grep("kilmartin, friends of|kilmartin, mr peter|kilmartin, peter", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% pKil] <- "Kilmartin, Peter"; rm(pKil)
domCarp <- sort(grep("carpianato, domenic|carpionato, domenic", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% domCarp] <- "Carpianato, Domenic"; rm(domCarp)
alCarp <- sort(grep("caprionato, alfred|carpianato, alfred|carpionato, alfred|carpionato, alfrred|carpionato, mr alfred", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% alCarp] <- "Carpianato, Alfred"; rm(alCarp)
domDel <- sort(grep("del monico, domenic|delmonico, domenic", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% domDel] <- "Delmonico, Domenic"; rm(domDel)
jShe <- sort(grep("sheehan, james$|sheehan, james c|sheehan, sen james", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% jShe] <- "Sheehan, James"; rm(jShe)
mikeNap <- sort(grep("napolitano esq, michael|napalitano, michael|napolitano, 9422894 michael|napolitano, hon michael|napolitano, michaeal|napolitano, michael|napolitano, mr michael|napolitano, mike", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% mikeNap] <- "Napolitano, Michael"; rm(mikeNap)
rebSchif <- sort(grep("schiff, rebecca", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% rebSchif] <- "Schiff, Rebecca"; rm(rebSchif)
jerKap <- sort(grep("kapstein, jeremy", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% jerKap] <- "Kapstein, Jeremy"; rm(jerKap)
arch <- sort(grep("archambault, stephen|friends of stephen archambault|archambault, steve|archambault, stephen|archambault, honorable stephen", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% arch] <- "Archambault, Stephen"; rm(arch)
sped <- sort(grep("dickinson, spence", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% sped] <- "Dickinson, Spencer"; rm(sped)
garab <- sort(grep("garabedian, aram|garabedian, hon aram|garabedian, mr aram", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% garab] <- "Garabedian, Aram"; rm(garab)
davIz <- sort(grep("igliozzi esq, david|igliozzi, mr david|igliozzi, david", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% davIz] <- "Igliozzi, David"; rm(davIz)
wilGil <- sort(grep("gilbert, william", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% wilGil] <- "Gilbert, William"; rm(wilGil)
gov <- sort(grep("raimondo esq, gina|raimondo, gina", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% gov] <- "Raimondo, Gina"; rm(gov)
wilHar <- sort(grep("harsch esq, j will|harsch, j will|harsch, will", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% wilHar] <- "Harsch, William"; rm(wilHar)
jonLom <- sort(grep("lombardi, john|lombardi, mr john", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% jonLom] <- "Lombardi, John"; rm(jonLom)
steveTetz <- sort(grep("tetzner, ste|tetzner, mr ste|tetzner 216", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% steveTetz] <- "Tetzner, Steven"; rm(steveTetz)
joeSol <- sort(grep("solomon, jos|solomon, mr jos", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
shawmut <- sort(grep("shawo", dfx$Address, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% joeSol & dfx$Address %in% shawmut] <- "Solomon, Joseph J"; rm(joeSol)
lizRob <- sort(grep("roberts, hon eliz|roberts, eliz|roberts, ms eliz|roberts ltgov, hon eliz", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% lizRob] <- "Roberts, Elizabeth"; rm(lizRob)
joeFern <- sort(grep("fernandez, jos|fernandez, mr jos", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% joeFern] <- "Fernandez, Joseph"; rm(joeFern)
johnFlorez <- sort(grep("florez, john|florez, mr john", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% johnFlorez] <- "Florez, John"; rm(johnFlorez)
blakeFil <- sort(grep("filippi, blake|filippi, mr blake", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% blakeFil] <- "Filippi, Blake"; rm(blakeFil)
chuckFog <- sort(grep("fogarty, charles", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% chuckFog] <- "Fogarty, Charles"; rm(chuckFog)
joeBurch <- sort(grep("burchfield, jos|burchfield, mr jos|burchfield, joe", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% joeBurch] <- "Burchfield, Joseph"; rm(joeBurch)
smileBret <- sort(grep("smiley, bret|smiley, mr brett", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% smileBret] <- "Smiley, Brett"; rm(smileBret)
lilChris <- sort(grep("little, chris|little, mr chris", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% lilChris] <- "Little, Christopher"; rm(lilChris)
jorge <- sort(grep("elorza, jorge|elorza, mr jorge", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% jorge] <- "Elorza, Jorge"; rm(jorge)
gregCon <- sort(grep("constantino, greg|constantino, mr greg|costantino, greg|costantino, mr greg", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% gregCon] <- "Constantino, Gregory"; rm(gregCon)
ernieAl <- sort(grep("almonte, ern|almonte cpa", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% ernieAl] <- "Almonte, Ernest"; rm(ernieAl)
cBoy <- sort(grep("boyle, chris|boyle, mr chris", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% cBoy] <- "Boyle, Christopher"; rm(cBoy)
kevMcken <- sort(grep("mckenna, kev|mckenna, mr kev|mckenna esq, kev", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% kevMcken] <- "Mckenna, Kevin"; rm(kevMcken)
buddy <- sort(grep("cianci, vin|cianci jr, vin", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% buddy] <- "Cianci, Vincent"; rm(buddy)
johnPag <- sort(grep("pagliarini jr, john|pagliarini jr, mr john|pagliarini, john|pagliarini, mr john", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% johnPag] <- "Pagliarini, John"; rm(johnPag)
rKelly <- sort(grep("sheridan esq, r kelly|sheridan esq, rkelly|sheridan, atty r kelly|sheridan, kelly|sheridan, r  kelly|sheridan, kelly r|sheridan, r k|sheridan, rkelly|sheridan, mr r kelly|sheridan, robert k", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% rKelly] <- "Sheridan, Robert Kelly"; rm(rKelly)
johnRob <- sort(grep("robitaille, john|robitaille for", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% johnRob] <- "Robitaille, John"; rm(johnRob)
markShwag <- sort(grep("schwager, mark|schwager, mr mark", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% markShwag] <- "Schwager, Mark"; rm(markShwag)
danMac <- sort(grep("mckee, dan|mckee, friends of dan", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% danMac] <- "Mckee, Daniel"; rm(danMac)
donGreb <- sort(grep("grebien, don|grebien, doanld|grebien, friends of, don", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% donGreb] <- "Grebien, Donald"; rm(donGreb)
maryShal <- sort(grep("shallcross", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% maryShal] <- "Shallcross-Smith, Mary Ann"; rm(maryShal)
antGia <- sort(grep("giarrusso, ant|giarrusso, mr ant", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% antGia] <- "Giarrusso, Anthony"; rm(antGia)
salReb <- sort(grep("rebecchi, sa", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% salReb] <- "Rebecchi, Saverio"; rm(salReb)
johnCul <- sort(grep("cullen jr, john|cullen, john", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% johnCul] <- "Cullen, John"; rm(johnCul)
joeMont <- sort(grep("montalbano esq, jos|montalbano mrs, jos|montalbano, hon jos|montalbano, sen jos|montalbano, senator jos|montalbano, jos", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% joeMont] <- "Montalbano, Joseph"; rm(joeMont)
tomWin <- sort(grep("winfield, hon thom|winfield, mr thom|winfield, representative thom|winfield, thom", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% tomWin] <- "Winfield, Thomas"; rm(tomWin)
robGold <- sort(grep("goldberg, rob|goldberg esq, rob|goldberg, atty rob|goldberg, mr rob|goldberg, rob", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% robGold] <- "Goldberg, Robert"; rm(robGold)
richMc <- sort(grep("mcauliffe jr, mr rich|mcauliffe jr, rich|mcauliffe, jr, rich|mcauliffe, rich|mcauliffe, rick", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% richMc] <- "Mcauliffe, Richard"; rm(richMc)
joeLar <- sort(grep("larisa jr, jos|larisa, joe|larisa, jos", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% joeLar] <- "Larisa, Joseph"; rm(joeLar)
davCar <- sort(grep("carlin iii, dav|carlin iii, mr dav|carlin, dav|carlin, mr dav|carlin jr, dav|carlin jr, mr dav", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% davCar & dfx$Address == "3 rose st"] <- "Carlin III, David"; rm(davCar)
sarDow <- sort(grep("dowling, sar|dowling, ms sar", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% sarDow] <- "Dowling, Sarah"; rm(sarDow)
antPires <- sort(grep("pires, ant|pires, tony", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% antPires] <- "Pires, Anthony"; rm(antPires)
miAck <- sort(grep("ackerman, mia|mia ackerman", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% miAck] <- "Ackerman, Mia"; rm(miAck)
merSher <- sort(grep("sherman, merr|sherman, mr merrill|merrill, sherman", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% merSher] <- "Sherman, Merrill"; rm(merSher)
jSkef <- sort(grep("skeffington, james|skeffington, mr james", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% jSkef] <- "Skeffington, James"; rm(jSkef)
richCor <- sort(grep("corrente, rich", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% richCor] <- "Corrente, Richard"; rm(richCor)
robRuss <- sort(grep("russo, rob|russo esq, rob|russo, esq, rob|russo, mr rob", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% robRuss] <- "Russo, Robert"; rm(robRuss)
markWein <- sort(grep("weiner, mar|weiner, mr mar|weiner sr, mark|weiner, mr mark", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% markWein] <- "Weiner, Mark"; rm(markWein)
johnAss <- sort(grep("assalone jr, john|assalone, john|assalone, mr john", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% johnAss] <- "Assalone, John"; rm(johnAss)
wasylyk <- sort(grep("wasylyk, peter", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% wasylyk] <- "Wasylyk, Peter"; rm(wasylyk)
richBread <- sort(grep("bready, mr rich|bready, rich", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% richBread] <- "Bready, Richard"; rm(richBread)
wilFar <- sort(grep("farrell, will|farrell esq, will|farrell, 2206|farrell, atty will|farrell, bill|farrell, mr will", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% wilFar] <- "Farrell, William"; rm(wilFar)

jonMan <- sort(grep("mancini esq, john o|mancini, john|mancini, mr john", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% jonMan & !dfx$EmployerName %in% c("mancini building contractors","mancini gas station","mancini service station","retired")] <- "Mancini, John O"; rm(jonMan)
mAzar <- sort(grep("azar, milan|azar esq, milan", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% mAzar] <- "Azar, Milan"; rm(mAzar)

mikeKel <- sort(grep("kelly, michael|kelly, mr michael|kelly esq, michael", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% mikeKel] <- "Kelly, Michael"; rm(mikeKel)

lanzi <- sort(grep("lanzi, bea", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% lanzi] <- "Lanzi, Beatrice"; rm(lanzi)
woods <- sort(grep("woods, michael", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% woods] <- "Woods, Michael"; rm(woods)
Jwoods <- sort(grep("woods, james", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Jwoods] <- "Woods, James"; rm(Jwoods)
Caprio <- sort(grep("caprio, david", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Caprio] <- "Caprio, David"; rm(Caprio)
Cardi <- sort(grep("cardi ii, ste|cardi jr, steve|cardi, mr ste|cardi, ste", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Cardi] <- "Cardi, Stephen"; rm(Cardi)
Singleton <- sort(grep("singleton, rich|singleton, rick", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Singleton] <- "Singleton, Richard"; rm(Singleton)
Mcconnell <- sort(grep("mcconnell jr, john|mcconnell jr, mr john|mcconnell, john", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Mcconnell] <- "Mcconnell, John"; rm(Mcconnell)
Goldman <- sort(grep("goldman esq, brian|goldman, brian", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Goldman] <- "Goldman, Brian"; rm(Goldman)
Ferri <- sort(grep("ferri, frank|ferri, mr frank", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Ferri] <- "Ferri, Frank"; rm(Ferri)
Chace <- sort(grep("Chace ii, arnold|chace jr, arnold|chace jr, mr arnold|chace, arnold|chace, jr, arnold|chace, mr arnold", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Chace] <- "Chace, Arnold"; rm(Chace)

Sepe <- sort(grep("Sepe, michael|sepe jr, michaelsepe, mike|sepe, mr michael", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
trees <- sort(grep("tree| tire |retired", dfx$EmployerName, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
dfx$FullName[dfx$FullName %in% Sepe & dfx$Address != "304 carpenter rd" & !dfx$EmployerName %in% trees] <- "Sepe, Michael"; rm(Sepe,trees)

Hassenfeld <- sort(grep("Hassenfeld, alan|hassenfeld, mr alan|hassenfeld, allan", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Hassenfeld] <- "Hassenfeld, Alan"; rm(Hassenfeld)
Maselli <- sort(grep("Maselli, chris|maselli, mr chris", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Maselli] <- "Maselli, Christopher"; rm(Maselli)
Mcmahon <- sort(grep("Mcmahon, fran|mcmahon, mr fran|mcmahon esq,, fran", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Mcmahon] <- "Mcmahon, Francis"; rm(Mcmahon)
Alves <- sort(grep("Alves, hon stephen|alves jr, steve|alves, steve|alves, steph", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Alves] <- "Alves, Stephen"; rm(Alves)
Augustus <- sort(grep("Uht", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Augustus] <- "Uht, Augustus"; rm(Augustus)
Brady <- sort(grep("Brady, alice", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Brady] <- "Brady, Alice"; rm(Brady)
Merolla <- sort(grep("Merolla esquire, steve|merolla, steve", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Merolla] <- "Merolla, Steven"; rm(Merolla)
Beretta <- sort(grep("beretta jr, atty rich|beretta jr, rich|beretta, mr rich|beretta, rich|beretta jr, rich|beretta esq, rich", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Beretta] <- "Beretta Jr, Richard"; rm(Beretta)

Hogan <- sort(grep("Hogan, thom|hogan esq, thom|hogan, atty thom|hogan, thomas", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Hogan] <- "Hogan, Thomas P"; rm(Hogan)
Beretta <- sort(grep("Beretta, joseph|beretta, mr joseph", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Beretta] <- "Beretta, Joseph R"; rm(Beretta)
Frias <- sort(grep("Frias, steve", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Frias] <- "Frias, Steven"; rm(Frias)
William <- sort(grep("O'Brien, will", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% William] <- "O'Brien, William"; rm(William)
Blais <- sort(grep("Blais, leo$|blais, mr leo|blais, leo r", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Blais] <- "Blais, Leo"; rm(Blais)
Lepore <- sort(grep("Lepore esq, james|lepore esq, mr james|lepore, james|lepore, mr james", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Lepore] <- "Lepore, James"; rm(Lepore)
Steven <- sort(grep("Campo, steve|campo$", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Steven] <- "Campo, Steven"; rm(Steven)
Choquette <- sort(grep("Choquette jr, mr paul|choquette jr, paul|choquette, jr, paul|choquette, paul", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Choquette] <- "Choquette Jr, Paul"; rm(Choquette)
Roberts <- sort(grep("Roberts ii, dennis|roberts ii, mr dennis|roberts iii, dennis|roberts jr, dennis|roberts ii, atty dennis|roberts, dennis", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Roberts] <- "Roberts, Dennis"; rm(Roberts)
Herreshoff <- sort(grep("Herreshoff", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Herreshoff] <- "Herreshoff, Halsey"; rm(Herreshoff)
Gallison <- sort(grep("Gallison jr, ray|gallison, friends of ray|gallison, ray|gallison jr, rep ray|ray gallison", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Gallison] <- "Gallison, Ray"; rm(Gallison)
Noble <- sort(grep("butera-noble, linda|butera noble, linda|noble, linda", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Noble] <- "Noble, Linda"; rm(Noble)
Fogarty <- sort(grep("Fogarty, cyn|fogarty, ms cyn", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Fogarty] <- "Fogarty, Cynthia"; rm(Fogarty)
Cavanagh <- sort(grep("Cavanagh, max|cavanagh, mrs max", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Cavanagh] <- "Cavanagh, Maxine"; rm(Cavanagh)




Conley <- sort(grep("Conley, will|conley jr, will|conley jr, hon will|conley jr, friends of billy|conley, bill|conley, mr will|conley, sen will", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Conley] <- "Conley, William J"; rm(Conley)
Waterson <- sort(grep("Waterson, bruce", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Waterson] <- "Waterson, Bruce"; rm(Waterson)
Kevin <- sort(grep("o'neill, kevin|o'neill, mr kev", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Kevin] <- "O'Neill, Kevin"; rm(Kevin)
Lombardi <- sort(grep("lombardo iii, frank|lombardo, frank|lombardo iii, sen frank|lombardo iii, honorable frank|Lombardi, frank|lombardi esq, frank|lombardi, friends of, frank|lombardi, honorable frank", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
rollingwood <- sort(grep("rollingwood", dfx$Address, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Lombardi & dfx$Address %in% rollingwood] <- "Lombardo III, Frank"; rm(Lombardi,rollingwood)
Victoria <- sort(grep("Victoria, anthony|Victoria, tony|victoria, mr ant", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Victoria] <- "Victoria, Anthony J"; rm(Victoria)
Lee <- sort(grep("^Lee, ms brooke|lee, brooke", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Lee] <- "Lee, Brooke"; rm(Lee)
Mesolella <- sort(grep("mesolella, vincent|mesolella jr, vincent|mesolella, mr vincent", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Mesolella] <- "Mesolella, Vincent"; rm(Mesolella)
Carter <- sort(grep("carter, john|carter sr, mr john|carter, mr john", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Carter & dfx$donor_city == "providence"] <- "Carter, John S"; rm(Carter)
gasbarro <- sort(grep("gasbarro, chris|gasbarro, mr chris", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% gasbarro] <- "Gasbarro, Christopher P"; rm(gasbarro)
Farina <- sort(grep("farina, michael|farina, mickey", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
redberry <- sort(grep("redberry|red berry", dfx$Address, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Farina & dfx$Address %in% redberry] <- "Farina, Michael J"; rm(Farina,redberry)
Mauran <- sort(grep("mauran iv, frank|mauran, frank|mauran, mr frank", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Mauran] <- "Mauran, Frank"; rm(Mauran)
Weizenbaum <- sort(grep("miriam, weizenbaum|weizenbaum esq, miriam|weizenbaum esquire, miriam|weizenbaum, miriam|weizenbaum, mriam", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Weizenbaum] <- "Weizenbaum, Miriam"; rm(Weizenbaum)
Cushman <- sort(grep("cushman, robert|cushman, bob", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Cushman] <- "Cushman, Robert"; rm(Cushman)
Stolzman <- sort(grep("stolzman esq, rob|stolzman, mr rob|stolzman, rob|stolzman, flynn rob|stolzman, atty rob", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Stolzman] <- "Stolzman, Robert I"; rm(Stolzman)
Cote <- sort(grep("cote, david", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Cote] <- "Cote, David A"; rm(Cote)
Dibiase <- sort(grep("dibiase esq, mr frank|dibiase jr, frank|dibiase jr, mr frank|dibiase, atty frank|dibiase, frank|dibiase, mr frank", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Dibiase] <- "Dibiase Jr, Frank"; rm(Dibiase)
Hodgson <- sort(grep("hodgson, dawson", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Hodgson] <- "Hodgson, Dawson"; rm(Hodgson)
King <- sort(grep("king, kernan|king, kerran|king, kerry", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% King] <- "King, Kernan"; rm(King)
Quattrocchi <- sort(grep("Quattrocchi, jos|quattrocchi, joe|quattrocchi, mr jos", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Quattrocchi] <- "Quattrocchi, Joseph A"; rm(Quattrocchi)
Jacquard <- sort(grep("jacquard, rob|jacquard esq, mr rob|jacquard esq, rob|jacquard, h rob|jacquard, rep rob", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Jacquard] <- "Jacquard, Robert B"; rm(Jacquard)
Struck <- sort(grep("struck, rob|struck, jr, mr rob|struck, jr, rob|struck jr, rob", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Struck] <- "Struck Jr, Robert"; rm(Struck)
Tyska <- sort(grep("tyska, andrew|tyska, andy", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Tyska] <- "Tyska, Andrew"; rm(Tyska)
Langlois <- sort(grep("langlois, mari|langlois, ms marie", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Langlois] <- "Langlois, Marie"; rm(Langlois)
Pinga <- sort(grep("pinga, michael|pinga, freinds of michael", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Pinga] <- "Pinga, Michael"; rm(Pinga)
Mcculloch <- sort(grep("mcculloch, norm|mcculloch jr, mr norm|mcculloch jr, norm", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Mcculloch] <- "Mcculloch Jr, Norman E"; rm(Mcculloch)
Moffitt <- sort(grep("moffitt, victor|moffitt, mr victor", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Moffitt] <- "Moffitt, Victor"; rm(Moffitt)
Hurley <- sort(grep("hurley, diane|hurley, ms diane", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Hurley] <- "Hurley, Diane S"; rm(Hurley)
Mandell <- sort(grep("mandell, mark$|mandell, mark s|mandell, mr mark|mandell esq, mark|mandell esquire, mark", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Mandell] <- "Mandell, Mark S"; rm(Mandell)
Almeida <- sort(grep("almeida, victoria|almeida esq, victoria|almeida, ms victoria|almeida, voctoria|almeida, vicki", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Almeida] <- "Almeida, Victoria"; rm(Almeida)
Barros <- sort(grep("barros, jean|barros, friends of jean", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Barros] <- "Barros, Jean"; rm(Barros)
matBrow <- sort(grep("brown, mathew|brown, matt|brown, mr matt", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% matBrow & dfx$Address != "4 eldon"] <- "Brown, Matthew A"; rm(matBrow)
cMor <- sort(grep("friends of charles moreau|moreau, charles|moreau, hon charles|moreau, mr charles", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% cMor] <- "Moreau, Charles D"; rm(cMor)
mikeFlynn <- sort(grep("Flynn, mr michael|flynn, michael|flynn, micheal", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% mikeFlynn] <- "Flynn, Michael"; rm(mikeFlynn)
seanSpice <- sort(grep("Spicer, sean", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% seanSpice] <- "Spicer, Sean"; rm(seanSpice)

mckiernan <- sort(grep("mckiernan, dan|mckiernan esq, dan", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% mckiernan] <- "Mckiernan, Daniel"; rm(mckiernan)
marzullo <- sort(grep("marzullo, vincent|marzullo, mr vincent", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% marzullo] <- "Marzullo, Vincent"; rm(marzullo)
ristaino <- sort(grep("ristainosiegel|ristaino-siegel", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% ristaino] <- "Ristaino-Siegel, Tia"; rm(ristaino)
veri <- sort(grep("veri, amy e|amy veri", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% veri] <- "Veri, Amy"; rm(veri)
thompson <- sort(grep("thompson, ray", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% thompson] <- "Thompson, Raymond T"; rm(thompson)
Carson <- sort(grep("carson, lauren", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Carson] <- "Carson, Lauren"; rm(Carson)
teixeira <- sort(grep("teixeira, antonio", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% teixeira] <- "Teixeira, Antonio"; rm(teixeira)
Fecteau <- sort(grep("fecteau, matthew", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Fecteau] <- "Fecteau, Matthew"; rm(Fecteau)
Beck <- sort(grep("beck, andrew", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Beck] <- "Beck, Andrew"; rm(Beck)
Rosenthal <- sort(grep("rosenthal, russ", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Rosenthal] <- "Rosenthal, Russell"; rm(Rosenthal)
Paolino <- sort(grep("paolino, thomas", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Paolino] <- "Paolino, Thomas J"; rm(Paolino)
Dowling <- sort(grep("dowling, diana", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Dowling] <- "Dowling, Diana"; rm(Dowling)
Sandberg <- sort(grep("sandberg, sheryl", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Sandberg] <- "Sandberg, Sheryl"; rm(Sandberg)
Dintersmith <- sort(grep("dintersmith, theodore", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Dintersmith] <- "Dintersmith, Theodore"; rm(Dintersmith)
Holland <- sort(grep("hills holland, anne|holland, anne", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Holland] <- "Holland, Anne"; rm(Holland)
Chace <- sort(grep("chace, elizabeth|chace, elizbabeth|chace, ms elizabeth", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Chace] <- "Chace, Elizabeth"; rm(Chace)
Delvicario <- sort(grep("Delvicario, ant", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Delvicario] <- "Delvicario, Anthony"; rm(Delvicario)
Kingston <- sort(grep("kingston, james|kingston, mr james|kingston, jim$", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Kingston] <- "Kingston, James"; rm(Kingston)
Avedisian <- sort(grep("avedisian for mayor|avedisian, mr scott|avedisian, scott", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Avedisian] <- "Avedisian, Scott"; rm(Avedisian)
Weinberg <- sort(grep("weinberg cpa, mr carl|weinberg, carl|weinberg, mr carl", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Weinberg] <- "Weinberg, Carl"; rm(Weinberg)
Berarducci <- sort(grep("berarducci, annette", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Berarducci] <- "Berarducci, Annette"; rm(Berarducci)
Lancia <- sort(grep("lancia, dr rob|lancia, hon rob|lancia, rev rob|lancia, rob", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Lancia] <- "Lancia, Robert B"; rm(Lancia)
Jones <- sort(grep("jones, mr patrick|jones, patrick|jones esquire, mr patrick|jones esq, patrick", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Jones] <- "Jones, Patrick T"; rm(Jones)
Cupelo <- sort(grep("cupelo \\(old\\), donna|cupelo, donna|cupelo, ms donna", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Cupelo] <- "Cupelo, Donna C"; rm(Cupelo)
Voccola <- sort(grep("voccola esq, joseph|voccola, joe|voccola, joseph", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Voccola] <- "Voccola, Joseph"; rm(Voccola)
Pesce <- sort(grep("pesce, george", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Pesce] <- "Pesce, George A"; rm(Pesce)
Gamba <- sort(grep("gamba, sharon", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Gamba] <- "Gamba, Sharon"; rm(Gamba)
Denehy <- sort(grep("denehy esq, mark|denehy, mark", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Denehy] <- "Denehy, Mark o"; rm(Denehy)
Horan <- sort(grep("horan, atty r kevin|horan, kevin|horan, r kevin|horan, robert kevin", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Horan] <- "Horan, Kevin R"; rm(Horan)
Crisafulli <- sort(grep("crisafulli esq, marc|crisafulli esquire, marc|crisafulli, atty marc|crisafulli, marc|crisafulli, mr marc", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Crisafulli] <- "Crisafulli, Marc"; rm(Crisafulli)
Dimuccio <- sort(grep("dimuccio cpa, rob|dimuccio, mr rob|dimuccio, robert", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Dimuccio] <- "Dimuccio, Robert A"; rm(Dimuccio)
Williamson <- sort(grep("williamson, mr tim|williamson, tim", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Williamson] <- "Williamson, Timothy"; rm(Williamson)
Fletcher <- sort(grep("fletcher, kate", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Fletcher] <- "Fletcher, Kate L"; rm(Fletcher)
Brown <- sort(grep("brown, doug", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Brown] <- "Brown, Douglas"; rm(Brown)
Croke <- sort(grep("croke jr, rich|croke, richard", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Croke] <- "Croke Jr, Richard G"; rm(Croke)
Wallin <- sort(grep("erik wallin|wallin esq, erik|wallin, erik", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Wallin] <- "Wallin, Erik"; rm(Wallin)
Robbins <- sort(grep("robbins, arthur|robbins, mr arthur", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Robbins] <- "Robbins, Arthur S"; rm(Robbins)
Rocha <- sort(grep("rocha, pat|rocha esq, ms patricia|rocha, ms patricia", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Rocha] <- "Rocha, Patricia K"; rm(Rocha)
Caprio <- sort(grep("caprio, anthony|caprio, mr anthony|committee, caprio", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Caprio] <- "Caprio, Anthony"; rm(Caprio)
Lopes <- sort(grep("Lopes", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Lopes] <- "Lopes, Leonard L"; rm(Lopes)
Gilbane <- sort(grep("gilbane jr, bill|gilbane jr, mr will|gilbane jr, will|gilbane, william", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Gilbane] <- "Gilbane Jr, William"; rm(Gilbane)
Procaccianti <- sort(grep("procaccianti, mr james|procaccianti, james", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Procaccianti] <- "Procaccianti, James"; rm(Procaccianti)
Rosciti <- sort(grep("rosciti jr, anthony|rosciti, anthony|rosciti, mr anthony", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Rosciti] <- "Rosciti, Anthony"; rm(Rosciti)
Lee <- sort(grep("lee, eugene|lee, mr eugene", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Lee] <- "Lee, Eugene"; rm(Lee)
Townsend <- sort(grep("townsend iii, charles|townsend, c c|townsend, cc|townsend, charles|townsend, charlie|townsend, mr charles", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
alfred <- grep("^63 al", dfx$Address, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$FullName[dfx$FullName %in% Townsend & dfx$Address %in% alfred] <- "Townsend III, Charles C"; rm(Townsend,alfred)
Mitchell <- sort(grep("mitchell, kevin", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Mitchell] <- "Mitchell, Kevin J"; rm(Mitchell)
Colaluca <- sort(grep("colaluca, anthony", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Colaluca] <- "Colaluca, Anthony J"; rm(Colaluca)
Monti <- sort(grep("monti, henry$|monti, henry s|monti, mr henry|monti esq, henry|monti esquire, henry", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Monti] <- "Monti, Henry S"; rm(Monti)
Gernt <- sort(grep("gernt, wallace|gernt jr, mr wallace|gernt jr, walace|gernt jr, wallace|gernt, jr, wallace", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Gernt] <- "Gernt Jr, Wallace"; rm(Gernt)
Bottella <- sort(grep("bottella, mr randy|bottella, randy", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Bottella] <- "Bottella, Randy"; rm(Bottella)
Dempsey <- sort(grep("dempsey esq, w glenn|dempsey, mr w glenn|dempsey, glen|dempsey, w g|dempsey, william", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Dempsey] <- "Dempsey, W Glenn"; rm(Dempsey)
Fradin <- sort(grep("fradin, charles", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Fradin] <- "Fradin, Charles S"; rm(Fradin)
Bready <- sort(grep("bready, barrett|bready, dr barrett|bready, mr barrett", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Bready] <- "Bready, Barrett"; rm(Bready)
Delatorre <- sort(grep("Delatorre, ralph", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Delatorre] <- "Delatorre, Ralph"; rm(Delatorre)
Migliori <- sort(grep("migliori dr, michael|migliori, michael|migliori dr, michael", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Migliori] <- "Migliori, Michael"; rm(Migliori)
sidMig <- sort(grep("migliori, sidney|migliori, dr sidney", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% sidMig] <- "Migliori, Sidney"; rm(sidMig)
Yatskar <- sort(grep("Yatskar, igor", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Yatskar] <- "Yatskar, Igor"; rm(Yatskar)
Buonanno <- sort(grep("buonanno jr, a robert|buonanno, a|buonanno, a robert|buonanno, dr a robert|buonanno, robert", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Buonanno] <- "Buonanno, A Robert"; rm(Buonanno)
Frishman <- sort(grep("frishman dr, gary|frishman, dr gary|frishman, gary", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Frishman] <- "Frishman, Gary N"; rm(Frishman)
Hollmann <- sort(grep("hollmann, peter", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Hollmann] <- "Hollmann, Peter A"; rm(Hollmann)
Guyon <- sort(grep("guyon jr, robert|guyon, robert e", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Guyon] <- "Guyon Jr, Robert"; rm(Guyon)
Lawson <- sort(grep("lawson, bart", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Lawson] <- "Lawson, Bartholomew j"; rm(Lawson)
Montella <- sort(grep("montella, mark", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Montella] <- "Montella, Mark"; rm(Montella)
Mccoy <- sort(grep("mccoy, charles", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Mccoy] <- "Mccoy, Charles"; rm(Mccoy)
snady <- sort(grep("mccoy, lory|snady-mccoy, lori|snady mccoy, dr lory", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% snady] <- "Snady Mccoy, Lory"; rm(snady)
Karczmar <- sort(grep("karczmar, peter|karczmar, dr peter", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Karczmar] <- "Karczmar, Peter"; rm(Karczmar)
Coletta <- sort(grep("coletta, sandra", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Coletta] <- "Coletta, Sandra"; rm(Coletta)
Hittner <- sort(grep("hittner, dr kat|hittner, kathleen$|hittner, kathleen c|hittner, ms kat", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Hittner] <- "Hittner, Kathleen C"; rm(Hittner)
Babineau <- sort(grep("babineau, tim", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Babineau] <- "Babineau, Timothy J"; rm(Babineau)
Pprzygoda <- sort(grep("przygoda, dr john|przygoda, john", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Pprzygoda] <- "Pprzygoda, John J"; rm(Pprzygoda)
Burke <- sort(grep("burke, virgin", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Burke] <- "Burke, Virginia M"; rm(Burke)
Schulman <- sort(grep("schulman, howard|schulman, mr howard", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Schulman] <- "Schulman, Howard"; rm(Schulman)
Barone <- sort(grep("barone, jeff|barone, mr jeff", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Barone] <- "Barone, Jeffrey P"; rm(Barone)
Charest <- sort(grep("charest, richard", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Charest] <- "Charest, Richard"; rm(Charest)
Sangermano <- sort(grep("sangermano 3rd, peter|sangermano iii, peter|sangermano, peter|sangermano, mr peter", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Sangermano] <- "Sangermano, Peter J"; rm(Sangermano)
Sikov <- sort(grep("sikov, william", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Sikov] <- "Sikov, William"; rm(Sikov)
Quinlan <- sort(grep("quinlan, edward|quinlan, mr edward", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Quinlan] <- "Quinlan, Edward J"; rm(Quinlan)
Barbery <- sort(grep("barbery jr, alfred|barbery jr, mr alfred|barbery, alfred|barbery, mr alfred", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Barbery] <- "Barbery Jr, Alfred"; rm(Barbery)
Dyer <- sort(grep("dyer, robert", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Dyer] <- "Dyer, Robert K"; rm(Dyer)
Lousararian <- sort(grep("lousararian, james", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Lousararian] <- "Lousararian, James"; rm(Lousararian)
Weiss <- sort(grep("weiss, arnold p", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Weiss] <- "Weiss, Arnold Peter"; rm(Weiss)
Vecchione <- sort(grep("vecchione, george|vecchione, mr george", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Vecchione] <- "Vecchione, George A"; rm(Vecchione)
Cronan <- sort(grep("cronan, dr john|cronan, john", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Cronan] <- "Cronan, John"; rm(Cronan)
Coppe <- sort(grep("coppe, david|coppe, dr david", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Coppe] <- "Coppe, David"; rm(Coppe)
Pichardo <- sort(grep("pichardo, juan", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Pichardo] <- "Pichardo, Juan M"; rm(Pichardo)
Powrie <- sort(grep("powrie, dr raymond|powrie, raymond", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Powrie] <- "Powrie, Raymond O"; rm(Powrie)
Charest <- sort(grep("charest, richard", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Charest] <- "Charest, Richard R"; rm(Charest)
Carter <- sort(grep("carter, letitia|carter, ms letitia", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Carter] <- "Carter, Letitia M"; rm(Carter)
Pattullo <- sort(grep("pattullo, elizabeth", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Pattullo] <- "Pattullo, Elizabeth A"; rm(Pattullo)
Rice <- sort(grep("rice, jana", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Rice] <- "Rice, Jana"; rm(Rice)
leclerc <- sort(grep("leclerc, richard", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% leclerc] <- "Leclerc, Richard"; rm(leclerc)
dzwierzynski <- sort(grep("dzwierzynski, ewa", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% dzwierzynski] <- "Dzwierzynski, Ewa"; rm(dzwierzynski)
minassian <- sort(grep("minassian, gary", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% minassian] <- "Minassian, Gary"; rm(minassian)
carroll <- sort(grep("carroll, tom|carroll, thomas", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% carroll] <- "Carroll, Thomas M"; rm(carroll)
palumbo <- sort(grep("palumbo, marc|palumbo, mark", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% palumbo] <- "Palumbo, Mark A"; rm(palumbo)
bayliss <- sort(grep("bayliss, george|bayliss, dr george", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% bayliss] <- "Bayliss, George"; rm(bayliss)
dalessandro <- sort(grep("d'alessandro, frank|dalessandro, dr frank|dalessandro, frand|dalessandro, frank", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% dalessandro] <- "D'alessandro, Frank"; rm(dalessandro)
dann <- sort(grep("dann, dr robert|dann, robert", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% dann] <- "Dann, Robert"; rm(dann)
geuss <- sort(grep("geuss jr, lawrence|geuss, lawrence", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% geuss] <- "Geuss, Lawrence"; rm(geuss)
Santilli <- sort(grep("santilli, lawrence", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Santilli] <- "Santilli, Lawrence G"; rm(Santilli)
mathews <- sort(grep("mathews, cara", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% mathews] <- "Mathews, Cara"; rm(mathews)
mcmillen <- sort(grep("mcmillen dds, fred|mcmillen dmd, fred|mcmillen, fred", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% mcmillen] <- "Mcmillen, Frederick H"; rm(mcmillen)
brown <- sort(grep("brown, joanna", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% brown] <- "Brown, Joanna"; rm(brown)
bannister <- sort(grep("bannister, dr holly|bannister, holly", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% bannister] <- "Bannister, Holly"; rm(bannister)
gibbs <- sort(grep("gibbs, frantz", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% gibbs] <- "Gibbs, Frantz"; rm(gibbs)
evangelista <- sort(grep("evangelista, dr peter|evangelista, pete", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% evangelista] <- "Evangelista, Peter"; rm(evangelista)
lasalle <- sort(grep("lasalle, susan", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% lasalle] <- "Lasalle, Susan A"; rm(lasalle)
iannuccilli <- sort(grep("iannuccilli, dr jason|iannuccilli, jason", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% iannuccilli] <- "Iannuccilli, Jason"; rm(iannuccilli)
helman <- sort(grep("helman, sam", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% helman] <- "Helman, Sam"; rm(helman)
marcantano <- sort(grep("marcantano, mark", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% marcantano] <- "Marcantano, Mark R"; rm(marcantano)
catallozzi <- sort(grep("catallozzi jr, rich|catallozzi, rich", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% catallozzi] <- "Catallozzi, Richard A"; rm(catallozzi)
oliver <- sort(grep("oliver, nicholas", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% oliver] <- "Oliver, Nicholas"; rm(oliver)
cervone <- sort(grep("cervone, lori", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% cervone] <- "Cervone, Lori A"; rm(cervone)
gauvin <- sort(grep("gauvin, rodney", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% gauvin] <- "Gauvin, Rodney J"; rm(gauvin)
purcell <- sort(grep("purcell jr, thomas|purcell jr, t e|purcell, thomas", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% purcell] <- "Purcell Jr, Thomas"; rm(purcell)
wakefield <- sort(grep("wakefield, mary", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% wakefield] <- "Wakefield, Mary A"; rm(wakefield)
mainiero <- sort(grep("mainiero, dr martha|mainiero, martha", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% mainiero] <- "Mainiero, Martha"; rm(mainiero)
weigner <- sort(grep("weigner, marilyn", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% weigner] <- "Weigner, Marilyn"; rm(weigner)
akelman <- sort(grep("akelman, dr ed|akelman, ed|akelman, mr ed", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% akelman] <- "Akelman, Edward R"; rm(akelman)
magaziner <- sort(grep("magaziner, jenn", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% magaziner] <- "Magaziner, Jennifer"; rm(magaziner)
mencoff <- sort(grep("mencoff, george", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% mencoff] <- "Mencoff, George"; rm(mencoff)
petrovas <- sort(grep("petrovas, george", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% petrovas] <- "Petrovas, George"; rm(petrovas)
pezzullo <- sort(grep("pezzullo iii, dr john|pezzullo iii, john|pezzullo, dr john|pezzullo, john|ezzullo, mr john", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% pezzullo] <- "Pezzullo, John"; rm(pezzullo)
sheehan <- sort(grep("sheehan, lynn", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% sheehan] <- "Sheehan, Lynn"; rm(sheehan)
haas <- sort(grep("haas, richard", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% haas] <- "Haas, Richard"; rm(haas)
yoo <- sort(grep("yoo, don|yoo, dr don", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% yoo] <- "Yoo, Don C"; rm(yoo)
plotz <- sort(grep("plotz, richard|plotz, dr richard", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% plotz] <- "Plotz, Richard"; rm(plotz)
Corvese <- sort(grep("corvese, john", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Corvese] <- "Corvese, John"; rm(Corvese)
beckwith <- sort(grep("beckwith, curt", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% beckwith] <- "Beckwith, Curt G"; rm(beckwith)
beland <- sort(grep("beland, michael|beland, dr michael", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% beland] <- "Beland, Michael"; rm(beland)
piccolello <- sort(grep("piccolello, dr marcelle|piccolello, marcelle|piccolelo, marcelle|picolello, dr marcelle", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% piccolello] <- "Piccolello, Marcelle"; rm(piccolello)
iannoni <- sort(grep("iannoni, f joseph", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% iannoni] <- "Iannoni, Joseph"; rm(iannoni)
healey <- sort(grep("healey, terrance|healey, dr terrance", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% healey] <- "Healey, Terrance"; rm(healey)
boxerman <- sort(grep("boxerman, dr jerrold|boxerman, jerrold", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% boxerman] <- "Boxerman, Jerrold"; rm(boxerman)
dasilva <- sort(grep("dasilva, manuel", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% dasilva] <- "Dasilva, Manuel"; rm(dasilva)
donegan <- sort(grep("donegan, dr linda|donegan, linda|linda, donegan", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% donegan] <- "Donegan, Linda"; rm(donegan)
khalil <- sort(grep("khalil, hanan", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% khalil] <- "Khalil, Hanan"; rm(khalil)
rogg <- sort(grep("rogg, dr jeff|rogg, jeff", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% rogg] <- "Rogg, Jeffrey"; rm(rogg)
hovan <- sort(grep("hovan, keith", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% hovan] <- "Hovan, Keith A"; rm(hovan)
lourenco <- sort(grep("lourenco, ana", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% lourenco] <- "Lourenco, Ana P"; rm(lourenco)
tracy <- sort(grep("tracy, molly|tracy, dr molly", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% tracy] <- "Tracy, Molly"; rm(tracy)
noel <- sort(grep("noel, arthur", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% noel] <- "Noel, Arthur"; rm(noel)
barile <- sort(grep("barile, a$|barile, anthony", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% barile] <- "Barile, Anthony"; rm(barile)
fanale <- sort(grep("fanale, james|fanale, mr james", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% fanale] <- "Fanale, James"; rm(fanale)
pensa <- sort(grep("pensa, frank", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% pensa] <- "Pensa, Frank"; rm(pensa)
ridlen <- sort(grep("ridlen, mark", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% ridlen] <- "Ridlen, Mark"; rm(ridlen)
brody <- sort(grep("brody, jeff|brody, mr jeff|brody, dr jeff", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% brody] <- "Brody, Jeffrey"; rm(brody)
pepitone <- sort(grep("pepitone, jessica", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% pepitone] <- "Pepitone, Jessica"; rm(pepitone)
gilstein <- sort(grep("gilstein, suzanne", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% gilstein] <- "Gilstein, Suzanne"; rm(gilstein)
jones <- sort(grep("jones, elaine", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% jones] <- "Jones, Elaine"; rm(jones)
pMoran <- sort(grep("moran, paul|moran, mr paul", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% pMoran] <- "Moran, Paul P"; rm(pMoran)
Morant <- sort(grep("moran, terren|moran, terran|moran, terry|moran, mr terr", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Morant] <- "Moran, Terennce, P"; rm(Morant)
Burgos <- sort(grep("Burgos, tonio", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Burgos] <- "Burgos, Tonio"; rm(Burgos)
Geoffroy <- sort(grep("geoffroy, colin", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Geoffroy] <- "Geoffroy, Colin"; rm(Geoffroy)
Izzo <- sort(grep("izzo, michelle", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Izzo] <- "Izzo, Michelle"; rm(Izzo)
Mckelvy <- sort(grep("mckelvy, michael", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Mckelvy] <- "Mckelvy, Michael E"; rm(Mckelvy)
Darrow <- sort(grep("darrow esq, mr zach|darrow, atty zach|darrow, zach|everett llp, darrow|darrow, mr zachary", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Darrow] <- "Darrow, Zachary G"; rm(Darrow)
Clark <- sort(grep("clark, robert g|clark, robert$|clarke, robert g", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Clark] <- "Clark, Robert G"; rm(Clark)
Marshall <- sort(grep("marshall, lianne", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Marshall] <- "Marshall, Lianne"; rm(Marshall)
Petrosinelli <- sort(grep("petrosinelli, guido|petrosinelli, mr guido", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Petrosinelli] <- "Petrosinelli, Guido"; rm(Petrosinelli)
Kilpatrick <- sort(grep("kilpatrick esquire, j scott|kilpatrick, scott|kilpatrick, j scott|kilpatrick, mr j scott", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Kilpatrick] <- "Kilpatrick, J Scott"; rm(Kilpatrick)
Ryan <- sort(grep("ryan, mr daniel|ryan, daniel|ryan, dan$", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Ryan] <- "Ryan, Daniel"; rm(Ryan)
Totten <- sort(grep("totten, a bart|totten, bart|totten, r bart", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Totten] <- "Totten, R Bart"; rm(Totten)
Marcello <- sort(grep("marcello, michael|marcello, mr michael|marcello, friends of michael", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Marcello] <- "Marcello, Michael J"; rm(Marcello)
Caruolo <- sort(grep("caruolo, atty george|caruolo, george|caruolo, hon george", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Caruolo] <- "Caruolo, George D"; rm(Caruolo)
Simon <- sort(grep("simon, mr peter|simon, peter", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Simon] <- "Simon, Peter"; rm(Simon)
Moses <- sort(grep("moses, thomas|moses esq, mr thomas|moses esq, thomas|moses, mr thomas", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Moses] <- "Moses, Thomas V"; rm(Moses)
Nardone <- sort(grep("nardone, george", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Nardone] <- "Nardone, George"; rm(Nardone)
Fain <- sort(grep("fain, jonathan", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Fain] <- "Fain, Jonathan D"; rm(Fain)
Acquavella <- sort(grep("acquavella, william", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Acquavella] <- "Acquavella, William"; rm(Acquavella)
Afonso <- sort(grep("afonso, paul", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Afonso] <- "Afonso, Paul"; rm(Afonso)
Barnacle <- sort(grep("barnacle, ian|barnacle, mr ian", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Barnacle] <- "Barnacle, Ian"; rm(Barnacle)
Belair <- sort(grep("belair, jerry", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Belair] <- "Belair, Jerry"; rm(Belair)
Barletta <- sort(grep("barletta, timothy", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Barletta] <- "Barletta, Timothy"; rm(Barletta)
Corrente <- sort(grep("corrente, judith-ann", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Corrente] <- "Corrente, Judith-Ann"; rm(Corrente)
Kooyker <- sort(grep("kooyker, willem", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Kooyker] <- "Kooyker, Willem"; rm(Kooyker)
Diprete <- sort(grep("diprete, dennis", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Diprete] <- "Diprete, Dennis L"; rm(Diprete)
Simon <- sort(grep("simon, anthony|simon, tony", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Simon] <- "Simon, Anthony"; rm(Simon)
Stasiunas <- sort(grep("stasiunas, mr tim|stasiunas, timothy", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Stasiunas] <- "Stasiunas, Timothy"; rm(Stasiunas)
White <- sort(grep("white, raymond", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% White] <- "White, Raymond S"; rm(White)
Donahue <- sort(grep("donahue, shawn|donahue, mr shawn|donahue, sawn", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Donahue] <- "Donahue, Shawn"; rm(Donahue)
Coia <- sort(grep("coia, arthur", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Coia] <- "Coia, Arthur"; rm(Coia)
Gummo <- sort(grep("gummo, steve", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Gummo] <- "Gummo, Steven E"; rm(Gummo)
Hayes <- sort(grep("hayes, david|hayes, mr david|hayes jr, david w", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Hayes] <- "Hayes, David W"; rm(Hayes)
Mccaffrey <- sort(grep("mccaffrey, sen michael|mccaffrey esq, michael|mccaffrey esq, sen michael|mccaffrey, michael|mccaffrey, senator michael|mccaffrey, mr michael", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Mccaffrey] <- "Mccaffrey, Michael J"; rm(Mccaffrey)
Petrarca <- sort(grep("petrarca-karampetos, jina|petrarca-karampetros, jina|petrarca-karampetsos, atty jina|petrarca karampetos, jina|Petrarca-Karampetsos, Jina|petrarca, jina|petrarca, atty jina", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Petrarca] <- "Petrarca-Karampetsos, Jina"; rm(Petrarca)
Ucci <- sort(grep("^ucci, stephen|^ucci, mr stephen|^ucci, steve|^ucci, honorable stephen|^ucci esquire, stephen|ucci, friends of stephen|^ucci esq, hon stephen", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Ucci] <- "Ucci, Stephen"; rm(Ucci)
Quezada <- sort(grep("quezada, ana", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Quezada] <- "Quezada, Ana B"; rm(Quezada)
Palangio <- sort(grep("palangio, tom|palangio, thomas", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Palangio] <- "Palangio, Thomas A"; rm(Palangio)
Martineau <- sort(grep("martineau, michael|martineau, j michael|martineau, j$|martineau, j michel", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Martineau] <- "Martineau, J Michel"; rm(Martineau)
Waidler <- sort(grep("waidler, bruce", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Waidler] <- "Waidler, Bruce"; rm(Waidler)
Coloian <- sort(grep("coloian, artin|coloian esq, artin|coloian, arthur", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Coloian] <- "Coloian, Artin"; rm(Coloian)
Mccarthy <- sort(grep("mccarthy, patrick", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Mccarthy] <- "Mccarthy, Patrick M"; rm(Mccarthy)
Christie <- sort(grep("christie, chris", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Christie] <- "Christie, Chris"; rm(Christie)
Civetti <- sort(grep("civetti, robert", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Civetti] <- "Civetti, Robert"; rm(Civetti)
Depasquale <- sort(grep("depasquale, mark", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Depasquale] <- "Depasquale, Mark"; rm(Depasquale)
Dilorenzo <- sort(grep("dilorenzo, steven|dilorenzo sr, mr & mrs steven|dilorenzo sr, steven|steven dilorenzo", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Dilorenzo] <- "Dilorenzo, Steven"; rm(Dilorenzo)

# Hyrzan <- sort(grep("Hyrzan", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
# dfx$FullName[dfx$FullName %in% Hyrzan] <- "Hyrzan, Stephanie"; rm(Hyrzan)
# Brown <- sort(grep("Brown, matt", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
# dfx$FullName[dfx$FullName %in% Brown] <- "Brown, Matt"; rm(Brown)
Derobbio <- sort(grep("Derobbio, robert|Derobbio, dr robert", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Derobbio] <- "Derobbio, Robert A"; rm(Derobbio)
Keith <- sort(grep("Keith, jon", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Keith] <- "Keith, Jonathan J"; rm(Keith)
sSchusterman <- sort(grep("Schusterman, stacy", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% sSchusterman] <- "Schusterman, Stacy"; rm(sSchusterman)
Bisignano <- sort(grep("Bisignano, Frank", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Bisignano] <- "Bisignano, Frank"; rm(Bisignano)
Boies <- sort(grep("Boies, David", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Boies] <- "Boies, David"; rm(Boies)
Conway <- sort(grep("Conway, ron", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Conway] <- "Conway, Ronald"; rm(Conway)
Dow <- sort(grep("Dow, steve", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Dow] <- "Dow, Steven"; rm(Dow)
Dupre <- sort(grep("Dupre, Denise", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Dupre] <- "Dupre, Denise"; rm(Dupre)
Hauptman <- sort(grep("Hauptman, andrew", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Hauptman] <- "Hauptman, Andrew"; rm(Hauptman)
Hoffman <- sort(grep("Hoffman, reid", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Hoffman] <- "Hoffman, Reid"; rm(Hoffman)
Mccarthy <- sort(grep("Mccarthy, ian", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Mccarthy] <- "Mccarthy, Ian"; rm(Mccarthy)
Menna <- sort(grep("Menna, gilbert", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Menna] <- "Menna, Gilbert"; rm(Menna)
Mixer <- sort(grep("Mixer, gail|mixer, ms gail", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Mixer] <- "Mixer, Gail"; rm(Mixer)
Rubin <- sort(grep("Rubin, judith", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Rubin] <- "Rubin, Judith"; rm(Rubin)
Schusterman <- sort(grep("Schusterman, Lynn", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Schusterman] <- "Schusterman, Lynn"; rm(Schusterman)
Sipprelle <- sort(grep("Sipprelle, Susan", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Sipprelle] <- "Sipprelle, Susan"; rm(Sipprelle)
Yee <- sort(grep("Yee, michelle", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Yee] <- "Yee, Michelle"; rm(Yee)
Zients <- sort(grep("Zients, Jeff", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Zients] <- "Zients, Jeff"; rm(Zients)
aAbrams <- sort(grep("Abrams, amy", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% aAbrams] <- "Abrams, Amy"; rm(aAbrams)
Abrams <- sort(grep("Abrams, david", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Abrams] <- "Abrams, David"; rm(Abrams)
Bloomberg <- sort(grep("Bloomberg, Michael", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Bloomberg] <- "Bloomberg, Michael"; rm(Bloomberg)
jCanning <- sort(grep("Canning, john", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% jCanning] <- "Canning, John"; rm(jCanning)
Canning <- sort(grep("Canning, rita", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Canning] <- "Canning, Rita"; rm(Canning)
Hale <- sort(grep("Hale, rob", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Hale] <- "Hale, Robert"; rm(Hale)
Hazard <- sort(grep("Hazard, eliz", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Hazard] <- "Hazard, Elizabeth"; rm(Hazard)
Klarman <- sort(grep("Klarman, seth", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Klarman] <- "Klarman, Seth"; rm(Klarman)
aMencoff <- sort(grep("Mencoff, ann", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% aMencoff] <- "Mencoff, Ann"; rm(aMencoff)
Mencoff <- sort(grep("Mencoff, samuel$|mencoff, samuel m", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Mencoff] <- "Mencoff, Samuel"; rm(Mencoff)
Mixer <- sort(grep("Mixer, dav", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Mixer] <- "Mixer, David"; rm(Mixer)
Nicoll <- sort(grep("Nicoll, edward", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Nicoll] <- "Nicoll, Edward"; rm(Nicoll)
Peterson <- sort(grep("Peterson, michael", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Peterson] <- "Peterson, Michael"; rm(Peterson)
Rosenstein <- sort(grep("Rosenstein, liz", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Rosenstein] <- "Rosenstein, Lizanne"; rm(Rosenstein)
Royce <- sort(grep("Royce, deb", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Royce] <- "Royce, Deborah"; rm(Royce)
Rubin <- sort(grep("Rubin, rob", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Rubin] <- "Rubin, Robert"; rm(Rubin)
Sipprelle <- sort(grep("Sipprelle, dwi|sipprelle, swi", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Sipprelle] <- "Sipprelle, Dwight"; rm(Sipprelle)
Sonnenfeldt <- sort(grep("Sonnenfeldt, michael", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Sonnenfeldt] <- "Sonnenfeldt, Michael"; rm(Sonnenfeldt)
jWhite <- sort(grep("White, jenn", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% jWhite] <- "White, Jennifer"; rm(jWhite)
White <- sort(grep("White, sean", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% White] <- "White, Sean"; rm(White)
Fish <- sort(grep("Fish, john", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Fish] <- "Fish, John"; rm(Fish)
Holt <- sort(grep("Holt, ryan", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Holt] <- "Holt, Ryan, J"; rm(Holt)
Mutter <- sort(grep("Mutter, jeff", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Mutter] <- "Mutter, Jeffrey J"; rm(Mutter)
Slaughter <- sort(grep("Slaughter, james|slaughter, r james|slaughter, jamie", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Slaughter] <- "Slaughter, James"; rm(Slaughter)
Stryker <- sort(grep("Stryker, jon", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Stryker] <- "Stryker, Jon"; rm(Stryker)
Tierney <- sort(grep("Tierney, dan", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Tierney] <- "Tierney, Daniel"; rm(Tierney)
Feroce <- sort(grep("Feroce, gio|feroce, mr gio", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Feroce] <- "Feroce, Giovanni"; rm(Feroce)
Beggs <- sort(grep("Beggs, kev", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Beggs] <- "Beggs, Kevin"; rm(Beggs)
Kingston <- sort(grep("Kingston, jess", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Kingston] <- "Kingston, Jessie"; rm(Kingston)
Mendez <- sort(grep("Mendez, mario", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Mendez] <- "Mendez, Mario"; rm(Mendez)
Mckenney <- sort(grep("Mckenney esq, mark|mckenney, mak|mckenney, mark|mckenney, mr mark", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Mckenney] <- "Mckenney, Mark"; rm(Mckenney)
Joyaux <- sort(grep("Joyaux, simone", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Joyaux] <- "Joyaux, Simone"; rm(Joyaux)
Bucci <- sort(grep("Bucci, ruth", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Bucci] <- "Bucci, Ruth"; rm(Bucci)
Feinstein <- sort(grep("Feinstein, mark", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Feinstein] <- "Feinstein, Mark"; rm(Feinstein)
Deveraux <- sort(grep("Deveraux, will", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Deveraux] <- "Deveraux, William P"; rm(Deveraux)
Liguori <- sort(grep("Liguori, ralph|liguori esq, ralph", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Liguori] <- "Liguori, Ralph R"; rm(Liguori)
Pannone <- sort(grep("Pannone esq, gary|pannone, gary|pannone, mr gary", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Pannone] <- "Pannone, Gary R"; rm(Pannone)
Doyle <- sort(grep("Doyle, patricia|doyle, pataricia|doyle, pat$", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Doyle] <- "Doyle, Patricia"; rm(Doyle)
Gara <- sort(grep("o'gara jr, mr william|ogara jr, william|ogara, william|ogara jr, william|o'gara jr, william|o'gara, atty william|o'gara, mr william|o'gara, williiam", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Gara] <- "O'Gara, William"; rm(Gara)
Wolf <- sort(grep("Wolf, gayle|wolf, gail|wolfe, gayle|wolfe, gail|wolf, ms gayle|wolf, gale|wolff, gayle|wolfe, gail|gayle, wolf", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Wolf & dfx$donor_st == "ri"] <- "Wolf, Gayle"; rm(Wolf)
Carlin <- sort(grep("Carlin, glen", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Carlin] <- "Carlin, Glenn"; rm(Carlin)
Hooper <- sort(grep("Hooper, john", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Hooper] <- "Hooper, John"; rm(Hooper)
Nelson <- sort(grep("Nelson, jane s|nelson, jane$", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Nelson] <- "Nelson, Jane S"; rm(Nelson)
Noret <- sort(grep("Noret, Thom", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Noret] <- "Noret, Thomas E"; rm(Noret)
Depasquale <- sort(grep("Depasquale, june", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Depasquale] <- "Depasquale, June C"; rm(Depasquale)
Depasquale <- sort(grep("Depasquale, mr steph|depasquale, steph|depasquale, mr steve|depasquale, stepven|depasquale, steve", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Depasquale] <- "Depasquale, Stephen"; rm(Depasquale)
Griffin <- sort(grep("Griffin jr, james|griffin, james|griffin, jr, james|griffing, james", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Griffin] <- "Griffin Jr, James E"; rm(Griffin)
Preston <- sort(grep("Preston, h leb", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Preston] <- "Preston, H Lebaron"; rm(Preston)
Padwa <- sort(grep("Padwa, jeff|padwa esq, jeff|padwa, mr jeff", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Padwa] <- "Padwa, Jeffrey"; rm(Padwa)
Kane <- sort(grep("Kane, colin|kane, mr colin", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Kane] <- "Kane, Colin P"; rm(Kane)
Milas <- sort(grep("Milas, joan", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Milas] <- "Milas, Joan P"; rm(Milas)
Disanto <- sort(grep("Disanto ii, gerald|disanto ii, gerard|disanto, gerard|disanto, gerald", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Disanto] <- "Disanto II, Gerard C"; rm(Disanto)
Meador <- sort(grep("Meador, ray", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Meador] <- "Meador, Raymond"; rm(Meador)
Sweeney <- sort(grep("Sweeney esq, marg|sweeney, marg|sweeney, mrs marg", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Sweeney] <- "Sweeney, Margaret H"; rm(Sweeney)
Caldwell <- sort(grep("Caldwell, dav|caldwell jr, dav", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Caldwell] <- "Caldwell, David A"; rm(Caldwell)
Mcgraw <- sort(grep("Mcgraw jr, gerald|mcgraw jr, gerry|mcgraw, gerald|mcgraw, gerry|mcgraw, judy and gerald", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Mcgraw] <- "Mcgraw Jr, Gerald J"; rm(Mcgraw)
Anthony <- sort(grep("Anthony, Helen", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Anthony] <- "Anthony, Helen"; rm(Anthony)
Marshall <- sort(grep("Marshall, david", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Marshall] <- "Marshall, David D"; rm(Marshall)
Hernandez <- sort(grep("Hernandez, jon", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Hernandez] <- "Hernandez, Jonathan A"; rm(Hernandez)
Frohman <- sort(grep("anderson, frohman|anderson, mr frohman|anderson, fronhowan", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Frohman] <- "Anderson, Frohman"; rm(Frohman)
Baginski <- sort(grep("Baginski, joseph", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Baginski] <- "Baginski, Joseph"; rm(Baginski)
Berick <- sort(grep("Berick, josh|berick, mr josh", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Berick] <- "Berick, Josh J"; rm(Berick)
Cielinski <- sort(grep("Cielinski, james|cielinski, jim", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Cielinski] <- "Cielinski, James"; rm(Cielinski)
Colapietro <- sort(grep("colapietro-corsetti, donna", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Colapietro] <- "Colapietro-Corsetti, Donna"; rm(Colapietro)
Collins <- sort(grep("collins, patrich g|collins, patrick$|collins, patrick g|collins, patrick c", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Collins] <- "Collins, Patrick G"; rm(Collins)
Ford <- sort(grep("Ford, ellen|ford, ms e", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Ford] <- "Ford, Ellen"; rm(Ford)
Halperin <- sort(grep("Halperin esquire, preston|halperin, mr preston|halperin, preston", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Halperin] <- "Halperin, Preston W"; rm(Halperin)
Kahane <- sort(grep("Kahane, eliz", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Kahane] <- "Kahane, Elizabeth"; rm(Kahane)
King <- sort(grep("King jr, jack|king, jack f", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% King] <- "King Jr, Jack F"; rm(King)
Levinson <- sort(grep("Levinson, dan", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Levinson] <- "Levinson, Daniel"; rm(Levinson)
Manaigo <- sort(grep("Manaigo, fed|Manaigo, fred", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Manaigo] <- "Manaigo, Federico"; rm(Manaigo)
Mancini <- sort(grep("mancini-morrocco, deb", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Mancini] <- "Mancini-Morrocco, Deborah"; rm(Mancini)
Moran <- sort(grep("Moran, tim|moran, mr tim", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Moran] <- "Moran, Timothy J"; rm(Moran)
Parmenter <- sort(grep("Parmenter, jack|parmenter, mr jack", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Parmenter] <- "Parmenter, Jackson C"; rm(Parmenter)
Rego <- sort(grep("Rego, carlos", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Rego] <- "Rego, Carlos"; rm(Rego)
Sloane <- sort(grep("Sloane, barry", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Sloane] <- "Sloane, Barry"; rm(Sloane)
Vargas <- sort(grep("Vargas, oscar", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Vargas] <- "Vargas, Oscar"; rm(Vargas)
Calvino <- sort(grep("Calvino esq, john|calvino ii, john|calvino sr, john|calvino, john", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Calvino] <- "Calvino, John N"; rm(Calvino)
Jordan <- sort(grep("Jordan, brian|jordan, mr b", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Jordan] <- "Jordan, Brian K"; rm(Jordan)
Cancel <- sort(grep("Cancel, george", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Cancel] <- "Cancel, George D"; rm(Cancel)
Teverow <- sort(grep("Teverow esq, josh|teverow, josh|teverow, mr josh", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Teverow] <- "Teverow, Joshua"; rm(Teverow)
Bernardo <- sort(grep("Bernardo pe, richard|bernardo, richard", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Bernardo] <- "Bernardo, Richard A"; rm(Bernardo)
Gilden <- sort(grep("Gilden, david|gilden esq, david|gilden esq, mr david", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Gilden] <- "Gilden, David M"; rm(Gilden)
Pannone <- sort(grep("Pannone, joyce", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Pannone] <- "Pannone, Joyce"; rm(Pannone)
Yip <- sort(grep("Yip, lou|yip, mr lou", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Yip] <- "Yip, Louis"; rm(Yip)
Benoit <- sort(grep("Benoit, norm|benoit, g normand", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Benoit] <- "Benoit, Normand"; rm(Benoit)
Farrelly <- sort(grep("farrelly jr, thomas|farrelly, thomas", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Farrelly] <- "Farrelly Jr, Thomas F"; rm(Farrelly)
Farrelly <- sort(grep("farrelly, keith|farrelly, mr keith", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Farrelly] <- "Farrelly, Keith J"; rm(Farrelly)
Raiola <- sort(grep("Raiola, lisa|raiola, ms lisa|raiola, 8462170 lisa", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Raiola] <- "Raiola, Lisa"; rm(Raiola)
Taub <- sort(grep("Taub, h russell|taub, russ|taub, mr russ", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Taub] <- "Taub, Russell"; rm(Taub)
Heller <- sort(grep("Heller, brian", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Heller] <- "Heller, Brian"; rm(Heller)
Fiore <- sort(grep("Fiore, roland|fiore, ronald|fiore, roalnd|fiore jr, roland", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Fiore] <- "Fiore, Roland"; rm(Fiore)
Cianci <- sort(grep("Cianci, mr steve|cianci, steve", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Cianci] <- "Cianci, Steven"; rm(Cianci)
Sculos <- sort(grep("Sculos, craig", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Sculos] <- "Sculos, Craig C"; rm(Sculos)
Bucci <- sort(grep("Bucci, brian", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Bucci] <- "Bucci, Brian"; rm(Bucci)
Raso <- sort(grep("Raso, perry", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Raso] <- "Raso, Perry"; rm(Raso)
Chace <- sort(grep("Chace, malcolm|chace, malcom|chace, mr malco|chace iv, malco|chace jr, malco|chace jr, mr malco", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Chace] <- "Chace, Malcolm"; rm(Chace)
Chertavian <- sort(grep("chertavian, gera|chertavian, mr gerald", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Chertavian] <- "Chertavian, Gerald"; rm(Chertavian)
Disanto <- sort(grep("Disanto cpa, david|disanto, david", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Disanto] <- "Disanto, David P"; rm(Disanto)
Hahn <- sort(grep("Hahn esq, james|hahn, james", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Hahn] <- "Hahn, James"; rm(Hahn)
Holmes <- sort(grep("Holmes, wendy|holmes, ms wendy", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Holmes] <- "Holmes, Wendy"; rm(Holmes)
Moore <- sort(grep("Moore, joseph|moore, mr joseph", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Moore] <- "Moore, Joseph"; rm(Moore)
Smith <- sort(grep("Smith, sandra", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Smith] <- "Smith, Sandra"; rm(Smith)
Sullivan <- sort(grep("Sullivan, paul$|sullivan, paul v", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
paulsAddress <- grep("hemlock|weybosset", dfx$Address, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
dfx$FullName[dfx$FullName %in% Sullivan & dfx$Address %in% paulsAddress] <- "Sullivan, Paul V"; rm(Sullivan,paulsAddress)
Dupont <- sort(grep("Dupont, melanie|dupont, mel$", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Dupont] <- "Dupont, Melanie"; rm(Dupont)
Cataldo <- sort(grep("Cataldo, ron", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Cataldo] <- "Cataldo, Ronald W"; rm(Cataldo)
Brooks <- sort(grep("Brooks, rob|brooks, mr rob|brooks, atty rob", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Brooks & dfx$donor_city == "east greenwich"] <- "Brooks, Robert P"; rm(Brooks)
Suever <- sort(grep("Suever esq, eliz|suever, eliz|suever, ms eliz", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Suever] <- "Suever, Elizabeth"; rm(Suever)
Marcano <- sort(grep("Marcano, jose", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Marcano] <- "Marcano, Jose M"; rm(Marcano)
Wilfred <- sort(grep("hill, wilfred", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Wilfred] <- "Hill, Wilfred K"; rm(Wilfred)
Bianchi <- sort(grep("bianchi esq, mr gil|bianchi jr, esq, mr gil|bianchi jr, gil|bianchi, gil|gil, bianchi", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Bianchi] <- "Bianchi Jr, Gil A"; rm(Bianchi)
Brody <- sort(grep("brody esq, will|brody, will", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Brody] <- "Brody, William"; rm(Brody)
Corcoran <- sort(grep("Corcoran, donna", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
dfx$FullName[dfx$FullName %in% Corcoran] <- "Corcoran, Donna R"; rm(Corcoran)
# x <- sort(grep("", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
# dfx$FullName[dfx$FullName %in% ] <- ""; rm()
# x <- sort(grep("", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
# dfx$FullName[dfx$FullName %in% ] <- ""; rm()
# x <- sort(grep("", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
# dfx$FullName[dfx$FullName %in% ] <- ""; rm()
# x <- sort(grep("", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
# dfx$FullName[dfx$FullName %in% ] <- ""; rm()
# x <- sort(grep("", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
# dfx$FullName[dfx$FullName %in% ] <- ""; rm()
# x <- sort(grep("", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
# dfx$FullName[dfx$FullName %in% ] <- ""; rm()
# x <- sort(grep("", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
# dfx$FullName[dfx$FullName %in% ] <- ""; rm()
# x <- sort(grep("", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
# dfx$FullName[dfx$FullName %in% ] <- ""; rm()
# x <- sort(grep("", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
# dfx$FullName[dfx$FullName %in% ] <- ""; rm()
# x <- sort(grep("", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique) %>% print
# dfx$FullName[dfx$FullName %in% ] <- ""; rm()

df1 <- dfx %>% 
     transform(MPFMatchAmount = as.numeric(MPFMatchAmount)) %>%
     select(-c())
     distinct()


# Save
#saveRDS(dfx, "//cdsfsp06/InterSite/Integra/analytics/Misc_Projects/Misc/Providence_Open_Data/campaign_finance_2018-07-30.rds")
#saveRDS(dfx, "//cdsfsp06/InterSite/Integra/analytics/Misc_Projects/Misc/Providence_Open_Data/campaign_finance_2018-08-01.rds")
saveRDS(dfx, "//cdsfsp06/InterSite/Integra/analytics/Misc_Projects/Misc/Providence_Open_Data/campaign_finance_2018-08-04.rds")

# Read in formatted finance data
#dfx <- readRDS("//cdsfsp06/InterSite/Integra/analytics/Misc_Projects/Misc/Providence_Open_Data/campaign_finance_2018-08-01.rds")
dfx <- readRDS("//cdsfsp06/InterSite/Integra/analytics/Misc_Projects/Misc/Providence_Open_Data/campaign_finance_2018-08-04.rds")

# ********************************************                                 ******************************************************
# ********************************************  Finish Formatting Donor Names  ******************************************************
# ********************************************                                 ******************************************************
# ***********************************************************************************************************************************
# ***********************************************************************************************************************************
# ***********************************************************************************************************************************
# ***********************************************************************************************************************************
# ***********************************************************************************************************************************
# ***********************************************************************************************************************************
# ********************************************                                 ******************************************************
# ********************************************                                 ******************************************************
# ********************************************         Exploratory             ******************************************************
# ********************************************                                 ******************************************************
partnersDonors <- sort(grep("cowan, william|finucane, anne|hockfield, dr susan|kaplan, james|fish, john|fish, mr john", dfx$FullName, ignore.case = TRUE, value = TRUE) %>% unique()) %>% print
topDonors <- dfx %>% filter(CY == 2018) %>% 
     #group_by(FirstName,LastName) %>% 
     group_by(FullName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE),
               #Donors = n_distinct(Donor),
               #FullNames = n_distinct(FullName),
               Orgs = n_distinct(OrganizationName),
               Employers = n_distinct(Employer),
               Donations = n_distinct(ContributionID),
               Avg_Donation = round(Total / Donations, 2),
               #Avg_per_Donor = round(Total / Donors, 2),
               Mean_Donation = mean(Amount, na.rm = TRUE),
               Median_Donation = median(Amount, na.rm = TRUE),
               Max_Donation = max(Amount, na.rm = TRUE),
               Donations_per_Org = round(Donations / Orgs, 3)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     mutate(Dollars = monify(Total)); head(topDonors, 20)



healthDonors <- dfx %>% filter(Industry2 == "Healthcare") %>% 
     #group_by(FirstName,LastName) %>% 
     group_by(FullName,Employer) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE),
               #Donors = n_distinct(Donor),
               #FullNames = n_distinct(FullName),
               Orgs = n_distinct(OrganizationName),
               Employers = n_distinct(Employer),
               Donations = n_distinct(ContributionID),
               Avg_Donation = round(Total / Donations, 2),
               #Avg_per_Donor = round(Total / Donors, 2),
               Mean_Donation = mean(Amount, na.rm = TRUE),
               Median_Donation = median(Amount, na.rm = TRUE),
               Max_Donation = max(Amount, na.rm = TRUE),
               Donations_per_Org = round(Donations / Orgs, 3)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     mutate(Dollars = monify(Total)); head(healthDonors, 20)



healthDonors2 <- dfx %>% filter(Industry2 == "Healthcare") %>% 
     #group_by(FirstName,LastName) %>% 
     group_by(Employer,Industry) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE),
               Donors = n_distinct(FullName),
               #FullNames = n_distinct(FullName),
               Orgs = n_distinct(OrganizationName),
               Employers = n_distinct(Employer),
               Donations = n_distinct(ContributionID),
               Donations_per_Donor = round(Donations / Donors, 1),
               Avg_Donation = round(Total / Donations, 2),
               #Avg_per_Donor = round(Total / Donors, 2),
               Mean_Donation = mean(Amount, na.rm = TRUE),
               Median_Donation = median(Amount, na.rm = TRUE),
               Max_Donation = max(Amount, na.rm = TRUE),
               Donations_per_Org = round(Donations / Orgs, 3)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     mutate(Dollars = monify(Total)); head(healthDonors2, 20)





td <- dfx %>% 
     #group_by(FirstName,LastName) %>% 
     #filter(CY %in% c(2016,2017,2018)) %>% 
     group_by(FullName,CY) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE),
               #Donors = n_distinct(Donor),
               #FullNames = n_distinct(FullName),
               Orgs = n_distinct(OrganizationName),
               Employers = n_distinct(Employer),
               Donations = n_distinct(ContributionID),
               Avg_Donation = round(Total / Donations, 2),
               #Avg_per_Donor = round(Total / Donors, 2),
               Mean_Donation = mean(Amount, na.rm = TRUE),
               Median_Donation = median(Amount, na.rm = TRUE),
               Max_Donation = max(Amount, na.rm = TRUE),
               Donations_per_Org = round(Donations / Orgs, 3)) %>% 
     ungroup() %>% 
     arrange(CY,desc(Total)) %>% 
     group_by(CY) %>% 
     top_n(Total, n = 125) %>% ungroup() %>% 
     mutate(Dollars = monify(Total)); head(td, 20)



topN <- topDonors %>% filter(Total > 25000)
bottomN <- topDonors %>% filter(Total <= 25000)

monify(sum(topN$Total))
monify(sum(bottomN$Total))

dfx %>% 
     group_by(ContDesc,ReceiptDesc) %>% 
     summarise(Orgs = n_distinct(OrganizationName),
               Donors = n_distinct(FullName),
               Total = sum(Amount, na.rm = TRUE),
               Donations = n_distinct(ContributionID)) %>% 
     ungroup() %>% 
     mutate(GrandTotal = sum(Total)) %>% 
     mutate(Pct_of_Total = round(Total / GrandTotal, 5)) %>% 
     arrange(desc(Total)) %>% View("q")

dfx %>% 
     group_by(ReceiptDesc) %>% 
     summarise(Orgs = n_distinct(OrganizationName),
               Donors = n_distinct(FullName),
               Total = sum(Amount, na.rm = TRUE),
               Donations = n_distinct(ContributionID)) %>% 
     ungroup() %>% 
     mutate(GrandTotal = sum(Total)) %>% 
     mutate(Pct_of_Total = round(Total / GrandTotal, 5)) %>% 
     arrange(desc(Total)) %>% View("q")




dfx %>% 
     filter(ReceiptDesc == "Cash") %>% 
     group_by(FullName,EmployerName,OrganizationName) %>% 
     summarise(Orgs = n_distinct(OrganizationName),
               Donors = n_distinct(FullName),
               Total = sum(Amount, na.rm = TRUE),
               Donations = n_distinct(ContributionID)) %>% 
     ungroup() %>% 
     mutate(GrandTotal = sum(Total)) %>% 
     mutate(Pct_of_Total = round(Total / GrandTotal, 5)) %>% 
     arrange(desc(Total)) %>% View("q")




dfx %>% 
     filter(ReceiptDesc == "Cash") %>% 
     group_by(CY,OrganizationName) %>% 
     summarise(Orgs = n_distinct(OrganizationName),
               Donors = n_distinct(FullName),
               Total = sum(Amount, na.rm = TRUE),
               Donations = n_distinct(ContributionID)) %>% 
     ungroup() %>% 
     mutate(GrandTotal = sum(Total)) %>% 
     mutate(Pct_of_Total = round(Total / GrandTotal, 5)) %>% 
     arrange(desc(Total)) %>% View("q")


dfx %>% 
     filter(ReceiptDesc == "Cash") %>% 
     group_by(CY) %>% 
     summarise(Orgs = n_distinct(OrganizationName),
               Donors = n_distinct(FullName),
               Total = sum(Amount, na.rm = TRUE),
               Donations = n_distinct(ContributionID)) %>% 
     ungroup() %>% 
     mutate(GrandTotal = sum(Total)) %>% 
     mutate(Pct_of_Total = round(Total / GrandTotal, 5)) %>% 
     arrange(CY) %>% View("q")


dfx %>% 
     filter(ReceiptDesc == "Cash") %>% 
     group_by(OrganizationName) %>% 
     summarise(Orgs = n_distinct(OrganizationName),
               Donors = n_distinct(FullName),
               Total = sum(Amount, na.rm = TRUE),
               Donations = n_distinct(ContributionID)) %>% 
     ungroup() %>% 
     mutate(GrandTotal = sum(Total)) %>% 
     mutate(Pct_of_Total = round(Total / GrandTotal, 5)) %>% 
     arrange(desc(Total)) %>% View("q")





cityDonate <- dfx %>% 
     #filter(donor_region != "Rhode Island") %>% 
     group_by(donor_city,donor_region) %>% 
     summarise(Amount = sum(Amount)) %>% 
     ungroup() %>% mutate(TotAmount = sum(Amount)) %>% mutate(Pct_Amount = round(Amount / TotAmount, 5)) %>% 
     arrange(desc(Amount)) %>% mutate(Dol = monify(Amount)); head(cityDonate, 20) 


riDonate <- dfx %>% 
     filter(donor_region == "Rhode Island") %>% 
     group_by(donor_city,donor_region) %>% 
     summarise(Amount = sum(Amount)) %>% 
     ungroup() %>% mutate(TotAmount = sum(Amount)) %>% mutate(Pct_Amount = round(Amount / TotAmount, 5)) %>% 
     arrange(desc(Amount)) %>% mutate(Dol = monify(Amount)); head(riDonate, 20) 

dfx %>% 
     #filter(donor_region == "Rhode Island") %>% 
     group_by(donor_city,Emp_City) %>% 
     summarise(Amount = sum(Amount)) %>% 
     ungroup() %>% mutate(TotAmount = sum(Amount)) %>% mutate(Pct_Amount = round(Amount / TotAmount, 5)) %>% 
     arrange(desc(Amount)) %>% mutate(Dol = monify(Amount)) %>% head(20)

electMo <- sort(grep("02-11|06-11|10-11|14-11|18-11", dfx$Month_Yr, value = TRUE) %>% unique()) %>% print


emo <- dfx %>% mutate(Var = ifelse(Month_Yr %in% electMo, "Elect Mo", "")) %>% ungroup() %>% select(Month_Yr,Var) %>% distinct() %>%
     arrange(Month_Yr) %>% mutate(Loc = dense_rank(Month_Yr)) %>% 
     filter(Var == "Elect Mo"); head(emo,20)


xlabs <- grep("-01|-07", dfx$Month_Yr, value = TRUE, ignore.case = TRUE) %>% unique(); xlabs


dfx %>% 
     filter(ContDesc != "Loan Proceeds") %>% 
     mutate(DState = case_when(donor_region == "Rhode Island" ~ "Rhode Island",
                               donor_region %in% c("Massachusetts","New York","California") ~ "Mass, NY & CA",
                               TRUE ~ "Out of State")) %>% 
     group_by(Month_Yr,donor_region,DState) %>% summarise(Amount = sum(Amount, na.rm= TRUE)) %>% ungroup() %>% 
     ggplot(aes(Month_Yr, Amount)) +
     geom_line(stat = "identity", aes(group = donor_region, color = donor_region)) +
     geom_vline(xintercept = emo$Loc, lty = 3, lwd = 0.8, color = "gray60") +
     scale_y_continuous(label = dollar) +#, breaks = seq(0,3000000,500000)) +
     scale_x_discrete(breaks = xlabs) +
     theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
     labs(x = "Receipt Month", y = "Contribution Amount",
          title = "Monthly RI Campaign Contributions",
          subtitle = "Loan Proceeds Removed") +
     labs(x = "Receipt Month", y = "Contribution Amount",
          title = "Monthly RI Campaign Contributions",
          subtitle = "Loan Proceeds Excluded") +
     facet_wrap(~DState, scales = "free_y", ncol = 1)


dlabs <- dfx %>% 
     filter(ContDesc != "Loan Proceeds") %>% 
     mutate(DState = case_when(donor_region == "Rhode Island" ~ "Rhode Island",
                               donor_region == "Massachusetts" ~ "Massachusetts",
                               donor_region == "New York" ~ "New York",
                               donor_region == "California" ~ "California",
                               #donor_region %in% c("Massachusetts","New York","California") ~ "Mass, NY & CA",
                               TRUE ~ "Out of State")) %>% 
     group_by(Month_Yr,donor_region) %>% summarise(Amount = sum(Amount, na.rm= TRUE)) %>% ungroup() %>% 
     group_by(donor_region) %>% 
     mutate(Rnk = rank(desc(Amount), ties.method = "first")) %>% 
     ungroup() %>% 
     filter(Rnk < 4); head(dlabs)

# electMo <- sort(grep("02-11|06-11|10-11|14-11|18-11", dfx$Month_Yr, value = TRUE) %>% unique()) %>% print

dfx %>% filter(ReceiptDate > "2008-12-31") %>% filter(donor_region != "") %>% 
     filter(ContDesc != "Loan Proceeds") %>% 
     mutate(DState = case_when(donor_region == "Rhode Island" ~ "Rhode Island",
                               donor_region %in% c("Massachusetts","New York","California") ~ "Mass, NY & CA",
                               TRUE ~ "Out of State")) %>% 
     group_by(Month_Yr,donor_region) %>% summarise(Amount = sum(Amount, na.rm= TRUE)) %>% ungroup() %>% 
     ggplot(aes(Month_Yr, Amount)) +
     geom_line(stat = "identity", aes(group = donor_region, color = donor_region)) +
     geom_vline(xintercept = emo$Loc, lty = 3, lwd = 0.8, color = "gray60") +
     geom_text(data = dlabs, hjust = 0.25, size = 2.5, aes(label = monify(Amount))) +
     scale_y_continuous(label = dollar) +#, breaks = seq(0,3000000,500000)) +
     scale_x_discrete(breaks = xlabs) +
     theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
           legend.position = "none",
           strip.text.x = element_text(margin = margin(0.04,0.04,0.04,0.04, "cm"))) +
     labs(x = "Receipt Month", y = "Contribution Amount",
          title = "Monthly RI Campaign Contributions",
          subtitle = "Loan Proceeds Removed") +
     labs(x = "Receipt Month", y = "Contribution Amount",
          title = "Monthly RI Campaign Contributions",
          subtitle = "Loan Proceeds Excluded") +
     facet_wrap(~donor_region, scales = "free_y", ncol = 1)







cityDonate <- dfx %>% 
     filter(employer_region != "Rhode Island") %>% 
     group_by(Emp_City,employer_region) %>% 
     summarise(Amount = sum(Amount)) %>% 
     ungroup() %>% mutate(TotAmount = sum(Amount)) %>% mutate(Pct_Amount = round(Amount / TotAmount, 5)) %>% 
     arrange(desc(Amount)) %>% mutate(Dol = monify(Amount)); head(cityDonate, 20) 


riDonate <- dfx %>% 
     filter(employer_region == "Rhode Island") %>% 
     group_by(Emp_City,employer_region) %>% 
     summarise(Amount = sum(Amount)) %>% 
     ungroup() %>% mutate(TotAmount = sum(Amount)) %>% mutate(Pct_Amount = round(Amount / TotAmount, 5)) %>% 
     arrange(desc(Amount)) %>% mutate(Dol = monify(Amount)); head(riDonate, 20) 



# ***** Misc, put here for now
#serviceCo <- c("") # 
#laundryCo <- c("")
#foodCo <- c("")
#restorationCo <- c("")
x <- dfx %>% 
     #group_by(FirstName,LastName) %>% 
     group_by(FullName,Employer,Industry,Industry2,OrganizationName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) 



noIndustry <- filter(dfx, Industry == "")%>% 
     group_by(Employer,Industry,Industry2) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) 


noIndustry2 <- filter(dfx, Industry == "")%>% 
     group_by(Employer,EmpAddress,EmpCityStZip,Industry,Industry2) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) 



EmployerIndustry <- dfx %>% 
     #group_by(FirstName,LastName) %>% 
     group_by(Employer,Industry,Industry2) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) 


topIndustries <- dfx %>% 
     #group_by(FirstName,LastName) %>% 
     group_by(Industry,Industry2) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     mutate(Dollars = monify(Total))


topIndustries2 <- dfx %>% 
     #group_by(FirstName,LastName) %>% 
     group_by(Industry2) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE),
               Donors = n_distinct(Donor),
               FullNames = n_distinct(FullName),
               Orgs = n_distinct(OrganizationName),
               Employers = n_distinct(Employer),
               Donations = n_distinct(ContributionID),
               Avg_Donation = round(Total / Donations, 2),
               Avg_per_Donor = round(Total / Donors, 2),
               Mean_Donation = mean(Amount, na.rm = TRUE),
               Median_Donation = median(Amount, na.rm = TRUE),
               Max_Donation = max(Amount, na.rm = TRUE),
               Donations_per_Donor = round(Donations / Donors, 3)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     mutate(Dollars = monify(Total))


healthPACs <- c("carepac of blue cross & blue shield of ri","chartercare employee pac",
                "delta dental of ri pac",
                "cvs health rhode island pac (frmly cvscaremark corporation ri pac",
                "hari pac (hospital association of ri)",
                "rhode island partnership for home care political action committee",
                "ri association of nurse anesthetists pac",
                "ri association of oral & maxillofacial surgeons pac",
                "ri chiropractic pac fund",
                "ri dental pac","ri em pac (emergency medicine)",
                "ri health care association pac",
                "ri medical pac","ri radiology society pac",
                "rioa pac (ri optometric association)",
                "risa pac (ri society anesthesiologist)")



# sort(grep("John m", dfx$Donor, value = TRUE, ignore.case = TRUE) %>% unique()

# filter(dfx, OrganizationName %in% healthPACs) %>% 
#      group_by(OrganizationName) %>% 
#      summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
#      ungroup() %>% 
#      arrange(desc(Total))
# 
# 
# filter(dfx, OrganizationName %in% healthPACs) %>% 
#      group_by(Employer,Industry,Industry2,OrganizationName) %>% 
#      summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
#      ungroup() %>% 
#      arrange(desc(Total)) %>% View("x")



# *************************************************************************************************************************
# ******************************************                      *********************************************************
# ******************************************   Exploratory Plots  *********************************************************
# ******************************************                      *********************************************************
# *************************************************************************************************************************


electMo <- sort(grep("02-11|06-11|10-11|14-11|18-11", dfx$Month_Yr, value = TRUE) %>% unique()) %>% print



emo <- dfx %>% mutate(Var = ifelse(Month_Yr %in% electMo, "Elect Mo", "")) %>% ungroup() %>% select(Month_Yr,Var) %>% distinct() %>%
     arrange(Month_Yr) %>% mutate(Loc = dense_rank(Month_Yr)) %>%
     filter(Var == "Elect Mo"); head(emo,20)

# Clear
#rm(file1, file2, pAff)
x <- x %>% 
     filter(ReceiptDate >= "2018-04-01")

x1 <- x %>%
     group_by(OrganizationName) %>% 
     summarise(Donations = n_distinct(ContributionID),
               Total_Amount = sum(Amount, na.rm = TRUE),
               Avg_Donation = round(Total_Amount / Donations, 2),
               Max_Donation = max(Amount, na.rm = TRUE),
               Donations_Under_100 = n_distinct(ContributionID[Amount <= 100]),
               Pct_Under_100 = round(Donations_Under_100 / Donations, 4),
               Donations_1000_plus = n_distinct(ContributionID[Amount > 1000]),
               Pct_1000_plus = round(Donations_1000_plus / Donations, 4),
               Donors = n_distinct(FullName)) %>% 
     ungroup() %>% 
     arrange(desc(Total_Amount))
     


xlabs <- grep("-01|-07", dfx$Month_Yr, value = TRUE, ignore.case = TRUE) %>% unique(); xlabs


dfx %>% group_by(Month_Yr) %>% summarise(Amount = sum(Amount, na.rm= TRUE)) %>% ungroup() %>% 
     ggplot(aes(Month_Yr, Amount)) +
     geom_line(stat = "identity", aes(group = 1)) +
     geom_vline(xintercept = emo$Loc, lty = 3, lwd = 0.8, color = "violetred") +
     scale_y_continuous(label = dollar, breaks = seq(0,3000000,500000)) +
     labs(x = "Donation Month", y = "Total Donations",
          title = "Rhode Island Monthly Campaign Finance Donations",
          subtitle = srce) +
     scale_x_discrete(breaks = xlabs) +
     theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


dfx %>% group_by(Month_Yr) %>% summarise(Amount = sum(Amount, na.rm= TRUE)) %>% ungroup() %>% 
     ggplot(aes(Month_Yr, Amount)) +
     geom_bar(stat = "identity", width = 0.5, fill = "dodgerblue") + geom_smooth() +
     geom_vline(xintercept = emo$Loc, lty = 3, lwd = 0.8, color = "darkorange") +
     scale_y_continuous(label = dollar, breaks = seq(0,3000000,500000)) +
     labs(x = "Donation Month", y = "Total Donations",
          title = "Rhode Island Monthly Campaign Finance Donations",
          subtitle = srce) +
     scale_x_discrete(breaks = xlabs) +
     theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


dfx %>% filter(ContDesc != "Loan Proceeds") %>% 
     group_by(Mo,CY) %>% 
     summarise(Amount = sum(Amount, na.rm= TRUE)) %>% 
     ungroup() %>% 
     ggplot(aes(Mo, CY)) +
     geom_tile(color = "gray50", alpha = 0.5, aes(fill = Amount)) +
     geom_label(size = 2.25, aes(label = monify(Amount))) +
     scale_y_continuous(breaks = seq(2002,2018,2)) +
     scale_fill_viridis(direction = -1, label = dollar#,
                        #labels = c("$0","$1M","$2M","3M")
     ) +
     labs(x = "", y = "", title = "Monthly RI Political Donations",
          subtitle = srce) +
     theme_bw() +
     theme(legend.position = "right")


dfx %>% group_by(Mo,CY) %>% summarise(Amount = sum(Amount, na.rm= TRUE)) %>% ungroup() %>% 
     ggplot(aes(Mo, Amount)) +
     geom_line(stat = "identity", aes(group = 1)) +
     geom_vline(xintercept = emo$Loc, lty = 3, lwd = 0.8, color = "darkorange") +
     scale_y_continuous(label = dollar) +#, breaks = seq(0,3000000,500000)) +
     labs(x = "Donation Month", y = "Total Donations",
          title = "Rhode Island Monthly Campaign Finance Donations",
          subtitle = srce) +
     #scale_x_discrete(breaks = xlabs) +
     theme(#axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          strip.text.x = element_text(margin = margin(0.06,0.06,0.06,0.06, "cm"))) + 
     facet_wrap(~CY, ncol = 2, scales = "free_y")

#jans <- sort(grep("-01", dfx$Month_Yr, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print

dfx %>%
     filter(ContDesc != "Loan Proceeds") %>% 
     group_by(Month_Yr) %>% summarise(Amount = sum(Amount, na.rm= TRUE)) %>% ungroup() %>% 
     ggplot(aes(Month_Yr, Amount)) +
     geom_line(stat = "identity", aes(group = 1)) +
     geom_vline(xintercept = emo$Loc, lty = 3, lwd = 0.8, color = "gray60") +
     scale_y_continuous(label = dollar, breaks = seq(0,3000000,500000)) +
     scale_x_discrete(breaks = xlabs) +
     theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
     labs(x = "Receipt Month", y = "Contribution Amount",
          title = "Monthly RI Campaign Contributions",
          subtitle = "Loan Proceeds Removed") +
     labs(x = "Receipt Month", y = "Contribution Amount",
          title = "Monthly RI Campaign Contributions",
          subtitle = "Loan Proceeds Excluded")

dfx %>%
     #filter(ContDesc != "Loan Proceeds") %>% 
     group_by(Month_Yr) %>% summarise(Amount = sum(Amount, na.rm= TRUE)) %>% ungroup() %>% 
     ggplot(aes(Month_Yr, Amount)) +
     geom_line(stat = "identity", aes(group = 1)) +
     geom_vline(xintercept = emo$Loc, lty = 3, lwd = 0.8, color = "gray60") +
     scale_y_continuous(label = dollar, breaks = seq(0,3000000,500000)) +
     scale_x_discrete(breaks = xlabs) +
     theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
     labs(x = "Receipt Month", y = "Contribution Amount",
          title = "Monthly RI Campaign Contributions",
          subtitle = "Loan Proceeds Included")

# 
# x1 <- dfx %>%
#      filter(ContDesc != "Loan Proceeds") %>% 
#      group_by(Month_Yr) %>% summarise(Amount = sum(Amount, na.rm= TRUE)) %>% ungroup() %>% mutate(Loan = "Excluded")
# 
# 
# x2 <- dfx %>%
#      #filter(ContDesc != "Loan Proceeds") %>% 
#      group_by(Month_Yr) %>% summarise(Amount = sum(Amount, na.rm= TRUE)) %>% ungroup() %>% mutate(Loan = "Included")
# 
# 
# bind_rows(x1,x2) %>% 
#      ggplot(aes(Month_Yr, Amount)) +
#      geom_line(stat = "identity", aes(group = 1, color = Loan)) +
#      scale_y_continuous(label = dollar, breaks = seq(0,3000000,500000)) +
#      geom_vline(xintercept = emo$Loc, lty = 3, lwd = 0.8, color = "gray60") +
#      scale_x_discrete(breaks = xlabs) + theme_bw() +
#      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), legend.position = "none") +
#      labs(x = "Receipt Month", y = "Contribution Amount",
#           title = "Monthly RI Campaign Contributions",
#           subtitle = "Loan Proceeds Included vs Excluded") +
#      facet_wrap(~Loan, ncol = 1)


# ***************************************************************************************************


# life <- grep("lifespan|ri hospital$|newport hos|hasbro childrens hospital|miriam|bradley|^gateway", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique() %>% print
# care <- grep("care new|butler h|kent h|^memorial h|providence center|vna|infant", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique() %>% print

life <- sort(grep("ri hospital$|newport hospital$|newport hospital life|hasbro childrens hospital|miriam|bradley hosp|^gateway health|^gateway mental|gateway partner|lifespan corp|lifespan ri|lifespan practice|^lifespan$", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print
cne <- sort(grep("care new e|butler h|kent h|^memorial h|providence center|infants hosp|wih", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print


dfx$CNE_Life <- ifelse(dfx$Employer %in% life, "Lifespan",
                       ifelse(dfx$Employer %in% cne, "Care New England", "Other"));table(dfx$CNE_Life)

# Clear
rm(cne,life)

# ***************************************************************************************************
# dfx$FullName[dfx$FullName %in% ] <- ""
# dfx$FullName[dfx$FullName %in% ] <- ""
# dfx$FullName[dfx$FullName %in% ] <- ""
# dfx$FullName[dfx$FullName %in% ] <- ""
# dfx$FullName[dfx$FullName %in% ] <- ""
# dfx$FullName[dfx$FullName %in% ] <- ""
# dfx$FullName[dfx$FullName %in% ] <- ""
# dfx$FullName[dfx$FullName %in% ] <- ""
# dfx$FullName[dfx$FullName %in% ] <- ""
# dfx$FullName[dfx$FullName %in% ] <- ""
# dfx$FullName[dfx$FullName %in% ] <- ""
# dfx$FullName[dfx$FullName %in% ] <- ""
# dfx$FullName[dfx$FullName %in% ] <- ""
# dfx$FullName[dfx$FullName %in% ] <- ""


# dfx %>% select(Donor,FullName) %>% distinct() %>% 
#      group_by(Donor) %>% 
#      summarise(FullNames = n_distinct(FullName)) %>% 
#      ungroup() %>% 
#      arrange(desc(FullNames)) %>% View("Fulls")
# 
# 
# filter(dfx, Donor == "Murphy, William") %>% View("murph")
# 
# dfx %>% select(Donor,FullName) %>% distinct() %>% 
#      group_by(FullName) %>% 
#      summarise(Donors = n_distinct(Donor)) %>% 
#      ungroup() %>% 
#      arrange(desc(Donors)) %>% View("Don")


healthDonors <- dfx %>% 
     filter(ReceiptDate >= "2014-01-01") %>% 
     filter(Industry2 == "Healthcare") %>% 
     group_by(FullName#,#OrganizationName,
              #Donor,
              #donor_region,donor_city,donor_st,donor_state_name,#Party,
              #Employer,Industry,Industry2
              ) %>% 
     summarise(Donations = n_distinct(ContributionID),
               To = n_distinct(OrganizationName),
               Total = sum(Amount, na.rm = TRUE),
               Avg_Donation = round(Total / Donations, 2)) %>% 
     ungroup() %>% #arrange(FullName)
     # transform(Party = ifelse(Party == "", "Other", Party)) %>% 
     # spread(Party, Total) %>% 
     # transform(P = ifelse(is.na(P), 0, P),
     #           D = ifelse(is.na(D), 0, D),
     #           U = ifelse(is.na(U), 0, U),
     #           R = ifelse(is.na(R), 0, R)) %>% 
     # mutate(Total = P + U + D + R) %>% 
     arrange(desc(Total)); head(healthDonors) #%>% 
     #mutate(Pct_D = round(D / Total, 3)); head(healthDonors)


topHealthOrgs <- dfx %>% 
     filter(Industry2 == "Healthcare") %>% 
     filter(ReceiptDate > "2014-12-31") %>% 
     group_by(OrganizationName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% head(25); head(topHealthOrgs)
     
topHealthInds <- dfx %>% 
     filter(Industry2 == "Healthcare") %>% 
     filter(ReceiptDate > "2014-12-31") %>% 
     group_by(Industry) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% head(25); head(topHealthInds)



healthx <- dfx %>% 
     filter(Industry2 == "Healthcare") %>% 
     filter(ReceiptDate > "2014-12-31") %>% 
     filter(OrganizationName %in% topHealthOrgs$OrganizationName) %>% 
     group_by(CY,OrganizationName) %>% 
     summarise(Donors = n_distinct(FullName),
               Donations = n_distinct(ContributionID),
               #Donations_per_Donor = round(Donations / Donors, 1),
               Total = sum(Amount, na.rm = TRUE),
               #Max_Donation = max(Amount, na.rm = TRUE),
               #Median_Donation = median(Amount, na.rm = TRUE),
               Avg_per_Donation = round(Total / Donations, 2)) %>% #,
               #Avg_per_Donor = round(Total / Donors, 2)) %>% 
     ungroup() %>% #arrange(FullName)
     # transform(Party = ifelse(Party == "", "Other", Party)) %>% 
     # spread(Party, Total) %>% 
     # transform(P = ifelse(is.na(P), 0, P),
     #           D = ifelse(is.na(D), 0, D),
     #           U = ifelse(is.na(U), 0, U),
     #           R = ifelse(is.na(R), 0, R)) %>% 
     # mutate(Total = P + U + D + R) %>% 
     arrange(OrganizationName, CY); head(healthx) #%>% 
#mutate(Pct_D = round(D / Total, 3)); head(healthDonors)


hlthx <- healthx %>% gather(Measure, Var, Donors:Avg_per_Donation); head(hlthx)
hlthx <- healthx %>% select(CY,OrganizationName,Total) %>% gather(Measure, Var, -c(1:2)); head(hlthx)

ggplot(hlthx, aes(CY,Var)) + 
     geom_line(stat = "identity", aes(group = OrganizationName, color = OrganizationName)) +
     geom_point(size = 1, alpha = 0.5, aes(color = OrganizationName)) +
     geom_text(size = 2.5, fontface = "bold", aes(label = monify(Var)), vjust = -0.5) +
     #geom_text(data = filter(hlthx, CY == "2014"), size = 3, aes(label = str_wrap(OrganizationName,30)), vjust = -1) +
     facet_wrap(~OrganizationName, scales = "free_y") +
     theme(legend.position = "none",
           strip.text.x = element_text(margin = margin(0.04,0.04,0.04,0.04, "cm"), size = 8))



healthy <- dfx %>% 
     filter(Industry2 == "Healthcare") %>% 
     filter(ReceiptDate > "2014-12-31") %>% 
     filter(Industry %in% topHealthInds$Industry) %>% 
     group_by(CY,Industry) %>% 
     summarise(Donors = n_distinct(FullName),
               Donations = n_distinct(ContributionID),
               Total = sum(Amount, na.rm = TRUE),
               Avg_per_Donation = round(Total / Donations, 2)) %>% #,
     ungroup() %>% #arrange(FullName)
     arrange(Industry, CY); head(healthy)

ggplot(healthy, aes(CY, Total)) + 
     geom_line(stat = "identity", aes(group = Industry, color = Industry)) + theme_bw() +
     theme(legend.position = "none",
           strip.text.x = element_text(margin = margin(0.04,0.04,0.04,0.04, "cm"), size = 8)) +
     facet_wrap(~Industry, scales = "free_y")


topDonors2 <- dfx %>% 
     filter(Industry2 == "Healthcare") %>% 
     group_by(FullName,donor_region,OrganizationName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% arrange(desc(Total)); head(topDonors2)



ggplot(dfx, aes(employer_region,donor_region)) + 
     geom_jitter(alpha = 0.2, aes(color = Party)) +
     facet_wrap(~Party) +
     theme_bw() +
     theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))



# Monthly by Donor State
dfx %>% 
     group_by(Month_Yr, donor_region) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     ggplot(aes(Month_Yr, Total)) +
     geom_line(stat = "identity", aes(color = donor_region, group = donor_region)) +
     #geom_vline(aes(xintercept = electMo), lty = 3, color = "darkorange") +
     geom_point(size = 2, color = "gray50") + geom_point(size = 1, aes(color = donor_region)) +
     scale_y_continuous(label = dollar) +
     scale_x_discrete(breaks = xlabs) +
     labs(x = "Receipt Month", y = "Total Donations",
          title = "Monthly Political Donations by State of Donor",
          subtitle = srce) +
     theme_bw() +
     theme(legend.position = "none",
           strip.text.x = element_text(margin = margin(0.1,0.1,0.1,0.1, "cm")),
           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
     facet_wrap(~donor_region,scales = "free_y", ncol = 2)



# Monthly by Donor State
dfx %>% 
     mutate(Gina = ifelse(OrganizationName == "gina m. raimondo", "Gina", "The Field")) %>% 
     group_by(Month_Yr, donor_region, Gina) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     ggplot(aes(Month_Yr, Total)) +
     geom_line(stat = "identity", aes(color = Gina, group = Gina)) +
     #geom_vline(aes(xintercept = electMo), lty = 3, color = "darkorange") +
     geom_point(size = 2, color = "gray50") + geom_point(size = 1, aes(color = Gina)) +
     scale_y_continuous(label = dollar) +
     scale_x_discrete(breaks = xlabs) +
     labs(x = "Receipt Month", y = "Total Donations",
          title = "Monthly Political Donations by State of Donor - Gina vs the Field",
          subtitle = srce) +
     theme_bw() +
     theme(legend.position = "bottom",
           strip.text.x = element_text(margin = margin(0.1,0.1,0.1,0.1, "cm")),
           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
     facet_wrap(~donor_region,scales = "free_y", ncol = 2)



# Monthly by Employer State
dfx %>% 
     group_by(Month_Yr, employer_region) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     ggplot(aes(Month_Yr, Total)) +
     geom_line(stat = "identity", aes(color = employer_region, group = employer_region)) +
     geom_point(size = 2, color = "gray50") + geom_point(size = 1, aes(color = employer_region)) +
     scale_y_continuous(label = dollar) +
     labs(x = "Receipt Month", y = "Total Donations",
          title = "Monthly Political Donations by Employer State",
          subtitle = srce) +
     theme_bw() +
     theme(legend.position = "none",
           strip.text.x = element_text(margin = margin(0.1,0.1,0.1,0.1, "cm")),
           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
     facet_wrap(~employer_region,scales = "free_y", ncol = 2)

dfx %>% 
     mutate(Gina = ifelse(OrganizationName == "gina m. raimondo", "Gina", "The Field")) %>% 
     group_by(Month_Yr, employer_region, Gina) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     ggplot(aes(Month_Yr, Total)) +
     geom_line(stat = "identity", aes(color = Gina, group = Gina)) +
     geom_point(size = 2, color = "gray50") + geom_point(size = 1, aes(color = Gina)) +
     scale_y_continuous(label = dollar) +
     scale_x_discrete(breaks = xlabs) +
     labs(x = "Receipt Month", y = "Total Donations",
          title = "Monthly Political Donations by Employer State",
          subtitle = srce) +
     theme_bw() +
     theme(legend.position = "bottom",
           strip.text.x = element_text(margin = margin(0.1,0.1,0.1,0.1, "cm")),
           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
     facet_wrap(~employer_region,scales = "free_y", ncol = 2)



states <- dfx %>% 
     #group_by(FirstName,LastName) %>% 
     group_by(donor_region,employer_region) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     mutate(Amt = monify(Total),
            TotAmt = sum(Total)) %>% 
     mutate(Pct_of_Total = round(Total / TotAmt, 4)); head(states)



dfx %>% 
     #group_by(FirstName,LastName) %>% 
     group_by(donor_region) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     mutate(Amt = monify(Total),
            TotAmt = sum(Total)) %>% 
     mutate(Pct_of_Total = round(Total / TotAmt, 4)) %>% 
     ggplot(aes(donor_region, Total)) +
     geom_bar(stat = "identity", color = "gray50", width = 0.5, aes(fill = donor_region)) +
     geom_label(size = 2.7, vjust = -0.25,
                aes(color = donor_region, label = percent(Pct_of_Total))) +
     scale_y_continuous(label = dollar) +
     labs(x = "", y = "Total", title = "Donations by Donor State",
          subtitle = srce) +
     theme(legend.position = "none",
           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 


dfx %>% 
     #group_by(FirstName,LastName) %>% 
     group_by(employer_region) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     mutate(Amt = monify(Total),
            TotAmt = sum(Total)) %>% 
     mutate(Pct_of_Total = round(Total / TotAmt, 4)) %>% 
     ggplot(aes(employer_region, Total)) +
     geom_bar(stat = "identity", color = "gray50", width = 0.5, aes(fill = employer_region)) +
     geom_label(size = 2.7, vjust = -0.25,
                aes(color = employer_region, label = percent(Pct_of_Total))) +
     scale_y_continuous(label = dollar) +
     labs(x = "", y = "Total", title = "Donations by Employer State",
          subtitle = srce) +
     theme(legend.position = "none",
           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 



statesD <- dfx %>% 
     #group_by(FirstName,LastName) %>% 
     group_by(donor_region) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     mutate(Amt = monify(Total),
            TotAmt = sum(Total)) %>% 
     mutate(Pct_of_Total = round(Total / TotAmt, 4)) %>% 
     mutate(Source = "Donor State") %>% 
     rename(State = donor_region); head(statesD)

statesE <- dfx %>% 
     #group_by(FirstName,LastName) %>% 
     group_by(employer_region) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     mutate(Amt = monify(Total),
            TotAmt = sum(Total)) %>% 
     mutate(Pct_of_Total = round(Total / TotAmt, 4)) %>% 
     mutate(Source = "Employer State") %>% 
     rename(State = employer_region); head(statesE)


s <- bind_rows(statesD, statesE) %>% 
     transform(#State = str_wrap(State, 8),
          Source = str_wrap(Source, 8))


ggplot(s, aes(Source, Total)) +
     geom_bar(stat = "identity", color = "gray50", width = 0.5, position = "dodge", aes(fill = Source)) +
     scale_y_continuous(label = dollar) +
     labs(x = "", y = "Total", 
          title = "RI Political Donations by State",
          subtitle = "Donor State vs Employer State") +
     theme_bw() +
     theme(legend.position = "bottom", 
           legend.background = element_rect(color = "gray50"),
           legend.title = element_blank()) +
     facet_wrap(~State, scales = "free")


# Clear
rm(statesD,statesE)

dfx %>% 
     #group_by(FirstName,LastName) %>% 
     filter(donor_region != "Rhode Island") %>% 
     group_by(donor_region) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     mutate(Amt = monify(Total),
            TotAmt = sum(Total)) %>% 
     mutate(Pct_of_Total = round(Total / TotAmt, 4)) %>% 
     ggplot(aes(donor_region, Total)) +
     geom_bar(stat = "identity", color = "gray50", aes(fill = donor_region), width = 0.5) +
     coord_polar()

dfx %>% 
     group_by(donor_region) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     mutate(Amt = monify(Total),
            TotAmt = sum(Total)) %>% 
     mutate(Pct_of_Total = round(Total / TotAmt, 4)) %>% 
     ggplot(aes(reorder(donor_region, Total), Total)) +
     geom_bar(stat = "identity", color = "gray50", aes(fill = donor_region), width = 0.5) +
     labs(x = "Donor State", y = "Total",
          title = "Political Donation by Donor State") +
     scale_y_continuous(label = dollar) +
     coord_flip() +
     theme(legend.position = "none")



# Gina vs the field
g2 <- dfx %>% 
     mutate(Gina = ifelse(OrganizationName == "gina m. raimondo", "Governor\nRaimondo", "Everybody\nElse")) %>% 
     group_by(donor_region,Gina) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     ggplot(aes(Gina, Total)) + 
     geom_bar(stat = "identity", color = "gray50", width = 0.5, position = "dodge", aes(fill = Gina)) +
     scale_fill_manual(values = c("dodgerblue","navyblue")) +
     scale_y_continuous(label = dollar) +
     labs(x = "", y = "Donations Received", 
          title = "Rhode Island 2017 Political Donations Received by State of Donor",
          subtitle = "The Governor vs the field",
          caption = paste(srce, "downloaded on Feb 9, 2017.  Donation 'ReceiptDate' between Jan 1, 2017 & Dec 31, 2017", sep = "\n")) +
     theme_bw() +
     theme(legend.position = "none", legend.background = element_rect(color = "gray50"),
           legend.title = element_blank(),
           plot.caption = element_text(hjust = 0.5)) +
     facet_wrap(~donor_region, scales = "free")
g2

g3 <- dfx %>% 
     mutate(Gina = ifelse(OrganizationName == "gina m. raimondo", "Governor\nRaimondo", "Everybody\nElse")) %>% 
     group_by(employer_region,Gina) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     ggplot(aes(Gina, Total)) + 
     geom_bar(stat = "identity", color = "gray50", width = 0.5, position = "dodge", aes(fill = Gina)) +
     scale_fill_manual(values = c("dodgerblue","navyblue")) +
     scale_y_continuous(label = dollar) +
     labs(x = "", y = "Donations Received", 
          title = "Rhode Island 2017 Political Donations Received by Employer State",
          subtitle = "The Governor vs the field",
          caption = paste(srce, "downloaded on Feb 9, 2017.  Donation 'ReceiptDate' between Jan 1, 2017 & Dec 31, 2017", sep = "\n")) +
     theme_bw() +
     theme(legend.position = "none", legend.background = element_rect(color = "gray50"),
           legend.title = element_blank(),
           plot.caption = element_text(hjust = 0.5)) +
     facet_wrap(~employer_region, scales = "free")
g3













library(qgraph)


x <- filter(dfx, OrganizationName %in% healthPACs) %>% 
     filter(ReceiptDate > "2016-12-31") %>% 
     group_by(Employer,OrganizationName) %>% 
     summarise(Total = round(sum(Amount, na.rm = TRUE),0)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     filter(Total > 499) %>% 
     transform(Employer = str_wrap(str_to_title(Employer), 12),
               OrganizationName = str_wrap(str_to_title(OrganizationName), 14)); head(x)


d <- data.frame(c(unique(x$Employer), unique(x$OrganizationName)), stringsAsFactors = FALSE) %>% 
     setNames("label") %>% 
     mutate(col = ifelse(label %in% x$Employer, "dodgerblue","darkorange"),
            shp = ifelse(label %in% x$Employer, "circle","triangle"))

xLabs <- monify(x$Total)


qgraph(x, 
       layout = "spring",
       vsize = 5, # Size of the nodes
       label.cex = 6, # Size of Node Labels
       mar = c(1,1,1,1),
       title = "Political Donations\nto RI Healthcare PAC's\nby Employer", 
       edge.color = "gray50", # Line color
       edge.width = 0.75, 
       fade = FALSE,
       edge.label.color = "black", # Color of the numeric labels
       bg = "whitesmoke",
       fade = TRUE,
       vTrans = 100,
       color = d$col, 
       shape = d$shp,
       #color = nodeLists$col,
       #color = nList$col,
       #shape = nodeLists$shp,
       label.scale.equal = TRUE,
       edge.label.cex = 0.6,
       edge.label.position = 0.5,
       edge.label.bg = "gray90",
       edge.labels= xLabs)





x <- filter(dfx, OrganizationName %in% healthPACs) %>% 
     filter(ReceiptDate > "2016-12-31") %>% 
     group_by(FullName,OrganizationName) %>% 
     summarise(Total = round(sum(Amount, na.rm = TRUE),0)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     filter(Total > 449) %>% 
     transform(FullName = str_wrap(str_to_title(FullName), 12),
               OrganizationName = str_wrap(str_to_title(OrganizationName), 14)); head(x)


d <- data.frame(c(unique(x$FullName), unique(x$OrganizationName)), stringsAsFactors = FALSE) %>% 
     setNames("label") %>% 
     mutate(col = ifelse(label %in% x$FullName, "dodgerblue","darkorange"),
            shp = ifelse(label %in% x$FullName, "circle","triangle"))

xLabs <- monify(x$Total)


qgraph(x, 
       layout = "spring",
       vsize = 4, # Size of the nodes
       label.cex = 6, # Size of Node Labels
       mar = c(1,1,1,1),
       title = "Political Donations\nto RI Healthcare PAC's\nby Donor", 
       edge.color = "gray50", # Line color
       edge.width = 0.75, 
       fade = FALSE,
       edge.label.color = "black", # Color of the numeric labels
       bg = "whitesmoke",
       fade = TRUE,
       vTrans = 100,
       color = d$col, 
       shape = d$shp,
       #color = nodeLists$col,
       #color = nList$col,
       #shape = nodeLists$shp,
       label.scale.equal = TRUE,
       edge.label.cex = 0.5,
       edge.label.position = 0.5,
       edge.label.bg = "gray90",
       edge.labels= xLabs)

topPacs <- dfx %>% 
     filter(PAC == "PAC") %>% filter(ReceiptDate > "2015-12-31") %>% 
     #group_by(FirstName,LastName) %>% 
     group_by(OrganizationName) %>% 
     summarise(Donations = n_distinct(ContributionID),
               Donors = n_distinct(FullName),
               Donations_per_Donor = round(Donations / Donors, 1),
               Total = sum(Amount, na.rm = TRUE),
               Avg_Donation = round(Total / Donations, 2)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     mutate(Dollars = monify(Total)); head(topPacs)


x <- dfx %>% 
     filter(PAC == "PAC") %>%
     group_by(CY,PAC) %>% 
     summarise(Donations = n_distinct(ContributionID),
               Donors = n_distinct(FullName),
               Donations_per_Donor = round(Donations / Donors, 1),
               Total = sum(Amount, na.rm = TRUE),
               Avg_Donation = round(Total / Donations, 2)) %>% 
     ungroup() %>% 
     arrange(CY,PAC) %>% 
     mutate(Dollars = monify(Total))


ggplot(x, aes(CY,Total)) + geom_line(stat = "identity", aes(group = 1)) +
     geom_point(alpha = 0.5) +
     scale_y_continuous(label = dollar) +
     scale_x_continuous(breaks = seq(2002,2018,2)) +
     geom_text(size = 2.75, aes(label = monify(Total)))


 dfx %>% 
     filter(PAC == "PAC") %>% 
     transform(OrganizationName = gsub("international", "int'l", OrganizationName)) %>%
     transform(OrganizationName = gsub("political committee", "pac", OrganizationName)) %>% 
     transform(OrganizationName = str_wrap(str_to_title(OrganizationName), 40)) %>% 
     group_by(OrganizationName, donor_region) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% #View("x")
     arrange(desc(Total)) %>% 
     group_by(donor_region) %>% 
     top_n(Total, n = 8) %>% 
     ungroup() %>% #View("x")
     filter(Total > 300) %>% 
     ggplot(aes(OrganizationName, Total)) + 
     geom_bar(stat = "identity", color = "gray50", width = 0.5, position = "dodge", aes(fill = donor_region)) +
     #scale_fill_manual(values = c("dodgerblue","darkorange")) +
     scale_y_continuous(label = dollar) +
     labs(x = "", y = "Donations Received", 
          title = "RI 2017 Political Action Committee Donations",
          subtitle = "Highest grossing PAC's by state of donor",
          caption = paste(srce, "Data downloaded on Feb 9, 2017.  Donation 'ReceiptDate' between Jan 1, 2017 & Dec 31, 2017", sep = "\n")) +
     theme_bw() +
     theme(legend.position = "none", legend.background = element_rect(color = "gray50"),
           legend.title = element_blank(),
           strip.text.x = element_text(margin = margin(0.05,0.05,0.05,0.05, "cm")),
           axis.text.x = element_text(size = 7.5, face = "bold", angle = 45, hjust = 1, vjust = 1),
           plot.caption = element_text(hjust = 0.5)) +
     facet_wrap(~donor_region, scales = "free")# +
     # coord_flip()


dfx %>% 
     filter(PAC == "Y") %>% 
     mutate(HealthPac = ifelse(OrganizationName %in% healthPACs, "Healthcare", "")) %>% 
     transform(OrganizationName = gsub("international", "int'l", OrganizationName)) %>%
     transform(OrganizationName = gsub("political committee", "pac", OrganizationName)) %>% 
     transform(OrganizationName = str_wrap(str_to_title(OrganizationName), 80)) %>% 
     transform(OrganizationName = gsub("^Ri ", "RI ", OrganizationName)) %>% 
     transform(OrganizationName = gsub(" Ri ", " RI ", OrganizationName)) %>% 
     transform(OrganizationName = gsub("Of Ri ", "of RI ", OrganizationName)) %>% 
     transform(OrganizationName = gsub("Pac$", "PAC", OrganizationName)) %>% 
     group_by(OrganizationName,HealthPac) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>%# View("x")
     arrange(desc(Total)) %>% 
     head(60) %>% 
     ggplot(aes(reorder(OrganizationName, Total), Total)) + 
     geom_bar(stat = "identity", color = "gray50", width = 0.5, position = "dodge", aes(fill = Total)) +
     scale_fill_viridis(labels = dollar, direction = -1) +
     scale_y_continuous(label = dollar, breaks = seq(0,100000,10000)) +
     labs(x = "", y = "Donations Received", 
          title = "Top Grossing Rhode Island Political Action Committees in 2017",
          subtitle = paste("Data downloaded on Feb 9, 2017 from ", srce,
                           ".  ('ReceiptDate' between Jan 1, 2017 & Dec 31, 2017)", sep = "")) +
          #caption = "  Donation ", sep = "\n")) +
     #theme_bw() +
     theme(legend.position = "none", legend.background = element_rect(color = "gray50"),
           legend.title = element_blank(),
           strip.text.x = element_text(margin = margin(0.05,0.05,0.05,0.05, "cm")),
           axis.text.y = element_text(size = 7.5, face = "bold"), #angle = 45, hjust = 1, vjust = 1),
           plot.caption = element_text(hjust = 0.5)) +
     coord_flip()


dfx %>% 
     filter(PAC == "Y") %>% 
     mutate(HealthPac = ifelse(OrganizationName %in% healthPACs, "Healthcare", "Non-Healthcare")) %>% 
     filter(HealthPac == "Healthcare")%>% 
     transform(OrganizationName = gsub("international", "int'l", OrganizationName)) %>%
     transform(OrganizationName = gsub("political committee", "pac", OrganizationName)) %>% 
     transform(OrganizationName = str_wrap(str_to_title(OrganizationName), 80)) %>% 
     transform(OrganizationName = gsub("^Ri ", "RI ", OrganizationName)) %>% 
     transform(OrganizationName = gsub(" Ri ", " RI ", OrganizationName)) %>% 
     transform(OrganizationName = gsub("Of Ri ", "of RI ", OrganizationName)) %>% 
     transform(OrganizationName = gsub("Pac$", "PAC", OrganizationName)) %>% 
     group_by(OrganizationName,HealthPac) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>%# View("x")
     head(50) %>% 
     ggplot(aes(reorder(OrganizationName, Total), Total)) + 
     geom_bar(stat = "identity", color = "gray50", width = 0.5, position = "dodge", aes(fill = Total)) +
     scale_fill_viridis(labels = dollar, direction = -1) +
     scale_y_continuous(label = dollar, breaks = seq(0,20000,2000)) +
     labs(x = "", y = "Donations Received", 
          title = "Rhode Island Healthcare PAC's",
          subtitle = "2017 Total Donations Received",
          caption = paste(srce, "Data downloaded on Feb 9, 2017.  Donation 'ReceiptDate' between Jan 1, 2017 & Dec 31, 2017", sep = "\n")) +
     theme_bw() +
     theme(legend.position = "none", legend.background = element_rect(color = "gray50"),
           legend.title = element_blank(),
           strip.text.x = element_text(margin = margin(0.05,0.05,0.05,0.05, "cm")),
           #axis.text.x = element_text(size = 9, angle = 45, hjust = 1, vjust = 1),
           plot.caption = element_text(hjust = 0.5)) +
     coord_flip() +
     facet_wrap(~HealthPac, scales = "free")


topEmployers <- dfx %>% 
     #group_by(FirstName,LastName) %>% 
     group_by(Employer) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     mutate(Dollars = monify(Total))


dfx %>%
     transform(Employer = str_to_title(Employer)) %>% 
     transform(Employer = gsub(" Ri$", "RI", Employer)) %>% 
     group_by(Employer) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>%# View("x")
     arrange(desc(Total)) %>% 
     head(70) %>% 
     ggplot(aes(reorder(Employer, Total), Total)) + 
     geom_bar(stat = "identity", color = "gray50", width = 0.5, position = "dodge", aes(fill = Total)) +
     scale_fill_viridis(labels = dollar, direction = -1) +
     scale_y_continuous(label = dollar, breaks = seq(0,200000,20000)) +
     labs(x = "", y = "Donations Received", 
          title = "Rhode Island 2017 Political Donations by Employer",
          #subtitle = "2017 Total Donations",
          caption = paste(srce, "Data downloaded on Feb 9, 2017.  Donation 'ReceiptDate' between Jan 1, 2017 & Dec 31, 2017", sep = "\n")) +
     theme_bw() +
     theme(legend.position = "none", legend.background = element_rect(color = "gray50"),
           legend.title = element_blank(),
           strip.text.x = element_text(margin = margin(0.05,0.05,0.05,0.05, "cm")),
           axis.text.y = element_text(size = 7.5, face = "bold"),
           plot.caption = element_text(hjust = 0.5)) +
     coord_flip() #+ facet_wrap(~Industry2, scales = "free")
#     facet_wrap(~HealthPac, scales = "free")



dfx %>%
     transform(Employer = str_to_title(Employer)) %>% 
     transform(Employer = gsub(" Ri$", "RI", Employer)) %>% 
     group_by(Employer,Industry2) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>%# View("x")
     arrange(desc(Total)) %>% 
     head(60) %>% 
     ggplot(aes(reorder(Employer, Total), Total)) + 
     geom_bar(stat = "identity", color = "gray50", width = 0.5, position = "dodge", aes(fill = Industry2)) +
     #scale_fill_viridis(labels = dollar, direction = -1) +
     scale_y_continuous(label = dollar, breaks = seq(0,200000,20000)) +
     labs(x = "", y = "Donations Received", 
          title = "Rhode Island 2017 Political Donations by Employer",
          #subtitle = "2017 Total Donations",
          caption = paste(srce, "Data downloaded on Feb 9, 2017.  Donation 'ReceiptDate' between Jan 1, 2017 & Dec 31, 2017", sep = "\n")) +
     theme_bw() +
     theme(legend.position = c(0.7,0.4), legend.background = element_rect(color = "gray50"),
           legend.title = element_blank(),
           strip.text.x = element_text(margin = margin(0.05,0.05,0.05,0.05, "cm")),
           axis.text.y = element_text(size = 7.5, face = "bold"),
           plot.caption = element_text(hjust = 0.5)) +
     coord_flip() #+ facet_wrap(~Industry2, scales = "free")




dfx %>%
     transform(Employer = str_to_title(Employer)) %>% 
     transform(Employer = gsub(" Ri$", "RI", Employer)) %>% 
     group_by(Employer,employer_region,Industry2) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>%# View("x")
     arrange(desc(Total)) %>% 
     #group_by(employer_region) 
     head(60) %>% 
     ggplot(aes(reorder(Employer, Total), Total)) + 
     geom_bar(stat = "identity", #color = "gray50", width = 0.25,
              position = "dodge", aes(fill = employer_region)) +
     #scale_fill_viridis(labels = dollar, direction = -1) +
     scale_y_continuous(label = dollar, breaks = seq(0,200000,20000)) +
     labs(x = "", y = "Donations Received", 
          title = "Rhode Island 2017 Political Donations by Employer",
          #subtitle = "2017 Total Donations",
          caption = paste(srce, "Data downloaded on Feb 9, 2017.  Donation 'ReceiptDate' between Jan 1, 2017 & Dec 31, 2017", sep = "\n")) +
     theme_bw() +
     theme(legend.position = "bottom", legend.background = element_rect(color = "gray50"),
           legend.title = element_blank(),
           strip.text.x = element_text(margin = margin(0.05,0.05,0.05,0.05, "cm")),
           axis.text.y = element_text(size = 7.5, face = "bold"),
           plot.caption = element_text(hjust = 0.5)) +
     coord_flip()# + facet_wrap(~Industry2, scales = "free")



dfx %>%
     transform(Employer = str_to_title(Employer)) %>% 
     transform(Employer = gsub(" Ri$", "RI", Employer)) %>% 
     group_by(Employer,employer_region) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>%# View("x")
     arrange(desc(Total)) %>%
     group_by(employer_region) %>% 
     top_n(Total, n = 12) %>% 
     ungroup() %>% 
     filter(Total > 199) %>%  #View("x")
     ggplot(aes(Employer, Total)) + 
     geom_bar(stat = "identity", color = "gray50", width = 0.5, position = "dodge", aes(fill = employer_region)) +
     #scale_fill_viridis(labels = dollar, direction = -1) +
     scale_y_continuous(label = dollar) +
     labs(x = "", y = "Donations Received", 
          title = "2017 Political Donations by Employer & State",
          #subtitle = "2017 Total Donations",
          caption = paste(srce, "Data downloaded on Feb 9, 2017.  Donation 'ReceiptDate' between Jan 1, 2017 & Dec 31, 2017", sep = "\n")) +
     theme_bw() +
     theme(legend.position = "none", legend.background = element_rect(color = "gray50"),
           legend.title = element_blank(),
           strip.text.x = element_text(margin = margin(0.05,0.05,0.05,0.05, "cm")),
           axis.text.y = element_text(size = 7.5, face = "bold"),
           axis.text.x = element_text(size = 8, angle = 45, hjust = 1, vjust = 1),
           plot.caption = element_text(hjust = 0.5)) +
     coord_flip() +
     facet_wrap(~employer_region, scales = "free", ncol = 3)
 #     facet_wrap(~HealthPac, scales = "free")


topRecipients <- dfx %>% 
     filter(OrganizationName != "william h gilbert") %>% 
     filter(!OrganizationName %in% c("william h gilbert","ri democratic state committee")) %>% 
     #group_by(FirstName,LastName) %>% 
     group_by(OrganizationName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     mutate(Dollars = monify(Total)) %>% 
     head(13)


topOrgs <- dfx %>% 
     #group_by(FirstName,LastName) %>% 
     group_by(OrganizationName,Party) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     mutate(Dollars = monify(Total)) 

noIndustry <- filter(dfx, Industry == "")%>% 
     group_by(Employer,Industry,Industry2) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) 

EmployerIndustry <- dfx %>% 
     #group_by(FirstName,LastName) %>% 
     group_by(Employer,Industry,Industry2) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) 


topIndustries <- dfx %>% 
     #group_by(FirstName,LastName) %>% 
     group_by(Industry,Industry2) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     mutate(Dollars = monify(Total))


topIndustries2 <- dfx %>% 
     #group_by(FirstName,LastName) %>% 
     group_by(Industry2) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     mutate(Dollars = monify(Total))

topIndustries3 <- dfx %>% 
     #group_by(FirstName,LastName) %>% 
     group_by(Industry2,Party) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     #mutate(Dollars = monify(Total)) %>% 
     transform(Party = ifelse(Party == "", "Other", Party)) %>% 
     spread(Party, Total) %>% 
     transform(Other = ifelse(is.na(Other), 0, Other),
               D = ifelse(is.na(D), 0, D),
               R = ifelse(is.na(R), 0, R)) %>% 
     mutate(Total = Other + D + R) %>% 
     arrange(desc(Total)) %>% 
     mutate(Pct_D = round(D / Total, 3),
            Pct_R = round(R / Total, 3))



gina <- dfx %>% 
     filter(OrganizationName == "gina m. raimondo") %>% 
     group_by(OrganizationName,#Industry2,
          employer_region) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total))

gina2 <- dfx %>% 
     filter(OrganizationName == "gina m. raimondo") %>% 
     group_by(Employer,Industry,Industry2) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total))


topRecipients <- dfx %>% 
     #filter(OrganizationName != "william h gilbert") %>% 
     filter(!OrganizationName %in% c("william h gilbert","ri democratic state committee")) %>% 
     #group_by(FirstName,LastName) %>% 
     group_by(OrganizationName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     mutate(Dollars = monify(Total)) %>% 
     head(13)


net <- dfx %>% 
     filter(OrganizationName %in% topRecipients$OrganizationName) %>% 
     transform(OrganizationName = str_to_title(OrganizationName)) %>% 
     group_by(employer_region,OrganizationName,Party,Candidate,PAC) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     transform(employer_region = str_wrap(employer_region, 8),
               OrganizationName = str_wrap(strtrim(OrganizationName, 40), 8)) %>% 
     arrange(desc(Total)) %>% 
     # group_by(employer_region) %>%
     # top_n(Total, n = 9) %>%
     # ungroup()
     filter(Total > 1999); head(net)


d <- data.frame(c(unique(net$employer_region), unique(net$OrganizationName)), stringsAsFactors = FALSE) %>% 
     setNames("label")




stList <- net %>% select(employer_region) %>% 
     distinct() %>% rename(label = employer_region) %>% 
     mutate(col = "yellow", shp = "rectangle");stList

demList <- filter(net, Party == "D") %>% 
     select(OrganizationName) %>% 
     distinct() %>% 
     rename(label = OrganizationName) %>% 
     mutate(col = "dodgerblue", shp = "circle")

gopList <- filter(net, Party == "R") %>% 
     select(OrganizationName) %>% distinct() %>% 
     rename(label = OrganizationName) %>% 
     mutate(col = "red", shp = "circle")


candList <- filter(net, Candidate == "Y") %>% 
     select(OrganizationName,Party) %>% distinct() %>% 
     transform(OrganizationName = gsub("^", "candidate ", OrganizationName)) %>% 
     rename(label = OrganizationName) %>% 
     mutate(col = ifelse(Party == "D", "dodgerblue",
                         ifelse(Party == "R", "red", "plum")), 
            shp = "circle") %>% 
     select(-Party); head(candList)
     
othList <- filter(net, Party == "U") %>% 
     #filter(PAC == "") %>% 
     #filter(Candidate == "") %>% 
     select(OrganizationName) %>% distinct() %>% 
     rename(label = OrganizationName) %>% 
     mutate(col = "plum", shp = "circle"); head(othList)


pacList <- net %>% 
     filter(PAC == "Y") %>% 
     select(OrganizationName,Party) %>% 
     distinct() %>% 
     rename(label = OrganizationName) %>% 
     mutate(col = ifelse(Party == "D", "dodgerblue",
                         ifelse(Party == "R", "red", "turquoise3")),
            shp = "diamond") %>% 
     select(-Party); head(pacList)
     
nodeLists <- bind_rows(pacList,candList,gopList,demList,stList,othList)

d$col <- ifelse(d$label %in% demList$label, "dodgerblue",
                ifelse(d$label %in% gopList$label, "red", 
                       ifelse(d$label %in% pacList$label, "yellow",
                              ifelse(d$label %in% othList$label, "purple", "white"))))


d$shp <- ifelse(d$label %in% pacList$label, "star",
                ifelse(d$label %in% stList$label, "rectangle", "circle"))


newNet <- net %>% select(employer_region,OrganizationName,Total) %>% transform(Total = round(Total, 0))


edgeCol <- ifelse(newNet$employer_region == "Rhode\nIsland", "dodgerblue",
                  ifelse(newNet$employer_region == "Massachusetts", "magenta", "green2"))

library(qgraph)

netLab <- monify(newNet$Total)

qgraph(newNet, 
       #layout = "circle",
       vsize = 3, # Size of the nodes
       label.cex = 5, # Size of Node Labels
       layout = "spring",
       mar = c(1,1,1,1),
       title = "RI Political Donations Received by State\nDem (Blue), GOP (red), Other (purple), State (white)\nPAC = star, state = rectangle", 
       edge.color = edgeCol, # Line color
       edge.width = 0.75, 
       #fade = FALSE,
       edge.label.color = "black", # Color of the numeric labels
       bg = "whitesmoke",
       fade = TRUE,
       vTrans = 100,
       color = d$col, 
       shape = d$shp,
       #color = nodeLists$col,
       #color = nList$col,
       #shape = nodeLists$shp,
       label.scale.equal = TRUE,
       edge.label.cex = 0.5,
       edge.label.position = 0.5,
       edge.label.bg = "gray90",
       #edge.labels=TRUE
       edge.labels= netLab
       )




dfx %>% 
     filter(Industry2 == "Healthcare") %>% 
     group_by(Industry, OrganizationName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total))# %>% View("x")


df2 <- dfx %>% 
     mutate(State = ifelse(employer_region == "Rhode Island", "Rhode_Island","Other_State")) %>% 
     group_by(OrganizationName,State) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     spread(State,Total) %>% 
     transform(Rhode_Island = ifelse(is.na(Rhode_Island), 0, Rhode_Island),
               Other_State = ifelse(is.na(Other_State), 0, Other_State)) %>% 
     mutate(Total = Rhode_Island + Other_State) %>% 
     mutate(Pct_Total = round(Rhode_Island / Total, 3)) %>% 
     arrange(desc(Total)); head(df2, 10)




g1 <- dfx %>% 
     filter(OrganizationName %in% topRecipients$OrganizationName) %>% 
     transform(OrganizationName = str_to_title(OrganizationName)) %>% 
     mutate(State = ifelse(donor_region == "Rhode Island", "Rhode Island","Other State")) %>% 
     group_by(OrganizationName,State) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     group_by(OrganizationName) %>% 
     mutate(GrandTot = sum(Total)) %>% ungroup() %>% 
     mutate(Pct_of_Total = round(Total / GrandTot, 3)) %>% 
     ggplot(aes(OrganizationName, Total)) +
     geom_bar(stat = "identity", color = "gray50", width = 0.5, #position = "stack",
              position = "dodge",
              aes(fill = State)) +
     scale_fill_manual(values = c("darkorange","dodgerblue")) +
     scale_y_continuous(label = dollar, breaks = seq(0,2000000,250000)) +
     labs(x = "", y = "Total Donations",
          title = "2017 RI Political Donations by Donor State",
          subtitle = "http://www.ricampaignfinance.com/RIPublic/Filings.aspx") +
     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
           legend.position = "bottom", legend.title = element_blank(), 
           legend.background = element_rect(color = "gray50")); g1


dfx %>% 
     filter(OrganizationName %in% topRecipients$OrganizationName | OrganizationName == "patricia l. morgan" |
                 OrganizationName == "dawn m. euer") %>% 
     transform(OrganizationName = str_to_title(OrganizationName)) %>% 
     group_by(ReceiptDate,OrganizationName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     ggplot(aes(ReceiptDate, Total)) +
     geom_line(stat = "identity", aes(group = OrganizationName, color = OrganizationName)) +
     #geom_point(size = 0.5, aes(color = OrganizationName)) +
     geom_smooth(aes(color = OrganizationName), se = FALSE) +
     scale_y_continuous(label = dollar) +
     labs(x = "Donation Receipt Date", y = "Total",
          title = "RI Political Donations Since Jan 1, 2017",
          subtitle = srce) +
     theme_bw() +
     theme(legend.position = "none", 
           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
     facet_wrap(~OrganizationName, scales = "free_y")



dfx %>% 
     # filter(OrganizationName %in% topRecipients$OrganizationName | OrganizationName == "patricia l. morgan" |
     #             OrganizationName == "dawn m. euer") %>% 
     # transform(OrganizationName = str_to_title(OrganizationName)) %>% 
     group_by(ReceiptDate,donor_region) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     ggplot(aes(ReceiptDate, Total)) +
     geom_line(stat = "identity", aes(group = donor_region, color = donor_region)) +
     geom_point(size = 0.5, aes(color = donor_region)) +
     #geom_point(size = 0.5, aes(color = OrganizationName)) +
     geom_smooth(aes(color = donor_region), se = FALSE) +
     scale_y_continuous(label = dollar) +
     labs(x = "Donation Receipt Date", y = "Total",
          title = "RI Political Donations Since Jan 1, 2017",
          subtitle = srce) +
     theme_bw() +
     theme(legend.position = "none", 
           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
     facet_wrap(~donor_region, scales = "free_y")




dfx %>% 
     # filter(OrganizationName %in% topRecipients$OrganizationName | OrganizationName == "patricia l. morgan" |
     #             OrganizationName == "dawn m. euer") %>% 
     # transform(OrganizationName = str_to_title(OrganizationName)) %>% 
     group_by(ReceiptDate,Industry2) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     ggplot(aes(ReceiptDate, Total)) +
     geom_line(stat = "identity", aes(group = Industry2, color = Industry2)) +
     geom_point(size = 0.5, aes(color = Industry2)) +
     #geom_point(size = 0.5, aes(color = OrganizationName)) +
     geom_smooth(aes(color = Industry2), se = FALSE) +
     scale_y_continuous(label = dollar) +
     labs(x = "Donation Receipt Date", y = "Total",
          title = "RI Political Donations Since Jan 1, 2017",
          subtitle = srce) +
     theme_bw() +
     theme(legend.position = "none", 
           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
     facet_wrap(~Industry2, scales = "free_y")

dfx %>% 
     filter(OrganizationName %in% topRecipients$OrganizationName) %>% 
     filter(Industry2 != "") %>% 
     group_by(OrganizationName, Industry2) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     group_by(OrganizationName) %>% 
     top_n(Total, n = 3) %>% 
     ungroup() %>% 
     ggplot(aes(OrganizationName, Total)) +
     geom_bar(stat = "identity", color = "gray50", width = 0.5, position = "stack",
              #position = "dodge",
              aes(fill = Industry2)) +
     #scale_fill_manual(values = c("darkorange","dodgerblue","limegreen")) +
     scale_y_continuous(label = dollar) +#, breaks = seq(0,2000000,250000)) +
     labs(x = "", y = "Total Donations",
          title = "2017 RI Political Donations by Donor State",
          subtitle = "http://www.ricampaignfinance.com/RIPublic/Filings.aspx") +
     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
           legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(color = "gray50")) 


dfx %>% 
     transform(Industry2 = ifelse(Industry2 == "", "Other", Industry2)) %>% 
     group_by(Industry2) %>% 
     mutate(IndustryTot = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     group_by(Industry2, IndustryTot, employer_region) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>%# View("X")
     mutate(Pct_of_Total = round(Total / IndustryTot, 4)) %>% 
     ggplot(aes(Industry2, employer_region)) +
     geom_tile(color = "gray50", alpha = 0.5, aes(fill = Pct_of_Total)) +
     scale_fill_viridis(label = percent) +
     labs(x = "", y = "State",
          title = "Donations by Industry & State") +
     theme(axis.text.x= element_text(angle = 45, hjust = 1, vjust = 1),
           legend.position = "bottom", legend.background = element_rect(color = "gray50")) +
     coord_flip()



dfx %>% 
     transform(Industry2 = ifelse(Industry2 == "", "Other", Industry2)) %>% 
     group_by(Industry2) %>% 
     mutate(IndustryTot = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     group_by(Industry2, IndustryTot, employer_region) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(IndustryTot), desc(Total)) %>%# View("X")
     mutate(Pct_of_Total = round(Total / IndustryTot, 4)) %>% 
     ggplot(aes(employer_region, Pct_of_Total)) +
     geom_point(size = 4, color = "gray50") + 
     geom_point(size = 3, aes(color = Pct_of_Total)) +
     scale_color_viridis(label = percent) +
     labs(x = "State", y = "Pct of Total", 
          title = "Donations by Industry") +
     scale_y_continuous(label = percent) +
     theme_bw() +
     theme(axis.text.x = element_text(size = 7.5, face = "bold", angle = 45, hjust = 1, vjust = 1),
           strip.text.y = element_text(margin = margin(0.05,0.05,0.05,0.05, "cm"), size = 8),
           legend.position = "bottom", legend.background = element_rect(color = "gray50")) +
     facet_wrap(~Industry2)




dfx %>% 
     transform(Industry2 = ifelse(Industry2 == "", "Other", Industry2)) %>% 
     group_by(Industry2) %>% 
     mutate(IndustryTot = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     group_by(Industry2, IndustryTot, employer_region) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(IndustryTot), desc(Total)) %>%# View("X")
     mutate(Pct_of_Total = round(Total / IndustryTot, 4)) %>% 
     ggplot(aes(employer_region, Pct_of_Total)) +
     geom_point(size = 4, color = "gray50") + 
     geom_point(size = 3, aes(color = employer_region)) +
     #scale_color_viridis(label = percent) +
     labs(x = "", y = "Pct of Total", 
          title = "Donations by Industry") +
     scale_y_continuous(label = percent) +
     theme_bw() +
     theme(axis.text.x = element_text(size = 7.5, face = "bold", angle = 45, hjust = 1, vjust = 1),
           strip.text.x = element_text(margin = margin(0.05,0.05,0.05,0.05, "cm"), size = 8),
           legend.position = "bottom", legend.background = element_rect(color = "gray50"),
           legend.title = element_blank()) +
     facet_wrap(~Industry2)




dfx %>% 
     transform(Industry2 = ifelse(Industry2 == "", "Other", Industry2)) %>% 
     group_by(Industry2) %>% 
     mutate(IndustryTot = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     group_by(Industry2, IndustryTot, employer_region) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(IndustryTot), desc(Total)) %>%# View("X")
     mutate(Pct_of_Total = round(Total / IndustryTot, 4)) %>% 
     ggplot(aes(employer_region, Pct_of_Total)) +
     geom_point(size = 4, color = "gray50") + 
     geom_point(size = 3, aes(color = employer_region)) +
     #scale_color_viridis(label = percent) +
     labs(x = "", y = "Pct of Total", 
          title = "Donations by Industry") +
     scale_y_continuous(label = percent) +
     theme_bw() +
     theme(axis.text.x = element_blank(),
           strip.text.x = element_text(margin = margin(0.05,0.05,0.05,0.05, "cm"), size = 8),
           legend.position = "bottom", legend.background = element_rect(color = "gray50"),
           legend.title = element_blank()) +
     facet_wrap(~Industry2)






dfx %>% 
     filter(OrganizationName %in% topRecipients$OrganizationName | OrganizationName == "patricia l. morgan" #|
                 #OrganizationName == "daniel j mckee" #|
                 #OrganizationName == "dawn m. euer"
            ) %>% 
     transform(Industry2 = ifelse(Industry2 == "", "Other", Industry2)) %>% 
     #filter(Industry2 != "") %>% 
     transform(OrganizationName = str_to_title(OrganizationName)) %>% 
     group_by(OrganizationName, Industry2) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     group_by(OrganizationName) %>% 
     top_n(Total, n = 10) %>% 
     ungroup() %>% 
     ggplot(aes(Industry2, Total)) +
     geom_bar(stat = "identity", color = "gray50", width = 0.5, position = "stack",
              #position = "dodge",
              aes(fill = Industry2)) +
     #scale_fill_manual(values = c("darkorange","dodgerblue","limegreen")) +
     scale_y_continuous(label = dollar) +#, breaks = seq(0,2000000,250000)) +
     labs(x = "", y = "Total Donations",
          title = "2017 RI Political Donations by Industry",
          subtitle = "http://www.ricampaignfinance.com/RIPublic/Filings.aspx") +
     theme_bw() +
     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
           legend.position = "none", legend.title = element_blank(), 
           legend.background = element_rect(color = "gray50")) +
     facet_wrap(~OrganizationName, scales = "free")



dfx %>% 
     filter(OrganizationName == "gina m. raimondo") %>% 
     filter(Industry2 == "Home Maker") %>% 
     group_by(OrganizationName, Industry2, donor_region) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% #View("x")
     # group_by(OrganizationName) %>% 
     # top_n(Total, n = 10) %>% 
     # ungroup() %>% 
     ggplot(aes(reorder(donor_region, Total), Total)) +
     geom_bar(stat = "identity", color = "gray50", width = 0.5, #position = "stack",
              position = "dodge",
              aes(fill = donor_region)) +
     #scale_fill_manual(values = c("darkorange","dodgerblue","limegreen")) +
     scale_y_continuous(label = dollar, breaks = seq(0,60000,10000)) +
     labs(x = "", y = "Total Donations",
          title = "Gov Raimondo 2017 Donations by Homemaker's State",
          subtitle = "http://www.ricampaignfinance.com/RIPublic/Filings.aspx") +
     theme_bw() +
     theme(#axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
           legend.position = "none", legend.title = element_blank(), 
           legend.background = element_rect(color = "gray50")) +
     coord_flip() +
     facet_wrap(~Industry2)




dfx %>% 
     filter(OrganizationName == "gina m. raimondo") %>% 
     filter(Industry2 == "Attorneys & Lawyers") %>% 
     group_by(OrganizationName, Industry2, donor_region) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% #View("x")
     # group_by(OrganizationName) %>% 
     # top_n(Total, n = 10) %>% 
     # ungroup() %>% 
     ggplot(aes(reorder(donor_region, Total), Total)) +
     geom_bar(stat = "identity", color = "gray50", width = 0.5, #position = "stack",
              position = "dodge",
              aes(fill = donor_region)) +
     #scale_fill_manual(values = c("darkorange","dodgerblue","limegreen")) +
     scale_y_continuous(label = dollar, breaks = seq(0,120000,20000)) +
     labs(x = "", y = "Total Donations",
          title = "Gov Raimondo 2017 Donations by Attornerys & Lawyers State",
          subtitle = "http://www.ricampaignfinance.com/RIPublic/Filings.aspx") +
     theme_bw() +
     theme(#axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          legend.position = "none", legend.title = element_blank(), 
          legend.background = element_rect(color = "gray50")) +
     coord_flip() +
     facet_wrap(~Industry2)




dfx %>% 
     filter(OrganizationName == "gina m. raimondo") %>% 
     filter(Industry2 != "") %>% 
     group_by(OrganizationName, Industry2, donor_region) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% #View("x")
     # group_by(OrganizationName) %>% 
     # top_n(Total, n = 10) %>% 
     # ungroup() %>% 
     ggplot(aes(reorder(donor_region, Total), Total)) +
     geom_bar(stat = "identity", color = "gray50", width = 0.5, #position = "stack",
              position = "dodge",
              aes(fill = donor_region)) +
     #scale_fill_manual(values = c("darkorange","dodgerblue","limegreen")) +
     #scale_y_continuous(label = dollar, breaks = seq(0,120000,20000)) +
     labs(x = "", y = "Total Donations",
          title = "Gov Raimondo 2017 Donations by Industry & State",
          subtitle = "http://www.ricampaignfinance.com/RIPublic/Filings.aspx") +
     theme_bw() +
     theme(#axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          legend.position = "none", legend.title = element_blank(), 
          legend.background = element_rect(color = "gray50")) +
     coord_flip() +
     facet_wrap(~Industry2, scales = "free")






nList <- data.frame(c(unique(net$OrganizationName), unique(net$employer_region)), 
                       stringsAsFactors = FALSE) %>% 
     setNames(., ("label")) %>% 
     transform(label = str_wrap(label, 8)) %>% 
     mutate(col = ifelse(label %in% stList$label, "yellow",
                         ifelse(label %in% pacList$label, "white",
                                ifelse(label %in% demList$label, "dodgerblue",
                                       ifelse(label %in% gopList$label, "red","purple")))))





n1 <- head(newNet, 30)


n1List <- data.frame(c(unique(n1$employer_region), unique(n1$OrganizationName)), stringsAsFactors = FALSE) %>% 
     setNames("label") %>% 
     mutate(col = ifelse(label %in% c("Rhode\nIsland","New York"), "yellow", "dodgerblue"))

edgeCol <- ifelse(n1$employer_region == "Rhode\nIsland", "dodgerblue",
                  ifelse(n1$employer_region == "Massachusetts", "magenta", "green2"))

qgraph(n1, 
       color = n1List$col)

qgraph(n1, 
       #layout = "circle",
       vsize = 3, # Size of the nodes
       label.cex = 6, # Size of Node Labels
       layout = "spring",
       mar = c(2,2,2,2),
       title = "RI Political Donations Received by State", 
       #edge.color = edgeCol, # Line color
       edge.width = 0.75, 
       #fade = FALSE,
       edge.label.color = "black", # Color of the numeric labels
       bg = "whitesmoke",
       fade = TRUE,
       vTrans = 100,
       #color = nodeLists$col,
       color = n1List$col,
       #shape = nodeLists$shp,
       label.scale.equal = TRUE,
       edge.label.cex = 0.65,
       edge.label.position = 0.5,
       edge.label.bg = "gray90",
       edge.labels=TRUE)









orgList <- unique(net$OrganizationName)
empList <- unique(net$employer_region)

nodeList <- data.frame(c(unique(dfx$OrganizationName), unique(dfx$employer_region)), 
                       stringsAsFactors = FALSE) %>% 
     setNames("labels") %>% 
     mutate(col = ifelse(labels %in% orgList, "dodgerblue")); str(nodeList)


topIndustries4 <- dfx %>% 
     #group_by(FirstName,LastName) %>% 
     group_by(Industry2,Party,OrganizationName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     #mutate(Dollars = monify(Total)) %>% 
     transform(Party = ifelse(Party == "", "Other", Party)) %>% 
     spread(Party, Total) %>% 
     transform(Other = ifelse(is.na(Other), 0, Other),
               D = ifelse(is.na(D), 0, D),
               R = ifelse(is.na(R), 0, R)) %>% 
     mutate(Total = Other + D + R) %>% 
     arrange(desc(Total)) %>% 
     mutate(Pct_D = round(D / Total, 3),
            Pct_R = round(R / Total, 3))


gina <- filter(dfx, OrganizationName == "gina m. raimondo")

gr <- gina %>% 
     group_by(OrganizationName,Employer,employer_region, Industry2) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE),
               Amount = monify(Total),
               Donors = n_distinct(FullName)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) 

gr2 <- gina %>% 
     group_by(OrganizationName,employer_region,Industry2) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE),
               Amount = monify(Total),
               Donors = n_distinct(FullName)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) 

gr3 <- gina %>% 
     group_by(OrganizationName,employer_region,Industry2) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     transform(employer_region = gsub("\\&", "and", employer_region)) %>% 
     transform(employer_region = gsub("\\s", "_", employer_region)) %>% 
     spread(employer_region,Total)

gr3[is.na(gr3)] <- 0


gr3$Total <- rowSums(gr3[, c(3:13)])


gr4 <- gina %>% 
     group_by(OrganizationName,employer_region) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     transform(employer_region = gsub("\\&", "and", employer_region)) %>% 
     transform(employer_region = gsub("\\s", "_", employer_region)) %>% 
     spread(employer_region,Total)


topOrgs <- dfx %>% 
     #group_by(FirstName,LastName) %>% 
     group_by(OrganizationName,Party,Candidate,PAC) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     mutate(Dollars = monify(Total))



parties <- dfx %>% 
     #group_by(FirstName,LastName) %>% 
     group_by(Party) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     mutate(Dollars = monify(Total))


# noIndustry <- dfx %>% 
#      filter(Industry == "") %>% 
#      group_by(Employer) %>% 
#      summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
#      ungroup() %>% 
#      arrange(desc(Total)) %>% 
#      mutate(Dollars = monify(Total))
# 
# 
# 
# 
# students <- dfx %>% 
#      filter(Employer == "student")


cneLife2 <- filter(dfx, CNE_Life != "Other") %>% 
     group_by(CNE_Life) %>% 
     mutate(DonationsTotal = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     group_by(CNE_Life,OrganizationName,DonationsTotal) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     mutate(Pct_of_Total = round(Total / DonationsTotal, 2)) %>% 
     arrange(desc(DonationsTotal), desc(Pct_of_Total))

cneLife3 <- filter(dfx, CNE_Life != "Other") %>% 
     group_by(CNE_Life,Employer) %>% 
     mutate(DonationsTotal = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     group_by(CNE_Life,Employer,OrganizationName,DonationsTotal) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     mutate(Pct_of_Total = round(Total / DonationsTotal, 2)) %>% 
     arrange(desc(DonationsTotal), desc(Pct_of_Total))


cneLife <- filter(dfx, CNE_Life != "Other") %>% 
     group_by(Donor#,FullName
              ) %>% 
     mutate(DonationsTotal = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     group_by(Donor,#FullName,
              Employer,OrganizationName,DonationsTotal) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     mutate(Pct_of_Total = round(Total / DonationsTotal, 2)) %>% 
     arrange(desc(DonationsTotal), desc(Pct_of_Total)); head(cneLife)


topCNEDonors <- filter(dfx, CNE_Life != "Other") %>% 
     group_by(Donor) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     filter(Total > 299)


topCNEOrgs <- filter(dfx, CNE_Life != "Other") %>% 
     group_by(OrganizationName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     filter(Total > 299.9)


cneLife <- filter(dfx, CNE_Life != "Other") %>% 
     group_by(Donor,CNE_Life,OrganizationName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     filter(OrganizationName %in% topCNEOrgs$OrganizationName & Donor %in% topCNEDonors$Donor) %>% 
     mutate(plotCol = ifelse(CNE_Life == "Lifespan", "violetred", "dodgerblue")) %>% 
     transform(OrganizationName = str_wrap(OrganizationName, 14),
               Donor = str_wrap(Donor, 12)); head(cneLife)

plotCol <- cneLife$plotCol
#plotShp <- cneLife$plotShp

# Lookup table
cne_life <- cneLife %>% select(Donor,CNE_Life) %>% distinct()

# Remove cols
cneLife <- cneLife %>% select(-c(plotCol,#plotShp,
                                 CNE_Life))

# Create a Node List
nodeList1 <- cneLife %>% select(Donor) %>% rename(label = Donor) %>% distinct() %>% mutate(Val = "Donor"); head(nodeList1)

nodeList2 <- cneLife %>% select(OrganizationName) %>% rename(label = OrganizationName) %>% distinct() %>% mutate(Val = "Org"); head(nodeList2)

# Combine
nodeList <- bind_rows(nodeList1, nodeList2) %>% 
     left_join(cne_life, by = c("label" = "Donor")) %>% 
     transform(Val = ifelse(Val == "Donor", CNE_Life, Val)) %>% 
     select(-CNE_Life) %>% 
     mutate(col = ifelse(Val == "Care New England", "lightblue",
                         ifelse(Val == "Lifespan", "lightpink", "lightgreen")),
            shp = ifelse(Val == "Care New England", "circle",
                         ifelse(Val == "Lifespan", "square", "star"))); head(nodeList); rm(nodeList1, nodeList2)

library(qgraph)


plotVal <- monify(round(cneLife$Total, 0))

# No_ESRD_To_From
qgraph(cneLife, 
       mar = c(1,1,1,1),
       layout = "circle",
       #overlay = TRUE, #layout = "groups",#esize = 14, # Thickness of the lines #color = plotCol, #vtrans = 1200, # transparency (0 - 250)
       title = "CNE & Lifespan Political Donations Since Jan 2017",
       vsize = 4, # Size of the nodes
       label.cex = 6, # Size of Node Labels
       #color = "lightblue",shape = plotShp,
       color = nodeList$col,
       shape = nodeList$shp,
       border.color = "gray30", # The color outlining the nodes
       edge.color = plotCol, # Line color
       edge.label.color = "black", # Color of the numeric labels
       bg = "white",
       label.scale.equal = TRUE,
       edge.label.cex = 0.75,
       edge.label.bg = "gray90",
       edge.labels=plotVal)

# No_ESRD_To_From
qgraph(cneLife, 
       mar = c(1,1,1,1),
       layout = "spring",
       #overlay = TRUE, #layout = "groups",#esize = 14, # Thickness of the lines #color = plotCol, #vtrans = 1200, # transparency (0 - 250)
       title = "CNE & Lifespan Political Donations Since Jan 2017\nhttp://www.ricampaignfinance.com/RIPublic/Filings.aspx\nCNE = blue circle, Lifespan = red square, Organization = green star",
       vsize = 2, # Size of the nodes
       label.cex = 10, # Size of Node Labels
       border.color = "gray30", # The color outlining the nodes
       edge.color = plotCol, # Line color
       edge.label.color = "black", # Color of the numeric labels
       bg = "white",
       #color = "lightblue",shape = plotShp,
       color = nodeList$col,
       shape = nodeList$shp,
       label.scale.equal = TRUE,
       edge.label.cex = 0.55,
       edge.label.bg = "gray90",
       edge.labels=plotVal)



# *********************************************************************************************



topCNEDonors2 <- filter(dfx, CNE_Life != "Other") %>% 
     group_by(FullName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     filter(Total > 299)


topCNEOrgs2 <- filter(dfx, CNE_Life != "Other") %>% 
     group_by(OrganizationName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     filter(Total > 299.9)


cneLife2 <- filter(dfx, CNE_Life != "Other") %>% 
     group_by(FullName,CNE_Life,OrganizationName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     filter(OrganizationName %in% topCNEOrgs2$OrganizationName & FullName %in% topCNEDonors2$FullName) %>% 
     mutate(plotCol = ifelse(CNE_Life == "Lifespan", "violetred", "dodgerblue")) %>% 
     transform(OrganizationName = str_wrap(OrganizationName, 14),
               FullName = str_wrap(FullName, 12)); head(cneLife2)

plotCol <- cneLife2$plotCol
#plotShp <- cneLife$plotShp

# Lookup table
cne_life2 <- cneLife2 %>% select(FullName,CNE_Life) %>% distinct()

# Remove cols
cneLife2 <- cneLife2 %>% select(-c(plotCol,#plotShp,
                                 CNE_Life))

# Create a Node List
nodeList1a <- cneLife2 %>% select(FullName) %>% rename(label = FullName) %>% distinct() %>% mutate(Val = "Donor"); head(nodeList1a)

nodeList2a <- cneLife2 %>% select(OrganizationName) %>% rename(label = OrganizationName) %>% distinct() %>% mutate(Val = "Org"); head(nodeList2a)

# Combine
nodeList2 <- bind_rows(nodeList1a, nodeList2a) %>% 
     left_join(cne_life2, by = c("label" = "FullName")) %>% 
     transform(Val = ifelse(Val == "Donor", CNE_Life, Val)) %>% 
     select(-CNE_Life) %>% 
     mutate(col = ifelse(Val == "Care New England", "lightblue",
                         ifelse(Val == "Lifespan", "lightpink", "lightgreen")),
            shp = ifelse(Val == "Care New England", "circle",
                         ifelse(Val == "Lifespan", "square", "star"))); head(nodeList); rm(nodeList1a, nodeList2a)

library(qgraph)


plotVal2 <- monify(round(cneLife2$Total, 0))

# No_ESRD_To_From
qgraph(cneLife2, 
       mar = c(1,1,1,1),
       layout = "circle",
       #overlay = TRUE, #layout = "groups",#esize = 14, # Thickness of the lines #color = plotCol, #vtrans = 1200, # transparency (0 - 250)
       title = "CNE & Lifespan Political Donations Since Jan 2017",
       vsize = 4, # Size of the nodes
       label.cex = 6, # Size of Node Labels
       #color = "lightblue",shape = plotShp,
       color = nodeList2$col,
       shape = nodeList2$shp,
       border.color = "gray30", # The color outlining the nodes
       edge.color = plotCol, # Line color
       edge.label.color = "black", # Color of the numeric labels
       bg = "white",
       label.scale.equal = TRUE,
       edge.label.cex = 0.75,
       edge.label.bg = "gray90",
       edge.labels=plotVal2)

# No_ESRD_To_From
qgraph(cneLife2, 
       mar = c(1,1,1,1),
       layout = "spring",
       #overlay = TRUE, #layout = "groups",#esize = 14, # Thickness of the lines #color = plotCol, #vtrans = 1200, # transparency (0 - 250)
       title = "CNE & Lifespan Political Donations Since Jan 2017\nhttp://www.ricampaignfinance.com/RIPublic/Filings.aspx\nCNE = blue circle, Lifespan = red square, Organization = green star",
       vsize = 2, # Size of the nodes
       label.cex = 10, # Size of Node Labels
       border.color = "gray30", # The color outlining the nodes
       edge.color = plotCol, # Line color
       edge.label.color = "black", # Color of the numeric labels
       bg = "white",
       #color = "lightblue",shape = plotShp,
       color = nodeList2$col,
       shape = nodeList2$shp,
       label.scale.equal = TRUE,
       edge.label.cex = 0.55,
       edge.label.bg = "gray90",
       edge.labels=plotVal2)





# **********************************************************************************

donate <- dfx %>% 
     transform(Employer = strtrim(str_wrap(Employer, 14),27),
               FullName = strtrim(str_wrap(FullName, 14),27),
               OrganizationName = strtrim(str_wrap(OrganizationName, 14),41)) %>% 
     group_by(Employer,
              Industry,
              FullName,
              OrganizationName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     filter(Total >= 5000); head(donate)




nList1 <- donate %>% 
     select(Employer) %>% 
     distinct() %>% 
     rename(label = Employer) %>% 
     mutate(Val = "Employer")

nList2 <- donate %>% 
     select(OrganizationName) %>% 
     distinct() %>% 
     rename(label = OrganizationName) %>% 
     mutate(Val = "Org")


nList <- bind_rows(nList1, nList2) %>% 
     mutate(col = ifelse(Val == "Employer", "lightblue", "lightgreen"),
            shp = ifelse(Val == "Employer", "circle", "square"))


# Clear
rm(nList1, nList2)

donate <- select(donate, FullName, OrganizationName,Total)

# No_ESRD_To_From
qgraph(donate, 
       mar = c(1,1,1,1),
       layout = "spring",
       #overlay = TRUE, #layout = "groups",#esize = 14, # Thickness of the lines #color = plotCol, #vtrans = 1200, # transparency (0 - 250)
       title = "Donations",
       vsize = 2, # Size of the nodes
       label.cex = 12, # Size of Node Labels
       border.color = "gray30", # The color outlining the nodes
       #edge.color = plotCol, # Line color
       edge.label.color = "black", # Color of the numeric labels
       bg = "white",
       #color = "lightblue",shape = plotShp,
       color = nList$col,
       shape = nList$shp,
       label.scale.equal = TRUE,
       edge.label.cex = 0.6,
       edge.label.bg = "gray90",
       edge.labels=TRUE)



adjMatrix <- cneLife %>% 
     spread(OrganizationName, Total)


















grep("vna", dfx$Employer, value = TRUE, ignore.case = TRUE) %>% unique() %>% print


DonorAddress <- dfx %>% 
     group_by(Address, CityStZip) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE),
               To = n_distinct(OrganizationName),
               Donors = n_distinct(FullName),
               Max_Donation = max(Amount, na.rm = TRUE),
               Avg_Donation = mean(Amount, na.rm = TRUE),
               Median_Donation = median(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)); head(DonorAddress)


EmpAddress <- dfx %>% 
     group_by(EmpAddress, EmpCityStZip) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE),
               To = n_distinct(OrganizationName),
               Donors = n_distinct(FullName),
               Max_Donation = max(Amount, na.rm = TRUE),
               Avg_Donation = mean(Amount, na.rm = TRUE),
               Median_Donation = median(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)); head(EmpAddress)



#ex <- grep("matiello", dfx$OrganizationName, value = TRUE, ignore.case = TRUE) %>% unique() %>% print

topEmployers <- dfx %>% 
     group_by(Employer) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE),
               To = n_distinct(OrganizationName),
               Donors = n_distinct(FullName),
               Max_Donation = max(Amount, na.rm = TRUE),
               Avg_Donation = mean(Amount, na.rm = TRUE),
               Median_Donation = median(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total))

topEmployers %>% filter(Total > 999) %>% arrange(desc(Employer)) %>% View("te")


topEmployers <- dfx %>% 
     group_by(EmployerName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE),
               To = n_distinct(OrganizationName),
               Donors = n_distinct(FullName),
               Max_Donation = max(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total))

grep("MA", blues, ignore.case = TRUE)
     

dfx %>% 
     group_by(ContribExplanation) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE),
               To = n_distinct(OrganizationName),
               Donors = n_distinct(FullName),
               Max_Donation = max(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% View("exp")




keefe <- grep("^keefe, d", dfx$FullName, value = TRUE, ignore.case = TRUE) %>% unique()

filter(dfx, FullName %in% keefe) %>% View("Dennis")

topHomemaker <- dfx %>% 
     filter(EmployerName == "homemaker") %>% 
     group_by(FullName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE),
               Donations = n_distinct(ReceiptDate),
               Orgs = n_distinct(OrganizationName)) %>% 
     ungroup() %>% 
     arrange(desc(Total))

topHomemakerTo <- dfx %>% 
     filter(EmployerName == "homemaker") %>% 
     group_by(OrganizationName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE),
               From = n_distinct(FullName),
               Avg_Donation = round(mean(Amount, na.rm = TRUE), 2),
               Median_Donation = round(median(Amount, na.rm = TRUE), 2)) %>% 
     ungroup() %>% 
     arrange(desc(Total))


topOrgs <- dfx %>% 
     group_by(OrganizationName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE),
               From = n_distinct(FullName),
               Avg_Donation = round(mean(Amount, na.rm = TRUE), 2),
               Median_Donation = round(median(Amount, na.rm = TRUE), 2)) %>% 
     ungroup() %>% 
     arrange(desc(Total))


topDonors <- dfx %>% 
     group_by(FullName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE),
               To = n_distinct(OrganizationName),
               Donations = n_distinct(ReceiptDate)) %>% 
     ungroup() %>% 
     arrange(desc(Total))


filter(dfx, FullName ==  "gilbert, william h") %>% View("Huh")

write.csv(df, paste(wd, "donations_Jan17_thru_2018-02-08.csv", sep = ""), na = "", row.names = FALSE) %>% 
     transform(Amount = as.numeric(Amount))
