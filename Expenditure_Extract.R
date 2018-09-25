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


dir <- "//cdsfsp06/InterSite/Integra/analytics/Misc_Projects/Misc/Providence_Open_Data/2018-09-03/Expenditures/"

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
p <- read.csv(paste(dir, "p.csv", sep = ""), colClasses = "character")
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



dir <- "/Users/jeffreyrichardson/Documents/Campaign_Finance/Psephology_App/"


# Read in the latest expenditure data
df <- readRDS(paste(dir, "Expend_thru_August2018.rds", sep = "")) 



dat <- bind_rows(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) %>%
     select(-c(BeginDate,EndDate)) %>% distinct()

# Clear
rm(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z)


exp <- bind_rows(df,dat) %>% 
     transform(ExpDate = mdy(ExpDate),
               OrganizationName = str_to_title(OrganizationName),
               Amount = as.numeric(Amount)) %>% 
     group_by(ExpenditureID) %>% 
     mutate(Tot = sum(Amount)) %>% 
     ungroup() %>% 
     arrange(ExpDate) %>% 
     separate(CityStZip, c("City","stzip"), sep = ",", remove = FALSE, extra = "merge") %>% 
     transform(City = trimws(City), stzip = trimws(stzip)) %>% 
     separate(stzip, c("State","Zip"), sep = "\\s", extra = "merge"); head(exp, 3)



saveRDS(exp, paste(dir, "Expend_thru_Sep03_2018.rds", sep = "")) 

df <- readRDS(paste(dir, "Expend_thru_Sep03_2018.rds", sep = ""))

# 
# # Read in the latest expenditure data
# df <- readRDS(paste(dir, "Expend_thru_August2018.rds", sep = "")) %>% 
#      transform(ExpDate = mdy(ExpDate),
#                OrganizationName = str_to_title(OrganizationName),
#                Amount = as.numeric(Amount)) %>% 
#      group_by(ExpenditureID) %>% 
#      mutate(Tot = sum(Amount)) %>% 
#      ungroup() %>% 
#      arrange(ExpDate) %>% 
#      separate(CityStZip, c("City","stzip"), sep = ",", remove = FALSE, extra = "merge") %>% 
#      transform(City = trimws(City), stzip = trimws(stzip)) %>% 
#      separate(stzip, c("State","Zip"), sep = "\\s", extra = "merge"); head(df, 4)
# 

# dir <- "//cdsfsp06/InterSite/Integra/analytics/Misc_Projects/Misc/Providence_Open_Data/Expenditures/"
# 
# a <- read.csv(paste(dir, "a.csv", sep = ""), colClasses = "character")
# b <- read.csv(paste(dir, "b.csv", sep = ""), colClasses = "character")
# c <- read.csv(paste(dir, "c.csv", sep = ""), colClasses = "character")
# d <- read.csv(paste(dir, "d_quote_to_apost.csv", sep = ""), colClasses = "character")
# e <- read.csv(paste(dir, "e_quote_to_apost.csv", sep = ""), colClasses = "character")
# f <- read.csv(paste(dir, "f.csv", sep = ""), colClasses = "character")
# g <- read.csv(paste(dir, "g_quote_to_apost.csv", sep = ""), colClasses = "character")
# h <- read.csv(paste(dir, "h.csv", sep = ""), colClasses = "character")
# i <- read.csv(paste(dir, "i.csv", sep = ""), colClasses = "character")
# j <- read.csv(paste(dir, "j_quote_to_apost.csv", sep = ""), colClasses = "character")
# k <- read.csv(paste(dir, "k.csv", sep = ""), colClasses = "character")
# l <- read.csv(paste(dir, "l_quote_to_apost.csv", sep = ""), colClasses = "character")
# m <- read.csv(paste(dir, "m.csv", sep = ""), colClasses = "character")
# n <- read.csv(paste(dir, "n.csv", sep = ""), colClasses = "character")
# o <- read.csv(paste(dir, "o_quote_to_apost.csv", sep = ""), colClasses = "character")
# p <- read.csv(paste(dir, "p_quote_to_apost.csv", sep = ""), colClasses = "character")
# q <- read.csv(paste(dir, "q.csv", sep = ""), colClasses = "character")
# r <- read.csv(paste(dir, "r.csv", sep = ""), colClasses = "character")
# s <- read.csv(paste(dir, "s.csv", sep = ""), colClasses = "character")
# t <- read.csv(paste(dir, "t_quote_to_apost.csv", sep = ""), colClasses = "character")
# u <- read.csv(paste(dir, "u.csv", sep = ""), colClasses = "character")
# v <- read.csv(paste(dir, "v.csv", sep = ""), colClasses = "character")
# w <- read.csv(paste(dir, "w.csv", sep = ""), colClasses = "character")
# x <- read.csv(paste(dir, "x.csv", sep = ""), colClasses = "character")
# y <- read.csv(paste(dir, "y.csv", sep = ""), colClasses = "character")
# z <- read.csv(paste(dir, "z.csv", sep = ""), colClasses = "character")
# 
# dat <- bind_rows(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) %>%
#      select(-c(BeginDate,EndDate)) %>% distinct()
# 
# # Clear
# rm(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z)
# 
# dir <- "//cdsfsp06/InterSite/Integra/analytics/Misc_Projects/Misc/Providence_Open_Data/"
# 
# # Save
# saveRDS(dat, paste(dir, "Expend_thru_August2018.rds", sep = ""))
# write.csv(dat, paste(dir, "Expend_thru_August2018.csv", sep = ""), na = "", row.names = FALSE)


dir <- "//cdsfsp06/InterSite/Integra/analytics/Misc_Projects/Misc/Providence_Open_Data/"#Expenditures/"

df <- readRDS(paste(dir, "Expend_thru_August2018.rds", sep = "")) %>% 
     transform(ExpDate = mdy(ExpDate),
               OrganizationName = str_to_title(OrganizationName),
               Amount = as.numeric(Amount)) %>% 
     group_by(ExpenditureID) %>% 
     mutate(Tot = sum(Amount)) %>% 
     ungroup() %>% 
     arrange(ExpDate) %>% 
     separate(CityStZip, c("City","stzip"), sep = ",", remove = FALSE, extra = "merge") %>% 
     transform(City = trimws(City), stzip = trimws(stzip)) %>% 
     separate(stzip, c("State","Zip"), sep = "\\s", extra = "merge"); head(df, 4)


x <- df %>% filter(OrganizationName == "Nicholas Anthony Mattiello") %>% 
     group_by(OrganizationName,FullName,City,State,ExpDesc) %>% 
     summarise(Expenditures = n_distinct(ExpenditureID),
               Total = sum(Amount, na.rm = TRUE),
               Avg = round(Total / Expenditures, 2),
               First = min(ExpDate, na.rm = TRUE),
               Latest = max(ExpDate, na.rm = TRUE)) %>% 
     ungroup() %>% mutate(GrandTot = sum(Total)) %>% 
     mutate(Pct_of_Total = round(Total / GrandTot, 5)) %>% 
     arrange(desc(Total)); head(x, 10)

x <- df %>% 
     group_by(FullName,City,State) %>% 
     summarise(Expenditures = n_distinct(ExpenditureID),
               Total = sum(Amount, na.rm = TRUE),
               Avg = round(Total / Expenditures, 2)) %>% 
     ungroup() %>% 
     arrange(desc(Total)); head(x, 10)


df %>% 
     filter(OrganizationName == "Gina M. Raimondo") %>% 
     group_by(DisbDesc,ExpDate) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     ggplot(aes(ExpDate, Total)) +
     geom_line(stat = "identity", aes(group = DisbDesc, color = DisbDesc)) +
     scale_y_continuous(label = dollar) +
     facet_wrap(~DisbDesc, ncol = 1, scales = "free_y") +
     theme(legend.position = "none") +
     NULL


gina <- df %>% 
     filter(OrganizationName == "Gina M. Raimondo")


gina %>% filter(ExpDate > "2017-01-01") %>% 
     group_by(FullName,DisbDesc,ExpDesc) %>% 
     summarise(Expenditures = n_distinct(ExpenditureID),
               Total = sum(Amount, na.rm = TRUE),
               Avg = round(Total / Expenditures, 2)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% View("x")

gov <- df %>% filter(OrganizationName %in% c("Allan W Fung"
                                             #,"Gina M. Raimondo"
                                             )) %>% 
     group_by(OrganizationName,FullName,DisbDesc,ExpDesc) %>% 
     summarise(#Expenditures = n_distinct(ExpenditureID),
          Total = sum(Amount, na.rm = TRUE)) %>% #,
     #Avg = round(Total / Expenditures, 2)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     spread(ExpDesc,Total); View(gov)


df %>% filter(OrganizationName == "Cyd  Mckenna") %>% 
     group_by(FullName,DisbDesc,ExpDesc) %>% 
     summarise(Expenditures = n_distinct(ExpenditureID),
               Total = sum(Amount, na.rm = TRUE),
               Avg = round(Total / Expenditures, 2)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% View("cyd")


df %>%
     transform(OrganizationName = ifelse(OrganizationName %in% c("Michael Anthony Araujo","Michael  Araujo"), "Michael Araujo", OrganizationName)) %>% 
     filter(OrganizationName %in% c("Cyd  Mckenna","Michael Araujo")) %>% 
     group_by(OrganizationName,FullName,DisbDesc,ExpDesc) %>% 
     summarise(#Expenditures = n_distinct(ExpenditureID),
               Total = sum(Amount, na.rm = TRUE)) %>% #,
               #Avg = round(Total / Expenditures, 2)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) %>% 
     spread(ExpDesc,Total) %>% 
     View("cyd")


gina %>% 
     mutate(DisbMo = format(ExpDate, "%Y-%m")) %>% 
     group_by(DisbDesc,DisbMo) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     ggplot(aes(DisbMo, Total)) +
     geom_point(aes(color = DisbDesc)) +
     geom_line(stat = "identity", aes(group = DisbDesc, color = DisbDesc)) +
     scale_y_continuous(label = dollar) +
     facet_wrap(~DisbDesc, ncol = 1, scales = "free_y") +
     theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
     NULL




x <- df %>% filter(nchar(DisbDesc) < 75) %>% 
     group_by(DisbDesc#,
              #ExpDesc
              ) %>% 
     summarise(Expenditures = n_distinct(ExpenditureID),
               Total = sum(Amount, na.rm = TRUE),
               Avg = round(Expenditures / Total, 2)) %>% 
     ungroup() %>% 
     arrange(desc(Total)); head(x, 20)



x <- df %>% filter(nchar(DisbDesc) < 75) %>% 
     group_by(#DisbDesc#,
              ExpDesc
     ) %>% 
     summarise(Expenditures = n_distinct(ExpenditureID),
               Total = sum(Amount, na.rm = TRUE),
               Avg = round(Expenditures / Total, 2)) %>% 
     ungroup() %>% 
     arrange(desc(Total)); head(x, 20)

z <- df %>% filter(nchar(DisbDesc) > 45) %>% distinct()
z2 <- df %>% filter(nchar(ExpDesc) > 45) %>% distinct()


zz <- z %>% gather(Meaure, Var, OrganizationName:Tot) %>% 
     #filter(nchar(Var) > 20) %>% 
     select(-Measure) %>% 
     distinct() 
     
