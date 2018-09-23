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


xpcost <- function(x) {
     var <- x
     q <- df %>% filter(FullName %in% var) %>% 
          group_by(FullName,ExpDesc,Address,City,State,Zip) %>% 
          summarise(Exps = n_distinct(ExpenditureID),
                    Total = sum(Amount, na.rm = TRUE),
                    First = min(ExpDate),
                    Recent = max(ExpDate)) %>% ungroup() %>% 
          #arrange(desc(Total)) %>% 
          arrange(desc(Address)) %>% 
          distinct()
     
     q2 <- df %>% filter(FullName %in% var) %>% 
          group_by(FullName,ExpDesc,Address,City,State,Zip) %>% 
          summarise(Exps = n_distinct(ExpenditureID),
                    Total = sum(Amount, na.rm = TRUE),
                    First = min(ExpDate),
                    Recent = max(ExpDate)) %>% ungroup() %>% 
          arrange(desc(Total)) %>% 
          #arrange(Address) %>% 
          distinct()
     # Addresses
     theAdds <- q2 %>% filter(Address != "") %>% select(Address) %>% distinct()
     
     q3 <- df %>% filter(Address %in% theAdds) %>% 
          filter(!FullName %in% q2$FullName) %>% 
          select(FullName) %>% 
          distinct() %>% arrange(FullName)
     
     q4 <- df %>% filter(FullName %in% var) %>% 
          group_by(FullName) %>% 
          summarise(Exps = n_distinct(ExpenditureID),
                    Total = sum(Amount, na.rm = TRUE),
                    First = min(ExpDate),
                    Recent = max(ExpDate)) %>% ungroup() %>% 
          #arrange(desc(Total)) %>% 
          arrange(desc(Total))
     
     if(nrow(q) > 0) {
          assign("byTotal", q4, envir = .GlobalEnv)
          assign("byAdd", q, envir = .GlobalEnv)
          assign("byCost", q2, envir = .GlobalEnv)
          #          if(nrow(q3) > 0) {
          assign("Alts", q3, envir = .GlobalEnv)
          #return(head(q,20))
          #         }
          #     } else {
          #         if(nrow(q3) > 0) {
          #              assign("Alts", q3, envir = .GlobalEnv)
     } else {
          assign("Alts", q3, envir = .GlobalEnv)
          return("Nothin")
          
     }
}



xpend <- function(x, add = FALSE) {
     var <- x
     q <- df %>% filter(FullName %in% var) %>% filter(!Address %in% c("","information requested","info requested"))
     qc <- df %>% filter(Address %in% q$Address) %>%
          filter(!FullName %in% q$FullName) %>%
          group_by(FullName) %>%
          summarise(Total = sum(Amount, na.rm = TRUE)) %>% ungroup() %>% arrange(desc(Total)) %>%
          distinct()
     if(nrow(qc) > 0) {
          return(sort(unique(qc$FullName)))
     } else {
          return("Nope, thats it")
     }}

xpOrg <- function(x) {
     var <- x
     q <- df %>% filter(OrganizationName %in% var) %>% 
          group_by(OrganizationName) %>% 
          summarise(Exps = n_distinct(ExpenditureID),
                    Total = sum(Amount, na.rm = TRUE),
                    First = min(ExpDate),
                    Recent = max(ExpDate)) %>% ungroup() %>% 
          distinct()
     if(nrow(q) > 0) {
          return(head(q,20))
     } else {
          return("Nah")
     }}


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


#dir <- "//cdsfsp06/InterSite/Integra/analytics/Misc_Projects/Misc/Providence_Open_Data/"#Expenditures/"
dir <- "/Users/jeffreyrichardson/Documents/Campaign_Finance/Psephology_App/"



# Campaign Finance Expenditures
df <- readRDS(paste(dir, "Expend_thru_Sep18_2018.rds", sep = "")) %>% distinct() %>% 
     transform(#ExpDate = mdy(ExpDate),
               OrganizationName = str_to_title(trimws(OrganizationName)),
               FullName = str_to_lower(trimws(FullName)),
               Amount = as.numeric(Amount)) %>% 
     transform(FullName = gsub(",llc", ", llc", FullName)) %>% 
     transform(FullName = gsub("\\s{2,}", " ", FullName)) %>% 
     transform(FullName = gsub("\\'", "", FullName)) %>% 
     transform(FullName = gsub(" / ", " ", FullName)) %>% 
     transform(FullName = gsub(" - ", " ", FullName)) %>% 
     transform(FullName = gsub(",{2,}", ",", FullName)) %>% 
     transform(FullName = gsub("\\(|\\)", "", FullName)) %>% 
     transform(FullName = gsub("\\,|\\.", "", FullName)) %>% 
     transform(FullName = gsub("\\&", "and", FullName)) %>% 
     transform(FullName = gsub("^fiiends of |^friend of |^frinds of |^f/o|^fo ", "friends of ", FullName)) %>% 
     transform(FullName = gsub("\\s{2,}", " ", FullName)) %>% 
     transform(OrganizationName = gsub("^Ri ", "RI ", OrganizationName)) %>% 
     transform(OrganizationName = gsub(" Ri$", " RI", OrganizationName)) %>% 
     transform(OrganizationName = gsub(" Ri ", " RI ", OrganizationName)) %>% 
     transform(OrganizationName = gsub("\\s{2,}", " ", OrganizationName)) %>% 
     transform(OrganizationName = gsub("\\'", "", OrganizationName)) %>% 
     transform(OrganizationName = gsub(" / ", " ", OrganizationName)) %>% 
     transform(OrganizationName = gsub(" - ", " ", OrganizationName)) %>% 
     transform(OrganizationName = gsub(",{2,}", ",", OrganizationName)) %>% 
     transform(OrganizationName = gsub("\\(|\\)", "", OrganizationName)) %>% 
     transform(OrganizationName = gsub("\\,|\\.", "", OrganizationName)) %>% 
     transform(OrganizationName = gsub("\\&", "and", OrganizationName)) %>% 
     # group_by(ExpenditureID) %>% 
     # mutate(Tot = sum(Amount)) %>% 
     # ungroup() %>% 
     arrange(ExpDate) %>% 
     mutate(CY = year(ExpDate)) %>% 
     separate(CityStZip, c("City","stzip"), sep = ",", remove = FALSE, extra = "merge") %>% 
     transform(City = trimws(City), stzip = trimws(stzip)) %>% 
     separate(stzip, c("State","Zip"), sep = "\\s", extra = "merge") %>% 
     transform(City = str_to_lower(City), State = str_to_upper(State), Address = str_to_lower(Address)); head(df, 4)



df$Address <- gsub(" street$", " st", df$Address)
df$Address <- gsub(" drive$", " dr", df$Address)
df$Address <- gsub(" avenue$", " ave", df$Address)
df$Address <- gsub(" av$", " ave", df$Address)
df$Address <- gsub(" road$", " rd", df$Address)
df$Address <- gsub(" court$", " ct", df$Address)
df$Address <- gsub(" circle$", " cir", df$Address)
df$Address <- gsub(" trail$", " trl", df$Address)
df$Address <- gsub(" lane$", " ln", df$Address)
df$Address <- gsub(" boulevard$", " blvd", df$Address)
df$Address <- gsub(" terrace$", " terr", df$Address)
df$Address <- gsub(" ter$", " terr", df$Address)
df$Address <- gsub(" parkway$", " pkwy", df$Address)
df$Address <- gsub(" place$", " pl", df$Address)
df$Address <- gsub(" highway$", " hwy", df$Address)
df$Address <- gsub(" turnpike$", " tpke", df$Address)


# # Format RI Cities
df$City <- gsub("\\-[0-9]{1,}", "", df$City); head(df$City)
df$City <- gsub(", [a-zA-Z]{2}$", "", df$City); head(df$City)
df$City <- gsub(",|\\.|\\/|\\;|'|`|\\:|_|\\?|\\[|\\*|\\\t|\\|\\^|\\-{1,}", " ", df$City); head(df$City)
df$City <- gsub("\\", "", df$City, fixed = TRUE); head(df$City)
df$City <- gsub("\\s{2,}", " ", df$City)
df$City <- gsub(" ri$", "", df$City)
df$City <- trimws(df$City)
df$City <- gsub("^n a$", "na", df$City)
df$City <- gsub("^e |^ea |^eat |^ease |^easty |^easr |^esat |^est |^eart |^eas |^easst |^eeast |^eats |^eaat |^eaast |^rast ", "east ", df$City)
df$City <- gsub("^w |^w est |^weat |^werst |^wet |^wwae |^wast |^wesk |^westw ", "west ", df$City)
df$City <- gsub("^s |^so |^ssouth |^sout ", "south ", df$City)
df$City <- gsub("^n |^orth |^no |^norht |^norh |^norrth |^nothr |^nor |^noerh |^niorth |^noirth |^norrh |^nort |^noth |^nroth ", "north ", df$City)
df$City <- gsub("^ashway$|^ahaway$|^asaway$|^shaway$", "ashaway", df$City)
df$City <- gsub("bsrrington|barrintgon|brarrington|barringtion|barrinngton| barriington|brrington|barringtonq|barringon$|barringtons|barringron$|barringtron|barringston|barrngton$|barringto$|barrrington|barringtom|barrignton$|^barr$|bwrrington|barringtonton|barringtontonton|west barrington|barringtn|barringotn|^barring$|barington|barrintong|barrinton|bafrrington", "barrington", df$City)
df$City <- gsub("blockisland|^black island$|^bi$|blcok island|new shoreham|^bock island", "block island", df$City)
df$City <- gsub("bristal|^bristo$|bristool|bristrol|briston|pristol$|^britol$|brsitol|brisdtol|brsitol|^brisol$|brtistol|brisotl", "bristol", df$City)
df$City <- gsub("burrillvile|burrilville|buriville|burriville|burillville", "burrillville", df$City)
df$City <- gsub("centerdale", "centredale", df$City)
df$City <- gsub("cranstn|ceranston|cranstpn|edgewood|key 9944 this was|^ccranston$|ceanston|crancton|carnton|crnanston|edgewood|crnston|cranst0n|ctranston|cranstin|cranstonn|cranstonr|craston|^cran$|canston|^ransron$|carnston|cxranston|cvranston|cranson|cranstonq|^crnt$|crnaston|cranstone|cranaton|craanston|cranston ri|crasnton|crantson|^cra$|cransto$|cranton|cranstron|cransrton", "cranston", df$City)
df$City <- gsub("central falss|centrla falls|centeral falls|centra falls|centrals fall|crntral falls|central fall$|central fallss|centrall falls|cental falls|^cf$", "central falls", df$City)
df$City <- gsub("chjarlestown|chralestown|charlseton|charlsetown|chaerlestown|charlestpwn|chaleston|chalestown|charlesown|charlesrtown|charlstown|chalrestown|charleston|charletown|charlestwon", "charlestown", df$City)
df$City <- gsub("chepechat|chpachet|chepchet|chepahet|chpaqchet|chepatchet|chepachrt|chepachwt|cheachet|cheepachet|chapachet|chepachat|chepcahet", "chepachet", df$City)
df$City <- gsub("covertry|coventing|converty|convetry|coventy|coverntry|covnetry|cobentry|conventry|coventr$", "coventry", df$City)
df$City <- gsub("cumberlad|cuberland|cumberalnd|cumbrland|cumberl and|cumebrland|cumberlan$|cumerland|cu berland|cumberlandr|cumbland|cumbeland|cumberlandq|cumberlane|cumberlnad|cumberlnd|cumblerland|cumbderland|cumbreland", "cumberland", df$City)
df$City <- gsub("^eg$|east green$|125 westfield rd|^e$|eastgreenwich|^greenwich$|^149 overfield road$", "east greenwich", df$City)
df$City <- gsub("^ep$|^east pro$|^eprovidence|^02916$|^eprov$", "east providence", df$City)
df$City <- gsub("^exter$|^exteter$|^exiter$|^exet$|^exeer$|^exdter$|exerter$", "exeter", df$City)
df$City <- gsub("fiskville", "fiskeville", df$City)
df$City <- gsub("fosrter$|foster city|^fister$|^forster$|^faster$", "foster", df$City)
df$City <- gsub("forsetdale", "forestdale", df$City)
df$City <- gsub("greenich|grewnwich|greendwich|greenwwich|ngreenwich|greenwihc|greenwichq|greenwih$|greemwich|grennwich|geeenwich|greewnich|greenwixh$|greenwiclle|greenwish$|greenwick|greebwich|gfreenwich|grenwich|geenwich|greenwhich|greewnwich|greewich|greeniwch|greenwiich|greeenwich|greenwichri|grrenwich", "greenwich", df$City)
df$City <- gsub("^green$", "greene", df$City)
df$City <- gsub("greenvilled|grennville|greenwille|cgreenville|greenwiclle|greenvkille|greenvill$|grenville|greenvile|^geenville$|greemville", "greenville", df$City)
df$City <- gsub("glocester|glcoester|gloster|^clocester$|glouster|golcester|glocetste|glochester", "gloucester", df$City)
df$City <- gsub("^greenwich$", "east greenwich", df$City)
df$City <- gsub("^harmon$|^harmoney$", "harmony", df$City)
df$City <- gsub("hopkington", "hopkinton", df$City)
df$City <- gsub("hhope|hooe|hope valley|hope vally|hopevalley|^hpoe$", "hope", df$City)
df$City <- gsub("harrissville|harrisvile|harrrisville|harriville", "harrisville", df$City)
df$City <- gsub("jamestowrn|jamestworn|jametown|jamesown|^james$|jamestrown|jamestwon|jamesetown|jamesstown|janestown|^jameston$|jamestonw|jamestownq", "jamestown", df$City)
df$City <- gsub("jct$|junchion|junctions$|junstion", "junction", df$City)
df$City <- gsub("johnstpn|johmston|jihnston|johsnston|^jstn$|josnton|johynston|johnstonb|jonnston|johnstron|hohnston|johnson|johsnton|johston|johnsotn|johnstoin|jonston|jphnston|johnstonq|jpohnston", "johnston", df$City)
df$City <- gsub("kingstowni|kongstown|kiongstown|knigstown|kignston|kinsgstown|kingsiown|kingstownq|kinsgtown|kingsown|kingstowon|kungstown|kingsrown|kingstpwm|kingswtown|kingstaown|kingstoen|kinkstown|ki8ngstown|kingstowm|kinngstown|kingstwon|kinsgston|kignstown|kingstonw|kinstown|kingstgown|dingstown|kingtown|kings$|kinghstown|kingstow$|kingdtown", "kingstown", df$City)
df$City <- gsub("linccoln|^20 steeple lane$|^incoln$|kincoln|linciln|lincoiln|lincolnc|lincolne|lincolnq|linconl|loncoln|linocln|lincioln|linoln|linclon|^lincol$|llincoln|licoln|liincoln|^lincon$|lincolnn", "lincoln", df$City)
df$City <- gsub("little commons|little comption|little comptom|02837|little comton|little comtpon|little copton", "little compton", df$City)
df$City <- gsub("mmanville", "manville", df$City)
df$City <- gsub("mapoleville|mappleville", "mapleville", df$City)
df$City <- gsub("matunick", "matunuck", df$City)
df$City <- gsub("midddletown|middetown|middfletown|middleown|middletowh|middlettown|middletwon|middeltown|middleotwn|middltown|midletown|middlet0wn|middleton", "middletown", df$City)
df$City <- gsub("^narragansettt|^narraganasett$|narragasett$|narrasgansett$|narraganestt$|narragasnsett$|narrragansett$|narraganett$|narragensett$|narragnasett$|narragansettt$|narragnsett$|narragansettte|narrangensett$|nargansett$|narraganstt$|narrangnsett$|narraganseet$|narrangsett$|narransett$|narraganset$|naragansett|narrasansett|narrgansett|narraganssett$|^narr$|^narra$|narrangansett$|narragannsett$|narrangasett$", "narragansett", df$City)
df$City <- gsub("neweport|newpart|^newort$|goat island|^new port$|wewport|west newport|newporft|newportq|newpot|newprot|newprt|newpoer", "newport", df$City)
df$City <- gsub("north kinston|north kingstown north kingstown|02852|north k$|north kingston|^nk$|^kingaton$|30 pojac point rd", "north kingstown", df$City)
df$City <- gsub("northscituate", "north scituate", df$City)
df$City <- gsub("pawtrucket|p0awtucket|pawtuicket|pqwtucket|pawtcket|pawtucketq|patwucket|pawtucklet|pawtcuet|pawtucdket|pawtuckeet|pwtucket|pawtcuket|pawtuckett|pawutcket|pawticket|pawtuckt|pawtudcket|pawtucet|patucket|paqwtucket|pawtucke$|pawtuket|pawtuvcket|pawtucker|pawrucket|^pawt$|pawucket", "pawtucket", df$City)
df$City <- gsub("portsmough|porthmouth|postsmouth|postsmouthc|porthsmouth|portmouth|prortsmouth|portsmith|posrtsmouth|porstmouth|porstsmouth|portsmoth|portsouth|poutsmouth|protsmouth", "portsmouth", df$City)
df$City <- gsub("quonset point", "quonset", df$City)
df$City <- gsub("^pacoag$|pascaog$|^pascog$|^pasqoag$", "pascoag", df$City)
df$City <- gsub("peace dale", "peacedale", df$City)
df$City[df$City == "pr" & df$emp_st == "ri"] <- "providence"
df$City <- gsub("providencee|providemce|pr9ovidence|peovidence|21 george street|^p\\{ rov|provvidence|^smith st providence$|68 kennedy plaza providence|259 benefit street|235 promenade st providence|perovidence|providencew|02903|rpovidence|providnece|provdience|prov$|providenc$|providenece|ptovidence|provicence|providene$|provisdence|proviidence|provicdence|proividence|procidence|prfovidence|prob$", "providence", df$City)
df$City <- gsub("provideence|providemce|representative|provedence|^pro vidence$|^pvd|providsence|prodence|pprovidence|parovidence|pr0vidence|^rovidence|provcidence|p0rovidence|prviddence$|providwence$|providennce$|providencre$|providence02903$|providencce$|provfidende$|providnce$|providenfce$|providenceri$|providenc e$|provdince$|proivdence$|prividence$|pprovidenc$|provivdence$|providenec$|providencer$|providence ri 2903$|providecne|provdence|proidence$|providenxe$|providende$|providenceq$|providence ri$|providece$|provd$|^prof$|povidence$|priovidence$|provience$|providenve$|providendce$|providenceence$|^provide$|procvidence$|prinvidence$|prvidence$|proviedence$|providencve$|providenced$|providencde$|^provi$|prov idence$|^proc$|porvidence$|porovidence$", "providence", df$City)
df$City <- gsub("preovidence|^providen$|smith st providence|probvidence|235 promenade st providence|south main st providence|15 humboldt|management corp|1165 n main|94 meeting st|8 blackstone blvd|1 citizens|68 kennedy plaza providence|^pro$|providencece|^ovidence$|providencw|providencw|providnence$|providenceprovidence$|proviednce$|provifence$|providcence$|provvidenc$|providencxe|providencd$|providcence$|ph providence$|providenxce$|p rovidence$| peov$|providernce|providewnce$| proiv$| prov1417$|providenvce$|orovidence$|providenxwe$|providence r$|provience$|provience|proviedence$|providencde$|providece$|providnce$|providenec$|proivdence$|providende$|proidence$", "providence", df$City)
df$City <- gsub("^np$|^northprovidence$|^nprovidence$|dorrance street|^noprovidence$|north prov senate|also 26 countryside dr", "north providence", df$City)
df$City <- gsub("riveside|^iverside$|riverisde|riversice|riversidse|riversie$", "riverside", df$City)
df$City <- gsub("rehobeth", "rehoboth", df$City)
df$City <- gsub("rumfordq|rimford|rumfod|02916|runford|^ruford$|rumfoed|^rumord$", "rumford", df$City)
df$City <- gsub("^same as bove$|^sameas above", "same as above", df$City)
df$City <- gsub("sanfrancisco", "san francisco", df$City)
df$City <- gsub("saunderstownri|saundastown|saunderstoqwnri|sdaunderstown|saunderstoen|^saunderstwon$|^sunderstown$|^sndstwn$|suanderstown|shanderstown|sounderstown|saundestown|sauponstown|saunderstonw|saunderstorn|saunderstow$|saunderstown rd|saudnerstown|saundersown|saunderatown|sauderstown|saundertown", "saunderstown", df$City)
df$City <- gsub("^scituae|^scitute|west scituate|^sctituate|^sctuate|^scituae|^scituater|scit$|situate|scituite|schituate|sciutate|scituateq|scitaue|scitaute", "scituate", df$City)
df$City <- gsub("slaterville|slattersville", "slatersville", df$City)
df$City <- gsub("smithfeld|smiithfield|smitthfield|^02917$|smthfield|smithfileld|smirthfield|smitfield|smithgfield|smithfild|smighfield|smighfiels|smith field$|smithfied|smithield|smithfiel$|smithfiled|smihfield|smithifeld|sithfiel|smithfeild|smtihfield", "smithfield", df$City)
df$City <- gsub("south kingstown|^sk$|^wk$|^kignston|^kingsto$|552 kingstown ro|^kinston$|^kingstown$|^kingston$|^kngston$", "south kingston", df$City)
df$City <- gsub("tivertson|02878", "tiverton", df$City)
df$City <- gsub("uunknown|unk$|searching|not applicable|anywhere|^tbd$|unknown at this time|^co$|north office location|27 cornell rd$|various|requested|unkown|don t know|info requested|information requested|investigating|^x$|^xx$|^x{1,} unknown$|obtaining info|^na$|^none$|^n$|awaiting info", "unknown", df$City)
df$City <- gsub("warrwn|^warrren$|^warrent$|^waren$|warrein|^warrem$|^warrenn$|^warrne$|^wareen$", "warren", df$City)
df$City <- gsub("^warrwn$|^warrren$|^warr$|^warrent$|^waren$|warrein|^warrem$|^warrenn$|^warrne$|^wareen$", "warren", df$City)
df$City <- gsub("wakdefield|wakelfield|wakefieldddddd|^wakefileld$|wakefeild|wakrefield|wakwfield|wakefiels|wakefiield|wakefiled|wakefiedl|wakefirld|wakfield|wakefieeld", "wakefield", df$City)
df$City <- gsub("wzarwick|warwickq|warowck$|warciwk|waewick|^war$|warwixck|wqartwick|warwickl$|cowesett|^abc$|warick$|warwickk|wariwkc|warwcick|^warrick$|warwcik|wrwick|wrawick|waerwick|warwic$|wrwick|wrawick|warwuck|warwck|warweick|warwivk|wawick|watwick|warwik|warkwick|wariwick|warrwick|warwiick|warwicvk|watrwick|warwich|wariwck|^warick$|^warwic$|warwickj|was 144 bignall st war|wawrick|warwickd", "warwick", df$City)
df$City <- gsub("west kinston|west kingstown|west kngston|west kingston", "south kingston", df$City)
df$City <- gsub("waesterly|westerley|^westerl$|westrerly|wester y$|westerlt$|westery$|weterly$|weserly$|wwesterly$|^westrerly$|westerly ri \\(state", "westerly", df$City)
df$City <- gsub("^wg$|westgreenwich", "west greenwich", df$City)
df$City <- gsub("waet warwick|west war$|^ww$|^wwarwick$|westwarwick|west w$|02893|east warwick|^wwae warwick$", "west warwick", df$City)
df$City <- gsub("woming|wynoming|^wyoning$", "wyoming", df$City)
df$City <- gsub("woodriver", "wood river", df$City)
df$City <- gsub("wood junction", "wood river junction", df$City)
df$City <- gsub("wickfor$", "wickford", df$City)
df$City <- gsub("wonnsocket|^woonsoket$|woonsocett|120 count st woon|^woonsockt$|woonsokcet|soonsocket|woomsocket|woonsocketq|woonsockrt|^woon$|wonsocket|woobsocket|woondocket|woosocket|woonscket|woodsocket|wioonsocket|woonsocke$|woonsockett|wooksocket", "woonsocket", df$City)
df$City <- gsub("woonsocett|wonnsocket|^woonsoket$|^woonsockt$|woonsokcet|soonsocket|woomsocket|woonsocketq|woonsockrt|^woon$|wonsocket|woobsocket|woondocket|woosocket|woonscket|woodsocket|wioonsocket|woonsocke$|woonsockett|wooksocket", "woonsocket", df$City)
df$City <- gsub("ttt", "tt", df$City)
df$City <- gsub("wakefie d$|wakefiekd|wakelfield|wakefield rd|^south$|wawkefield|wakdefield|wakefieldddddd|^wakefileld$|wakefeild|wakrefield|wakwfield|wakefiels|wakefiield|wakefiled|wakefiedl|wakefirld|wakfield|wakefieeld", "wakefield", df$City)
df$City <- gsub("warwicik|warciwk|warwickq|warowck|warwwick|wareick|wzarwick|waewick|^war$|warwixck|wqartwick|warwickl$|warick$|warwickk|wariwkc|warwcick|^warrick$|warwcik|wrwick|wrawick|waerwick|warwic$|wrwick|wrawick|warwuck|warwck|warweick|warwivk|wawick|watwick|warwik|warkwick|wariwick|warrwick|warwiick|warwicvk|watrwick|warwich|wariwck|^warick$|^warwic$|warwickj|was 144 bignall st war|wawrick|warwickd", "warwick", df$City)
df$City <- gsub("west kingstown|west kngston|west kingston", "south kingston", df$City)
df$City <- gsub("waesterly|westerley|^westerl$|westrerly|shelter harbor|wester y$|westerlt$|westery$|weterly$|weserly$|wwesterly$|^westrerly$|westerly ri \\(state", "westerly", df$City)
df$City <- gsub("^wickfor$", "wickford", df$City)
df$City <- gsub("^wg$|westgreenwich", "west greenwich", df$City)
df$City <- gsub("^ww$|west arwick|west war$|^wes$|^wwarwick$|westwarwick|west w$|east warwick|^wwae warwick$|02893", "west warwick", df$City)
df$City <- gsub("woming|wynoming|^wyoning$", "wyoming", df$City)
df$City <- gsub("woodriver", "wood river", df$City)
df$City <- gsub("watxh hill", "westerly", df$City)
df$City <- gsub("wood junction", "wood river junction", df$City)
df$City <- gsub("bsrrington|barringtion|130 ferry ln|brarrington|barrinngton|barrintgon|barriington|swanseabarrington|^carrington|brrington|barringtonq|barringon$|barringtons|barringron$|barringtron|barringston|barrngton$|barringto$|barrrington|barringtom|barrignton$|^barr$|bwrrington|barringtonton|barringtontonton|west barrington|barringtn|barringotn|^barring$|barington|barrintong|barrinton|bafrrington", "barrington", df$City)
df$City <- gsub("^bi$|blockisland|black island|blcok island|new shoreham|^bock island", "block island", df$City)
df$City <- gsub("brisotl|^brstol$|bristal$|^bristo$|^bistol$|bristool|bristrol|briston|pristol$|^britol$|brsitol|brisdtol|brsitol|^brisol$|brtistol", "bristol", df$City)
df$City <- gsub("buriville|burriville|burillville|burrilville|burrillvile", "burrillville", df$City)
df$City <- gsub("centerdale", "centredale", df$City)
df$City <- gsub("crenaston|^cranron$|02921|edgewood|cranstn|ceranston|ceanston|cransotn|cranstpn|^ccranston$|crancton|carnton|crnanston|crnston|cranst0n|ctranston|cranstin|cranstonn|cranstonr|craston|^cran$|canston|^ransron$|carnston|cxranston|cvranston|cranson|cranstonq|^crnt$|crnaston|cranstone|cranaton|craanston|cranston ri|crasnton|crantson|^cra$|cransto$|cranton|cranstron|cransrton", "cranston", df$City)
df$City <- gsub("centeral falls|central falss|centrla falls|centra falls|centrals fall|crntral falls|central fall$|central fallss|centrall falls|cental falls|^cf$", "central falls", df$City)
df$City <- gsub("charelstown|chjarlestown|chralestown|chrlestown|charlseton|charlsetown|charlestownw|chaerlestown|charlestpwn|chaleston|chalestown|charlesown|charlesrtown|charlstown|chalrestown|charleston|charletown|charlestwon", "charlestown", df$City)
df$City <- gsub("chepchet|chepahet|chapatchet|chpaqchet|chpachet|chepachaet|chepacet|chepatchet|chepachrt|chepachwt|cheachet|cheepachet|chapachet|chepachat|chepcahet", "chepachet", df$City)
df$City <- gsub("claville|clayvile|claville", "clayville", df$City)
df$City <- gsub("covenry|^covetry$|coventing|coventery|covertry|converty|convetry|coventy|coverntry|covnetry|cobentry|conventry|coventr$", "coventry", df$City)
df$City <- gsub("cumberlandnd|cumberand|02864|cmberland|cumbelrand|ciumberland|cumberlandnd|cumberlad|cuberland|^cu$|cumberalnd|cumbrland|cumberl and|cumebrland|cumberlan$|cumerland|cu berland|cumberlandr|cumbland|cumbeland|cumberlandq|cumberlane|cumberlnad|cumberlnd|cumblerland|cumbderland|cumbreland", "cumberland", df$City)
df$City <- gsub("^eg$|east reenwich|75 tipping rick dr|east green$|125 westfield rd|eastgreenwich|^greenwich$|^149 overfield road$", "east greenwich", df$City)
df$City <- gsub("east pro$|east prov$|^east prob$|^ep$|^eprovidence$|^02916$|^eprov$", "east providence", df$City)
df$City <- gsub("exerter|^exter$|^exteter$|^exiter$|^exet$|^exeer$|^exdter$", "exeter", df$City)
df$City <- gsub("fiskville", "fiskeville", df$City)
df$City <- gsub("fosrter$|foster city|foster center|^fister$|^forster$|^faster$", "foster", df$City)
df$City <- gsub("forsetdale", "forestdale", df$City)
df$City <- gsub("gelndale", "glendale", df$City)
df$City <- gsub("grewnwich|greenwicb|greendwich|^greenich$|greenwwich|ngreenwich|greenwihc|greenwichq|greenwih$|greemwich|grennwich|geeenwich|greewnich|greenwixh$|greenwiclle|greenwish$|greenwick|greebwich|gfreenwich|grenwich|geenwich|greenwhich|greewnwich|greewich|greeniwch|greenwiich|greeenwich|greenwichri|grrenwich", "greenwich", df$City)
df$City <- gsub("^green$|^grenne$", "greene", df$City)
df$City <- gsub("greeville|greenvilled|grennville|greenwille|cgreenville|greenwiclle|greenvkille|greenvill$|grenville|greenvile|^geenville$|greemville", "greenville", df$City)
df$City <- gsub("glcoester|glocester|gloster|^clocester$|glouster|golcester|glocetste|glochester", "gloucester", df$City)
df$City <- gsub("^greenwich$", "east greenwich", df$City)
df$City <- gsub("^harmon$|^harmoney$", "harmony", df$City)
df$City <- gsub("hopkington", "hopkinton", df$City)
df$City <- gsub("hopevalley|hope valey|hooe|hope valley|hhope|hope vally$", "hope", df$City)
df$City <- gsub("harriville|harisville|harrisvillle|harrissville|harrrisville", "harrisville", df$City)
df$City <- gsub("jmestown|jamestowrn|jamestworn|jametown|jamesown|^james$|jamestrown|jamestwon|jamesetown|jamesstown|janestown|^jameston$|jamestonw|jamestownq", "jamestown", df$City)
df$City <- gsub("jct$|junchion|junctions$|junstion", "junction", df$City)
df$City <- gsub("johnaston|jihnston|johsnston|josnton|johynston|johnstonb|jonnston|johnstron|hohnston|johnson|johsnton|johston|johnsotn|johnstoin|jonston|jphnston|johnstonq|jpohnston", "johnston", df$City)
df$City <- gsub("kingtstown|kuingston|kngstown$|kindgtown|kintstown|kignston|kinsgstown|kingsiown|kingstownq|kinsgtown|kingsown|kingstowon|kungstown|kingsrown|kingstpwm|kingswtown|kingstaown|kingstoen|kinkstown|ki8ngstown|kingstowm|kinngstown|kingstwon|kinsgston|kignstown|kingstonw|kinstown|kingstgown|dingstown|kingtown|kings$|kinghstown|kingstow$|kingdtown", "kingstown", df$City)
df$City <- gsub("lincolin|limcoln|lincolln|linccoln|^20 steeple lane$|^incoln$|kincoln|linciln|lincoiln|lincolnc|lincolne|lincolnq|linconl|loncoln|linocln|lincioln|linoln|linclon|^lincol$|llincoln|licoln|liincoln|^lincon$|lincolnn", "lincoln", df$City)
df$City <- gsub("little commons|little comption|little comptom|02837|little comton|little comtpon|little copton", "little compton", df$City)
df$City <- gsub("mmanville|^maville$", "manville", df$City)
df$City <- gsub("mapoleville|mappleville", "mapleville", df$City)
df$City <- gsub("matunick|east matunuck|natunick|matunuk", "matunuck", df$City)
df$City <- gsub("midlletown|middletow$|midddletown|^m$|middetown|middfletown|middleown|middletowh|middlettown|middletwon|middeltown|middleotwn|middltown|midletown|middlet0wn|middleton", "middletown", df$City)
#df$City <- gsub("^narragansettt$|^narraganasett$|narragasett|narrasgansett|narraganestt|narragasnsett|narrragansett|narraganett|narragensett|narragnasett|narragansettt|narragnsett|narragansettte|narrangensett|nargansett|narraganstt|narrangnsett|narraganseet|narrangsett|narransett|narraganset|naragansett|narrasansett|narrgansett|narraganssett|^narr$|^narra$|narrangansett|narragannsett|narrangasett", "narragansett", df$City)
df$City <- gsub("narragabsett|narragqansett|narraansett|narrahansett|^narragansettte$|narragansette$|^narrangensett$|^nargansett$|^narraganstt$|^narrangnsett$|^narraganseet$|^narrangsett$|^narransett|^narraganset$|naragansett$|^narrasansett$|^narrgansett$|^narraganssett$|^narr$|^narra$|^narrangansett$|^narragannsett$|^narrangasett$", "narragansett", df$City)
df$City <- gsub("^narragansettt$|^narraganasett$|^narragasett$|^narrasgansett$|^narraganestt$|^narragasnsett$|^narrragansett$|^narraganett|^narragensett$|^narragnasett$|^narragansettt$|^narragnsett", "narragansett", df$City)
df$City <- gsub("neweport|newpart|^new$|^new port$|wewport|west newport|newporft|newportq|newpot|newprot|newprt|newpoer", "newport", df$City)
df$City <- gsub("north kinston$|^north kingstowni|^north kingatown|north kingstown north kingstown|02852|north k$|north kingston|^nk$|^kingaton$|30 pojac point rd", "north kingstown", df$City)
df$City <- gsub("northscituate", "north scituate", df$City)
df$City <- gsub("pawtuckdet|pastucket|pawacket|pantucket|pawtukcet|paawtucket|p0awtucket|pqwtucket|pawtcket|pawtucketq|patwucket|pawtucklet|pawtcuet|pawtucdket|pawtuckeet|pwtucket|pawtcuket|pawtuckett|pawutcket|pawticket|pawtuckt|pawtudcket|pawtucet|patucket|paqwtucket|pawtucke$|pawtuket|pawtuvcket|pawtucker|pawrucket|^pawt$|pawucket", "pawtucket", df$City)
df$City <- gsub("portsmoiuth|portsmlouth|portsmoouth|potsmouth|pottsmouth|portsmoujth|portsmouoth|portsmouh|portsmough$|porsmouth$|portsmourth|porthmouth|postsmouth|postsmouthc|porthsmouth|portmouth|prortsmouth|portsmith|posrtsmouth|porstmouth|porstsmouth|portsmoth|portsouth|poutsmouth|protsmouth", "portsmouth", df$City)
df$City <- gsub("quonset point", "quonset", df$City)
df$City <- gsub("^pacoag$|pascaog$|^pascog$|^pasqoag$", "pascoag", df$City)
df$City <- gsub("peace dale|peace pike", "peacedale", df$City)
df$City <- gsub("dorrance street$|provvidence|perovidence|providencew|02903|rpovidence|providnece|provdience|prov$|providenc$|providenece|ptovidence|provicence|providene$|provisdence|proviidence|provicdence|proividence|procidence|prfovidence|prob$", "providence", df$City)
df$City <- gsub("provbidence|providence1|providence woburn|prrovidence|prpvidence|proveidence|pronvidence|provedence|providdence|providsence|pronvidence|proficende$|paovidence|pr9ovidence|providencei$|preovidence|^pvd$|peovidence|^providen$|proviidence| providen$|provdience|providnece$|providene$|parovidence|pr0vidence|^rovidence|provcidence|p0rovidence|prviddence$|providwence$|providennce$|providencre$|providence02903$|providencce$|provfidende$|providnce$|providenfce$|providenceri$|providenc e$|provdince$|proivdence$|prividence$|pprovidenc$|provivdence$|providenec$|providencer$|providence ri 2903$|providecne|provdence|proidence$|providenxe$|providende$|providenceq$|providence ri$|providece$|provd$|^prof$|povidence$|priovidence$|provience$|providenve$|providendce$|providenceence$|^provide$|procvidence$|prinvidence$|prvidence$|proviedence$|providencve$|providenced$|providencde$|^provi$|prov idence$|^proc$|porvidence$|porovidence$", "providence", df$City)
df$City <- gsub("^providene$|broad street|procidence|ptovidence|rprovidence$|^providenc$|probvidence|15 humboldt|management corp|1165 n main|94 meeting st|8 blackstone blvd|1 citizens|68 kennedy plaza providence|^pro$|providencece|^ovidence$|providencw|providencw|providnence$|providenceprovidence$|proviednce$|provifence$|providcence$|provvidenc$|providencxe|providencd$|providcence$|ph providence$|providenxce$|p rovidence$| peov$|providernce|providewnce$| proiv$| prov1417$|providenvce$|orovidence$|providenxwe$|providence r$|provience$|provience|proviedence$|providencde$|providece$|providnce$|providenec$|proivdence$|providende$|proidence$", "providence", df$City)
df$City <- gsub("^north priv$|^north p$|86a nipmuc trail|^np$|^northprovidence$|^nprovidence$|^noprovidence$|also 26 countryside dr", "north providence", df$City)
df$City <- gsub("riversdie|rivereside|^iverside$|riveside|riverisde|riversice|riversidse|riversie$", "riverside", df$City)
df$City <- gsub("rehobeth", "rehoboth", df$City)
df$City <- gsub("rimford|rumfod|02916|runford|^ruford$|rumfoed|^rumord$|^rumfor$", "rumford", df$City)
df$City <- gsub("sanfrancisco", "san francisco", df$City)
df$City <- gsub("shanrock", "shannock", df$City)
df$City <- gsub("saundersrown|saundastown|sanderstown|saunderston$|saunderstoqwnri|sdaunderstown|106 mourning dove dr|saunderstownri|saunderstoen|^saunderstwon$|^sunderstown$|^sndstwn$|suanderstown|shanderstown|sounderstown|saundestown|sauponstown|saunderstonw|saunderstorn|saunderstow$|saunderstown rd|saudnerstown|saundersown|saunderatown|sauderstown|saundertown", "saunderstown", df$City)
df$City <- gsub("sciuate|02831|scitutate|^scituae|west scituate|scidtuate|^scitute|^sctituate|^sctuate|^scituae|^scituater|scit$|situate|scituite|schituate|sciutate|scituateq|scitaue|scitaute", "scituate", df$City)
df$City <- gsub("slaterville|slattersville", "slatersville", df$City)
df$City <- gsub("smithfeld|smithefield|smitthfield|smithfiield|smtithfield|smnithfield|smiithfield|^02917$|smthfield|smithfileld|smirthfield|smitfield|smithgfield|smithfild|smighfield|smighfiels|smith field$|smithfied|smithield|smithfiel$|smithfiled|smihfield|smithifeld|sithfiel|smithfeild|smtihfield", "smithfield", df$City)
df$City <- gsub("south kingstown|^sk$|^wk$|^kignston|^kingsto$|^wake$|^kinston$|^kingstown$|^kingston$|^kngston$|552 kingstown ro", "south kingston", df$City)
df$City <- gsub("tivertson|02878", "tiverton", df$City)
df$City <- gsub("unk$|^tbd$|anywhere|unknown at this time|^co$|27 cornell rd$|various|requested|unkown|don t know|info requested|information requested|investigating|^x$|^xx$|^x{1,} unknown$|obtaining info|^na$|^none$|^n$|awaiting info", "unknown", df$City)


df <- df %>% filter(ExpDate >= "2002-01-01") %>%
     transform(City = str_to_title(City), Address = str_to_title(Address)) %>%
     filter(!is.na(ExpDate)); head(df)


exp_to_here <- saveRDS(df, paste(dir, "exp_to_here.rds", sep = ""))
#df <- readRDS(paste(dir, "exp_to_here.rds", sep = ""))

bluewest <- sort(grep("blue west|bluewest", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(bluewest)
df$FullName[df$FullName %in% bluewest] <- "Blue West Media"; rm(bluewest)
primedia <- sort(grep("^primedia|^pri media", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(primedia)
df$FullName[df$FullName %in% primedia] <- "Primedia"; rm(primedia)
buyingtime <- sort(grep("buying time", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(buyingtime)
df$FullName[df$FullName %in% buyingtime] <- "Buying Time LLC"; rm(buyingtime)
knickerbocker <- sort(grep("knickerbocker", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(knickerbocker); xpend(knickerbocker)
df$FullName[df$FullName %in% knickerbocker] <- "SKDKnickerbocker"; rm(knickerbocker)
raimondo <- sort(grep("gina raimondo|gina m. raimondo|gov raimondo|friends of raimondo|^raimondo$|^raimondo for", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(raimondo); xpend(raimondo)
df$FullName[df$FullName %in% raimondo] <- "Gina Raimondo"; rm(raimondo)
mattiello <- sort(grep("friends of mattiello|speaker mattiello|nicholad mattiello|nicholes mattielo|^mattielo for rep|^mattiello$|anthony mattiello|mattiello for rep|nicholas matiello|nicholas anthony mattiello|nicolas mattielonicholad mattiello|nick mattiello|nicholas a mattiello|nicholas mattiello|^n mattiello", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(mattiello); xpend(mattiello)
df$FullName[df$FullName %in% mattiello] <- "Nicholas A Mattiello"; rm(mattiello)
demLead <- sort(grep("fund for democratic l", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(demLead)
df$FullName[df$FullName %in% demLead] <- "Fund for Democratic Leadership"; rm(demLead)
demPri <- sort(grep("fund or democratic priorities|funds for denocratic priorities|funds for democratic priorities|fund for deomcratic priorities|fund for democratic parties|fund for democratic$|fund for democratic prior", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(demPri)
df$FullName[df$FullName %in% demPri] <- "Fund for Democratic Priorities"; rm(demPri)
goodgovern <- sort(grep("good govern|good govt|good goverment|good govmt", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(goodgovern); xpend(goodgovern)
df$FullName[df$FullName %in% goodgovern] <- "RI Good Government PAC"; rm(goodgovern)
senatelead <- sort(grep("senate lead", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(senatelead); xpend(senatelead)
df$FullName[df$FullName %in% senatelead] <- "RI Senate Leadership PAC"; rm(senatelead)
houselead <- sort(grep("house leader", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(houselead); xpend(houselead)
df$FullName[df$FullName %in% houselead] <- "RI House Leadership PAC"; rm(houselead)
ruggerio <- sort(grep("nic ruggerio|nick ruggerio|j ruggerio|nick ruggierio|nic ruggierio|nick ruggiero", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(ruggerio); xpend(ruggerio)
df$FullName[df$FullName %in% ruggerio] <- "Dominick J Ruggerio"; rm(ruggerio)
fung <- sort(grep("alan fung|allan fung|alan w fung|allan w fung|alen fung|allen fung|mayor fung|^fung |friends of fung", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(fung); xpend(fung)
df$FullName[df$FullName %in% fung] <- "Allan W Fung"; rm(fung)
morgan <- sort(grep("pat morgan|patrica morgan|patricia morgan|patricia l morgan|^p morgan", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(morgan); xpend(morgan)
df$FullName[df$FullName %in% morgan] <- "Patricia Morgan"; rm(morgan)
brown <- sort(grep("matthew a brown|matt brown$|matthew brown|friends of brown|mathew a brown|mathew brown", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(brown); xpend(brown)
df$FullName[df$FullName %in% brown] <- "Matthew A Brown"; rm(brown)
arch <- sort(grep("archambault for senate|stephen archmbault|stephen archambeault|stephen r archambault|stephen archambault|steve archambault|steven r archambault|steven archambault|steve r archambault", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(arch); xpend(arch)
df$FullName[df$FullName %in% arch] <- "Stephen R Archambault"; rm(arch)
shekarchi <- sort(grep("joseph shekarchi|joe shekarchi|k j shekarchi|^j shekarchi|joeseph shekarchi|^shekarchi$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(shekarchi); xpend(shekarchi)
df$FullName[df$FullName %in% shekarchi] <- "Joseph Shekarchi"; rm(shekarchi)
regun <- sort(grep("aaron regunberg|j a regunberg|jonathan a regunberg", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(regun); xpend(regun)
df$FullName[df$FullName %in% regun] <- "Aaron Regunberg"; rm(regun)
mckee <- sort(grep("daniel mckee|daniel j mckee|dan mckee|^mckee for lt|can mckee", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(mckee); xpend(mckee)
df$FullName[df$FullName %in% mckee] <- "Daniel J Mckee"; rm(mckee)
galvin <- sort(grep("galvin and assoc", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(galvin); xpend(galvin)
df$FullName[df$FullName %in% galvin] <- "Galvin and Associates"; rm(galvin)
smiley <- sort(grep("brett smiley", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(smiley); xpend(smiley)
df$FullName[df$FullName %in% smiley] <- "Brett Smiley"; rm(smiley)
magazine <- sort(grep("magaziner for|seth magaziner", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(magazine); xpend(magazine)
df$FullName[df$FullName %in% magazine] <- "Seth Magaziner"; rm(magazine)
kilmartin <- sort(grep("peter kilmartin|peter f kilmartin|friends of kilmartin|kilmartin for", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(kilmartin); xpend(kilmartin)
df$FullName[df$FullName %in% kilmartin] <- "Peter Kilmartin"; rm(kilmartin)
elorza <- sort(grep("jorge elorza|jorge o elorza|elorza for|elorza fro|mayor elorza", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(elorza); xpend(elorza)
df$FullName[df$FullName %in% elorza] <- "Jorge Elorza"; rm(elorza)
neronha <- sort(grep("peter neronha|nehrona for|neronha for|peter f neronha", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(neronha); xpend(neronha)
df$FullName[df$FullName %in% neronha] <- "Peter Neronha"; rm(neronha)
gorbea <- sort(grep("nellie gordea|nellie gorbea|nellie m gorbea|nelli gorbea|^gorbea$|gorbea for|friends of gorrbea", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(gorbea); xpend(gorbea)
df$FullName[df$FullName %in% gorbea] <- "Nellie Gorbea"; rm(gorbea)
solomon <- sort(grep("joseph j solomon|joseph solomon|joseph soloomon|joe solomon|friends of solomon|joe soloman|^solomon for|^solomon$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(solomon); xpend(solomon)
df$FullName[df$FullName %in% solomon] <- "Joseph J Solomon"; rm(solomon)
mccaffrey <- sort(grep("michael mccaffery|michael mccaffrey|michael j mccaffrey|^mccaffrey for|friends of mccaffrey|mike mccaffrey|senator mccaffrey", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(mccaffrey); xpend(mccaffrey)
df$FullName[df$FullName %in% mccaffrey] <- "Michael Mccaffrey"; rm(mccaffrey)
rucci <- sort(grep("stephen ucci|stephen r ucci|^ucci for", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(rucci); xpend(rucci)
df$FullName[df$FullName %in% rucci] <- "Stephen R Ucci"; rm(rucci)
polisena <- sort(grep("joe polisena|joseph polisena|^polisena$|joseph m polisena|friends of polisena|mayor polisena|polisena committee", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(polisena); xpend(polisena)
df$FullName[df$FullName %in% polisena] <- "Joseph Polisena"; rm(polisena)
ciccilline <- sort(grep("david n cicilline|cicciline comm|david cicciline|friends of cicilline|david ciccilline|ciccilline comm|cicilline comm|david cicilline", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(ciccilline); xpend(ciccilline)
df$FullName[df$FullName %in% ciccilline] <- "David N Cicilline"; rm(ciccilline)
chafee <- sort(grep("^chaffee for|linclon chafee|lincoln chaffee|lincoln d chafee|lincoln chafee|^chafee for", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(chafee); xpend(chafee)
df$FullName[df$FullName %in% chafee] <- "Lincoln Chafee"; rm(chafee)
carcieri <- sort(grep("donald carcieri|donald l carcieri|^carcieri for|friends of carcieri", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(carcieri); xpend(carcieri)
df$FullName[df$FullName %in% carcieri] <- "Donald Carcieri"; rm(carcieri)
salvatore <- sort(grep("david anthony salvatore|david salvatore|david a salvatore|friends of salvatore|salvator for", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(salvatore); xpend(salvatore)
df$FullName[df$FullName %in% salvatore] <- "David Salvatore"; rm(salvatore)
kislak <- sort(grep("rebecca kislak", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(kislak); xpend(kislak)
df$FullName[df$FullName %in% kislak] <- "Rebecca Kislak"; rm(kislak)
whitehouse <- sort(grep("sheldon whitehouse|^whitehouse$|whitehouse for", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(whitehouse); xpend(whitehouse)
df$FullName[df$FullName %in% whitehouse] <- "Sheldon Whitehouse"; rm(whitehouse)
patricks <- sort(grep("patricks pub|patrick pub", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(patricks); xpend(patricks)
df$FullName[df$FullName %in% patricks] <- "Patricks Pub"; rm(patricks)
jayne <- sort(grep("moira jayne walsh|moira walsh|moira j walsh", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(jayne); xpend(jayne)
df$FullName[df$FullName %in% jayne] <- "Moira Walsh"; rm(jayne)
paolino <- sort(grep("joseph r paolino|paolino properties|joseph paolino jr", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(paolino); xpend(paolino)
df$FullName[df$FullName %in% paolino] <- "Joseph R Paolino Jr"; rm(paolino)
paolino <- sort(grep("joseph paolino|anthony j paolino|anthony paolino", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(paolino); xpend(paolino)
df$FullName[df$FullName %in% paolino] <- "Anthony J Paolino"; rm(paolino)
murphy <- sort(grep("william j murphy|william murphy", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(murphy); xpend(murphy)
df$FullName[df$FullName %in% murphy] <- "William J Murphy"; rm(murphy)
fox <- sort(grep("^gordan fox|^gorden fox|gordon d fox|gordon fox|grodon d fox|fox for rep", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(fox); xpend(fox)
df$FullName[df$FullName %in% fox] <- "Gordon Fox"; rm(fox)
gallison <- sort(grep("raymond e gallison|raymond gallison|ray gallison|gallison for|friends of gallison", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(gallison); xpend(gallison)
df$FullName[df$FullName %in% gallison] <- "Raymond E Gallison"; rm(gallison)
caprio <- sort(grep("frank t caprio|frank caprio|^caprio for|^caprio comm|friends of caprio", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(caprio); xpend(caprio)
df$FullName[df$FullName %in% caprio] <- "Frank T Caprio"; rm(caprio)
langevin <- sort(grep("james r langevin|james langevin|jim langevin|james longevin|^langevin for|james langvin|james lanevin", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(langevin); xpend(langevin)
df$FullName[df$FullName %in% langevin] <- "James Langevin"; rm(langevin)
projo <- sort(grep("projo|providence journal|prov journal|providence jounral", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(projo); xpend(projo)
df$FullName[df$FullName %in% projo] <- "Providence Journal"; rm(projo)
blues <- sort(grep("blue cross|bcbs", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(blues); xpend(blues)
df$FullName[df$FullName %in% blues] <- "Blue Cross Blue Shield"; rm(blues)
checkmate <- sort(grep("^checkmate$|check mate consulting|checkmate co|checkmate adver|checkmate cg", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(checkmate); xpend(checkmate)
df$FullName[df$FullName %in% checkmate] <- "Checkmate Consulting"; rm(checkmate)
gabarra <- sort(grep("amy gabarra|amy e gabarra|amy gabara", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(gabarra); xpend(gabarra)
df$FullName[df$FullName %in% gabarra] <- "Amy Gabarra"; rm(gabarra)
ridem <- sort(grep("^ri democractic party|ri democratic state party|ri demstate committee|ri democratic party|ri state democratic party|rhode island democratic party", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(ridem); xpend(ridem)
df$FullName[df$FullName %in% ridem] <- "RI Democratic Party"; rm(ridem)
ristatecomm <- sort(grep("^democratic state committee|rhode island democratic state committee|^ri democratic state comm|senat demo committee|^ri democratic comm|^ri democratic state cmte|^ri democratic senate comm", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(ristatecomm); xpend(ristatecomm)
df$FullName[df$FullName %in% ristatecomm] <- "RI Democratic State Committee"; rm(ristatecomm)
wjar <- sort(grep("wjar", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(wjar); xpend(wjar)
df$FullName[df$FullName %in% wjar] <- "WJAR"; rm(wjar)
wpri <- sort(grep("wpri", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(wpri); xpend(wpri)
df$FullName[df$FullName %in% wpri] <- "WPRI"; rm(wpri)
kenblock <- sort(grep("kenneth j block|kenneth block|ken block", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(kenblock); xpend(kenblock)
df$FullName[df$FullName %in% kenblock] <- "Kenneth J Block"; rm(kenblock)
morabito <- sort(grep("edward m morabito", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(morabito); xpend(morabito)
df$FullName[df$FullName %in% morabito] <- "Edward M Morabito"; rm(morabito)
kapstein <- sort(grep("jeremy a kapstein|jeremy kapstein", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(kapstein); xpend(kapstein)
df$FullName[df$FullName %in% kapstein] <- "Jeremy A Kapstein"; rm(kapstein)
napolitano <- sort(grep("michael t napolitano|michael napolitano|mike napolitano|michael napalitano", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(napolitano); xpend(napolitano)
df$FullName[df$FullName %in% napolitano] <- "Michael Napolitano"; rm(napolitano)
ngp <- sort(grep("ngp software|npg software|^npg|^ngp", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(ngp); xpend(ngp)
df$FullName[df$FullName %in% ngp] <- "NGP Software"; rm(ngp)
onmessage <- sort(grep("onmessage|^on mess", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(onmessage); xpend(onmessage)
df$FullName[df$FullName %in% onmessage] <- "Onmessage Inc"; rm(onmessage)
cfo <- sort(grep("^cfo co", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(cfo); xpend(cfo)
df$FullName[df$FullName %in% cfo] <- "CFO Consulting Group"; rm(cfo)
verizon <- sort(grep("verizon", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(verizon); xpend(verizon)
df$FullName[df$FullName %in% verizon] <- "Verizon"; rm(verizon)
wnacfax <- sort(grep("wnac", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(wnacfax); xpend(wnacfax)
df$FullName[df$FullName %in% wnacfax] <- "WNAC"; rm(wnacfax)
patricklynch <- sort(grep("patrick lynch", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(patricklynch); xpend(patricklynch)
df$FullName[df$FullName %in% patricklynch] <- "Patrick Lynch"; rm(patricklynch)
maryellen <- sort(grep("mary ellen goodwin|maryellen goodwin", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(maryellen); xpend(maryellen)
df$FullName[df$FullName %in% maryellen] <- "Maryellen Goodwin"; rm(maryellen)
keable <- sort(grep("cale keable|cale p keable|representative keable", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(keable); xpend(keable)
df$FullName[df$FullName %in% keable] <- "Cale Keable"; rm(keable)
gracediaz <- sort(grep("grace diaz", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(gracediaz); xpend(gracediaz)
df$FullName[df$FullName %in% gracediaz] <- "Grace Diaz"; rm(gracediaz)
scottslater <- sort(grep("scott slater|scott a slater|^slater for|friends of slater", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(scottslater); xpend(scottslater)
df$FullName[df$FullName %in% scottslater] <- "Scott Slater"; rm(scottslater)
ryanperason <- sort(grep("ryan w pearson|ryan pearson", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(ryanperason); xpend(ryanperason)
df$FullName[df$FullName %in% ryanperason] <- "Ryan W Pearson"; rm(ryanperason)
danieldaponte <- sort(grep("danial daponte|daniel da ponte|dan daponte|daniel daponte|dan da ponte", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(danieldaponte); xpend(danieldaponte)
df$FullName[df$FullName %in% danieldaponte] <- "Daniel Daponte"; rm(danieldaponte)
hannagallo <- sort(grep("hanna gallo|hannah gallo|hanna m gallo|hannah m gallo", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(hannagallo); xpend(hannagallo)
df$FullName[df$FullName %in% hannagallo] <- "Hanna Gallo"; rm(hannagallo)
helio <- sort(grep("helio melo|helio mello|helio m melo|helio m mello", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(helio); xpend(helio)
df$FullName[df$FullName %in% helio] <- "Helio Melo"; rm(helio)
patriciaserpa <- sort(grep("patricia serpa|patricia a serpa|pat serpa|patrcia a serpa", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(patriciaserpa); xpend(patriciaserpa)
df$FullName[df$FullName %in% patriciaserpa] <- "Patricia Serpa"; rm(patriciaserpa)
lisatomasso <- sort(grep("lisa p tomasso|lisa tomasso", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(lisatomasso); xpend(lisatomasso)
df$FullName[df$FullName %in% lisatomasso] <- "Lisa Tomasso"; rm(lisatomasso)
charlenelima <- sort(grep("charlene lima|charlene m lima|^c lima$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(charlenelima); xpend(charlenelima)
df$FullName[df$FullName %in% charlenelima] <- "Charlene Lima"; rm(charlenelima)
katkazarian <- sort(grep("katherine kazarian|katherine s kazarian|kathie kazarian|kathryn kazarian|kathy kazarian|kathleen kazarian|catherine kazarian|k kazarian|katerine kazarian|kat kazarian|kate kazarian", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(katkazarian); xpend(katkazarian)
df$FullName[df$FullName %in% katkazarian] <- "Katherine Kazarian"; rm(katkazarian)
davidbennet <- sort(grep("david bennet|david a bennett|^bennett for|dave bennet", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(davidbennet); xpend(davidbennet)
df$FullName[df$FullName %in% davidbennet] <- "David Bennet"; rm(davidbennet)
miaackerman <- sort(grep("mia ackerman|mia a ackerman", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(miaackerman); xpend(miaackerman)
df$FullName[df$FullName %in% miaackerman] <- "Mia A Ackerman"; rm(miaackerman)
juanpichardo <- sort(grep("juan pichardo|juan m pichardo|pichardo for senate|friends of pichardo|^pichardo$|^picardo for|juan picardo", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(juanpichardo); xpend(juanpichardo)
df$FullName[df$FullName %in% juanpichardo] <- "Juan Pichardo"; rm(juanpichardo)
robertcraven <- sort(grep("robert craven|robert e craven|bob craven|rep craven|robert emmett craven", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(robertcraven); xpend(robertcraven)
df$FullName[df$FullName %in% robertcraven] <- "Robert E Craven"; rm(robertcraven)
jayedwards <- sort(grep("jay edwards|john g edwards|john edwards|^j edwards|^jason edwards", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(jayedwards); xpend(jayedwards)
df$FullName[df$FullName %in% jayedwards] <- "Jay Edwards"; rm(jayedwards)
gaylegoldin <- sort(grep("gayle goldin|gayle l goldin|goldin for senate", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(gaylegoldin); xpend(gaylegoldin)
df$FullName[df$FullName %in% gaylegoldin] <- "Gayle Goldin"; rm(gaylegoldin)
williamobrien <- sort(grep("william obrien|william w obrien|bill obrien|william o brien|representative obrien|o;brien|willilam obrien|^obrien$|william wobrien|willams obrien", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(williamobrien); xpend(williamobrien)
df$FullName[df$FullName %in% williamobrien] <- "William O'Brien"; rm(williamobrien)
ralphmollis <- sort(grep("ralph mollis|ralph a mollis|ralph r mollis|^a r mollis|^a mollis|^mollis for|ralpha mollis|friends of mollis|^mollis$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(ralphmollis); xpend(ralphmollis)
df$FullName[df$FullName %in% ralphmollis] <- "A Ralph Mollis"; rm(ralphmollis)
jaohncarnevale <- sort(grep("john carnevale|john m carnevale|john m caenevale|friends of carnevale|representative carnevale|^carnevale$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(jaohncarnevale); xpend(jaohncarnevale)
df$FullName[df$FullName %in% jaohncarnevale] <- "John Carnevale"; rm(jaohncarnevale)
blazejewski <- sort(grep("chris blazejewski|christopher blazejewski|christopher r blazejewski|friends of blazejewski", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(blazejewski); xpend(blazejewski)
df$FullName[df$FullName %in% blazejewski] <- "Chris Blazejewski"; rm(blazejewski)
adamsatchel <- sort(grep("adam j satchell|adam satchel|friends of satchell|satchell for", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(adamsatchel); xpend(adamsatchel)
df$FullName[df$FullName %in% adamsatchel] <- "Adam Satchel"; rm(adamsatchel)
jimclyburn <- sort(grep("jim clyburn", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(jimclyburn); xpend(jimclyburn)
df$FullName[df$FullName %in% jimclyburn] <- "Jim Clyburn"; rm(jimclyburn)
mcnamara <- sort(grep("joseph mcnamara|joseph m mcnamara|joe mcnamara", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(mcnamara); xpend(mcnamara)
df$FullName[df$FullName %in% mcnamara] <- "Joseph Mcnamara"; rm(mcnamara)
costantino <- sort(grep("steven m costantino|steven costantino|steve costantino|cconstantino|friends of constantino|^costantinos$|^constantino for|costantino comm|representative costantino|elect costantino", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(costantino); xpend(costantino)
df$FullName[df$FullName %in% costantino] <- "Steven M Costantino"; rm(costantino)
lizcrowley <- sort(grep("beth crowley|betty crowley|^crowley for|^e crowley|friends of crowley|beth a crowley |bet crowley|elizabeht crowley", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(lizcrowley); xpend(lizcrowley)
df$FullName[df$FullName %in% lizcrowley] <- "Elizabeth Crowley"; rm(lizcrowley)
scottguthrie <- sort(grep("scott guthrie|scott j guthrie|^guthrie comm", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(scottguthrie); xpend(scottguthrie)
df$FullName[df$FullName %in% scottguthrie] <- "Scott Guthrie"; rm(scottguthrie)
angeltaveras <- sort(grep("angel taveras|angel tavaras|angel tavares|mayor taveras|^taveras for|^angel for", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(angeltaveras); xpend(angeltaveras)
df$FullName[df$FullName %in% angeltaveras] <- "Angel Taveras"; rm(angeltaveras)
stephencasey <- sort(grep("stephen casey|steve casey|stephen m casey|stephen w casey", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(stephencasey); xpend(stephencasey)
df$FullName[df$FullName %in% stephencasey] <- "Stephen M Casey"; rm(stephencasey)
anastasia <- sort(grep("anastasia will|anastasia p will|williams friends of anastasia", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(anastasia); xpend(anastasia)
df$FullName[df$FullName %in% anastasia] <- "Anastasia P Williams"; rm(anastasia)
jacquard <- sort(grep("robert jacquard|robert jacquard|bob jacquard|bob jaquard|^jacquard for", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(jacquard); xpend(jacquard)
df$FullName[df$FullName %in% jacquard] <- "Robert Jacquard"; rm(jacquard)
johnbarrow <- sort(grep("john barrow", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(johnbarrow); xpend(johnbarrow)
df$FullName[df$FullName %in% johnbarrow] <- "John Barrow"; rm(johnbarrow)
denniscanario <- sort(grep("dennis canario|dennis m canario|dennis canaro|dennis cannario", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(denniscanario); xpend(denniscanario)
df$FullName[df$FullName %in% denniscanario] <- "Dennis Canario"; rm(denniscanario)
marymessier <- sort(grep("mary messier|mary d messier|mary duffy messier|mary duffy-messier|mary duff-messier|mary duff messier|mary messier-duffy|^messier for|^duffy-messier", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(marymessier); xpend(marymessier)
df$FullName[df$FullName %in% marymessier] <- "Mary Duffy Messier"; rm(marymessier)
michaelmorin <- sort(grep("michael morin|michael a morin|mike morin|micheal morin", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(michaelmorin); xpend(michaelmorin)
df$FullName[df$FullName %in% michaelmorin] <- "Michael Morin"; rm(michaelmorin)
franklombardo <- sort(grep("frank lombardo|^lombardo for|frank lombardi", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(franklombardo); xpend(franklombardo)
df$FullName[df$FullName %in% franklombardo] <- "Frank Lombardo III"; rm(franklombardo)
ogrady <- sort(grep("jeremiah o grady|jeremiah ogrady|jay ogrady|jeremiah t ogrady|friends of ogrady|ogrady comm|jeramiah ogrady|jerimiah ogrady|jeremiah grady|jay o grady|jeremiah ogardy|jeremiah t o’grady", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(ogrady); xpend(ogrady)
df$FullName[df$FullName %in% ogrady] <- "Jeremiah O'Grady"; rm(ogrady)
cindycoyne <- sort(grep("cindy coyne|cynthia a coyne|cynthia coyne|cynchia armour coyne|cynthia armour coyne", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(cindycoyne); xpend(cindycoyne)
df$FullName[df$FullName %in% cindycoyne] <- "Cindy Coyne"; rm(cindycoyne)
janmalik <- sort(grep("jan malik|jan p malik|jan d malik|representative malik", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(janmalik); xpend(janmalik)
df$FullName[df$FullName %in% janmalik] <- "Jan Malik"; rm(janmalik)
joyhearn <- sort(grep("joy hearn|joy hern", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(joyhearn); xpend(joyhearn)
df$FullName[df$FullName %in% joyhearn] <- "Joy Hearn"; rm(joyhearn)
greggamore <- sort(grep("gregg amore|greg amore|^amore for|friends of amore|representative amore", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(greggamore); xpend(greggamore)
df$FullName[df$FullName %in% greggamore] <- "Gregg Amore"; rm(greggamore)
evanshanley <- sort(grep("evan shan|evan p shan|even shan|shanley for|friends of shanley", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(evanshanley); xpend(evanshanley)
df$FullName[df$FullName %in% evanshanley] <- "Evan Shanley"; rm(evanshanley)
felag <- sort(grep("wally felag|walter felag|walter s felag|friends of wall felag|friends of felag|^felag for", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(felag); xpend(felag)
df$FullName[df$FullName %in% felag] <- "Wally Felag"; rm(felag)
haroldmetts <- sort(grep("harold metts|harold m metts|^metts for|harold metis", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(haroldmetts); xpend(haroldmetts)
df$FullName[df$FullName %in% haroldmetts] <- "Harold Metts"; rm(haroldmetts)
donnanesselbush <- sort(grep("donna nesselbush|donna m nesselbush|^d nesselbush|^nesselbush for|sen nesselbush", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(donnanesselbush); xpend(donnanesselbush)
df$FullName[df$FullName %in% donnanesselbush] <- "Donna Nesselbush"; rm(donnanesselbush)
briannewberry <- sort(grep("brian newberry|brian c newberry|^b newberry|^newberry elect|^newberry for", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(briannewberry); xpend(briannewberry)
df$FullName[df$FullName %in% briannewberry] <- "Brian Newberry"; rm(briannewberry)
kathyfogarty <- sort(grep("kathy fogarty|kathleen fogarty|kathleen a fogarty|kathleen fogerty|kathy forgarty|cathy fogarty|kay fogarty|kathy fogerty|kathleen a gogarty", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(kathyfogarty); xpend(kathyfogarty)
df$FullName[df$FullName %in% kathyfogarty] <- "Kathy Fogarty"; rm(kathyfogarty)
debruggiero <- sort(grep("deb ruggiero|deborah ruggiero|deborah l ruggiero|devorah ruggiero|deborah ruggerio|deb ruggeiro|debra ruggeiro|deb ruggerio|debroah ruggerio", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(debruggiero); xpend(debruggiero)
df$FullName[df$FullName %in% debruggiero] <- "Deborah Ruggiero"; rm(debruggiero)
rogerpicard <- sort(grep("roger picard|roger a picard|picard roger|^picard for", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(rogerpicard); xpend(rogerpicard)
df$FullName[df$FullName %in% rogerpicard] <- "Roger Picard"; rm(rogerpicard)
kenmarshall <- sort(grep("ken marshal|kenneth marshall|kenneth a marshall|rep marshall", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(kenmarshall); xpend(kenmarshall)
df$FullName[df$FullName %in% kenmarshall] <- "Kenneth Marshall"; rm(kenmarshall)
rayhull <- sort(grep("ray hull|friends of r hull|^hull$|raymond hall|raymond a hull|raymond hull|^hull for|ray hall", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(rayhull); xpend(rayhull)
df$FullName[df$FullName %in% rayhull] <- "Raymond Hull"; rm(rayhull)
marszalkowski <- sort(grep("marszalkowski|marszalkowsi|marszylkowski|marzsalkowski|marszalkouski", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(marszalkowski); xpend(marszalkowski)
df$FullName[df$FullName %in% marszalkowski] <- "Alex Marszalkowski"; rm(marszalkowski)
frankciccone <- sort(grep("frank ciccone|frank a ciccone|^ciccone for|senator ciccone|^ciccone$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(frankciccone); xpend(frankciccone)
df$FullName[df$FullName %in% frankciccone] <- "Frank Ciccone"; rm(frankciccone)
paiva <- sort(grep("teresa paiva|paiva weed|paivaweed|paiva-weed|teresa weed", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(paiva); xpend(paiva)
df$FullName[df$FullName %in% paiva] <- "M Teresa Paiva Weed"; rm(paiva)
ldl <- sort(grep("laborers political league|laboers political league|lpl|liuna", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(ldl); xpend(ldl)
df$FullName[df$FullName %in% ldl] <- "Laborers Political League"; rm(ldl)
seiu <- sort(grep("seiu|service employees international union", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(seiu); xpend(seiu)
df$FullName[df$FullName %in% seiu] <- "Service Employees International Unions"; rm(seiu)
joshuamiller <- sort(grep("joshua miller|josh miller|friends of miller|^miller for", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(joshuamiller); xpend(joshuamiller)
df$FullName[df$FullName %in% joshuamiller] <- "Joshua Miller"; rm(joshuamiller)
ibew <- sort(grep("ibew", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(ibew); xpend(ibew)
df$FullName[df$FullName %in% ibew] <- "Intl Bureau of Electrical Workers"; rm(ibew)
saferroad <- sort(grep("safer road|safer roadcommittee|safe road|saferroad|saferoad", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(saferroad); xpend(saferroad)
df$FullName[df$FullName %in% saferroad] <- "Safer Road Committee"; rm(saferroad)
reedcomm <- sort(grep("reed comm|jack reed|john reed", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(reedcomm); xpend(reedcomm)
df$FullName[df$FullName %in% reedcomm] <- "Jack Reed"; rm(reedcomm)
lizroberts <- sort(grep("beth robert|^roberts for|friends of roberts", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(lizroberts); xpend(lizroberts)
df$FullName[df$FullName %in% lizroberts] <- "Elizabeth Roberts"; rm(lizroberts)
workingfamilies <- sort(grep("working fam", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(workingfamilies); xpend(workingfamilies)
df$FullName[df$FullName %in% workingfamilies] <- "Working Families"; rm(workingfamilies)
briankennedy <- sort(grep("brian patrick kennedy|brian kennedy|brian p kennedy", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(briankennedy); xpend(briankennedy)
df$FullName[df$FullName %in% briankennedy] <- "Brian P Kennedy"; rm(briankennedy)
perterpalumbo <- sort(grep("peter palumbo|peter palombo|peter g palumbo|friends of p palumbo|pete palumbo|^palumbo comm", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(perterpalumbo); xpend(perterpalumbo)
df$FullName[df$FullName %in% perterpalumbo] <- "Peter Palumbo"; rm(perterpalumbo)
greenbergquinlan <- sort(grep("greenberg quinlan", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(greenbergquinlan); xpend(greenbergquinlan)
df$FullName[df$FullName %in% greenbergquinlan] <- "Greenberg Quinlan Research"; rm(greenbergquinlan)
johndesimone <- sort(grep("john desiomne|john desimone|john j desimone|^desimone$|^de simone$|desimone john|desimone committee", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(johndesimone); xpend(johndesimone)
df$FullName[df$FullName %in% johndesimone] <- "John Desimone"; rm(johndesimone)
tassoni <- sort(grep("john j tassoni|john tassoni|john tassonni|john t tassoni|senator john s tassoni", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(tassoni); xpend(tassoni)
df$FullName[df$FullName %in% tassoni] <- "John J Tassoni Jr"; rm(tassoni)
spencer <- sort(grep("spencer dickinson|spencer e dickinson|spencer dickenson", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(spencer); xpend(spencer)
df$FullName[df$FullName %in% spencer] <- "Spencer E Dickinson"; rm(spencer)
paulfog <- sort(grep("paul fogarty|paul w fogarly|paul w fogarty|^fogarty$|^fogarty for", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(paulfog); xpend(paulfog)
df$FullName[df$FullName %in% paulfog] <- "Paul W Fogarty"; rm(paulfog)
bealanzi <- sort(grep("bea lanzi|beatrice lanzi|beatrice a lanzi|betrice lanzi|beatricew lanzi|lanzi for senate", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(bealanzi); xpend(bealanzi)
df$FullName[df$FullName %in% bealanzi] <- "Bea Lanzi"; rm(bealanzi)
frankferri <- sort(grep("frank g ferri|frank ferry|frank ferri|ferri for lt gov", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(frankferri); xpend(frankferri)
df$FullName[df$FullName %in% frankferri] <- "Frank Ferri"; rm(frankferri)
teresatanzi <- sort(grep("teresa tanzi|tanzi for rep|teresa a tanzi|teresa ann tanzi|theresa tanzi|teresea tanzi|representative tanzi|^tanzi$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(teresatanzi); xpend(teresatanzi)
df$FullName[df$FullName %in% teresatanzi] <- "Teresa Tanzi"; rm(teresatanzi)
billsanbento <- sort(grep("william san bento|william sanbento|bill san bento|bill sanbento|^san bento$|willliam san bento|willim san bento|^san bento for|friends of san bento|willian sanbento", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(billsanbento); xpend(billsanbento)
df$FullName[df$FullName %in% billsanbento] <- "Bill San Bento"; rm(billsanbento)
billwalaska <- sort(grep("bill walaska|william a walaska|bill waleska|william walaska|walaska for senate|^walaska$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(billwalaska); xpend(billwalaska)
df$FullName[df$FullName %in% billwalaska] <- "Bill Walaska"; rm(billwalaska)
cbc <- sort(grep("^cbc-p", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(cbc); xpend(cbc)
df$FullName[df$FullName %in% cbc] <- "CBC PAC"; rm(cbc)
renemenard <- sort(grep("rene menard|rene r menard", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(renemenard); xpend(renemenard)
df$FullName[df$FullName %in% renemenard] <- "Rene Menard"; rm(renemenard)
bobdasilva <- sort(grep("bob desilva|bob da silva|robert l dasilva|robert dasilva|bob dasilva|^dasilva for rep|roberto dasila|roberto dasilva|roberto l dasilva|roberto da silva", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(bobdasilva); xpend(bobdasilva)
df$FullName[df$FullName %in% bobdasilva] <- "Bob Dasilva"; rm(bobdasilva)
loudipalma <- sort(grep("lou di palma|lou depalma|louis dipalma|louis di palma|lou dipalma|louis p dipalma|lou diplama|friends of lou dipama|friends of louis depalma", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(loudipalma); xpend(loudipalma)
df$FullName[df$FullName %in% loudipalma] <- "Lou Dipalma"; rm(loudipalma)
laborunited <- sort(grep("labor united", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(laborunited); xpend(laborunited)
df$FullName[df$FullName %in% laborunited] <- "Labor United for Connecticut"; rm(laborunited)
erinlynch <- sort(grep("erin lynch|erin p lynch", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(erinlynch); xpend(erinlynch)
df$FullName[df$FullName %in% erinlynch] <- "Erin Lynch"; rm(erinlynch)
sosnowski <- sort(grep("sosnowski|susan sossnowski|sosnowksi for|susan sosknowski|susan sonowski", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(sosnowski); xpend(sosnowski)
df$FullName[df$FullName %in% sosnowski] <- "Susan Sosnowski"; rm(sosnowski)
iannazzi <- sort(grep("andrea m iannazzi|andrea iannazzi", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(iannazzi); xpend(iannazzi)
df$FullName[df$FullName %in% iannazzi] <- "Andrea Iannazzi"; rm(iannazzi)
jamiedoyle <- sort(grep("jamie e doyle|jaimie doyle|jaime doyle|jamie doyle|james doyle|james e doyle|doyle for|^doyle$|mayor doyle|senator doyle|senatordoyle", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(jamiedoyle); xpend(jamiedoyle)
df$FullName[df$FullName %in% jamiedoyle] <- "Jamie Doyle"; rm(jamiedoyle)
deborahfellela <- sort(grep("deb felella|deborah faella|deborash fallela|deborah a faellela|deborah a faellela|friends of fellela|debra fellela|deborah fallela|debrorah fellela|debroah fellela|friends fallela|deborah fel|deb fell|deborah fell|deborah a fell|^fellela$|debra faella", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(deborahfellela); xpend(deborahfellela)
df$FullName[df$FullName %in% deborahfellela] <- "Deborah A Fellela"; rm(deborahfellela)
sabinamatos <- sort(grep("sabrina matos|sabina matos|^matos$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(sabinamatos); xpend(sabinamatos)
df$FullName[df$FullName %in% sabinamatos] <- "Sabina Matos"; rm(sabinamatos)
charleslombardi <- sort(grep("charlie lombardi|charles lombardi|mayor lombardi|^lombardi$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(charleslombardi); xpend(charleslombardi)
df$FullName[df$FullName %in% charleslombardi] <- "Charles Lombardi"; rm(charleslombardi)
hoperi <- sort(grep("hope ri|hope rhode island", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(hoperi); xpend(hoperi)
df$FullName[df$FullName %in% hoperi] <- "Hope RI PAC"; rm(hoperi)
richardmorrison <- sort(grep("richard morrison|richard morrinson|richard p morrison|richard morrisson", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(richardmorrison); xpend(richardmorrison)
df$FullName[df$FullName %in% richardmorrison] <- "Richard Morrison"; rm(richardmorrison)
santamaria <- sort(grep("friends of santa maria|richard santamaria|richard santamria|richard santamria|richard d santamaria|richard santa maria", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(santamaria); xpend(santamaria)
df$FullName[df$FullName %in% santamaria] <- "Richard Santamaria"; rm(santamaria)
maselli <- sort(grep("christopher masselli|chris maselli|christopher maselli|christopher b maselli", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(maselli); xpend(maselli)
df$FullName[df$FullName %in% maselli] <- "Christopher Maselli"; rm(maselli)
giannini <- sort(grep("elect giannini|joanne m giannini|joann gianinni|joanne gianinni|jo anne gianninni|joanne giannini", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(giannini); xpend(giannini)
df$FullName[df$FullName %in% giannini] <- "Joanne Giannini"; rm(giannini)
afl <- sort(grep("^afl|ri afl|rhode island afl|coalition of labor union|cluw ri chapter", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(afl); xpend(afl)
df$FullName[df$FullName %in% afl] <- "RI AFL-CIO"; rm(afl)
laborstud <- sort(grep("^ilsr|^ilsr|institute for laobr|labor stud", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(laborstud); xpend(laborstud)
df$FullName[df$FullName %in% laborstud] <- "Institute for Labor Studies and Research"; rm(laborstud)
provchamb <- sort(grep("prov chamb|providence chamber", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(provchamb); xpend(provchamb)
df$FullName[df$FullName %in% provchamb] <- "Greater Providence Chamber of Commerce"; rm(provchamb)
danielconnors <- sort(grep("daniel connor|dan connor|daniel p connor|connors campaign|dan c connor|daniel conner|^connors$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(danielconnors); xpend(danielconnors)
df$FullName[df$FullName %in% danielconnors] <- "Daniel Connors"; rm(danielconnors)
mikefarina <- sort(grep("mike farina|michael j farina|michael farina|friends of mfarina|councilman farina", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(mikefarina); xpend(mikefarina)
df$FullName[df$FullName %in% mikefarina] <- "Michael Farina"; rm(mikefarina)
baldelli <- sort(grep("lisa baldeli|lisa baldelli|baldelli hunt|lisa baldelli-hunt|lisa baldelli hunt|lisa baldeli hunt|baldell-hunt|bardelli-hunt", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(baldelli); xpend(baldelli)
df$FullName[df$FullName %in% baldelli] <- "Lisa Baldelli-Hunt"; rm(baldelli)
kencarter <- sort(grep("kennneth carter|kenneth carter|ken carter", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(kencarter); xpend(kencarter)
df$FullName[df$FullName %in% kencarter] <- "Kenneth Carter"; rm(kencarter)
patrickoneill <- sort(grep("patrick j oneill|james p oneill|j p oneill|partick j oneil|patrick onieill|patrick oneil|patrick o neill|patrick j oneill|patrick oneal|patrick oneill", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(patrickoneill); xpend(patrickoneill)
df$FullName[df$FullName %in% patrickoneill] <- "J Patrick O'Neill"; rm(patrickoneill)
artcorvese <- sort(grep("arthur j corverse|rep corvese|arthur j corvese|arthur corvese|arthur converse|arthur j corvese|corvese for rep|^corvese$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(artcorvese); xpend(artcorvese)
df$FullName[df$FullName %in% artcorvese] <- "Arthur Corvese"; rm(artcorvese)
harrop <- sort(grep("daniel s harrop|daniel harrop|david s harrop", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(harrop); xpend(harrop)
df$FullName[df$FullName %in% harrop] <- "Daniel S Harrop iii"; rm(harrop)
autiello <- sort(grep("dino autirllo|dino autielo|dino autiello|dino autielo|dino autellio", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(autiello); xpend(autiello)
df$FullName[df$FullName %in% autiello] <- "Dino Autiello"; rm(autiello)
aralph <- sort(grep("a ralph|ralph molis", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(aralph); xpend(aralph)
df$FullName[df$FullName %in% aralph] <- "A Ralph Mollis"; rm(aralph)
absprint <- sort(grep("abs print|a b s printing|^as printing$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(absprint); xpend(absprint)
df$FullName[df$FullName %in% absprint] <- "ABS Printing"; rm(absprint)
acorn <- sort(grep("^acorn opg|^acorn-opg|^opg graphics", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(acorn); xpend(acorn)
df$FullName[df$FullName %in% acorn] <- "Acorn OPG Graphics"; rm(acorn)
advantagepay <- sort(grep("advantage pay|^advantage$|^advantage apyroll", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(advantagepay); xpend(advantagepay)
df$FullName[df$FullName %in% advantagepay] <- "Advantage Payroll Services"; rm(advantagepay)
alltheans <- sort(grep("all the ans", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(alltheans); xpend(alltheans)
df$FullName[df$FullName %in% alltheans] <- "All the Answers"; rm(alltheans)
allegra <- sort(grep("allegra prinitng|alegra printing|allegra print|alegna print|^allegra$|allegra design|allegra east", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(allegra); xpend(allegra)
df$FullName[df$FullName %in% allegra] <- "Allegra Printing"; rm(allegra)
amazing <- sort(grep("amazing special|amzazing specialties", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(amazing); xpend(amazing)
df$FullName[df$FullName %in% amazing] <- "Amazing Specialties"; rm(amazing)
amazon <- sort(grep("amazoncom|^amazon$|amazon market|amazon com|amazon inc|amazon prime", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(amazon); xpend(amazon)
df$FullName[df$FullName %in% amazon] <- "Amazon"; rm(amazon)
americanspeed <- sort(grep("^speedy printing|american speed", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(americanspeed); xpend(americanspeed)
df$FullName[df$FullName %in% americanspeed] <- "American Speedy Printing"; rm(americanspeed)
beaconcomm <- sort(grep("^becon comm|beacon-comm|^beacon com$|beacon comm", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(beaconcomm); xpend(beaconcomm)
df$FullName[df$FullName %in% beaconcomm] <- "Beacon Communications"; rm(beaconcomm)
bjs <- sort(grep("b js whole|bjs|bj whole|^bj store|^b j s$|^b js$|^bj$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(bjs); xpend(bjs)
df$FullName[df$FullName %in% bjs] <- "BJs Wholesale Club"; rm(bjs)
media <- sort(grep("media strat", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(media); xpend(media)
df$FullName[df$FullName %in% media] <- "Media Strategies and Research"; rm(media)
media <- sort(grep("multi media serices corp|multi media service", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(media); xpend(media)
df$FullName[df$FullName %in% media] <- "Multi Media Services Corp"; rm(media)
consult <- sort(grep("johnston consult", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(consult); xpend(consult)
df$FullName[df$FullName %in% consult] <- "Johnston Consulting Inc"; rm(consult)
consult <- sort(grep("strategic consulting sol|strategic consulting serv", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(consult); xpend(consult)
df$FullName[df$FullName %in% consult] <- "Strategic Consulting Solutions"; rm(consult)
consult <- sort(grep("reilly consult", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(consult); xpend(consult)
df$FullName[df$FullName %in% consult] <- "Reilly Consulting Association"; rm(consult)
consult <- sort(grep("michael trainor|michael f trainor|mike trainor|^trainor comm", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(consult); xpend(consult)
df$FullName[df$FullName %in% consult] <- "Michael Trainor Consulting"; rm(consult)
consult <- sort(grep("new partner", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(consult); xpend(consult)
df$FullName[df$FullName %in% consult] <- "New Partners Consulting"; rm(consult)
strat <- sort(grep("fourtier strat|four tier strategies", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(strat); xpend(strat)
df$FullName[df$FullName %in% strat] <- "Fourtier Strategies"; rm(strat)
strat <- sort(grep("vision strat", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(strat); xpend(strat)
df$FullName[df$FullName %in% strat] <- "Vision Strategies"; rm(strat)
strat <- sort(grep("berger hirsch|berger\\/hirschberg", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(strat); xpend(strat)
df$FullName[df$FullName %in% strat] <- "Berger Hirschberg Strategies"; rm(strat)
strat <- sort(grep("public opinion strat", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(strat); xpend(strat)
df$FullName[df$FullName %in% strat] <- "Public Opinion Strategies"; rm(strat)
bogh <- sort(grep("bogh audio|bogh production|bogh av", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(bogh); xpend(bogh)
df$FullName[df$FullName %in% bogh] <- "Bogh Audio Visual"; rm(bogh)
comm <- sort(grep("coxs comm|coxpac|cox comm|cox media|cox business|^cox$|cox cable", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(comm); xpend(comm)
df$FullName[df$FullName %in% comm] <- "Cox Communications"; rm(comm)
comm <- sort(grep("bridge comm|bridgw communication", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(comm); xpend(comm)
df$FullName[df$FullName %in% comm] <- "Bridge Communications"; rm(comm)
comm <- sort(grep("^jh comm|j h comm", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(comm); xpend(comm)
df$FullName[df$FullName %in% comm] <- "JH Communications"; rm(comm)
comm <- sort(grep("true north comm|^tru north comm|^true north$|truenorth comm", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(comm); xpend(comm)
df$FullName[df$FullName %in% comm] <- "True North Communications"; rm(comm)
comm <- sort(grep("targeted creative", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(comm); xpend(comm)
df$FullName[df$FullName %in% comm] <- "Targeted Creative Communications"; rm(comm)
comm <- sort(grep("kennedy comm", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(comm); xpend(comm)
df$FullName[df$FullName %in% comm] <- "Kennedy Communications"; rm(comm)
comm <- sort(grep("mack-sumner comm|mack sumner", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(comm); xpend(comm)
df$FullName[df$FullName %in% comm] <- "Mack Sumner Communications"; rm(comm)
comm <- sort(grep("perspective comm", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(comm); xpend(comm)
df$FullName[df$FullName %in% comm] <- "Perspective Communications Group"; rm(comm)
comm <- sort(grep("renaissance comm", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(comm); xpend(comm)
df$FullName[df$FullName %in% comm] <- "Renaissance Communications"; rm(comm)
pay <- sort(grep("quickbooks|quickenbook", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(pay); xpend(pay)
df$FullName[df$FullName %in% pay] <- "Quickbooks Payroll Services"; rm(pay)
pay <- sort(grep("paypal|^pay pal", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(pay); xpend(pay)
df$FullName[df$FullName %in% pay] <- "Paypal"; rm(pay)
pay <- sort(grep("payden and", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(pay); xpend(pay)
df$FullName[df$FullName %in% pay] <- "Payden and Company"; rm(pay)
pay <- sort(grep("paychex", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(pay); xpend(pay)
df$FullName[df$FullName %in% pay] <- "Paychex Inc"; rm(pay)
pay <- sort(grep("interpay$|interpay elect", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(pay); xpend(pay)
df$FullName[df$FullName %in% pay] <- "Interpay Electronic Transfer"; rm(pay)
pay <- sort(grep("harpers pay", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(pay); xpend(pay)
df$FullName[df$FullName %in% pay] <- "Harpers Payroll Service"; rm(pay)
pay <- sort(grep("sage pay", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(pay); xpend(pay)
df$FullName[df$FullName %in% pay] <- "Sage Payment Solutions"; rm(pay)
group <- sort(grep("torrey group", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(group); xpend(group)
df$FullName[df$FullName %in% group] <- "The Torrey Group"; rm(group)
group <- sort(grep("sentinel group|sentinal media|sentinel print", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(group); xpend(group)
df$FullName[df$FullName %in% group] <- "The Sentinel Group"; rm(group)
group <- sort(grep("siren group", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(group); xpend(group)
df$FullName[df$FullName %in% group] <- "Siren Group"; rm(group)
group <- sort(grep("ri newspaper group|rhode island newspaper group", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(group); xpend(group)
df$FullName[df$FullName %in% group] <- "RI Newspaper Group"; rm(group)
group <- sort(grep("national development group", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(group); xpend(group)
df$FullName[df$FullName %in% group] <- "National Development Group"; rm(group)
group <- sort(grep("mayforth group", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(group); xpend(group)
df$FullName[df$FullName %in% group] <- "Mayforth Group"; rm(group)
group <- sort(grep("luc media group", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(group); xpend(group)
df$FullName[df$FullName %in% group] <- "Luc Media Group"; rm(group)
group <- sort(grep("jdmj", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(group); xpend(group)
df$FullName[df$FullName %in% group] <- "Jdmj Restaurant Group"; rm(group)
group <- sort(grep("hamilton group", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(group); xpend(group)
df$FullName[df$FullName %in% group] <- "The Hamilton Group"; rm(group)
group <- sort(grep("brain storm", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(group); xpend(group)
df$FullName[df$FullName %in% group] <- "Brain Storm Campaign Policy Group"; rm(group)
usps <- sort(grep("^u s postal office|united state postal service|united states post service|^uspostal service|^u s postal service|^uspo$|^u s p s$|usps|us post|postmaster|pastaster|post office|united states postal|u s postal service", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(usps); xpend(usps)
df$FullName[df$FullName %in% usps] <- "US Postal Service"; rm(usps)
valley <- sort(grep("valley breez", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(valley); xpend(valley)
df$FullName[df$FullName %in% valley] <- "Valley Breeze"; rm(valley)
advert <- sort(grep("robert lachance|ri lachance advertising|rj lachan", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(advert); xpend(advert)
df$FullName[df$FullName %in% advert] <- "RJ Lachance Advertising"; rm(advert)
advert <- sort(grep("lamar ad|lamar outdoor ad|^lamar$|lamar billboard|lamar comp|lamar enterprise|lamar prov", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(advert); xpend(advert)
df$FullName[df$FullName %in% advert] <- "Lamar Advertising"; rm(advert)
advert <- sort(grep("^siva advertising|silva advert|silvia advert|^silver advert", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(advert); xpend(advert)
df$FullName[df$FullName %in% advert] <- "Silva Advertising"; rm(advert)
crossroads <- sort(grep("^crossroads$|crossroads of ri|crossroads rhode|crossroads ri|crossroadsri$|crossrroads ri", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(crossroads); xpend(crossroads)
df$FullName[df$FullName %in% crossroads] <- "Crossroads RI"; rm(crossroads)
films <- sort(grep("upgrade films", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(films); xpend(films)
df$FullName[df$FullName %in% films] <- "Upgrade Films LLC"; rm(films)
daily <- sort(grep("^newport daily$|newport daily news|^the daily news$|newprt daily news|newport daily times", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(daily); xpend(daily)
df$FullName[df$FullName %in% daily] <- "Newport Daily News"; rm(daily)
public <- sort(grep("public affairs support|publica affairs support", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(public); xpend(public)
df$FullName[df$FullName %in% public] <- "Public Affairs Support Services"; rm(public)
devine <- sort(grep("devine mulve", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(devine); xpend(devine)
df$FullName[df$FullName %in% devine] <- "Devine Mulvey Longabaugh"; rm(devine)
mercury <- sort(grep("mecur print and mail|mercury mail|murcury printing|mercury mailing|mercurey press|mecury print|^mercury$|mercury print", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(mercury); xpend(mercury)
df$FullName[df$FullName %in% mercury] <- "Mercury Print and Mail"; rm(mercury)
prime <- sort(grep("prime cut|primecut", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(prime); xpend(prime)
df$FullName[df$FullName %in% prime] <- "Prime Cut Steakhouse"; rm(prime)
promail <- sort(grep("promail|^pro mail|^pro-mail|pro typing and mailing", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(promail); xpend(promail)
df$FullName[df$FullName %in% promail] <- "Promail"; rm(promail)
social <- sort(grep("tiempo social|tempo social", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(social); xpend(social)
df$FullName[df$FullName %in% social] <- "Tiempo Social Magazine"; rm(social)
roma <- sort(grep("^roma$|^roma llc|^roma food|via roma$|ristorante roma$|^roma prov|^roma rest", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpcost(roma); xpend(roma)
df$FullName[df$FullName %in% roma] <- "Via Roma"; rm(roma)
spiritos <- sort(grep("spiritos|spirit os restaurant|^spiritos$|^spirito$|^spirito gregg", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(spiritos)
df$FullName[df$FullName %in% spiritos] <- "Spiritos Restaurant"; rm(spiritos)
wines <- sort(grep("^wines and more", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(wines); xpcost(wines)
df$FullName[df$FullName %in% wines] <- "Wines and More"; rm(wines)
barrington <- sort(grep("barrington liq|barrignton liq|philip gasbarro liquors", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(barrington); xpcost(barrington)
df$FullName[df$FullName %in% barrington] <- "Barrington Liquors"; rm(barrington)
gasbarro <- sort(grep("gasbarro|^gasbaros$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(gasbarro); xpcost(gasbarro)
df$FullName[df$FullName %in% gasbarro] <- "Gasbarros Liquors"; rm(gasbarro)
wineand <- sort(grep("wine and cheese", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(wineand); xpcost(wineand)
df$FullName[df$FullName %in% wineand] <- "Wine and Cheese Restaurant"; rm(wineand)
aandt <- sort(grep("^a and t casali|^aandt|^a and t casale|^at casali", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(aandt); xpcost(aandt)
df$FullName[df$FullName %in% aandt] <- "A and T Casali Liquors"; rm(aandt)
haxton <- sort(grep("^haxton|haxtons liquor", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(haxton); xpcost(haxton)
df$FullName[df$FullName %in% haxton] <- "Haxtons Liquors"; rm(haxton)
imgan <- sort(grep("^i m gan$|im gan", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(imgan); xpcost(imgan)
df$FullName[df$FullName %in% imgan] <- "Im Gan Discount Liquors"; rm(imgan)
joyal <- sort(grep("joyal", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(joyal); xpcost(joyal)
df$FullName[df$FullName %in% joyal] <- "Joyals Liquors"; rm(joyal)
maliks <- sort(grep("maliks liq|maliks fine wine|malik liquor", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(maliks); xpcost(maliks)
df$FullName[df$FullName %in% maliks] <- "Maliks Liquors"; rm(maliks)
nocera <- sort(grep("lauren nocera|lauren s nocera", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(nocera); xpcost(nocera)
df$FullName[df$FullName %in% nocera] <- "Lauren Nocera"; rm(nocera)
saks <- sort(grep("saks", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(saks); xpcost(saks)
df$FullName[df$FullName %in% saks] <- "Saks Centerdale Wineseller"; rm(saks)
seekonk <- sort(grep("seekonk wine|seekonk liq|seekonk spirit", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(seekonk); xpcost(seekonk)
df$FullName[df$FullName %in% seekonk] <- "Seekonk Wine and Spirits"; rm(seekonk)
sween <- sort(grep("sweeneys wine|sweeneys nine and spirits|sweeneys liquor store", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sween); xpcost(sween)
df$FullName[df$FullName %in% sween] <- "Sweeneys Wine and Spirits"; rm(sween)
townwine <- sort(grep("^town wine", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(townwine); xpend(townwine); xpcost(townwine)
df$FullName[df$FullName %in% townwine] <- "Town Wine and Spirits"; rm(townwine)
regine <- sort(grep("regina printing|regime printing|regine priinting|regine prin|^regine$|^regines prin|regine prti|regineprinting", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(regine); xpcost(regine)
df$FullName[df$FullName %in% regine] <- "Regine Printing"; rm(regine)
nelabor <- sort(grep("new england labor", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(nelabor); xpcost(nelabor)
df$FullName[df$FullName %in% nelabor] <- "MA and Northern NE Laborers District Council"; rm(nelabor)
steven <- sort(grep("stevens reed", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(steven); xpcost(steven)
df$FullName[df$FullName %in% steven] <- "Stevens Reed Curcio and Co"; rm(steven)
staples <- sort(grep("staples", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(staples); xpcost(staples)
df$FullName[df$FullName %in% staples] <- "Staples"; rm(staples)
irs <- sort(grep("^irs| irs | irs$|internal rev", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(irs); xpcost(irs)
df$FullName[df$FullName %in% irs] <- "Internal Revenue Service"; rm(irs)
mclaughlin <- sort(grep("mclaughlin and assoc", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mclaughlin); xpcost(mclaughlin)
df$FullName[df$FullName %in% mclaughlin] <- "Mclaughlin and Assoc"; rm(mclaughlin)
screen <- sort(grep("screen strat", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(screen); xpcost(screen)
df$FullName[df$FullName %in% screen] <- "Screen Strategies Media"; rm(screen)
screen <- sort(grep("east coast screen|east coast printing", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(screen); xpcost(screen)
df$FullName[df$FullName %in% screen] <- "East Coast Screen Printing"; rm(screen)
petel <- sort(grep("petel and co|petal and company", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(petel); xpcost(petel)
df$FullName[df$FullName %in% petel] <- "Petel and Co"; rm(petel)
macwilliams <- sort(grep("macwilliams", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(macwilliams); xpcost(macwilliams)
df$FullName[df$FullName %in% macwilliams] <- "Macwilliams Robinson"; rm(macwilliams)
rising <- sort(grep("rising tide", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(rising); xpcost(rising)
df$FullName[df$FullName %in% rising] <- "Rising Tide Interactive"; rm(rising)
adelstein <- sort(grep("adelstein", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(adelstein); xpcost(adelstein)
df$FullName[df$FullName %in% adelstein] <- "Adelstein Liston"; rm(adelstein)
revolution <- sort(grep("revolution agency", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(revolution); xpcost(revolution)
df$FullName[df$FullName %in% revolution] <- "Revolution Agency"; rm(revolution)
amex <- sort(grep("^american express|american expres|amex", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(amex); xpcost(amex)
df$FullName[df$FullName %in% amex] <- "American Express"; rm(amex)
vista <- sort(grep("vistaprting|vistaprint|vista print|vistaprntcom", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(vista); xpcost(vista)
df$FullName[df$FullName %in% vista] <- "Vista Print"; rm(vista)
mission <- sort(grep("mission control", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mission); xpcost(mission)
df$FullName[df$FullName %in% mission] <- "Mission Control"; rm(mission)
slade <- sort(grep("joe slade", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(slade); xpcost(slade)
df$FullName[df$FullName %in% slade] <- "Joe Slade White and Co"; rm(slade)
merchant <- sort(grep("merchant bank|merchants bank", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(merchant); xpcost(merchant)
df$FullName[df$FullName %in% merchant] <- "Merchant Bank"; rm(merchant)
exped <- sort(grep("expedition strat", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(exped); xpcost(exped)
df$FullName[df$FullName %in% exped] <- "Expedition Strategies"; rm(exped)
eastbay <- sort(grep("^east bay newpapers|^e bay newspapers$|^eastbay news|^east bay news|^east bay paper", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(eastbay); xpcost(eastbay)
sak <- grep("sakonnet times", eastbay) %>% print
eastbay <- eastbay[-c(sak)];xpend(eastbay); xpcost(eastbay)
df$FullName[df$FullName %in% eastbay] <- "East Bay Newspapers"; rm(eastbay)
sakonnet <- sort(grep("sakonet times|sakonett times|sakonnet times|sakonnett times", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sakonnet); xpcost(sakonnet)
df$FullName[df$FullName %in% sakonnet] <- "Sakonnet Times"; rm(sakonnet)
warwickbeacon <- sort(grep("warwcik beacon|warwick beacon", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(warwickbeacon); xpcost(warwickbeacon)
df$FullName[df$FullName %in% warwickbeacon] <- "Warwick Beacon"; rm(warwickbeacon)
msa <- sort(grep("msa media", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(msa); xpcost(msa)
df$FullName[df$FullName %in% msa] <- "MSA Media Buying Inc"; rm(msa)
bankri <- sort(grep("^bankri|^bank of ri|^bank rhode island|^bank ri$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(bankri); xpcost(bankri)
df$FullName[df$FullName %in% bankri] <- "Bank RI"; rm(bankri)
treasury <- sort(grep("^us department of treasury|^u s treasury$|^us department of treasury$|united states treasury|^us treasury", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(treasury); xpcost(treasury)
df$FullName[df$FullName %in% treasury] <- "United States Treasury"; rm(treasury)
campaign <- sort(grep("campaign finance|camapign finance officer", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(campaign); xpcost(campaign)
df$FullName[df$FullName %in% campaign] <- "Campaign Finance Officers"; rm(campaign)
realtor <- sort(grep("national association of realtors", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(realtor); xpcost(realtor)
df$FullName[df$FullName %in% realtor] <- "National Association of Realtors"; rm(realtor)
canal <- sort(grep("canal partners media", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(canal); xpcost(canal)
df$FullName[df$FullName %in% canal] <- "Canal Partners Media"; rm(canal)
consumer <- sort(grep("voter\\/consumer research|voter consumer research", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(consumer); xpcost(consumer)
df$FullName[df$FullName %in% consumer] <- "Voter Consumer Research"; rm(consumer)
citizen <- sort(grep("citizens bank", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(citizen); xpcost(citizen)
df$FullName[df$FullName %in% citizen] <- "Citizens Bank"; rm(citizen)
view <- sort(grep("jay s goodman", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(view); xpcost(view)
df$FullName[df$FullName %in% view] <- "Jay S Goodman"; rm(view)
majority <- sort(grep("majority strat", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(majority); xpcost(majority)
df$FullName[df$FullName %in% majority] <- "Majority Strategies"; rm(majority)
trilogy <- sort(grep("trilogy inter", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(trilogy); xpcost(trilogy)
df$FullName[df$FullName %in% trilogy] <- "Trilogy Interactive"; rm(trilogy)
multimedia <- sort(grep("^e and r", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(multimedia); xpcost(multimedia)
df$FullName[df$FullName %in% multimedia] <- "E and R Multimedia Products"; rm(multimedia)
paul <- sort(grep("paul mac donald|paul mcdonald", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(paul); xpcost(paul)
df$FullName[df$FullName %in% paul] <- "Paul Macdonald"; rm(paul)
grassroot <- sort(grep("grassroot", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grassroot); xpcost(grassroot)
df$FullName[df$FullName %in% grassroot] <- "Grassroots Solutions"; rm(grassroot)
smk <- sort(grep("^smk|seth m klaiman", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(smk); xpcost(smk)
df$FullName[df$FullName %in% smk] <- "SMK Enterprises"; rm(smk)
julieand <- sort(grep("julie andrews|julie e andrews", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(julieand); xpcost(julieand)
df$FullName[df$FullName %in% julieand] <- "Julie E Andrews"; rm(julieand)
federalsign <- sort(grep("fedenne signs|federal hill signs|federal sign|hub fderal signs|hub federal inc|hub sign company|hub sign|hub-fed inc|hub-federal inc", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(federalsign); xpcost(federalsign)
df$FullName[df$FullName %in% federalsign] <- "Federal Signs"; rm(federalsign)
della <- sort(grep("susann g della", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(della); xpcost(della)
df$FullName[df$FullName %in% della] <- "Susann G Della Rosa"; rm(della)
twobolt <- sort(grep("twobolt|two bolt|^tow bow$|two-bolt", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(twobolt); xpcost(twobolt)
df$FullName[df$FullName %in% twobolt] <- "Twobolt"; rm(twobolt)
abbar <- sort(grep("abbar hutton", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(abbar); xpcost(abbar)
df$FullName[df$FullName %in% abbar] <- "Abbar Hutton Media"; rm(abbar)
ralston <- sort(grep("ralston", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(ralston); xpcost(ralston)
df$FullName[df$FullName %in% ralston] <- "Ralston Lapp Media"; rm(ralston)
strategy <- sort(grep("benenson", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(strategy); xpcost(strategy)
df$FullName[df$FullName %in% strategy] <- "Benenson Strategy Group"; rm(strategy)
strategy <- sort(grep("the strategy group", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(strategy); xpcost(strategy)
df$FullName[df$FullName %in% strategy] <- "The Strategy Group"; rm(strategy)
strategy <- sort(grep("^strategy group$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(strategy); xpcost(strategy)
df$FullName[df$FullName %in% strategy] <- "Strategy Group"; rm(strategy)
strategy <- sort(grep("Global Strat", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(strategy); xpcost(strategy)
df$FullName[df$FullName %in% strategy] <- "Global Strategy Group"; rm(strategy)
ramstad <- sort(grep("ramstad", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(ramstad); xpcost(ramstad)
df$FullName[df$FullName %in% ramstad] <- "Kathryn Ramstad-Albert"; rm(ramstad)
duffy <- sort(grep("james duffy", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(duffy); xpcost(duffy)
df$FullName[df$FullName %in% duffy] <- "James Duffy Co"; rm(duffy)
duffy <- sort(grep("duffy and shan", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(duffy); xpcost(duffy)
df$FullName[df$FullName %in% duffy] <- "Duffy and Shanley"; rm(duffy)
america <- sort(grep("^bank america$|^bank of america|bank of america$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(america); xpcost(america)
df$FullName[df$FullName %in% america] <- "Bank of America"; rm(america)
america <- sort(grep("american dream", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(america); xpcost(america)
df$FullName[df$FullName %in% america] <- "American Dream Fund"; rm(america)
america <- sort(grep("american media", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(america); xpcost(america)
df$FullName[df$FullName %in% america] <- "American Media and Advocacy Group"; rm(america)
america <- sort(grep("american air", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(america); xpcost(america)
df$FullName[df$FullName %in% america] <- "American Airlines"; rm(america)
america <- sort(grep("^ab signworks$|^a b signworks$|^american beauty|fleming 7 associates", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(america); xpcost(america)
df$FullName[df$FullName %in% america] <- "American Beauty Signworks"; rm(america)
sheahan <- sort(grep("sheahan print|sheehan print|sheahans print|shenhan print", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sheahan); xpcost(sheahan)
df$FullName[df$FullName %in% sheahan] <- "Sheahan Printing"; rm(sheahan)
fleming <- sort(grep("jospeh fleming|joseph t fleming|joseph fleming|joseph fleming|flemming and assoc|joseph fleming|fleming 7 assoc|^fleming$|joe flemming|joseph flemming|joseph t flemming|fleming and assic|fleming and assoc|^fleming assoc|fleming assic", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(fleming); xpcost(fleming)
df$FullName[df$FullName %in% fleming] <- "Fleming and Associates"; rm(fleming)
primegroup <- sort(grep("prime group", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(primegroup); xpcost(primegroup)
df$FullName[df$FullName %in% primegroup] <- "Prime Group"; rm(primegroup)
intuit <- sort(grep("^intuit$|^intuit supplies|^intuit inc|intuitcom", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(intuit); xpcost(intuit)
df$FullName[df$FullName %in% intuit] <- "Intuit"; rm(intuit)
kiley <- sort(grep("kiley", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(kiley); xpcost(kiley)
df$FullName[df$FullName %in% kiley] <- "Kiley and Company"; rm(kiley)
anzalone <- sort(grep("anzalone liszt|anzolone research", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(anzalone); xpcost(anzalone)
df$FullName[df$FullName %in% anzalone] <- "Anzalone Liszt Research"; rm(anzalone)
fieldworks <- sort(grep("field works", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(fieldworks); xpcost(fieldworks)
df$FullName[df$FullName %in% fieldworks] <- "Field Works LLC"; rm(fieldworks)
dixon <- sort(grep("dixon\\/davis", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(dixon); xpcost(dixon)
df$FullName[df$FullName %in% dixon] <- "Dixon Davis Media Group"; rm(dixon)
myers <- sort(grep("myers research", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(myers); xpcost(myers)
df$FullName[df$FullName %in% myers] <- "Myers Reseach"; rm(myers)
taxation <- sort(grep("^r i e t division of taxation|^division of taxation|ri division taxation|ri division of taxation|rhode island division of tax", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(taxation); xpcost(taxation)
df$FullName[df$FullName %in% taxation] <- "RI Division of Taxation"; rm(taxation)
pcs <- sort(grep("^pcs inc", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(pcs); xpcost(pcs)
df$FullName[df$FullName %in% pcs] <- "PCS Inc"; rm(pcs)
hyers <- sort(grep("eric hyers", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(hyers); xpcost(hyers)
df$FullName[df$FullName %in% hyers] <- "Eric Hyers"; rm(hyers)
walsworth <- sort(grep("waisworth landset|walsworth landset", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(walsworth); xpcost(walsworth)
df$FullName[df$FullName %in% walsworth] <- "Walsworth Landset Research"; rm(walsworth)
hart <- sort(grep("hart research", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(hart); xpcost(hart)
df$FullName[df$FullName %in% hart] <- "Hart Research Associates"; rm(hart)
metacomet <- sort(grep("metacomet", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(metacomet); xpcost(metacomet)
df$FullName[df$FullName %in% metacomet] <- "Metacomet Country Club"; rm(metacomet)
verdolino <- sort(grep("verdolino", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(verdolino); xpcost(verdolino)
df$FullName[df$FullName %in% verdolino] <- "Verdolino and Lowey"; rm(verdolino)
lombardi <- sort(grep("johnlombardi|john j lombardi|john lombardi", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(lombardi); xpcost(lombardi)
df$FullName[df$FullName %in% lombardi] <- "John J Lombardi"; rm(lombardi)
lombardi <- sort(grep("lombardi for senate|lombardi frank|frank s lombardi|frank lombardi", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(lombardi); xpcost(lombardi)
df$FullName[df$FullName %in% lombardi] <- "Frank S Lombardi"; rm(lombardi)
operating <- sort(grep("rigop victory fund|rhode island republican party|^ri gop$|rhode island gop$|^ri republican party", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(operating); xpcost(operating)
df$FullName[df$FullName %in% operating] <- "RI Republican Party Federal Operating Account"; rm(operating)
directmail <- sort(grep("direct mail|direct mai lmanager", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(directmail); xpcost(directmail)
df$FullName[df$FullName %in% directmail] <- "Direct Mail Manager"; rm(directmail)
mediapeel <- sort(grep("mediapeel", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mediapeel); xpcost(mediapeel)
df$FullName[df$FullName %in% mediapeel] <- "Mediapeel"; rm(mediapeel)
universityclub <- sort(grep("^university club$|the university club$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(universityclub); xpcost(universityclub)
df$FullName[df$FullName %in% universityclub] <- "The University Club"; rm(universityclub)
marriot <- sort(grep("marriott providence|downtown marriott providence|marriott providence downtown|marriott hotels providence|marriott downton providence|marriott downtown providence|^providence marriot", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(marriot); xpcost(marriot)
df$FullName[df$FullName %in% marriot] <- "Providence Marriot"; rm(marriot)
printsource <- sort(grep("printsource|print source", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(printsource); xpcost(printsource)
df$FullName[df$FullName %in% printsource] <- "Print Source Group"; rm(printsource)
sirspeedy <- sort(grep("sirspeedy|sir speedy", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sirspeedy); xpcost(sirspeedy)
df$FullName[df$FullName %in% sirspeedy] <- "Sir Speedy"; rm(sirspeedy)
facebook <- sort(grep("facebook|face book|faceboob", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(facebook); xpcost(facebook)
df$FullName[df$FullName %in% facebook] <- "Facebook"; rm(facebook)
bergman <- sort(grep("bergman", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(bergman); xpcost(bergman)
df$FullName[df$FullName %in% bergman] <- "Bergmann Zwerdling Direct"; rm(bergman)
gilbert <- sort(grep("william gilbert|william h gilbert", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(gilbert); xpcost(gilbert)
df$FullName[df$FullName %in% gilbert] <- "William H Gilbert"; rm(gilbert)
solidarity <- sort(grep("solidarity", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(solidarity); xpcost(solidarity)
df$FullName[df$FullName %in% solidarity] <- "Solidarity Strategies"; rm(solidarity)
martinelli <- sort(grep("vito martinelli", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(martinelli); xpcost(martinelli)
df$FullName[df$FullName %in% martinelli] <- "Vito Martinelli"; rm(martinelli)
clarity <- sort(grep("clarity", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(clarity); xpcost(clarity)
df$FullName[df$FullName %in% clarity] <- "Clarity Campaign Labs"; rm(clarity)
innov <- sort(grep("^fii marketing|financial inovations|financial innov", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(innov); xpcost(innov)
df$FullName[df$FullName %in% innov] <- "Financial Innovations"; rm(innov)
renai <- sort(grep("renaissance image|renaissance imagine|renaissace imaging|renaisance imaging|renaissance rimaging|renaisance|renaissance printing|renaaissance imaging|renaissance imaging", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(renai); xpcost(renai)
df$FullName[df$FullName %in% renai] <- "Renaissance Imaging"; rm(renai)
balloons <- sort(grep("borigraphix|^boris graphixs|^bori |^bori$|ballons over rhode island|ballons over ri|baloons over ri|balloon over ri|ballons over ri|ballons of ri|balloons over", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(balloons); xpcost(balloons)
df$FullName[df$FullName %in% balloons] <- "Balloons Over RI"; rm(balloons)
coie <- sort(grep("perkins coie", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(coie); xpcost(coie)
df$FullName[df$FullName %in% coie] <- "Perkins Coie"; rm(coie)
anderson <- sort(grep("anderson group", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(anderson); xpcost(anderson)
df$FullName[df$FullName %in% anderson] <- "The Anderson Group"; rm(anderson)
bargain <- sort(grep("^baragin buyer|^b argain buyer|bargain boyex|bargian buyer|bargain buy", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(bargain); xpcost(bargain)
df$FullName[df$FullName %in% bargain] <- "Bargain Buyer"; rm(bargain)
warwickmall <- sort(grep("warwick mall", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(warwickmall); xpcost(warwickmall)
df$FullName[df$FullName %in% warwickmall] <- "Warwick Mall"; rm(warwickmall)
cent <- sort(grep("century mail", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(cent); xpcost(cent)
df$FullName[df$FullName %in% cent] <- "Century Mailing"; rm(cent)
roach <- sort(grep("adam roach", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(roach); xpcost(roach)
df$FullName[df$FullName %in% roach] <- "Adam Roach"; rm(roach)
print <- sort(grep("express print", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(print); xpcost(print)
df$FullName[df$FullName %in% print] <- "Express Printing"; rm(print)
grid <- sort(grep("national grid|nation grid|nartional grid|narragansett electric", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grid); xpcost(grid)
df$FullName[df$FullName %in% grid] <- "National Grid"; rm(grid)
stanford <- sort(grep("stanford", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(stanford); xpcost(stanford)
df$FullName[df$FullName %in% stanford] <- "Stanford Campaigns"; rm(stanford)
dorrance <- sort(grep("dorance engraving|dorrance engrav", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(dorrance); xpcost(dorrance)
df$FullName[df$FullName %in% dorrance] <- "Dorrance Engraving"; rm(dorrance)
venda <- sort(grep("^constantinos$|costantinos venda bar|costantinos ristorante|^venda$|^venda |venda ravioli|^vendas cafe|^vendas$|costantino restaurant|constantinos ristorante|costantinos ristorante", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(venda); xpcost(venda)
df$FullName[df$FullName %in% venda] <- "Venda Ravioli"; rm(venda)
siena <- sort(grep("siena|sienna", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(siena); xpcost(siena)
df$FullName[df$FullName %in% siena] <- "Siena Restaurant"; rm(siena)
civita <- sort(grep("civita|angelo restaurant|^angelos restaurant", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(civita); xpcost(civita)
df$FullName[df$FullName %in% civita] <- "Angelos Civita Farnese"; rm(civita)
sciallo <- sort(grep("^scialo brothers|scialos$|^scialos bros|^scialos bakery|scialo bros", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sciallo); xpcost(sciallo)
df$FullName[df$FullName %in% sciallo] <- "Scialo Bros Bakery"; rm(sciallo)
canteen <- sort(grep("old canteen|old cateen|old ccanteen", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(canteen); xpcost(canteen)
df$FullName[df$FullName %in% canteen] <- "The Old Canteen"; rm(canteen)
caserta <- sort(grep("^caserta$|caserta pizz|casserta pizz|casertas pizza", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(caserta); xpcost(caserta)
df$FullName[df$FullName %in% caserta] <- "Caserta Pizza"; rm(caserta)
pizza <- sort(grep("tommys pizza|tommy pizza|^tommys$|tommys fine italian food|tommys pizzeria", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(pizza); xpcost(pizza)
df$FullName[df$FullName %in% pizza] <- "Tommys Pizza"; rm(pizza)
pizza <- sort(grep("providence coal fire pizza|coal fired pizza|prov coal-fired pizza|providence coal fired pizza|providence coal-fired pizza", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(pizza); xpcost(pizza)
df$FullName[df$FullName %in% pizza] <- "Providence Coal Fire Pizza"; rm(pizza)
pizza <- sort(grep("laguenshamburgerstone", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(pizza); xpcost(pizza)
df$FullName[df$FullName %in% pizza] <- "Laguenshamburgerstone"; rm(pizza)
bluegrotto <- sort(grep("blue grotto", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(bluegrotto); xpcost(bluegrotto)
df$FullName[df$FullName %in% bluegrotto] <- "Blue Grotto"; rm(bluegrotto)
camille <- sort(grep("^camiles$|^camilles$|^camiles rest|^camilles rest|^camilles on the hill|^camilles roman", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(camille); xpcost(camille)
df$FullName[df$FullName %in% camille] <- "Camilles"; rm(camille)
grille <- sort(grep("capital grill|capitol grill", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Capital Grille"; rm(grille)
grille <- sort(grep("waterman grill|warerman grill", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Waterman Grille"; rm(grille)
grille <- sort(grep("cactus grill", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Cactus Grille"; rm(grille)
grille <- sort(grep("^orourkes pub$|^orourkes$|oroukes bar|oroarkes bar|orourkes bar", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "O'Rourkes Bar and Grill"; rm(grille)
grille <- sort(grep("^z bar|^z-bar", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Z Bar and Grille"; rm(grille)
grille <- sort(grep("ladder 133|ladders 133|ladder 33", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Ladder 133"; rm(grille)
grille <- sort(grep("chapel grill", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Chapel Grille"; rm(grille)
grille <- sort(grep("^asia grill|^asian grille|^asia restaurant$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Asia Grille"; rm(grille)
grille <- sort(grep("wicked good", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Wicked Good Sports Bar and Grill"; rm(grille)
grille <- sort(grep("blooperz|bloopers", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Blooperz Bar and Grille"; rm(grille)
grille <- sort(grep("^carries", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Carries Restaurant"; rm(grille)
grille <- sort(grep("harris bar|harris grill", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Harris Bar and Grill"; rm(grille)
grille <- sort(grep("italian americang|italo american grille", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Italian American Grille"; rm(grille)
grille <- sort(grep("cattails", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Cattails City Grille"; rm(grille)
grille <- sort(grep("atwood grill", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Atwood Grille"; rm(grille)
grille <- sort(grep("^luckys bar|^luckys$|^luckys american bar", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Luckys Bar and Grille"; rm(grille)
grille <- sort(grep("joes american|joes americal bar", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Joes American Bar and Grill"; rm(grille)
grille <- sort(grep("^spicolis$|^spicolis bar|^spicolis grill", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Spicolis Bar and Grill"; rm(grille)
grille <- sort(grep("chubby", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Chubbys Bar and Grille"; rm(grille)
grille <- sort(grep("tyler point|tyler pt", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Tyler Point Grille"; rm(grille)
grille <- sort(grep("^chophouse|^chop house|^the chophouse", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Chophouse Grille"; rm(grille)
grille <- sort(grep("prata bar", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Prata Bar and Grille"; rm(grille)
grille <- sort(grep("napa valley", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Napa Valley Grille"; rm(grille)
grille <- sort(grep("sonoma grill|sonoma bis", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Sonoma Grille"; rm(grille)
grille <- sort(grep("marina grill|marina cafe|marina pub", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Marina Cafe"; rm(grille)
grille <- sort(grep("grill 47|grille 47|the call sports|grill 77", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "The Call Grill 47"; rm(grille)
grille <- sort(grep("overtime|over time", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Overtime Bar and Grille"; rm(grille)
grille <- sort(grep("two ten", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Two Ten Oyster Bar"; rm(grille)
grille <- sort(grep("bienvenidas lat|^bienvenidas", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Bienvenidas Latin Grill"; rm(grille)
grille <- sort(grep("^chellos$|chelos|^cellos$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grille); xpcost(grille)
df$FullName[df$FullName %in% grille] <- "Chelos Restaurant"; rm(grille)
alforno <- sort(grep("alforno|al forno", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(alforno); xpcost(alforno)
df$FullName[df$FullName %in% alforno] <- "Al Forno Restaurant"; rm(alforno)
medit <- sort(grep("mediteraneo|mediterranean|meditteraneo|mediterraneo|medterreano|medit", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(medit); xpcost(medit)
df$FullName[df$FullName %in% medit] <- "Mediterraneo"; rm(medit)
andino <- sort(grep("andinos", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(andino); xpcost(andino)
df$FullName[df$FullName %in% andino] <- "Andinos Restaurant"; rm(andino)
twinoak <- sort(grep("twinoak|twin oak", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(twinoak); xpcost(twinoak)
df$FullName[df$FullName %in% twinoak] <- "Twin Oaks"; rm(twinoak)
publickitchen <- sort(grep("publickitchen", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(publickitchen); xpcost(publickitchen)
df$FullName[df$FullName %in% publickitchen] <- "publickitchen"; rm(publickitchen)
southstreet <- sort(grep("south street cafe|soule st cafe|sout st cafe|south st cafe", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(southstreet); xpcost(southstreet)
df$FullName[df$FullName %in% southstreet] <- "South Street Cafe"; rm(southstreet)
nuovo <- sort(grep("cafe nouvo|cafe nuovo", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(nuovo); xpcost(nuovo)
df$FullName[df$FullName %in% nuovo] <- "Cafe Nuovo"; rm(nuovo)
gators <- sort(grep("gators", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(gators); xpcost(gators)
df$FullName[df$FullName %in% gators] <- "Gators Pub"; rm(gators)
cafeg <- sort(grep("cafe g-tech|cafe gtech|cafe g tech|g-tech cafe|cafe igt", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(cafeg); xpcost(cafeg)
df$FullName[df$FullName %in% cafeg] <- "Cafe Gtech"; rm(cafeg)
butchershop <- sort(grep("butcher shop", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(butchershop); xpcost(butchershop)
df$FullName[df$FullName %in% butchershop] <- "The Butcher Shop Cafe and Deli"; rm(butchershop)
pinellis <- sort(grep("bpinellis|b pinellis", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(pinellis); xpcost(pinellis)
df$FullName[df$FullName %in% pinellis] <- "B Pinellis"; rm(pinellis)
pinellis <- sort(grep("pinellis north|pinellis rest|pinellis resturant|north end cafe", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(pinellis); xpcost(pinellis)
df$FullName[df$FullName %in% pinellis] <- "Pinellis North End Cafe"; rm(pinellis)
pjs <- sort(grep("^pjs$|^pjs pub|^p js pub$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(pjs); xpcost(pjs)
df$FullName[df$FullName %in% pjs] <- "PJs Pub"; rm(pjs)
mudvill <- sort(grep("mudville", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mudvill); xpcost(mudvill)
df$FullName[df$FullName %in% mudvill] <- "Mudville Pub"; rm(mudvill)
sunflower <- sort(grep("sunflower cafe", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sunflower); xpcost(sunflower)
df$FullName[df$FullName %in% sunflower] <- "Sunflower Cafe"; rm(sunflower)
littlecafe <- sort(grep("kellis a little cafe|^little cafe$|^a little cafe$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(littlecafe); xpcost(littlecafe)
df$FullName[df$FullName %in% littlecafe] <- "Little Cafe"; rm(littlecafe)
subway <- sort(grep("subway", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(subway); xpcost(subway)
df$FullName[df$FullName %in% subway] <- "Subway"; rm(subway)
sandwich <- sort(grep("sandwich hut", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sandwich); xpcost(sandwich)
df$FullName[df$FullName %in% sandwich] <- "Sandwich Hut"; rm(sandwich)
roccos <- sort(grep("roccos pub", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(roccos); xpcost(roccos)
df$FullName[df$FullName %in% roccos] <- "Roccos Pub"; rm(roccos)
upriver <- sort(grep("up river", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(upriver); xpcost(upriver)
df$FullName[df$FullName %in% upriver] <- "The Up River Cafe"; rm(upriver)
kemp <- sort(grep("^kempennars|^kempenarrs|^kempenaars|^kempernaars", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(kemp); xpcost(kemp)
df$FullName[df$FullName %in% kemp] <- "Kempennars Clambake Club"; rm(kemp)
mcgrath <- sort(grep("mcgrath clam|mcgraths clam", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mcgrath); xpcost(mcgrath)
df$FullName[df$FullName %in% mcgrath] <- "Mcgrath Clambakes"; rm(mcgrath)
lasalle <- sort(grep("lasalle bake|la salle bake", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(lasalle); xpcost(lasalle)
df$FullName[df$FullName %in% lasalle] <- "Lasalle Bakery"; rm(lasalle)
palmier <- sort(grep("dipalmeiris bake|dipalmeiris bake|palmieris bake|palmieri bake|^palmieri$|^d palmieris$|^d palmeiries|^palmieris pizza", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(palmier); xpcost(palmier)
df$FullName[df$FullName %in% palmier] <- "D Palmieris Bakery"; rm(palmier)
deluise <- sort(grep("de luise bakery|deluise", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(deluise); xpcost(deluise)
df$FullName[df$FullName %in% deluise] <- "Deluise Bakery"; rm(deluise)
bachini <- sort(grep("bachini", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(bachini); xpcost(bachini)
df$FullName[df$FullName %in% bachini] <- "Bachini Bakery"; rm(bachini)
emillios <- sort(grep("emilios", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(emillios); xpcost(emillios)
df$FullName[df$FullName %in% emillios] <- "Emilios Bakery"; rm(emillios)
antonios <- sort(grep("antonios home bakery|antonios bake", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(antonios); xpcost(antonios)
df$FullName[df$FullName %in% antonios] <- "Antonios Bakery"; rm(antonios)
bakemy <- sort(grep("bake my", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(bakemy); xpcost(bakemy)
df$FullName[df$FullName %in% bakemy] <- "Bake My Day"; rm(bakemy)
depetril <- sort(grep("^deperrillos bake|^depetril", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(depetril); xpcost(depetril)
df$FullName[df$FullName %in% depetril] <- "Depetrillos Pizza and Bakery"; rm(depetril)
sevenstar <- sort(grep("seven star|^seven$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sevenstar); xpcost(sevenstar)
df$FullName[df$FullName %in% sevenstar] <- "Seven Stars Bakery"; rm(sevenstar)
barand <- sort(grep("daves bar and|^daves grill$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(barand); xpcost(barand)
df$FullName[df$FullName %in% barand] <- "Daves Bar and Grill"; rm(barand)
dining <- sort(grep("ri college dining|rr college dining", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(dining); xpcost(dining)
df$FullName[df$FullName %in% dining] <- "RI College Dining Services"; rm(dining)
brick <- sort(grep("brick al", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(brick); xpcost(brick)
df$FullName[df$FullName %in% brick] <- "Brick Alley Pub"; rm(brick)
domino <- sort(grep("domino", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(domino); xpcost(domino)
df$FullName[df$FullName %in% domino] <- "Dominos Pizza"; rm(domino)
hut <- sort(grep("pizza hut", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(hut); xpcost(hut)
df$FullName[df$FullName %in% hut] <- "Pizza Hut"; rm(hut)
legalsea <- sort(grep("legal sea", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(legalsea); xpcost(legalsea)
df$FullName[df$FullName %in% legalsea] <- "Legal Sea Foods"; rm(legalsea)
mickey <- sort(grep("mickeys g clamshack|^mikes clam shack$|^micky gs$|micky gs catering|mickeys gs|mickey gs|gs clam", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mickey); xpcost(mickey)
df$FullName[df$FullName %in% mickey] <- "Mickey GS Clam Shack"; rm(mickey)
lobsterpot <- sort(grep("lobsterpot|lobster pot", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(lobsterpot); xpcost(lobsterpot)
df$FullName[df$FullName %in% lobsterpot] <- "The Lobster Pot"; rm(lobsterpot)
cartersea <- sort(grep("carter sea|carters sea", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(cartersea); xpcost(cartersea)
df$FullName[df$FullName %in% cartersea] <- "Carters Sea Food"; rm(cartersea)
tuns <- sort(grep("tuns clam", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(tuns); xpcost(tuns)
df$FullName[df$FullName %in% tuns] <- "Tuns Clam Bake Co"; rm(tuns)
blount <- sort(grep("blount", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(blount); xpcost(blount)
df$FullName[df$FullName %in% blount] <- "Blount Clam Shack"; rm(blount)
lawton <- sort(grep("shawna lawton", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(lawton); xpcost(lawton)
df$FullName[df$FullName %in% lawton] <- "Shawna Lawton"; rm(lawton)
ruths <- sort(grep("^ruths$|^ruths chris|^ruth chris", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(ruths); xpcost(ruths)
df$FullName[df$FullName %in% ruths] <- "Ruths Chris Steak House"; rm(ruths)
emerys <- sort(grep("^emerys food|^emerys dining|^emerys cat|emery catering|emery dining|emorys cater|emorys dining", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(emerys); xpcost(emerys)
df$FullName[df$FullName %in% emerys] <- "Emerys Catering"; rm(emerys)
stopand <- sort(grep("stop 7 shop|stop and sthop|stopandshop|^sos$|stop and shop|^stio and shop|^stop and stop|^stop n shop|stop-n-shop|stop andshop", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(stopand); xpcost(stopand)
df$FullName[df$FullName %in% stopand] <- "Stop and Shop"; rm(stopand)
dunkin <- sort(grep("dunkin", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(dunkin); xpcost(dunkin)
df$FullName[df$FullName %in% dunkin] <- "Dunkin Donuts"; rm(dunkin)
hanley <- sort(grep("hanleys ale|hanley ale|^hanleys$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(hanley); xpcost(hanley)
df$FullName[df$FullName %in% hanley] <- "Hanleys Ale House"; rm(hanley)
pranzi <- sort(grep("^pranzi", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(pranzi); xpcost(pranzi)
df$FullName[df$FullName %in% pranzi] <- "Pranzi Catering"; rm(pranzi)
morin <- sort(grep("russell moran fine|^morins inc$|morin fine|morin cater|russell morin|russ morin|russel morin|rusell morin", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(morin); xpcost(morin)
df$FullName[df$FullName %in% morin] <- "Russel Morin Fine Catering"; rm(morin)
greggs <- sort(grep("^gregg rest|^greggs|gregs restruant|gregg restaurant|gregs restaurant|gresss restaurant", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(greggs); xpcost(greggs)
df$FullName[df$FullName %in% greggs] <- "Greggs Restaurant"; rm(greggs)
silvios <- sort(grep("^silvios$|^silvios rest|^silviois cafe|^s silvio", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(silvios); xpcost(silvios)
df$FullName[df$FullName %in% silvios] <- "Silvios Restaurant"; rm(silvios)
capriccio <- sort(grep("^cappriccio|^capriccio|^capricio", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(capriccio); xpcost(capriccio)
df$FullName[df$FullName %in% capriccio] <- "Capriccio Restaurant"; rm(capriccio)
quonset <- sort(grep("quonset o", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(quonset); xpcost(quonset)
df$FullName[df$FullName %in% quonset] <- "Quonset O Club"; rm(quonset)
jackies <- sort(grep("^jackies$|^jackies galaxy|^jackies waterplace|^jackys galaxie|^jackys ", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(jackies); xpcost(jackies)
df$FullName[df$FullName %in% jackies] <- "Jackies Galaxy"; rm(jackies)
ninos <- sort(grep("^ninos|ninoo restaurant|nnos on lake tiogue", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(ninos); xpcost(ninos)
df$FullName[df$FullName %in% ninos] <- "Ninos on Lake Tiogue"; rm(ninos)
daves <- sort(grep("^dawes market|davges market|davesmarketplace|^daves$|^daves market|daves gift basket", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(daves); xpcost(daves)
df$FullName[df$FullName %in% daves] <- "Daves Marketplace"; rm(daves)
fredand <- sort(grep("fred and steves", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(fredand); xpcost(fredand)
df$FullName[df$FullName %in% fredand] <- "Fred and Steves Steak House"; rm(fredand)
atomic <- sort(grep("atomic cater", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(atomic); xpcost(atomic)
df$FullName[df$FullName %in% atomic] <- "Atomic Catering"; rm(atomic)
restaurantdepot <- sort(grep("restaurant depot|^rest depot|^resturant depot", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(restaurantdepot); xpcost(restaurantdepot)
df$FullName[df$FullName %in% restaurantdepot] <- "Restaurant Depot"; rm(restaurantdepot)
valleyin <- sort(grep("^west valey inn$|west valley inn|west vally inn|west valley club", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(valleyin); xpcost(valleyin)
df$FullName[df$FullName %in% valleyin] <- "West Valley Inn"; rm(valleyin)
basta <- sort(grep("^basta", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(basta); xpcost(basta)
df$FullName[df$FullName %in% basta] <- "Basta Restaurant"; rm(basta)
justellen <- sort(grep("just ellen", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(justellen); xpcost(justellen)
df$FullName[df$FullName %in% justellen] <- "Just Ellens"; rm(justellen)
riviera <- sort(grep("^riviera|^riveira inn|^riveria rest", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(riviera); xpcost(riviera)
df$FullName[df$FullName %in% riviera] <- "Riviera Inn"; rm(riviera)
meeting <- sort(grep("meeting st", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(meeting); xpcost(meeting)
df$FullName[df$FullName %in% meeting] <- "Meeting Street"; rm(meeting)
putnampart <- sort(grep("putnam partners", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(putnampart); xpcost(putnampart)
df$FullName[df$FullName %in% putnampart] <- "Putnam Partners"; rm(putnampart)
flexpoint <- sort(grep("flexpoint", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(flexpoint); xpcost(flexpoint)
df$FullName[df$FullName %in% flexpoint] <- "Flexpoint Media"; rm(flexpoint)
jonathanblair <- sort(grep("jonathan blair", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(jonathanblair); xpcost(jonathanblair)
df$FullName[df$FullName %in% jonathanblair] <- "Jonathan Blair"; rm(jonathanblair)
deedee <- sort(grep("dee dee wit|dianne s witman|dianne witman", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(deedee); xpcost(deedee)
df$FullName[df$FullName %in% deedee] <- "Dee Dee Witman"; rm(deedee)
jonesmandel <- sort(grep("jones mandel", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(jonesmandel); xpcost(jonesmandel)
df$FullName[df$FullName %in% jonesmandel] <- "Jones Mandel"; rm(jonesmandel)
civis <- sort(grep("civis analytics", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(civis); xpcost(civis)
df$FullName[df$FullName %in% civis] <- "Civis Analytics"; rm(civis)
lakeresearch <- sort(grep("lake research", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(lakeresearch); xpcost(lakeresearch)
df$FullName[df$FullName %in% lakeresearch] <- "Lake Research Partners"; rm(lakeresearch)
ortiz <- sort(grep("david ortiz|davidd ortiz", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(ortiz); xpcost(ortiz)
df$FullName[df$FullName %in% ortiz] <- "David Ortiz"; rm(ortiz)
ronknox <- sort(grep("ron knox", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(ronknox); xpcost(ronknox)
df$FullName[df$FullName %in% ronknox] <- "Ron Knox"; rm(ronknox)
emc <- sort(grep("^emc insurance|^emc$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(emc); xpcost(emc)
df$FullName[df$FullName %in% emc] <- "EMC Insurance"; rm(emc)
emc <- sort(grep("emc research", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(emc); xpcost(emc)
df$FullName[df$FullName %in% emc] <- "EMC Research"; rm(emc)
hollydavis <- sort(grep("holly davis", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(hollydavis); xpcost(hollydavis)
df$FullName[df$FullName %in% hollydavis] <- "Holly Davis"; rm(hollydavis)
gabrielamo <- sort(grep("gabriel amo|gabe amo", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(gabrielamo); xpcost(gabrielamo)
df$FullName[df$FullName %in% gabrielamo] <- "Gabriel Amo"; rm(gabrielamo)
quinlan <- sort(grep("patrick j quinlan|patrick quinlan", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(quinlan); xpcost(quinlan)
df$FullName[df$FullName %in% quinlan] <- "Patrick J Quinlan"; rm(quinlan)
actblue <- sort(grep("actblue|^act blue", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(actblue); xpcost(actblue)
df$FullName[df$FullName %in% actblue] <- "Actblue"; rm(actblue)
targeted <- sort(grep("targeted strat", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(targeted); xpcost(targeted)
df$FullName[df$FullName %in% targeted] <- "Targeted Strategies"; rm(targeted)
paquin <- sort(grep("paquin iii|robert a paquin|robert paquin", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(paquin); xpcost(paquin)
df$FullName[df$FullName %in% paquin] <- "Robert Paquin III"; rm(paquin)
vantiv <- sort(grep("vantiv", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(vantiv); xpcost(vantiv)
df$FullName[df$FullName %in% vantiv] <- "Vantiv Ecommerce"; rm(vantiv)
google <- sort(grep("google|alphabet", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(google); xpcost(google)
df$FullName[df$FullName %in% google] <- "Google"; rm(google)
walmart <- sort(grep("walmart|wal-mart|wall-mart|wal mart", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(walmart); xpcost(walmart)
df$FullName[df$FullName %in% walmart] <- "Walmart"; rm(walmart)
constant <- sort(grep("constant contact", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(constant); xpcost(constant)
df$FullName[df$FullName %in% constant] <- "Constant Contact"; rm(constant)
nationbuilder <- sort(grep("nationbuilder|nation builder", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(nationbuilder); xpcost(nationbuilder)
df$FullName[df$FullName %in% nationbuilder] <- "Nationbuilder"; rm(nationbuilder)
firstbank <- sort(grep("first bank", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(firstbank); xpcost(firstbank)
df$FullName[df$FullName %in% firstbank] <- "First Bank"; rm(firstbank)
bank <- sort(grep("chase bank", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(bank); xpcost(bank)
df$FullName[df$FullName %in% bank] <- "Chase Bankcard"; rm(bank)
data <- sort(grep("datapay", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(data); xpcost(data)
df$FullName[df$FullName %in% data] <- "Datapay"; rm(data)
data <- sort(grep("first data$|first data usa", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(data); xpcost(data)
df$FullName[df$FullName %in% data] <- "First Data USA"; rm(data)
data <- sort(grep("political data", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(data); xpcost(data)
df$FullName[df$FullName %in% data] <- "Political Data Inc"; rm(data)
data <- sort(grep("datafind", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(data); xpcost(data)
df$FullName[df$FullName %in% data] <- "Datafinder"; rm(data)
data <- sort(grep("peachtree data", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(data); xpcost(data)
df$FullName[df$FullName %in% data] <- "Peachtree Data"; rm(data)
data <- sort(grep("mellissa data|melissa data", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(data); xpcost(data)
df$FullName[df$FullName %in% data] <- "Melissa Data Corp"; rm(data)
data <- sort(grep("variable data|variable date", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(data); xpcost(data)
df$FullName[df$FullName %in% data] <- "Variable Data Printing"; rm(data)
data <- sort(grep("peacock data", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(data); xpcost(data)
df$FullName[df$FullName %in% data] <- "Peacock Data"; rm(data)
stmarysfeast <- sort(grep("saint marys feast|st marys feast|st mary feast", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(stmarysfeast); xpcost(stmarysfeast)
df$FullName[df$FullName %in% stmarysfeast] <- "St Marys Feast Society"; rm(stmarysfeast)
saint <- sort(grep("st kevin", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(saint); xpcost(saint)
df$FullName[df$FullName %in% saint] <- "St Kevins Church"; rm(saint)
saint <- sort(grep("st rita", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(saint); xpcost(saint)
df$FullName[df$FullName %in% saint] <- "St Ritas Church"; rm(saint)
saint <- sort(grep("josephs school", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(saint); xpcost(saint)
df$FullName[df$FullName %in% saint] <- "St Josephs School"; rm(saint)
saint <- sort(grep("lady of grace|^olg holy name society", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(saint); xpcost(saint)
df$FullName[df$FullName %in% saint] <- "Our Lady of Grace Church"; rm(saint)
saint <- sort(grep("catholic charities|the providence visitor|catholic charity|diocese of providence|diocesan administration corp|rhode island catholic", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(saint); xpcost(saint)
df$FullName[df$FullName %in% saint] <- "Rhode Island Catholic"; rm(saint)
disanto <- sort(grep("disanto priest", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(disanto); xpcost(disanto)
df$FullName[df$FullName %in% disanto] <- "Disanto Priest and Co"; rm(disanto)
saint <- sort(grep("saint anns church|saint ann church|st annes church|st anns church|st anns parish|st anns school", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(saint); xpcost(saint)
df$FullName[df$FullName %in% saint] <- "St Anns Church"; rm(saint)
saint <- sort(grep("st bartholomews society|saint barts society|st barts club|st barts rest|saint barts club|saint barts rest", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(saint); xpcost(saint)
df$FullName[df$FullName %in% saint] <- "St Barts Club"; rm(saint)
saint <- sort(grep("st bartholomeus|saint bartholomew|st bartholomew", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(saint); xpcost(saint)
df$FullName[df$FullName %in% saint] <- "St Bartholomew Church"; rm(saint)
saint <- sort(grep("saint marys home|st marys home", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(saint); xpcost(saint)
df$FullName[df$FullName %in% saint] <- "St Marys Home for Children"; rm(saint)
saint <- sort(grep("st roccos|st rocco", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(saint); xpcost(saint)
df$FullName[df$FullName %in% saint] <- "St Roccos School"; rm(saint)
saint <- sort(grep("holy ghost church", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(saint); xpcost(saint)
df$FullName[df$FullName %in% saint] <- "Holy Ghost Church"; rm(saint)
saint <- sort(grep("st lucy", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(saint); xpcost(saint)
df$FullName[df$FullName %in% saint] <- "St Lucy Church"; rm(saint)
saint <- sort(grep("pbvm church|church of the prese|churh of the presentation of the blessed virgin", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(saint); xpcost(saint)
df$FullName[df$FullName %in% saint] <- "Church of the Presentation"; rm(saint)
saint <- sort(grep("saint anthonys club", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(saint); xpcost(saint)
df$FullName[df$FullName %in% saint] <- "Saint Anthonys Club"; rm(saint)
saint <- sort(grep("straphaels academy|st raphaels academy|saint raphael academy|st raphael academy", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(saint); xpcost(saint)
df$FullName[df$FullName %in% saint] <- "St Raphael Academy"; rm(saint)
saint <- sort(grep("stteresas|st teresa", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(saint); xpcost(saint)
df$FullName[df$FullName %in% saint] <- "St Teresas Church"; rm(saint)
saint <- sort(grep("st marys church|st marys school|saint marys holy", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(saint); xpcost(saint)
df$FullName[df$FullName %in% saint] <- "St Marys Church"; rm(saint)
saint <- sort(grep("saint agnes", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(saint); xpcost(saint)
df$FullName[df$FullName %in% saint] <- "Saint Agnes Church"; rm(saint)
# saint <- sort(grep("st anthony holy name society|^st anthony$|st anthonys society|st anthonys club|st anthony society|saint anthony society|saint anthonys society", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(saint); xpcost(saint)
# df$FullName[df$FullName %in% saint] <- "saint"; rm(saint)
# saint <- sort(grep("st anthony church|saint anthonys church|saint anthony church|st anthonys church|st anthony holy name society", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(saint); xpcost(saint)
# df$FullName[df$FullName %in% saint] <- "saint"; rm(saint)
temp <- sort(grep("temple street restaurant|temple downtown|temple rest|temple bar|^temple$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(temp); xpcost(temp)
df$FullName[df$FullName %in% temp] <- "Temple Downtown Restaurant"; rm(temp)
boardofelect <- sort(grep("^ribe$|^riboe| boe$|ri boe$|^boe$|ri b of e|ri boardof elect|broard of elect|board of elect", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(boardofelect); xpcost(boardofelect)
df$FullName[df$FullName %in% boardofelect] <- "Board of Elections"; rm(boardofelect)
southwest <- sort(grep("southwast airlines|^sw airline|southwest$|southwestcom|southwest air", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(southwest); xpcost(southwest)
df$FullName[df$FullName %in% southwest] <- "Southwest Airlines"; rm(southwest)
airline <- sort(grep("^us air|^u s airways", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(airline); xpcost(airline)
df$FullName[df$FullName %in% airline] <- "US Airways"; rm(airline)
airline <- sort(grep("delta air", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(airline); xpcost(airline)
df$FullName[df$FullName %in% airline] <- "Delta Airlines"; rm(airline)
airline <- sort(grep("united air", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(airline); xpcost(airline)
df$FullName[df$FullName %in% airline] <- "United Airlines"; rm(airline)
airline <- sort(grep("northwest airline", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(airline); xpcost(airline)
df$FullName[df$FullName %in% airline] <- "Northwest Airline"; rm(airline)
airline <- sort(grep("virgin america air", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(airline); xpcost(airline)
df$FullName[df$FullName %in% airline] <- "Virgin America Airline"; rm(airline)
airline <- sort(grep("alaska airline", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(airline); xpcost(airline)
df$FullName[df$FullName %in% airline] <- "Alaska Airlines"; rm(airline)
tfgreen <- sort(grep("^providence apirport|^providence airport$|^t f green airport|tf green", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(tfgreen); xpcost(tfgreen)
df$FullName[df$FullName %in% tfgreen] <- "TF Green Airport"; rm(tfgreen)
senate <- sort(grep("senate dem|democratic senate", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(senate); xpcost(senate)
df$FullName[df$FullName %in% senate] <- "Senate Democrats"; rm(senate)
senate <- sort(grep("senate republican majority|senate repub|senate gop", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(senate); xpcost(senate)
df$FullName[df$FullName %in% senate] <- "Senate Republicans"; rm(senate)
house <- sort(grep("wes rib|wes’ rib|wess rib|wests rib", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(house); xpcost(house)
df$FullName[df$FullName %in% house] <- "Wes Rib House"; rm(house)
sprint <- sort(grep("^sprint|sprint$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sprint); xpcost(sprint)
df$FullName[df$FullName %in% sprint] <- "Sprint"; rm(sprint)
godaddy <- sort(grep("godaddy|^go daddy", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(godaddy); xpcost(godaddy)
df$FullName[df$FullName %in% godaddy] <- "Godaddycom"; rm(godaddy)
kathleena <- sort(grep("kathleena", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(kathleena); xpcost(kathleena)
df$FullName[df$FullName %in% kathleena] <- "Kathleena Madera"; rm(kathleena)
guild <- sort(grep("paper guild", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(guild); xpcost(guild)
df$FullName[df$FullName %in% guild] <- "Providence Newspaper Guild"; rm(guild)
winning <- sort(grep("winning co", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(winning); xpcost(winning)
df$FullName[df$FullName %in% winning] <- "Winning Connections"; rm(winning)
westerlysun <- sort(grep("westerlysun|westerly sun|^the sun |^the sun-|^sun graphics", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(westerlysun); xpcost(westerlysun)
df$FullName[df$FullName %in% westerlysun] <- "The Westerly Sun"; rm(westerlysun)
wnri <- sort(grep("wnri|bouchard broadcast|^w n r i|^wnre radio", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(wnri); xpcost(wnri)
df$FullName[df$FullName %in% wnri] <- "WNRI"; rm(wnri)
wpro <- sort(grep("wpro", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(wpro); xpcost(wpro)
df$FullName[df$FullName %in% wpro] <- "WPRO"; rm(wpro)
w <- sort(grep("wadk", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(w); xpcost(w)
df$FullName[df$FullName %in% w] <- "WADK"; rm(w)
w <- sort(grep("wbru", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(w); xpcost(w)
df$FullName[df$FullName %in% w] <- "WBRU"; rm(w)
w <- sort(grep("wkkb", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(w); xpcost(w)
df$FullName[df$FullName %in% w] <- "WKKB"; rm(w)
w <- sort(grep("whjy", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(w); xpcost(w)
df$FullName[df$FullName %in% w] <- "WHJY"; rm(w)
w <- sort(grep("wlne|abc 6|abc channel 6|^abc chanel 6", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(w); xpcost(w)
df$FullName[df$FullName %in% w] <- "WLNE"; rm(w)
w <- sort(grep("wwli|lite 105", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(w); xpcost(w)
df$FullName[df$FullName %in% w] <- "WWLI"; rm(w)
w <- sort(grep("wbna|^west broadwayneighborhood", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(w); xpcost(w)
df$FullName[df$FullName %in% w] <- "West Broadway Neighborhood Assoc"; rm(w)
w <- sort(grep("wsne", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(w); xpcost(w)
df$FullName[df$FullName %in% w] <- "WSNE"; rm(w)
w <- sort(grep("^w b mas|^wb mas", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(w); xpcost(w)
df$FullName[df$FullName %in% w] <- "WB Mason"; rm(w)
w <- sort(grep("wpmz|^poder 11|^radio poder", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(w); xpcost(w)
df$FullName[df$FullName %in% w] <- "Poder 1110 WPMZ"; rm(w)
w <- sort(grep("whjj", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(w); xpcost(w)
df$FullName[df$FullName %in% w] <- "WHJJ"; rm(w)
w <- sort(grep("wwbb", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(w); xpcost(w)
df$FullName[df$FullName %in% w] <- "WWBB"; rm(w)
w <- sort(grep("wblq|^wb lq", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(w); xpcost(w)
df$FullName[df$FullName %in% w] <- "WBLQ"; rm(w)
mundo <- sort(grep("video mundo|mundo broad|videomundo|^vibeo mundo", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mundo); xpcost(mundo)
df$FullName[df$FullName %in% mundo] <- "Videomundo Broadcasting"; rm(mundo)
radio <- sort(grep("latino public radio", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(radio); xpcost(radio)
df$FullName[df$FullName %in% radio] <- "RI Latino Public Radio"; rm(radio)
radio <- sort(grep("^woon-am radio|^woon 1240|woonsocket radio|woon radio|^woonsocket 1240|^wooon 1240|^won radio inc|^woon$|1240 radio", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(radio); xpcost(radio)
df$FullName[df$FullName %in% radio] <- "Woon Radio"; rm(radio)
radio <- sort(grep("power radio", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(radio); xpcost(radio)
df$FullName[df$FullName %in% radio] <- "Full Power Radio"; rm(radio)
tv <- sort(grep("nbc", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(tv); xpcost(tv)
df$FullName[df$FullName %in% tv] <- "NBC TV"; rm(tv)
tv <- sort(grep("full chan", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(tv); xpcost(tv)
df$FullName[df$FullName %in% tv] <- "Full Channel TV"; rm(tv)
tv <- sort(grep("tvworld", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(tv); xpcost(tv)
df$FullName[df$FullName %in% tv] <- "Tvworldwidecom"; rm(tv)
tv <- sort(grep("wlwc", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(tv); xpcost(tv)
df$FullName[df$FullName %in% tv] <- "WLWC TV"; rm(tv)
tv <- sort(grep("cbtv", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(tv); xpcost(tv)
df$FullName[df$FullName %in% tv] <- "CBTV Inc"; rm(tv)
tv <- sort(grep("clear chan", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(tv); xpcost(tv)
df$FullName[df$FullName %in% tv] <- "Clear Channel"; rm(tv)
product <- sort(grep("atmosphere prod", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(product); xpcost(product)
df$FullName[df$FullName %in% product] <- "Atmosphere Production Group"; rm(product)
wilkinson <- sort(grep("vela-wilk|vella-wilk|vella wilkinson|camille wilk", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(wilkinson); xpcost(wilkinson)
df$FullName[df$FullName %in% wilkinson] <- "Camille Vella-Wilkinson"; rm(wilkinson)
sullivan <- sort(grep("ray sullivan|raymond sullivan|raymond j sullivan", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sullivan); xpcost(sullivan)
df$FullName[df$FullName %in% sullivan] <- "Raymond J Sullivan"; rm(sullivan)
sullivan <- sort(grep("bonnie sullivan", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sullivan); xpcost(sullivan)
df$FullName[df$FullName %in% sullivan] <- "Bonnie Sullivan"; rm(sullivan)
sullivan <- sort(grep("francis p sullivan|francis sullivan|frank sullivan", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sullivan); xpcost(sullivan)
df$FullName[df$FullName %in% sullivan] <- "Francis P Sullivan"; rm(sullivan)
sullivan <- sort(grep("sullivan white", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sullivan); xpcost(sullivan)
df$FullName[df$FullName %in% sullivan] <- "Sullivan Whitehead and Deluca"; rm(sullivan)
sullivan <- sort(grep("sullivan comp", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sullivan); xpcost(sullivan)
df$FullName[df$FullName %in% sullivan] <- "Sullivan Company"; rm(sullivan)
sullivan <- sort(grep("megan sullivan", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sullivan); xpcost(sullivan)
df$FullName[df$FullName %in% sullivan] <- "Megan Sullivan"; rm(sullivan)
sullivan <- sort(grep("osullivan4nk|colin osull", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sullivan); xpcost(sullivan)
df$FullName[df$FullName %in% sullivan] <- "Colin Osullivan"; rm(sullivan)
sullivan <- sort(grep("michael sullivan", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sullivan); xpcost(sullivan)
df$FullName[df$FullName %in% sullivan] <- "Michael Sullivan"; rm(sullivan)
sullivan <- sort(grep("david j sullivan|david sullivan|dave sullivan", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sullivan); xpcost(sullivan)
df$FullName[df$FullName %in% sullivan] <- "David J Sullivan"; rm(sullivan)
sullivan <- sort(grep("patrick sullivan|patrick j sullivan", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sullivan); xpcost(sullivan)
df$FullName[df$FullName %in% sullivan] <- "Patrick J Sullivan"; rm(sullivan)
sullivan <- sort(grep("joseph sullivan|joe sullivan", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sullivan); xpcost(sullivan)
df$FullName[df$FullName %in% sullivan] <- "Joe Sullivan"; rm(sullivan)
patterson <- sort(grep("kammerer", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(patterson); xpcost(patterson)
df$FullName[df$FullName %in% patterson] <- "Patricia A Kammerer"; rm(patterson)
sovereign <- sort(grep("sovereigh bank|sovereign bank|soverign bank", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sovereign); xpcost(sovereign)
df$FullName[df$FullName %in% sovereign] <- "Sovereign Bank"; rm(sovereign)
homedepot <- sort(grep("home depot|home deport|home depot", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(homedepot); xpcost(homedepot)
df$FullName[df$FullName %in% homedepot] <- "Home Depot"; rm(homedepot)
tdbank <- sort(grep("^t d bank|tdbank|td bank", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(tdbank); xpcost(tdbank)
df$FullName[df$FullName %in% tdbank] <- "TD Bank"; rm(tdbank)
officemax <- sort(grep("offfice depot|officemax|office max|offic max|offiice max|office depot|office deport", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(officemax); xpcost(officemax)
df$FullName[df$FullName %in% officemax] <- "Office Max"; rm(officemax)
lowes <- sort(grep("^lowes", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(lowes); xpcost(lowes)
df$FullName[df$FullName %in% lowes] <- "Lowes"; rm(lowes)
washington <- sort(grep("^wahington trust|washingotn trust|washington trust", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(washington); xpcost(washington)
df$FullName[df$FullName %in% washington] <- "Washington Trust"; rm(washington)
atandt <- sort(grep("atandt|^a t and t|^at and t$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(atandt); xpcost(atandt)
df$FullName[df$FullName %in% atandt] <- "A T and T"; rm(atandt)
santander <- sort(grep("santander|^santandar$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(santander); xpcost(santander)
df$FullName[df$FullName %in% santander] <- "Santander Bank"; rm(santander)
cvs <- sort(grep("cvs", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(cvs); xpcost(cvs)
df$FullName[df$FullName %in% cvs] <- "CVS"; rm(cvs)
minuteman <- sort(grep("minuteman|minute man press|minute man|minuteman press", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(minuteman); xpcost(minuteman)
df$FullName[df$FullName %in% minuteman] <- "Minuteman Press"; rm(minuteman)
jamestownpress <- sort(grep("jamestown press", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(jamestownpress); xpcost(jamestownpress)
df$FullName[df$FullName %in% jamestownpress] <- "Jamestown Press"; rm(jamestownpress)
thisweek <- sort(grep("^ntw$|newport this week", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(thisweek); xpcost(thisweek)
df$FullName[df$FullName %in% thisweek] <- "Newport This Week"; rm(thisweek)
dollartree <- sort(grep("dollartree|dollar tree", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(dollartree); xpcost(dollartree)
df$FullName[df$FullName %in% dollartree] <- "Dollar Tree"; rm(dollartree)
reminder <- sort(grep("the reminder|coventry remind|^reminder$|^ri reminder|reminder coventry", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(reminder); xpcost(reminder)
df$FullName[df$FullName %in% reminder] <- "The Coventry Reminder"; rm(reminder)
walgreens <- sort(grep("walgreens", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(walgreens); xpcost(walgreens)
df$FullName[df$FullName %in% walgreens] <- "Walgreens"; rm(walgreens)
joblot <- sort(grep("stat job lot$|state job jot$|state job log$|state job lot$|joblot|state jon lot", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(joblot); xpcost(joblot)
df$FullName[df$FullName %in% joblot] <- "Ocean State Job Lot"; rm(joblot)
bestbuy <- sort(grep("bestbuy|best buy", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(bestbuy); xpcost(bestbuy)
df$FullName[df$FullName %in% bestbuy] <- "Best Buy"; rm(bestbuy)
reporter <- sort(grep("^ep reporter|^the reporter$|reporter news|providence reporter|prov reporter", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(reporter); xpcost(reporter)
df$FullName[df$FullName %in% reporter] <- "East Providence Reporter"; rm(reporter)
reporter <- sort(grep("^rehoboth reports$|rehobeth reporter|rehoboth reporter|rehoboth\\/seekonk reporter", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(reporter); xpcost(reporter)
df$FullName[df$FullName %in% reporter] <- "Rehoboth Reporter"; rm(reporter)
webster <- sort(grep("webster bank", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(webster); xpcost(webster)
df$FullName[df$FullName %in% webster] <- "Webster Bank"; rm(webster)
grunwald <- sort(grep("grunwald", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grunwald); xpcost(grunwald)
df$FullName[df$FullName %in% grunwald] <- "Grunwald Communications"; rm(grunwald)
putnam <- sort(grep("murphy\\/putnam", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(putnam); xpcost(putnam)
df$FullName[df$FullName %in% putnam] <- "Murphy Putnam Media"; rm(putnam)
oldtown <- sort(grep("old town", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(oldtown); xpcost(oldtown)
df$FullName[df$FullName %in% oldtown] <- "Old Towne Media"; rm(oldtown)
strategicmedia <- sort(grep("strategic media", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(strategicmedia); xpcost(strategicmedia)
df$FullName[df$FullName %in% strategicmedia] <- "Strategic Media"; rm(strategicmedia)
jamestownass <- sort(grep("jamestown assoc", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(jamestownass); xpcost(jamestownass)
df$FullName[df$FullName %in% jamestownass] <- "Jamestown Associates"; rm(jamestownass)
cianci <- sort(grep("cianci jr scholarship|cianci education|cianci scholarship|cianci jr library", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(cianci); xpcost(cianci)
df$FullName[df$FullName %in% cianci] <- "Vincent A Cianci Jr Library Scholarship Fund"; rm(cianci)
cianci <- sort(grep("vincent cianci|cianci for mayor", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(cianci); xpcost(cianci)
df$FullName[df$FullName %in% cianci] <- "Vincent Cianci"; rm(cianci)
akpd <- sort(grep("akpd", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(akpd); xpcost(akpd)
df$FullName[df$FullName %in% akpd] <- "Akpd Message and Media"; rm(akpd)
neari <- sort(grep("^nea child|^nea pro|^nea-ri|^nea ri|^nea-rho|^nea rho|national education assoc|neari", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(neari); xpcost(neari)
df$FullName[df$FullName %in% neari] <- "Neari"; rm(neari)
viewpoint <- sort(grep("viewpoint", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(viewpoint); xpcost(viewpoint)
df$FullName[df$FullName %in% viewpoint] <- "Viewpoint Strategies"; rm(viewpoint)
intuitpay <- sort(grep("intuit pay", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(intuitpay); xpcost(intuitpay)
df$FullName[df$FullName %in% intuitpay] <- "Intuit Payroll"; rm(intuitpay)
spectrum <- sort(grep("spectrum", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(spectrum); xpcost(spectrum)
df$FullName[df$FullName %in% spectrum] <- "Spectrum Marketing"; rm(spectrum)
ams <- sort(grep("ams comm", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(ams); xpcost(ams)
df$FullName[df$FullName %in% ams] <- "AMS Communications"; rm(ams)
stateof <- sort(grep("state of rhode island and providence plantations|^state of rhode island$|^state of ri$|^state of rr$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(stateof); xpcost(stateof)
df$FullName[df$FullName %in% stateof] <- "State of RI"; rm(stateof)
stateof <- sort(grep("ri state internal|ri-internal|ri internal|ri int service|island- internal|island internal|internal service|internal svc|central service|central svc", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(stateof); xpcost(stateof)
df$FullName[df$FullName %in% stateof] <- "RI Internal Fund"; rm(stateof)
sphere <- sort(grep("social sphere", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sphere); xpcost(sphere)
df$FullName[df$FullName %in% sphere] <- "Social Sphere"; rm(sphere)
aalmonte <- sort(grep("ernest a almonte|ernie almonte|^almonte for treasurer", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(aalmonte); xpcost(aalmonte)
df$FullName[df$FullName %in% aalmonte] <- "Ernest A Almonte"; rm(aalmonte)
feldman <- sort(grep("feldman group", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(feldman); xpcost(feldman)
df$FullName[df$FullName %in% feldman] <- "The Feldman Group"; rm(feldman)
healthcare <- sort(grep("new england health care union|health care employee", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(healthcare); xpcost(healthcare)
df$FullName[df$FullName %in% healthcare] <- "New England Health Care Employees Union"; rm(healthcare)
dimezza <- sort(grep("roberta dimezza|roberta cicilline", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(dimezza); xpcost(dimezza)
df$FullName[df$FullName %in% dimezza] <- "Roberta Cicilline-Dimezza"; rm(dimezza)
grilo <- sort(grep("tim grilo|timothy grilo", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grilo); xpcost(grilo)
df$FullName[df$FullName %in% grilo] <- "Timothy Grilo"; rm(grilo)
bgp <- sort(grep("bgp", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(bgp); xpcost(bgp)
df$FullName[df$FullName %in% bgp] <- "BGP Strategies"; rm(bgp)
meuse <- sort(grep("daniel meuse|daniel j meuse", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(meuse); xpcost(meuse)
df$FullName[df$FullName %in% meuse] <- "Daniel Meuse"; rm(meuse)
crounse <- sort(grep("crounse", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(crounse); xpcost(crounse)
df$FullName[df$FullName %in% crounse] <- "Mack Crounse Group"; rm(crounse)
tap <- sort(grep("tap print", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(tap); xpcost(tap)
df$FullName[df$FullName %in% tap] <- "Tap Printing"; rm(tap)
cpm <- sort(grep("cpm station", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(cpm); xpcost(cpm)
df$FullName[df$FullName %in% cpm] <- "CPM Station"; rm(cpm)
baptista <- sort(grep("peter baptista|peter a baptista", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(baptista); xpcost(baptista)
df$FullName[df$FullName %in% baptista] <- "Peter Baptista"; rm(baptista)
gelfuso <- sort(grep("gelfuso and lachut|alan gelfuso|joseph lachut", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(gelfuso); xpcost(gelfuso)
df$FullName[df$FullName %in% gelfuso] <- "Gelfuso and Lachut"; rm(gelfuso)
laffey <- sort(grep("laffey", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(laffey); xpcost(laffey)
df$FullName[df$FullName %in% laffey] <- "Stephen P Laffey"; rm(laffey)
betty <- sort(grep("betty gallo", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(betty); xpcost(betty)
df$FullName[df$FullName %in% betty] <- "Betty Gallo and Co"; rm(betty)
louis <- sort(grep("louis press|louis printing", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(louis); xpcost(louis)
df$FullName[df$FullName %in% louis] <- "Louis Press"; rm(louis)
drew <- sort(grep("victoria drew", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(drew); xpcost(drew)
df$FullName[df$FullName %in% drew] <- "Victoria Drew"; rm(drew)
espanol <- sort(grep("providence en espanel|providnece en espanol|providence espanol|espanol prov|providence en espanol|^ri espanol|island en espanol|espinol news", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(espanol); xpcost(espanol)
df$FullName[df$FullName %in% espanol] <- "Providence En Espanol"; rm(espanol)
moderate <- sort(grep("moderate party of rhode|moderate party of ri$|island moderate party|^moderate party$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(moderate); xpcost(moderate)
df$FullName[df$FullName %in% moderate] <- "Moderate Party of RI"; rm(moderate)
moderate <- sort(grep("moderate party pac|moderate party state comm|party of ri state comm", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(moderate); xpcost(moderate)
df$FullName[df$FullName %in% moderate] <- "Moderate Party State Committee"; rm(moderate)
majority <- sort(grep("majority commun", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(majority); xpcost(majority)
df$FullName[df$FullName %in% majority] <- "Majority Communications"; rm(majority)
countyind <- sort(grep("^sc independent|ri independent newpapers|^sc newspaper|^s c independent|the independant news|the independant news|^the independent$|soco independent|south couty ind|south county news|county ind", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(countyind); xpcost(countyind)
df$FullName[df$FullName %in% countyind] <- "South County Independent"; rm(countyind)
srin <- sort(grep("narragansett times|^srin- chariho|standard times$|^srin standard|^sri kent|^srin nar|^srin times|^srn$|south rhode island news|^sr news|^sri$|^sri news|^srin$|^srin news|southern rhode island news|southern ri news", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(srin); xpcost(srin)
df$FullName[df$FullName %in% srin] <- "Southern RI Newspapers"; rm(srin)
exeterc <- sort(grep("exeter cc|exeter country club", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(exeterc); xpcost(exeterc)
df$FullName[df$FullName %in% exeterc] <- "Exeter Country Club"; rm(exeterc)
trillo <- sort(grep("^j trillo election|joseph a trillo|joseph trillo|joe trillo|jospeh trillo", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(trillo); xpcost(trillo)
df$FullName[df$FullName %in% trillo] <- "Joseph A Trillo"; rm(trillo)
ohlsen <- sort(grep("ohlsen", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(ohlsen); xpcost(ohlsen)
df$FullName[df$FullName %in% ohlsen] <- "Ohlsen Research"; rm(ohlsen)
derentis <- sort(grep("james v derentis", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(derentis); xpcost(derentis)
df$FullName[df$FullName %in% derentis] <- "James V Derentis"; rm(derentis)
berglund <- sort(grep("berglund", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(berglund); xpcost(berglund)
df$FullName[df$FullName %in% berglund] <- "Collin Berglund"; rm(berglund)
shuster <- sort(grep("shuster", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(shuster); xpcost(shuster)
df$FullName[df$FullName %in% shuster] <- "Shuster Realty"; rm(shuster)
realty <- sort(grep("lehigh realty", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(realty); xpcost(realty)
df$FullName[df$FullName %in% realty] <- "Lehigh Realty"; rm(realty)
realtor <- sort(grep("ri assoc of realtor", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(realtor); xpcost(realtor)
df$FullName[df$FullName %in% realtor] <- "RI Assoc of Realtors"; rm(realtor)
baughman <- sort(grep("baughman", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(baughman); xpcost(baughman)
df$FullName[df$FullName %in% baughman] <- "The Baughman Company"; rm(baughman)
cogens <- sort(grep("cogens|^cogen$|^cogen print|^corens print", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(cogens); xpcost(cogens)
df$FullName[df$FullName %in% cogens] <- "Cogens Printing"; rm(cogens)
hoffman <- sort(grep("david t hoffman", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(hoffman); xpcost(hoffman)
df$FullName[df$FullName %in% hoffman] <- "David T Hoffman"; rm(hoffman)
hoffman <- sort(grep("j c hoffman|christian hoffman", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(hoffman); xpcost(hoffman)
df$FullName[df$FullName %in% hoffman] <- "JC Hoffman"; rm(hoffman)
essex <- sort(grep("janet essex", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(essex); xpcost(essex)
df$FullName[df$FullName %in% essex] <- "Janet Essex"; rm(essex)
rokoff <- sort(grep("laura rokoff", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(rokoff); xpcost(rokoff)
df$FullName[df$FullName %in% rokoff] <- "Laura Rokoff"; rm(rokoff)
grebien <- sort(grep("don grebian|^don grebun$|dan grebien|donald grebian|dan grezebien|donald grebin$|^grebien$|donald r grebien|don grebien|donald grebien|mayor grebien", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(grebien); xpcost(grebien)
df$FullName[df$FullName %in% grebien] <- "Donald R Grebien"; rm(grebien)
horowitz <- sort(grep("horowitz", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(horowitz); xpcost(horowitz)
df$FullName[df$FullName %in% horowitz] <- "Rob Horowitz and Assoc"; rm(horowitz)
driscol <- sort(grep("devin driscol", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(driscol); xpcost(driscol)
df$FullName[df$FullName %in% driscol] <- "Devin Driscoll"; rm(driscol)
driscol <- sort(grep("frances driscol|frances m driscol", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(driscol); xpcost(driscol)
df$FullName[df$FullName %in% driscol] <- "Frances M Driscoll"; rm(driscol)
aram <- sort(grep("aram g garabedian|aram garabedian", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(aram); xpcost(aram)
df$FullName[df$FullName %in% aram] <- "Aram G Garabedian"; rm(aram)
sway <- sort(grep("sway", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sway); xpcost(sway)
df$FullName[df$FullName %in% sway] <- "Sway"; rm(sway)
tonprint <- sort(grep("barrington printing", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(tonprint); xpcost(tonprint)
df$FullName[df$FullName %in% tonprint] <- "Barrington Printing"; rm(tonprint)
shawmut <- sort(grep("shawmut group", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(shawmut); xpcost(shawmut)
df$FullName[df$FullName %in% shawmut] <- "The Shawmut Group"; rm(shawmut)
leothom <- sort(grep("leo thom", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(leothom); xpcost(leothom)
df$FullName[df$FullName %in% leothom] <- "Leo Thompson"; rm(leothom)
pagliarini <- sort(grep("john r pagliarini", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(pagliarini); xpcost(pagliarini)
df$FullName[df$FullName %in% pagliarini] <- "John R Pagliarini"; rm(pagliarini)
pagliarini <- sort(grep("pagliarini campaign|pagliarini for senate|john pagliarini|john a pagliarini", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(pagliarini); xpcost(pagliarini)
df$FullName[df$FullName %in% pagliarini] <- "John A Pagliarini Jr"; rm(pagliarini)
rocket <- sort(grep("sign roacket|signrocket|sign rocket", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(rocket); xpcost(rocket)
df$FullName[df$FullName %in% rocket] <- "Signrocket"; rm(rocket)
starting <- sort(grep("starting point", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(starting); xpcost(starting)
df$FullName[df$FullName %in% starting] <- "Starting Point Ops"; rm(starting)
rocket <- sort(grep("rocket signs|rocketsigns", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(rocket); xpcost(rocket)
df$FullName[df$FullName %in% rocket] <- "Rocket Signs"; rm(rocket)
lsg <- sort(grep("lsg", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(lsg); xpcost(lsg)
df$FullName[df$FullName %in% lsg] <- "LSG Strategies"; rm(lsg)
robit <- sort(grep("robitailli for gov|robitaille for governor|robitaille for gov|john robitailee|john f robitaille|john robitaille", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(robit); xpcost(robit)
df$FullName[df$FullName %in% robit] <- "John F Robitaille"; rm(robit)
romash <- sort(grep("romash", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(romash); xpcost(romash)
df$FullName[df$FullName %in% romash] <- "Romash Communications"; rm(romash)
tincan <- sort(grep("tin can alley", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(tincan); xpcost(tincan)
df$FullName[df$FullName %in% tincan] <- "Tin Can Alley Studios"; rm(tincan)
proprint <- sort(grep("proprint|pro print|pro-print", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(proprint); xpcost(proprint)
df$FullName[df$FullName %in% proprint] <- "Proprint Inc"; rm(proprint)
victorystore <- sort(grep("victorey store|victory store|victorystorecom|victory signs", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(victorystore); xpcost(victorystore)
df$FullName[df$FullName %in% victorystore] <- "Victorystorecom"; rm(victorystore)
joneill <- sort(grep("^oneill for state senate|ward j oneil|edward oneil|ed oneil", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(joneill); xpcost(joneill)
df$FullName[df$FullName %in% joneill] <- "Edward J Oneill"; rm(joneill)
discover <- sort(grep("^discover$|discover card", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(discover); xpcost(discover)
df$FullName[df$FullName %in% discover] <- "Discover Card"; rm(discover)
formatt <- sort(grep("formatt|format print", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(formatt); xpcost(formatt)
df$FullName[df$FullName %in% formatt] <- "Formatt Printing"; rm(formatt)
tangopix <- sort(grep("tango pix", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(tangopix); xpcost(tangopix)
df$FullName[df$FullName %in% tangopix] <- "Tango Pix"; rm(tangopix)
hunsinger <- sort(grep("hunsinger|christine hunsigner", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(hunsinger); xpcost(hunsinger)
df$FullName[df$FullName %in% hunsinger] <- "Christine Hunsinger"; rm(hunsinger)
saverio <- sort(grep("saverio reb", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(saverio); xpcost(saverio)
df$FullName[df$FullName %in% saverio] <- "Saverio Rebecchi"; rm(saverio)
fischer <- sort(grep("william j fischer|bill fischer", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(fischer); xpcost(fischer)
df$FullName[df$FullName %in% fischer] <- "William J Fischer"; rm(fischer)
jlm <- sort(grep("jlm", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(jlm); xpcost(jlm)
df$FullName[df$FullName %in% jlm] <- "JLM Consulting"; rm(jlm)
scanlon <- sort(grep("scanlon design", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(scanlon); xpcost(scanlon)
df$FullName[df$FullName %in% scanlon] <- "Scanlon Design"; rm(scanlon)
nailcomm <- sort(grep("^nail comm", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(nailcomm); xpcost(nailcomm)
df$FullName[df$FullName %in% nailcomm] <- "Nail Communications"; rm(nailcomm)
damiani <- sort(grep("michael damiani", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(damiani); xpcost(damiani)
df$FullName[df$FullName %in% damiani] <- "Michael Damiani"; rm(damiani)
graphic <- sort(grep("graphic innov|^graphics innov", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(graphic); xpcost(graphic)
df$FullName[df$FullName %in% graphic] <- "Graphic Innovations"; rm(graphic)
graphic <- sort(grep("^graphic inc|^graphi ink|^graphic ink|idea graphic|^graphics$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(graphic); xpcost(graphic)
df$FullName[df$FullName %in% graphic] <- "Graphic Ink"; rm(graphic)
karampetsos <- sort(grep("jina petraca-karampetsos|petrarca-karampetsos|jina karampetsos|jina petrarca", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(karampetsos); xpcost(karampetsos)
df$FullName[df$FullName %in% karampetsos] <- "Jina N Petrarca- Karampetsos"; rm(karampetsos)
imagepointe <- sort(grep("image pointe", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(imagepointe); xpcost(imagepointe)
df$FullName[df$FullName %in% imagepointe] <- "Image Pointe"; rm(imagepointe)
kellyhall <- sort(grep("kelly hall", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(kellyhall); xpcost(kellyhall)
df$FullName[df$FullName %in% kellyhall] <- "Kelly Hall"; rm(kellyhall)
employ <- sort(grep("dept of labor and training|^riet$|employment and training", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(employ); xpcost(employ)
df$FullName[df$FullName %in% employ] <- "RI Dept of Employment and Training"; rm(employ)
decision <- sort(grep("decision research", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(decision); xpcost(decision)
df$FullName[df$FullName %in% decision] <- "Decision Research"; rm(decision)
fund <- sort(grep("fundraising manag", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(fund); xpcost(fund)
df$FullName[df$FullName %in% fund] <- "Fundraising Management Group"; rm(fund)
normington <- sort(grep("normington", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(normington); xpcost(normington)
df$FullName[df$FullName %in% normington] <- "Normington Petts and Assoc"; rm(normington)
moore <- sort(grep("moore media", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(moore); xpcost(moore)
df$FullName[df$FullName %in% moore] <- "Moore Media"; rm(moore)
moore <- sort(grep("alex moore|alexander moore", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(moore); xpcost(moore)
df$FullName[df$FullName %in% moore] <- "Alex Moore"; rm(moore)
moore <- sort(grep("andrew moore", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(moore); xpcost(moore)
df$FullName[df$FullName %in% moore] <- "Andrew Moore"; rm(moore)
paperandpress <- sort(grep("paper and press", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(paperandpress); xpcost(paperandpress)
df$FullName[df$FullName %in% paperandpress] <- "New England Newspaper and Press Association"; rm(paperandpress)
england <- sort(grep("evan england", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(england); xpcost(england)
df$FullName[df$FullName %in% england] <- "Evan England"; rm(england)
mottola <- sort(grep("chris mottola", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mottola); xpcost(mottola)
df$FullName[df$FullName %in% mottola] <- "Chris Mottola Consulting"; rm(mottola)
zogby <- sort(grep("zogby int", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(zogby); xpcost(zogby)
df$FullName[df$FullName %in% zogby] <- "Zogby International"; rm(zogby)
zogby <- sort(grep("john zogby", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(zogby); xpcost(zogby)
df$FullName[df$FullName %in% zogby] <- "John Zogby Strategies"; rm(zogby)
rfa <- sort(grep("rfa real", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(rfa); xpcost(rfa)
df$FullName[df$FullName %in% rfa] <- "RFA Realty"; rm(rfa)
directmedia <- sort(grep("direct media", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(directmedia); xpcost(directmedia)
df$FullName[df$FullName %in% directmedia] <- "Direct Media Inc"; rm(directmedia)
fosterhome <- sort(grep("fosterhome scituate star|foster-home-journal|^foster journal$|foster home journ", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(fosterhome); xpcost(fosterhome)
df$FullName[df$FullName %in% fosterhome] <- "Foster Home Journal"; rm(fosterhome)
topofthebay <- sort(grep("top of the bay|^top of bay$|^top of teh bay", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(topofthebay); xpcost(topofthebay)
df$FullName[df$FullName %in% topofthebay] <- "Top of the Bay"; rm(topofthebay)
cablecomm <- sort(grep("cable comm", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(cablecomm); xpcost(cablecomm)
df$FullName[df$FullName %in% cablecomm] <- "National Cable Communications"; rm(cablecomm)
ralphs <- sort(grep("^rrd catering|^ralph catering|ralphs cater|ralphs kitchen|^ralphs$|^ralphs at the", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(ralphs); xpcost(ralphs)
df$FullName[df$FullName %in% ralphs] <- "Ralphs Catering"; rm(ralphs)
prestige <- sort(grep("prestige mfg", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(prestige); xpcost(prestige)
df$FullName[df$FullName %in% prestige] <- "Prestige Mfg"; rm(prestige)
elkhay <- sort(grep("ross elkhay|ross elkay", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(elkhay); xpcost(elkhay)
df$FullName[df$FullName %in% elkhay] <- "Ross Elkhay"; rm(elkhay)
gildea <- sort(grep("gildea-darl", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(gildea); xpcost(gildea)
df$FullName[df$FullName %in% gildea] <- "The Gildea-Darlington Group"; rm(gildea)
moreau <- sort(grep("charles d moreau|charles moreau|chalrles moreau|^moreau comm|charles e moreau", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(moreau); xpcost(moreau)
df$FullName[df$FullName %in% moreau] <- "Charles D Moreau"; rm(moreau)
emily <- sort(grep("emilys list", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(emily); xpcost(emily)
df$FullName[df$FullName %in% emily] <- "Emilys List"; rm(emily)
vota <- sort(grep("americo vota|^vota$|^rico vota", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(vota); xpcost(vota)
df$FullName[df$FullName %in% vota] <- "Americo Vota"; rm(vota)
lammis <- sort(grep("lammis varga$|lammis vargas|lammis vargss|lammies varga$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(lammis); xpcost(lammis)
df$FullName[df$FullName %in% lammis] <- "Lammis Vargas"; rm(lammis)
pawtucketc <- sort(grep("pawtucket country club|^pcc$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(pawtucketc); xpcost(pawtucketc)
df$FullName[df$FullName %in% pawtucketc] <- "Pawtucket Country Club"; rm(pawtucketc)
countryclub <- sort(grep("^valley country club", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(countryclub); xpcost(countryclub)
df$FullName[df$FullName %in% countryclub] <- "Valley Country Club"; rm(countryclub)
countryclub <- sort(grep("green valley country club|green valley golf|^green valley cc$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(countryclub); xpcost(countryclub)
df$FullName[df$FullName %in% countryclub] <- "Green Valley Country Club"; rm(countryclub)
golf <- sort(grep("triggs", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(golf); xpcost(golf)
df$FullName[df$FullName %in% golf] <- "Triggs Memorial Golf Course"; rm(golf)
golf <- sort(grep("potowomut", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(golf); xpcost(golf)
df$FullName[df$FullName %in% golf] <- "Potowomut Golf Club"; rm(golf)
quidness <- sort(grep("quidness|quianessett country club", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(quidness); xpcost(quidness)
df$FullName[df$FullName %in% quidness] <- "Quidnessett Country Club"; rm(quidness)
convention <- sort(grep("^ri convention|island convention", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(convention); xpcost(convention)
df$FullName[df$FullName %in% convention] <- "RI Convention Center"; rm(convention)
clurman <- sort(grep("meg clurman", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(clurman); xpcost(clurman)
df$FullName[df$FullName %in% clurman] <- "Meg Clurman"; rm(clurman)
facente <- sort(grep("william r facente", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(facente); xpcost(facente)
df$FullName[df$FullName %in% facente] <- "William R Facente"; rm(facente)
dencemedia <- sort(grep("dence media", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(dencemedia); xpcost(dencemedia)
df$FullName[df$FullName %in% dencemedia] <- "Providence Media"; rm(dencemedia)
mcallister <- sort(grep("cara mcallister", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mcallister); xpcost(mcallister)
df$FullName[df$FullName %in% mcallister] <- "Cara Mcallister"; rm(mcallister)
mcallister <- sort(grep("steve mccalister|steve mc callister|stephen p mccallister|stephen mcallister|steven mcallister|steve mcallister", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mcallister); xpcost(mcallister)
df$FullName[df$FullName %in% mcallister] <- "Stephen Mcallister"; rm(mcallister)
biltmore <- sort(grep("^biltmore$|providence biltmore|biltmore hotel|biltmore prov", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(biltmore); xpcost(biltmore)
df$FullName[df$FullName %in% biltmore] <- "Providence Biltmore"; rm(biltmore)
newblue <- sort(grep("new blue", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(newblue); xpcost(newblue)
df$FullName[df$FullName %in% newblue] <- "New Blue Interactive"; rm(newblue)
campaignstore <- sort(grep("campaignstore|campaign store|^my campagin", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(campaignstore); xpcost(campaignstore)
df$FullName[df$FullName %in% campaignstore] <- "My Campaign Store"; rm(campaignstore)
gravis <- sort(grep("gravis", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(gravis); xpcost(gravis)
df$FullName[df$FullName %in% gravis] <- "Gravis Marketing"; rm(gravis)
images <- sort(grep("images des|^image design", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(images); xpcost(images)
df$FullName[df$FullName %in% images] <- "Images Design"; rm(images)
mezcla <- sort(grep("mezcla", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mezcla); xpcost(mezcla)
df$FullName[df$FullName %in% mezcla] <- "Media Mezcla"; rm(mezcla)
picturepark <- sort(grep("picture park", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(picturepark); xpcost(picturepark)
df$FullName[df$FullName %in% picturepark] <- "Picture Park"; rm(picturepark)
coolair <- sort(grep("cool air|cool ari creations|coolair creations", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(coolair); xpcost(coolair)
df$FullName[df$FullName %in% coolair] <- "Cool Air Creations"; rm(coolair)
andrewroos <- sort(grep("andrew roos", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(andrewroos); xpcost(andrewroos)
df$FullName[df$FullName %in% andrewroos] <- "Andrew Roos"; rm(andrewroos)
kayner <- sort(grep("nicole kayner", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(kayner); xpcost(kayner)
df$FullName[df$FullName %in% kayner] <- "Nicole Kayner"; rm(kayner)
rshcamp <- sort(grep("rsh camp", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(rshcamp); xpcost(rshcamp)
df$FullName[df$FullName %in% rshcamp] <- "RSH Campaigns"; rm(rshcamp)
fliteways <- sort(grep("fliteways", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(fliteways); xpcost(fliteways)
df$FullName[df$FullName %in% fliteways] <- "Executive Fliteways"; rm(fliteways)
ogara <- sort(grep("marisa ogara", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(ogara); xpcost(ogara)
df$FullName[df$FullName %in% ogara] <- "Marisa Ogara"; rm(ogara)
harris <- sort(grep("harris media", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(harris); xpcost(harris)
df$FullName[df$FullName %in% harris] <- "Harris Media"; rm(harris)
harrington <- sort(grep("jill harrington", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(harrington); xpcost(harrington)
df$FullName[df$FullName %in% harrington] <- "Jill Harrington"; rm(harrington)
mpri <- sort(grep("^mpri", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mpri); xpcost(mpri)
df$FullName[df$FullName %in% mpri] <- "MPRI"; rm(mpri)
appenfeller <- sort(grep("matthew apperfeller|appenfeller", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(appenfeller); xpcost(appenfeller)
df$FullName[df$FullName %in% appenfeller] <- "Matthew Appenfeller"; rm(appenfeller)
vesuvio <- sort(grep("vesuvio|versuvios restaurant", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(vesuvio); xpcost(vesuvio)
df$FullName[df$FullName %in% vesuvio] <- "Vesuvio Restaurant"; rm(vesuvio)
apponaug <- sort(grep("apponaug business", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(apponaug); xpcost(apponaug)
df$FullName[df$FullName %in% apponaug] <- "Apponaug Business Services"; rm(apponaug)
boucher <- sort(grep("boucher", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(boucher); xpcost(boucher)
df$FullName[df$FullName %in% boucher] <- "Jonathan Boucher"; rm(boucher)
little <- sort(grep("chris little|topher little|topher h little", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(little); xpcost(little)
df$FullName[df$FullName %in% little] <- "Christopher Little"; rm(little)
success <- sort(grep("success mail", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(success); xpcost(success)
df$FullName[df$FullName %in% success] <- "Success Mail"; rm(success)
logistics <- sort(grep("print logistics", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(logistics); xpcost(logistics)
df$FullName[df$FullName %in% logistics] <- "Print Logistics"; rm(logistics)
sds <- sort(grep("sds", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sds); xpcost(sds)
df$FullName[df$FullName %in% sds] <- "SDS"; rm(sds)
trail <- sort(grep("trail blazer campaign", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(trail); xpcost(trail)
df$FullName[df$FullName %in% trail] <- "Trail Blazer Campaign Services"; rm(trail)
dsl <- sort(grep("dsl", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(dsl); xpcost(dsl)
df$FullName[df$FullName %in% dsl] <- "DSL Properties"; rm(dsl)
reganmedia <- sort(grep("regan media", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(reganmedia); xpcost(reganmedia)
df$FullName[df$FullName %in% reganmedia] <- "Kyle Regan Media"; rm(reganmedia)
fabrizio <- sort(grep("fabrizio lee", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(fabrizio); xpcost(fabrizio)
df$FullName[df$FullName %in% fabrizio] <- "Fabrizio Lee"; rm(fabrizio)
strasberg <- sort(grep("rabin strasberg", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(strasberg); xpcost(strasberg)
df$FullName[df$FullName %in% strasberg] <- "Rabin Strasberg"; rm(strasberg)
scm <- sort(grep("scm assoc", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(scm); xpcost(scm)
df$FullName[df$FullName %in% scm] <- "SCM Associates"; rm(scm)
flsconnect <- sort(grep("fls connect", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(flsconnect); xpcost(flsconnect)
df$FullName[df$FullName %in% flsconnect] <- "FLS Connect"; rm(flsconnect)
dextradeur <- sort(grep("dextradeur", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(dextradeur); xpcost(dextradeur)
df$FullName[df$FullName %in% dextradeur] <- "Jacques Dextradeur"; rm(dextradeur)
pawsox <- sort(grep("pawsox|pawtucket red sox", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(pawsox); xpcost(pawsox)
df$FullName[df$FullName %in% pawsox] <- "Pawtucket Red Sox"; rm(pawsox)
unitedhealth <- sort(grep("united health", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(unitedhealth); xpcost(unitedhealth)
df$FullName[df$FullName %in% unitedhealth] <- "United Health Care"; rm(unitedhealth)
mstreet <- sort(grep("^m street sol", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mstreet); xpcost(mstreet)
df$FullName[df$FullName %in% mstreet] <- "M Street Solutions"; rm(mstreet)
andrewsia <- sort(grep("andrew sia", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(andrewsia); xpcost(andrewsia)
df$FullName[df$FullName %in% andrewsia] <- "Andrew Sia"; rm(andrewsia)
independent <- sort(grep("independence strat", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(independent); xpcost(independent)
df$FullName[df$FullName %in% independent] <- "Independence Strategy"; rm(independent)
lawrence <- sort(grep("lawrence and brook", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(lawrence); xpcost(lawrence)
df$FullName[df$FullName %in% lawrence] <- "Lawrence and Brooks"; rm(lawrence)
lawrence <- sort(grep("lawrence mancini|larry mancini|lawrence j mancini", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(lawrence); xpcost(lawrence)
df$FullName[df$FullName %in% lawrence] <- "Lawrence Mancini"; rm(lawrence)
lawrence <- sort(grep("lawrence m valencia|lawrence valencia|^valencia$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(lawrence); xpcost(lawrence)
df$FullName[df$FullName %in% lawrence] <- "Lawrence M Valencia"; rm(lawrence)
stones <- sort(grep("stones phone", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(stones); xpcost(stones)
df$FullName[df$FullName %in% stones] <- "Stones Phones"; rm(stones)
donnaj <- sort(grep("donna j perry|donna perry", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(donnaj); xpcost(donnaj)
df$FullName[df$FullName %in% donnaj] <- "Donna J Perry"; rm(donnaj)
redmaverick <- sort(grep("red maverick", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(redmaverick); xpcost(redmaverick)
df$FullName[df$FullName %in% redmaverick] <- "Red Maverick Media"; rm(redmaverick)
eastsidemonthly <- sort(grep("east side news|east side monthly|providence monthly|eastside news", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(eastsidemonthly); xpcost(eastsidemonthly)
df$FullName[df$FullName %in% eastsidemonthly] <- "East Side Monthly"; rm(eastsidemonthly)
asolomon <- sort(grep("solomon friends of michael|micheal a solomon|micahel solomon|michael a solomon|michael solomon", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(asolomon); xpcost(asolomon)
df$FullName[df$FullName %in% asolomon] <- "Michael A Solomon"; rm(asolomon)
deercreek <- sort(grep("deer creek", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(deercreek); xpcost(deercreek)
df$FullName[df$FullName %in% deercreek] <- "Deer Creek Research and Consulting"; rm(deercreek)
keven <- sort(grep("keven mckenna|keven a mckenna|kevin mckenna", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(keven); xpcost(keven)
df$FullName[df$FullName %in% keven] <- "Keven Mckenna"; rm(keven)
cenicola <- sort(grep("cenicola", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(cenicola); xpcost(cenicola)
df$FullName[df$FullName %in% cenicola] <- "Cenicola Consulting"; rm(cenicola)
gerencser <- sort(grep("gerencser", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(gerencser); xpcost(gerencser)
df$FullName[df$FullName %in% gerencser] <- "Stephen Gerencser"; rm(gerencser)
publickitchen <- sort(grep("public kitchen", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(publickitchen); xpcost(publickitchen)
df$FullName[df$FullName %in% publickitchen] <- "Public Kitchen and Bar"; rm(publickitchen)
federalreserve <- sort(grep("federal reserve", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(federalreserve); xpcost(federalreserve)
df$FullName[df$FullName %in% federalreserve] <- "Federal Reserve"; rm(federalreserve)
kensprinting <- sort(grep("kens printing|ken anderson", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(kensprinting); xpcost(kensprinting)
df$FullName[df$FullName %in% kensprinting] <- "Kens Printing"; rm(kensprinting)
golocalprov <- sort(grep("^golocal|golocalprov|go local prov|golocal24", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(golocalprov); xpcost(golocalprov)
df$FullName[df$FullName %in% golocalprov] <- "Golocalprov"; rm(golocalprov)
directioncom <- sort(grep("strategic directioncom", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(directioncom); xpcost(directioncom)
df$FullName[df$FullName %in% directioncom] <- "Strategic Directioncom"; rm(directioncom)
joyfox <- sort(grep("joy fox|joyfox", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(joyfox); xpcost(joyfox)
df$FullName[df$FullName %in% joyfox] <- "Joy Fox"; rm(joyfox)
jewishvoice <- sort(grep("holocaust education|jewis alliance|jewish comm|jewish voce|jewish voice|jewish alliance|jewish voice|jewish fed", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(jewishvoice); xpcost(jewishvoice)
df$FullName[df$FullName %in% jewishvoice] <- "Jewish Voice and Herald"; rm(jewishvoice)
donnawalsh <- sort(grep("donna walsh|donna m wash|donna m walsh", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(donnawalsh); xpcost(donnawalsh)
df$FullName[df$FullName %in% donnawalsh] <- "Donna Walsh"; rm(donnawalsh)
ronaldallen <- sort(grep("ronald allen", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(ronaldallen); xpcost(ronaldallen)
df$FullName[df$FullName %in% ronaldallen] <- "Ronald Allen"; rm(ronaldallen)
chasecard <- sort(grep("chase card|cardmember|card member", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(chasecard); xpcost(chasecard)
df$FullName[df$FullName %in% chasecard] <- "Chase Card Services"; rm(chasecard)
viking <- sort(grep("hotel viking", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(viking); xpcost(viking)
df$FullName[df$FullName %in% viking] <- "Hotel Viking"; rm(viking)
zeplowitz <- sort(grep("zeplowitz", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(zeplowitz); xpcost(zeplowitz)
df$FullName[df$FullName %in% zeplowitz] <- "Barry Zeplowitz and Assoc"; rm(zeplowitz)
schoen <- sort(grep("douglas e schoen", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(schoen); xpcost(schoen)
df$FullName[df$FullName %in% schoen] <- "Douglas E Schoen Nyc"; rm(schoen)
denice <- sort(grep("nick denise|nicholas denice|nick denice|^nicholas a denice", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(denice); xpcost(denice)
df$FullName[df$FullName %in% denice] <- "Nicholas Denice"; rm(denice)
organizing <- sort(grep("organizing group", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(organizing); xpcost(organizing)
df$FullName[df$FullName %in% organizing] <- "Organizing Group"; rm(organizing)
realtors <- sort(grep("greric real", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(realtors); xpcost(realtors)
df$FullName[df$FullName %in% realtors] <- "Greric Realty"; rm(realtors)
ppac <- sort(grep("ppac|providence performing arts center", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(ppac); xpcost(ppac)
df$FullName[df$FullName %in% ppac] <- "PPAC"; rm(ppac)
conley <- sort(grep("wlliam j conley|william j conley|bill conley|billy conley|william conley", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(conley); xpcost(conley)
df$FullName[df$FullName %in% conley] <- "William J Conley"; rm(conley)
hihat <- sort(grep("high hat|hihat|^the hi hat|hi-hat|^hi hat$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(hihat); xpcost(hihat)
df$FullName[df$FullName %in% hihat] <- "Hihat Club"; rm(hihat)
smithfield <- sort(grep("smithfield mag|smithfield times|smithfield publish", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(smithfield); xpcost(smithfield)
df$FullName[df$FullName %in% smithfield] <- "Your Smithfield Magazine"; rm(smithfield)
tomorrow <- sort(grep("signs by tommorrow|signs by tomorrow", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(tomorrow); xpcost(tomorrow)
df$FullName[df$FullName %in% tomorrow] <- "Signs by Tomorrow"; rm(tomorrow)
printcraft <- sort(grep("printcraft|print craft", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(printcraft); xpcost(printcraft)
df$FullName[df$FullName %in% printcraft] <- "Printcraft"; rm(printcraft)
sstrategies <- sort(grep("l s strategies", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sstrategies); xpcost(sstrategies)
df$FullName[df$FullName %in% sstrategies] <- "L S Strategies"; rm(sstrategies)
darlington <- sort(grep("david a darlington", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(darlington); xpcost(darlington)
df$FullName[df$FullName %in% darlington] <- "David A Darlington"; rm(darlington)
humphrey <- sort(grep("richard s humphrey|richard humphrey", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(humphrey); xpcost(humphrey)
df$FullName[df$FullName %in% humphrey] <- "Richard Humphrey"; rm(humphrey)
hopeclub <- sort(grep("hope club", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(hopeclub); xpcost(hopeclub)
df$FullName[df$FullName %in% hopeclub] <- "Hope Club"; rm(hopeclub)
checkmate <- sort(grep("checkmate inc", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(checkmate); xpcost(checkmate)
df$FullName[df$FullName %in% checkmate] <- "Checkmate Inc"; rm(checkmate)
mikus <- sort(grep("mikus", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mikus); xpcost(mikus)
df$FullName[df$FullName %in% mikus] <- "Mike Mikus"; rm(mikus)
islandtimes <- sort(grep("block island times", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(islandtimes); xpcost(islandtimes)
df$FullName[df$FullName %in% islandtimes] <- "Block Island Times"; rm(islandtimes)
nixon <- sort(grep("nixon peabody|nixon and peabody", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(nixon); xpcost(nixon)
df$FullName[df$FullName %in% nixon] <- "Nixon Peabody"; rm(nixon)
sinclairstrat <- sort(grep("sinclair strat", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sinclairstrat); xpcost(sinclairstrat)
df$FullName[df$FullName %in% sinclairstrat] <- "Sinclair Strategies"; rm(sinclairstrat)
bennett <- sort(grep("james s bennett", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(bennett); xpcost(bennett)
df$FullName[df$FullName %in% bennett] <- "James S Bennett"; rm(bennett)
harty <- sort(grep("harty press", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(harty); xpcost(harty)
df$FullName[df$FullName %in% harty] <- "Harty Press"; rm(harty)
aimee <- sort(grep("aimee audette", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(aimee); xpcost(aimee)
df$FullName[df$FullName %in% aimee] <- "Aimee Audette"; rm(aimee)
portable <- sort(grep("portable insight", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(portable); xpcost(portable)
df$FullName[df$FullName %in% portable] <- "Portable Insights"; rm(portable)
petrarca <- sort(grep("^petrarca for rep|peter john petrarca|peter petrarca|peter j petrarca|peterr petrarca|peter petraca|peter patrarca", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(petrarca); xpcost(petrarca)
df$FullName[df$FullName %in% petrarca] <- "Peter Petrarca"; rm(petrarca)
idc <- sort(grep("ocean cliff|oceancliff|idc inc|icd inc", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(idc); xpcost(idc)
df$FullName[df$FullName %in% idc] <- "IDC Inc"; rm(idc)
portuguese <- sort(grep("portuguese time|portugese times", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(portuguese); xpcost(portuguese)
df$FullName[df$FullName %in% portuguese] <- "Portuguese Times"; rm(portuguese)
greek <- sort(grep("annunciation|greek orth", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(greek); xpcost(greek)
df$FullName[df$FullName %in% greek] <- "Annunciation Greek Orthodx Church"; rm(greek)
madden <- sort(grep("courtney madden", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(madden); xpcost(madden)
df$FullName[df$FullName %in% madden] <- "Courtney Madden"; rm(madden)
signature <- sort(grep("signature print", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(signature); xpcost(signature)
df$FullName[df$FullName %in% signature] <- "Signature Printing"; rm(signature)
noble <- sort(grep("david l noble", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(noble); xpcost(noble)
df$FullName[df$FullName %in% noble] <- "David L Noble"; rm(noble)
mellman <- sort(grep("mellman", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mellman); xpcost(mellman)
df$FullName[df$FullName %in% mellman] <- "The Mellman Group"; rm(mellman)
walsh <- sort(grep("thomas e walsh", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(walsh); xpcost(walsh)
df$FullName[df$FullName %in% walsh] <- "Thomas E Walsh Jr"; rm(walsh)
alpine <- sort(grep("alpine country club|alphine country club", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(alpine); xpcost(alpine)
df$FullName[df$FullName %in% alpine] <- "Alpine Country Club"; rm(alpine)
lenz <- sort(grep("hew lenz", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(lenz); xpcost(lenz)
df$FullName[df$FullName %in% lenz] <- "Matthew Lenz"; rm(lenz)
apodaca <- sort(grep("martine apodaca", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(apodaca); xpcost(apodaca)
df$FullName[df$FullName %in% apodaca] <- "Martine Apodaca"; rm(apodaca)
horowski <- sort(grep("ith horowski", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(horowski); xpcost(horowski)
df$FullName[df$FullName %in% horowski] <- "Meredith Horowski"; rm(horowski)
rightstrat <- sort(grep("right strat", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(rightstrat); xpcost(rightstrat)
df$FullName[df$FullName %in% rightstrat] <- "Right Strategies"; rm(rightstrat)
cyd <- sort(grep("cyd mckenna", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(cyd); xpcost(cyd)
df$FullName[df$FullName %in% cyd] <- "Cyd Mckenna"; rm(cyd)
pui <- sort(grep("^pui-o|^pui o", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(pui); xpcost(pui)
df$FullName[df$FullName %in% pui] <- "Pui O"; rm(pui)
britt <- sort(grep("jeff britt|jeffrey britt", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(britt); xpcost(britt)
df$FullName[df$FullName %in% britt] <- "Jeffrey Britt"; rm(britt)
integrated <- sort(grep("integrated media", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(integrated); xpcost(integrated)
df$FullName[df$FullName %in% integrated] <- "Integrated Media Group"; rm(integrated)
bigger <- sort(grep("bigger", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(bigger); xpcost(bigger)
df$FullName[df$FullName %in% bigger] <- "The Bigger Know Company"; rm(bigger)
pauls <- sort(grep("pauls press|^print press$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(pauls); xpcost(pauls)
df$FullName[df$FullName %in% pauls] <- "Pauls Press"; rm(pauls)
wasylyk <- sort(grep("peter wasylk|peter wasylsk|peter wasylyk|peter n wasylyk", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(wasylyk); xpcost(wasylyk)
df$FullName[df$FullName %in% wasylyk] <- "Peter N Wasylyk"; rm(wasylyk)
chism <- sort(grep("^chism$|^chism strat", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(chism); xpcost(chism)
df$FullName[df$FullName %in% chism] <- "Chism Strategies"; rm(chism)
catanzaro <- sort(grep("kristen j catanzaro|kristen catanzaro|kristen catanzao", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(catanzaro); xpcost(catanzaro)
df$FullName[df$FullName %in% catanzaro] <- "Kristen J Catanzaro"; rm(catanzaro)
xtek <- sort(grep("x tek", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(xtek); xpcost(xtek)
df$FullName[df$FullName %in% xtek] <- "X Tek Solutions"; rm(xtek)
tatewosian <- sort(grep("tatewosian", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(tatewosian); xpcost(tatewosian)
df$FullName[df$FullName %in% tatewosian] <- "Campana Sarza and Tatewosian"; rm(tatewosian)
amtrak <- sort(grep("amtrak", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(amtrak); xpcost(amtrak)
df$FullName[df$FullName %in% amtrak] <- "Amtrak"; rm(amtrak)
jennings <- sort(grep("seth jennings", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(jennings); xpcost(jennings)
df$FullName[df$FullName %in% jennings] <- "Seth Jennings"; rm(jennings)
black <- sort(grep("black and white|^b and m printing|^d and m printing", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(black); xpcost(black)
df$FullName[df$FullName %in% black] <- "Black and White Printing"; rm(black)
blake <- sort(grep("blake collins", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(blake); xpcost(blake)
df$FullName[df$FullName %in% blake] <- "Blake Collins"; rm(blake)
blake <- sort(grep("blake filippi", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(blake); xpcost(blake)
df$FullName[df$FullName %in% blake] <- "Blake Filippi"; rm(blake)
montalbano <- sort(grep("joseph j montalbano|joseph mantalbano|joseph a montelbano|montalbono for senate|joseph montalbano|joseph a montalbano|^montalbano for senate", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(montalbano); xpcost(montalbano)
df$FullName[df$FullName %in% montalbano] <- "Joseph A Montalbano"; rm(montalbano)
handy <- sort(grep("arthur handy|art handy", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(handy); xpcost(handy)
df$FullName[df$FullName %in% handy] <- "Arthur Handy"; rm(handy)
hummell <- sort(grep("sarah hummell", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(hummell); xpcost(hummell)
df$FullName[df$FullName %in% hummell] <- "Sarah Hummell"; rm(hummell)
binder <- sort(grep("david binder", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(binder); xpcost(binder)
df$FullName[df$FullName %in% binder] <- "David Binder Research"; rm(binder)
mckendall <- sort(grep("friends of mckendall$|philip mckendall|philip r mckendall|phil mckendall|phillip mckendall", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mckendall); xpcost(mckendall)
df$FullName[df$FullName %in% mckendall] <- "Phillip Mckendall"; rm(mckendall)
sheehan <- sort(grep("james c sheehan|james sheehan|^jim sheehan", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sheehan); xpcost(sheehan)
df$FullName[df$FullName %in% sheehan] <- "James C Sheehan"; rm(sheehan)
horowtiz <- sort(grep("horowtiz", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(horowtiz); xpcost(horowtiz)
df$FullName[df$FullName %in% horowtiz] <- "Rob Horowtiz Assoc"; rm(horowtiz)
burchfield <- sort(grep("joseph s burchfield|joseph burchfield", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(burchfield); xpcost(burchfield)
df$FullName[df$FullName %in% burchfield] <- "Joseph S Burchfield"; rm(burchfield)
bannon <- sort(grep("bannon", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(bannon); xpcost(bannon)
df$FullName[df$FullName %in% bannon] <- "Bannon Communication Research"; rm(bannon)
foley <- sort(grep("jb foley|^j b foley|^jb foely|^jbfoley|^jp foely|^jp foley|^j p foley|^jd foley|^jb floey|foley print", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(foley); xpcost(foley)
df$FullName[df$FullName %in% foley] <- "JB Foley Printing"; rm(foley)
mueller <- sort(grep("lynn mueller", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mueller); xpcost(mueller)
df$FullName[df$FullName %in% mueller] <- "Lynn Mueller"; rm(mueller)
mahoney <- sort(grep("kelly mahoney", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mahoney); xpcost(mahoney)
df$FullName[df$FullName %in% mahoney] <- "Kelly Mahoney"; rm(mahoney)
damiano <- sort(grep("steven damiano", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(damiano); xpcost(damiano)
df$FullName[df$FullName %in% damiano] <- "Steven Damiano"; rm(damiano)
targetpoint <- sort(grep("target point", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(targetpoint); xpcost(targetpoint)
df$FullName[df$FullName %in% targetpoint] <- "Target Point Consulting"; rm(targetpoint)
gould <- sort(grep("george patrick gould", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(gould); xpcost(gould)
df$FullName[df$FullName %in% gould] <- "George Patrick Gould"; rm(gould)
profilestrat <- sort(grep("profile strat", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(profilestrat); xpcost(profilestrat)
df$FullName[df$FullName %in% profilestrat] <- "Profile Strategy Group"; rm(profilestrat)
connection <- sort(grep("connection strat|connections strategies", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(connection); xpcost(connection)
df$FullName[df$FullName %in% connection] <- "Connection Strategies"; rm(connection)
hamilton <- sort(grep("hamilton campaigns", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(hamilton); xpcost(hamilton)
df$FullName[df$FullName %in% hamilton] <- "Hamilton Campaigns"; rm(hamilton)
imageprint <- sort(grep("image print", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(imageprint); xpcost(imageprint)
df$FullName[df$FullName %in% imageprint] <- "Image Printing"; rm(imageprint)
kellyharris <- sort(grep("kelly harris", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(kellyharris); xpcost(kellyharris)
df$FullName[df$FullName %in% kellyharris] <- "Kelly Harris"; rm(kellyharris)
merolla <- sort(grep("steven b merolla|steve merolla|stephen merolla|steve mernola|steve merrolla|steven meralla", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(merolla); xpcost(merolla)
df$FullName[df$FullName %in% merolla] <- "Steve Merolla"; rm(merolla)
zeta <- sort(grep("zeta|zata", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(zeta); xpcost(zeta)
df$FullName[df$FullName %in% zeta] <- "Zata 3 Consulting"; rm(zeta)
youngdem <- sort(grep("rhode island young dem|^ydri pac$|ri young dem|young democrats of rhode|young democrats of ri", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(youngdem); xpcost(youngdem)
df$FullName[df$FullName %in% youngdem] <- "RI Young Democrats"; rm(youngdem)
xzito <- sort(grep("xzito|^yzito|^x2ito|^xcito creat", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(xzito); xpcost(xzito)
df$FullName[df$FullName %in% xzito] <- "Xzito Creative Solutions"; rm(xzito)
xtreme <- sort(grep("^xtreme|^x-treme", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(xtreme); xpcost(xtreme)
df$FullName[df$FullName %in% xtreme] <- "Xtreme Marketing and Graphics"; rm(xtreme)
wriw <- sort(grep("wriw", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(wriw); xpcost(wriw)
df$FullName[df$FullName %in% wriw] <- "WRIW Telemundo"; rm(wriw)
wrights <- sort(grep("wrights", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(wrights); xpcost(wrights)
df$FullName[df$FullName %in% wrights] <- "Wrights Dairy Farm"; rm(wrights)
worldtroph <- sort(grep("world troph|world tropies|wold throphies", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(worldtroph); xpcost(worldtroph)
df$FullName[df$FullName %in% worldtroph] <- "World Trophies"; rm(worldtroph)
woodbridge <- sort(grep("woodbridge|woodridge\\/ccp|woodridge ccb", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(woodbridge); xpcost(woodbridge)
df$FullName[df$FullName %in% woodbridge] <- "Woodbridge CCP"; rm(woodbridge)
winning <- sort(grep("winning way", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(winning); xpcost(winning)
df$FullName[df$FullName %in% winning] <- "Winning Ways"; rm(winning)
winning <- sort(grep("winning camp", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(winning); xpcost(winning)
df$FullName[df$FullName %in% winning] <- "Winning Campaign Products"; rm(winning)
willett <- sort(grep("willett real|willett ave", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(willett); xpcost(willett)
df$FullName[df$FullName %in% willett] <- "Willett Realty Assoc"; rm(willett)
waterplace <- sort(grep("waterplace|water place rest|^skyline$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(waterplace); xpcost(waterplace)
df$FullName[df$FullName %in% waterplace] <- "Waterplace Restaurant"; rm(waterplace)
visa <- sort(grep("visa", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(visa); xpcost(visa)
df$FullName[df$FullName %in% visa] <- "Visa"; rm(visa)
vintage <- sort(grep("vintage", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(vintage); xpcost(vintage)
df$FullName[df$FullName %in% vintage] <- "Vintage Restaurant"; rm(vintage)
vanasse <- sort(grep("vanasse med", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(vanasse); xpcost(vanasse)
df$FullName[df$FullName %in% vanasse] <- "Vanasse Media"; rm(vanasse)
lawson <- sort(grep("valarie lawson|valerie lawson|val lawson", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(lawson); xpcost(lawson)
df$FullName[df$FullName %in% lawson] <- "Valerie Lawson"; rm(lawson)
ups <- sort(grep("^ups|the ups|^u p s", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(ups); xpcost(ups)
df$FullName[df$FullName %in% ups] <- "UPS Store"; rm(ups)
toms <- sort(grep("toms mark", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(toms); xpcost(toms)
df$FullName[df$FullName %in% toms] <- "Toms Market"; rm(toms)
tomatocity <- sort(grep("tomato city", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(tomatocity); xpcost(tomatocity)
df$FullName[df$FullName %in% tomatocity] <- "Tomato City Pizza"; rm(tomatocity)
tomasellis <- sort(grep("tomasellis|^tomaselis|^tomaselli brothers", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(tomasellis); xpcost(tomasellis)
df$FullName[df$FullName %in% tomasellis] <- "Tomasellis"; rm(tomasellis)
toddbrien <- sort(grep("todd brien|todd r brien", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(toddbrien); xpcost(toddbrien)
df$FullName[df$FullName %in% toddbrien] <- "Todd Brien"; rm(toddbrien)
tyson <- sort(grep("tyson org", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(tyson); xpcost(tyson)
df$FullName[df$FullName %in% tyson] <- "The Tyson Organization"; rm(tyson)
hartford <- sort(grep("hartford ins", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(hartford); xpcost(hartford)
df$FullName[df$FullName %in% hartford] <- "The Hartford Insurance Co"; rm(hartford)
armory <- sort(grep("armory rev", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(armory); xpcost(armory)
df$FullName[df$FullName %in% armory] <- "The Armory Revival Co"; rm(armory)
stupid <- sort(grep("stupid t", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(stupid); xpcost(stupid)
df$FullName[df$FullName %in% stupid] <- "That Stupid T-Shirt Company"; rm(stupid)
targetmarket <- sort(grep("target market|target marget", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(targetmarket); xpcost(targetmarket)
df$FullName[df$FullName %in% targetmarket] <- "Target Marketing Group"; rm(targetmarket)
tailormade <- sort(grep("tailor made", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(tailormade); xpcost(tailormade)
df$FullName[df$FullName %in% tailormade] <- "Tailor Made Promotional Products"; rm(tailormade)
tmobile <- sort(grep("tmobile|t mobil|t-mobil", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(tmobile); xpcost(tmobile)
df$FullName[df$FullName %in% tmobile] <- "T-Mobile"; rm(tmobile)
damico <- sort(grep("damicos catering|^t damico$|^t damico cater|^tdamico|thomas damico|tom damico", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(damico); xpcost(damico)
df$FullName[df$FullName %in% damico] <- "T Damico Catering"; rm(damico)
sunpublish <- sort(grep("sun publish|sun public|^sun media", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sunpublish); xpcost(sunpublish)
df$FullName[df$FullName %in% sunpublish] <- "Sun Publishing"; rm(sunpublish)
alves <- sort(grep("steven alves|steve alves|stephen o alves|^alves$|stephen d alves|stephen alves|^alves for senate|^alves comm", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(alves); xpcost(alves)
df$FullName[df$FullName %in% alves] <- "Stephen D Alves"; rm(alves)
desilva <- sort(grep("stephanie desilva", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(desilva); xpcost(desilva)
df$FullName[df$FullName %in% desilva] <- "Stephanie Desilva"; rm(desilva)
spotrun <- sort(grep("spot run", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(spotrun); xpcost(spotrun)
df$FullName[df$FullName %in% spotrun] <- "Spot Runner"; rm(spotrun)
spotdes <- sort(grep("spot des", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(spotdes); xpcost(spotdes)
df$FullName[df$FullName %in% spotdes] <- "Spot Design"; rm(spotdes)
spain <- sort(grep("spain", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(spain); xpcost(spain)
df$FullName[df$FullName %in% spain] <- "Spain Restaurant"; rm(spain)
smoke <- sort(grep("^smoke$|smoke cigar lounge|smoke lounge", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(smoke); xpcost(smoke)
df$FullName[df$FullName %in% smoke] <- "Smoke Cigar Lounge"; rm(smoke)
smithfielddem <- sort(grep("^smithfield dem|^the smithfield dem", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(smithfielddem); xpcost(smithfielddem)
df$FullName[df$FullName %in% smithfielddem] <- "Smithfield Democratic Town Committee"; rm(smithfielddem)
smartcamp <- sort(grep("smart camp", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(smartcamp); xpcost(smartcamp)
df$FullName[df$FullName %in% smartcamp] <- "Smart Campaigns"; rm(smartcamp)
silverfox <- sort(grep("silver fox", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(silverfox); xpcost(silverfox)
df$FullName[df$FullName %in% silverfox] <- "Silver Fox Studios"; rm(silverfox)
signson <- sort(grep("signs on|signsonthecheapcom", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(signson); xpcost(signson)
df$FullName[df$FullName %in% signson] <- "Signs on the Cheap"; rm(signson)
signgraph <- sort(grep("sign graph|signs graph|sign grapics", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(signgraph); xpcost(signgraph)
df$FullName[df$FullName %in% signgraph] <- "Sign Graphics"; rm(signgraph)
shaws <- sort(grep("shaws", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(shaws); xpcost(shaws)
df$FullName[df$FullName %in% shaws] <- "Shaws"; rm(shaws)
yurdin <- sort(grep("seth yurdin|seth m yurdin|set yurdin|seeth yurdin", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(yurdin); xpcost(yurdin)
df$FullName[df$FullName %in% yurdin] <- "Seth Yurdin"; rm(yurdin)
screenprint <- sort(grep("^screen print", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(screenprint); xpcost(screenprint)
df$FullName[df$FullName %in% screenprint] <- "Screen Print"; rm(screenprint)
pollard <- sort(grep("scott m pollard|scott pollard|scot pollard", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(pollard); xpcost(pollard)
df$FullName[df$FullName %in% pollard] <- "Scott Pollard"; rm(pollard)
schofield <- sort(grep("sclofield print|schofield pront|schefield print|scholfield print|schofield paint|shofield print|schofield prt|schofeld print|schofield print", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(schofield); xpcost(schofield)
df$FullName[df$FullName %in% schofield] <- "Schofield Printing"; rm(schofield)
safeguard <- sort(grep("safeguard perfect", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(safeguard); xpcost(safeguard)
df$FullName[df$FullName %in% safeguard] <- "Safeguard Perfect Partners"; rm(safeguard)
rosalina <- sort(grep("rosalina", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(rosalina); xpcost(rosalina)
df$FullName[df$FullName %in% rosalina] <- "Rosalina Restaurant"; rm(rosalina)
risaff <- sort(grep("risaff|state association of fire|state assoc of fire|state assoc-fire", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(risaff); xpcost(risaff)
df$FullName[df$FullName %in% risaff] <- "Rhode Island State Association of Firefighters"; rm(risaff)
ribs <- sort(grep("ribs and co|^place for ribs|^place of ribs", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(ribs); xpcost(ribs)
df$FullName[df$FullName %in% ribs] <- "Ribs and Company"; rm(ribs)
trial <- sort(grep("^ri trial judge|^rhode island trial judge", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(trial); xpcost(trial)
df$FullName[df$FullName %in% trial] <- "RI Trial Judges Association"; rm(trial)
tolife <- sort(grep("^ri rtl$|right to life|rite to life", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(tolife); xpcost(tolife)
df$FullName[df$FullName %in% tolife] <- "RI Right to Life"; rm(tolife)
planned <- sort(grep("^ppvri$|^pp votes|^ppv!ri$|planned parenthood", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(planned); xpcost(planned)
df$FullName[df$FullName %in% planned] <- "Planned Parenthood Votes RI"; rm(planned)
jobs <- sort(grep("jobs for justice|jobs with justice|jobs w\\/ justice", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(jobs); xpcost(jobs)
df$FullName[df$FullName %in% jobs] <- "RI Jobs for Justice"; rm(jobs)
leadership <- sort(grep("^house democratic committee|^house democratic campaign committee|ri democratic leadership|island democratic leadership|ouse democrats$|house dem leader|house democrat leadership|house democratic leadership", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(leadership); xpcost(leadership)
df$FullName[df$FullName %in% leadership] <- "RI House Democratic Leadership Committee"; rm(leadership)
rhodes <- sort(grep("susanna rhodes|susanna r beckwith", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(rhodes); xpcost(rhodes)
df$FullName[df$FullName %in% rhodes] <- "Susanna Rhodes Beckwith"; rm(rhodes)
rhodes <- sort(grep("rhodes on the|rhodes of the", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(rhodes); xpcost(rhodes)
df$FullName[df$FullName %in% rhodes] <- "Rhodes on the Pawtuxet"; rm(rhodes)
college <- sort(grep("^ric found|r i college found|^ri college found|rhode island college found", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(college); xpcost(college)
df$FullName[df$FullName %in% college] <- "RI College Foundation"; rm(college)
college <- sort(grep("^ri college|^ric |^r i c$|^rhode island college|^ri college$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(college); xpcost(college)
df$FullName[df$FullName %in% college] <- "RI College"; rm(college)
college <- sort(grep("providence college|providene college athletic|providence college athletics", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(college); xpcost(college)
df$FullName[df$FullName %in% college] <- "Providence College"; rm(college)
rhoda <- sort(grep("rhodaa perry|rhonda e perry|rhoda perry|rhoda e perry", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(rhoda); xpcost(rhoda)
df$FullName[df$FullName %in% rhoda] <- "Rhoda Perry"; rm(rhoda)
devall <- sort(grep("frank devall|frank a devall|^devall for|^f devall|^devall$|senator devall|frank allen devall", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(devall); xpcost(devall)
df$FullName[df$FullName %in% devall] <- "Frank Devall"; rm(devall)
johnston <- sort(grep("ray johnston|raymond h johnston|raymond johnston|raymond h johnson|raymond johnson|robert johnston jr", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(johnston); xpcost(johnston)
df$FullName[df$FullName %in% johnston] <- "Raymond Johnston Jr"; rm(johnston)
rapid <- sort(grep("^rapid$|^rapid prin", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(rapid); xpcost(rapid)
df$FullName[df$FullName %in% rapid] <- "Rapid Printing"; rm(rapid)
radisson <- sort(grep("radison airport hotel|radisson airport hotel|radisson hotel|radisson plaza hotel|radisson warwick|radisson bistro", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(radisson); xpcost(radisson)
df$FullName[df$FullName %in% radisson] <- "Radisson Hotel"; rm(radisson)
rentals <- sort(grep("^ri rentals$|^ri rental$|rhode island rental$|rhode island rentals$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(rentals); xpcost(rentals)
df$FullName[df$FullName %in% rentals] <- "RI Rentals"; rm(rentals)
rentals <- sort(grep("quality rental|qualtiy rental|qualty rental|^rentals of north providence$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(rentals); xpcost(rentals)
df$FullName[df$FullName %in% rentals] <- "Quality Rentals"; rm(rentals)
purepost <- sort(grep("purepostcard|pure postcard", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(purepost); xpcost(purepost)
df$FullName[df$FullName %in% purepost] <- "Pure Postcards"; rm(purepost)
central <- sort(grep("central federated|central lab", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(central); xpcost(central)
df$FullName[df$FullName %in% central] <- "Providence Central Federated Council"; rm(central)
printshop <- sort(grep("printshop|^print shops$|^paint shops$|^print shop$|print shops inc", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(printshop); xpcost(printshop)
df$FullName[df$FullName %in% printshop] <- "Print Shop"; rm(printshop)
premiumgraph <- sort(grep("premier graphicx|^sign elect|^signelect|premium graph|^gsp graphic", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(premiumgraph); xpcost(premiumgraph)
df$FullName[df$FullName %in% premiumgraph] <- "Premium Graphics"; rm(premiumgraph)
postcard <- sort(grep("postcard mania|^postcardmania|^postacard mania", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(postcard); xpcost(postcard)
df$FullName[df$FullName %in% postcard] <- "Postcard Mania"; rm(postcard)
pma <- sort(grep("^pma$|^pma engine|^pma ind|^pma audio", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(pma); xpcost(pma)
df$FullName[df$FullName %in% pma] <- "PMA Industries"; rm(pma)
pdq <- sort(grep("^pdq|pdg graphics|^pdg print|^poq print|^p d q print", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(pdq); xpcost(pdq)
df$FullName[df$FullName %in% pdq] <- "PDQ Printing"; rm(pdq)
germain <- sort(grep("paul st germain|paul stgerm|paul st germ|paul e st germ", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(germain); xpcost(germain)
df$FullName[df$FullName %in% germain] <- "Paul St Germain"; rm(germain)
afl <- sort(grep("jersey afl|jersey state afl", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(afl); xpcost(afl)
df$FullName[df$FullName %in% afl] <- "New Jersey AFL-CIO Cope"; rm(afl)
patricks <- sort(grep("patricks pier|patrick pier one", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(patricks); xpcost(patricks)
df$FullName[df$FullName %in% patricks] <- "Patricks Pier One"; rm(patricks)
patch <- sort(grep("patch media|^the patch|patchcom|^patch$|coventry patch|newport patch|patch east green|middletown patch|prov patch|johnston patch", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(patch); xpcost(patch)
df$FullName[df$FullName %in% patch] <- "Patch Media Group"; rm(patch)
parkprint <- sort(grep("park print|^pork print", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(parkprint); xpcost(parkprint)
df$FullName[df$FullName %in% parkprint] <- "Park Printers"; rm(parkprint)
pane <- sort(grep("pane e vino|pane a vino|^pane vino$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(pane); xpcost(pane)
df$FullName[df$FullName %in% pane] <- "Pane E Vino"; rm(pane)
orion <- sort(grep("orion precision", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(orion); xpcost(orion)
df$FullName[df$FullName %in% orion] <- "Orion Precision"; rm(orion)
opggraph <- sort(grep("total graphic solution|^t g s$|^tgs$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(opggraph); xpcost(opggraph)
df$FullName[df$FullName %in% opggraph] <- "Total Graphic Solutions"; rm(opggraph)
northeast <- sort(grep("^the independent news|^independent newpapers|northeast ind|^n e inde|^ne inde|^north east inde|^independent newsp", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(northeast); xpcost(northeast)
df$FullName[df$FullName %in% northeast] <- "Northeast Independent"; rm(northeast)
newporttent <- sort(grep("newport tent", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(newporttent); xpcost(newporttent)
df$FullName[df$FullName %in% newporttent] <- "Newport Tent Company"; rm(newporttent)
englandimage <- sort(grep("^ne image and print|^new englan image and print|^ne imagek print|england image", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(englandimage); xpcost(englandimage)
df$FullName[df$FullName %in% englandimage] <- "New England Image and Print"; rm(englandimage)
nationalresearch <- sort(grep("national research", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(nationalresearch); xpcost(nationalresearch)
df$FullName[df$FullName %in% nationalresearch] <- "National Research Inc"; rm(nationalresearch)
nationalpen <- sort(grep("national pen", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(nationalpen); xpcost(nationalpen)
df$FullName[df$FullName %in% nationalpen] <- "National Pen co"; rm(nationalpen)
narragansettac <- sort(grep("narragansett ac", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(narragansettac); xpcost(narragansettac)
df$FullName[df$FullName %in% narragansettac] <- "Narragansett Accessories"; rm(narragansettac)
msl <- sort(grep("^msl$|^msl tele|^msl solution", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(msl); xpcost(msl)
df$FullName[df$FullName %in% msl] <- "MSL Teleservices"; rm(msl)
mills <- sort(grep("mills tav", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mills); xpcost(mills)
df$FullName[df$FullName %in% mills] <- "Mills Tavern"; rm(mills)
millonzi <- sort(grep("millonzi|^milonzi|^milonzzi", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(millonzi); xpcost(millonzi)
df$FullName[df$FullName %in% millonzi] <- "Millonzi Fine Catering"; rm(millonzi)
melba <- sort(grep("melba depena|melba d depena|^lemba depena", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(melba); xpcost(melba)
df$FullName[df$FullName %in% melba] <- "Melba Depena"; rm(melba)
mealwork <- sort(grep("^mealworks|^meal works", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mealwork); xpcost(mealwork)
df$FullName[df$FullName %in% mealwork] <- "Meal Works"; rm(mealwork)
marriot <- sort(grep("marriot", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(marriot); xpcost(marriot)
prov <- grep("prov", marriot, value = TRUE, ignore.case = TRUE) %>% print
marriot <- marriot[!marriot %in% prov]; xpend(marriot); xpcost(marriot)
df$FullName[df$FullName %in% marriot] <- "Marriot Hotels"; rm(marriot)
jimenez <- sort(grep("manuel jimenez", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(jimenez); xpcost(jimenez)
df$FullName[df$FullName %in% jimenez] <- "Manuel Jimenez"; rm(jimenez)
mailingsol <- sort(grep("mailing sol", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mailingsol); xpcost(mailingsol)
df$FullName[df$FullName %in% mailingsol] <- "Mailing Solutions"; rm(mailingsol)
luigis <- sort(grep("^luigis gourmet|luigis rest|luigis groumet|^luigis$|^luigis express", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(luigis); xpcost(luigis)
df$FullName[df$FullName %in% luigis] <- "Luigis"; rm(luigis)
andes <- sort(grep("los andes|^los andeas", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(andes); xpcost(andes)
df$FullName[df$FullName %in% andes] <- "Los Andes Restaurant"; rm(andes)
leftbrain <- sort(grep("leftbrain|left brain", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(leftbrain); xpcost(leftbrain)
df$FullName[df$FullName %in% leftbrain] <- "Left Brain"; rm(leftbrain)
lancellottas <- sort(grep("^lancellotta|^lancelottas|^lancellotas|^lancello ttas|lancellottas", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(lancellottas); xpcost(lancellottas)
df$FullName[df$FullName %in% lancellottas] <- "Lancellottas"; rm(lancellottas)
kernan <- sort(grep("kernan f king|kernan king", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(kernan); xpcost(kernan)
df$FullName[df$FullName %in% kernan] <- "Kernan F King"; rm(kernan)
mckay <- sort(grep("kenneth mckay$|kenneth k mckay$|kenneth mckay iv|kenneth k mckay iv", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mckay); xpcost(mckay)
df$FullName[df$FullName %in% mckay] <- "Kenneth K Mckay IV"; rm(mckay)
julios <- sort(grep("julios rest|julios family|^julios pizza", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(julios); xpcost(julios)
df$FullName[df$FullName %in% julios] <- "Julios Restaurant"; rm(julios)
baginski <- sort(grep("baginski", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(baginski); xpcost(baginski)
df$FullName[df$FullName %in% baginski] <- "Jacquelyn Baginski"; rm(baginski)
bissaillon <- sort(grep("jacob bissaillon|jake bissaillon|jacob e bissaillon", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(bissaillon); xpcost(bissaillon)
df$FullName[df$FullName %in% bissaillon] <- "Jacob Bissaillon"; rm(bissaillon)
harsch <- sort(grep("j w harsch|j william w harsch", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(harsch); xpcost(harsch)
df$FullName[df$FullName %in% harsch] <- "J William W Harsch"; rm(harsch)
ims <- sort(grep("^ims", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(ims); xpcost(ims)
df$FullName[df$FullName %in% ims] <- "IMS Inc"; rm(ims)
image <- sort(grep("^image$|^image media|^imagemedia", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(image); xpcost(image)
df$FullName[df$FullName %in% image] <- "Image Media"; rm(image)
fornello <- sort(grep("il fornello|^fornello$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(fornello); xpcost(fornello)
df$FullName[df$FullName %in% fornello] <- "Il Fornello"; rm(fornello)
iggy <- sort(grep("iggy", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(iggy); xpcost(iggy)
df$FullName[df$FullName %in% iggy] <- "Iggys Chowder House"; rm(iggy)
hyatt <- sort(grep("hyatt", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(hyatt); xpcost(hyatt)
df$FullName[df$FullName %in% hyatt] <- "Hyatt Hotels"; rm(hyatt)
hose <- sort(grep("^hose co", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(hose); xpcost(hose)
df$FullName[df$FullName %in% hose] <- "Hose Company #6 Restaurant"; rm(hose)
hopkins <- sort(grep("hopkins press|hopkin press", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(hopkins); xpcost(hopkins)
df$FullName[df$FullName %in% hopkins] <- "Hopkins Press"; rm(hopkins)
artiste <- sort(grep("hope artiste", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(artiste); xpcost(artiste)
df$FullName[df$FullName %in% artiste] <- "Hope Artiste Village"; rm(artiste)
hometown <- sort(grep("hometown news|home town news", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(hometown); xpcost(hometown)
df$FullName[df$FullName %in% hometown] <- "Hometown Newspapers"; rm(hometown)
hgbb <- sort(grep("hgbc hall|^hgbc$|^hgbm$|^h g b b of r i|holy ghost bortherhood|hgbb|holy ghost brotherhood|holy ghost beneficial", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(hgbb); xpcost(hgbb)
df$FullName[df$FullName %in% hgbb] <- "HGBB Organization"; rm(hgbb)
healthsource <- sort(grep("healthsource|health source", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(healthsource); xpcost(healthsource)
df$FullName[df$FullName %in% healthsource] <- "Health Source RI"; rm(healthsource)
ramel <- sort(grep("guillame h de ramel|guillaune deranel|guillaume de ramel|friends of deramel|friends of de ramel|guillaume deramel|guillaume h de ramel|guillame deramel", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(ramel); xpcost(ramel)
df$FullName[df$FullName %in% ramel] <- "Guillaume De Ramel"; rm(ramel)
gragert <- sort(grep("gragert jones", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(gragert); xpcost(gragert)
df$FullName[df$FullName %in% gragert] <- "Gragert Jones"; rm(gragert)
gian <- sort(grep("gian carl", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(gian); xpcost(gian)
df$FullName[df$FullName %in% gian] <- "Gian Carlos Restaurant"; rm(gian)
general <- sort(grep("general treasure", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(general); xpcost(general)
df$FullName[df$FullName %in% general] <- "RI General Treasurer"; rm(general)
gatehouse <- sort(grep("gatehouse rest", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(gatehouse); xpcost(gatehouse)
df$FullName[df$FullName %in% gatehouse] <- "Gatehouse Restaurant"; rm(gatehouse)
gannon <- sort(grep("gannon sign|^gannon$|^gannon graph|^ganon sign|^grann on sign", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(gannon); xpcost(gannon)
df$FullName[df$FullName %in% gannon] <- "Gannon Sign"; rm(gannon)
galvinand <- sort(grep("galvin and gudas", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(galvinand); xpcost(galvinand)
df$FullName[df$FullName %in% galvinand] <- "Galvin and Gudas"; rm(galvinand)
marcello <- sort(grep("michael marcello|mike marcello|rep marcello|^marcello for rep|michael j marcello|micheal marcello", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(marcello); xpcost(marcello)
df$FullName[df$FullName %in% marcello] <- "Michael Marcello"; rm(marcello)
naughton <- sort(grep("eileeen naughton|ellen naughton|eileen slattery naughton|eillen naughton|eileen noaghton|naughton eileen|eileen naughton|eileen s naughton", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(naughton); xpcost(naughton)
df$FullName[df$FullName %in% naughton] <- "Eileen Naughton"; rm(naughton)
leased <- sort(grep("leased business", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(leased); xpcost(leased)
df$FullName[df$FullName %in% leased] <- "Flexible Leased Business Spaces"; rm(leased)
fleming <- sort(grep("flemings", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(fleming); xpcost(fleming)
df$FullName[df$FullName %in% fleming] <- "Flemings"; rm(fleming)
fleet <- sort(grep("fleet bank|fleet credit|fleet platinum", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(fleet); xpcost(fleet)
df$FullName[df$FullName %in% fleet] <- "Fleet Bank"; rm(fleet)
fedex <- sort(grep("fedex|^feo ex|fed ex|fed\\/ex|^fedx|^federal express|^fed-ex", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(fedex); xpcost(fedex)
df$FullName[df$FullName %in% fedex] <- "Fedex"; rm(fedex)
election <- sort(grep("federal election", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(election); xpcost(election)
df$FullName[df$FullName %in% election] <- "Federal Election Commission"; rm(election)
expedia <- sort(grep("expedia", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(expedia); xpcost(expedia)
df$FullName[df$FullName %in% expedia] <- "Expedia"; rm(expedia)
evan <- sort(grep("evan jul", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(evan); xpcost(evan)
df$FullName[df$FullName %in% evan] <- "Evan Juliano"; rm(evan)
evan <- sort(grep("evan gill", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(evan); xpcost(evan)
df$FullName[df$FullName %in% evan] <- "Evan Gillessie"; rm(evan)
eramian <- sort(grep("^eramian sign|john eramian|^eramian$|^aramian sign|^eramain sign|^eramain sign|^eramin sign|^eranion sign|^eremian sign|ermanian john sign|^ermanian sign", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(eramian); xpcost(eramian)
df$FullName[df$FullName %in% eramian] <- "Eramian Sign Corp"; rm(eramian)
enterprise <- sort(grep("ballyhoo enterprise", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(enterprise); xpcost(enterprise)
df$FullName[df$FullName %in% enterprise] <- "Ballyhoo Enterprises"; rm(enterprise)
enterprise <- sort(grep("merlyn enterprise", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(enterprise); xpcost(enterprise)
df$FullName[df$FullName %in% enterprise] <- "Merlyn Enterprises"; rm(enterprise)
enterprise <- sort(grep("dano enterprise", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(enterprise); xpcost(enterprise)
df$FullName[df$FullName %in% enterprise] <- "Dano Enterprises"; rm(enterprise)
enterprise <- sort(grep("enterprise rent", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(enterprise); xpcost(enterprise)
df$FullName[df$FullName %in% enterprise] <- "Enterprise Rent a Car"; rm(enterprise)
elmwood <- sort(grep("elmwood sports|elmwood sorts", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(elmwood); xpcost(elmwood)
df$FullName[df$FullName %in% elmwood] <- "Elmwood Sports"; rm(elmwood)
elmhurst <- sort(grep("elmhurst youth|elmhurst little league|elmhurst youth baseball|elmhurst boys baseball", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(elmhurst); xpcost(elmhurst)
df$FullName[df$FullName %in% elmhurst] <- "Elmhurst Youth Baseball"; rm(elmhurst)
silverlake <- sort(grep("olneyville little league|silver lake little|silverlake\\/olneyville little league", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(silverlake); xpcost(silverlake)
df$FullName[df$FullName %in% silverlake] <- "Silver Lake Olneyville Little League"; rm(silverlake)
johnstonlittle <- sort(grep("johnston little", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(johnstonlittle); xpcost(johnstonlittle)
df$FullName[df$FullName %in% johnstonlittle] <- "Johnston Little League"; rm(johnstonlittle)
johnstonlittle <- sort(grep("johnston girls softball|johnston softball", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(johnstonlittle); xpcost(johnstonlittle)
df$FullName[df$FullName %in% johnstonlittle] <- "Johnston Girls Softball"; rm(johnstonlittle)
providencewest <- sort(grep("^no providence east little league|north prov little|north providence east little league|north providence west little|npwll", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(providencewest); xpcost(providencewest)
df$FullName[df$FullName %in% providencewest] <- "North Providence Little League"; rm(providencewest)
mountpleasant <- sort(grep("^mt plesant little league|^mt pleasant little league|^mt pleasant league|mount pleasant little|mtount pleasant little league", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mountpleasant); xpcost(mountpleasant)
df$FullName[df$FullName %in% mountpleasant] <- "Mount Pleasant Little League"; rm(mountpleasant)
elmwood <- sort(grep("elmwood little", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(elmwood); xpcost(elmwood)
df$FullName[df$FullName %in% elmwood] <- "Elmwood Little League"; rm(elmwood)
softball <- sort(grep("north providence fastpitch|north providence fast pitch|north providence girls fast|^np girls fast|^n providence girls fast", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(softball); xpcost(softball)
df$FullName[df$FullName %in% softball] <- "North Providence Girls Fast Pitch Softball"; rm(softball)
edwin <- sort(grep("edward pacheco|friends of ed pacheco|friends of rep pacheco|pacheco r edwin|edwin r pacheco|edwin pacheco", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(edwin); xpcost(edwin)
df$FullName[df$FullName %in% edwin] <- "Edwin R Pacheco"; rm(edwin)
easy <- sort(grep("easy entertain|^east entertaining", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(easy); xpcost(easy)
df$FullName[df$FullName %in% easy] <- "Easy Entertaining"; rm(easy)
artisan <- sort(grep("east coast artisaus|east coast artisians|east coast arisians|coast artisan", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(artisan); xpcost(artisan)
df$FullName[df$FullName %in% artisan] <- "East Coast Artisans"; rm(artisan)
lally <- sort(grep("donald j lally|^lally$|donald lally|don lally", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(lally); xpcost(lally)
df$FullName[df$FullName %in% lally] <- "Donald J Lally"; rm(lally)
dawson <- sort(grep("hodgeson for senate|dodgson for senate|dawson tucker hodgson|^hodgson for senate|^hodgson for state|dawson t hodgenson|dawson t hodgson|dawson hodgson", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(dawson); xpcost(dawson)
df$FullName[df$FullName %in% dawson] <- "Dawson T Hodgson"; rm(dawson)
dawson <- sort(grep("dawson mc", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(dawson); xpcost(dawson)
df$FullName[df$FullName %in% dawson] <- "Dawson Mccarthy Nelson Media"; rm(dawson)
cumulus <- sort(grep("cumulus", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(cumulus); xpcost(cumulus)
df$FullName[df$FullName %in% cumulus] <- "Cumulus Media"; rm(cumulus)
crown <- sort(grep("crowne plaza|crown plaza", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(crown); xpcost(crown)
df$FullName[df$FullName %in% crown] <- "Crowne Plaza"; rm(crown)
cozy <- sort(grep("cozy cater", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(cozy); xpcost(cozy)
df$FullName[df$FullName %in% cozy] <- "Cozy Caterers"; rm(cozy)
connolly <- sort(grep("andrew p connolly", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(connolly); xpcost(connolly)
df$FullName[df$FullName %in% connolly] <- "Andrew P Connolly"; rm(connolly)
connolly <- sort(grep("connolly print", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(connolly); xpcost(connolly)
df$FullName[df$FullName %in% connolly] <- "Connolly Printing"; rm(connolly)
connectcall <- sort(grep("connect call|connectcall", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(connectcall); xpcost(connectcall)
df$FullName[df$FullName %in% connectcall] <- "Connect Call USA"; rm(connectcall)
llincoln <- sort(grep("churchhill lincoln|churchill lincoln", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(llincoln); xpcost(llincoln)
df$FullName[df$FullName %in% llincoln] <- "Churchill Lincoln"; rm(llincoln)
tsonos <- sort(grep("charles tsonos|charles s tsonos|charlie tsonos|charles stephen tsonos", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(tsonos); xpcost(tsonos)
df$FullName[df$FullName %in% tsonos] <- "Charles Tsonos"; rm(tsonos)
levesque <- sort(grep("charles levesque|charles j levesque|chuck levesque|^levesque for", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(levesque); xpcost(levesque)
df$FullName[df$FullName %in% levesque] <- "Charles J Levesque"; rm(levesque)
drumm <- sort(grep("carolyn drumm|carolyn j drumm", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(drumm); xpcost(drumm)
df$FullName[df$FullName %in% drumm] <- "Carolyn Drumm"; rm(drumm)
mcentee <- sort(grep("carol hagan|carol mcenter|carol h mcentee|carol mcentee|carol hagan mcentee|hagen-mcentee|friends of mcentee", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mcentee); xpcost(mcentee)
df$FullName[df$FullName %in% mcentee] <- "Carol H Mcentee"; rm(mcentee)
candidatesignscom <- sort(grep("candidatesignscom|candidate sign", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(candidatesignscom); xpcost(candidatesignscom)
df$FullName[df$FullName %in% candidatesignscom] <- "Candidatesignscom"; rm(candidatesignscom)
buildasigncom <- sort(grep("buildasignco|build a sign", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(buildasigncom); xpcost(buildasigncom)
df$FullName[df$FullName %in% buildasigncom] <- "Buildasigncom"; rm(buildasigncom)
bradford <- sort(grep("bradford press|branford press|brandford press", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(bradford); xpcost(bradford)
df$FullName[df$FullName %in% bradford] <- "Bradford Press"; rm(bradford)
ballyhoo <- sort(grep("ballyhoo promo|^ballyhoo$|^ballyhoo prod", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(ballyhoo); xpcost(ballyhoo)
df$FullName[df$FullName %in% ballyhoo] <- "Ballyhoo Promotions"; rm(ballyhoo)
bacaro <- sort(grep("^bacaro|^bacard$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(bacaro); xpcost(bacaro)
df$FullName[df$FullName %in% bacaro] <- "Bacaro Restaurant"; rm(bacaro)
auroracivic <- sort(grep("^aurora club$|aurora club civic assoc|aurpra civic assoc|auror civic assoc|aurora civ", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(auroracivic); xpcost(auroracivic)
df$FullName[df$FullName %in% auroracivic] <- "Aurora Civic Association"; rm(auroracivic)
finkelstein <- sort(grep("finkelstein", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(finkelstein); xpcost(finkelstein)
df$FullName[df$FullName %in% finkelstein] <- "Arthur J Finkelstein and Assoc"; rm(finkelstein)
alpha <- sort(grep("alpha research", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(alpha); xpcost(alpha)
df$FullName[df$FullName %in% alpha] <- "Alpha Research"; rm(alpha)
alpha <- sort(grep("^alph graphics|alphagraphics|alpha graphics|alpharaphics|alpha dog graphics", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(alpha); xpcost(alpha)
df$FullName[df$FullName %in% alpha] <- "Alpha Graphics"; rm(alpha)
airportplaza <- sort(grep("airport plaza", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(airportplaza); xpcost(airportplaza)
df$FullName[df$FullName %in% airportplaza] <- "Airport Plaza Associates"; rm(airportplaza)
advantage <- sort(grep("advantage spec|advanatge specialties|advatage special", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(advantage); xpcost(advantage)
df$FullName[df$FullName %in% advantage] <- "Advantage Specialties"; rm(advantage)
acumen <- sort(grep("acumen print|accumen printing|acuman print", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(acumen); xpcost(acumen)
df$FullName[df$FullName %in% acumen] <- "Acumen Printing"; rm(acumen)
allthings <- sort(grep("all things considered|^atc promotions", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(allthings); xpcost(allthings)
df$FullName[df$FullName %in% allthings] <- "All Things Considered Promotions"; rm(allthings)
rosenthal <- sort(grep("rosenthal", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(rosenthal); xpcost(rosenthal)
df$FullName[df$FullName %in% rosenthal] <- "Amy F Rosenthal"; rm(rosenthal)
cherry <- sort(grep("anthony cherry", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(cherry); xpcost(cherry)
df$FullName[df$FullName %in% cherry] <- "Anthony Cherry"; rm(cherry)
berg <- sort(grep("andrew berg", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(berg); xpcost(berg)
df$FullName[df$FullName %in% berg] <- "Andrew Berg"; rm(berg)
broesder <- sort(grep("broesder", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(broesder); xpcost(broesder)
df$FullName[df$FullName %in% broesder] <- "Brett Broesder"; rm(broesder)
baer <- sort(grep("baer comm", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(baer); xpcost(baer)
df$FullName[df$FullName %in% baer] <- "Baer Communications"; rm(baer)
bristolphoenix <- sort(grep("bristol phoenix", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(bristolphoenix); xpcost(bristolphoenix)
df$FullName[df$FullName %in% bristolphoenix] <- "Bristol Phoenix"; rm(bristolphoenix)
bluelabs <- sort(grep("bluelabs", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(bluelabs); xpcost(bluelabs)
df$FullName[df$FullName %in% bluelabs] <- "Bluelabs"; rm(bluelabs)
farrell <- sort(grep("chris farrell", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(farrell); xpcost(farrell)
df$FullName[df$FullName %in% farrell] <- "Chris Farrell"; rm(farrell)
cleanwater <- sort(grep("clean water", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(cleanwater); xpcost(cleanwater)
df$FullName[df$FullName %in% cleanwater] <- "Clean Water Action"; rm(cleanwater)
creative <- sort(grep("creative imp", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(creative); xpcost(creative)
df$FullName[df$FullName %in% creative] <- "Creative Impression"; rm(creative)
allard <- sort(grep("david allard|david ballard", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(allard); xpcost(allard)
df$FullName[df$FullName %in% allard] <- "David Allard"; rm(allard)
divers <- sort(grep("diversified research", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(divers); xpcost(divers)
df$FullName[df$FullName %in% divers] <- "Diversified Research"; rm(divers)
euer <- sort(grep("dawn euer|friends of dawn$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(euer); xpcost(euer)
df$FullName[df$FullName %in% euer] <- "Dawn Euer"; rm(euer)
arcand <- sort(grep("erin arcand", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(arcand); xpcost(arcand)
df$FullName[df$FullName %in% arcand] <- "Erin Arcand"; rm(arcand)
wallin <- sort(grep("erik wallin", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(wallin); xpcost(wallin)
df$FullName[df$FullName %in% wallin] <- "Erik Wallin"; rm(wallin)
lamb <- sort(grep("erika lamb", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(lamb); xpcost(lamb)
df$FullName[df$FullName %in% lamb] <- "Erika Lamb"; rm(lamb)
bouclin <- sort(grep("ed bouclin|edward bouclin", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(bouclin); xpcost(bouclin)
df$FullName[df$FullName %in% bouclin] <- "Edward Bouclin"; rm(bouclin)
samsel <- sort(grep("emily samsel|samsel", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(samsel); xpcost(samsel)
df$FullName[df$FullName %in% samsel] <- "Emily Samsel"; rm(samsel)
focus <- sort(grep("focus bus", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(focus); xpcost(focus)
df$FullName[df$FullName %in% focus] <- "Focus Business Solutions"; rm(focus)
schadone <- sort(grep("gregory j schodone|greg schadone|gregory schadone|gregory j schadone|friends of schadone|schadone committee", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(schadone); xpcost(schadone)
df$FullName[df$FullName %in% schadone] <- "Gregory Schadone"; rm(schadone)
abelson <- sort(grep("guy abelson", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(abelson); xpcost(abelson)
df$FullName[df$FullName %in% abelson] <- "Guy Abelson Catering"; rm(abelson)
maynard <- sort(grep("greg maynard", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(maynard); xpcost(maynard)
df$FullName[df$FullName %in% maynard] <- "Greg Maynard"; rm(maynard)
mcneilly <- sort(grep("greg mcneilly", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mcneilly); xpcost(mcneilly)
df$FullName[df$FullName %in% mcneilly] <- "Greg Mcneilly"; rm(mcneilly)
pilkington <- sort(grep("grant pilkington", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(pilkington); xpcost(pilkington)
df$FullName[df$FullName %in% pilkington] <- "Grant Pilkington"; rm(pilkington)
ikon <- sort(grep("ikon office|^ikon$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(ikon); xpcost(ikon)
df$FullName[df$FullName %in% ikon] <- "Ikon Office Solutions"; rm(ikon)
london <- sort(grep("jake london", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(london); xpcost(london)
df$FullName[df$FullName %in% london] <- "Jake London"; rm(london)
josephsitalian <- sort(grep("josephs italian", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(josephsitalian); xpcost(josephsitalian)
df$FullName[df$FullName %in% josephsitalian] <- "Josephs Italian Kitchen"; rm(josephsitalian)
jmcomm <- sort(grep("jm comm", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(jmcomm); xpcost(jmcomm)
df$FullName[df$FullName %in% jmcomm] <- "JM Communications"; rm(jmcomm)
espinoza <- sort(grep("juan espinoza", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(espinoza); xpcost(espinoza)
df$FullName[df$FullName %in% espinoza] <- "Juan Espinoza"; rm(espinoza)
clynch <- sort(grep("john c lynch", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(clynch); xpcost(clynch)
df$FullName[df$FullName %in% clynch] <- "John C Lynch"; rm(clynch)
hypolite <- sort(grep("katherine hypolite", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(hypolite); xpcost(hypolite)
df$FullName[df$FullName %in% hypolite] <- "Katherine Hypolite"; rm(hypolite)
cantwell <- sort(grep("kathryn cantwell", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(cantwell); xpcost(cantwell)
df$FullName[df$FullName %in% cantwell] <- "Kathryn Cantwell"; rm(cantwell)
kristenmurphy <- sort(grep("kristen murphy|kristin murphy", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(kristenmurphy); xpcost(kristenmurphy)
df$FullName[df$FullName %in% kristenmurphy] <- "Kristen Murphy"; rm(kristenmurphy)
horan <- sort(grep("kevin horan", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(horan); xpcost(horan)
df$FullName[df$FullName %in% horan] <- "Kevin Horan"; rm(horan)
kirkbrae <- sort(grep("kirkbrae", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(kirkbrae); xpcost(kirkbrae)
df$FullName[df$FullName %in% kirkbrae] <- "Kirkbrae Country Club"; rm(kirkbrae)
laguens <- sort(grep("laguens kelly", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(laguens); xpcost(laguens)
df$FullName[df$FullName %in% laguens] <- "Laguens Kelly Klose"; rm(laguens)
raptakis <- sort(grep("^raptakis for|leonidis p raptakis|senator raptakis|leonidas raptakis|leonidas p raptakis|lou raptakis|louis raptakis", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(raptakis); xpcost(raptakis)
df$FullName[df$FullName %in% raptakis] <- "Lou Raptakis"; rm(raptakis)
blais <- sort(grep("leo blais|leo r blais|blais election committee", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(blais); xpcost(blais)
df$FullName[df$FullName %in% blais] <- "Leo R Blais"; rm(blais)
estrada <- sort(grep("luis estrada|estrada book", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(estrada); xpcost(estrada)
df$FullName[df$FullName %in% estrada] <- "Luis Estrada"; rm(estrada)
ehrhardt <- sort(grep("laurence ehrhardt|laurence w ehrnardt|laurence w ehrhardt|larry ehrhardt|^erhardt for|^ehrhardt for|larry ehrhardy", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(ehrhardt); xpcost(ehrhardt)
df$FullName[df$FullName %in% ehrhardt] <- "Laurence W Ehrhardt"; rm(ehrhardt)
lsstrat <- sort(grep("ls strat", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(lsstrat); xpcost(lsstrat)
df$FullName[df$FullName %in% lsstrat] <- "LS Strategies"; rm(lsstrat)
metlife <- sort(grep("metlife", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(metlife); xpcost(metlife)
df$FullName[df$FullName %in% metlife] <- "Metlife Inc"; rm(metlife)
mcmahon <- sort(grep("mcmahon squier", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mcmahon); xpcost(mcmahon)
df$FullName[df$FullName %in% mcmahon] <- "Mcmahon Squier and Assoc"; rm(mcmahon)
mass <- sort(grep("mass stratery", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mass); xpcost(mass)
df$FullName[df$FullName %in% mass] <- "Mass Stratery Group Inc"; rm(mass)
rucci <- sort(grep("matthew rucci", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(rucci); xpcost(rucci)
df$FullName[df$FullName %in% rucci] <- "Matthew Rucci"; rm(rucci)
narducci <- sort(grep("micheal narducci|michael narducci|mikchael narducci", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(narducci); xpcost(narducci)
df$FullName[df$FullName %in% narducci] <- "Michael Narducci"; rm(narducci)
narducci <- sort(grep("friends of narducci|nickolas narducci|councilman narducci|nicholas narducci|nicholas j narducci|nick narducci", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(narducci); xpcost(narducci)
df$FullName[df$FullName %in% narducci] <- "Nicholas Narducci"; rm(narducci)
gamba <- sort(grep("sharon gamba|sharon l gamba", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(gamba); xpcost(gamba)
df$FullName[df$FullName %in% gamba] <- "Sharon Gamba"; rm(gamba)
childs <- sort(grep("michael childs", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(childs); xpcost(childs)
df$FullName[df$FullName %in% childs] <- "Michael Childs"; rm(childs)
marketwise <- sort(grep("market wise", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(marketwise); xpcost(marketwise)
df$FullName[df$FullName %in% marketwise] <- "Market Wise Communications"; rm(marketwise)
malinda <- sort(grep("malinda howard|malinda m howard", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(malinda); xpcost(malinda)
df$FullName[df$FullName %in% malinda] <- "Malinda M Howard"; rm(malinda)
schweich <- sort(grep("matthew schweich|matthew h schweich", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(schweich); xpcost(schweich)
df$FullName[df$FullName %in% schweich] <- "Matthew Schweich"; rm(schweich)
giarusso <- sort(grep("anthony giarrusso|antonio giarrusso|^giarrusso comm|anthony giarrusso|anothony giarusso|antonio giarusso|anthony giarusso|^giarusso for|^giarusso$", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(giarusso); xpcost(giarusso)
df$FullName[df$FullName %in% giarusso] <- "Anthony Giarusso"; rm(giarusso)
netop <- sort(grep("netop", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(netop); xpcost(netop)
df$FullName[df$FullName %in% netop] <- "Netop Advisors"; rm(netop)
norway <- sort(grep("norway hill", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(norway); xpcost(norway)
df$FullName[df$FullName %in% norway] <- "Norway Hill Associates"; rm(norway)
taylor <- sort(grep("nathanial taylor|nathaniel p taylor", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(taylor); xpcost(taylor)
df$FullName[df$FullName %in% taylor] <- "Nathaniel P Taylor"; rm(taylor)
omnia <- sort(grep("omnia", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(omnia); xpcost(omnia)
df$FullName[df$FullName %in% omnia] <- "Omnia LLC"; rm(omnia)
opendoor <- sort(grep("opendoor|open door", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(opendoor); xpcost(opendoor)
df$FullName[df$FullName %in% opendoor] <- "Open Doors"; rm(opendoor)
olpcenter <- sort(grep("olp center", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(olpcenter); xpcost(olpcenter)
df$FullName[df$FullName %in% olpcenter] <- "OLP Center"; rm(olpcenter)
paula <- sort(grep("paula mcfar|paula macfar|paula mc far", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(paula); xpcost(paula)
df$FullName[df$FullName %in% paula] <- "Paula Mcfarland"; rm(paula)
paula <- sort(grep("paula d dinucci|paula dinucci", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(paula); xpcost(paula)
df$FullName[df$FullName %in% paula] <- "Paula D Dinucci"; rm(paula)
sweeney <- sort(grep("patrick sweeney|^sweeney for|patrick a sweeney", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sweeney); xpcost(sweeney)
df$FullName[df$FullName %in% sweeney] <- "Patrick Sweeney"; rm(sweeney)
dinucci <- sort(grep("dinucci", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(dinucci); xpcost(dinucci)
df$FullName[df$FullName %in% dinucci] <- "dinucci"; rm(dinucci)
tencher <- sort(grep("paul tencher|paul j tencher", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(tencher); xpcost(tencher)
df$FullName[df$FullName %in% tencher] <- "Paul J Tencher"; rm(tencher)
publiceye <- sort(grep("public eye", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(publiceye); xpcost(publiceye)
df$FullName[df$FullName %in% publiceye] <- "Public Eye Media"; rm(publiceye)
payroll <- sort(grep("payroll advantage", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(payroll); xpcost(payroll)
df$FullName[df$FullName %in% payroll] <- "Payroll Advantage"; rm(payroll)
quidnick <- sort(grep("quidneck greenhouse|guidneck greenhouse|quidnick greenwhouses|quidnick greenhouse", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(quidnick); xpcost(quidnick)
df$FullName[df$FullName %in% quidnick] <- "Quidnick Greenhouses Inc"; rm(quidnick)
rigop <- sort(grep("^rigop", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(rigop); xpcost(rigop)
df$FullName[df$FullName %in% rigop] <- "RI GOP"; rm(rigop)
redprint <- sort(grep("redprint|redpoint", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(redprint); xpcost(redprint)
df$FullName[df$FullName %in% redprint] <- "Redprint Strategy"; rm(redprint)
dileonardo <- sort(grep("robert dileonardo", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(dileonardo); xpcost(dileonardo)
df$FullName[df$FullName %in% dileonardo] <- "Robert Dileonardo"; rm(dileonardo)
russo <- sort(grep("robert v russo|robert russo", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(russo); xpcost(russo)
df$FullName[df$FullName %in% russo] <- "Robert V Russo"; rm(russo)
russolino <- sort(grep("russolino and young", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(russolino); xpcost(russolino)
df$FullName[df$FullName %in% russolino] <- "Russolino and Young"; rm(russolino)
healey <- sort(grep("robert j healey", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(healey); xpcost(healey)
df$FullName[df$FullName %in% healey] <- "Robert J Healey"; rm(healey)
resonance <- sort(grep("resonance cam", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(resonance); xpcost(resonance)
df$FullName[df$FullName %in% resonance] <- "Resonance Campaigns"; rm(resonance)
desiderato <- sort(grep("ronald desiderato", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(desiderato); xpcost(desiderato)
df$FullName[df$FullName %in% desiderato] <- "Ronald Desiderato"; rm(desiderato)
hryzan <- sort(grep("russell hryzan|russell c hryzan", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(hryzan); xpcost(hryzan)
df$FullName[df$FullName %in% hryzan] <- "Russell C Hryzan"; rm(hryzan)
stages <- sort(grep("stages of freedom", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(stages); xpcost(stages)
df$FullName[df$FullName %in% stages] <- "Stages of Freedom"; rm(stages)
deviney <- sort(grep("sean deviney", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(deviney); xpcost(deviney)
df$FullName[df$FullName %in% deviney] <- "Sean Deviney"; rm(deviney)
sams <- sort(grep("sams club|sams wholesale", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sams); xpcost(sams)
df$FullName[df$FullName %in% sams] <- "Sams Club"; rm(sams)
dorrance <- sort(grep("the dorrance", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(dorrance); xpcost(dorrance)
df$FullName[df$FullName %in% dorrance] <- "The Dorrance"; rm(dorrance)
squantum <- sort(grep("squantum assoc", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(squantum); xpcost(squantum)
df$FullName[df$FullName %in% squantum] <- "The Squantum Association"; rm(squantum)
threepoint <- sort(grep("three point", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(threepoint); xpcost(threepoint)
df$FullName[df$FullName %in% threepoint] <- "Three Point Media"; rm(threepoint)
clarendon <- sort(grep("clarendon", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(clarendon); xpcost(clarendon)
df$FullName[df$FullName %in% clarendon] <- "The Clarendon Group"; rm(clarendon)
spokenhub <- sort(grep("spoken hub", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(spokenhub); xpcost(spokenhub)
df$FullName[df$FullName %in% spokenhub] <- "The Spoken Hub"; rm(spokenhub)
mynett <- sort(grep("mynett group", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(mynett); xpcost(mynett)
df$FullName[df$FullName %in% mynett] <- "The Mynett Group"; rm(mynett)
tilt <- sort(grep("^tilt com", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(tilt); xpcost(tilt)
df$FullName[df$FullName %in% tilt] <- "Tilt Communications"; rm(tilt)
unite <- sort(grep("unite here", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(unite); xpcost(unite)
df$FullName[df$FullName %in% unite] <- "Unite Here"; rm(unite)
pichette <- sort(grep("victor pichette|vic pichette|victor j pichette|^v ic pichette", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(pichette); xpcost(pichette)
df$FullName[df$FullName %in% pichette] <- "Victor Pichette"; rm(pichette)
irons <- sort(grep("william irons", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(irons); xpcost(irons)
df$FullName[df$FullName %in% irons] <- "William Irons"; rm(irons)
wannamoisett <- sort(grep("wannamoisett", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(wannamoisett); xpcost(wannamoisett)
df$FullName[df$FullName %in% wannamoisett] <- "Wannamoisett Country Club"; rm(wannamoisett)
call <- sort(grep("woonsocket call", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(call); xpcost(call)
df$FullName[df$FullName %in% call] <- "Woonsocket Call"; rm(call)
sherman <- sort(grep("zachary w sherman|zachary sherman", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(sherman); xpcost(sherman)
df$FullName[df$FullName %in% sherman] <- "Zachary W Sherman"; rm(sherman)

# searchfor <- sort(grep("searchfor", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(searchfor); xpcost(searchfor)
# df$FullName[df$FullName %in% searchfor] <- "searchfor"; rm(searchfor)
# searchfor <- sort(grep("searchfor", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(searchfor); xpcost(searchfor)
# df$FullName[df$FullName %in% searchfor] <- "searchfor"; rm(searchfor)
# searchfor <- sort(grep("searchfor", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(searchfor); xpcost(searchfor)
# df$FullName[df$FullName %in% searchfor] <- "searchfor"; rm(searchfor)
# searchfor <- sort(grep("searchfor", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(searchfor); xpcost(searchfor)
# df$FullName[df$FullName %in% searchfor] <- "searchfor"; rm(searchfor)
# searchfor <- sort(grep("searchfor", df$FullName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpend(searchfor); xpcost(searchfor)
# df$FullName[df$FullName %in% searchfor] <- "searchfor"; rm(searchfor)

# grep("cruise", df$FullName, value = TRUE, ignore.case = TRUE) %>% unique()
# cruise <- df %>% filter(FullName %in% c("bay queen cruises","newport cruise company")); head(cruise)
# values <- sort(grep("mat", df$OrganizationName, value = TRUE, ignore.case = TRUE)) %>% unique() %>% print; xpOrg(values)
# df$OrganizationName[df$OrganizationName %in% values] <- "values"; rm(values)

# Save
saveRDS(df, paste(dir, "Exp_Clean.rds", sep = ""))
df <- readRDS(paste(dir, "Exp_Clean.rds", sep = ""))


df %>% 
     group_by(ExpDesc) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(desc(Total)) 

expMos <- df %>% 
     mutate(Mo = factor(strtrim(months(ExpDate), 3),
                        levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))) %>% 
     group_by(ExpDesc,Mo,CY) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     arrange(CY,Mo, desc(Total)); head(expMos, 10)


pltList <- list()
descs <- sort(unique(df$ExpDesc))


for(i in seq_along(descs)) {
     
     d <- expMos %>% filter(ExpDesc == descs[i]) %>% filter(Total > 0)
     
     p.x <- ggplot(d, aes(Mo, CY)) +
          geom_tile(color = "gray50", size = 0.1, aes(fill = Total)) +
          scale_fill_viridis(direction = -1, option = "B", label = dollar) +
          geom_label(size = 2.75, aes(label = monify(Total))) +
          theme_bw() +
          labs(x = "", y = "",
               title = paste("Expenditures for ", unique(d$ExpDesc), sep = "")) +
          scale_y_continuous(breaks = seq(2002,2018,1)) +
          theme(strip.text.x = element_text(margin = margin(0.05,0.05,0.05,0.05, "cm")),
                legend.position = "right",
                axis.text.x = element_text(size = 12),
                axis.text.y = element_text(size = 12),
                legend.background = element_rect(color = "gray50"))
     
     pltList[[i]] <- assign(paste0("p", i), p.x)
}

p1
p2
p12

pdf(paste(dir, "Exp_by_Month.pdf", sep = ""), onefile = TRUE, height = 7.5, width = 10.5)

for(i in 1:length(pltList)) {
     plot(pltList[[i]])
}

dev.off()


# 3691 rows
x1000 <- df %>% 
     #filter(ExpDesc == "Food, Beverages and Meals") %>% 
     #filter(ExpDate > "2017-01-01") %>% 
     group_by(FullName) %>% 
     summarise(Expenditures = n_distinct(ExpenditureID),
               Total = sum(Amount, na.rm = TRUE),
               avg = round(Total / Expenditures, 2),
               max = max(Amount, na.rm = TRUE),
               First = min(ExpDate, na.rm = TRUE),
               Most_Recent = max(ExpDate, na.rm = TRUE)) %>% 
     ungroup() %>%
     filter(Total > quantile(Total, probs = 0.9)) %>% 
     arrange(desc(Total)); head(x1000)


expDesc <- df %>% 
     group_by(ExpDesc,CY) %>% 
     summarise(Expenditures = n_distinct(ExpenditureID),
               Total = sum(Amount, na.rm = TRUE),
               avg = round(Total / Expenditures, 2),
               max = max(Amount, na.rm = TRUE),
               First = min(ExpDate, na.rm = TRUE),
               Most_Recent = max(ExpDate, na.rm = TRUE)) %>% 
     ungroup() %>%
     mutate(GrandTot = sum(Total)) %>% 
     mutate(Pct_of_Total = round(Total / GrandTot, 5)) %>% 
     mutate(Dollars = monify(Total),Avg = monify(avg), Grand_Total = monify(GrandTot), Max = monify(max),Pct = percent(Pct_of_Total)) %>% 
     select(ExpDesc,Expenditures,Dollars,Avg,Max,Grand_Total,Pct,First,Most_Recent,everything()) %>% 
     arrange(desc(Total)); head(expDesc, 10)

# Save
saveRDS(expDesc, paste(dir, "expenditure_desc.rds", sep = ""))



expComp <- df %>% transform(FullName = str_to_title(FullName)) %>% 
     transform(FullName = gsub("^Ri ", "RI ", FullName)) %>% 
     transform(FullName = gsub(" Pac$", " PAC", FullName)) %>% 
     group_by(ExpDesc,FullName,#City,State,
              CY) %>% 
     summarise(Expenditures = n_distinct(ExpenditureID),
               Total = sum(Amount, na.rm = TRUE),
               avg = round(Total / Expenditures, 2),
               max = max(Amount, na.rm = TRUE),
               First = min(ExpDate, na.rm = TRUE),
               Most_Recent = max(ExpDate, na.rm = TRUE)) %>% 
     ungroup() %>%
     group_by(ExpDesc,CY) %>% 
     mutate(GrandTot = sum(Total)) %>% 
     ungroup() %>% 
     group_by(ExpDesc) %>% 
     mutate(col = sample(colors()[!colors() %in% grep("^grey[1-2]|^gray[1-2|^grey[4-5]|^gray[4-5]|^grey[7-9]|^gray[7-9]", colors(), value = TRUE)], 1)) %>% 
     ungroup() %>% 
     mutate(Pct_of_Total = round(Total / GrandTot, 5)) %>% 
     mutate(Dollars = monify(Total),Avg = monify(avg), Grand_Total = monify(GrandTot), Max = monify(max),Pct = percent(Pct_of_Total)) %>% 
     select(-c(GrandTot,avg,max,Pct_of_Total)) %>% 
     select(FullName,#City,State,
            Expenditures,Dollars,Avg,Max,Grand_Total,Pct,First,Most_Recent,everything()) %>% 
     transform(ExpDesc = factor(ExpDesc)) %>% 
     arrange(desc(Total)); head(expComp, 10); str(expComp)

# Save
saveRDS(expComp, paste(dir, "company_expenditures.rds", sep = ""))



topComp <- df %>% 
     group_by(FullName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>%
     arrange(desc(Total)) %>% 
     filter(Total > 1999); tail(topComp, 10); str(topComp);n_distinct(topComp$FullName)

# Save
saveRDS(topComp, paste(dir, "top_company_expenditures.rds", sep = ""))

set.seed(87)
expOrg <- df %>% 
     group_by(ExpDesc,OrganizationName,CY) %>% 
     summarise(Expenditures = n_distinct(ExpenditureID),
               Total = sum(Amount, na.rm = TRUE),
               avg = round(Total / Expenditures, 2),
               max = max(Amount, na.rm = TRUE),
               First = min(ExpDate, na.rm = TRUE),
               Most_Recent = max(ExpDate, na.rm = TRUE)) %>% 
     ungroup() %>%
     group_by(ExpDesc,CY) %>% 
     mutate(GrandTot = sum(Total)) %>% 
     ungroup() %>% 
     group_by(ExpDesc) %>% 
     mutate(col = sample(colors()[!colors() %in% grep("^grey[1-2]|^gray[1-2|^grey[4-5]|^gray[4-5]|^grey[7-9]|^gray[7-9]", colors(), value = TRUE)], 1)) %>% 
     ungroup() %>% 
     mutate(Pct_of_Total = round(Total / GrandTot, 5)) %>% 
     mutate(Dollars = monify(Total),Avg = monify(avg), Grand_Total = monify(GrandTot), Max = monify(max),Pct = percent(Pct_of_Total)) %>% 
     select(-c(GrandTot,avg,max,Pct_of_Total)) %>% 
     select(OrganizationName,#City,State,
            Expenditures,Dollars,Avg,Max,Grand_Total,Pct,First,Most_Recent,everything()) %>% 
     transform(ExpDesc = factor(ExpDesc)) %>% 
     arrange(desc(Total)); head(expOrg, 10); str(expOrg)

# Save
saveRDS(expOrg, paste(dir, "org_expenditures.rds", sep = ""))

topOrg <- df %>% 
     group_by(OrganizationName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>%
     arrange(desc(Total)) %>% 
     filter(Total > 1999); tail(topOrg, 10); str(topOrg);n_distinct(topOrg$OrganizationName)

# Expenditures from Organization to Company
expOrgComp <- df %>% 
     group_by(ExpDesc,FullName,OrganizationName,CY) %>% 
     summarise(Expenditures = n_distinct(ExpenditureID),
               Total = sum(Amount, na.rm = TRUE),
               avg = round(Total / Expenditures, 2),
               max = max(Amount, na.rm = TRUE),
               First = min(ExpDate, na.rm = TRUE),
               Most_Recent = max(ExpDate, na.rm = TRUE)) %>% 
     ungroup() %>%
     group_by(ExpDesc,CY) %>% 
     mutate(GrandTot = sum(Total)) %>% 
     ungroup() %>% 
     # group_by(ExpDesc) %>% 
     # mutate(col = sample(colors()[!colors() %in% grep("^grey[1-2]|^gray[1-2|^grey[4-5]|^gray[4-5]|^grey[7-9]|^gray[7-9]", colors(), value = TRUE)], 1)) %>% 
     # ungroup() %>% 
     mutate(Pct_of_Total = round(Total / GrandTot, 5)) %>% 
     mutate(Dollars = monify(Total),Avg = monify(avg), Grand_Total = monify(GrandTot), Max = monify(max),Pct = percent(Pct_of_Total)) %>% 
     select(-c(GrandTot,avg,max,Pct_of_Total)) %>% 
     select(OrganizationName,
            FullName,#City,State,
            Expenditures,Dollars,Avg,Max,Grand_Total,Pct,First,Most_Recent,everything()) %>% 
     transform(ExpDesc = factor(ExpDesc)) %>% 
     arrange(desc(Total)); head(expOrgComp, 10); str(expOrgComp)

# Save
saveRDS(expOrgComp, paste(dir, "comp_org_expenditures.rds", sep = ""))


# Expenditure Network
expNet <- df %>% 
     group_by(ExpDesc,OrganizationName,FullName,CY) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup(); head(expNet)
     
# Save
saveRDS(expNet, paste(dir, "expNet.rds", sep = ""))


# Expenditure Descriptions
expDesc <- readRDS(paste(dir, "expenditure_desc.rds", sep = ""))


colVals <- sample(colors()[!colors() %in% grep("^grey[1-2]|^gray[1-2|^grey[4-5]|^gray[4-5]|^grey[7-9]|^gray[7-9]", colors(), value = TRUE)], 17); colVals

head(expOrgComp)
head(expOrg)
head(expComp)

e_x <- expDesc %>% 
     filter(CY >= 2014 & CY <= 2018) %>% 
     mutate(Range = paste("Jan 1, ", min(CY), " to Dec 31, ", max(CY), sep = "")) %>%
     group_by(ExpDesc,Range) %>% 
     summarise(Expenditures = sum(Expenditures),
               Total = sum(Total),
               avg = round(Total / Expenditures, 2),
               max = max(Total, na.rm = TRUE),
               First = min(First, na.rm = TRUE),
               Most_Recent = max(Most_Recent, na.rm = TRUE)) %>% 
     ungroup() %>%
     mutate(GrandTot = sum(Total)) %>% 
     mutate(Pct_Total = round(Total / GrandTot, 5)) %>% 
     mutate(Dollars = monify(Total),Avg = monify(avg), Grand_Total = monify(GrandTot), Max = monify(max),Pct_of_Total = percent(Pct_Total)) %>% 
     select(ExpDesc,Range,Expenditures,Dollars,Avg,Max,Grand_Total,Pct_of_Total,First,Most_Recent,everything()) %>% 
     arrange(desc(Total))


ggplot(e_x, aes(reorder(ExpDesc,Total), Total)) +
     geom_bar(stat = "identity", width = 0.6, color = "gray50", fill = "navyblue") +
     scale_y_continuous(label = dollar) +
     geom_text(size = 4, fontface = "bold", 
               aes(label = Dollars), hjust = -0.25) +
     labs(x = "Expenditure Description", y = "Total",
          title = "Expenditure Types",
          subtitle = paste(e_x$Range, sep = "")) +
     theme(axis.text = element_text(size = 12)) +
     coord_flip() +
     NULL
# Save


# Read in company expenditures
expComp <- readRDS(paste(dir, "company_expenditures.rds", sep = "")); head(expComp)


# Company Expenditures by Organizationn
e_comp <- expComp %>% 
     filter(CY >= 2014 & CY <= 2018) %>% 
     mutate(Range = paste("Jan 1, ", min(CY), " to Dec 31, ", max(CY), sep = "")) %>%
     #filter(ExpDesc == "Advertising") %>% 
     #filter(ExpDesc == "Consultant & Professional Services") %>% 
     group_by(ExpDesc,FullName,col,#City,State,
              Range) %>% 
     summarise(Expenditures = sum(Expenditures),
               Total = sum(Total),
               avg = round(Total / Expenditures, 2),
               max = max(Total, na.rm = TRUE),
               First = min(First, na.rm = TRUE),
               Most_Recent = max(Most_Recent, na.rm = TRUE)) %>% 
     ungroup() %>%
     group_by(ExpDesc) %>% 
     mutate(ExpType_GrandTot = sum(Total)) %>% ungroup() %>% 
     mutate(Pct_Total = round(Total / ExpType_GrandTot, 5)) %>% 
     mutate(Dollars = monify(Total),Avg = monify(avg), Grand_Total_by_Type = monify(ExpType_GrandTot), Max = monify(max),Pct_of_Total = percent(Pct_Total)) %>% 
     select(FullName,#City,State,
            ExpDesc,Expenditures,Dollars,Avg,Max,Grand_Total_by_Type,Pct_of_Total,First,Most_Recent,everything()) %>% 
     arrange(desc(Total)); e_comp


exp_comp <- e_comp %>% #filter(ExpDesc == "Advertising") %>% 
     arrange(desc(Total)) %>% 
     #head(input$expCo_slider) %>% 
     head(25) %>% droplevels()

str(exp_comp)
scale_fill_expend <- function(...){
     set.seed(83)
     ggplot2:::manual_scale('fill', 
                            values = sample(colors()[!colors() %in% grep("^grey[1-2]|^gray[1-2|^grey[4-5]|^gray[4-5]|^grey[7-9]|^gray[7-9]", 
                                                                         colors(), value = TRUE)], length(unique(expComp$ExpDesc))),
                            sort(unique(expDesc$ExpDesc)), 
                            ...)
}



ggplot(exp_comp,#filter(exp_comp,ExpDesc != "Advertising"),
       aes(reorder(FullName, Total), Total)) +
     geom_bar(stat = "identity", width = 0.6, color = "gray50", aes(fill = ExpDesc)) +
     scale_y_continuous(labels = dollar, limits = c(0, round(max(exp_comp$Total) * 1.2, 0))) +
     #scale_fill_discrete(limits = levels(expComp$ExpDesc)) +
     #     scale_fill_expend() +
     geom_text(size = 4, fontface = "bold", aes(label = Dollars), hjust = -0.25) +
     labs(x = "Company Name", y = "Total",
          title = "Companies Receiving Campaign Expenditures",
          subtitle = paste(exp_comp$Range, sep = "")) +
     theme(axis.text = element_text(size = 12)) +
     coord_flip() +
     NULL

ggplot(exp_comp,#filter(exp_comp,ExpDesc != "Advertising"),
       aes(reorder(FullName, Total), Total)) +
     geom_bar(stat = "identity", width = 0.6, color = "gray50", aes(fill = ExpDesc)) +
     scale_y_continuous(labels = dollar, limits = c(0, round(max(exp_comp$Total) * 1.2, 0))) +
     #scale_fill_discrete(limits = levels(expComp$ExpDesc)) +
     scale_fill_expend() +
     geom_text(size = 4, fontface = "bold", aes(label = Dollars), hjust = -0.25) +
     labs(x = "Company Name", y = "Total",
          title = "Companies Receiving Campaign Expenditures",
          subtitle = paste(exp_comp$Range, sep = "")) +
     theme(axis.text = element_text(size = 12)) +
     coord_flip() +
     NULL



# Read in company expenditures
expOrg <- readRDS(paste(dir, "org_expenditures.rds", sep = "")); head(expOrg)


# Company Expenditures by Organizationn
e_org <- expOrg %>% 
     filter(CY >= 2014 & CY <= 2018) %>% 
     mutate(Range = paste("Jan 1, ", min(CY), " to Dec 31, ", max(CY), sep = "")) %>%
     #filter(ExpDesc == "Advertising") %>% 
     #filter(ExpDesc == "Consultant & Professional Services") %>% 
     group_by(ExpDesc,OrganizationName,col,#City,State,
              Range) %>% 
     summarise(Expenditures = sum(Expenditures),
               Total = sum(Total),
               avg = round(Total / Expenditures, 2),
               max = max(Total, na.rm = TRUE),
               First = min(First, na.rm = TRUE),
               Most_Recent = max(Most_Recent, na.rm = TRUE)) %>% 
     ungroup() %>%
     group_by(OrganizationName) %>% 
     mutate(OrgTotal = sum(Total)) %>% ungroup() %>% 
     mutate(Pct_Total = round(Total / OrgTotal, 5)) %>% 
     mutate(Dollars = monify(Total),Avg = monify(avg), Org_Total = monify(OrgTotal),Max = monify(max),Pct_of_Total = percent(Pct_Total)) %>% 
     select(OrganizationName,#City,State,
            ExpDesc,Expenditures,Dollars,Avg,Max,Org_Total,Pct_of_Total,First,Most_Recent,everything()) %>% 
     arrange(desc(Total)); e_org


exp_comp <- e_comp %>% #filter(ExpDesc == "Advertising") %>% 
     arrange(desc(Total)) %>% 
     #head(input$expCo_slider) %>% 
     head(25) %>% droplevels()





compz <- df %>% filter(FullName != "") %>% 
     group_by(FullName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     #filter(Total > 249) %>% 
     rename(Llamo = FullName) %>% 
     mutate(Srce = "Company", Set = "Expenditures"); head(compz)

orgz <- dfx %>% 
     group_by(OrganizationName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     #filter(Total > 249) %>% 
     rename(Llamo = OrganizationName) %>% 
     mutate(Srce = "Org", Set = "Expenditures"); head(orgz)

entz <- bind_rows(donorz, orgz); rm(donorz,orgz)

# Save
saveRDS(entz, paste(dir, "contribution_entities.rds", sep = ""))





# ********************************

x <- df %>% filter(OrganizationName == "Nicholas Anthony Mattiello") %>% 
     filter(ExpDesc == "Food, Beverages and Meals") %>% 
     group_by(OrganizationName,FullName,City,State,ExpDesc) %>% 
     summarise(Expenditures = n_distinct(ExpenditureID),
               Total = sum(Amount, na.rm = TRUE),
               Avg = round(Total / Expenditures, 2),
               First = min(ExpDate, na.rm = TRUE),
               Latest = max(ExpDate, na.rm = TRUE)) %>% 
     ungroup() %>% mutate(GrandTot = sum(Total)) %>% 
     mutate(Pct_of_Total = round(Total / GrandTot, 5)) %>% 
     arrange(desc(Total)); head(x, 10)

dining <- df %>% filter(OrganizationName == "Nicholas Anthony Mattiello") %>% 
     filter(ExpDesc == "Food, Beverages and Meals") %>% 
     group_by(FullName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% arrange(desc(Total)); head(dining)

atwells <- grep("atwel", df$Address, value = TRUE, ignore.case = TRUE) %>% unique() %>% print

atwells_establish <- df %>% filter(Address %in% atwells) %>% select(FullName,Address) %>% distinct(); head(atwells_establish)


lq <- grep("liquor|wine|spirit|beer", eats$FullName, ignore.case = TRUE, value = TRUE) %>% unique() %>% print
cig <- grep("cigar|shellys back room|la habana hemingway|havana", eats$FullName, ignore.case = TRUE, value = TRUE) %>% unique() %>% print
#grill <- grep("grill", eats$FullName, ignore.case = TRUE, value = TRUE) %>% unique() %>% print
#pizza <- grep("pizza", eats$FullName, ignore.case = TRUE, value = TRUE) %>% unique() %>% print
bake <- grep("creamery|cafe$|bake|dunkin|coffee|ihop|honey dew|panera bread", eats$FullName, ignore.case = TRUE, value = TRUE) %>% unique() %>% print
#cater <- grep("cater|black sheep barbecue", eats$FullName, ignore.case = TRUE, value = TRUE) %>% unique() %>% print
pub <- grep("providence g club|snookers|red fez|the eddy$| tap$|red stripe|bar$|^bar |pub|tavern|brewery|rooftop at the g|lounge", eats$FullName, ignore.case = TRUE, value = TRUE) %>% unique() %>% print
events <- grep("cater|black sheep barbecue|marriot|sports services|john manni|lori cat|alan rudolph|phoenix house|ricaodd|rolfe street station|teachers union|democratic|fidelity|ricaodd|fogarty center|^brown|diversity|chamber|shriners|trial lawyer|twin river|newspaper|hall of fame", eats$FullName, ignore.case = TRUE, value = TRUE) %>% unique() %>% print
supplies <- grep("targetcom|^shaws|stop and shop|bjs|daves market|walmart|food store", eats$FullName, ignore.case = TRUE, value = TRUE) %>% unique() %>% print
rec <- grep("yacht|country|golf", eats$FullName, ignore.case = TRUE, value = TRUE) %>% unique() %>% print
rest <- grep("Pizza|grill|tgi fridays|ristorante|restaurant|ale house|formello|jackies|resaurant", eats$FullName, ignore.case = TRUE, value = TRUE) %>% unique() %>% print


dining$Cat <- case_when(dining$FullName %in% lq ~ "Wine and Spirits",
                        dining$FullName %in% cig ~ "Cigar Lounge",
                        #dining$FullName %in% pizza ~ "",
                        dining$FullName %in% pub ~ "Drinks",
                        dining$FullName %in% bake ~ "Coffee / Bakery / Dessert",
                        dining$FullName %in% events ~ "Events / Catering",
                        dining$FullName %in% supplies ~ "Food Supplies",
                        dining$FullName %in% rec ~ "Recreation",
                        dining$FullName %in% atwells_establish$FullName ~ "Federal Hill",
#                        dining$FullName %in% cater ~ "Catering",
                        dining$FullName %in% rest ~ "Meals",
                        TRUE ~ "Meals"); table(dining$Cat)


eats <- df %>% filter(OrganizationName == "Nicholas Anthony Mattiello") %>% 
     filter(ExpDesc == "Food, Beverages and Meals") %>% 
     group_by(ExpDate,OrganizationName,FullName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% 
     left_join(select(dining, FullName, Cat), by = "FullName") %>% 
     mutate(ExpMo = format(ExpDate, "%Y %m")) %>% 
     mutate(Wkdy = weekdays(ExpDate)) %>% 
     transform(Wkdy = factor(Wkdy, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))) %>% 
     arrange(ExpDate); head(eats)


eats %>% 
     group_by(ExpMo,Cat) %>% 
     summarise(Total = sum(Total)) %>% 
     ungroup() %>% 
     ggplot(aes(ExpMo, Total)) +
     geom_line(stat = "identity", aes(group = Cat, color = Cat)) +
     theme_bw() +
     theme(legend.position = "bottom",
           axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.25)) +
     facet_wrap(~Cat, scales = "free")

eats %>% 
     group_by(ExpMo,Cat) %>% 
     summarise(Total = sum(Total)) %>% 
     ungroup() %>% 
     ggplot(aes(ExpMo, Total)) +
     geom_bar(stat = "identity", position = "stack", width = 0.5, size = 0.1, color = "gray50", aes(fill = Cat))
     
     
ggplot(eats, aes(ExpDate, Total)) +
     geom_point(size = 1, aes(color = Wkdy)) +
     theme_bw() +
     theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.25)) +
     facet_wrap(~Cat, scales = "free")


meals <- df %>% filter(ExpDesc == "Food, Beverages and Meals") %>% 
     group_by(#OrganizationName,
          FullName,City,State,ExpDesc) %>% 
     summarise(Expenditures = n_distinct(ExpenditureID),
               Total = sum(Amount, na.rm = TRUE),
               Avg = round(Total / Expenditures, 2),
               Max = max(Amount, na.rm = TRUE),
               First = min(ExpDate, na.rm = TRUE),
               Latest = max(ExpDate, na.rm = TRUE)) %>% 
     ungroup() %>% mutate(GrandTot = sum(Total)) %>% 
     mutate(Pct_of_Total = round(Total / GrandTot, 5)) %>% 
     arrange(desc(Total)); head(meals, 10)


org <- sort(grep("nicholas|matiel|mattiel|nick", df$OrganizationName, value = TRUE, ignore.case = TRUE) %>% unique()) %>% print




consults <- df %>% filter(ExpDesc == "Advertising") %>% 
     group_by(FullName,City,State) %>% 
     summarise(Expenditures = n_distinct(ExpenditureID),
               Total = sum(Amount, na.rm = TRUE),
               Avg = round(Total / Expenditures, 2),
               Max = max(Amount, na.rm = TRUE),
               First = min(ExpDate, na.rm = TRUE),
               Latest = max(ExpDate, na.rm = TRUE)) %>% 
     ungroup() %>% mutate(GrandTot = sum(Total)) %>% 
     mutate(Pct_of_Total = round(Total / GrandTot, 5)) %>% 
     mutate(Dollars = monify(Total),avg = monify(Avg), Sum = monify(GrandTot), Top = monify(Max),pct = percent(Pct_of_Total)) %>% 
     select(-c(GrandTot,Avg,Max,Pct_of_Total)) %>% select(FullName,City,State,Expenditures,Dollars,avg,pct,Sum,Top,First,Latest,everything()) %>% 
     arrange(desc(Total)); head(adverts, 10)

exp_desc <- df %>% 
     group_by(ExpDesc) %>% 
     summarise(Expenditures = n_distinct(ExpenditureID),
               Total = sum(Amount, na.rm = TRUE),
               Avg = round(Total / Expenditures, 2)) %>% 
     ungroup() %>% mutate(Dollars = monify(Total),avg = monify(Avg)) %>% 
     arrange(desc(Total)); head(exp_desc, 10)


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

gov <- df %>%
     filter(ExpDate >= "2017-01-01") %>% 
     filter(OrganizationName %in% c("Allan W Fung",
                                    "Gina M. Raimondo"
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

gina <- df %>% 
     filter(OrganizationName == "Gina M. Raimondo") %>% filter(ExpDesc == "Food, Beverages and Meals") 

df %>% filter(ExpDesc == "Food, Beverages and Meals") %>% 
     group_by(FullName,City,State) %>% 
     summarise(Expenditures = n_distinct(ExpenditureID),
               Total = sum(Amount, na.rm = TRUE),
               Avg = round(Total / Expenditures, 2),
               Max = max(Amount, na.rm = TRUE),
               First = min(ExpDate, na.rm = TRUE),
               Latest = max(ExpDate, na.rm = TRUE)) %>% 
     ungroup() %>% mutate(GrandTot = sum(Total)) %>% 
     mutate(Pct_of_Total = round(Total / GrandTot, 5)) %>% 
     mutate(Dollars = monify(Total),avg = monify(Avg), Sum = monify(GrandTot), Top = monify(Max),pct = percent(Pct_of_Total)) %>% 
     select(-c(GrandTot,Avg,Max,Pct_of_Total)) %>% select(FullName,City,State,Expenditures,Dollars,avg,pct,Sum,Top,First,Latest,everything()) %>% 
     arrange(desc(Total)); head(adverts, 10)



df %>% filter(OrganizationName == "Gina M. Raimondo") %>% 
     filter(ExpDesc == "Food, Beverages and Meals") %>% #filter(ExpDate >= "2018-01-01") %>% 
     #mutate(DisbMo = format(ExpDate, "%Y-%m")) %>% 
     group_by(FullName) %>% 
     summarise(Total = sum(Amount, na.rm = TRUE)) %>% 
     ungroup() %>% arrange(desc(Total)) %>% head(40) %>% # View("x")
     ggplot(aes(reorder(FullName, Total), Total)) +
     geom_bar(stat = "identity", width = 0.7, color = "navyblue", fill = "yellow") + 
     #geom_line(stat = "identity", aes(group = DisbDesc, color = DisbDesc)) +
     theme(axis.text = element_text(size = 12)) +
     scale_y_continuous(label = dollar, breaks = seq(0,200000,2000)) +
     coord_flip() +
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

