# Inversions
Replication data for Are Presidential Inversions Inevitable? Comparing Eight Counterfactual Rules for Electing the U.S. President
```
##########################################################################################################
# ••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# ••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# ••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# ──╔╦═══╦═╗─╔╦═══╦════╦╗─╔╦═══╦═╗─╔╗
# ──║║╔═╗║║╚╗║║╔═╗║╔╗╔╗║║─║║╔═╗║║╚╗║║
# ──║║║─║║╔╗╚╝║║─║╠╝║║╚╣╚═╝║║─║║╔╗╚╝║
# ╔╗║║║─║║║╚╗║║╚═╝║─║║─║╔═╗║╚═╝║║╚╗║║
# ║╚╝║╚═╝║║─║║║╔═╗║─║║─║║─║║╔═╗║║─║║║
# ╚══╩═══╩╝─╚═╩╝─╚╝─╚╝─╚╝─╚╩╝─╚╩╝─╚═╝
#         ╔═══╦═══╦═══╦╗──╔╦═══╦═══╗
#         ║╔═╗║╔══╣╔═╗║╚╗╔╝║╔═╗║╔═╗║
#         ║║─╚╣╚══╣╚═╝╠╗║║╔╣║─║║╚══╗
#         ║║─╔╣╔══╣╔╗╔╝║╚╝║║╚═╝╠══╗║
#         ║╚═╝║╚══╣║║╚╗╚╗╔╝║╔═╗║╚═╝║
#         ╚═══╩═══╩╝╚═╝─╚╝─╚╝─╚╩═══╝
### Code to Replicate "Tools for Identifying Partisan Gerrymandering"
### Jonathan R. Cervas, University of California Irvine
### Bernard N. Grofman, University of California Irvine
### Note: 
# ••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# ••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# ••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
##########################################################################################################

    rm(list=ls(all=TRUE))   # Remove all objects just to be safe.
    options(scipen=999)     # Turn off Scientific Notation

    doInstall <- F          # Change to FALSE if you don't want packages installed.
        toInstall <- c("maptools", "rgdal", "ggplot2", "spatstat", "RColorBrewer", "maps", "stargazer", "astro")
        if(doInstall){install.packages(toInstall, repos = "http=//cran.r-project.org")}

    lapply(toInstall, library, character.only = TRUE)
    library(JudgeIt)
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
    setwd("/Users/cervas/Google Drive/School/UCI")  # Main directory
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
  projection <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0" #proj4string(counties)
# =================================================================
# -- FUNCTIONS -- -- FUNCTIONS -- -- FUNCTIONS -- -- FUNCTIONS  -- 
# =================================================================
    source("Papers/Gerrymandering/GERRYfunctions.R")
    source("R Functions/seatsvotes.R")
    source("Papers/(2019) Tools for Identifying Partisan Gerrymandering (PA)/PA_Congressional_Data.R")
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
    plan_names <- 
    c(
      "2011 Enacted",
      "2018 Remedial",
      "Join Legislative",
      "Gov. Wolf",
      "DailyKos",
      "Author's V1",
      "Author's V2",
      "Author's V3"
    )
# =================================================================
# -- DATA -- -- DATA -- -- DATA -- -- DATA  -- -- DATA  -- -- DATA 
# =================================================================


# 50/50 Hypothetical (uniform shift)
    shift(votes.2008.default, w=house.2008.turnout, c=0.5)
      shift(votes.2010.default, w=house.2010.turnout, c=0.5)
        shift(votes.2012.default, w=house.2012.turnout, c=0.5)
          shift(votes.2014.default, w=house.2014.turnout, c=0.5)
            shift(votes.2016.default, w=house.2016.turnout, c=0.5)
              shift(votes.2018.default, w=house.2018.turnout, c=0.5)


  # 2018 General Election Results
  # Precinct File  
    PA_2018 <- read.table("/Users/cervas/Google Drive/Data/Elections/PA 2018 election results/ERStat_2018_G(151980)_20181220.txt", sep=",")
      colnames(PA_2018) <- c("year", "type", "county", "precinct", "office", "district", "party", "ballot_pos", "office_code", "party_code", "cand_num", "first_name", "last_name", "middle_name", "suffix", "votes", "yes_votes", "no_votes", "USHouse_district", "ST_Senate_distric", "ST_House_district", "munic_type", "munic_name", "mun_code1", "mun_name1", "mun_code2", "mun_name2", "bicounty_code", "mcd_code", "fips", "vtd", "ballotquest", "record_type", "previous_precinct", "prevous_USHouse", "previous_STSenate", "previous_STHouse")
    
    PA_2018_HOUSE <- PA_2018[PA_2018$office_code %in% c("USC"),]
    PA_2018_HOUSE <- PA_2018_HOUSE[PA_2018_HOUSE$party_code %in% c("DEM", "REP"),]
    PA2018dem <- PA_2018_HOUSE[PA_2018_HOUSE$party_code %in% c("DEM"),]
    PA2018rep <- PA_2018_HOUSE[PA_2018_HOUSE$party_code %in% c("REP"),]
    PA2018 <- merge(PA2018dem, PA2018rep, by = "precinct", all.x = T, all.y = T)
    PA2018$code <- paste0(PA2018$precinct, PA2018$county.x, PA2018$district.x)
    PA2018 <- PA2018[!duplicated(PA2018$code),]
      two_party(sum(PA2018$votes.x, na.rm = T), sum(PA2018$votes.y, na.rm = T))

# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••


#### Presidential returns at CD level, 1956-2016
pres <- read.csv("/Users/cervas/Google Drive/Data/Elections/Presidential/FINAL Pres by CD.csv")
	colnames(pres)[colnames(pres)=="dem"] <- "PresDEM"
	colnames(pres)[colnames(pres)=="rep"] <- "PresREP"
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••

#### Precinct Level Election Returns
precincts <- read.csv("Papers/(2019) Tools for Identifying Partisan Gerrymandering (PA)/PA Redistricting/Data/Precinct_Partisanship.csv")
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••

#### State-Wide Aggregate Election Returns
# 2016 Presidential
DEM2016_pres <- sum(precincts$T16PRESD)
REP2016_pres <- sum(precincts$T16PRESR)
  two_party(DEM2016_pres, REP2016_pres)
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••

# 2016 Composite
DEM2016_COMPOS <- sum(precincts$T16PRESD + precincts$T16SEND + precincts$T16ATGD + precincts$T16AUDD + precincts$T16TREASD)
REP2016_COMPOS <- sum(precincts$T16PRESR + precincts$T16SEND + precincts$T16ATGD + precincts$T16AUDD + precincts$T16TREASD)
  two_party(DEM2016_COMPOS, REP2016_COMPOS)
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
#### Congressional Level Election returns
# house2014 <- read.csv("/Users/cervas/Google Drive/Data/Elections/House/house_jacobson.csv")
# house_2014 <- house_2014[house_2014$year=="2014",]
house2016 <- read.csv("Papers/(2019) Tools for Identifying Partisan Gerrymandering (PA)/PA Redistricting/Data/house2016inc.csv")
  house_2016 <- house2016[house2016$state=="Pennsylvania",]
  house_2016dem <- replaceNA(house_2016$dem)
  house_2016rep <- replaceNA(house_2016$rep)
    two_party(house_2016dem, house_2016rep)
    mean.w( default.unc(two_party(house_2016dem, house_2016rep)), w = (house_2016dem+house_2016rep)) # IMPUTED 25/75
    mean.w( two_party(house_2016dem, house_2016rep), w = (house_2016dem + house_2016rep)) # RAW
    # summary(lm(two_party(house_2016dem, house_2016rep) ~ 1, weight = (house_2016dem+house_2016rep))) # REGRESSION WEIGHTED MEAN
pa2016 <- c(0.822,0.902,0,0.339,0.328,0.428,0.405,0.456,0.367,0.298,0.363,0.382,1,0.744,0.394,0.444,0.538,0)
total2016 <- c(299010,357645,244893,334000,307843,362469,379502,380818,294565,301105,313221,359204,239316,343292,314747,303255,293164,293684)
  mean.w(two_party(house_2016dem, house_2016rep), w = (house_2016dem + house_2016rep))

find.winner(two_party(house_2016dem, house_2016rep))
declination(default.unc(two_party(house_2016dem, house_2016rep)))
seatsvotes(default.unc(two_party(house_2016dem, house_2016rep)))
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# =================================================================
# -- SHAPEFILES -- -- SHAPEFILES -- -- SHAPEFILES -- -- SHAPEFILES 
# =================================================================
#### STATE SHAPEFLES readOGR("Data/Shapefiles/Places", "PopulatedPlaces", GDAL1_integer64=TRUE), CRS(projection))
usa_shp <- spTransform(readOGR("/Users/cervas/Google Drive/GIS/Shapefiles/States/tl_2017_us_state.shp"), CRS(projection))
  pa_shp <- usa_shp[usa_shp@data$NAME == "Pennsylvania",]
#### County Shapefiles
pa_counties <- spTransform(readOGR("Papers/(2019) Tools for Identifying Partisan Gerrymandering (PA)/PA Redistricting/Shapefiles/PA Counties/PA_Counties.shp"), CRS(projection))
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
#### PA Cities
pa_places <- spTransform(readOGR("Papers/(2019) Tools for Identifying Partisan Gerrymandering (PA)/PA Redistricting/Shapefiles/Places/tl_2010_42_place10.shp"), CRS(projection))
	pa_major <- pa_places[pa_places@data$NAME10 %in% c("Pittsburgh", "Philadelphia", "Erie", "Scranton", "State College "),]
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
plot(pa_shp)
points(coordinates(pa_major), cex=0.55, pch=16)
# =================================================================
# -- PLANS 2016 DATA -- -- PLANS 2016 DATA -- -- PLANS 2016 DATA --
# =================================================================
directory <- "Papers/(2019) Tools for Identifying Partisan Gerrymandering (PA)/PA Redistricting/Data/2016 Plan Comparisons"
tp <- numeric(0)
files16 <- list.files(path = directory)
district_votes <- numeric(0)
diff_partisanship <- numeric(0)
plans_2016 <- list()

for (i in 1:length(files16)) 
  {
    dt <- read.csv(paste0(directory, "/", files16[i]))

    dt$Dsum <- (dt$DEMpres + dt$T16SEND + dt$T16ATGD + dt$T16AUDD + dt$T16TREASD)
    dt$Rsum <- (dt$REPpres + dt$T16SENR + dt$T16ATGR + dt$T16AUDR + dt$T16TREASR)
    dt$sumTotal <- dt$Dsum+dt$Rsum
    dt$DsumTP <- two_party(dt$Dsum, dt$Rsum)

    dt$D_ave_TP <- colMeans(rbind(two_party(dt$DEMpres,dt$REPpres), two_party(dt$T16SEND,dt$T16SENR), two_party(dt$T16ATGD,dt$T16ATGR), two_party(dt$T16AUDD,dt$T16AUDR), two_party(dt$T16TREASD,dt$T16TREASR)))

    dt <- dt[order(-dt$DsumTP),]
    plans_2016[[files16[i]]] <- dt
    print(files16[i])
    cat("Composite District Partisanship")
    print(paste0(18-sum(find.winner(dt$DsumTP)), "R-", sum(find.winner(dt$DsumTP)), "D"))
    cat("2016 Presidential Districts")
    print(paste0(sum(find.winner(dt$REPpres/(dt$DEMpres+dt$REPpres))), "R-", sum(find.winner(dt$DEMpres/(dt$DEMpres+dt$REPpres))), "D"))
    cat("Average District Partisanship")
    print(paste0(18-sum(find.winner(dt$D_ave_TP)), "R-", sum(find.winner(dt$D_ave_TP)), "D"))
    # print(dt)

    tp <- rbind(tp,dt$DsumTP)
    district_votes <- cbind(district_votes,dt$DsumTP)

    diff_partisanship <- cbind(diff_partisanship , dt$DsumTP[1:9] -(1-dt$DsumTP[18:10]))
  }

colnames(district_votes) <- files16
# colnames(diff_partisanship) <- files16

# - HOW DOES THE 2016 COMPOSITE COMPARE TO THE THE ACTUAL ELECTION RESULTS?
composite2016 <- read.csv(paste0(directory, "/1_111thOldMap.csv"))
composite2016 <- composite2016[order(composite2016$District),]
composite2016$house.dem.2016 <- house.dem.2016
composite2016$house.rep.2016 <- house.rep.2016
composite2016$TP_house.2016 <- two_party(house.dem.2016,house.rep.2016)
    composite2016$Dsum <- (composite2016$DEMpres + composite2016$T16SEND + composite2016$T16ATGD + composite2016$T16AUDD + composite2016$T16TREASD)
    composite2016$Rsum <- (composite2016$REPpres + composite2016$T16SENR + composite2016$T16ATGR + composite2016$T16AUDR + composite2016$T16TREASR)
    composite2016$TP_composite.2016 <- two_party(composite2016$Dsum, composite2016$Rsum)
    composite2016$dif <- default.unc(composite2016$TP_house.2016) - composite2016$TP_composite.2016
    composite2016$dif.pres <- default.unc(composite2016$TP_house.2016) - two_party(composite2016$DEMpres, composite2016$REPpres) 

# w <- impute.turnouts(house.dem.2016, house.rep.2016, w = 2)

# - CORRELATION BETWEEN COMPOSITE MEASURE AND ACTUAL RESULTS
cor(composite2016$TP_house.2016,
composite2016$TP_composite.2016)

cor(default.unc(composite2016$TP_house.2016),
default.unc(composite2016$TP_composite.2016))

mean(default.unc(composite2016$TP_house.2016, 0.25, 0.75)-
default.unc(composite2016$TP_composite.2016, 0.25, 0.75), na.rm=T)

mean(delete.unc(composite2016$TP_house.2016, 0.25, 0.75)-
delete.unc(composite2016$TP_composite.2016, 0.25, 0.75), na.rm=T)


shift(composite2016$TP_composite.2016, c = 0.5266)

c <- composite2016
pres16 <- cbind.data.frame(VOTE = two_party(c$DEMpres, c$REPpres), TURNOUT = c$DEMpres + c$REPpres)
ussen16 <- cbind.data.frame(VOTE = two_party(c$T16SEND, c$T16SENR), TURNOUT = c$T16SEND + c$T16SENR)
atg16 <- cbind.data.frame(VOTE = two_party(c$T16ATGD, c$T16ATGR), TURNOUT = c$T16ATGD + c$T16ATGR)
aud16 <- cbind.data.frame(VOTE = two_party(c$T16AUDD, c$T16AUDR), TURNOUT = c$T16AUDD + c$T16AUDR)
trea16 <- cbind.data.frame(VOTE = two_party(c$T16TREASD, c$T16TREASR), TURNOUT = c$T16TREASD + c$T16TREASR)
comp2016 <-  cbind.data.frame(VOTE = c$TP_composite.2016, TURNOUT = c$Dsum + c$Rsum)
cong2016 <- cbind.data.frame(VOTE = c$TP_house.2016, TURNOUT = c$Dsum + c$Rsum)
races <- c("cong2016", "pres16", "ussen16", "atg16", "aud16", "trea16", "comp2016")


# Average vote by district
means_races <- numeric()
  base.form <- formula(. ~ 1)
  cong.form <- formula(default.unc(cong2016$VOTE) ~ 1)
for (k in races)
{
  r.tmp <- get(k)
    reg_races <- summary(lm(update(base.form, as.formula(default.unc(r.tmp$VOTE) ~ .))))
    means_races[k] <- coef(reg_races)[1]
}
means_races

# Regression of Race on Congressional Results
    reg_cong_pres <- lm(update(cong.form, as.formula(. ~ default.unc(pres16$VOTE))))
    reg_cong_ussen <- lm(update(cong.form, as.formula(. ~ default.unc(ussen16$VOTE))))
    reg_cong_atg <- lm(update(cong.form, as.formula(. ~ default.unc(atg16$VOTE))))
    reg_cong_aud <- lm(update(cong.form, as.formula(. ~ default.unc(aud16$VOTE))))
    reg_cong_trea <- lm(update(cong.form, as.formula(. ~ default.unc(trea16$VOTE))))
    reg_cong_comp <- lm(update(cong.form, as.formula(. ~ default.unc(comp2016$VOTE))))

    reg_cong_all <- lm(update(cong.form, 
      as.formula(. ~ 
        default.unc(pres16$VOTE) + 
        default.unc(ussen16$VOTE) +
        default.unc(atg16$VOTE) +
        default.unc(aud16$VOTE) +
        default.unc(trea16$VOTE))
    ))

tab_congress <- stargazer(reg_cong_pres, 
          reg_cong_ussen, 
          reg_cong_atg, 
          reg_cong_aud, 
          reg_cong_trea, 
          reg_cong_comp,
          reg_cong_all,
  star.cutoffs = c(0.05,0.01, 0.001),
  style = "apsr", 
  header = FALSE,
  model.numbers = FALSE,
  initial.zero = TRUE,
  digits = 2,
  column.sep.width = "0pt",
  multicolumn = TRUE,
  omit.stat = c("rsq","ll", "F", "ser"),
  label = "tab:reg_congress",
  title = "Comparing Pennsylvania Congressional Results with State-wide Elections (2016)",
  dep.var.labels = "Actual Congressional Results (2016)",
  covariate.labels = c("Presidential", "US Senate", "PA Attorneys General", "PA Auditor", "PA Treasurer", "Composite"),
  notes = "REPLACE WITH NOTES" 
    )
tab_congress <- tab_congress[c(-6, -24, -26, -27)]
tab_congress <- append(tab_congress, "\\tabnotes{$^{*}$p $<$ 0.05; $^{**}$p $<$ .01; $^{***}$p $<$ 0.001 \\\\ Uncontested (or non-competitive) elections replaced with 0.25 \\& 0.75 vote shares. Regressions are unweighted, ie, all districts are assumed to have identical turnout. This is the usual way political scientist measure aggregate congressional vote \\cite{GelmanKing1994_unifiedAJPS}.} \\\\", 
  after = 24)
cat(paste(tab_congress, collapse = "\n"), "\n")


# =================================================================
# -- PLANS 2008 DATA -- -- PLANS 2008 DATA -- -- PLANS 2008 DATA --
# =================================================================
directory <- "/Users/cervas/Google Drive/School/UCI/Papers/(2019) Tools for Identifying Partisan Gerrymandering (PA)/PA Redistricting/Data/2008 Plan Comparisons"
tp <- numeric(0)
files08 <- list.files(path = directory)
district_votes_2008 <- numeric()
plans_2008 <- list()
  
  for (i in 1:length(files08)) 
    {
      dt <- read.csv(paste0(directory, "/", files08[i]))
      dt$Dsum <- (dt$DEMpres2008 + dt$DEMatt2008 + dt$DEMaud2008 + dt$DEMtre2008)
      dt$Rsum <- (dt$REPpres2008 + dt$REPatt2008 + dt$REPaud2008 + dt$REPtre2008)
      dt$sumTotal <- dt$Dsum+dt$Rsum
      dt$DsumTP <- dt$Dsum/dt$sumTotal
      dt$D_ave_TP <- colMeans(rbind(two_party(dt$DEMpres2008,dt$REPpres2008), two_party(dt$DEMatt2008,dt$REPatt2008), two_party(dt$DEMaud2008,dt$REPaud2008), two_party(dt$DEMtre2008,dt$REPtre2008)))


      dt <- dt[order(-dt$DsumTP),]
      plans_2008[[files08[i]]] <- dt

      print(files08[i])
      cat("2008 Composite District Partisanship")
      print(paste0(18-sum(find.winner(dt$DsumTP)), "R-", sum(find.winner(dt$DsumTP)), "D"))
      cat("2008 Presidential Districts")
      print(paste0(sum(find.winner(dt$REPpres2008/(dt$DEMpres2008+dt$REPpres2008))), "R-", sum(find.winner(dt$DEMpres2008/(dt$DEMpres2008+dt$REPpres2008))), "D"))
      cat("2000-2010 Decade Average")
      print(paste0(sum(find.winner(dt$REPdecadeAVE/(dt$DEMdecadeAVE+dt$REPdecadeAVE))), "R-", sum(find.winner(dt$DEMdecadeAVE/(dt$DEMdecadeAVE+dt$REPdecadeAVE))),"D"))
      cat("2008 Average District Partisanship")
      print(paste0(18-sum(find.winner(dt$D_ave_TP)), "R-", sum(find.winner(dt$D_ave_TP)), "D"))
      # print(dt)

      tp <- rbind(tp,dt$DsumTP)
      district_votes_2008 <- cbind(district_votes_2008,dt$DsumTP)
    }

colnames(district_votes_2008) <- files08








# =================================================================
# -- PLOTS -- -- PLOTS -- -- -- PLOTS -- -- -- PLOTS -- -- -- PLOTS 
# =================================================================
# Bar Graph
# Strong Dem, Lean Dem, Lean Rep, Strong Rep 2016 Average Statewide Elections

# Old <-  t(percent(matrix(c(4, 1, 5, 8), nrow=1)))
# Remedial <- t(percent(matrix(c(5, 4, 2, 7), nrow=1)))
# Cervas1 <- t(percent(matrix(c(5,3,3,7), nrow=1)))
# Cervas2 <- t(percent(matrix(c(5,2,3,8), nrow=1)))
# Cervas3 <- t(percent(matrix(c(5,4,2,7), nrow=1)))
# DailyKos <- t(percent(matrix(c(5,3,2,8), nrow=1)))
# Wolf <- t(percent(matrix(c(5,2,3,8), nrow=1)))
# Joint <- t(percent(matrix(c(4, 3, 3, 8), nrow=1)))


# colorbarplot <- c("gray10", "gray20", "gray20", "gray10")

# pdf(paste0("Papers/Grofman Cervas Gerrymandering 2018/Figures/Categoricalbarplot.pdf"), width=6, height=8)
# par(mfrow=c(10,1),
	# mar=c(1,.5,1.5,1.5))
# barplot(Old , col= colorbarplot, border="white", axes=F, horiz=T)
# mtext(side=3, las=1, line=0, text="111th Congress", cex=1)

# barplot(Remedial , col= colorbarplot, border="white", axes=F, horiz=T)
# mtext(side=3, las=1, line=0, text="Remedial", cex=1)

# barplot(Cervas1 , col= colorbarplot, border="white", axes=F, horiz=T)
# mtext(side=3, las=1, line=0, text="Cervas-Grofman V1", cex=1)

# barplot(Cervas2 , col= colorbarplot, border="white", axes=F, horiz=T)
# mtext(side=3, las=1, line=0, text="Cervas-Grofman V2", cex=1)

# barplot(Cervas3 , col= colorbarplot, border="white", axes=F, horiz=T)
# mtext(side=3, las=1, line=0, text="Cervas-Grofman V3", cex=1)

# barplot(DailyKos , col= colorbarplot, border="white", axes=F, horiz=T)
# mtext(side=3, las=1, line=0, text="DailyKos", cex=1)

# barplot(Wolf , col= colorbarplot, border="white", axes=F, horiz=T)
# mtext(side=3, las=1, line=0, text="Governor Wolf", cex=1)

# jointbar <- barplot(Joint , col= colorbarplot, border="white", axes=F, horiz=T)
# mtext(side=3, las=1, line=0, text="Joint Legislative", cex=1)

# text(x=c(Joint[1]-1, Joint[1]+Joint[2], Joint[1]+Joint[2]+Joint[3], Joint[1]+Joint[2]+Joint[3]+Joint[4]-1) , y= jointbar[1], labels=c("Strong Democratic", "Lean Democratic", "Lean Republican", "Strong Republican"), col="white", adj=1)

# barplot(Joint , col="white", border="white", axes=F, horiz=T)

# plot(x=0, y=0, xlim=c(0,100), col="white", axes=F, bty="n")
# axis(side=1, las=1, at=c(0, 25, 50, 75 , 100), line=-6, labels=c("0%", "25%", "50%", "75%", "100%"), cex=2)
# dev.off()

# Alternative Partisanship Graph

Old <-  t((matrix(c(4, 1, 5, 8), nrow=1)))
Remedial <- t((matrix(c(5, 4, 2, 7), nrow=1)))
Cervas1 <- t((matrix(c(5,3,3,7), nrow=1)))
Cervas2 <- t((matrix(c(5,2,3,8), nrow=1)))
Cervas3 <- t((matrix(c(5,4,2,7), nrow=1)))
DailyKos <- t((matrix(c(5,3,2,8), nrow=1)))
Wolf <- t((matrix(c(5,2,3,8), nrow=1)))
Joint <- t((matrix(c(4, 3, 3, 8), nrow=1)))

district_per <- abs(50-round(district_votes*100, digits=0))
plan_names <- c("2011 Map", "Court Remedial", "Joint Submission", "Gov. Wolf", "Dailykos", "Authors' V1", "Authors' V2", "Authors' V3")

# Gradiant Version
pdf(paste0("Papers/Grofman Cervas Gerrymandering 2018/Figures/barplotPartisanship.pdf"), width=8, height=8)
par(mfrow=c(1,9),
	mar=c(0,0,0,0))
for (i in 1:8) 
{
plot(x=c(0,1), y=c(0,18), col="white", axes=F)
rect(0,0,1,1, col = paste0("grey", abs(100-district_per[1,i]*2)), border = "white")
rect(0,1,1,2, col = paste0("grey", abs(100-district_per[2,i]*2)), border = "white")
rect(0,2,1,3, col = paste0("grey", abs(100-district_per[3,i]*2)), border = "white")
rect(0,3,1,4, col = paste0("grey", abs(100-district_per[4,i]*2)), border = "white")
rect(0,4,1,5, col = paste0("grey", abs(100-district_per[5,i]*2)), border = "white")
rect(0,5,1,6, col = paste0("grey", abs(100-district_per[6,i]*2)), border = "white")
rect(0,6,1,7, col = paste0("grey", abs(100-district_per[7,i]*2)), border = "white")
rect(0,7,1,8, col = paste0("grey", abs(100-district_per[8,i]*2)), border = "white")
rect(0,8,1,9, col = paste0("grey", abs(100-district_per[9,i]*2)), border = "white")
rect(0,9,1,10, col = paste0("grey", abs(100-district_per[10,i]*2)), border = "white")
rect(0,10,1,11, col = paste0("grey", abs(100-district_per[11,i]*2)), border = "white")
rect(0,11,1,12, col = paste0("grey", abs(100-district_per[12,i]*2)), border = "white")
rect(0,12,1,13, col = paste0("grey", abs(100-district_per[13,i]*2)), border = "white")
rect(0,13,1,14, col = paste0("grey", abs(100-district_per[14,i]*2)), border = "white")
rect(0,14,1,15, col = paste0("grey", abs(100-district_per[15,i]*2)), border = "white")
rect(0,15,1,16, col = paste0("grey", abs(100-district_per[16,i]*2)), border = "white")
rect(0,16,1,17, col = paste0("grey", abs(100-district_per[17,i]*2)), border = "white")
rect(0,17,1,18, col = paste0("grey", abs(100-district_per[18,i]*2)), border = "white")


# rect(0,0,1,Old[1,], col = "black", density=4, border = "black", lwd=3)
# rect(0,Old[1,],1,Old[1,]+Old[2,], col = "gray60", density=.65, border = "gray60", lwd=3)
# rect(0,Old[1,]+Old[2,],1,Old[1,]+Old[2,]+Old[3,], col = "gray40", density=.65, border = "gray40", lwd=3)
# rect(0,Old[1,]+Old[2,]+Old[3,],1,Old[1,]+Old[2,]+Old[3,]+Old[4,], col = "black", density=.65, border = "black", lwd=3)

# 50/50
abline(h=9, lty=3, lwd=1, col="gray20")
shadowtext(.5,9, pos=1, srt=90, label=plan_names[i], cex=1.5, col="black", bg="gray90")
}
plot(x=c(0,1), y=c(-200,400), col="white", axes=F)
for (j in 0:99)
{
	rect(.4,j,.6,(j+1), col = paste0("grey", j+1), border = "transparent")
}
for (j in 99:0)
{
	rect(.4,j+100,.6,j+101, col = paste0("grey", abs(j-100)), border = "transparent")
}
mtext(side=4,line=-5.25,"<---- More Democratic", cex=.75, adj=.5, at=-150)
mtext(side=4,line=-5.25,"More Republican ---->", cex=.75, adj=.5, at=350)
mtext(side=4, las=2, line=-6,"50/50", cex=.5, adj=.5, at=100)
axis(side=4, las=1, at=c(0, 25, 50, 75, 100, 125, 150, 175, 200), line=-2.5, labels=c("100%", "75%", "50%", "25%", "0%", "25%", "50%", "75%", "100%"), cex.axis=.75)
dev.off()


#### Folded Gradiant ####

# district_percent <- round(district_votes*100, digits=0)

# pdf(paste0(directory,"/barplotPartisanship2.pdf"), width=8, height=8)
# par(mfrow=c(2,4),
# 	mar=c(2,0,1,0))
#   for (i in 1:8) 
#       {
#         plot(x=c(-.25,1.35), y=c(0,9), col="white", axes=F)
#         rect(0,0,.45,1, col = paste0("grey", abs(100-district_per[9,i]*2)), border = "white")
#         rect(0,1,.45,2, col = paste0("grey", abs(100-district_per[8,i]*2)), border = "white")
#         rect(0,2,.45,3, col = paste0("grey", abs(100-district_per[7,i]*2)), border = "white")
#         rect(0,3,.45,4, col = paste0("grey", abs(100-district_per[6,i]*2)), border = "white")
#         rect(0,4,.45,5, col = paste0("grey", abs(100-district_per[5,i]*2)), border = "white")
#         rect(0,5,.45,6, col = paste0("grey", abs(100-district_per[4,i]*2)), border = "white")
#         rect(0,6,.45,7, col = paste0("grey", abs(100-district_per[3,i]*2)), border = "white")
#         rect(0,7,.45,8, col = paste0("grey", abs(100-district_per[2,i]*2)), border = "white")
#         rect(0,8,.45,9, col = paste0("grey", abs(100-district_per[1,i]*2)), border = "white")
#         rect(0.55,8,1,9, col = paste0("grey", abs(100-district_per[18,i]*2)), border = "white")
#         rect(0.55,7,1,8, col = paste0("grey", abs(100-district_per[17,i]*2)), border = "white")
#         rect(0.55,6,1,7, col = paste0("grey", abs(100-district_per[16,i]*2)), border = "white")
#         rect(0.55,5,1,6, col = paste0("grey", abs(100-district_per[15,i]*2)), border = "white")
#         rect(0.55,4,1,5, col = paste0("grey", abs(100-district_per[14,i]*2)), border = "white")
#         rect(0.55,3,1,4, col = paste0("grey", abs(100-district_per[13,i]*2)), border = "white")
#         rect(0.55,2,1,3, col = paste0("grey", abs(100-district_per[12,i]*2)), border = "white")
#         rect(0.55,1,1,2, col = paste0("grey", abs(100-district_per[11,i]*2)), border = "white")
#         rect(0.55,0,1,1, col = paste0("grey", abs(100-district_per[10,i]*2)), border = "white")

#         mtext(side=3,line=-.25, plan_names[i], cex=.75)
#         text(.25,-.25, "DEM")
#         text(.75,-.25, "REP")
#         shadowtext(rep(.25,9),seq(.5,8.5,1), paste0(district_percent[9:1,i], "%"), cex=.8, col="black", bg="gray90")
#         shadowtext(rep(.5,9),seq(.5,8.5,1), "-", cex=.8, col="black", bg="gray90")
#         shadowtext(rep(.75,9),seq(.5,8.5,1), paste0(100-district_percent[10:18,i], "%"), cex=.8, col="black", bg="gray90")
#         shadowtext(rep(1.05,9),seq(.5,8.5,1), "=", cex=.8, col="black", bg="gray90")
#         shadowtext(rep(1.25,9),seq(.5,8.5,1), paste0(district_percent[9:1,i]-(100-district_percent[10:18,i]), "%"), cex=.8, col="black", bg="gray90")
#       }

# plot(x=c(0,1), y=c(-200,400), col="white", axes=F)
#   for (j in 0:99)
#     {
# 	   rect(.4,j,.6,(j+1), col = paste0("grey", j+1), border = "transparent")
#     }
#   for (j in 99:0)
#     {
# 	 rect(.4,j+100,.6,j+101, col = paste0("grey", abs(j-100)), border = "transparent")
#     }
#       mtext(side=4,line=-5.25,"<---- More Democratic", cex=.75, adj=.5, at=-150)
#       mtext(side=4,line=-5.25,"More Republican ---->", cex=.75, adj=.5, at=350)
#       mtext(side=4, las=2, line=-6,"50/50", cex=.5, adj=.5, at=100)
#   axis(side=4, las=1, at=c(0, 25, 50, 75, 100, 125, 150, 175, 200), line=-2.5, labels=c("100%", "75%", "50%", "25%", "0%", "25%", "50%", "75%", "100%"), cex.axis=.75)
# dev.off()

# plot(x=c(0,1), y=c(0,18), col="white", axes=F)
# rect(0,0.45,.5,.55, col = "black", border = "transparent")
# rect(0,17.45,.5,17.55, col = "black", border = "transparent")
# rect(0.45,.5,.5, 17.55, col = "black", border = "transparent")
# shadowtext(.5,9, pos=1, srt=90, label="Compare", cex=1.5, col="black", bg="gray90")



# =================================================================
# -- COMPACTNESS -- -- COMPACTNESS -- -- COMPACTNESS -- -- COMPACTN
# =================================================================
# Compactness
compactness <- list()
plan_compactness <- list()

old <- "Side Projects/PA Redistricting/Shapefiles/Current PA Districts/PA_115th.shp"
wolf <- "Side Projects/PA Redistricting/Shapefiles/Gov Wolf Shape Files - 006818/Wolf_Map_Submission.shp"
remedial <- "Side Projects/PA Redistricting/Shapefiles/Remedial Plan Shape Files - 006845/Remedial Plan Shapefile.shp"
cervasv1 <- "Side Projects/PA Redistricting/Shapefiles/Cervas_Grofman_Plans/PlanNon-Partisan_V1_adjusted.shp"
cervasv2 <- "Side Projects/PA Redistricting/Shapefiles/Cervas_Grofman_Plans/PlanNon-Partisan_V2.shp"
cervasv3 <- "Side Projects/PA Redistricting/Shapefiles/Cervas_Grofman_Plans/PlanV3.shp"
joint <- "Side Projects/PA Redistricting/Shapefiles/PA Legislative Proposal feb 9/JointSubmissionShape.shp"
dailykos <- "Side Projects/PA Redistricting/Shapefiles/DailyKos Shapefiles/Plan A Districts.shp"

plans <- c("old", "remedial", "joint", "wolf", "dailykos", "cervasv1", "cervasv2", "cervasv3")
planNames <- c("2011 Plan", "Court Remedial", "Joint Legislative", "Gov. Wolf", "DailyKos", "Authors' V1", "Authors' V2", "Authors' V3")

  for (plan in plans) 
    {
      shapeFile <- readOGR(get(plan))
      #shapeFile <- spTransform(shapeFile, CRS("+proj=merc"))
      mapObject <- fortify(shapeFile)  # Convert to a data.frame
      mapObject <- data.frame(mapObject, shapeFile@data[mapObject$id, ])
      mapObject$piece <- as.character(mapObject$piece)
      mapObject$Area <- mapObject$Perimeter <- mapObject$smallestcircle <- mapObject$centroid <- NA
      uniqueCDs <- sort(unique(mapObject$id))
      for(id in uniqueCDs)
        {  # This loop will take some time
          print(id)
          cdShape <- mapObject[mapObject$id == id, ]

          cdPoly <- SpatialPolygons(list(Polygons(lapply(split(cdShape[, c("long", "lat")], cdShape$piece), Polygon), ID = "b")))
          owinObject <- try(as(cdPoly, "owin"))
          mapObject[mapObject$id == id, "Area"] <- area.owin(owinObject)

          mapObject[mapObject$id == id, "Perimeter"] <- perimeter(owinObject)
          
          mapObject[mapObject$id == id, "smallestcircle"] <- boundingradius(owinObject)
           
          cent <- centroid.owin(owinObject, as.ppp = TRUE)
          compactness[[id]] <-  data.frame(area= area.owin(owinObject), perimeter=perimeter(owinObject), smallestcircle=boundingradius(owinObject))
        }
        plan_compactness[[plan]] <- do.call("rbind", compactness)
      }

REOCK <- plan_compactness[["remedial"]]$area/(plan_compactness[["remedial"]]$smallestcircle^2 * 3.1415926535)
POLSBYPOPPER <- (4 * 3.1415926535 * plan_compactness[["remedial"]]$area)/ (plan_compactness[["remedial"]]$perimeter^2)
#### Plot Compactness ####


shapeFile <- readOGR(remedial)
mapObject <- fortify(shapeFile)  # Convert to a data.frame
mapObject <- data.frame(mapObject, shapeFile@data[mapObject$id, ])
mapObject$piece <- as.character(mapObject$piece)
 cdShape <- mapObject[mapObject$id == 15, ]
  cdPoly <- SpatialPolygons(list(Polygons(lapply(split(cdShape[, c("long", "lat")], cdShape$piece), Polygon), ID = "b")))
  owinObject <- try(as(cdPoly, "owin"))

old_plan <- readOGR(old)
old_plan_mapObject <- fortify(old_plan)  # Convert to a data.frame
old_plan_mapObject <- data.frame(old_plan_mapObject, old_plan@data[old_plan_mapObject$id, ])
old_plan_mapObject$piece <- as.character(old_plan_mapObject$piece)
 old_plan_Shape <- old_plan_mapObject[old_plan_mapObject$id == 5, ]
  old_plan_Poly <- SpatialPolygons(list(Polygons(lapply(split(old_plan_Shape[, c("long", "lat")], old_plan_Shape$piece), Polygon), ID = "b")))
  old_plan_owin <- try(as(old_plan_Poly, "owin")) 

pdf(paste0("Papers/Grofman Cervas Gerrymandering 2018/Figures/compactness.pdf"), width = 6, height = 6)
par(mfrow = c(2, 2), mar = c(0, 0, 1, 0), oma = c(0, 0, 0, 0), mai = c(0, 0, 0.4, 0), xpd = NA, pty = "s")

# Reock	
plot(boundingcircle(owinObject), main = "")
rect(1 * par("usr")[1], (1 * par("usr")[3]), 0.995 * par("usr")[2], (1 * par("usr")[4]), density = NULL, angle = 45, col = "gray70", border = NA, 
	lty = par("lty"), lwd = par("lwd"), xpd = NA)

plot(boundingcircle(owinObject), col = "white", add = T)
plot(owinObject, add = T, col = "gray30", border = "gray30")
text(centroid.owin(owinObject)$x, centroid.owin(owinObject)$y, label = paste("Area:", round(area.owin(owinObject), digits = 2)), col = "gray50", 
	cex = 0.7, srt = 0)
mtext(side = 3, line = -2, text = paste("Area:", round(area.owin(boundingcircle(owinObject)), digits = 2)), col = "gray30", cex = 0.6, srt = 0)

#Plot Polsby-Popper
r <- perimeter(owinObject)/(2 * 3.1415926535)
circle.new(centroid.owin(owinObject)$x, centroid.owin(owinObject)$y, r, main = "", add = F)
rect(1.0015 * par("usr")[1], (1 * par("usr")[3]), 1 * par("usr")[2], (1 * par("usr")[4]), density = NULL, angle = 45, col = "gray70", border = NA, 
	lty = par("lty"), lwd = par("lwd"), xpd = NA)
circle(centroid.owin(owinObject)$x, centroid.owin(owinObject)$y, r, col = "white")

plot(owinObject, col = "gray30", border = "gray30", add = T)
text(centroid.owin(owinObject)$x, centroid.owin(owinObject)$y, label = paste("Area:", round(area.owin(owinObject), digits = 2)), col = "gray50", 
	cex = 0.7)
mtext(side = 3, line = -2, text = paste("Area:", round((3.1415926535) * r^2, digits = 2)), col = "gray30", cex = 0.6, srt = 0)

# Plot Second plan
# Reock	
plot(boundingcircle(old_plan_owin), main = "Reock")
rect(1 * par("usr")[1], (1 * par("usr")[3]), 0.995 * par("usr")[2], (1 * par("usr")[4]), density = NULL, angle = 45, col = "gray70", border = NA, 
	lty = par("lty"), lwd = par("lwd"), xpd = NA)
plot(boundingcircle(old_plan_owin), col = "white", add = T)

plot(old_plan_owin, add = T, col = "gray30", border = "gray30")
text(centroid.owin(old_plan_owin)$x, centroid.owin(old_plan_owin)$y, label = paste("Area:", round(area.owin(old_plan_owin), digits = 2)), col = "gray50", 
	cex = 0.7, srt = 0)
mtext(side = 3, line = -2, text = paste("Area:", round(area.owin(boundingcircle(old_plan_owin)), digits = 2)), col = "gray30", cex = 0.6, srt = 0)

#Plot Polsby-Popper
r <- perimeter(old_plan_owin)/(2 * 3.1415926535)
circle.new(centroid.owin(old_plan_owin)$x, centroid.owin(old_plan_owin)$y, r, main = "Polsby-Popper", add = F)
rect(1.0015 * par("usr")[1], (1 * par("usr")[3]), 1 * par("usr")[2], (1 * par("usr")[4]), density = NULL, angle = 45, col = "gray70", border = NA, 
	lty = par("lty"), lwd = par("lwd"), xpd = NA)
circle(centroid.owin(old_plan_owin)$x, centroid.owin(old_plan_owin)$y, r, col = "white")

plot(old_plan_owin, col = "gray30", border = "gray30", add = T)
text(centroid.owin(old_plan_owin)$x, centroid.owin(old_plan_owin)$y, label = paste("Area:", round(area.owin(old_plan_owin), digits = 2)), col = "gray50", 
	cex = 0.7)
mtext(side = 3, line = -2, text = paste("Area:", round((3.1415926535) * r^2, digits = 2)), col = "gray30", cex = 0.6, srt = 0)

title("Remedial Plan\n District 18", outer = TRUE, line = -5, cex.main = 0.7)
title("2011 Plan Plan\n District 14", outer = TRUE, line = -23, cex.main = 0.7)
dev.off()

  # write.csv(do.call("rbind", plan_compactness),  "Side Projects/PA Redistricting/Data/Plan Comparisons/compactness.csv")
  
  
# =================================================================
# -- MAPS -- -- MAPS -- -- MAPS -- -- MAPS -- -- MAPS -- -- MAPS -
# =================================================================
  
  # Plot Plan Maps
pdf(paste0("Papers/Grofman Cervas Gerrymandering 2018/Figures/maps.pdf"), 
  	width=8, 
  	height=10)
par(mfrow=c(4,2),
	mar=c(0,0,1,0),
	bg="white")

		n <- 1  
	for (plan in plans) {
		shapeFile <- readOGR(get(plan))
		shapeFile <- spTransform(shapeFile, CRS("+proj=merc"))

			plot(shapeFile, 
				col="gray50", 
				border="white")
			mtext(side=2, srt=45, line=-2, text=planNames[n], cex=1)
		n <- n+1 
  }
dev.off()

color_ramp <- c(rgb(220,220,220, 127, maxColorValue=255), rgb(0,0,0, 127, maxColorValue=255), rgb(211,211,211, 127, maxColorValue=255), rgb(112,128,144, 127, maxColorValue=255), rgb(169,169,169, 127, maxColorValue=255), rgb(119,136,153, 127, maxColorValue=255), rgb(128,128,128, 127, maxColorValue=255), rgb(105,105,105, 127, maxColorValue=255))
# --------------------------------------------------------------------------------
# Detailed Maps of 111th Congress and Court Remedial
# --------------------------------------------------------------------------------
		pa_counties <- spTransform(pa_counties, CRS("+proj=merc"))
		pa_major <- spTransform(pa_major, CRS("+proj=merc"))
		
		shapeFile <- readOGR(old)
		shapeFile <- spTransform(shapeFile, CRS("+proj=merc"))

			plot(pa_counties, 
				col="gray50")
			plot(shapeFile, 
				col= color_ramp,
				border="white",
				add=T)
			plot(pa_counties, 
				border="black",
				col=NA,
				add=T)
			plot(shapeFile, 
				border="white",
				col=NA,
				add=T)
			points(coordinates(pa_major)[,1],coordinates(pa_major)[,2], 
				cex=1.5, 
				pch=16)
  
		shapeFile <- readOGR(remedial)
		shapeFile <- spTransform(shapeFile, CRS("+proj=merc"))
	
# ================================================================= #
# -- MEASURES OF GERRYMANDERING -- -- MEASURES OF GERRYMANDERING -- #
# ================================================================= #
# Measures of Gerrymandering
      # WE SHOULD SIMULATE RANDOM DISTRICT AND STATEWIDE SWINGS AND REPORT 
    # AVERAGE SCORES SINCE SOME OF THE MEASURES DO NOT CONTAIN INFORMATION ON RESPONSIVENESS!
unc <- function(inp) -1*(inp<0.25)+1*(inp>0.75)

elecyears <- as.numeric(names(PA_house))
same.d <- 1*(elecyears %% 10 != 2)
judgeit.bias <- new.list(length(elecyears))
names(judgeit.bias) <- elecyears
seatsvotes_PA <- new.list(length(elecyears))
names(seatsvotes_PA) <- elecyears
j.ob <- JudgeIt::judgeit(model.formula=default.unc(VOTE)~unc(VOTE),vote.formula=TURNOUT~1, weight= "turnout", use.last.votes=T, same.d=same.d, data=PA_house)



for (j in elecyears) {
  judgeit.bias[[as.character(j)]] <- jug.tmp <- JudgeIt::bias.resp(j.ob,year=j)
bias_45_55 <- jug.tmp[1]$svsums[2,1]
responsiveness_45_55 <- jug.tmp[1]$svsums[3,1]
}

for (j in elecyears) {
  seatsvotes_PA[[as.character(j)]] <- seatsvotes(year=j, pct=default.unc(PA_house[[as.character(j)]]$VOTE), weights= PA_house[[as.character(j)]]$TURNOUT, center="average", range=c(0.45, 0.55))
}

judgeit.seats <- new.list(length(elecyears))
names(judgeit.seats) <- elecyears

for (j in elecyears) {
  judgeit.seats[[as.character(j)]] <- JudgeIt::seats(j.ob, year = j)
}


judgeit.plans <- new.list(length(plans_2016))
  judgeit.bias.plans <- rep(NA, length(plans_2016))
names(judgeit.plans) <- names(plans_2016)
  for (p in 1:length(plans_2016))
    {
  j.ob.plans <- JudgeIt::judgeit(model.formula=D_ave_TP~unc(D_ave_TP),vote.formula=sumTotal~1, weight= "turnout", use.last.votes=F, data=plans_2016[[p]])
  judgeit.plans[[as.character(p)]] <- jug.tmp <- JudgeIt::bias.resp(j.ob.plans,year=1)
  judgeit.bias.plans[p] <- jug.tmp[1]$svsums[2,1]
}


measures_of_gerrymandering <- numeric(0)
  for (i in 1:length(plans_2016))
    {
      plan.index <- names(plans_2016)[[i]]
      D_TP_comp <- two_party(plans_2016[[i]]$Dsum, plans_2016[[i]]$Rsum)
      D_TP_comp.w <- plans_2016[[i]]$Dsum + plans_2016[[i]]$Rsum
  composite.tmp <- cbind.data.frame(VOTE=default.unc(D_TP_comp), TURNOUT=D_TP_comp.w)
      seatsvotes_tmp <- seatsvotes(pct=composite.tmp$VOTE, weights= composite.tmp$TURNOUT, year=2016, center="average", range=c(0.45, 0.55))
      seatsvotes_centered_tmp <- seatsvotes(pct=composite.tmp$VOTE, year=2016, center="actual", range=c(0.45, 0.55))
      eff_tmp <- eff_gap(plans_2016[[i]]$Dsum, plans_2016[[i]]$Rsum)
      declination(plans_2016[[i]]$DsumTP)
      meanmedian_tmp <- meanmedian(plans_2016[[i]]$DsumTP)
      declination_tmp <- declination(D_TP_comp)

      measures_of_gerrymandering <- rbind(measures_of_gerrymandering,
      	cbind.data.frame(
          plan = plan_names[i], 
          # bias_judgeit = judgeit.bias.plans[i],
          # bias_gelman = bias.gelman[i],
      		bias = seatsvotes_tmp$bias, 
      		# biascenteractual= seatsvotes_centered_tmp$bias, 
      		# swing_ratio=seatsvotes_tmp$swing_ratio, 
      		efficiency_gap = eff_tmp, 
      		mean_median = meanmedian_tmp,
          declination = declination_tmp,
      		seats = percent(seatsvotes_tmp$seats, digits=1)
      		# votes = seatsvotes_tmp$votes)
          )
      			
      # measures_of_gerrymandering <- rbind(measures_of_gerrymandering,
      # 	cbind.data.frame(plan="se",
      #     bias_judgeit = as.numeric(""),
      #     bias_gelman = as.numeric(""),
      # 		bias=seastsvotes_tmp$bias_se, 
      # 		biascenteractual= seastsvotes_centered_tmp$bias_se, 
      # 		swing_ratio=seastsvotes_tmp$swing_ratio_se, 
      # 		efficiency_gap= as.numeric(""), 
      # 		mean_median=as.numeric(""),
      #     declination = as.numeric(""),
      # 		seats=as.numeric(""), 
      # 		votes=as.numeric(""))
        )
    }


    measures_of_gerrymandering

 gerry_table <- stargazer(
    measures_of_gerrymandering, 
    type="latex",
    style = "apsr", 
    covariate.labels = 
      c("Plan", 
        # "JudgeIt", 
        # "Gelman", 
        "Partisan Bias", 
        # "Seats/Votes - Actual", 
        # "Swing Ratio", 
        "Efficiency Gap", 
        "Mean/Median", 
        "Declination", 
        "Seats"
        # "Votes"
        ), 
    # dep.var.labels = c("Prop Democrat", "Affirm. Action", "Racial Resentment"),
    # column.lables = c("d", "Partisan Bias", "d"),
    column.separate = c(1, 5, 5),
    summary=F,
    column.sep.width = "0pt", 
    float = FALSE, 
    header = FALSE,
    multicolumn = TRUE,
    rownames=F, 
    title="Measures of Gerrymandering for Proposed Plans", 
    label="tab:a1", 
    digits=3)
gerry_table <- gsub("\\$\\$-\\$", "\\$-", gerry_table)
cat(paste(gerry_table, collapse = "\n"), "\n")

# Gather data from precinct files 2008 
folders <- list.files(path = "Side Projects/PA Redistricting/Data/2000_2010_Partisanship by District")

for (j in 1:length(folders))
  {
    files08 <- list.files(path = paste0("Side Projects/PA Redistricting/Data/2000_2010_Partisanship by District/", folders[j]))
    temp <- numeric(0)
    for (i in 1:length(files08))
    {
    	dataframe <- read.csv(paste0("Side Projects/PA Redistricting/Data/2000_2010_Partisanship by District/", folders[j], "/", files08[i]))
      new.data <- data.frame(
      	district = i,
      	DEMpres2008 = round(sum(dataframe[,"USPDV2008"])),
      	REPpres2008 = round(sum(dataframe[,"USPRV2008"])),
      	DEMatt2008 = round(sum(dataframe[,"ATTDV2008"])),
      	REPatt2008 = round(sum(dataframe[,"ATTRV2008"])),
      	DEMaud2008 = round(sum(dataframe[,"AUDDV2008"])),
      	REPaud2008 = round(sum(dataframe[,"AUDRV2008"])),
      	DEMtre2008 = round(sum(dataframe[,"TREDV2008"])),
      	REPtre2008 = round(sum(dataframe[,"TRERV2008"])),
      	DEMdecadeAVE = round(sum(dataframe[,"NDV"])),
      	REPdecadeAVE = round(sum(dataframe[,"NRV"])),
      	DEMpresTP = round(sum(dataframe[,"USPDV2008"]))/(round(sum(dataframe[,"USPDV2008"])) + round(sum(dataframe[,"USPRV2008"])))
      	)
    	temp <- rbind(temp, new.data)
    }
  temp <- temp[order(temp$DEMpresTP),]
  write.csv(temp, paste0("Side Projects/PA Redistricting/Data/2008 District Partisanship/2008_", folders[j], ".csv"), row.names=F)

  }

pa_cds <- readOGR(old)

uniqueCDs <- unique(pa_cds@data$CD115FP)

partisanship <- data.frame(
District=uniqueCDs,
totalpersons=NA,
DEMpres=NA,
REPpres=NA,
T16SEND=NA,
T16SENR=NA,
T16ATGD=NA,
T16ATGR=NA,
T16AUDD=NA,
T16AUDR=NA,
T16TREASD=NA,
T16TREASR=NA
)
partisanship <- numeric(0)
  partisanship.in <- list()
     pa_partisanship.d <- pa2008
for(cd in uniqueCDs){  # This loop will take some time

  print(cd)
mapObject <- pa_cds
mapObject <- spTransform(mapObject, CRS("+proj=merc"))
pa_partisanship.d <- spTransform(pa_partisanship.d, CRS("+proj=merc"))

  cdShape <- mapObject[mapObject@data$CD115FP == cd, ]

 counties.d <- pa_counties
     # pa_partisanship.d <- spTransform(pa_partisanship.d, CRS(proj4string(cdShape)))
        pa_partisanship.d@data$within <- sapply(1:length(gCentroid(pa_partisanship.d, byid=T)), function(x) {
     gIntersects(cdShape, gCentroid(pa_partisanship.d, byid=T)[x,])
   })
     partisanship.in[[cd]] <- pa_partisanship.d@data[pa_partisanship.d@data$within=="TRUE",]
 # partisanship [partisanship$District == cd, "totalpersons"] <- sum(as.numeric(as.character(partisanship.in@data$TAPERSONS)))
 # partisanship [partisanship$District == cd, "DEMpres"] <- sum(as.numeric(as.character(partisanship.in@data$T16PRESD)))
 # partisanship [partisanship$District == cd, "REPpres"] <-  sum(as.numeric(as.character(partisanship.in@data$T16PRESR)))
 # partisanship [partisanship$District == cd, "T16SEND"] <-  sum(as.numeric(as.character(partisanship.in@data$T16SEND)))
 # partisanship [partisanship$District == cd, "T16SENR"] <-  sum(as.numeric(as.character(partisanship.in@data$T16SENR)))
 # partisanship [partisanship$District == cd, "T16ATGD"] <-  sum(as.numeric(as.character(partisanship.in@data$T16ATGD)))
 # partisanship [partisanship$District == cd, "T16ATGR"] <-  sum(as.numeric(as.character(partisanship.in@data$T16ATGR)))
 # partisanship [partisanship$District == cd, "T16AUDD"] <-  sum(as.numeric(as.character(partisanship.in@data$T16AUDD)))
 # partisanship [partisanship$District == cd, "T16AUDR"] <-  sum(as.numeric(as.character(partisanship.in@data$T16AUDR)))
 # partisanship [partisanship$District == cd, "T16TREASD"] <-  sum(as.numeric(as.character(partisanship.in@data$T16TREASD)))
 # partisanship [partisanship$District == cd, "T16TREASR"] <-  sum(as.numeric(as.character(partisanship.in@data$T16TREASR)))
     pa_partisanship.d <- pa_partisanship.d[pa_partisanship.d@data$within=="FALSE",]

  }

  for (i in 1:18)
 {
 	write.csv(partisanship.in[[i]], paste0("Side Projects/PA Redistricting/Data/2000_2010_Partisanship by District/2011Map/District", i, ".csv"))
 }
 
 
 #### Additional Plot Options
 
D <- dt[,3]
R <- dt[,4]
    total <- sum(D)+sum(R)
    dem_wasted <- ifelse(D>R, D[D>R] - R[D>R], D[D<R])
    rep_wasted <- ifelse(D<R, R[D<R] - D[D<R], R[D>R])

Dcumsum <- cumsum(dem_wasted[order(-dem_wasted)])
Rcumsum <- cumsum(rep_wasted[order(-rep_wasted)])
Tcumsum <- (sum(dem_wasted)+sum(rep_wasted))
temp <- Dcumsum - ((Rcumsum+Rcumsum)/2)
Dtem <- temp[temp>0]
Rtem <- abs(temp[temp<0])
Rtem <- Rtem[order(-Rtem)]
# Plot Wasted Votes
plot(1:length(Dcumsum), Dcumsum/Tcumsum, type="l", xlim=c(1,18), ylim=c(0,1), bty="n", ylab="", cex.lab=.85, cex.axis=.65, xaxt="n", xlab="")
lines(1:length(Dcumsum), Rcumsum/Tcumsum, lty=3, col="gray50", add=TRUE)
 axis(side=1, las=1, at=c(1,18), lab=c("Most Democratic", "Least Democratic"), cex.axis=.65)
#points(1:length(Rtem), cumsum(Rtem), type="l", lty=3)
abline(v=9, lty=3, col="gray70")
abline(v=seq(1,18), col="gray80", lty=3)
abline(h=seq(0,1, .2), col="gray80", lty=3)
abline(h= .5, lty=2, col="gray40")
abline(v= 9, lty=2, col="gray40")
 mtext(side=2, line=2.5, "Percent of Waste Votes", cex=.65)
 # text(12, total/2, pos=1, expression(italic("Half the Wasted Votes")), cex=.6, srt=0) 
 text(9.25, .5, pos=1, expression(italic("Middle District")), cex=.6, srt=90) 
 text(1:18, 1.05, pos=1, 1:18, cex=.6, srt=0) 

dt$Dsum - dt$Rsum[order(-dt$Rsum)]



temp <- dt$Dsum / ((dt$Dsum+dt$Rsum))
Dtem <- temp[9:1]
Rtem <- abs(temp[10:18])

# Difference in Share of the Two Party vote by Competitiveness
axis1 <- c(0, .25, .5)
plot(Dtem-Rtem, type="l", bty="n", xaxt="n", yaxt="n", xlim=c(1,9), xlab="", ylab="")
 axis(side=1, las=1, at=c(1,9), lab=c("Most Competitive", "Most Partisan"), cex.axis=.65)
 axis(side=2, las=2, at= pretty(axis1), lab=paste0(pretty(axis1)*100, "%"), cex.axis=.65)
 mtext(side=2, line=2.5, "Democratic Share - Republican Share", cex=.65)
grid(NULL, NULL, lwd=.5, lty=3, col="gray80")


barplot(t(cbind(temp, 1-temp )))
abline(h=0.5, lty=3, col="gray70")
 axis(side=1, las=1, at=c(0,21.75), lab=c("Most Democratic", "Least Democratic"), cex.axis=.65)




## Other States
pres2016 <- read.csv("https://raw.githubusercontent.com/jcervas/data/master/Elections/Presidential/2016%20Pres%20by%20CD.csv")
states <- c("Arizona", "Arkansas", "Delaware", "Illinois", "Indiana", "Kentucky", "Oklahoma", "Oregon", "South Dakota", "Tennessee", "Washington", "Wyoming")

for (st in states)
{
	st_temp <- pres2016[pres2016$state==st,]
	cat(st)
	cat("\n")
cat("Seats/Votes Centered at 50/50")
cat("\n")
	print(seatsvotes(st_temp$dem, st_temp$rep, year=2016))
cat("Seats/Votes Centered at Actual")
cat("\n")
	print(seatsvotes(st_temp$dem, st_temp$rep, year=2016, center="actual"))
cat("Efficiency Gap")
cat("\n")
	print(eff_gap(st_temp$dem, st_temp$rep))
cat("Declination")
cat("\n")
	print(declination(st_temp$margin))
cat("Mean/Median")
cat("\n")
	print(meanmedian(st_temp$margin))
cat("\n")
cat("\n")	
}


### Incumbency Advantage
pres2016 <- pres

pres2016 <- pres2016[, c("abv", "year", "state", "district", "PresDEM", "PresREP")]
cong <- read.csv("https://raw.githubusercontent.com/jcervas/data/master/Elections/House/elections1968_2016.csv")
prescong <- merge(pres, cong, by=c("abv", "district", "year"))
cong2016 <- cong[cong$year=="2016",]
prescong2016 <- merge(pres2016, cong2016, by=c("abv", "district", "year"))
prescong2016$demwin <- find.winner((prescong2016$dem/(prescong2016$dem + prescong2016$rep)))

prescong2016$dem[is.na(prescong2016$dem)] <- 0
prescong2016$rep[is.na(prescong2016$rep)] <- 0

prescong$dem[prescong$dem==0] <- NA
prescong$rep[prescong$rep==0] <- NA

mean(two_party(prescong2016$PresDEM, prescong2016$PresREP) - two_party(prescong2016$dem, prescong2016$rep) , na.rm=T) * 100

mean((prescong2016$PresDEM[prescong2016$demwin==1]/(prescong2016$PresDEM[prescong2016$demwin==1] + prescong2016$PresREP[prescong2016$demwin==1])) - (prescong2016$dem[prescong2016$demwin==1]/(prescong2016$dem[prescong2016$demwin==1] + prescong2016$rep[prescong2016$demwin==1])) , na.rm=T) * 100
mean((prescong2016$PresREP[prescong2016$demwin==0]/(prescong2016$PresDEM[prescong2016$demwin==0] + prescong2016$PresREP[prescong2016$demwin==0])) - (prescong2016$rep[prescong2016$demwin==0]/(prescong2016$dem[prescong2016$demwin==0] + prescong2016$rep[prescong2016$demwin==0])) , na.rm=T) * 100

 returns <- runif(100)



pdf(paste0("Papers/Grofman Cervas Gerrymandering 2018/Figures/prescong_scatterplots.pdf"), width=12, height=6)
par(mfrow=c(1,2),
	mar=c(3,3.5,0,0),
	pty="s")
plot(x=0, y=0, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", xaxt="n", yaxt="n", col="white", bty="n")
mtext(side=1, line=2, "Presidential Votes Share 2016", cex=0.65)
mtext(side=2, line=2.5, "Congressional Vote Share 2016", cex=0.65)
axis(side=2, las=2, cex.axis=0.65, at=pretty(returns), lab=paste0(pretty(returns)*100, "%"))
axis(side=1, cex.axis=0.65, at=pretty(returns), lab=paste0(pretty(returns)*100, "%"))
abline(0,1, lty=4, col="gray20",lwd=1)
points(x=two_party(prescong$PresDEM[prescong$year=="2016"], prescong$PresREP[prescong$year=="2016"]), y=two_party(prescong$dem[prescong$year=="2016"], prescong$rep[prescong$year=="2016"]), pch=19, col = "#33333333")
		# rect(-.05,-.05,0,1.05, col = rgb(0.1,0.1,0.1,1/2), density=40, border = "transparent") #left
		# rect(0,0,1.05 ,-.05, col = rgb(0.1,0.1,0.1,1/2), density=40, border = "transparent") #bottom
		# rect(1,0,1.05,1.05, col = rgb(0.1,0.1,0.1,1/2), density=40, border = "transparent") #right
		# rect(0,1,1,1.05, col = rgb(0.1,0.1,0.1,1/2), density=40, border = "transparent") #top
plot(x=0, y=0, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", xaxt="n", yaxt="n", col="white", bty="n")
mtext(side=1, line=2, "Presidential Votes Share 1956-2016", cex=0.65)
mtext(side=2, line=2.5, "Congressional Vote Share 1956-2016", cex=0.65)
axis(side=2, las=2, cex.axis=0.65, at=pretty(returns), lab=paste0(pretty(returns)*100, "%"))
axis(side=1, cex.axis=0.65, at=pretty(returns), lab=paste0(pretty(returns)*100, "%"))
abline(0,1, lty=4, col="gray20",lwd=1)
points(x=two_party(prescong$PresDEM, prescong$PresREP), y=two_party(prescong$dem, prescong$rep), pch=19, col = "#33333333")

dev.off()


precincts[precincts==0] <- NA
precincts[precincts==0] <- NA

prec_list <- list(
cbind(precincts$T16PRESD, precincts$T16PRESR),
cbind(precincts$T16SEND, precincts$T16SENR),
cbind(precincts$T16ATGD, precincts$T16ATGR),
cbind(precincts$T16AUDD, precincts$T16AUDR),
cbind(precincts$T16TREASD, precincts$T16TREASR),
cbind(precincts$T16PRESD+precincts$T16SEND+precincts$T16ATGD+precincts$T16AUDD+precincts$T16TREASD, precincts$T16PRESR+precincts$T16SENR+precincts$T16ATGR+precincts$T16AUDR+precincts$T16TREASR)
)
race <- c("Presidential", "US Senate", "Attorneys General", "Auditor", "Treasurer", "Composite")


pdf(paste0("Papers/Grofman Cervas Gerrymandering 2018/Figures/cong_statewide_scatterplots.pdf"), width=12, height=10)
par(mfrow=c(2,3),
	mar=c(3,4,1,0.5),
	pty="s")
for (i in 1:6)
{
plot(x=0, y=0, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", xaxt="n", yaxt="n", col="white", bty="n")
mtext(side=1, line=2, paste(race[i], "Vote Share 2016"), cex=0.65)
if (i == 1) { mtext(side=2, line=2.75, "Congressional Vote Share 2016", cex=0.65) }
if (i == 4) { mtext(side=2, line=2.75, "Congressional Vote Share 2016", cex=0.65) }
axis(side=2, las=2, cex.axis=1, at=pretty(returns), lab=paste0(pretty(returns)*100, "%"))
axis(side=1, cex.axis=1, at=pretty(returns), lab=paste0(pretty(returns)*100, "%"))
abline(0,1, lty=4, col="gray20",lwd=1)
points(x=two_party(prec_list[[i]][,1], prec_list[[i]][,2]), y=two_party(precincts$T16CONGD, precincts$T16CONGR), pch=19, col = "#33333333", cex=precincts$TAPERSONS/sum(precincts$TAPERSONS, na.rm=T)*2000)	
		rect(-.05,-.05,0,1.05, col = rgb(0.1,0.1,0.1,1/2), density=40, border = "transparent") #left
		rect(0,0,1.05 ,-.05, col = rgb(0.1,0.1,0.1,1/2), density=40, border = "transparent") #bottom
		rect(1,0,1.05,1.05, col = rgb(0.1,0.1,0.1,1/2), density=40, border = "transparent") #right
		rect(0,1,1,1.05, col = rgb(0.1,0.1,0.1,1/2), density=40, border = "transparent") #top
}

dev.off()

prescong[prescong$year.x=='2014',]


prescong2016 <- merge(pres2016[pres2016$year=="2016",], house, by=c("abv", "district", "year"))
prescong2016 <- prescong2016[order(prescong2016$state.y),]
prescong2016 <- cbind.data.frame(prescong2016, ptynow=find.winner(house_2014$dpres/100))
summary(reg_prescong2016_1 <- lm(two_party(dem,rep) ~ two_party(PresDEM,PresREP), data=prescong2016))
summary(reg_prescong2016_2 <- lm(two_party(dem,rep) ~ two_party(PresDEM,PresREP) + inc3, data=prescong2016))
summary(reg_prescong2016_3 <- lm(two_party(dem,rep) ~ two_party(PresDEM,PresREP) + inc3 + ptynow, data=prescong2016))

### Table B - Incumbency Advantage in 2016
stargazer(reg_prescong2016_1, reg_prescong2016_2, reg_prescong2016_3, 
			type="text", 
			omit.stat=c("rsq", "ser"), 
			style="AJPS", 
			covariate.labels=c("Presidential Vote", "Incumbency", "Party"), 
			dep.var.labels="",
			title="Incumbency Advantage in 2016", 
			notes="Note: All Regressions calculated using Democratic share of the two-party vote.")




```


