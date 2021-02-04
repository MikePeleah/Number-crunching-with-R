#################################################################################################
#
# Code for peer learning Webinar 1 December 2020 
#
#################################################################################################

# Load two libraries, dplyr for data manipualtions, and ggplot2 for charts 
# if necessary, install these packages through menu Tools > Install Packages...
library(dplyr)
library(ggplot2)

# Set up working directory. You could check current working directory by typing getwd() in console
# Note that you should use either slash (/) or double backslash (\\) for path 
setwd("V:\\$R Webinar for SDT\\Webinar #1 Numbercrunching 2020-12-01")

# Working directory should contain two datasets downloaded from web. You could read data directly from web by providing url in read.csv.
# However, due to file size (230Mb+ for Google mobility data, 18Mb+ for COVID tracker data) it could take long time.
#   Global Mobility Report data 
#     https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv
#   Oxford Covid Stringency Policy Data
#     https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv
mobility_data <- read.csv("Global_Mobility_Report.csv", stringsAsFactors = FALSE)
stringency_data <- read.csv("OxCGRT_latest.csv", stringsAsFactors = FALSE)
# Load table with ISO3, ISO2, M49 coes and country names. 
# UNSTAT M49 Area codes are available here https://unstats.un.org/unsd/methodology/m49/
country_codes <- read.csv("Country Codes (ISO3-ISO2-M49).csv", stringsAsFactors = FALSE)

# coi is "countries of interest"
# c is a powerful function, which combines arguments in a vector 
coi <- c("ALB", "BIH", "MKD", "MNE", "SRB", "XKX", "TUR",
         "ARM", "AZE", "BLR", "GEO", "MDA", "UKR", 
         "KAZ", "KGZ", "TJK", "TKM", "UZB")

# use unique function to check unique country codes in both datasets 
# dollar sign operator ($) address / exctract a column from a dataset. 
unique(mobility_data$country_region_code)  
unique(stringency_data$CountryCode)  

# Filter ccountries of inetrest from mobility dataset and save it as reagional dataset
mobility_data_reg <- mobility_data %>%
  # use mutate comand to convert date from text to date format 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  # merge with country code table to add three-letter country codes
  left_join(country_codes, by=c("country_region_code" = "ISO2_Code")) %>%
  # filter countries which in the list of countries of interest
  filter(ISO3_Code %in% coi) %>%
  # filter country data -- sub region and matro area fields are blank
  filter(sub_region_1=="" & sub_region_2=="" & metro_area=="") 
# save filtered regional data 
write.csv(mobility_data_reg, file="Global_Mobility_Report RBEC.csv", col.names = TRUE, row.names = FALSE)

stringency_data_reg <- stringency_data %>%
  # filter countries which in the list of countries of interest
  filter(CountryCode %in% coi) %>%
  # use mutate comand to convert date from numeric yyyymmdd format  to date format 
  mutate(Date = as.Date(as.character(Date), format = "%Y%m%d"))
# save filtered regional data 
write.csv(stringency_data_reg, file="OxCGRT_stringency RBEC.csv", col.names = TRUE, row.names = FALSE)

# use unique function to check unique country codes in both regional (filtered) datasets 
unique(mobility_data_reg$country_region_code)  
unique(stringency_data_reg$CountryCode)  

# Regional (filtered) datasets as still in raw fromat, with daily data. 
# For chart we need to aggregate these data on a weekly basis
mobility_data_wk <- mobility_data_reg %>%
  # use mutate to generate WoY Week of the Year column. It uses function strftime very handy for converting dates 
  # into strings using formats. Check help by typing ?strftime in console
  mutate(WoY = strftime(date, "%W")) %>%
  # group by countries and weeks of the year
  group_by(ISO3_Code, Name, WoY) %>%
  # summarise weekly data using mean function. summarise offers a great range of functions, see dplyr cheetsheet (Help>Cheetsheets)
  summarise(rnr = mean(retail_and_recreation_percent_change_from_baseline),
            gnf = mean(grocery_and_pharmacy_percent_change_from_baseline), 
            prk = mean(parks_percent_change_from_baseline), 
            trn = mean(transit_stations_percent_change_from_baseline),
            wrk = mean(workplaces_percent_change_from_baseline),
            res = mean(residential_percent_change_from_baseline)) %>%
  # ungroup after calculation 
  ungroup() %>%
  # select is used to leave only certain variables in the resulting dataframe, also rearrange variables. 
  select(ISO3_Code, Name, WoY, rnr, gnf, prk, trn, wrk, res) 

# Regional (filtered) datasets as still in raw fromat, with daily data. 
# For chart we need to aggregate these data on a weekly basis
stringency_data_wk <- stringency_data_reg %>%
  # Use only national data
  filter(Jurisdiction == "NAT_TOTAL") %>%
  # use mutate to generate WoY Week of the Year column. It uses function strftime very handy for converting dates 
  # into strings using formats. Check help by typing ?strftime in console
  mutate(WoY = strftime(Date, "%W")) %>%
  # group by countries and weeks of the year
  group_by(CountryCode, CountryName, WoY) %>%
  # summarise weekly data using mean function. summarise offers a great range of functions, see dplyr cheetsheet (Help>Cheetsheets)
  summarise(avgStringency = mean(StringencyIndexForDisplay)) %>%
  # ungroup after calculation 
  ungroup() %>%
  # select is used to leave only certain variables in the resulting dataframe, also rearrange variables. 
  select(CountryCode, CountryName, WoY, avgStringency)

# Create a list of countries for charts 
c4plt <- c("BIH", "MDA")

# for loop walk through all items in c4plt vector and put curent value in variable cntry. all instructions for the llop are in brackets {}
for (cntry in c4plt) {
  # join data on mobility an stringency by country code and week number, filter country for chat  
  plot_data <- inner_join(mobility_data_wk, stringency_data_wk, by=c("ISO3_Code"="CountryCode", "WoY")) %>%
    filter(ISO3_Code==cntry)
  
  # ggplot creates charts. We will deal with them in details during week 3.
  # specify data for the plot
  plt <- ggplot(data = plot_data) + 
    # add column chart for stringency . aes specifies 'aestetics' of plot, i.e. data to be plotted on x and y. fill is color
    geom_col(aes(x = WoY, y = avgStringency), fill = "pink") +
    # add line chart for mobility. it uses wrk -- mobility at workplaces. you could use other places as well
    geom_line(aes(x = WoY, y = wrk), size = 2, color="blue", group = 1) +
    # some theme formatting 
    theme_minimal() +  theme(legend.position="bottom") + 
    # add title with current country name
    ggtitle(cntry)
  
  # show the pplot ]
  print(plt)

}
