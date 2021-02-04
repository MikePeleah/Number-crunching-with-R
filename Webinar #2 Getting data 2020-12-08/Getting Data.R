#################################################################################################
#
# Code for peer learning Webinar 8 December 2020 
#
#################################################################################################

# Load threelibraries
#   dplyr for data manipualtions
#   ggplot2 for charts 
#   jsonlite for geteting and manipulating JSON data  
# if necessary, install these packages through menu Tools > Install Packages...

library(dplyr)
library(ggplot2)
library(jsonlite)

#----------------------------------------------------------------------------------------------------------
# Example of COMTRADE
#----------------------------------------------------------------------------------------------------------
# Get Export data for Kazakhstan, by 2 digit codes
# You could get API parameters through the data query page https://comtrade.un.org/data/ > Get Data > View API call (at the bootom of the page)
# See COMTRADE API documentation here https://comtrade.un.org/Data/Doc/API
# 
url = "http://comtrade.un.org/api/get?max=500&type=C&freq=A&px=HS&ps=2019&r=398&p=0&rg=2&cc=AG2&fmt=csv"
KAZ_export = read.csv(url, stringsAsFactors = FALSE)

# Manipulate data for chart 
chart_data <- KAZ_export %>%
  # * Calculate export in mil USD
  mutate(Trade.Value = Trade.Value..US../1e6) %>%
  # * Select top-10 export top_n()
  top_n(10) %>%
  # * Sort by trade value 
  arrange(-Trade.Value) %>%
  # * Keep only necessary fields 
  select(Year, Reporter, Trade.Flow, Trade.Value, Commodity.Code, Commodity)

# Construct a bar chart 
plt <- ggplot(data = chart_data) + 
  # Bar chart. reorder is used to arrange columns and labels in decreasing order of trade value: -Trade.Value. stat="identity" means use values, rather than counts
  geom_bar(aes(x=reorder(Commodity.Code, -Trade.Value) , y=Trade.Value, fill=reorder(Commodity, -Trade.Value)), stat="identity") + 
  # Add chert title by combining Reporter, Trade.Flow, and Year from dataset
  ggtitle(paste(KAZ_export$Reporter[1], KAZ_export$Trade.Flow[1], KAZ_export$Year[1], "Top 10", sep=", ")) + 
  # Add labels to axis and legend 
  ylab('Export, mln. USD') + xlab('Commodity (2 dight group)') + labs(fill = "Commodity (2 dight group)") +
  # Use minimal theme 
  theme_minimal()
plt

  
#----------------------------------------------------------------------------------------------------------
# Example of the WorldBank WDI Data
#----------------------------------------------------------------------------------------------------------
# Get remittances data as $ and %GDP 
# See WorldBank WDI API documentation here https://datahelpdesk.worldbank.org/knowledgebase/articles/898581-api-basic-call-structures
# Countries of interest: Moldova, Kyrgyzstan and Tajikistan
coi <- c("MDA", "KGZ", "TJK")
# indicators of interest. You could get series codes through WDI datapage or using Metadata API http://api.worldbank.org/v2/sources/2/search/remittances%20received?format=json 
ioi <- c("BX.TRF.PWKR.CD.DT", "BX.TRF.PWKR.DT.GD.ZS")

# Create empty dataframe 
wdi_data = data.frame()
# loop through countries and indicator
for (country in coi) {
  for (indicator in ioi) {
    # Generate url to access API
    url = paste("http://api.worldbank.org/v2/country/",country,"/indicator/",indicator,"?date=2000:2020&format=json&per_page=9999", sep="")
    print(paste0("Getting data from ", url))
    # getting data and converting from json using fromJSON function. flatten = TRUE is used to flatten the resulting set
    # what we get is list of two elements, first is technical part, like page, number of pages, etc. Second is a dataframe with requested data 
    json_data <- fromJSON(url, flatten = TRUE)
    # exctracting dataframe from the list using [[]]. Keep in mind that R counts from 1, hence [[2]] extracting second elemnt of the list
    temp_data <- as.data.frame(json_data[[2]], row.names = NULL)
    # add extracted data to the full dataset. rbind function bind rows from one dataset top rows from another dataset 
    wdi_data <- rbind(wdi_data, temp_data)
  }
}

# prepare data fro chart -- select Remittencase as %GDP series and convert year to numeric 
chart_data <- wdi_data %>%
  filter(indicator.id == "BX.TRF.PWKR.DT.GD.ZS") %>%
  mutate(year = as.integer(date))

# Construct a line chart 
plt <- ggplot(data = chart_data) +
  # Line chart x for Years, y for values, coloring by country
  geom_line(aes(x = year, y = value, color = country.value), size = 2) +
  # Add chart title--name of the series
  ggtitle(chart_data$indicator.value[1]) + 
  # add axis and legend labels
  ylab('Year') + xlab(chart_data$indicator.value[1]) + labs(color = "Country") +
  # minimal theme 
  theme_minimal()
plt
