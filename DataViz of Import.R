bigdata_1<-read_csv("Trade_DetailedTradeMatrix_E_All_Data.csv")

#code to format the display
library(pander)
panderOptions("table.alignment.default", function(df) ifelse(sapply(df, is.numeric),"right", "left"))
panderOptions("table.split.table", Inf)
panderOptions("keep.trailing.zeros", TRUE)

#Code to filter the dataset for wheat and the year 2011
bigdata_2<-bigdata_1%>% filter(Item %in% c("Wheat"),Element=="Import Quantity") %>% select(-grep(c("F$"),names(bigdata_1),perl=T,value=F))
bigdata_3<-bigdata_2%>% filter(Unit %in% c("tonnes")) %>% select(-(Y1986:Y2010))
big_data4<-bigdata_3%>%select(-(Y2012:Y2013))
big_data5<-big_data4%>%filter(Y2011 !="NA")
big_data6<-select(big_data5,`Reporter Country Code`,`Partner Country Code`,`Reporter Countries`,
`Partner Countries`,Y2011)
head(big_data6)%>%pander()
#code to homogenize the country codes 
big_data6$countryin<-countrycode(big_data6$`Reporter Countries`,"country.name","iso3c")
big_data6$countryout<-countrycode(big_data6$`Partner Countries`,"country.name","iso3c")
dim(big_data6)
wheat<-big_data6 %>% select(countryin,countryout,Y2011)
head(wheat)%>%pander()

#code to get the world bank data on regions,income levels
getWorldBankCountries <- function() {
  require(RJSONIO)
  wbCountries <- fromJSON("http://api.worldbank.org/countries?per_page=12000&format=json")
  wbCountries <- data.frame(t(sapply(wbCountries[[2]], unlist)))
  wbCountries$longitude <- as.numeric(wbCountries$longitude)
  wbCountries$latitude <- as.numeric(wbCountries$latitude)
  levels(wbCountries$region.value) <- gsub(" \\(all income levels\\)", "", 
                                           levels(wbCountries$region.value))
  return(wbCountries)
}
wbCountries <- getWorldBankCountries()
wbCountries %<>% select(-grep("lend", names(wbCountries)))

#Merging the 2 data sets to compute the flow matrix between the regions 
wheat.mer<- wheat%>%left_join(wbCountries[c("id","region.value","incomeLevel.id")], by=c(countryin="id"))


wheat.mer<- wheat.mer%>%left_join(wbCountries[c("id","region.value","incomeLevel.id")], by=c(countryout="id"))

head(wheat.mer,10)%>%pander()
setdiff(unique(wheat.mer$countryout), wbCountries$id)
dim(wheat.mer)

wheat.reg <-wheat.mer %>% select(Y2011,incomeLevel.id.x ,incomeLevel.id.y ) %>% 
group_by(incomeLevel.id.x,incomeLevel.id.y) %>% summarise(trade = sum(Y2011))


wheat.reg$region.value.x<-droplevels(wheat.reg$region.value.x)
wheat.reg$region.value.y<-droplevels(wheat.reg$region.value.y)
wheat.reg<-na.exclude(wheat.reg)
wheat.reg %>% data.frame() %>% head(15) %>% pander()

wheat.reg %>% data.frame() %>% spread(region.value.y, trade) %>%pander()
#code to plot the network graph
library(igraph)
wheat_g <- graph.data.frame(wheat.reg, directed = TRUE)
E(wheat_g)$width <- E(wheat_g)$trade/2055701
plot(wheat_g)
