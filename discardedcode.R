#loading data: bog invertebrates
#analysed separately because unique environment 

#bog <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="BOG")

#extracting necessary columns; want to separate by method rather than site here, because want to analyse methods separately 

#bog_subset <- bog %>%
select(samplingProtocol, order, individualCount)

#removing n/a values
bog_clean <- bog_subset %>%
  filter(!is.na(individualCount) & !is.na(order))

#reshaping the data so each method is a row, for ease of calculation
library(tidyr)
bogshan <- bog_clean %>%
  pivot_wider(
    names_from = order,
    values_from = individualCount,
    values_fill = list(individualCount = 0),
    values_fn = list(individualCount = sum)
  )

#shannon index for bog 

shannon_div_bog <- diversity(bogshan[, -1], index = "shannon") #-1 removes site column 

shannon_results_bog <- data.frame(
  samplingProtocol = bogshan$samplingProtocol,  
  shannon_result = shannon_div_bog
)

print(shannon_results_bog)

#graphs for each method
ggplot(bog_clean, aes(x = order, y = individualCount, fill = order)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ samplingProtocol, 
             ncol = 1, labeller = as_labeller(c( #labeller is a function used in facet wrap to change the titles of individual grids- help with this from DataNovie blog "HOW TO CHANGE GGPLOT FACET LABELS" 
               "Kick net sampling" = "Aquatic invertebrates",
               "Sweep netting" = "Terrestrial invertebrates"
             ))) +
  labs(title = "Abundance of terrestrial and aquatic insect orders in bog") +
  xlab("Insect Order") +
  ylab("Count") +
  scale_fill_manual(values = c(
    "Araneae" = "lightblue3",
    "Diptera" = "orchid3",
    "Hemiptera" = "orange2",
    "Odonata" = "darkgreen")) +  #colourblind friendly palette
  theme_grey()





#plotting terrestrial invertebrate orders per site
library(ggplot2)
plot_datain <- data.frame( #making a data frame so i can plot the two above objects against each other
  site = c("South", "North"), #creates a site column
  UniqueOrders = c(south_invertord, north_invertord) #creates a unique orders column
)

ggplot(plot_datain, aes(x = site, y = UniqueOrders, fill = site)) + #using dataframe just created above 
  geom_bar(stat = "identity", show.legend = FALSE, color = "black", width=0.5) + # stat = "identity" uses the provided y-values
  labs(title = "Number of terrestrial invertebrate orders per site") + 
  ylab("Number of orders present") +
  xlab("Site") + #axis titles 
  theme_grey() + #grey theme
  scale_fill_manual(values = c("lightblue3", "orange2")) #colour blind friendly colours for each bar 





#plotting aquatic invertebrate orders per site 
plot_datain_a <- data.frame(
  Site = c("South", "North"),
  UniqueOrders = c(south_invertorda, north_invertorda)
)

ggplot(plot_datain_a, aes(x = Site, y = UniqueOrders, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE, color = "black", width=0.5) + # stat = "identity" uses the provided y-values
  labs(title = "Number of aquatic invertebrate orders per site") +
  ylab("Number of orders") +
  xlab("Site") +
  theme_grey() +
  scale_fill_manual(values = c("lightblue3", "orange2")) #colour blind friendly colours

