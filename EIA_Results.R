#RESULTS FOR ECOLOGICAL IMPACT ASSESSMENT

#loading data: vertebrates, field surveys 
#install.packages("readxl")
library(readxl)

vert_vis <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="vert_vis")

#unique vertebrate species at each site 
south_vertspe <- length(unique(vert_vis$scientificName[vert_vis$site == "South"])) #15 unique species
north_vertspe <- length(unique(vert_vis$scientificName[vert_vis$site == "North"])) #20 unique species  

#plotting vertebrate species per site 
library(ggplot2)

plot_data <- data.frame(
  Site = c("South", "North"),
  Uniquespecies = c(south_vertspe, north_vertspe)
)

ggplot(plot_data, aes(x = Site, y = Uniquespecies, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE, color = "black", width=0.5) + # stat = "identity" uses the provided y-values
  labs(title = "Number of vertebrate species per site from field survey") + 
       ylab("Number of species") + 
       xlab("Site") +
  theme_grey() +
  scale_fill_manual(values = c("lightblue3", "orange2")) #colour blind friendly colours

#loading data: vertebrates, tech surveys: audiomoth

vert_aud <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="vert_aud")

#unique vertebrate species at each site 
south_vertaud <- length(unique(vert_aud$scientificName[vert_aud$site == "South"])) #3 unique species
north_vertaud <- length(unique(vert_aud$scientificName[vert_aud$site == "North"])) #5 unique species  

#plotting vertebrate species per site 
library(ggplot2)

plot_dataaud <- data.frame(
  Site = c("South", "North"),
  Uniquespecies = c(south_vertaud, north_vertaud)
)

ggplot(plot_dataaud, aes(x = Site, y = Uniquespecies, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE, color = "black", width=0.5) + # stat = "identity" uses the provided y-values
  labs(title = "Number of vertebrate species per site from audio survey") + 
  ylab("Number of species") + 
  xlab("Site") +
  theme_grey() +
  scale_fill_manual(values = c("lightblue3", "orange2")) #colour blind friendly colours

#loading data: invertebrates, terrestrial

inverts_t <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="Inverts_t") #extracting the sheet with terrestrial invert data from whole excel book

#extracting necessary columns for my own ease of analysis 
library(dplyr) #package allowing subset with function "select"
inverts_subset <- inverts_t %>%
  select(site, order, individualCount) #here, i only want to analyse these three variables 

#removing n/a values
invert_clean <- inverts_subset %>%
  filter(!is.na(individualCount) & !is.na(order)) #remove na from both individualCount and order columns

#unique terrestrial invertebrate orders at each site 
south_invertord <- length(unique(invert_clean$order[invert_clean$site == "South"])) #9 unique orders
north_invertord <- length(unique(invert_clean$order[invert_clean$site == "North"])) #8 unique orders 

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

#relative abundance of each order
#barplot of relative abundances for each order
ggplot(invert_clean, aes(x = order, y = individualCount, fill = site)) + #using invert_clean subset
  geom_bar(stat="identity", position = "stack") +  #identity forces geom_bar to plot individualCount, stack for stacked bars - i felt this was easier to visualise for this particular data rather than side by side bars 
  theme_grey() + xlab("Order") + ylab("Abundance (count)") + #grey theme, x and y axis labels 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + #x labels angled to make legible 
  scale_fill_manual(values = c("lightblue3", "orange2")) + #colour blind friendly 
  labs(fill="Site", title="Relative abundances of terrestrial invertebrate orders between sites") #changing legend title, adding graph title

#shannon index for terrestrial invertebrate orders 
#install.packages("vegan")
library(vegan)

#reshaping the data so each site is a row(wide data), for ease of calculation
library(tidyr) #package for reshaping data frame, used help page for this package to determine this code along w practical 7 p1
invertshan <- invert_clean %>%
  pivot_wider( #reshapes the data by creating new columns 
    names_from = order, #makes order into column names 
    values_from = individualCount, #makes individual count values into data for each column 
    values_fill = list(individualCount = 0), #makes na values into 0 
    values_fn = list(individualCount = sum) #adds up individualCount for each order (have doubled checked calculating by hand, is correct)
  )

#calculating shannon diversity index
#method combined knowledge from undergrad + help from SQLPad blog "What is Shannon Diversity Index and How to Calculate It in R", 2024  
library(vegan)
shannon_div_in <- diversity(invertshan[, -1], index = "shannon") # -1 removes site column 
                                                                #diversity() function is from vegan, specify index ("shannon") 

shannon_results_in <- data.frame( #dataframe to match the site with the shannon result, for ease of analysis
  site = invertshan$site,  
  shannon_result = shannon_div_in  
) 

print(shannon_results_in)

#loading data: invertebrates, aquatic

inverts_a <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="Inverts_a")

#extracting necessary columns
invertsa_subset <- inverts_a %>%
  select(site, order, individualCount)

#removing n/a values
inverta_clean <- invertsa_subset %>%
  filter(!is.na(individualCount) & !is.na(order))

#unique aquatic invertebrate orders at each site 
south_invertorda <- length(unique(inverta_clean$order[inverta_clean$site == "South"])) #9 unique orders
north_invertorda <- length(unique(inverta_clean$order[inverta_clean$site == "North"])) #13 unique orders 

#plotting aquatic invertebrate orders per site 
plot_dataina <- data.frame(
  Site = c("South", "North"),
  UniqueOrders = c(south_invertorda, north_invertorda)
)

ggplot(plot_dataina, aes(x = Site, y = UniqueOrders, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE, color = "black", width=0.5) + # stat = "identity" uses the provided y-values
  labs(title = "Number of aquatic invertebrate orders per site") +
       ylab("Number of orders") +
       xlab("Site") +
  theme_grey() +
  scale_fill_manual(values = c("lightblue3", "orange2")) #colour blind friendly colours

#relative abundance of each order
#barplot of relative abundances for each order
ggplot(inverta_clean, aes(x = order, y = individualCount, fill = site)) + 
  geom_bar(stat = "identity", position = "stack") +  #stack for stacked bars
  theme_grey() + xlab("Order") + ylab("Abundance (count)") + #grey theme, x and y axis labels 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + #x labels angled to make legible 
  scale_fill_manual(values = c("lightblue3", "orange2")) + #colour blind friendly 
  labs(fill="Site", title="Relative abundances of aquatic invertebrate orders between sites") #changing legend title, adding graph title

#shannon index for aquatic invertebrate orders 
#install.packages("vegan")
library(vegan)

#reshaping the data so each site is a row, for ease of calculation
library(tidyr)
inverta_shan <- inverta_clean %>%
  pivot_wider(
    names_from = order,
    values_from = individualCount,
    values_fill = list(individualCount = 0),
    values_fn = list(individualCount = sum)
  )

#calculating shannon diversity index
library(vegan)
shannon_div_ina <- diversity(inverta_shan[, -1], index = "shannon") #-1 removes site column 

shannon_results_ina <- data.frame(
  site = inverta_shan$site,  
  shannon_result = shannon_div_ina
)

print(shannon_results_ina)

#loading data: bog invertebrates
#analysed separately because unique environment 

bog <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="BOG")

#extracting necessary columns; want to separate by method rather than site here, because want to analyse methods separately 

bog_subset <- bog %>%
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

#comparison of all data collected 
#load data for all 

all <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="All")

#unique orders at each site 
all_orders_s <- length(unique(all$order[all$site == "South"])) #26 unique orders
all_orders_n <- length(unique(all$order[all$site == "North"])) #26 unique orders 

plot_data_all <- data.frame(
  Site = c("South", "North"),
  UniqueOrders = c(all_orders_s, all_orders_n)
)

ggplot(plot_data_all, aes(x = Site, y = UniqueOrders, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE, color = "black", width=0.5) + # stat = "identity" uses the provided y-values
  labs(title = "Total number of orders per site", 
       y = "Number of orders", 
       x = "Site") +
  theme_grey() +
  scale_fill_manual(values = c("lightblue3", "orange2")) #colour blind friendly colours




