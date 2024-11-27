#RESULTS FOR ECOLOGICAL IMPACT ASSESSMENT

#install.packages("ggplot2")
#install.packages("vegan")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("readxl")
#install.packages("betapart")
#install.packages("ggpubr")

library(ggplot2)
library(vegan)
library(tidyverse)
library(dplyr)
library(readxl)
library(betapart)
library(ggpubr)

#field vertebrates 
#loading data: vertebrates, field surveys 

vert_vis <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="vert_vis") #uses function readxl, selects sheet

#unique field vertebrate species at each site 
south_vertspe <- length(unique(vert_vis$scientificName[vert_vis$site == "South"])) #15 unique species
north_vertspe <- length(unique(vert_vis$scientificName[vert_vis$site == "North"])) #20 unique species  

#plotting field vertebrate species per site 
plot_data <- data.frame(
  Site = c("South", "North"),
  Uniquespecies = c(south_vertspe, north_vertspe)
)

ggplot(plot_data, aes(x = Site, y = Uniquespecies, fill = Site)) + #uses ggplot2 package 
  geom_bar(stat = "identity", show.legend = FALSE, width=0.4) + # stat = "identity" uses the provided y-values
  labs(title = "Vertebrate species richness per site - field survey") + 
       ylab("Number of species") + 
       xlab("Site") +
  theme_grey() +
  scale_fill_manual(values = c("lightblue3", "orange2")) #colour blind friendly colours


#beta diversity of field verts 
#creating a presence-absence matrix for vertebrate field data 
#use dplyr package for summarise function 
beta_vert_vis <- vert_vis[,c("scientificName","site")] 
beta_vert_vis$presence <- 1 
beta_vert_vis <- beta_vert_vis %>%
  group_by(site, scientificName) %>%
  summarise(presence=sum(presence), .groups = "drop")

beta_vert_pa <- beta_vert_vis %>% 
  pivot_wider(names_from=scientificName,values_from=c(presence))
list0 <- as.list(rep(0,ncol(beta_vert_pa)))
names(list0) <- names(beta_vert_pa)
beta_vert_pa <- as.data.frame(beta_vert_pa %>% replace_na(list0))
row.names(beta_vert_pa) <- beta_vert_pa$site
beta_vert_pa <- beta_vert_pa[,-1]
beta_vert_pa[beta_vert_pa > 0] <- 1

#calculating beta diversity- sorensen
#use betapart package
vert_vis_sor <- beta.pair(beta_vert_pa, index.family="sorensen")
mean(vert_vis_sor$beta.sor) #0.44

#calculating beta diversity- jaccard
vert_vis_jac <- beta.pair(beta_vert_pa, index.family="jaccard")
mean(vert_vis_jac$beta.jac) #0.61



#looading data: vertebrates, tech surveys: audiomoth

vert_aud <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="vert_aud")

#unique tech vertebrate species at each site 
south_vertaud <- length(unique(vert_aud$scientificName[vert_aud$site == "South"])) #3 unique species
north_vertaud <- length(unique(vert_aud$scientificName[vert_aud$site == "North"])) #5 unique species  

#plotting tech vertebrate species per site 
plot_dataaud <- data.frame(
  Site = c("South", "North"),
  Uniquespecies = c(south_vertaud, north_vertaud)
)

ggplot(plot_dataaud, aes(x = Site, y = Uniquespecies, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE, width=0.4) + # stat = "identity" uses the provided y-values
  labs(title = "Vertebrate species richness per site - audio survey") + 
  ylab("Number of species") + 
  xlab("Site") +
  theme_grey() +
  scale_fill_manual(values = c("lightblue3", "orange2")) #colour blind friendly colours

#beta diversity of tech verts 
#creating a presence-absence matrix for vertebrate tech data 
beta_vert_aud <- vert_aud[,c("scientificName","site")] 
beta_vert_aud$presence <- 1 
beta_vert_aud <- beta_vert_aud %>%
  group_by(site, scientificName) %>%
  summarise(presence=sum(presence), .groups = "drop")

beta_aud_pa <- beta_vert_aud %>% 
  pivot_wider(names_from=scientificName,values_from=c(presence))
list0 <- as.list(rep(0,ncol(beta_aud_pa)))
names(list0) <- names(beta_aud_pa)
beta_aud_pa <- as.data.frame(beta_aud_pa %>% replace_na(list0))
row.names(beta_aud_pa) <- beta_aud_pa$site
beta_aud_pa <- beta_aud_pa[,-1]
beta_aud_pa[beta_aud_pa > 0] <- 1

#calculating beta diversity- sorensen
vert_aud_sor <- beta.pair(beta_aud_pa, index.family="sorensen")
mean(vert_aud_sor$beta.sor) #0.25

#calculating beta diversity- jaccard
vert_aud_jac <- beta.pair(beta_aud_pa, index.family="jaccard")
mean(vert_aud_jac$beta.jac) #0.4

#combining figures 
#use ggarrange from ggpubr package 
fieldplot <- ggplot(plot_data, aes(x = Site, y = Uniquespecies, fill = Site)) + #make each plot an object 
  geom_bar(stat = "identity", show.legend = FALSE, width=0.4) + # stat = "identity" uses the provided y-values
  labs(title = "Vertebrate species richness per site - field survey") + 
  ylab("Number of species") + 
  xlab("Site") +
  theme_grey() +
  scale_fill_manual(values = c("lightblue3", "orange2")) 

audplot <- ggplot(plot_dataaud, aes(x = Site, y = Uniquespecies, fill = Site)) + #make each plot an object 
  geom_bar(stat = "identity", show.legend = FALSE, width=0.4) + # stat = "identity" uses the provided y-values
  labs(title = "Vertebrate species richness per site - audio survey") + 
  ylab("Number of species") + 
  xlab("Site") +
  theme_grey() +
  scale_fill_manual(values = c("lightblue3", "orange2")) #colour blind friendly colours

vertfigure <- figure <- ggarrange(fieldplot, audplot, #again make an object 
                                  labels = c("A", "B"), #label the figures with a and b 
                                  ncol = 1, nrow = 2) #one column, two rows, so the figures are stacked 
vertfigure #shows the figure 

#all vertebrates in combination
#loading data
verts <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="vertebrates")

#extracting necessary columns for my own ease of analysis 
verts_subset <- verts %>%
  select(site, scientificName) #here, i only want to analyse these three variables; select from dyplr package allows this  

#removing n/a values
vert_clean <- verts_subset %>%
 filter(!is.na(scientificName)) #remove na from scientificName

#unique vertebrate species at each site 
south_verts <- length(unique(vert_clean$scientificName[vert_clean$site == "South"])) #19 unique species
north_verts <- length(unique(vert_clean$scientificName[vert_clean$site == "North"])) #25 unique species

#plotting vertebrate species per site 
plot_dataverts <- data.frame(
  site = c("South", "North"),
  species = c(south_verts, north_verts)
)

ggplot(plot_dataverts, aes(x = site, y = species, fill = site)) +
  geom_bar(stat = "identity", show.legend = FALSE, width=0.4) + # stat = "identity" uses the provided y-values
  labs(title = "Vertebrate species richness per site") + 
  ylab("Number of species") + 
  xlab("Site") +
  theme_grey() +
  scale_fill_manual(values = c("lightblue3", "orange2")) #colour blind friendly colours

#beta diversity of all verts 
#creating a presence-absence matrix for vertebrate  data 
beta_vert <- vert_clean[,c("scientificName","site")] 
beta_vert$presence <- 1 
beta_vert <- beta_vert %>%
  group_by(site, scientificName) %>%
  summarise(presence=sum(presence), .groups = "drop")

beta_all_pa <- beta_vert %>% 
  pivot_wider(names_from=scientificName,values_from=c(presence))
list0 <- as.list(rep(0,ncol(beta_all_pa)))
names(list0) <- names(beta_all_pa)
beta_all_pa <- as.data.frame(beta_all_pa %>% replace_na(list0))
row.names(beta_all_pa) <- beta_all_pa$site
beta_all_pa <- beta_all_pa[,-1]
beta_all_pa[beta_all_pa > 0] <- 1

#calculating beta diversity- sorensen
vert_all_sor <- beta.pair(beta_all_pa, index.family="sorensen")
mean(vert_all_sor$beta.sor) #0.36

#calculating beta diversity- jaccard
vert_all_jac <- beta.pair(beta_all_pa, index.family="jaccard")
mean(vert_all_jac$beta.jac) #0.53



#terrestrial invertebrates
#loading data: invertebrates, terrestrial

inverts_t <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="Inverts_t") #extracting the sheet with terrestrial invert data from whole excel book

#extracting necessary columns for my own ease of analysis 
inverts_subset <- inverts_t %>%
  select(site, order, individualCount) #here, i only want to analyse these three variables; select from dyplr package allows this  

#removing n/a values
invert_clean <- inverts_subset %>%
  filter(!is.na(individualCount) & !is.na(order)) #remove na from both individualCount and order columns

#unique terrestrial invertebrate orders at each site 
south_invertord <- length(unique(invert_clean$order[invert_clean$site == "South"])) #9 unique orders
north_invertord <- length(unique(invert_clean$order[invert_clean$site == "North"])) #8 unique orders 

#relative abundance of each order
#barplot of relative abundances for each order
ggplot(invert_clean, aes(x = order, y = individualCount, fill = site)) + #using invert_clean subset
  geom_bar(stat="identity", position = "stack") +  #identity forces geom_bar to plot individualCount, stack for stacked bars - i felt this was easier to visualise for this particular data rather than side by side bars 
  theme_grey() + xlab("Order") + ylab("Abundance (count)") + #grey theme, x and y axis labels 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + #x labels angled to make legible 
  scale_fill_manual(values = c("lightblue3", "orange2")) + #colour blind friendly 
  labs(fill="Site", title="Relative abundances of terrestrial invertebrate orders between sites") #changing legend title, adding graph title

#shannon index for terrestrial invertebrate orders 
#uses functions from vegan package 

#reshaping the data so each site is a row(wide data), for ease of calculation
library(tidyr) #tidyr for reshaping data frame, used help page for this package to determine this code along w practical 7 p1
invertshan <- invert_clean %>%
  pivot_wider( #reshapes the data by creating new columns 
    names_from = order, #makes order into column names 
    values_from = individualCount, #makes individual count values into data for each column 
    values_fill = list(individualCount = 0), #makes na values into 0 
    values_fn = list(individualCount = sum) #adds up individualCount for each order (have doubled checked calculating by hand, is correct)
  )

#calculating shannon diversity index
#method combined knowledge from undergrad + help from SQLPad blog "What is Shannon Diversity Index and How to Calculate It in R", 2024  
shannon_div_in <- diversity(invertshan[, -1], index = "shannon") # -1 removes site column 
                                                                #diversity() function is from vegan, specify index ("shannon") 

shannon_results_in <- data.frame( #dataframe to match the site with the shannon result, for ease of analysis
  site = invertshan$site,  
  shannon_result = shannon_div_in  
) 

print(shannon_results_in)

#beta diversity of terrestrial inverts 
#creating a presence-absence matrix 
invert_t_beta <- invert_clean[,c("order","site")] 
invert_t_beta$presence <- 1 
invert_t_beta <- invert_t_beta %>%
  group_by(site, order) %>%
  summarise(presence=sum(presence), .groups = "drop")

invert_t_pa <- invert_t_beta %>% 
  pivot_wider(names_from=order,values_from=c(presence))
list0 <- as.list(rep(0,ncol(invert_t_pa)))
names(list0) <- names(invert_t_pa)
invert_t_pa <- as.data.frame(invert_t_pa %>% replace_na(list0))
row.names(invert_t_pa) <- invert_t_pa$site
invert_t_pa <- invert_t_pa[,-1]
invert_t_pa[invert_t_pa > 0] <- 1

#calculating beta diversity- sorensen
invert_t_sor <- beta.pair(invert_t_pa, index.family="sorensen")
mean(invert_t_sor$beta.sor) #0.29

#calculating beta diversity- jaccard
invert_t_jac <- beta.pair(invert_t_pa, index.family="jaccard")
mean(invert_t_jac$beta.jac) #0.45



#loading data: invertebrates, aquatic
inverts_a <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="Inverts_a")

#extracting necessary columns
invertsa_subset <- inverts_a %>%
  select(site, order, individualCount)

#removing n/a values
inverta_clean <- invertsa_subset %>%
  filter(!is.na(individualCount) & !is.na(order))

#unique aquatic invertebrate orders at each site 
south_invertorda <- length(unique(inverta_clean$order[inverta_clean$site == "South"])) #12 unique orders
north_invertorda <- length(unique(inverta_clean$order[inverta_clean$site == "North"])) #13 unique orders 

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

#reshaping the data so each site is a row, for ease of calculation
inverta_shan <- inverta_clean %>%
  pivot_wider(
    names_from = order,
    values_from = individualCount,
    values_fill = list(individualCount = 0),
    values_fn = list(individualCount = sum)
  )

#calculating shannon diversity index
shannon_div_ina <- diversity(inverta_shan[, -1], index = "shannon") #-1 removes site column 

shannon_results_ina <- data.frame(
  site = inverta_shan$site,  
  shannon_result = shannon_div_ina
)

print(shannon_results_ina)

#beta diversity of aquatic inverts 
#creating a presence-absence matrix
invert_a_beta <- inverta_clean[,c("order","site")] 
invert_a_beta$presence <- 1 
invert_a_beta <- invert_a_beta %>%
  group_by(site, order) %>%
  summarise(presence=sum(presence), .groups = "drop")

invert_a_pa <- invert_a_beta %>% 
  pivot_wider(names_from=order,values_from=c(presence))
list0 <- as.list(rep(0,ncol(invert_a_pa)))
names(list0) <- names(invert_a_pa)
invert_a_pa <- as.data.frame(invert_a_pa %>% replace_na(list0))
row.names(invert_a_pa) <- invert_a_pa$site
invert_a_pa <- invert_a_pa[,-1]
invert_a_pa[invert_a_pa > 0] <- 1

#calculating beta diversity- sorensen
invert_a_sor <- beta.pair(invert_a_pa, index.family="sorensen")
mean(invert_a_sor$beta.sor) #0.2

#calculating beta diversity- jaccard
invert_a_jac <- beta.pair(invert_a_pa, index.family="jaccard")
mean(invert_a_jac$beta.jac) #0.33


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




