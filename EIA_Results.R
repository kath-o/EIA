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
library(ggpubr) #load all packaged necessary to run this code 

#1: field vertebrates 
  #loading data: vertebrates, field surveys 
  
  vert_vis <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="vert_vis") #uses function readexcel from readxl, selects sheet from specified book 
  
  #unique field vertebrate species at each site 
  south_vertspe <- length(unique(vert_vis$scientificName[vert_vis$site == "South"])) #16 unique species
  #vert_vis$scientificName[vert_vis$site] creates a subset of the scientific name column based on site column; =="" separates them into north and south 
  #unique extracts all of the unique scientific names
  #length counts the unique scientific names 
  north_vertspe <- length(unique(vert_vis$scientificName[vert_vis$site == "North"])) #20 unique species  
  
  #plotting field vertebrate species per site 
  plot_data <- data.frame(
    Site = c("South", "North"),
    Uniquespecies = c(south_vertspe, north_vertspe) #i combined the above objects into an object using data.frame, for ease of plotting
    
  )
  
  ggplot(plot_data, aes(x = Site, y = Uniquespecies, fill = Site)) + #uses ggplot2 package 
    geom_bar(stat = "identity", show.legend = FALSE, width=0.4) + # stat = "identity" uses the provided y-values; i prefferred a thinner bar so set width to 0.4
    labs(title = "Vertebrate species richness per site - field survey") + #graph title 
         ylab("Number of species") + #x axis title
         xlab("Site") + #y axis title 
    theme_grey() + #sets a gret background; i find this easier on the eye 
    scale_fill_manual(values = c("lightblue3", "orange2")) #colour blind friendly colours
  
  
  #beta diversity of field verts 
  #creating a presence-absence matrix for vertebrate field data 
  #use dplyr package for summarise function 
  beta_vert_vis <- vert_vis[,c("scientificName","site")] #creating a new data frame only containing scientificName and site
  beta_vert_vis$presence <- 1 #create a new column called "presence" and set all values within this coumn to 1
  beta_vert_vis <- beta_vert_vis %>%
    group_by(site, scientificName) %>% #groups of all combinations of scientificName and site
    summarise(presence=sum(presence), .groups = "drop") #summarise create  an output of each species name, the site, and in the presence column how many times that species name appears in that site; .groups="drop" ungroups the data, so they don't affect further analysis 
  
  beta_vert_pa <- beta_vert_vis %>% 
    pivot_wider(names_from=scientificName,values_from=c(presence)) #pivoting the data to a wide format(species names as columns and presence absence filling the data)
  list0 <- as.list(rep(0,ncol(beta_vert_pa))) 
  names(list0) <- names(beta_vert_pa) 
  beta_vert_pa <- as.data.frame(beta_vert_pa %>% replace_na(list0)) #these three lines replaces any na values, i.e if a species is not present on one of the sites, with a 0
  row.names(beta_vert_pa) <- beta_vert_pa$site #sets the row names to the site
  beta_vert_pa <- beta_vert_pa[,-1] #removes site column so all data is numeric 
  beta_vert_pa[beta_vert_pa > 0] <- 1 #since this is presence-absence data, we don't want to know how many times a species was present on a site, just whether it was or not: this changes any number above 0 to 1, because any number above xero indictaes presence 
  #for example, Anthus pratensis was present 2 times on the south site: 2 is changed to 1, because we only want to know that it was present, and no more 
  
  #calculating beta diversity- sorensen
  #use beta.pair from betapart package, and specify the index wanted with index.family=
  vert_vis_sor <- beta.pair(beta_vert_pa, index.family="sorensen")
  mean(vert_vis_sor$beta.sor) #0.44
  #three indices are output, but i only want sorensen; hence $beta.sor; mean averages beta.sor values for final value
  
  #calculating beta diversity- jaccard
  vert_vis_jac <- beta.pair(beta_vert_pa, index.family="jaccard")
  mean(vert_vis_jac$beta.jac) #0.61
  #same method as above for different index 


#2. tech vertebrates
  #loading data: vertebrates, tech surveys: audiomoth
  
  vert_aud <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="vert_aud")
  
  #unique tech vertebrate species at each site: same method as used previously 
  south_vertaud <- length(unique(vert_aud$scientificName[vert_aud$site == "South"])) #3 unique species
  north_vertaud <- length(unique(vert_aud$scientificName[vert_aud$site == "North"])) #5 unique species  
  
  #plotting tech vertebrate species per site: same method as used previously 
  plot_dataaud <- data.frame(
    Site = c("South", "North"),
    Uniquespecies = c(south_vertaud, north_vertaud)
  )
  
  ggplot(plot_dataaud, aes(x = Site, y = Uniquespecies, fill = Site)) +
    geom_bar(stat = "identity", show.legend = FALSE, width=0.4) + 
    labs(title = "Vertebrate species richness per site - audio survey") + 
    ylab("Number of species") + 
    xlab("Site") +
    theme_grey() +
    scale_fill_manual(values = c("lightblue3", "orange2")) #colour blind friendly colours
  #all of this code is the same as described above for the vert field data, other than changing data input 
  
  #beta diversity of tech verts: same method as used previously
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
    geom_bar(stat = "identity", show.legend = FALSE, width=0.4) + 
    labs(title = "Vertebrate species richness per site - field survey") + 
    ylab("Number of species") + 
    xlab("Site") +
    theme_grey() +
    scale_fill_manual(values = c("lightblue3", "orange2")) 
  
  audplot <- ggplot(plot_dataaud, aes(x = Site, y = Uniquespecies, fill = Site)) + #make each plot an object 
    geom_bar(stat = "identity", show.legend = FALSE, width=0.4) + 
    labs(title = "Vertebrate species richness per site - audio survey") + 
    ylab("Number of species") + 
    xlab("Site") +
    theme_grey() +
    scale_fill_manual(values = c("lightblue3", "orange2")) #colour blind friendly colours
  
  vertfigure <- figure <- ggarrange(fieldplot, audplot, #again make an object, then combine them with ggarrange package  
                                    labels = c("A", "B"), #label the figures with a and b 
                                    ncol = 1, nrow = 2) #one column, two rows, so the figures are stacked 
  vertfigure #shows the figure 
  
  


#3. all vertebrates in combination
  #loading data
  verts <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="vertebrates")
  
  #extracting necessary columns for my own ease of analysis 
  verts_subset <- verts %>% 
    select(site, scientificName) #here, i only want to analyse these three variables; select() function from dyplr package allows this  
  
  #removing n/a values
  vert_clean <- verts_subset %>%
   filter(!is.na(scientificName))
  #filter() subsets rows that fit into a certain condition; is.na checks for na values; ! reverses this so only NON na values are being subset  
  
  
  #unique vertebrate species at each site: same method as used previously 
  south_verts <- length(unique(vert_clean$scientificName[vert_clean$site == "South"])) #19 unique species
  north_verts <- length(unique(vert_clean$scientificName[vert_clean$site == "North"])) #25 unique species
  
  #plotting vertebrate species per site: same method as used previously
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
  
  
  
  #beta diversity of all verts: same method as used previously
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
  
  
  
#4. terrestrial invertebrates
  #loading data: invertebrates, terrestrial
  
  inverts_t <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="Inverts_t") #extracting the sheet with terrestrial invert data from whole excel book
  
  #extracting necessary columns for my own ease of analysis
  inverts_subset <- inverts_t %>%
    select(site, order, individualCount) #here, i only want to analyse these three variables; select from dyplr package allows this  
  
  #removing n/a values: same method as used previously
  invert_clean <- inverts_subset %>%
    filter(!is.na(individualCount) & !is.na(order)) #remove na from both individualCount and order columns
  
  #unique terrestrial invertebrate orders at each site: same method as used previously
  south_invertord <- length(unique(invert_clean$order[invert_clean$site == "South"])) #9 unique orders
  north_invertord <- length(unique(invert_clean$order[invert_clean$site == "North"])) #8 unique orders 
  
  #relative abundance of each order
  #barplot of relative abundances for each order: same method as used previously
  ggplot(invert_clean, aes(x = order, y = individualCount, fill = site)) + #using invert_clean subset
    geom_bar(stat="identity", position = "stack") +  #identity forces geom_bar to plot individualCount, stack for stacked bars - i felt this was easier to visualise for this particular data rather than side by side bars 
    theme_grey() + xlab("Order") + ylab("Abundance (count)") + #grey theme, x and y axis labels 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + #x labels angled to make legible 
    scale_fill_manual(values = c("lightblue3", "orange2")) + #colour blind friendly 
    labs(fill="Site", title="Relative abundances of terrestrial invertebrate orders between sites") #changing legend title, adding graph title
  
  #shannon index for terrestrial invertebrate orders 
  #uses functions from vegan package 
  #reshaping the data so each site is a row(wide data), for ease of calculation
  #tidyr package functions for reshaping data frame, used help page for this package to determine this code along w practical 7 p1
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
  
  print(shannon_results_in) #shows the results matched with their site 
  
  #beta diversity of terrestrial inverts: same method as used previously
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



#5. aquatic invertebrates
  #loading data: invertebrates, aquatic
  inverts_a <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="Inverts_a")
  
  #extracting necessary columns: same method as used previously
  invertsa_subset <- inverts_a %>%
    select(site, order, individualCount)
  
  #removing n/a values: same method as used previously
  inverta_clean <- invertsa_subset %>%
    filter(!is.na(individualCount) & !is.na(order))
  
  #unique aquatic invertebrate orders at each site: same method as used previously
  south_invertorda <- length(unique(inverta_clean$order[inverta_clean$site == "South"])) #12 unique orders
  north_invertorda <- length(unique(inverta_clean$order[inverta_clean$site == "North"])) #13 unique orders 
  #all code same as has been annoted and desrcibed above 
  
  #relative abundance of each order
  #barplot of relative abundances for each order: same method as used previously
  ggplot(inverta_clean, aes(x = order, y = individualCount, fill = site)) + 
    geom_bar(stat = "identity", position = "stack") +  #stack for stacked bars
    theme_grey() + xlab("Order") + ylab("Abundance (count)") + #grey theme, x and y axis labels 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + #x labels angled to make legible 
    scale_fill_manual(values = c("lightblue3", "orange2")) + #colour blind friendly 
    labs(fill="Site", title="Relative abundances of aquatic invertebrate orders between sites") #changing legend title, adding graph title
  #same code as annoted and used for the terrestrial invertebrate graph   
  
  #shannon index for aquatic invertebrate orders: same method as used previously 
  #reshaping the data so each site is a row, for ease of calculation: done in the same way as was done above for terrestrial invertebrates
  inverta_shan <- inverta_clean %>%
    pivot_wider(
      names_from = order,
      values_from = individualCount,
      values_fill = list(individualCount = 0),
      values_fn = list(individualCount = sum)
    )
  
  #calculating shannon diversity index: same method as used previously
  shannon_div_ina <- diversity(inverta_shan[, -1], index = "shannon") #-1 removes site column 
  
  shannon_results_ina <- data.frame(
    site = inverta_shan$site,  
    shannon_result = shannon_div_ina
  )
  
  print(shannon_results_ina)
  
  #beta diversity of aquatic inverts: same method as used previously
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
  
  
  #6. comparison of all data collected 
  #load data for all 
  
  all <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="All")
  
  #unique orders at each site: same method as used previously
  all_orders_s <- length(unique(all$order[all$site == "South"])) #26 unique orders
  all_orders_n <- length(unique(all$order[all$site == "North"])) #26 unique orders 
  
  plot_data_all <- data.frame(
    Site = c("South", "North"),
    UniqueOrders = c(all_orders_s, all_orders_n)
  )
  
  #because no further analysis was being done, i felt it did not require subsetting as was done for others 
  
  #plot of unique orders at each site: same method as used previously
  ggplot(plot_data_all, aes(x = Site, y = UniqueOrders, fill = Site)) +
    geom_bar(stat = "identity", show.legend = FALSE, width=0.5) + 
         y = "Number of orders", 
         x = "Site") +
    theme_grey() +
    scale_fill_manual(values = c("lightblue3", "orange2")) #colour blind friendly colours
  #code the same as for previous barplots 



#7. bog invertebrates
  #loading data
  bog <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="BOG")
  
  #extracting necessary columns; want to separate by method rather than site here, because want to analyse methods separately: same method as used previously
  bog_subset <- bog %>%
  select(samplingProtocol, order, individualCount)
  
  #removing n/a values: same method as used previously
  bog_clean <- bog_subset %>%
    filter(!is.na(individualCount) & !is.na(order))
  
  #reshaping the data so each method is a row, for ease of calculation: same method as used previously
  bogshan <- bog_clean %>%
    pivot_wider(
      names_from = order,
      values_from = individualCount,
      values_fill = list(individualCount = 0),
      values_fn = list(individualCount = sum)
    )
  
  #shannon index for bog: same method as used previously 
  shannon_div_bog <- diversity(bogshan[, -1], index = "shannon") #-1 removes site column 
  
  shannon_results_bog <- data.frame(
    samplingProtocol = bogshan$samplingProtocol,  
    shannon_result = shannon_div_bog
  )
  
  print(shannon_results_bog)
  
  #graphs for each method: same method as used previously 
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
      "Odonata" = "darkgreen")) +  #colour blind friendly palette
    theme_grey()


#8. inverts without bog
  #terrestrial invertebrates
  #loading data: invertebrates, terrestrial, w/o bog 
  
  inverts_t2 <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="Inverts_t2") #extracting the sheet with terrestrial invert data from whole excel book
  
  #extracting necessary columns for my own ease of analysis: same method as used previously
  inverts_subset2 <- inverts_t2 %>%
    select(site, order, individualCount) #here, i only want to analyse these three variables; select from dyplr package allows this  
  
  #removing n/a values: same method as used previously
  invert_clean2 <- inverts_subset2 %>%
    filter(!is.na(individualCount) & !is.na(order)) #remove na from both individualCount and order columns
  
  #unique terrestrial invertebrate orders at each site: same method as used previously 
  south_invertord2 <- length(unique(invert_clean2$order[invert_clean2$site == "South"])) #9 unique orders
  north_invertord2 <- length(unique(invert_clean2$order[invert_clean2$site == "North"])) #8 unique orders 
  
  #relative abundance of each order
  #barplot of relative abundances for each order: same method as used previously
  ggplot(invert_clean2, aes(x = order, y = individualCount, fill = site)) + 
    geom_bar(stat="identity", position = "stack") +   
    theme_grey() + xlab("Order") + ylab("Abundance (count)") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    scale_fill_manual(values = c("lightblue3", "orange2")) + 
    labs(fill="Site", title="Relative abundances of terrestrial invertebrate orders between sites") 
  
  #shannon index for terrestrial invertebrate orders: same method as used previously 
  
  #reshaping the data so each site is a row(wide data), for ease of calculation: same method as used previously
  invertshan2 <- invert_clean2 %>%
    pivot_wider( 
      names_from = order, 
      values_from = individualCount, 
      values_fill = list(individualCount = 0), 
      values_fn = list(individualCount = sum) 
    )
  
  #calculating shannon diversity index: same method as used previously
  shannon_div_in2 <- diversity(invertshan2[, -1], index = "shannon") 
  
  shannon_results_in2 <- data.frame( 
    site = invertshan2$site,  
    shannon_result = shannon_div_in2 
  ) 
  
  print(shannon_results_in2)
  
  
  
  #loading data: invertebrates, aquatic w/o bog
  inverts_a2 <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="Inverts_a2")
  
  #extracting necessary columns: same method as used previously 
  invertsa_subset2 <- inverts_a2 %>%
    select(site, order, individualCount)
  
  #removing n/a values: same method as used previosuly 
  inverta_clean2 <- invertsa_subset2 %>%
    filter(!is.na(individualCount) & !is.na(order))
  
  #unique aquatic invertebrate orders at each site: same method as used previously 
  south_invertorda2 <- length(unique(inverta_clean2$order[inverta_clean2$site == "South"])) #9 unique orders
  north_invertorda2 <- length(unique(inverta_clean2$order[inverta_clean2$site == "North"])) #13 unique orders 
  
  #relative abundance of each order
  #barplot of relative abundances for each order: same method as used previously 
  ggplot(inverta_clean2, aes(x = order, y = individualCount, fill = site)) + 
    geom_bar(stat = "identity", position = "stack") +  #stack for stacked bars
    theme_grey() + xlab("Order") + ylab("Abundance (count)") + #grey theme, x and y axis labels 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + #x labels angled to make legible 
    scale_fill_manual(values = c("lightblue3", "orange2")) + #colour blind friendly 
    labs(fill="Site", title="Relative abundances of aquatic invertebrate orders between sites") #changing legend title, adding graph title
  
  #shannon index for aquatic invertebrate orders 
  #reshaping the data so each site is a row, for ease of calculation: same method as used previously 
  inverta_shan2 <- inverta_clean2 %>%
    pivot_wider(
      names_from = order,
      values_from = individualCount,
      values_fill = list(individualCount = 0),
      values_fn = list(individualCount = sum)
    )
  
  #calculating shannon diversity index: same method as used previously 
  shannon_div_ina2 <- diversity(inverta_shan2[, -1], index = "shannon") #-1 removes site column 
  
  shannon_results_ina2 <- data.frame(
    site = inverta_shan2$site,  
    shannon_result = shannon_div_ina2
  )
  
  print(shannon_results_ina2)
  
  #beta diversity of aquatic inverts w/o bog: same method as used previously 
  #creating a presence-absence matrix
  invert_a_beta2 <- inverta_clean2[,c("order","site")] 
  invert_a_beta2$presence <- 1 
  invert_a_beta2 <- invert_a_beta2 %>%
    group_by(site, order) %>%
    summarise(presence=sum(presence), .groups = "drop")
  
  invert_a_pa2 <- invert_a_beta2 %>% 
    pivot_wider(names_from=order,values_from=c(presence))
  list0 <- as.list(rep(0,ncol(invert_a_pa2)))
  names(list0) <- names(invert_a_pa2)
  invert_a_pa2 <- as.data.frame(invert_a_pa2 %>% replace_na(list0))
  row.names(invert_a_pa2) <- invert_a_pa2$site
  invert_a_pa2 <- invert_a_pa2[,-1]
  invert_a_pa2[invert_a_pa2 > 0] <- 1
  
  #calculating beta diversity- sorensen
  invert_a_sor2 <- beta.pair(invert_a_pa2, index.family="sorensen")
  mean(invert_a_sor2$beta.sor) #0.27
  
  #calculating beta diversity- jaccard
  invert_a_jac2 <- beta.pair(invert_a_pa2, index.family="jaccard")
  mean(invert_a_jac2$beta.jac) #0.42
  
  

