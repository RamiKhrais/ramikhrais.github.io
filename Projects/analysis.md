---
title: Analysis of Airlines Market in USA
author: Rami Kh
date: '2018-11-16'
slug: analysis-of-airlines-market-in-usa
categories:
  - Data Analysis
  - Visualisation
tags: []
---

##Data Overview


<p style='text-align: justify;'> The data that will be used in this analysis is extracted from built-in R dataset **hflights**. The dataset consists of 21 variables and 227,496 observations and it covers the activity of 15 carriers in United States in 2011.</p>


##What is next?   


<p style='text-align: justify;'> What I'm going to do is to imagine myself as an analyst who works in a market research consultancy. I have been requested by my manager to extract some useful insights from the dataset. I have also been asked to visualize the resulted insights in a nice and precise way so the manager can draw few conclusions about the main trends in the market.</p>


##Packages used in the analysis 

```{r warning=FALSE, message=FALSE}
library(hflights) 
library(ggplot2) 
library(dplyr)
library(ggthemes)
library(scales)
library(gridExtra)
```


<p style='text-align: justify;'>Let's first have a look at our dataset.</p>

```{r}
glimpse(hflights)
```


<p style='text-align: justify;'> It seems that the carriers have been regestired by their codes instead of their names. It might be more convenient to assign the full name for each carrier.</p>


```{r}
lut <- c("AA" = "American", "AS" = "Alaska", "B6" = "JetBlue", "CO" = "Continental", 
         "DL" = "Delta", "OO" = "SkyWest", "UA" = "United", "US" = "US_Airways", 
         "WN" = "Southwest", "EV" = "Atlantic_Southeast", "F9" = "Frontier", 
         "FL" = "AirTran", "MQ" = "American_Eagle", "XE" = "ExpressJet", "YV" = "Mesa")
hflights$Carrier <- lut[hflights$UniqueCarrier]
glimpse(hflights)
```


<p style='text-align: justify;'> Not that much of data manipulation as the dataset is staraightforward. Let's start analysing our data by asking: <span style="color:#2A3132">**Which carriers dominate the market?**</span></p>


```{r warning=FALSE, message=FALSE}
market_snapshot <- hflights %>%
  filter(Cancelled == 0) %>%
  group_by(Carrier) %>%
  summarise(flights = n()) %>%
  arrange(desc(flights))
market_snapshot
```

```{r}
highest <- c("two", rep("one", 14))
ggplot(market_snapshot, aes(x = reorder(Carrier, flights), y = flights)) + 
  geom_bar(stat = "identity", aes(fill = highest)) + 
  scale_fill_manual(values = c("#F98866","#FF420E")) + coord_flip() +
  geom_text(aes(y= flights, x = seq(15,1), label = paste0(round(flights),"")), nudge_y = 4700, color="black", size = 5) + 
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 11, colour = "black", face = "bold"),
        legend.position = "None",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 16, margin = margin(0,0,20,0))) + 
  labs(title = "The Most Active Carriers in 2011", 
       caption = "Source: hflights")
```


<p style='text-align: justify;'> It seems that **ExpressJet** along with **Continental** have the lion's share of the number of flights. We can go much deeper now and examine the performance of both carriers within different time ranges.</p>


<p style='text-align: justify;'><span style="color:#2A3132">**How do ExpressJet and Continental perform daily within one week?**</p></span>

```{r}
two_largest_daily <- hflights %>%
  filter(DayOfWeek %in% c(1, 2, 3, 4, 5, 6, 7)) %>%
  filter(Cancelled == 0) %>%
  filter(Carrier %in% c("Continental", "ExpressJet")) %>%
  group_by(Carrier, DayOfWeek) %>%
  summarise(flights = n())
two_largest_daily
```



```{r}
ggplot(two_largest_daily, aes(x= DayOfWeek, y= flights, col = Carrier)) + 
    geom_line(size = 1.2) +
    theme_minimal() +
    geom_point(size = 3.2, shape = 21, fill = "white", stroke = 2.5) + 
    scale_colour_manual(values = c("firebrick1", "goldenrod1")) +
    scale_x_continuous(breaks = 1:7, labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) + 
    theme(axis.text.x = element_text(colour = "black", size = 12, vjust = 0.6, angle = 60)) +
    theme(axis.text.y = element_text(colour = "black", size = 12)) + 
    theme(axis.title.x = element_blank()) +
    ylab("Number of Flights") +
    theme(axis.title.y = element_text(size = 14, margin = margin(r = 20))) +
    theme(panel.grid.major.x = element_blank()) + 
    theme(panel.grid.minor.x = element_blank()) +
    theme(legend.position = "bottom") + 
    theme(legend.title = element_blank()) + 
    theme(legend.text = element_text(size = 12, colour = "black", face = "bold")) +
    labs(title = "The Two Largest Players in the Market", subtitle = "Daily Flights in a Week") + 
    theme(plot.title = element_text(size = 15))
```
    

<p style='text-align: justify;'><span style="color:#2A3132">**How do ExpressJet and Continental perform daily within one month?**</p></span>


```{r warning=FALSE, message=FALSE}
two_largest_daily_month <- hflights %>%
  filter(DayofMonth %in% c(1:30)) %>%
  filter(Cancelled == 0) %>%
  filter(Carrier %in% c("ExpressJet", "Continental")) %>%
  group_by(Carrier, DayofMonth) %>%
  summarise(flights = n())
two_largest_daily_month <- as.data.frame(two_largest_daily_month)
two_largest_daily_month
```


```{r}
ggplot(two_largest_daily_month, aes(x= DayofMonth, y= flights, col= Carrier)) +
  geom_line(size = 1.2) +
  geom_point(size = 3.2, shape = 21, fill = "white", stroke = 2.5) +
  theme_minimal() + 
  scale_colour_manual(values = c("firebrick1", "goldenrod1")) + 
  scale_x_continuous(breaks = 1:30, limits = c(0, 30)) + 
  scale_y_continuous(limits = c(2000, 2600), breaks = c(2000, 2100, 2200, 2300, 2400, 2500, 2600), labels = scales :: comma) +
  ylab("Number of Flights") +
  theme(panel.grid.major.x = element_blank()) + 
  theme(panel.grid.minor.x = element_blank()) +
  theme(axis.text.x = element_text(size = 10, colour = "black")) +
  theme(axis.text.y = element_text(size = 10, colour = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(size= 14, margin = margin(r =20))) + 
  theme(legend.position = "bottom") + 
  theme(legend.title = element_blank()) + 
  theme(legend.text = element_text(size = 12, face = "bold", colour = "black")) +
  labs(title = "The Two Largest Players in the Market", subtitle = "Daily Flights in a Month") + 
  theme(plot.title = element_text(size = 15))
```


<p style='text-align: justify;'><span style="color:#2A3132">**How do ExpressJet and Continental perform monthly?**</p></span>


```{r}
two_largest_monthly <- hflights %>%
  filter(Month %in% c(1:12)) %>%
  filter(Cancelled == 0) %>%
  filter(Carrier %in% c("ExpressJet", "Continental")) %>%
  group_by(Carrier, Month) %>%
  summarise(flights = n())
    
    
two_largest_monthly <- as.data.frame(two_largest_monthly)
two_largest_monthly
```


```{r}
ggplot(two_largest_monthly, aes(x= Month, y= flights, col = Carrier)) +
  geom_line(size = 1.2) +
  geom_point(size = 3.2, shape = 21, fill = "white", stroke = 2.5) +
  theme_minimal() +
  ylab("Number of Flights") +
  scale_y_continuous(breaks = seq(5000, 7000, 300)) +
  scale_x_continuous(breaks = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(axis.text.x = element_text(size = 12, colour = "black")) +
  theme(axis.text.y = element_text(size = 12, colour = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) + 
  scale_colour_manual(values = c("firebrick1", "goldenrod1")) +
  theme(axis.title.y = element_text(size = 14, margin = margin(r=20))) +
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size= 12, face = "bold", colour= "black")) + 
  theme(legend.title = element_blank()) +
  labs(title = "The Largest Players in the Market", subtitle = "Monthly Flights") +
  theme(plot.title = element_text(size = 15, colour = "black"))
```


##More Insights Ahead!

<p style='text-align: justify;'>Everthing looks good so far. Let's try now to get some spatial insights by aaking: **What is the most active departure airport for each carrier in the weekend?**</p>


```{r}
Origin_carr <- hflights %>%
  filter(DayOfWeek %in% c(6, 7)) %>%
  group_by(Carrier, Origin) %>%
  summarise(flights = n()) %>%
  mutate(rank = rank(desc(flights))) %>%
  filter(rank == 1) %>%
  arrange(desc(flights))
Origin_carr
```



<p style='text-align: justify;'>**What is the most visited destination for each carrier in the weekend?**</p>

```{r}
Dest_carr <- hflights %>% 
  filter(DayOfWeek %in% c(6, 7)) %>%
  group_by(Carrier, Dest) %>%
  summarise(flights = n()) %>%
  mutate(rank = rank(desc(flights))) %>%
  filter(rank == 1) %>%
  arrange(desc(flights))
Dest_carr
```


<p style='text-align: justify;'>**What are the most active airline routes in the weekend?**</p>

```{r}
Airline_route <- hflights %>%
  filter(DayOfWeek %in% c(6, 7)) %>%
  group_by(Origin, Dest) %>%
  summarise(flights = n()) %>%
  arrange(desc((flights))) %>%
  mutate(rank = rank(desc(flights))) %>%
  filter(rank %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
Airline_route
```


##Four Seasons!

<p style='text-align: justify;'>We will work now to extract seasonal insights from our dataset to see how the market trends differ from season to season. So, let's ask: **What are the ten most visited destinations in the weekend during each season?**</p>

```{r}
allseasons <- hflights %>%
  filter(DayOfWeek %in% c(6, 7)) %>%
  mutate(Season = case_when(Month %in% 3:5 ~ "Spring", 
                            Month %in% 9:11 ~ "Autumn",
                            Month %in% 6:8 ~ "Summer", 
                            Month %in% c(12, 1, 2) ~ "Winter")) %>%
  filter(!is.na(Season)) %>%
  group_by(Season, Dest) %>%
  summarise(flights = n()) %>%
  filter(!is.na(flights)) %>%
  arrange(Season, desc(flights)) %>%
  slice(1:10)
allseasons <- as.data.frame(allseasons)
allseasons
```


```{r}
colours <- c("gray74", "chartreuse3", "darkgoldenrod1", "deepskyblue3")
ggplot(allseasons, aes(x = Dest, y = flights)) + geom_bar(aes(fill = Season), stat = "identity") + 
  facet_wrap(~ Season, scales = "free") +
  theme_minimal() + 
  scale_fill_manual(values = colours)+
  theme(panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom", 
        legend.text = element_text(size = 12, colour = "black"),
        legend.key.size = unit(1, 'lines'))
```

<p style='text-align: justify;'>Let's plot a graph for each season.</p>

###Spring

```{r}
ggplot(subset(allseasons, Season == "Spring"), aes(x = reorder(Dest, flights), y = flights)) + 
  scale_y_continuous(limits = c(0, 600), breaks = c(0, 100, 200, 300, 400, 500, 600), labels = scales :: comma, name = "Number of Flights") +
  geom_bar(stat = "identity", fill = "chartreuse3", width = 0.65) +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14, colour = "black", margin = margin(t=20)),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour = "black", size = 13),
        axis.text.y = element_text(colour = "black", size = 13),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 16, colour = "black", margin = margin(0,0,30,0))) + 
  coord_flip() + 
  labs(title = "The Ten Most Visited Destinations in Spring")
```


###Summer

```{r}
ggplot(subset(allseasons, Season == "Summer"), aes(x = reorder(Dest, flights), y = flights)) + 
  scale_y_continuous(limits = c(0, 600), breaks = c(0, 100, 200, 300, 400, 500, 600), labels = scales :: comma, name = "Number of Flights") +
  geom_bar(stat = "identity", fill = "darkgoldenrod1", width = 0.65) +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14, colour = "black", margin = margin(t=20)),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour = "black", size = 13),
        axis.text.y = element_text(colour = "black", size = 13),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 16, colour = "black", margin = margin(0,0,30,0))) + 
  coord_flip() + 
  labs(title = "The Ten Most Visited Destinations in Summer")
```


###Autumn

```{r}
ggplot(subset(allseasons, Season == "Autumn"), aes(x = reorder(Dest, flights), y = flights)) + 
  scale_y_continuous(limits = c(0, 600), breaks = c(0, 100, 200, 300, 400, 500, 600), labels = scales :: comma, name = "Number of Flights") +
  geom_bar(stat = "identity", fill = "gray74", width = 0.65) +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14, colour = "black", margin = margin(t=20)),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour = "black", size = 13),
        axis.text.y = element_text(colour = "black", size = 13),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 16, colour = "black", margin = margin(0,0,30,0))) + 
  coord_flip() + 
  labs(title = "The Ten Most Visited Destinations in Autumn")
```


###Winter

```{r}
ggplot(subset(allseasons, Season == "Winter"), aes(x = reorder(Dest, flights), y = flights)) + 
  scale_y_continuous(limits = c(0, 600), breaks = c(0, 100, 200, 300, 400, 500, 600), labels = scales :: comma, name = "Number of Flights") +
  geom_bar(stat = "identity", fill = "deepskyblue3", width = 0.65) +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14, colour = "black", margin = margin(t=20)),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour = "black", size = 13),
        axis.text.y = element_text(colour = "black", size = 13),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 16, colour = "black", margin = margin(0,0,30,0))) + 
  coord_flip() + 
  labs(title = "The Ten Most Visited Destinations in Winter")
```


##Cancelled ... Delayed!

<p style='text-align: justify;'>Let's try to extract some useful insights regarding the dealyed and cancelled flights by asking: **Which carrier has the highest level of cancelled flights?**</P> 

```{r}
Cancelled_Carr <- hflights %>%
  filter(Cancelled == 1) %>%
  group_by(Carrier) %>%
  summarise(cancelledflights = n()) %>%
  arrange(desc(cancelledflights)) %>%
  slice(1:5)
Cancelled_Carr
```



```{r}
ggplot(Cancelled_Carr, aes(x=reorder(Carrier, cancelledflights), y=cancelledflights)) + 
  geom_bar(stat = "identity", fill = "coral1") +
  theme_minimal() + 
  theme(panel.grid.major.x = element_blank()) + 
  scale_y_continuous(limits = c(0, 1200), breaks = c(0, 200, 400, 600, 800, 1000, 1200), labels = scales :: comma) +
  theme(axis.text.x = element_text(size = 12, face = "bold", colour = "black", angle = 65, vjust = 0.6)) + 
  theme(axis.text.y = element_text(size = 10, colour = "black")) + 
  ylab("Number of Cancelled Flights") +
  theme(axis.title.x = element_blank()) + 
  theme(axis.title.y = element_text(size = 12, colour = "black", face = "bold", margin = margin(r=30))) +
  labs(title = "The Carriers with Highest Level of Cancelled Flights") +
  theme(plot.title = element_text(size = 15, colour = "black", margin = margin(0,0,30,0)))
```


<p style='text-align: justify;'>**Which carrier has the highest average of arrival delay?**</p>

```{r}
Carrier_arrdelay <- hflights %>%
  filter(!is.na(ArrDelay), ArrDelay > 0) %>%
  group_by(Carrier) %>%
  summarise(avg = mean(ArrDelay)) %>%
  mutate(rank = rank(avg)) %>%
  arrange(desc(rank))
  
Carrier_arrdelay
```


```{r}
ggplot(Carrier_arrdelay, aes(x = reorder(Carrier, avg), y = avg)) + 
  geom_bar(stat = "identity", fill = "#20948B") +
  theme_minimal()+
  scale_y_continuous(limits = c(0, 50), breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50), labels = scales :: comma) +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(size = 10, face = "bold", colour = "black", angle = 65, vjust = 0.6)) +
  theme(axis.text.y = element_text(size = 10, face = "bold", colour = "black")) +
  ylab("Average of Delay") +
  theme(axis.title.y = element_text(size = 12, face = "bold", colour = "black", margin = margin(r=30))) +
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "The Average of Arrival Delay for each Carrier", subtitle = "Minutes") +
  theme(plot.title = element_text(size = 15, colour = "black"))
```



<p style='text-align: justify;'>**Which carrier has the highest average of departure delay?**</p>

```{r}
Carrier_depdelay <- hflights %>%
  filter(!is.na(DepDelay), DepDelay > 0) %>%
  group_by(Carrier) %>%
  summarise(avg = mean(DepDelay)) %>%
  mutate(rank = rank(avg)) %>%
  arrange(rank)
Carrier_depdelay
```


```{r}
ggplot(Carrier_depdelay, aes(x = reorder(Carrier, avg), y = avg)) + 
  geom_bar(stat = "identity", fill = "#6AB187") +
  theme_minimal()+
  scale_y_continuous(limits = c(0, 50), breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50), labels = scales :: comma) +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(size = 10, face = "bold", colour = "black", angle = 65, vjust = 0.6)) +
  theme(axis.text.y = element_text(size = 10, face = "bold", colour = "black")) +
  ylab("Average of Delay") +
  theme(axis.title.y = element_text(size = 12, face = "bold", colour = "black", margin = margin(r=30))) +
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "The Average of Departure Delay for each Carrier", subtitle = "Minutes") +
  theme(plot.title = element_text(size = 15, colour = "black"))
```


###Conclusion1:

<p style='text-align: justify;'>The first three graphs show that the two dominant carriers share almost the same performance pattern. Within a given week, both **ExpressJet** and **Continental** have their highest numbers of flights on Monday, Thursday and Friday, while the number of flights reaches its trough on Saturday. The data for daily performance within a month reveals that the number of flights usually slip on 4th and 29th for both carriers and that **ExpressJet’s** flights peak on 28th. The monthly data shows that both carriers have their flight reaching a peak in July at 6174 and 6728 flights for **Continental** and **ExpressJet** respectively. The same data shows also that the trough for both carriers takes place in February and that, opposed to the general trend, **Continental’s** flights outpace **ExpressJet’s** ones during the last three month of the year.</p>

###Conclusion2:

<p style='text-align: justify;'>The extracted data reveals that we have only two airports (George Bush Intercontinental/Houston Airport and William P. Hobby Airport) as departure points for all carriers in the dataset. The data also shows that Dallas Love Airport is the most visited destination in the weekend when it comes to Southwest airlines while Charlotte Douglas International Airport is the most visited destination in the weekend when it comes to Mesa Airlines. The data reveals in addition that the route from William P. Hobby Airport to Dallas Love Field Airport is the most active route during the weekend.</p>

###Conclusion3: 

<p style='text-align: justify;'>The seasonal insights shows that Dallas Love Field Airport is the most visited destination in autumn, winter and summer during the weekend while Hartsfield–Jackson Atlanta International Airport is the most visited destination in spring. The data also shows that Charlotte Douglas International Airport is the less visited destination in autumn and spring during the weekend. As for summer and winter, Newark Liberty International Airport and San Antonio International Airport are the less visited destinations respectively.</p>

###Conclusion4:

<p style='text-align: justify;'>The data shows that **ExpressJet** and **Southwest** have the highest numbers of cancelled flights and this result seems intuitive as these carriers have the highest number of flights. The data shows that **JetBlue** and **AtlanticSoutheast** have the highest average of departure and arrival delays while **Mesa** has the lwoest average of dealys.</p>
 
 
