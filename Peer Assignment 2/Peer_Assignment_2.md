# Peer Assignment 2
Muralidhar Areti  
Jul 24, 2015

## Synopsis
The NOAA Storm Database is created from storm data provided by the National Weather Service. This analysis aims to answer the following questions:  

1. Which types of events are most harmful with respect to population health?  
2. Which types of events have the greatest economic consequences?  

To achieve these goals we need to provide the following results:  

* Assessment of data  
    - Number of records per year  
* Top 10 disaster types (overall or specific date range?) by  
    - Fatalities  
    - Injuries  
    - Property Damage  


## Data Processing


### Assumptions


### Initializing the Data


```r
suppressPackageStartupMessages(library(dplyr))
library(lubridate)
library(ggplot2)
library(scales)
```




```r
data = read.csv("repdata-data-StormData.csv")
```


```r
print(dim(data))
```

```
## [1] 902297     37
```

```r
print(names(data))
```

```
##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"    
##  [6] "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"   
## [11] "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN"
## [16] "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"     
## [21] "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"   
## [26] "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
## [36] "REMARKS"    "REFNUM"
```

```r
# print(str(data))
# print(summary(data))
# print(head(data))
```

## Results



```r
top_pd = working_data %>%
    group_by(EVTYPE) %>%
    summarize(SumPropertyDamage = sum(PROPDMG)) %>%
    top_n(5) %>% 
    arrange(desc(SumPropertyDamage)) %>%
    select(EVTYPE)
```

```
## Selecting by SumPropertyDamage
```

```r
sub_data = inner_join(x=working_data, y=top_pd)
```

```
## Joining by: "EVTYPE"
```

```r
sub_data %>% 
    group_by(bgn_year, EVTYPE) %>%
    summarize(SumPropertyDamage = sum(PROPDMG)) %>%
    ggplot(aes(x=factor(bgn_year), y=SumPropertyDamage, fill=EVTYPE)) + 
    geom_bar(stat="identity") + 
    scale_y_continuous(labels=dollar) +
    scale_fill_brewer() +
    labs(x="Incident Year") + 
    labs(y="Measure Value") + 
    labs(title="Property Damage of the 5 Most Damaging Incidents") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=90, size=10))
```

![](Peer_Assignment_2_files/figure-html/2.3 Yearly Property Damage Trend-1.png) 

## Conclusion


End of File
