Synopsis
--------

Severe weather events such as Tornados, floods or draughts often cause
fatalities among the population and damage to the economy by destroying
property or causing crop failure. Developping efficient counter
strategies for these event to minimize the death toll and economic
damage, it demands a quantitative understanding of the damages induced
by such weather events in the past. In this manuscript, we present an
quantitative account for the effects of severe weather events on the
population health and the economy. For this purpose, we study the
climate data set provided on the Coursera course wegpage, which contains
records on severe weather events across the United States from
1950-2011. We employ the total amount of fatalities and the property and
agricultural damage accumulated across the United States as a probe to
study these effects. From a statistical analysis, we find that Tornados
are the main cause for both fatalities among the population and property
damage. By contrast, 60 % of the agricultural damage is caused by
draughts and floods and Tornados only play a minor role. Our study sheds
light on the quantitative impact of severe weather events on population
health and the economy and could help to devise efficient counter
strategies.

Loading data and libraries
--------------------------

The climate data used for this survey,"repdata-data-StormData.csv.bz2",
is provided within the course material and downloaded from the Coursera
webpage on May 3, 2017, 5:45 PM. The data set will be stored in the
table dataframe format of the dplyr package.

    df<-tbl_df(read.csv("repdata-data-StormData.csv.bz2", header=TRUE))
    print(df)

    ## # A tibble: 902,297 × 37
    ##    STATE__           BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME  STATE
    ##      <dbl>             <fctr>   <fctr>    <fctr>  <dbl>     <fctr> <fctr>
    ## 1        1  4/18/1950 0:00:00     0130       CST     97     MOBILE     AL
    ## 2        1  4/18/1950 0:00:00     0145       CST      3    BALDWIN     AL
    ## 3        1  2/20/1951 0:00:00     1600       CST     57    FAYETTE     AL
    ## 4        1   6/8/1951 0:00:00     0900       CST     89    MADISON     AL
    ## 5        1 11/15/1951 0:00:00     1500       CST     43    CULLMAN     AL
    ## 6        1 11/15/1951 0:00:00     2000       CST     77 LAUDERDALE     AL
    ## 7        1 11/16/1951 0:00:00     0100       CST      9     BLOUNT     AL
    ## 8        1  1/22/1952 0:00:00     0900       CST    123 TALLAPOOSA     AL
    ## 9        1  2/13/1952 0:00:00     2000       CST    125 TUSCALOOSA     AL
    ## 10       1  2/13/1952 0:00:00     2000       CST     57    FAYETTE     AL
    ## # ... with 902,287 more rows, and 30 more variables: EVTYPE <fctr>,
    ## #   BGN_RANGE <dbl>, BGN_AZI <fctr>, BGN_LOCATI <fctr>, END_DATE <fctr>,
    ## #   END_TIME <fctr>, COUNTY_END <dbl>, COUNTYENDN <lgl>, END_RANGE <dbl>,
    ## #   END_AZI <fctr>, END_LOCATI <fctr>, LENGTH <dbl>, WIDTH <dbl>, F <int>,
    ## #   MAG <dbl>, FATALITIES <dbl>, INJURIES <dbl>, PROPDMG <dbl>,
    ## #   PROPDMGEXP <fctr>, CROPDMG <dbl>, CROPDMGEXP <fctr>, WFO <fctr>,
    ## #   STATEOFFIC <fctr>, ZONENAMES <fctr>, LATITUDE <dbl>, LONGITUDE <dbl>,
    ## #   LATITUDE_E <dbl>, LONGITUDE_ <dbl>, REMARKS <fctr>, REFNUM <dbl>

Data Processing
---------------

In this report, we are interested in the effects of severe weather
conditions on population health and economy. Hence, we are mainly
interested in the variables *FATALITIES*, *PROPDMG* and *CROPDMG* as
well as the corresponding scaling variables *PROPDMGEXP* and
*CROPDMGEXP*. Accordingly, we will select the repsective columns to
reduce the data set.

Looking at the levels of the factor variable EVTYPE, we see that there
are 985 factors. Since our goal is to provide the audience a quick
summary on the weather data, we'll filter our data to only show top 10
events of severe weather conditions.

Regarding the scaling variables *PROPDMGEXP* and *CROPDMGEXP*, we will
filter only for indicating thousands, *K* and millions, *M* of dollars.
In the next step, we multiply each entry of *PROPDMG* and *CROPDMG* by
the respective factor scaling factor to obtain the actual dollar value.

Additionally, looking at the EVTYPE levels we find the event
*THUNDERSTORM WIND* duplicated as *TSTM WIND*. In order to provide
proper statistical results, we clean the data by replacing *TSTM WIND*
with *THUNDERSTORM WIND*.

After these data processing steps are taken, we calculate the sums of
*FATALITIES*, *PROPDMG* and *CROPDMG* broken down into event types.

    # select fatality data, calculate sums and filter for top 10 events
    df %>% group_by(EVTYPE) %>% summarise(tot.fat=sum(FATALITIES)) %>% filter(tot.fat>206) %>% arrange(desc(tot.fat)) ->df.fatalities.top10


    # select property damage data and filter scaling variable
    df %>% select(EVTYPE, PROPDMG, PROPDMGEXP) %>% filter(!PROPDMG==0) %>% filter (PROPDMGEXP=="K" | PROPDMGEXP=="M")-> df.prop

    # cleaning the event levels
    df.prop$EVTYPE<- gsub("TSTM WIND", "THUNDERSTORM WIND", df.prop$EVTYPE)

    # Scale the PROPDMG variable to obtain USD value
    df.prop %>% mutate(total=ifelse(PROPDMGEXP=="K", 1000*PROPDMG, 1000000*PROPDMG)) -> df.prop

    # calculate property damage sums and filter for top 10 events
    df.prop %>% group_by(EVTYPE) %>% summarise(propdamage=sum(total)) %>% arrange(propdamage) %>% filter(propdamage>2553890550) -> prop.dmg

    # select crop damage data and filter scaling variable
    df %>% select(EVTYPE, CROPDMG, CROPDMGEXP) %>% filter(!CROPDMG==0) %>% filter (CROPDMGEXP=="K" | CROPDMGEXP=="M")-> df.crop

    # cleaning the event levels
    df.crop$EVTYPE<- gsub("TSTM WIND", "THUNDERSTORM WIND", df.crop$EVTYPE)

    # Scale the CROPDMG variable to obtain USD value
    df.crop %>% mutate(total=ifelse(CROPDMGEXP=="K", 1000*CROPDMG, 1000000*CROPDMG)) -> df.crop

    # calculate crop damage sums and filter for top 10 events
    df.crop %>% group_by(EVTYPE) %>% summarise(cropdamage=sum(total)) %>% arrange(cropdamage) %>% filter(cropdamage>678346000) -> crop.dmg

Results
-------

### Effects of severe weather events on population health

To understand the effect of severe wheather events on popuation health,
we plot the total amount of fatalities in the United states from
1950-2011 in Fig.1 broken down into the respective events. As can be
seen, Tornados make the largest contribution to the total fatalities
with a death toll of 5633, outnumbering the second largest contributor,
excessive heat with a death toll of 1903 by a factor of three. The
Tornado death toll is almost as high as the total death toll from all
the nine other events of 6448.

    df.fatalities.top10$fac<-factor(df.fatalities.top10$EVTYPE, levels=df.fatalities.top10$EVTYPE)

    g<-ggplot(df.fatalities.top10, aes(x=fac, y=tot.fat))+geom_bar( stat="identity", col="steelblue4", fill="steelblue1")+labs(title="Nationwide fatalities from severe weather events")+
        labs(y="Fatalities")+labs(x="Event")+coord_flip()
    print(g)

![Fig. 1: Total fatalities resulting from severe weather events in the
United States from
1950-2011.](CourseProject2_files/figure-markdown_strict/fig1-1.png)

### Effects of severe weather events on the economy

To understand the effect of severe wheather events on the economy, we
plot the total amount of property damage broken down into the respective
events in the United States from 1950-2011 in Fig.2. As was the case for
the fatalities, also the economy suffers the most from Tornados causing
an accumulated property damage exceeding USD 50 billions over the 60
year period. However, we see that flash floods, floods and hail also
cause significant property damage. In total, property equal an amount of
USD 132.4338452 billions was damaged in the United States over this
period.

    prop.dmg$fac<-factor(prop.dmg$EVTYPE, levels=prop.dmg$EVTYPE)

    g<-ggplot(prop.dmg, aes(x=fac, y=propdamage*1e-9))+geom_bar( stat="identity", col="steelblue4", fill="steelblue1")+labs(title="Economic damage from severe weather events")+labs(y="Costs (Billion $)")+labs(x="")+coord_flip()
    print(g)

![Fig.2: Total property damage resulting from severe weather events in
the United States from
1950-2011.](CourseProject2_files/figure-markdown_strict/fig2-1.png)

In addition, we plot the total amount of agricultural damage caused by
severe weather events broken down into the respective events in the
United States from 1950-2011 in Fig.3. In contrast to fatalities and
property damage, agricultural damage is mostly caused by droughts and
floods accounting for 59.4370651% of the total damage that corresponds
to USD 30.510481 billions.

    crop.dmg$fac<-factor(crop.dmg$EVTYPE, levels=crop.dmg$EVTYPE)

    g<-ggplot(crop.dmg, aes(x=fac, y=cropdamage*1e-9))+geom_bar( stat="identity", col="steelblue4", fill="steelblue1")+labs(title="Agricultural damage from severe weather events")+labs(y="Costs (Billion $)")+labs(x="")+coord_flip()
    print(g)

![Fig.3: Total agricultural damage resulting from severe weather events
in the United States from
1950-2011.](CourseProject2_files/figure-markdown_strict/fig3-1.png)

Discussion & Conclusion
-----------------------

The following discussion will only focus on the main events that cause
fatalities among the population and damage to property and agriculture.
A detailed analysis would lie beyond the scope of this work. We start
our discussion by comparing the results from the effects of severe
weather events on population health and property shown in Fig.1 and 2,
respectively. As can be seen, Tornados are the main contributing event
in both cases causing almost 50 % of the total fatalities and
38.9822257% of the total property damage. Hence, investing resources
into tornado shelter could help reduce the death toll significantly.
Also, dedicating resources to Tornado-safe infrastructure, such as
under-ground power grids and land lines, and personal housing, such as
wind-proof building structures, could help to reduce the economic impact
of this event.

Looking at the data of agricultural damage induced by severe weather
event, we can observe a different picture. Here, 59.4370651% of the
damage is caused by droughts and flood events. While droughts may be
difficult to fight, a possible starting point to reduce the impact of
severe weather events on agriculture were to dedicate resources into
flood protection. For instance, building dikes and reservoirs would help
to manage heavy rainfalls and, at the same time allow to store water
resources for the dry season. As an additional effect, handling heavy
rainfall by means of dikes and reservoirs would also help to reduce the
large impact of flood events on property damage shown in Fig. 2.
