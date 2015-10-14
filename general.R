########################################################################################################
## Stats for Yarraloola
rm(list=ls())

dir = "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\Fire_stats"
setwd(dir)
## Constants
yarra = 163213.70
habitat = 65665.00023


#Libraries
library(dplyr)
library(tidyr)
library(ggplot2)

#raw data
df = read.csv("fire_freq_veg_cat_yarraloola.csv", header = TRUE, stringsAsFactors = FALSE)

#rowwise sum (total area by veg_class)
df2 <- df %>%
        rowwise() %>%
        mutate(vtot = sum(b0, b1, b2, b3, b4, b5))

#total area burnt by fire freq period
df3 <- df %>%
        select(starts_with("b")) %>%
        rename(zero=b0, one=b1, two=b2, three=b3, four=b4, five=b5) %>%
        slice(1:7) %>%
        summarise_each(funs(sum)) %>%
        gather("freq", "area", 1:6)
        

ggplot(df3, aes(x=factor(freq), y=area, fill=freq)) + 
        geom_bar(stat = "identity") +
        theme_bw() +
        ylab("area (ha)") +
        xlab("number of times burnt") +
        ggtitle("Area burnt per fire frequency period")
ggsave("area_by_freq.pdf")

#percentage burnt by fire freq period
df4 <- df3 %>%
        mutate(perc = area/yarra *100)

ggplot(df4, aes(x=factor(freq), y=perc, fill=freq)) + 
        geom_bar(stat = "identity") +
        theme_bw() +
        ylab("percentage of Yarraloola") +
        xlab("number of times burnt") +
        ggtitle("Percentage of total area burnt per fire frequency period")
ggsave("perc_by_freq.pdf")

#total area burnt by fire freq period by fire regime
df5 <- df %>%
        rename(regime=veg_class,zero=b0, one=b1, two=b2, three=b3, four=b4, five=b5) %>%
        slice(8:9) %>%
        gather("freq", "area", 2:7)

ggplot(df5, aes(x=factor(freq), y=area, fill=regime)) +
        geom_bar(position="dodge", stat="identity") +
        theme_bw() +
        ylab("area (ha)") +
        xlab("number of times burnt") +
        scale_fill_discrete(name="Fire Regime",
                          breaks=c("r1", "r2"),
                          labels=c("8-20 yrs", "21-40 yrs")) +
        ggtitle("Area burnt per fire frequency period by fire regime")
ggsave("area_by_freq_by_regime.pdf")

df6 <- df5 %>%
        mutate(perc = area/yarra *100)

ggplot(df6, aes(x=factor(freq), y=perc, fill=regime)) +
        geom_bar(position="dodge", stat="identity") +
        theme_bw() +
        ylab("percentage of Yarraloola") +
        xlab("number of times burnt") +
        scale_fill_discrete(name="Fire Regime",
                            breaks=c("r1", "r2"),
                            labels=c("8-20 yrs", "21-40 yrs")) +
        ggtitle("Percentage of total area burnt per fire frequency period by fire regime")
ggsave("perc_by_freq_by_regime.pdf")



