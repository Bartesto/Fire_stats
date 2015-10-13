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

