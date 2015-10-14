########################################################################################################
## Stats for Yarraloola
rm(list=ls())

dir = "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\Fire_stats"
setwd(dir)
## Constants
yarra = 163213.70
habitat = 65665.00023
#regime totals R1 is 8-20, R2 is 21-40
R1=120649.4
R2=42563.2
#vegclass totals
C1=35384.38769
C2=37418.77911
C3=6674.229096
C4=19113.0063
C5=64117.64018
C6=223.661856
C7=280.890712


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
        scale_fill_manual(values=c("#9ACD32", "#008B00"),
                          name="Fire Regime",
                          breaks=c("r1", "r2"),
                          labels=c("8-20 yrs", "21-40 yrs")) +
        ggtitle("Area burnt per fire frequency period by fire regime")
ggsave("area_by_freq_by_regime.pdf")


#percentage burnt by fire freq period by fire regime
df6 <- df %>%
        rename(regime=veg_class,zero=b0, one=b1, two=b2, three=b3, four=b4, five=b5) %>%
        slice(8:9)
df6$tot <- c(R1,R2)
df6 <- df6 %>%
        gather("freq", "area", 2:7) %>%
        mutate(perc=area/tot *100)
        

ggplot(df6, aes(x=factor(freq), y=perc, fill=regime)) +
        geom_bar(position="dodge", stat="identity") +
        theme_bw() +
        ylab("percentage of Yarraloola") +
        xlab("number of times burnt") +
        scale_fill_manual(values=c("#9ACD32", "#008B00"),
                           name="Fire Regime",
                           breaks=c("r1", "r2"),
                           labels=c("8-20 yrs", "21-40 yrs")) +
        ggtitle("Percentage of total area burnt per fire frequency period by fire regime")
ggsave("perc_by_freq_by_regime.pdf")

#area by fire freq period by vegclass
df7 <- df %>%
        slice(1:7) %>%
        rename(zero=b0, one=b1, two=b2, three=b3, four=b4, five=b5) %>%
        gather("freq", "area", 2:7)

ggplot(df7, aes(x=factor(freq), y=area, fill=veg_class)) +
        geom_bar(position="dodge", stat="identity") +
        theme_bw() +
        ylab("area (ha)") +
        xlab("number of times burnt") +
        scale_fill_discrete(name="Veg Class") +
        ggtitle("Area burnt per fire frequency period by veg class")
ggsave("area_by_freq_by_vegclass.pdf")

#facet example
df7.1 <- df7
df7.1$veg_class <- as.factor(df7.1$veg_class)
levels(df7.1$veg_class) <- c("class 1", "class 2", "class 3", "class 4", "class 5", "class 6", "class 7")
ggplot(df7.1, aes(x=factor(freq), y=area, fill=freq)) +
        geom_bar(stat="identity") +
        facet_wrap(~veg_class) +
        theme_bw() +
        xlab("number of times burnt") +
        ylab("area (ha)") +
        ggtitle("Area burnt per fire frequency period by veg class")
ggsave("area_by_freq_by_vegclass_facet.pdf")

#percentage by fire freq period by vegclass
df8 <- df %>%
        slice(1:7) %>%
        rename(zero=b0, one=b1, two=b2, three=b3, four=b4, five=b5)
df8$tot <- c(C1,C2,C3,C4,C5,C6,C7)
df8 <- df8 %>%
        gather("freq", "area", 2:7) %>%
        mutate(perc=area/tot *100)

ggplot(df8, aes(x=factor(freq), y=perc, fill=veg_class)) +
        geom_bar(position="dodge", stat="identity") +
        theme_bw() +
        ylab("percentage of veg class") +
        xlab("number of times burnt") +
        scale_fill_discrete(name="Veg Class") +
        ggtitle("Percentage burnt per fire frequency period by veg class")
ggsave("perc_by_freq_by_vegclass.pdf")

#facet example
df8.1 <- df8
df8.1$veg_class <- as.factor(df8.1$veg_class)
levels(df8.1$veg_class) <- c("class 1", "class 2", "class 3", "class 4", "class 5", "class 6", "class 7")
ggplot(df8.1, aes(x=factor(freq), y=perc, fill=freq)) +
        geom_bar(stat="identity") +
        facet_wrap(~veg_class) +
        theme_bw() +
        xlab("number of times burnt") +
        ylab("percentage of veg class") +
        ggtitle("Percentage burnt per fire frequency period by veg class")
ggsave("perc_by_freq_by_vegclass_facet.pdf")
        







