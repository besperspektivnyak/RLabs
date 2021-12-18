library(dplyr)
library(tibble)
library(ggplot2)
library(magrittr)
library(tidyr)
library(tidyverse)
library(data.table)
library(reshape2)

#getwd()
load('C:/Users/Елизавета/RLab2/Lab4/ExpImp.RData')

expimp <- tibble(ExpImp)
expimp <- slice(expimp, 3:20)
expimp[2:13] <- lapply(expimp[2:13], function(x) as.numeric(sub('-', '0', x)))
exp <- grep('.*Экспорт', colnames(expimp))
imp <- grep('.*Импорт', colnames(expimp))

expimp <- expimp %>%
    mutate(Суммарный_экспорт = select(., exp) %>% rowSums(na.rm = TRUE)) %>%
    mutate(Суммарный_импорт = select(., imp) %>% rowSums(na.rm = TRUE))

colnames(expimp)[1] <- "Region"
expimp <- expimp[c(1, 14, 15)]

expimp2 <- tibble(melt(data.table(expimp), id.vars="Region"))
plot <- ggplot(expimp2, aes(x=value, y=Region, fill=variable)) +
  geom_bar(stat='identity', position='dodge') + xlab("Объем") + ylab("Регион") +
  ggtitle("Объем экспорта и импорта в Центральном Федеральном округе") + 
  scale_fill_discrete(name = "Экспорт/Импорт")
 
# print(plot)

print(expimp)
difference = expimp$Суммарный_экспорт - expimp$Суммарный_импорт
print(difference)
difference = rep(difference, 2)
print(difference)

g <- ggplot(expimp2, aes(x=value, y=Region, fill=variable)) +
  geom_bar(stat='identity') + xlab("Объем") + ylab("Регион") +
  ggtitle("Объем экспорта и импорта в Центральном Федеральном округе") + 
  scale_fill_discrete(name = "Экспорт/Импорт") + 
  geom_text(aes(label=ifelse(variable=="Суммарный_экспорт",difference, "")), 
                                                           position = position_stack(vjust = 1))

