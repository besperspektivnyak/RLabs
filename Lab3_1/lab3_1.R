library(dplyr)
library(tibble)
library(ggplot2)
library(magrittr)

getwd()
load('Lab3/trades.RData')
print(trades)

table <- Reduce(function(...) merge(..., all=TRUE), trades)
table$geo <- NULL
table <- tibble(table) %>% group_by(time)
export <- table %>% filter(indic_et=='Exports in million of ECU/EURO')
import <- table %>% filter(indic_et=='Imports in million of ECU/EURO')
exp <- aggregate(export$values, by=list(export$sitc06, export$time), sum)
imp <- aggregate(import$values, by=list(export$sitc06, export$time), sum)

colnames(exp) <- c("Group", "Time", "Value")
colnames(imp) <- c("Group", "Time", "Value")

plot_exp <- ggplot(exp, aes(Time, Value, colour = Group, label=Value))  + geom_point() + geom_line() + geom_label(size = 5, vjust = 1, aes(label=exp$Value), colour = 'black') + ggtitle("Объемы эксопорта 2008-2019гг.") + xlab("Год") + ylab("Объем экспорта")

plot_imp <- ggplot(imp, aes(Time, Value, colour = Group, label=Value))  + geom_point() + geom_line() +  geom_label(size = 5, vjust = 1, aes(label=exp$Value), colour = 'black') + ggtitle("Объемы импорта 2008-2019гг.") + xlab("Год") + ylab("Объем импорта")

