library(reshape2)
library(ranger)
library(ggplot2)
library(dplyr)
data = read.csv("data/genes_expression.csv", sep = ",")
dim(data)
data$Group <- factor(data$group, levels = c("GG", "IG", "IP", "PP"))
levels(data$Group)
colnames(data)
############################
library(dplyr)
group_by(data, Group) %>%
  summarise(
    count = n(),
    mean = mean(data$INPP4B, na.rm = TRUE),
    sd = sd(data$INPP4B, na.rm = TRUE)
  )
data$Expression <- data$INPP4B
######
library("ggpubr")
ggboxplot(data, x = "Group", y = "Expression", 
          color = "Group", 
          palette = "jco",
          order = c("GG", "IG","IP", "PP"),
          ylab = "Expression", xlab = "Group")
res.aov <- aov(data$INPP4B ~ Group, data = data)
aov
summary(res.aov)
TKHSD_Tc <- TukeyHSD(res.aov)
TKHSD_Tc
TK<-(TKHSD_Tc)
TK_data<-as.data.frame(TK[1])
###########
x <- which(names(data) == "Group") # name of grouping variable
y <- which(
  names(data) == "Expression" # names of variables to test
)
method1 <- "anova" # one of "anova" or "kruskal.test"
method2 <- "t.test" # one of "wilcox.test" or "t.test"
my_comparisons <- list(c("IG", "IP"),c("IG", "GG"), c("IP","PP"), c("PP", "GG"),c("IP", "GG"),
                       c("IG", "PP")) # comparisons for post-hoc tests

###
library(ggpubr)
for (i in y) {
  for (j in x) {
    p <- ggboxplot(data,
                   x = colnames(data[j]), y = colnames(data[i]),
                   color = "Group",
                   legend = "none",
                   palette = "npg",
                   add = "jitter",
                   title = "\t\t\t\t\   INPP4B"
    )
    print(
      p + stat_compare_means(comparisons = my_comparisons, method = method2, label = "p.format")
    )
  }
}
###

