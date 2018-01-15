# Load a dataset
data <- read.csv(file="./data.csv",head=TRUE,sep=",")

data$precision <- with(data, truePositives / deadIdentified)
data$recall <- with(data, truePositives / trueDead)
data$fscore <- with(data, 2 * ((precision * recall)/(precision + recall)))

library(ggplot2)

fontSize <- 10

dynamic <- data[which(data$analysisType == "dynamic"),]
static <- data[which(data$analysisType == "static"),]
hybrid <- data[which(data$analysisType == "hybrid"),]

longByPR <- reshape(data, direction="long", varying=list(names(data)[10:11]), v.names="Value", 
                idvar=c("WebApp"), timevar="Measure") 
longByPR$Measure <- as.factor(longByPR$Measure)
levels(longByPR$Measure)[levels(longByPR$Measure)=="1"] <- "Precision"
levels(longByPR$Measure)[levels(longByPR$Measure)=="2"] <- "Recall"

# visually check correlations
# ggplot(data=static, aes(x=total, y=executionTime)) +
#   geom_point() +
#   stat_smooth(method="lm", se=T) +
#   theme_linedraw() +
#   theme(axis.text=element_text(size=fontSize), axis.title=element_text(size=fontSize))
# 
# ggplot(data=data, aes(x=total, y=executionTime / 1000, colour=analysisType)) +
#   geom_line() +
#   theme_linedraw() +
#   xlab("Total number of JavaScript functions") + ylab("Execution time (seconds)") +
#   labs(color="Analysis type") +
#   theme(axis.text=element_text(size=fontSize), axis.title=element_text(size=fontSize),
#         legend.text=element_text(size=fontSize), legend.title=element_text(size=fontSize))
# 
# stop()

# Boxplot of execution times per analysis type
ggplot(data=data, aes(x=analysisType, y=executionTime / 1000)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=2) + 
  theme_linedraw() +
  xlab("Analysis Technique") + ylab("Execution time (seconds)") +
  scale_x_discrete(limits=c("dynamic","static","hybrid"), labels=c("Dynamic", "Static", "Hybrid")) + 
  theme(axis.text=element_text(size=fontSize), axis.title=element_text(size=fontSize))

ggsave("./plots/executionTime.pdf", scale = 0.5, height = 10, unit = "cm")

# Precision-recall plot per analysis type
# ggplot(data=data, aes(x=recall, y=precision, colour=analysisType)) +
#   geom_line() + 
#   theme_linedraw() +
#   xlab("Recall") + ylab("Precision") +
#   labs(color="Analysis type") + 
#   theme(axis.text=element_text(size=fontSize), axis.title=element_text(size=fontSize), 
#         legend.text=element_text(size=fontSize), legend.title=element_text(size=fontSize))
# 
# ggsave("./plots/pr.pdf", scale = 0.5)

# Boxplot of F-scores per analysis type
ggplot(data=data, aes(x=analysisType, y=fscore)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 1)) + 
  stat_summary(fun.y=mean, geom="point", shape=5, size=2) + 
  theme_linedraw() +
  xlab("Analysis Technique") + ylab("F-score") +
  scale_x_discrete(limits=c("dynamic","static","hybrid"), labels=c("Dynamic", "Static", "Hybrid")) + 
  theme(axis.text=element_text(size=fontSize), axis.title=element_text(size=fontSize))

ggsave("./plots/fscore.pdf", scale = 0.5, height = 10, unit = "cm")

# Boxplot of precision per analysis type
ggplot(data=data, aes(x=analysisType, y=precision)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 1)) + 
  stat_summary(fun.y=mean, geom="point", shape=5, size=1) + 
  theme_linedraw() +
  xlab("Analysis Technique") + ylab("Precision") +
  scale_x_discrete(limits=c("dynamic","static","hybrid"), labels=c("Dynamic", "Static", "Hybrid")) + 
  theme(axis.text=element_text(size=fontSize), axis.title=element_text(size=fontSize))

ggsave("./plots/precision.pdf", scale = 0.5, height = 10, unit = "cm")

# Boxplot of recall per analysis type
ggplot(data=data, aes(x=analysisType, y=recall)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 1)) + 
  stat_summary(fun.y=mean, geom="point", shape=5, size=1) + 
  theme_linedraw() +
  xlab("Analysis Technique") + ylab("Recall") +
  scale_x_discrete(limits=c("dynamic","static","hybrid"), labels=c("Dynamic", "Static", "Hybrid")) + 
  theme(axis.text=element_text(size=fontSize), axis.title=element_text(size=fontSize))

ggsave("./plots/recall.pdf", scale = 0.5, height = 10, unit = "cm")

# Boxplot of precision and recall per analysis type
ggplot(data=longByPR, aes(x=analysisType, y=Value,fill=Measure)) +
  geom_boxplot(position=position_dodge(0.8), outlier.size = 0.5) +
  coord_cartesian(ylim = c(0, 1)) + 
  #stat_summary(fun.y=mean, geom="point", shape=5, size=1) + 
  theme_linedraw() +
  guides(fill=guide_legend(title="Metric")) + 
  xlab("Analysis Technique") + ylab("Precision and recall") +
  scale_x_discrete(limits=c("dynamic","static","hybrid"), labels=c("Dynamic", "Static", "Hybrid")) + 
  theme(axis.text=element_text(size=fontSize), axis.title=element_text(size=fontSize), legend.text=element_text(size=fontSize), legend.title=element_text(size=fontSize))

ggsave("./plots/pr.pdf", scale = 0.5, height = 10, unit = "cm")

# ggplot(data=data, aes(x=total, y=executionTime / 1000, colour=analysisType)) +
#   geom_line() + 
#   theme_linedraw() +
#   xlab("Total number of JavaScript functions") + ylab("Execution time (seconds)") +
#   labs(color="Analysis type") + 
#   theme(axis.text=element_text(size=fontSize), axis.title=element_text(size=fontSize), 
#         legend.text=element_text(size=fontSize), legend.title=element_text(size=fontSize))
