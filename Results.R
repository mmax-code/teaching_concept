library("ggplot2")
library("reshape2")

df = read.csv("data.txt", sep="")

###############################################################################
# Figure 1 (a-d)
###############################################################################

### (a) full sample

proz1 = (df$estimate1-df$wahr)/df$wahr*100
proz2 = (df$estimate2-df$wahr)/df$wahr*100

data.proz = data.frame(I = proz1, II = proz2, id = 1:length(proz1))
data.proz$delta = "negative"

data.proz$delta[data.proz$II<data.proz$I] = "positive"
data.gg = melt(data.proz, id.vars = c("id","delta"))

ggplot(data.gg, aes(x = variable, y = value, group = id, colour = delta)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme_minimal() +
  #theme_few() +
  theme(legend.position = "none") +
  ylab("Difference in %") +
  xlab("Phase") + 
  xlim("I", "II") + 
  theme(axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 15))


t.test(proz1, alternative = "greater")
t.test(proz2, alternative = "greater")
t.test(proz1,proz2, paired = TRUE, alternative = "greater")

shapiro.test(proz1)
shapiro.test(proz2)
shapiro.test(proz1-proz2)

wilcox.test(proz1, exact = TRUE, alternative = "greater")
wilcox.test(proz2, exact = TRUE, alternative = "greater")
wilcox.test(proz1,proz2, paired = TRUE, exact = TRUE, alternative = "greater")


### (b) grades

df.grades = df[df$grade == "high", ] 
proz1 = (df.grades$estimate1-df.grades$wahr)/df.grades$wahr*100
proz2 = (df.grades$estimate2-df.grades$wahr)/df.grades$wahr*100

data.proz = data.frame(I = proz1, II = proz2, id = 1:length(proz1))
data.proz$delta = "negative"

data.proz$delta[data.proz$II<data.proz$I] = "positive"
data.gg = melt(data.proz, id.vars = c("id","delta"))

ggplot(data.gg, aes(x = variable, y = value, group = id, colour = delta)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme_minimal() +
  #theme_few() +
  theme(legend.position = "none") +
  ylab("Difference in %") +
  xlab("Phase") + 
  xlim("I", "II") + 
  theme(axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 15))



t.test(proz1, alternative = "greater")
t.test(proz2, alternative = "greater")
t.test(proz1,proz2, paired = TRUE, alternative = "greater")

shapiro.test(proz1)
shapiro.test(proz2)
shapiro.test(proz1-proz2)

wilcox.test(proz1, exact = TRUE, alternative = "greater")
wilcox.test(proz2, exact = TRUE, alternative = "greater")
wilcox.test(proz1,proz2, paired = TRUE, exact = TRUE, alternative = "greater")




### (c) overestimation

df.over = df[df$estimate1 > df$wahr   , ] 
proz1 = (df.over$estimate1-df.over$wahr)/df.over$wahr*100
proz2 = (df.over$estimate2-df.over$wahr)/df.over$wahr*100

data.proz = data.frame(I = proz1, II = proz2, id = 1:length(proz1))
data.proz$delta = "negative"

data.proz$delta[data.proz$II<data.proz$I] = "positive"
data.gg = melt(data.proz, id.vars = c("id","delta"))

ggplot(data.gg, aes(x = variable, y = value, group = id, colour = delta)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme_minimal() +
  #theme_few() +
  theme(legend.position = "none") +
  ylab("Difference in %") +
  xlab("Phase") + 
  xlim("I", "II")+ 
  theme(axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 15))



t.test(proz1, alternative = "greater")
t.test(proz2, alternative = "greater")
t.test(proz1,proz2, paired = TRUE, alternative = "greater")

shapiro.test(proz1)
shapiro.test(proz2)
shapiro.test(proz1-proz2)

wilcox.test(proz1, exact = TRUE, alternative = "greater")
wilcox.test(proz2, exact = TRUE, alternative = "greater")
wilcox.test(proz1,proz2, paired = TRUE, exact = TRUE, alternative = "greater")



### (d) gender

df.gender = df[df$sex == "w"  , ] 
proz1 = (df.gender$estimate1-df.gender$wahr)/df.gender$wahr*100
proz2 = (df.gender$estimate2-df.gender$wahr)/df.gender$wahr*100

data.proz = data.frame(I = proz1, II = proz2, id = 1:length(proz1))
data.proz$delta = "negative"

data.proz$delta[data.proz$II<data.proz$I] = "positive"
data.gg = melt(data.proz, id.vars = c("id","delta"))

ggplot(data.gg, aes(x = variable, y = value, group = id, colour = delta)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme_minimal() +
  #theme_few() +
  theme(legend.position = "none") +
  ylab("Difference in %") +
  xlab("Phase") + 
  xlim("I", "II") + 
  theme(axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 15))



t.test(proz1, alternative = "greater")
t.test(proz2, alternative = "greater")
t.test(proz1,proz2, paired = TRUE, alternative = "greater")

shapiro.test(proz1)
shapiro.test(proz2)
shapiro.test(proz1-proz2)

wilcox.test(proz1, exact = TRUE, alternative = "greater")
wilcox.test(proz2, exact = TRUE, alternative = "greater")
wilcox.test(proz1,proz2, paired = TRUE, exact = TRUE, alternative = "greater")








