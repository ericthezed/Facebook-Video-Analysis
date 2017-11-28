# Load data
videodata_2015_FB <- read.csv("videodata_2015_FB1.csv")
videodata <- data.frame(videodata_2015_FB)

# Remove unneeded rows
videodata <- videodata[-c(133),]

# Convert variable types
videodata$VIDEO.Video.URL <- as.character(videodata$VIDEO.Video.URL)
videodata$VIDEO.Published.Date <- as.Date(videodata$VIDEO.Published.Date, format = "%m/%d/%Y")

videodata$Targeted. <- factor(videodata$Targeted, levels = c("N", "Y"), labels = c("No", "Yes"))
videodata$Repost. <- factor(videodata$Repost, levels = c("N", "Y"), labels = c("No", "Yes"))
videodata$Bug. <- factor(videodata$Bug., levels = c("N", "Y"), labels = c("No", "Yes"))

# Name variables
names(videodata) <- c("URL", "Date", "Thirty_Views_Organic", "Thirty_Views_Paid", "Thirty_Views_Total", "Complete_Views_Organic", "Complete_Views_Paid", "Complete_Views_Total", "Views_Organic", "Views_Paid", "Views_Total", "Shares", "Reaches", "Fan_Reaches", "Likes", "Comments", "Negative_Feedback", "Targeted", "Repost", "Bug")

# Compute new variables
videodata$days_since_first_post <- as.numeric(videodata$Date - as.Date("2015-01-07"))
videodata$ratio_thirty_views_anyviews <- videodata$Thirty_Views_Total/videodata$Views_Total
videodata$reaches_per_fanreach <- videodata$Reaches/videodata$Fan_Reaches
videodata$shares_per_reach <- videodata$Shares/videodata$Reaches
videodata$likes_per_reach <- videodata$Likes/videodata$Reaches
videodata$comments_per_reach <- videodata$Comments/videodata$Reaches
videodata$neg_feedback_per_reach <- videodata$Negative_Feedback/videodata$Reaches # Hide post, hide all posts from page, unlie=ke page, report spam
videodata$views_per_reach <- videodata$Views_Total/videodata$Reaches
videodata$thirty_views_per_reach <- videodata$Thirty_Views_Total/videodata$Reaches

# Correlation Matrix
library("dplyr")

cor(subset(videodata, select = c(Thirty_Views_Total, ratio_thirty_views_anyviews, reaches_per_fanreach, shares_per_reach, likes_per_reach, comments_per_reach, neg_feedback_per_reach, views_per_reach, thirty_views_per_reach, Days_since_first_post)), method = "pearson", use = "pairwise")
cor(subset(videodata_2, select = c(Thirty_Views_Total, ratio_thirty_views_anyviews, reaches_per_fanreach, shares_per_reach, likes_per_reach, comments_per_reach, neg_feedback_per_reach, views_per_reach, thirty_views_per_reach, Days_since_first_post)), method = "pearson", use = "pairwise")


# Step-wise regression
library("MASS")
library("QuantPsyc")

# Forward
step_forward <- stepAIC(video_perform_a1, direction = "forward")
summary(step_forward)
lm.beta(step_forward)
anova(step_forward)

step_forward_b <- stepAIC(video_perform_b1, direction = "forward")
summary(step_forward_b)
lm.beta(step_forward_b)
anova(step_forward_b)

# Backward
step_backward <- stepAIC(video_perform_a1, direction = "backward")
summary(step_backward)
lm.beta(step_backward)
anova(step_backward)

step_backward_b <- stepAIC(video_perform_b1, direction = "backward")
summary(step_backward_b)
lm.beta(step_backward_b)
anova(step_backward_b)

# Both
step_both <- stepAIC(video_perform_a1, direction = "both")
summary(step_both)
lm.beta(step_both)
anova(step_both)

step_both_b <- stepAIC(video_perform_b1, direction = "both")
summary(step_both_b)
lm.beta(step_both_b)
anova(step_both_b)


# Multiple regression models

video_perform_a1 <- lm(Thirty_Views_Total ~ ratio_thirty_views_anyviews + reaches_per_fanreach + shares_per_reach + likes_per_reach + comments_per_reach + neg_feedback_per_reach + views_per_reach + thirty_views_per_reach + Days_since_first_post + Targeted + Repost + Bug, data = videodata)
summary(video_perform_a1)
lm.beta(video_perform_a1)

video_perform_a2 <- lm(Thirty_Views_Total ~ reaches_per_fanreach + neg_feedback_per_reach + thirty_views_per_reach + Days_since_first_post + Targeted, data = videodata)
summary(video_perform_a2)
lm.beta(video_perform_a2)

video_perform_a3 <- lm(Thirty_Views_Total ~ reaches_per_fanreach + neg_feedback_per_reach + thirty_views_per_reach + Days_since_first_post, data = videodata)
summary(video_perform_a3)
lm.beta(video_perform_a3)

video_perform_a4 <- lm(Thirty_Views_Total ~ reaches_per_fanreach, data = videodata)
summary(video_perform_a4)
lm.beta(video_perform_a4)



video_perform_b1 <- lm(Thirty_Views_Total ~ ratio_thirty_views_anyviews + reaches_per_fanreach + shares_per_reach + likes_per_reach + comments_per_reach + neg_feedback_per_reach + views_per_reach + thirty_views_per_reach + Days_since_first_post + Targeted + Repost + Bug, data = videodata_2)
summary(video_perform_b1)
lm.beta(video_perform_b1)

video_perform_b2 <- lm(Thirty_Views_Total ~ reaches_per_fanreach + likes_per_reach + neg_feedback_per_reach + thirty_views_per_reach + Days_since_first_post, data = videodata_2)
summary(video_perform_b2)
lm.beta(video_perform_b2)

video_perform_b3 <- lm(Thirty_Views_Total ~ reaches_per_fanreach + thirty_views_per_reach + Days_since_first_post, data = videodata_2)
summary(video_perform_b3)
lm.beta(video_perform_b3)

# video_perform_b3_1 <- lm(Thirty_Views_Total ~ reaches_per_fanreach*thirty_views_per_reach*Days_since_first_post, data = videodata_2)
# summary(video_perform_b3_1)
# lm.beta(video_perform_b3_1)

# video_perform_b3_2 <- lm(Thirty_Views_Total ~ reaches_per_fanreach + thirty_views_per_reach + Days_since_first_post + reaches_per_fanreach:thirty_views_per_reach, data = videodata_2)
# summary(video_perform_b3_2)
# lm.beta(video_perform_b3_2)

video_perform_b4 <- lm(Thirty_Views_Total ~ reaches_per_fanreach, data = videodata_2)
summary(video_perform_b4)
lm.beta(video_perform_b4)


# Collinearity tests
# library("usdm")
# vif(videodata)[c(videodata$Days_since_first_post, videodata$ratio_thirty_views_anyviews, videodata$reaches_per_fanreach, videodata$shares_per_reach, videodata$likes_per_reach, videodata$comments_per_reach, videodata$neg_feedback_per_reach, videodata$views_per_reach, videodata$thirty_views_per_reach, videodata$Targeted, videodata$Repost, videodata$Bug)]

# Compute residuals
# videodata$thirty_views_residuals <- videodata$reaches_per_fanreach - (-585559 + 367314*videodata$reaches_per_fanreach)

# Remove outliers
cook <- cooks.distance(video_perform_4)
which(cook >=1)
which(cook >= 4/length(videodata[,1]))
videodata_2 <- videodata[-c(7,9,26,27,31,114,115),]


# Plot histograms
hist(videodata$reaches_per_fanreach)
hist(videodata$Thirty_Views_Total)

hist(videodata_2$reaches_per_fanreach)
hist(videodata_2$Thirty_Views_Total)


# Scatterplots with Regression Line
library("ggplot2")

ggplot(videodata, aes(reaches_per_fanreach, Thirty_Views_Total)) + geom_point(shape = 1) + geom_smooth(method = lm) + xlab("Reaches per Fan Reach") + ylab("Facebook Video Views (>= 30s)") + ggtitle("Reaches per Fan Reach vs. Facebook Video Views (>= 30s)")
ggplot(videodata_2, aes(reaches_per_fanreach, Thirty_Views_Total)) + geom_point(shape = 1) + geom_smooth(method = lm)+ xlab("Reaches per Fan Reach") + ylab("Facebook Video Views (>= 30s)") + ggtitle("Reaches per Fan Reach vs. Facebook Video Views (>= 30s)")

ggplot(videodata_2, aes(thirty_views_per_reach, Thirty_Views_Total)) + geom_point(shape = 1) + geom_smooth(method = lm)+ xlab("Facbebook Video Views (>= 30s) per Reach") + ylab("Facebook Video Views (>= 30s)") + ggtitle("Facbebook Video Views (>= 30s) per Reach vs. Facebook Video Views (>= 30s)")
ggplot(videodata_2, aes(likes_per_reach, Thirty_Views_Total)) + geom_point(shape = 1) + geom_smooth(method = lm)+ xlab("Likes per Reach") + ylab("Facebook Video Views (>= 30s)") + ggtitle("Likes per Reach vs. Facebook Video Views (>= 30s)")
ggplot(videodata_2, aes(neg_feedback_per_reach, Thirty_Views_Total)) + geom_point(shape = 1) + geom_smooth(method = lm)+ xlab("Negative Feedback per Reach") + ylab("Facebook Video Views (>= 30s)") + ggtitle("Negative Feedback per Reach vs. Facebook Video Views (>= 30s)")


# Add regression equation and R^2

p <- ggplot(videodata, aes(reaches_per_fanreach, Thirty_Views_Total)) + geom_point(shape = 1) + geom_smooth(method = lm) + xlab("Reaches per Fan Reach") + ylab("Facebook Video Views (>= 30s)") +ggtitle("Reaches per Fan Reach vs. Facebook Video Views (>= 30s)")

lm_eqn <- function(videodata){
  m <- lm(Thirty_Views_Total ~ reaches_per_fanreach, videodata);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

p1 <- p + geom_text(x = 25, y = 300, label = lm_eqn(videodata), parse = TRUE)
p1

# Reaches per Fan Reach

q <- ggplot(videodata_2, aes(reaches_per_fanreach, Thirty_Views_Total)) + geom_point(shape = 1) + geom_smooth(method = lm) + xlab("Reaches per Fan Reach") + ylab("Facebook Video Views (>= 30s)") +ggtitle("Reaches per Fan Reach vs. Facebook Video Views (>= 30s)")
m <- lm(Thirty_Views_Total ~ reaches_per_fanreach, videodata_2)
a <- format(coef(m)[1], digits = 2) 
b <- format(coef(m)[2], digits = 2) 
r2 <- format(summary(m)$r.squared, digits = 3)
textlab <- paste("y = ",a," + ",b,"x, ","R^2 =",r2, sep="")
print(textlab)

q1 <- q + annotate("text", x = 2.5, y = 1250000, label = textlab, color="black", size = 5, parse=FALSE)
q1

# 30s Views per Reach

r <- ggplot(videodata_2, aes(thirty_views_per_reach, Thirty_Views_Total)) + geom_point(shape = 1) + geom_smooth(method = lm) + xlab("Facebook Video Views (>= 30s) per Reach") + ylab("Facebook Video Views (>= 30s)") +ggtitle("Facebook Video Views (>= 30s) per Reach vs. Facebook Video Views (>= 30s)")
m <- lm(Thirty_Views_Total ~ thirty_views_per_reach, videodata_2)
a <- format(coef(m)[1], digits = 2) 
b <- format(coef(m)[2], digits = 2) 
r2 <- format(summary(m)$r.squared, digits = 3)
textlab <- paste("y = ",a," + ",b,"x, ","R^2 =",r2, sep="")
print(textlab)

r1 <- r + annotate("text", x = 0.05, y = 1250000, label = textlab, color="black", size = 5, parse=FALSE)
r1


# Days Since Earliest Post

s <- ggplot(videodata_2, aes(Days_since_first_post, Thirty_Views_Total)) + geom_point(shape = 1) + geom_smooth(method = lm) + xlab("# Days Since Earliest Post (1-07-15)") + ylab("Facebook Video Views (>= 30s)") +ggtitle("# Days Since Earliest Post (1-07-15) vs. Facebook Video Views (>= 30s)")
m <- lm(Thirty_Views_Total ~ Days_since_first_post, videodata_2)
a <- format(coef(m)[1], digits = 2) 
b <- format(coef(m)[2], digits = 2) 
r2 <- format(summary(m)$r.squared, digits = 3)
textlab <- paste("y = ",a," + ",b,"x, ","R^2 =",r2, sep="")
print(textlab)

s1 <- s + annotate("text", x = 75, y = 1250000, label = textlab, color="black", size = 5, parse=FALSE)
s1

