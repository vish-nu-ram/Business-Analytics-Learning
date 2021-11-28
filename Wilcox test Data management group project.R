library(tidyverse)
library(readxl)


df<- read_xlsx("Modify the NA of industry.xlsx")
# Picking the two columns we are interested in

df_tweet <- cbind(df$AVG_LK_PT_AC, df$AVG_LK_PT_COV_AC)

#Removing NA Values
df_tweet_clean <- df_tweet[complete.cases(df_tweet),]
na2 <-df_tweet_clean[,2]!= "NA"
df_tweet_clean2 <- df_tweet_clean[na2,]
summary(df_tweet_clean2)

##We are now left with 407 rows of data after removing the null values.

df_tweet_num <- as.data.frame(cbind(as.numeric(df_tweet_clean2[,1]), as.numeric(df_tweet_clean2[,2])))
summary(df_tweet_num)

## We can see that the mean likes per tweet relating to covid is much higher than meann likes in general.
##We would like to look at the impact of outliers on this statistics

boxplot(df_tweet_num)

# We can observe that there are way too many outliers in the data and that will definetly impact the mean values. 
# We will try to treat the outliers and then compare the two samples again

stats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  md <- median(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, median = md, stdev=s,min = min,max=max, UC=UC, LC=LC ))
}

stats(df_tweet_num[,1])
stats(df_tweet_num[,2])
summary(df_tweet_num[,1])
summary(df_tweet_num[,2])
IQR(df_tweet_num[,1])
IQR(df_tweet_num[,2])
# We will remove values over 3rd quartile or below 1st quartile
IQR(df_tweet_num[,1])
# 1st quartile - 1.5*IQR is a negative number, which dont exist in our data, so we only need to remove outliers that are greater than 3rd Q + 1.5*IQR
df_tweet_num2 <- subset(df_tweet_num, df_tweet_num[,1]<24+1.5*IQR(df_tweet_num[,1]) & df_tweet_num[,2]<30+1.5*IQR(df_tweet_num[,2]))
stats(df_tweet_num2)
stats(df_tweet_num2[,1])
stats(df_tweet_num2[,2])
boxplot(df_tweet_num2)

#Almost all outliers are now gone and we now have 332 rows remaining.
 
summary(df_tweet_num2)

# There is some difference in the two means after treating outliers , 
# so we will proceed with a statistical test to prove that mean of second column is significantly higher than first column

# Before doing a test we would like to understand the distribution of the underlying data

hist(df_tweet_num2[,1])
hist(df_tweet_num2[,2])

#The underlying data is clearly not normally distributed
shapiro.test(df_tweet_num2[,1])
shapiro.test(df_tweet_num2[,2])

# Let us try to transform the data into a normal curve

a<- sqrt(sqrt(df_tweet_num2[,1]))
b<- sqrt(sqrt(df_tweet_num2[,2]))

hist(a)
hist(b)

shapiro.test(a)
shapiro.test(b)

# Even though the histogram looks to tend to a normal curve, 
#the results from a shapiro test reveals that the data is still not normally distributed
# So instead of doing a T-Test , we will use a non-parametric test such as the Mann Whitney U Test

wilcox.test(df_tweet_num2$V1,df_tweet_num2$V2,paired = FALSE)
wilcox.test(df_tweet_num2$V2,df_tweet_num2$V1,paired = FALSE)
wilcox.test(df_tweet_num2$V1,df_tweet_num2$V2,paired = TRUE)
wilcox.test(df_tweet_num2$V2,df_tweet_num2$V1,paired = TRUE)


# T test anyways

t.test(df_tweet_num2$V1, df_tweet_num2$V2, paired = TRUE)
t.test(df_tweet_num2$V1, df_tweet_num2$V2, paired = FALSE)

## Both results show that the shift between the means is not 0, 
# i.e the difference between the mean likes is statistically significant
# => Average likes on covid relted posts are greater than average likes per tweet, when considering posts after lockdown
mean(df_tweet_num2$V1)
mean(df_tweet_num2$V2)

###########################################################################################
############     TEST TO COMPARE COVID RELATED POSTS TO SUPPLY CHAIN RELATED POSTS    #####
###########################################################################################

df_tweet <- cbind(df$FRAC_TWT_COV_AC , df$FRAC_TWT_SC_AC)

#Removing NA Values
df_tweet_clean <- df_tweet[complete.cases(df_tweet),]
na2 <-df_tweet_clean[,2]!= "NA"
df_tweet_clean2 <- df_tweet_clean[na2,]
summary(df_tweet_clean2)

##We are now left with 407 rows of data after removing the null values.

df_tweet_num <- as.data.frame(cbind(as.numeric(df_tweet_clean2[,1]), as.numeric(df_tweet_clean2[,2])))
summary(df_tweet_num)

## We can see that the mean likes per tweet relating to covid is much higher than meann likes in general.
##We would like to look at the impact of outliers on this statistics

boxplot(df_tweet_num)

# We can observe that there are way too many outliers in the data and that will definetly impact the mean values. 
# We will try to treat the outliers and then compare the two samples again

stats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  md <- median(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, median = md, stdev=s,min = min,max=max, UC=UC, LC=LC ))
}

stats(df_tweet_num[,1])
stats(df_tweet_num[,2])
summary(df_tweet_num[,1])
summary(df_tweet_num[,2])
IQR(df_tweet_num[,1])
IQR(df_tweet_num[,2])
# We will remove values over 3rd quartile or below 1st quartile
IQR(df_tweet_num[,1])
# 1st quartile - 1.5*IQR is a negative number, which dont exist in our data, so we only need to remove outliers that are greater than 3rd Q + 1.5*IQR
df_tweet_num2 <- subset(df_tweet_num, df_tweet_num[,1]<0.16587+1.5*IQR(df_tweet_num[,1]) & df_tweet_num[,2]<0.027397+1.5*IQR(df_tweet_num[,2]))
stats(df_tweet_num2)
stats(df_tweet_num2[,1])
stats(df_tweet_num2[,2])
summary(df_tweet_num2[,2])
summary(df_tweet_num2[,1])
boxplot(df_tweet_num2)

#Almost all outliers are now gone and we now have 332 rows remaining.

summary(df_tweet_num2)

# There is some difference in the two means after treating outliers , 
# so we will proceed with a statistical test to prove that mean of second column is significantly higher than first column

# Before doing a test we would like to understand the distribution of the underlying data

hist(df_tweet_num2[,1])
hist(df_tweet_num2[,2])

#The underlying data is clearly not normally distributed
shapiro.test(df_tweet_num2[,1])
shapiro.test(df_tweet_num2[,2])

# Let us try to transform the data into a normal curve

a<- sqrt(sqrt(df_tweet_num2[,1]))
b<- sqrt(sqrt(df_tweet_num2[,2]))
hist(a)
hist(b)

shapiro.test(a)
shapiro.test(b)

#the results from a shapiro test reveals that the data is still not normally distributed
# So instead of doing a T-Test , we will use a non-parametric test such as the Mann Whitney U Test

wilcox.test(df_tweet_num2$V1,df_tweet_num2$V2,paired = TRUE)
wilcox.test(df_tweet_num2$V2,df_tweet_num2$V1,paired = TRUE)
#wilcox.test(df_tweet_num2$V1,df_tweet_num2$V2,paired = FALSE)
#wilcox.test(df_tweet_num2$V2,df_tweet_num2$V1,paired = FALSE)

# T test anyways

t.test(df_tweet_num2$V1, df_tweet_num2$V2,paired = TRUE)

## Both results show that the shift between the means is not 0, 
# i.e the difference between the mean likes is statistically significant
# => Average likes on covid related posts are greater than average likes per tweet, when considering posts after lockdown
mean(df_tweet_num2$V1)
mean(df_tweet_num2$V2)

###################################### Trinity College Dublin  ######################################
##############################    MSc Business Analytics 2021-22   ##################################

#%#\,             o                            ,s                                      ', .3 ~;@QMQ2b
###wl*                                        #8@5w                                     .l;-w#Q@Q@#QO
#l8##bx                ;:                     @b#fb                                   * IJQ@@##@##5#@
#####^b7    GGSpSp     '                     ,N####,                                 - ,a;sy7Q2i#@7bN
#9@S@#-- . ^*GGGGGGG                       ,#Q3p;QbQb                                 .-AW8@SGbbj\b##
#{(`       GGG GGGGG~                     (##7`^^"`#@#p                                ^.NlG\^^3WwS#Q
#&9#} ^'^^**GGGo  G                       `Sj ^,mp^jp#                                'bs#Wbpf.aQp@##
#              '''^                        G3#!@#C #bb                                1"X"]p8s#7@#f;5
# .                           ^            Gjb!@#b #bb                                  '`  (T^QSM#$M
#2N-~*o..                                  Gj# @#b jbb                                    ^.^@j##TQ#*
#*C9p"^; /                                wN@^`88b`7bN,                                ~~ "l@"@wGb#sS
#kQ"#b"C".:o  ,;                         #v,sS#m####ppGw                                -=J=:#/'z,G9#
#3WWww#%w#"Wb.,.                        [8#@GGSS@@##@N##p                                 ."" <'wjyb#
##*f%u%^b,"T\`l*  ..,                  ;##f@########@####                        .~G^a~<jb,}w"',`@"S2
#\@bQ@##QQ######N###'@N,             "#####################              ;N ,QNNN#NN#####@@l$#NQQQ;D#
######QS9Q8O,bG#f77,7GGC3G,  ,         S,,,SSGSSGSSGSQ,,lS         , ,eW 3G"\}77. l$#b#$$####@QW####Q
###Q@@######QQ3bG9''Q99G7H#GS,@        (,lpjGGs###QGGplbjG       ,b,GSw"`^G''',G?9fb3##T#b373#@#N@##G
#b@#N@###@##b*~  **  boG^.##N,`~<~**~~;ss$pGG#"  `"#$G#NWGppppw#pp,;pNl' ^'*..^b;S@####N#l@####@Q####
##@@#######@##b("#  @p G*b  [b^s. .   $l3@G3@b**y**]GG@G3#p9S   *G|[ | * C @^~GGG$@S#######G##@7#@###
##@###@##3b3#W#^ \~~" GS,G     bdGlpQ@QQp@p#@Q .* .(j@Q#NQQ$N#Sb#lG       * "  ^"8G#Q##3@##@##@####bQ#
#####5GGb####M##"@####@b$Gj b jbpQ}QNlS#G@##@bp;#S$j@#@###@@Q@bQ@l   j ' %G"9 ;ss#Q#G8@b2#N##########
#######@Q#s#b$@@w@3GSQlGGN#Ny###QQ@QQ#####G##Q@8QN##@########Q#@@@smCQ] #$G@#w#Sp@#sGp8,pQS@#@###b' ,
#@#bJ3lILs####Nb,..lb83OC!l'` b      ,#QS#@Q##@#####QQ#Q###G  /S8  jw 3dbj.m@@#N####wpsb}p3I@#QQpQQQQ
###################SSpSSSSSSGGGGGGSGGGGGGGbb^'^'''''"@333GGGG3############S##@####Q##################
######SGGSl$#SQQ#S##Q#QS###l##SS#####Q###b^      .    '8##lSSSlQGSGQGGSSGGGGGGlGGGGGGGGGGGGGGGGGGGG##
#"""""""""""""""""""""""""""""""""""""""^^^^^^^^^^^^^^^^^^"""""""""""""""""""""""""""""""""""""""""""
