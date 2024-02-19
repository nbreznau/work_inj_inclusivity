# from their R code in OSF repo
#' Reverse Scoring and Computing Composite Individualist Practices 
psych::alpha(as.matrix(CCHANGE[,c('famsize','alone','oldalone','divorce')]), check.keys=TRUE) #Checking reliability, famsize and chborn are automatically reversed in this analysis (see - sign) reversed

CCHANGE$zfamsize.raw<- scale(CCHANGE$famsize, center=TRUE, scale=TRUE) #Standardizing variables - Household size
CCHANGE$zalone.raw<- scale(CCHANGE$alone, center=TRUE, scale=TRUE) # Living alone
CCHANGE$zoldalone.raw<- scale(CCHANGE$oldalone, center=TRUE, scale=TRUE) # Older adults living alone
CCHANGE$zdivorce.raw<- scale(CCHANGE$divorce, center=TRUE, scale=TRUE) # Divorce
CCHANGE$zfamsize.raw<-CCHANGE$zfamsize.raw[,] #Removing attributes R attached to the bottom of the standardized variables
CCHANGE$zalone.raw<-CCHANGE$zalone.raw[,]
CCHANGE$zoldalone.raw<-CCHANGE$zoldalone.raw[,]
CCHANGE$zdivorce.raw<-CCHANGE$zdivorce.raw[,]
CCHANGE$zfamsize.raw.r<-CCHANGE$zfamsize.raw*-1 #Reversing household size
CCHANGE$Practices.raw<-apply(CCHANGE[,c('zfamsize.raw.r','zalone.raw','zoldalone.raw','zdivorce.raw')], 1, mean, na.rm=TRUE) #Computing mean
#' Computing Importance of Friends vs. Family (Difference Score) 
CCHANGE$FriendsFam.raw <- CCHANGE$friends.imp - CCHANGE$fam.imp
CCHANGE$FriendsFam.raw[is.na(CCHANGE$friends.imp)] <- NA
CCHANGE$FriendsFam.raw[is.na(CCHANGE$fam.imp)] <- NA
#' Computing Composite Individualist Values 
cor(CCHANGE[,c('FriendsFam.raw','ind.child','PostMat')], use='pairwise.complete.obs', method='kendall') 
psych::alpha(as.matrix(CCHANGE[,c('FriendsFam.raw','ind.child','PostMat')]), check.keys=TRUE) #Checking reliability
CCHANGE$zFriendsFam.raw<- scale(CCHANGE$FriendsFam.raw, center=TRUE, scale=TRUE) #Standardizing variables - Friends-Family
CCHANGE$zIndChild.raw<- scale(CCHANGE$ind.child, center=TRUE, scale=TRUE) #Independent Children
CCHANGE$zPostMat.raw<- scale(CCHANGE$PostMat, center=TRUE, scale=TRUE) #Preference for self-expression
CCHANGE$zFriendsFam.raw<-CCHANGE$zFriendsFam.raw[,] #Removing attributes R attached to the bottom of the standardized variables
CCHANGE$zIndChild.raw<-CCHANGE$zIndChild.raw[,]
CCHANGE$zPostMat.raw<-CCHANGE$zPostMat.raw[,]
CCHANGE$Values.raw <- apply(CCHANGE[,c('zFriendsFam.raw','zIndChild.raw','zPostMat.raw')], 1, mean, na.rm=TRUE) 
