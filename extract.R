library(stats)
library(plyr)
library(reshape2)

# ------------
# Prepare data
# ------------
tableName = 'MoocsData/events_1000_Users.csv'
events = read.csv(tableName )
tableName = 'MoocsData/Userinfo.csv'
userInfo = read.csv(tableName )

# Determine sessions
events = events[order(events$UserID,events$TimeStamp),]
n = nrow(events)
events$TimeDiff[2:n] = events$TimeStamp[2:n] - events$TimeStamp[1:(n-1)] 
events$TimeDiff[1] = 0
events$userEnd[2:n] = (events$UserID[2:n] != events$UserID[1:(n-1)])
events$userEnd[1] = FALSE
events$sessionEnd = (events$TimeDiff > 60)
events$sessionEnd[1] = FALSE
events$newSession = events$sessionEnd | events$userEnd
events$sessionID = cumsum(events$newSession)

# Choose relevant columns
E = events[,c(4,5,6,14)]
UI = userInfo[,c(50, 16)]

TS = aggregate(E$TimeStamp, by=list(sessionID=E$sessionID), FUN = function(vec){ max(vec) - min(vec)} )
colnames(TS)[2] = "time"
TS$time = TS$time + 1

# Generic function for reading measure value
apply.measure = function(E,measure,mname,persession=TRUE){
  M = aggregate(E$EventType, by=list(UserID=E$UserID,sessionID=E$sessionID), FUN = measure)
  M = merge(M,TS)
  if (persession){
    M$x = M$x / M$time
  }
  U = aggregate(M$x, by=list(UserID = M$UserID), FUN = mean)
  
  
  colnames(U)[2] = mname
  U
}

# -------------------
# Measure definitions
# -------------------

users = UI
E = E[1:100000,]

# Measure 1
measure = function(vec){
  length(vec)
}
users = merge(users,apply.measure(E,measure,"M1"))

# Measure 2
measure = function(vec){
  sum(vec=="Video.Play") * 10 +
    sum(vec=="Forum.Thread.PostOn") * 10 +
    sum(vec=="Problem.Check") * 60
}
users = merge(users,apply.measure(E,measure,"M2"))

# Measure 3
measure = function(vec){
  m = 0
  m = m + 0.5*( sum(vec == "Video.Play")  + sum(vec=='Forum.Subscribe') + sum(vec=='Forum.ThreadSubscribe') + sum(vec=='Forum.Thread.View') + sum(vec=='Forum.Load'))
  m = m + (sum(vec=='Forum.Post.Downvote') + sum(vec=='Forum.Post.Upvote') + sum(vec=='Forum.Post.Downvote') + sum(vec=='Forum.Post.Upvote'))
  m = m + (sum(vec== 'Forum.Thread.CommentOn') + sum(vec == "Forum.Thread.PostOn") + sum(vec == "Problem.Check") + sum(vec== 'Forum.Thread.Launch') ) * 2 
  sum(m)
}
users = merge(users,apply.measure(E,measure,"M3"))

# Measure 4
measure = function(vec){
  length(unique(vec))
}
users = merge(users,apply.measure(E,measure,"M4"))

# -----------------------------
# Build models and show results
# -----------------------------
M = lm(Grade ~ M1,data=users[,-1])
summary(M)
plot(users$M1,users$Grade)
abline(M)

M = lm(Grade ~ M2,data=users[,-1])
summary(M)
plot(users$M2,users$Grade)
abline(M)

M = lm(Grade ~ M3,data=users[,-1])
summary(M)
plot(users$M3,users$Grade)
abline(M)

M = lm(Grade ~ M4,data=users[,-1])
summary(M)
plot(users$M4,users$Grade)
abline(M)
