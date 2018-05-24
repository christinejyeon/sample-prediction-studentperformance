thedata_lit <- read.csv(file="student-por.csv",header=TRUE,sep=";")
thedata_lit$average <- round((thedata_lit$G1 + thedata_lit$G2 + thedata_lit$G3)/3,0)
thedata_lit$grade <- ifelse(thedata_lit$average>=17,"Excellent", ifelse(thedata_lit$average>=14,"Good", ifelse(thedata_lit$average>=11,"Fair", ifelse(thedata_lit$average>=8,"Poor", "Fail"))))
thedata_lit$grade <- as.factor(thedata_lit$grade)

sampling_lit <- sample(2, nrow(thedata_lit), replace=TRUE, prob=c(0.7,0.3))
trainDataLit <- thedata_lit[sampling_lit==1,]
testDataLit <- thedata_lit[sampling_lit==2,]
littree <- tree(grade ~ sex+age+address+famsize+Pstatus+Medu+Fedu+Mjob+reason+guardian+traveltime+studytime+failures+schoolsup+famsup+paid+activities+nursery+higher+internet+romantic+famrel+freetime+goout+Dalc+Walc+health+absences, data=trainDataLit)
littree_cv <- cv.tree(littree, FUN=prune.tree)
print(littree_cv)
plot(littree_cv)
littree_final <- prune.tree(littree, best=5)
print(littree_final)
plot(littree_final, type="u")
text(littree_final, col=pal)

littree_pre <- predict(littree_final, newdata = testDataLit, type="class")
table(littree_pre, testDataLit$grade)
confusionMatrix(littree_pre, testDataLit$grade)
