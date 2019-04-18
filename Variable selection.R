prob1<- read.csv("MUDAC_data_Problem1.csv", header=TRUE)

#DATA CLEANING
prob1=prob1[-1,]
prob1[] <- lapply(prob1, as.character)
colnames(prob1) <- prob1[1, ]
prob1 <- prob1[-1 ,]
prob1<- prob1[-c(51,52),]
write.csv(prob1, file = "Problem1_data.csv")
prob1_temp<-cbind(prob1[,1], prob1_temp)
colnames(prob1_temp)[1]<- "Names"

#correlation
prob1_temp<- data.frame(apply(prob1[,2:20], 2, function(x) as.numeric(sub("\\%", "", x))))

install.packages("corrplot")
library(corrplot)
prob1_cor<- cor(prob1_temp)
corrplot(prob1_cor, method="circle")
cor(prob1_temp$Total.Suspended.Solid..TSS., prob1_temp$Nitrate.)



#elsatic_net
library(caret)
MSE_matrix<-matrix(nrow=nrow(prob1_temp),ncol=2)
for(i in 1:nrow(prob1_temp)){
    data <- prob1_temp[,-1] #remove names column
    test<-data[i,] #test data with ith row
    train.data <- data[-1,] # remove ith row
    rctrl1 <- trainControl(method = "cv", number = 5, returnResamp = "all")
    mod_TSS <- train(x=train.data[,-c(16:17)], y=train.data$Total.Suspended.Solid..TSS., 
                     method='enet', trControl = rctrl1) #model for TSS 
    mod_nit <- train(x=train.data[,-c(16:17)], y=train.data$Nitrate., method='enet', 
                     trControl = rctrl1) #model for nitrate
    #prediction
    pred_TSS<- predict(mod_TSS, test) 
    pred_nit<- predict(mod_nit, test)
    
    #output
    MSE_matrix[i,1]<- mean((test_TSS$Total.Suspended.Solid..TSS. - pred_TSS) ^ 2)
    MSE_matrix[i,2]<- mean((test_nit$Nitrate. - pred_nit) ^ 2)
}
MSE_table<- as.data.frame(MSE_matrix) #convert matrix to data frame
MSE_table<- cbind(as.character(prob1_temp$Names), MSE_table) #include watershed names
colnames(MSE_table)<- c("Watershed", "TSS MSE", "Nitrate MSE")

#plotting
rotate_x <- function(data, column_to_plot, labels_vec, rot_angle) {
    plt <- barplot(data[[column_to_plot]], col='steelblue', xaxt="n")
    text(plt, par("usr")[3], labels = labels_vec, srt = rot_angle, adj = c(1.1,1.1), 
         xpd = TRUE, cex=0.6) 
}
rotate_x(MSE_table, 'Nitrate MSE', MSE_table$Watershed, 45)
hist(MSE_table$`TSS MSE`, main= "Nitrate MSE", xlab="MSE")
plot(MSE_table$Watershed, MSE_table$`TSS MSE`)
hist(MSE_table$`TSS MSE`, main= "Nitrate MSE", xlab="MSE")
plot(MSE_table$Watershed, MSE_table$`TSS MSE`)

write.csv(MSE_table, file = "Problem1_MSEtable.csv")

########
write.csv(prob1_temp, file = "Problem1_mudac.csv")


########
mod <- lm(Total.Suspended.Solid..TSS. ~ ., data=prob1_temp)
cooksd <- cooks.distance(mod)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),
                                                   names(cooksd),""), col="red")  # add labels
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
head(ozone[influential, ])
car::outlierTest(mod)
