

environmental_matrix <- as.matrix(raw_data[,-c(1:7)])
princomp(environmental_matrix)
head(environmental_matrix)
is.numeric(environmental_matrix)
is.data.frame(raw_data)

newenvi_matrix<- matrix(as.numeric(environmental_matrix), nrow(environmental_matrix), 
                        ncol(environmental_matrix))
princomp(scale(environmental_matrix))$loadings
summary(princomp(environmental_matrix))

hist(environmental_matrix[,8])

princomp_score <- princomp(scale(environmental_matrix))$scores
summary(lm(log_CBL~princomp_score))

plot(princomp_score, col = hsv(h = (log_CBL-min(log_CBL))/
                                 (max(log_CBL)-min(log_CBL))))

plot(princomp_score, col = hsv(h = (princomp_score[,1]-min(princomp_score[,1]))/
                                 (max(princomp_score[,1])-min(princomp_score[,1]))))
