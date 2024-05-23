require(readr)
require(ggcorrplot)
require(FactoMineR)
require(factoextra)
require(maps)
# Read in the Possum dataset.
# Didn't like the raw data, so encoding required altering.
raw_data = read_csv("data/Possum_data.csv", locale = locale(encoding = "ISO-8859-1"))

# Change from a tibble.
data <- as.data.frame(raw_data)
head(data)

# Check for null values.
# colSums(is.na(data))

# Removing non-numeric data.
numeric_data <- data[,sapply(data,is.numeric)]
head(numeric_data)

# Remove unwanted columns.
filtered_data <- numeric_data[-c(2:3, 15:18)]
head(filtered_data)

# Normalise the numeric data.
normalised_data <- scale(filtered_data)
head(normalised_data)

# Create and plot correlation matrix.
corr_matrix <- cor(normalised_data)
ggcorrplot(corr_matrix)

# Principal Components Analysis (PCA).
normalised_data.pca <- princomp(corr_matrix)
summary(normalised_data.pca)

# Viewing first two components.
normalised_data.pca$loadings[,1:2]

# Visualising PCA.
fviz_eig(normalised_data.pca, addlabels = TRUE) + labs(title = NULL, x = 'Principle Components')
fviz_pca_var(normalised_data.pca, col.var = "black")
fviz_cos2(normalised_data.pca, choice = "var", axes = 1:2)
fviz_pca_var(normalised_data.pca, col.var = "cos2", 
             gradient.cols = c("red", "purple", "lightgreen"), 
             repel = TRUE)


head(raw_data)
raw_data$CBL
hist(log(raw_data$CBL), breaks = 30, xlab = "log(CBL)", main = NULL)
# Test for normality
log_CBL <- log(raw_data$CBL)
shapiro.test(log_CBL)

shapiro.test(raw_data$CBL)

#map(raw_data[,"Longitude"], raw_data[,"Latitude"])
is.array(raw_data$Latitude)
raw_data[,"Latitude"]
is.data.frame(raw_data)
lat <- as.vector(raw_data$Latitude)
long <- as.vector(raw_data$Longitude)

is.array(lat)

plot(long,lat)
#map(long,lat)
is.numeric(lat)
is.vector(lat)
map(regions = "Australia", xlim = c(110,160), mar = c(1, 1, 1, 1))
points(long,lat, cex = (log_CBL/4)^6, col = hsv(h = (log_CBL-min(log_CBL))/
                                                  (max(log_CBL)-min(log_CBL))))
# example of geographic variation in size consistent with Bergmann's rule

plot(rank(raw_data$`Soil bulk density (0  30 cm)`) , rank(raw_data$AnnualMinTemp))

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

plot(princomp_score[,1], (log_CBL), xlab = "Principal Component 1 Scores", ylab = "log(CBL)")

hyp1_reg <- lm(log_CBL~filtered_data$MinSeasNDVI)
summary(hyp1_reg)
plot(filtered_data$MinSeasNDVI,log_CBL, xlab = "MinSeasNDVI", ylab = "log(CBL)", col = "dodgerblue")
abline(hyp1_reg, col = "blue3")

hyp2_reg <- lm(log_CBL~filtered_data$SummerMaxTemp)
summary(hyp2_reg)
plot(filtered_data$SummerMaxTemp,log_CBL, xlab = "Summer Maximum Temperature (°C)", ylab = "log(CBL)", col = "darkorange")
abline(hyp2_reg, col = "sienna")

hyp3_reg <- lm(log_CBL~filtered_data$AnnualRain)
summary(hyp3_reg)
plot(filtered_data$AnnualRain,log_CBL, xlab = "Annual Rain (mm)", ylab = "log(CBL)", col = "seagreen")
abline(hyp3_reg, col = "darkolivegreen")

hyp4_reg <- lm(log_CBL~filtered_data$`Soil nutrient availability`)
summary(hyp4_reg) 
plot(filtered_data$`Soil nutrient availability`,log_CBL, xlab = "Soil Nutrient Availability", ylab = "log(CBL)", col = 'violetred')
abline(hyp4_reg, col = 'darkmagenta')