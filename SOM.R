##################
#data
##################
setwd("~/Desktop/Diplomka/SystEvents")
count <- read.csv(file="country_factors.csv", header=TRUE, sep=",")
count$ShortIntR = as.numeric(count$ShortIntR)

#SOM
#outliers
measures0 = c('Inflat', 'QGDP', 'GenGovDeficittoGDP', 'HousePriceIndex', 'CAbalancetoGDP', 'CredGovGDP', 'CredPrivateAllGDP', 'CredPrivateBanksGDP', 'CreditPrivateGapGDP', 'ShortIntR', 'LongIntR', 'CLIFS_cat', 'Name', 'location', 'Quarter', 'year', 'CLIFS')
country = count[,measures0]
for(i in 1:12) boxplot(country[,i], main=colnames(country)[i])
#boxplot(count[,1])
#boxplot(country[,1])
#hist(country[,2])
for (i in c(9,8,1:3)){
  x<-quantile(country[,i],c(0.01,0.98))
  country = country[country[,i] >=x[1] & country[,i]<=x[2],]
}


#Define FSI categories
for (i in 1:dim(country)[1]){
  #calculate inverse quantile
  quantInv = function(distr, value) ecdf(distr)(value)
  x = country[which(country[,'location']==as.character(country[i,'location'])),'CLIFS']
  country[i,'CLIFS_cat'] = quantInv(x, country[i,'CLIFS'])
}
#transform into categories
for (i in 1:dim(country)[1]){
  if(country[i,"CLIFS_cat"]>0.75 ) country[i,"CLIFS_cat"] = 3
  if(country[i,"CLIFS_cat"]>0.50 & country[i,"CLIFS_cat"]<=0.75) country[i,"CLIFS_cat"]=2
  if(country[i,"CLIFS_cat"]>0.25 & country[i,"CLIFS_cat"]<=0.50) country[i,"CLIFS_cat"]=1
  if(country[i,"CLIFS_cat"]<=0.25) country[i,"CLIFS_cat"] = 0
}


table(country[,'CLIFS_cat'])
country[,'CLIFS_log'] = as.numeric(country[,'CLIFS_cat']>2)

#Linear Discriminant Analysis (LDA) 
#It is necessary to carry out, a priori, a statistical analysis of the variables, discarding those that do not possess discriminatory power. For this purpose we use a discriminant analysis, discarding non-significant variables by means of a uni-variate F-ratio analysis
#the Fisher analysis aims at simultaneously maximising the between-class separation, while minimising the within-class dispersion. A useful measure of the discrimination power of a variable is hence given by the diagonal quantity: Bii/Wii
#B_ii/W_ii


#train/test
measures1 = c('Inflat', 'QGDP', 'GenGovDeficittoGDP', 'HousePriceIndex', 'CAbalancetoGDP', 'CredGovGDP', 'CredPrivateAllGDP', 'CredPrivateBanksGDP', 'CreditPrivateGapGDP', 'ShortIntR', 'LongIntR')
risk_category = c('CLIFS_cat')
bin_risk_category = c('CLIFS_log')

set.seed(42)
training <- sample(nrow(country), round(dim(country)[1]/100*90,0))
Xtrain_raw <- country[training, measures1]
Xtest_raw = country[-training, measures1]
Ytrain0 = as.factor(country[training, bin_risk_category])
Ytest0 = as.factor(country[-training, bin_risk_category])
Ytrain = as.factor(country[training, risk_category])
Ytest = as.factor(country[-training, risk_category])


#log regression
# Logistics Regression
glm.fit <- glm(Ytrain0 ~ ., data = Xtrain_raw, family = binomial)
glm.probs <- predict(glm.fit,type = "response")
glm.pred1 <- ifelse(glm.probs > 0.5, 1, 0)
table(glm.pred1,Ytrain0)
mean(glm.pred1 == Ytrain0)

glm.probs <- predict(glm.fit, 
                     newdata = Xtest_raw, 
                     type = "response")
glm.pred <- ifelse(glm.probs > 0.5, 1, 0)
table(glm.pred,Ytest0)
mean(glm.pred == Ytest0)

#precision, accuracy, F
result1 <- confusionMatrix(as.factor(glm.pred1),Ytrain0)
result <- confusionMatrix(as.factor(glm.pred),Ytest0)

  
## fit ordered logit model and store results 'm'
lr_ord <- polr(Ytrain ~ ., data = Xtrain_raw, Hess=TRUE)
summary(lr_ord)
(ctable <- coef(summary(lr_ord)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
## odds ratios
exp(coef(lr_ord))


#Scale
Xtrain <- scale(Xtrain_raw)
Xtest = scale(Xtest_raw, center = attr(Xtrain, "scaled:center"), scale = attr(Xtrain, "scaled:scale"))

trainingdata <- list(measurements = Xtrain,risk_category = Ytrain)
testdata <- list(measurements = Xtest, risk_category = Ytest)


colors <- function(n, alpha = 1) {rev(heat.colors(n, alpha))}
colors0 <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
colors1 <- function(n, alpha = 1) {rev(terrain.colors(n, alpha))}
colour1 <- tricolor(som.risk$grid)
colour2 <- tricolor(som.risk$grid, phi = c(pi/6, 0, -pi/6))
colour3 <- tricolor(som.risk$grid, phi = c(pi/6, 0, -pi/6), offset = .5)


## ################################################################
## Situation 0: obtain expected values for training data (all layers,
## also if not used in training) on the basis of the position in the map
# som.risk <- supersom(trainingdata, grid = mygrid, rlen=1000)
# som.prediction <- predict(som.risk)

## ###############################################################
## Situation 3: predictions for layers not present in the original
## data. Training data need to be provided for those layers.
#options(device = "RStudioGD")
mygrid = somgrid(6, 9, "hexagonal")
som.risk <- supersom(Xtrain, 
                     grid = mygrid, 
                     rlen=10,
                     alpha=c(0.05,0.01),
                     keep.data = TRUE)


# plot(som.risk, type="counts", shape = "straight", palette.name = colors1, main = "How many samples are mapped to each node?")
# #Color the crises element: 1(low)-4(high)
# var_unscaled = aggregate(as.numeric(as.character(Ytrain)), by=list(som.risk$unit.classif), FUN=mean, simplify=TRUE)
# names(var_unscaled) = c("Node", "Value")
# plot(som.risk, type = "property", property=var_unscaled$Value, main='Financial Stress Composite Indicator',shape='straight', palette.name=colors)


#som.risk1 <- supersom(Xtrain, grid = mygrid, rlen=100)
som.prediction <- predict(som.risk, newdata = testdata,
                          trainingdata = trainingdata)
table(country[-training, risk_category], som.prediction$predictions[["risk_category"]])
#precision, accuracy, F
result <- confusionMatrix(as.factor(country[-training, risk_category]), as.factor(som.prediction$predictions[["risk_category"]]))


#type = c( "changes","counts","codes","dist.neighbours", "mapping", "property", "quality")

#Iteration
#As the SOM training iterations progress, the distance from each node’s weights to the samples represented by that node is reduced. Ideally, this distance should reach a minimum plateau. This plot option shows the progress over time. If the curve is continually decreasing, more iterations are required. 
plot(som.risk, type="changes")
par(mar=c(5.1,4.1,4.1,2.1))

#Count
#The Kohonen packages allows us to visualise the count of how many samples are mapped to each node on the map. This metric can be used as a measure of map quality – ideally the sample distribution is relatively uniform. Large values in some map areas suggests that a larger map would be benificial. Empty nodes indicate that your map size is too big for the number of samples. Aim for at least 5-10 samples per node when choosing map size. 
par(mfrow = c(1,1))
plot(som.risk, type="counts", shape = "straight", palette.name = colors1, main = "How many samples are mapped to each node?")
plot(som.risk, type="mapping", shape = "straight", main = "How many samples are mapped to each node?")


#Neighbour Distance
#the “U-Matrix” - visualisation of the distance between each node and its neighbours.Areas of low neighbour distance indicate groups of nodes that are similar. Areas with large distances indicate the nodes are much more dissimilar – and indicate natural boundaries between node clusters. The U-Matrix can be used to identify clusters within the SOM map. 
plot(som.risk, type="dist.neighbours", palette.name = colors, shape='straight', main='The Unified Distance Matrix')

#Because of low dimension - we can use Codes / Weight vectors
#The node weight vectors, or “codes”, are made up of normalised values of the original variables used to generate the SOM. Each node’s weight vector is representative / similar of the samples mapped to that node. By visualising the weight vectors across the map, we can see patterns in the distribution of samples and variables. The default visualisation of the weight vectors is a “fan diagram”, where individual fan representations of the magnitude of each variable in the weight vector is shown for each node. Other represenations are available, see the kohonen plot documentation for details. 
plot(som.risk, type="codes", shape='straight', main='Individual Factors')

nam = c('Inflation','GDP', 'House Price Index', 'Quarterly Goverment Debt', 'Current Account Balance', 'Credit from Banks','Credit from All', 'Interest Rate - short', 'Interets Rate - long')





#Heatmap
#Each node coloured using average value of all linked datapoints
#A SOM heatmap allows the visualisation of the distribution of a single variable across the map. Typically, a SOM investigative process involves the creation of multiple heatmaps, and then the comparison of these heatmaps to identify interesting areas on the map. It is important to remember that the individual sample positions do not move from one visualisation to another, the map is simply coloured by different variables.

#variables 
#- shows distribution of variables across map
#useful when number of parameters<5
par(mfrow = c(1,1))


nam = c('Inflation','GDP Change', 
        'Goverment Debt to GDP',
        'House Price Index',
        'Current Account Balance to GDP ', 
        'Credit to Goverment to GDP',
        'Credit from All Sectors to GDP',
        'Credit from Banks to GDP',
        'Credit to GDP Gap',
        'Interest Rate - 3M', 
        'Interets Rate - 10Y')

for (i in 1:11) plot(som.risk, type = "property", property = som.risk$codes[[1]][,i], main=colnames(som.risk$data[[1]])[i], palette.name=colors)
#THOUGH this default visualisation plots the normalised version of the variable of interest. A more intuitive and useful visualisation is of the variable prior to scaling – use the aggregate function to regenerate the variable from the original training set and the SOM node/sample mappings. The result is scaled to the real values of the training variable.
#Aside: Heatmaps with empty nodes in your SOM grid
#In some cases, your SOM training may result in empty nodes in the SOM map. In this case, you won’t have a way to calculate mean values for these nodes in the “aggregate” line above when working out the unscaled version of the map. With a few additional lines, we can discover what nodes are missing from the som_model$unit.classif and replace these with NA values – this step will prevent empty nodes from distorting your heatmaps.

#A: Examine Heatmaps
par(mfrow=c(1,1))
par(mfrow = c(2, 2),     # 2x2 layout
    oma = c(2, 2, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(1, 1, 0, 0), # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
    xpd = NA) 


for (i in 1:11){
  var <- i #define the variable to plot
  # Plotting unscaled variables when there are empty nodes in the SOM
  var_unscaled = aggregate(as.numeric(Xtrain_raw[[var]]), by=list(som.risk$unit.classif), FUN=mean, simplify=TRUE)
  names(var_unscaled) = c("Node", "Value")
  # Add in NA values for non-assigned nodes - first find missing nodes:
  missingNodes = which(!(seq(1,dim(som.risk$codes[[1]])[1]) %in% var_unscaled$Node))
  # Add them to the unscaled variable data frame
  var_unscaled = rbind(var_unscaled, data.frame(Node=missingNodes, Value=NA))
  # order the resulting data frame
  var_unscaled = var_unscaled[order(var_unscaled$Node),]
  # Now create the heat map only using the "Value" which is in the correct order.
  plot(som.risk, type = "property", property=var_unscaled$Value, main=nam[var], palette.name=colors,shape = "straight")
}


#Color the crises element: 1(low)-4(high)
var_unscaled = aggregate(as.numeric(as.character(Ytrain)), by=list(som.risk$unit.classif), FUN=mean, simplify=TRUE)
names(var_unscaled) = c("Node", "Value")
plot(som.risk, type = "property", property=var_unscaled$Value, main='Financial Stress Indicator',shape='straight', palette.name=colors)


####Map country to SOM
par(mfrow = c(2, 2),     # 2x2 layout
    oma = c(2, 2, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(1, 1, 0, 0), # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
    xpd = NA) 

#Color the crises element: 1(low)-4(high)
var_unscaled = aggregate(as.numeric(as.character(Ytrain)), by=list(som.risk$unit.classif), FUN=mean, simplify=TRUE)
names(var_unscaled) = c("Node", "Value")
plot(som.risk, type = "property", property=var_unscaled$Value, main='Euro Area Over Time',shape='straight', palette.name=colors)

#Color the crises element: 1(low)-4(high)
var_unscaled = aggregate(as.numeric(as.character(Ytrain)), by=list(som.risk$unit.classif), FUN=mean, simplify=TRUE)
names(var_unscaled) = c("Node", "Value")
plot(som.risk, type = "property", property=var_unscaled$Value, main='Countries 2003',shape='straight', palette.name=colors)

#Color the crises element: 1(low)-4(high)
var_unscaled = aggregate(as.numeric(as.character(Ytrain)), by=list(som.risk$unit.classif), FUN=mean, simplify=TRUE)
names(var_unscaled) = c("Node", "Value")
plot(som.risk, type = "property", property=var_unscaled$Value, main='Countries 2008',shape='straight', palette.name=colors)

#Color the crises element: 1(low)-4(high)
var_unscaled = aggregate(as.numeric(as.character(Ytrain)), by=list(som.risk$unit.classif), FUN=mean, simplify=TRUE)
names(var_unscaled) = c("Node", "Value")
plot(som.risk, type = "property", property=var_unscaled$Value, main='Countries 2012',shape='straight', palette.name=colors)

mygrid = somgrid(6, 9, "hexagonal")
som.risk <- supersom(Xtrain, 
                     grid = mygrid, 
                     rlen=10,
                     alpha=c(0.05,0.01),
                     keep.data = TRUE)

par(mfrow=c(1,1))
#Map 
#EA19
head(country)
country_sc = country
country_sc[,measures1] = scale(country_sc[,measures1], center = attr(Xtrain, "scaled:center"), scale = attr(Xtrain, "scaled:scale"))
filter = country_sc[which(country_sc['location']=='EA19' & country_sc['Quarter']=='Q1'),]

xx = map(som.risk, as.matrix(filter[,measures1]))
yy = som
yy$data[[1]] = as.matrix(filter[,measures1])
yy$unit.classif = xx$unit.classif
yy$distances = xx$distances

plot(yy, type="mapping",
     labels = filter[,'year'],
     main = "mapping plot",shape='straight')


#Map 
#2007Q3
head(country)
filter = country_sc[which(country_sc['year']==2006 & country_sc['Quarter']=='Q3'),]
xx = predict(som.risk, as.matrix(filter[,measures1]))
yy = som.risk
yy$data[[1]] = as.matrix(filter[,measures1])
yy$unit.classif = xx$unit.classif
yy$distances = xx$distances


plot(som.risk, type="mapping",
     labels = filter[,'location'],
     shape='straight',main='2006')

#Map 
#2007Q3
head(country)
filter = country_sc[which(country_sc['year']==2007 & country_sc['Quarter']=='Q3'),]
xx = map(som.risk, as.matrix(filter[,measures1]))
yy = som.risk
yy$data[[1]] = as.matrix(filter[,measures1])
yy$unit.classif = xx$unit.classif
yy$distances = xx$distances

plot(yy, type="mapping",
     labels = filter[,'location'],
     shape='straight',main='2007')

#Map 
#2007Q3
head(country)
filter = country_sc[which(country_sc['year']==2008 & country_sc['Quarter']=='Q3'),]
xx = map(som.risk, as.matrix(filter[,measures1]))
yy = som.risk
yy$data[[1]] = as.matrix(filter[,measures1])
yy$unit.classif = xx$unit.classif
yy$distances = xx$distances

plot(yy, type="mapping",
     labels = filter[,'location'],
     shape='straight', main='2008')

#Map 
#2007Q3
head(country)
filter = country_sc[which(country_sc['year']==2009 & country_sc['Quarter']=='Q3'),]
xx = map(som.risk, as.matrix(filter[,measures1]))
yy = som.risk
yy$data[[1]] = as.matrix(filter[,measures1])
yy$unit.classif = xx$unit.classif
yy$distances = xx$distances

plot(yy, type="mapping",
     labels = filter[,'location'],
     shape='straight',main='2009')


#Map 
#2010Q3
head(country)
filter = country_sc[which(country_sc['year']==2008 & country_sc['Quarter']=='Q4'),]
xx = map(som.risk, as.matrix(filter[,measures1]))
yy = som.risk
yy$data[[1]] = as.matrix(filter[,measures1])
yy$unit.classif = xx$unit.classif
yy$distances = xx$distances

plot(yy, type="mapping",
     labels = filter[,'location'],
     shape='straight',main='2011')


#Map 
#2007Q3
head(country)
filter = country_sc[which(country_sc['year']==2012 & country_sc['Quarter']=='Q3'),]
xx = map(som.risk, as.matrix(filter[,measures1]))
yy = som.risk
yy$data[[1]] = as.matrix(filter[,measures1])
yy$unit.classif = xx$unit.classif
yy$distances = xx$distances

plot(yy, type="mapping",
     labels = filter[,'location'],
     shape='straight',main='2012')

#Map 
#2007Q3
head(country)
filter = country_sc[which(country_sc['year']==2013 & country_sc['Quarter']=='Q3'),]
xx = map(som.risk, as.matrix(filter[,measures1]))
yy = som.risk
yy$data[[1]] = as.matrix(filter[,measures1])
yy$unit.classif = xx$unit.classif
yy$distances = xx$distances

plot(yy, type="mapping",
     labels = filter[,'location'],
     shape='straight', main='2013')

#Map 
#2007Q3
head(country)
filter = country_sc[which(country_sc['year']==2014 & country_sc['Quarter']=='Q3'),]
xx = map(som.risk, as.matrix(filter[,measures1]))
yy = som.risk
yy$data[[1]] = as.matrix(filter[,measures1])
yy$unit.classif = xx$unit.classif
yy$distances = xx$distances

plot(yy, type="mapping",
     labels = filter[,'location'],
     shape='straight',main='2014')


#Map 
#2010Q3
head(country)
filter = country_sc[which(country_sc['year']==2015 & country_sc['Quarter']=='Q1'),]
xx = map(som.risk, as.matrix(filter[,measures1]))
yy = som.risk
yy$data[[1]] = as.matrix(filter[,measures1])
yy$unit.classif = xx$unit.classif
yy$distances = xx$distances

plot(yy, type="mapping",
     labels = filter[,'location'],
     shape='straight',main='2015')


#A: Examine Clusters
#Clustering can be performed on the SOM nodes to isolate groups of samples with similar metrics. Manual identification of clusters is completed by exploring the heatmaps for a number of variables and drawing up a “story” about the different areas on the map. 
#An estimate of the number of clusters that would be suitable can be ascertained using a kmeans algorithm and examing for an “elbow-point” in the plot of “within cluster sum of squares”.  The Kohonen package documentation shows how a map can be clustered using hierachical clustering. The results of the clustering can be visualised using the SOM plot function again.
#Hierarchical Clustering with Connectivity Constrains
#In order to implement connectivity constrains we have to factor in the distance of each cluster to another cluster on the SOM map - kohonen::unit.distances takes a som grid and returns a distance matrix reflecting the positional distance among clusters on the som grid

#Determining the optimal number of clusters
#An estimate of the number of clusters that would be suitable can be ascertained using a kmeans algorithm and examing for an “elbow-point” in the plot of “within cluster sum of squares”. 
mydata <- som.risk$codes
wss <- (nrow(mydata[[1]])-1)*sum(apply(mydata[[1]],2,var))
for (i in 1:11) {
  wss[i] <- sum(kmeans(mydata[[1]], centers=i)$withinss)
}
plot(wss)
## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(som.risk$codes[[1]])), 6)
# plot these results:
# Colour palette definition
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')
plot(som.risk, type="mapping", bgcol = pretty_palette[som_cluster],main = "Clusters") 
add.cluster.boundaries(som.risk, som_cluster)

summary(som.risk)

par(mfrow = c(1,1))
similarities <- plot(som.risk, type="quality", palette.name = terrain.colors)


## add background colors to units according to their predicted class labels
xyfpredictions <- classmat2classvec(getCodes(kohmap, 2))
bgcols <- c("gray", "pink", "lightgreen")
plot(kohmap, type="mapping", col = as.integer(vintages),
     pchs = as.integer(vintages), bgcol = bgcols[as.integer(xyfpredictions)],
     main = "another mapping plot", shape = "straight", border = NA)

## Show 'component planes'
set.seed(7)
sommap <- som(scale(wines), grid = somgrid(6, 4, "hexagonal"))
plot(sommap, type = "property", property = getCodes(sommap, 1)[,1],
     main = colnames(getCodes(sommap, 1))[1])

## Show the U matrix
Umat <- plot(som.risk, type="dist.neighbours", main = "SOM neighbour distances")
## use hierarchical clustering to cluster the codebook vectors
som.hc <- cutree(hclust(object.distances(som.risk, "codes")), 5)
add.cluster.boundaries(som.risk, som.hc)



#WEIGHT VECTORS
#each cell displays its representative weight vector
count.som1 <- som(scale(country[measures1]), grid = somgrid(10, 10, "rectangular"),rlen = 4000)
plot(count.som1)

#HEATMAP
#identify cells on the map by assigning each input to the cell with representative vector closest to that item’s stat line. The “count” type SOM does exactly this, and creates a heatmap based on the number of items assigned to each cell. 

# reverse color ramp so that red represents the most frequent
par(mfrow = c(1, 1))

colors <- function(n, alpha = 1) {
  rev(heat.colors(n, alpha))
}

plot(count.som1, type = "counts", palette.name = colors, heatkey = TRUE)

#HEATMAP 2.0
#Alternatively plot the players as points on the grid using the “mapping” type SOM
plot(count.som1, type = "mapping", pchs = 20, main = "Mapping Type SOM")
plot(count.som1, main = "Default SOM Plot")


#DISTANCE
#The cells are colored depending on the overall distance to their nearest neighbors, which allows us to visualize how far apart different features are in the higher dimensional space.
plot(count.som1, type = "dist.neighbours", palette.name = terrain.colors)

#Supervised SOMs
#classification of items by their FSI
#Randomly divide data into training and testing sets
training <- sample(nrow(country), round(dim(country)[1]/100*80,0))
train <- scale(country[training, measures1])
test <- scale(country[-training, measures1], center = attr(train, "scaled:center"), scale = attr(train, "scaled:scale"))


#Rescale testing data according to how we scaled training data
count.som3 <- xyf(data=train, classvec2classmat(country$CLIFS_cat[training]), grid = somgrid(10, 10, "hexagonal"), rlen = 1000)
summary(count.som3)
#xweight - allows to weight the set of training variables versus the prediction variable in the training algorithm

#accuracy of the prediction:
pos.prediction <- predict(count.som3, newdata = test)
table(country[-training, "Pos"], pos.prediction$prediction)



##########Banks
##################
#data
##################
mydata <- read.csv(file="~/Desktop/Diplomka/variables/mydata.csv", header=TRUE, sep=",", na.strings=c("","NA"))
sapply(mydata,function(x) sum(is.na(x)))

mydata = na.omit(mydata)

measures1 = c("UBPRE600", "UBPRE595", 
              "UBPRK447", "UBPRE591", "UBPRE597", "UBPRE021", "UBPR7316", "UBPRE027", 
              "UBPR7408", "UBPRE541", "UBPR7414", "UBPRE023", "UBPRE542", "UBPRE006", 
              "UBPRE024", "UBPR7402", "UBPRE025", "UBPRE007", "UBPRE013", "UBPRE003", "UBPRE004", 
              "UBPRE005", "UBPRE018", "UBPRE017", "UBPRE016", "UBPRE015", "UBPRE029", 
              "UBPRE028", "UBPRD486", "UBPRE633", "UBPRD487", "UBPRD488", "UBPRE630", 
              "UBPRE088", "UBPR7400")

risk_category = c("X2008.2009")

mydata0 = mydata

for (i in 1: length(measures1)){
  mydata[,measures1[i]] = as.numeric(as.character(mydata[,measures1[i]]))
}

#str(mydata[,X_raw])
#boxplot(mydata[,measures1[1]], main=measures1[1])
# for (i in 1:length(measures1)){
#   boxplot(mydata[,measures1[i]],main=measures1[i])
# }

#outliers
#x<-quantile(mydata[,X_raw[2]],c(0.01,0.99), na.rm=TRUE)
for (i in measures1){
  x<-quantile(mydata[,i],c(0.01,0.98))
  mydata = mydata[mydata[,i] >=x[1] & mydata[,i]<=x[2],]
}



#train/test
set.seed(42)
training <- sample(nrow(mydata), round(dim(mydata)[1]/100*90,0))
Xtrain_raw <- mydata[training, measures1]
Xtest_raw = mydata[-training, measures1]
Ytrain = as.factor(mydata[training, risk_category])
Ytest = as.factor(mydata[-training, risk_category])


#Scale
Xtrain <- scale(Xtrain_raw)
Xtest = scale(Xtest_raw, center = attr(Xtrain, "scaled:center"), scale = attr(Xtrain, "scaled:scale"))


trainingdata <- list(measurements = Xtrain,risk_category = Ytrain)
testdata <- list(measurements = Xtest, risk_category = Ytest)





## ################################################################
## Situation 0: obtain expected values for training data (all layers,
## also if not used in training) on the basis of the position in the map
#mygrid = somgrid(30, 30, "hexagonal")
# som.risk <- supersom(trainingdata, grid = mygrid, rlen=4000)
# som.prediction <- predict(som.risk)

## ###############################################################
## Situation 3: predictions for layers not present in the original
## data. Training data need to be provided for those layers.
mygrid = somgrid(35, 30, "hexagonal")
som.risk <- supersom(Xtrain, 
                     grid = mygrid, 
                     rlen=100,
                     alpha=c(0.05,0.01),
                     keep.data = TRUE)




colors <- function(n, alpha = 1) {rev(heat.colors(n, alpha))}
colors0 <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
colors1 <- function(n, alpha = 1) {rev(terrain.colors(n, alpha))}
colour1 <- tricolor(som.risk$mygrid)

#Color the crises element: 1(low)-4(high)
var_unscaled = aggregate(as.numeric(as.character(Ytrain)), by=list(som.risk$unit.classif), FUN=mean, simplify=TRUE)
names(var_unscaled) = c("Node", "Value")
plot(som.risk, type = "property", property=var_unscaled$Value, main='category - crises',shape='straight', palette.name=colors)

#plot(som.risk, type="counts", shape = "straight", palette.name = colors, main = "Default SOM Plot")
par(mfrow=c(1,1))




#som.risk1 <- supersom(Xtrain, grid = mygrid, rlen=100)
som.prediction <- predict(som.risk, newdata = testdata,
                          trainingdata = trainingdata)
table(mydata[-training, risk_category], som.prediction$predictions[["risk_category"]])


#type = c( "changes","counts","codes","dist.neighbours", "mapping", "property", "quality")


#Iteration
#As the SOM training iterations progress, the distance from each node’s weights to the samples represented by that node is reduced. Ideally, this distance should reach a minimum plateau. This plot option shows the progress over time. If the curve is continually decreasing, more iterations are required. 
plot(som.risk, type="changes")


#Count
#The Kohonen packages allows us to visualise the count of how many samples are mapped to each node on the map. This metric can be used as a measure of map quality – ideally the sample distribution is relatively uniform. Large values in some map areas suggests that a larger map would be benificial. Empty nodes indicate that your map size is too big for the number of samples. Aim for at least 5-10 samples per node when choosing map size. 
par(mfrow = c(1,1))
plot(som.risk, type="counts", shape = "straight", palette.name = colors1, main = "Default SOM Plot")



#Neighbour Distance
#the “U-Matrix” - visualisation of the distance between each node and its neighbours.Areas of low neighbour distance indicate groups of nodes that are similar. Areas with large distances indicate the nodes are much more dissimilar – and indicate natural boundaries between node clusters. The U-Matrix can be used to identify clusters within the SOM map. 
plot(som.risk, type="dist.neighbours", palette.name = colors, shape='straight', main="The Unified Distance Matrix")


#because low dimension - we can use Codes / Weight vectors
#The node weight vectors, or “codes”, are made up of normalised values of the original variables used to generate the SOM. Each node’s weight vector is representative / similar of the samples mapped to that node. By visualising the weight vectors across the map, we can see patterns in the distribution of samples and variables. The default visualisation of the weight vectors is a “fan diagram”, where individual fan representations of the magnitude of each variable in the weight vector is shown for each node. Other represenations are available, see the kohonen plot documentation for details. 
plot(som.risk, type="codes", shape='straight')


#Heatmap
#each node coloured using average value of all linked datapoints
#A SOM heatmap allows the visualisation of the distribution of a single variable across the map. Typically, a SOM investigative process involves the creation of multiple heatmaps, and then the comparison of these heatmaps to identify interesting areas on the map. It is important to remember that the individual sample positions do not move from one visualisation to another, the map is simply coloured by different variables.

#variables 
#- shows distribution of variables across map
#useful when number of parameters<5
par(mfrow = c(2, 2),     # 2x2 layout
    oma = c(2, 2, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(1, 1, 0, 0), # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
    xpd = NA) 
for (i in 1:53) plot(som.risk, type = "property", property = som.risk$codes[[1]][,i], main=colnames(som.risk$data[[1]])[i], palette.name=colors)
#THOUGH this default visualisation plots the normalised version of the variable of interest. A more intuitive and useful visualisation is of the variable prior to scaling – use the aggregate function to regenerate the variable from the original training set and the SOM node/sample mappings. The result is scaled to the real values of the training variable.
#Aside: Heatmaps with empty nodes in your SOM grid
#In some cases, your SOM training may result in empty nodes in the SOM map. In this case, you won’t have a way to calculate mean values for these nodes in the “aggregate” line above when working out the unscaled version of the map. With a few additional lines, we can discover what nodes are missing from the som_model$unit.classif and replace these with NA values – this step will prevent empty nodes from distorting your heatmaps.

#A: Examine Heatmaps
#Color the crises element: 1(low)-4(high)
var_unscaled = aggregate(as.numeric(as.character(Ytrain)), by=list(som.risk$unit.classif), FUN=mean, simplify=TRUE)
names(var_unscaled) = c("Node", "Value")
plot(som.risk, type = "property", property=var_unscaled$Value, main='category - crises',shape='straight', palette.name=colors)




#########################################################
#banks_rssd = {"Bank of New York Mellon": 3587146, "Bank of America":1073757, "BB_T Corporation":1074156, "Capital One":2277860, "Citigroup":1951350, "Fifth Third Bancorp":1070345, "Goldman Sachs":2380443, "JP Morgan Chase":1039502, "KeyCorp":1034806, "Morgan Stanley":2162966, "PNC, State Street":1069778, "Sun Trust":1131787, "Regions Financial_2004":1078332,"Regions Financial_2016":3242838, "U.S. Bancorp":1119794, "Wells Fargo":1120754}
#banks_reuters = {"Bank of New York Mellon": ['BNY Mellon', 'Bank of New York Mellon', 'BK.N'], "Bank of America":['BAC.N', 'Bank of America', 'Bank of America\'s'], "BB_T Corporation":['BB&T Corp', 'BB&T'], "Capital One":['COF.N', 'Capital One'], "Citigroup":['Citigroup', 'CIT2.L'], "Fifth Third Bancorp":['Fifth Third', 'FITB.OQ'], "Goldman Sachs":['Goldman', 'GS.N'], "JP Morgan Chase":['JP Morgan'], "KeyCorp":['KEY.N', 'KeyCorp'], "Morgan Stanley":['MS.N', 'Morgan Stanley'], "PNC, State Street":['PNC'], "Sun Trust":['Suntrust', 'SunTrust', 'STI.N'],"Regions Financial":['Regions', 'Regions Financial', 'RF.N'], "U.S. Bancorp":['Bancorp', 'USB.N'], "Wells Fargo":['Wells Fargo', 'WFC.N']}

#c("Bank of America",'Bank of New York Mellon', "BB_T Corporation", 'Capital One',"Fifth Third Bancorp", "Goldman Sachs", "Morgan Stanley","PNC, State Street")
banks = c('1443266','541101', '852320', '112837', '723112','2182786','1456501', '817824') 

for (i in banks){
  print(sum(country_sc[,'ID.RSSD']==i))
}


#2010Q3
year=2016
quarter="Q1"

country_sc = mydata0
country_sc[,'ID.RSSD'] = as.character(country_sc[,'ID.RSSD'])
head(country_sc)
filter1 = country_sc[which(country_sc[,'ID.RSSD'] %in% banks),]
#add names of banks
filter1[,'company'] = 0
filter1[which(filter1$ID.RSSD=='1443266'),'company'] = "Bank of America"
filter1[which(filter1$ID.RSSD=='541101'),'company'] = 'Bank of New York Mellon'
filter1[which(filter1$ID.RSSD=='852320'),'company'] = "BB_T Corporation"
filter1[which(filter1$ID.RSSD=='112837'),'company'] = 'Capital One'
filter1[which(filter1$ID.RSSD=='723112'),'company'] = "Fifth Third Bancorp"
filter1[which(filter1$ID.RSSD=='2182786'),'company'] = "Goldman Sachs"
filter1[which(filter1$ID.RSSD=='1456501'),'company'] = "Morgan Stanley"
filter1[which(filter1$ID.RSSD=='817824'),'company'] = "PNC, State Street"


filter = filter1[which(filter1[,'Quarter']==quarter & filter1[,'year']==year),]

xx = predict(som.risk, as.matrix(filter[,measures1]))
som.risk$data = xx$predictions
som.risk$unit.classif = xx$unit.classif
plot(som.risk, type="mapping",
     labels = filter[,'company'],
     shape='straight',main='2006')



bank=banks[1]
filter = filter1[which(filter1[,'ID.RSSD']==bank & filter1[,'Quarter']==quarter),]
xx = predict(som.risk, as.matrix(filter[,measures1]))
som.risk$data = xx$predictions
som.risk$unit.classif = xx$unit.classif
plot(som.risk, type="mapping",
     labels = filter[,'year'],
     shape='straight',main='2006')






filter = country_sc[which(country_sc['year']==2006 & country_sc['Quarter']=='Q3'),]
xx = predict(som.risk, as.matrix(filter[,measures1]))
xx = predict(som.risk, as.matrix(filter[,measures1]))
som.risk$data = xx$predictions
som.risk$unit.classif = xx$unit.classif

plot(som.risk, type="mapping",
     labels = filter[,'location'],
     shape='straight',main='2006')



#Hierarchical clustering
mydata <- som.risk$codes[[1]]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
for (i in 1:35) {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}
plot(wss)
## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(som.risk$codes[[1]])), 4)
# plot these results:
plot(som.risk,type="code", bgcol = pretty_palette[som_cluster], main = "Clusters", shape='straight') 
add.cluster.boundaries(som.risk, som_cluster)

