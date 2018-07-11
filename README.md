# SUMMARY
-----------------------------------
Leverage Self-organizing map (SOM) for visualizing systemic risks and predicting systemic events

1. Collect the quarterly data for countries from combination of sources (Oecd, BIS,..)
2. For the countries, moreover collecte the classification information - composite financial stress indicator (range from 0-1)
3. Calculate 0.25, 0.5, 0.75 for the quantile for composite financial stress indicator for each country, based on this, split data into 4 categories - assign the 4th category as the crises one
4. Train logistic regression on the country level to get accuracy and false negatives results - to serve as a benchmark for SOM
5. Hyperparametrization to obtain the “correct” SOM inputs (size of the grid, distance metrics, learning rate) - trained with classified data
6. Use the grid size from previous step and trained SOM model on unclassified data (without composite financial stress indicator)
7. Visualize metrics for SOM quality assessment - u-matrix, number of samples linked to each node,..
8. Visualize SOM and mapped countries over time and how they have been moving around the map (Use the composite financial stress indicator - to be able to compare this to our SOM results)
9. Collect quarterly financial data for financial institutions across USA (about 500k entries) - these datapoints collected based on CAMEL scoring system
10. Possibly add textual information - get all Reuters articles for 16 most significant US bank holding companies, apply sensitivity analysis on those articles (dictionary in nltk package in Python that maps each word to positive, negative, neutral number, it maps the whole article and calculates overall ‘sense’ of the article), Reuters only stores article data starting 2007, which would restrict the dataset (current state - data 2003-2016) and including this within a simple ANN (Classification used for this - the approximation that every financial bank institution in US in 2008,2009 was in crises state) didn't show any improvement, therefore droping this textual information.
11. Create visual SOM maps - when presenting new bankSOM shows it’s neighbours as well as, again, we can see how institutions have been moving around the map
12. Apply elbow rule and hierarchical clustering to demonstrate 2nd level clustering




# TRAINING DATASET FILE DESCRIPTION
-----------------------------------
SOM_variables.ipynb - Jupyter notebook describing variables creation
TextDataLink.csv - List of Reuters Article archive





# SOM SCRIPTS
-----------------------------------
- R using The Kohonen package
- Python TF for hyper parameter. optimization

The algorithm to produce a SOM from a sample data set can be summarised as follows:
- Select the size and type of the map. The shape can be hexagonal or square, depending on the shape of the nodes your require. Typically, hexagonal grids are preferred since each node then has 6 immediate neighbours.
- Initialise all node weight vectors randomly.
- Choose a random data point from training data and present it to the SOM.
- Find the “Best Matching Unit” (BMU) in the map – the most similar node. Similarity is calculated using the Euclidean distance formula.
- Determine the nodes within the “neighbourhood” of the BMU.
– The size of the neighbourhood decreases with each iteration.
- Adjust weights of nodes in the BMU neighbourhood towards the chosen datapoint.
– The learning rate decreases with each iteration.
– The magnitude of the adjustment is proportional to the proximity of the node to the BMU.
- Repeat Steps 2-5 for N iterations / convergence.
