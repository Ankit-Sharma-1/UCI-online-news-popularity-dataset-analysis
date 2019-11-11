# UCI-online-news-popularity-dataset-analysis

The dataset and associated information can be found at https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity

The project focuses on predicting the popularity of online articles published by Mashable over two years from 7th Jan 2013, as measured through the number of times an article was shared online.

In particular, I try to predict popularity based on explanatory variables (features) which would be available before the publication of the article, as mentioned in the accompanying
paper by Fernandes et al. (2015) who collected the dataset. The articles have already been heavily processed to produce a large set of numerical and categorical attributes

I try to predict popularity in two ways. The first target is as it is recorded, as the number of times the articles is shared over the period. The second target is a binary variable
which discretises the above. Following Fernandes et al. (section 3.1), an article is considered popular if it exceeds 1400 shares. You should create this binary variable. I would like you to
use multiple linear regression for the first task and binary logistic regression for the second.
