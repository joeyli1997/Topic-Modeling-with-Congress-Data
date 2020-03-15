# Topic Modeling with Congress Data

- Step 1: 

We loaded “congress.RData”, which contains all speeches of members in the United States House and Senate, the number times of terms, and ideology of each speaker. We normalized each element in the vector by subtracting the mean and dividing it by the standard deviation so that difference in scale between variables will not influence the results. Then, we put the normalized data into the k mean model with parameter k in 5, 10, 15, 20, 25. The k mean algorithm selects k centroids to assign each data point to its closest centroid, recalculates the centroids as the average of all data points in a cluster, and then reassigns and recalculates until the observation are not reassigned or the maximum iteration is reached.

- Step 2: 

We applied different measures to compare different k. Firstly, we used Akaike Information Criteria corrected (AICc). The graph indicates that k = 25 is optimal with minimum Information Criteria (IC) (which is shown in Figure 1).

Both AICc and BIC are the likelihoods penalized by the number of parameters in the model calculated as likelihood plus penalty. To interpret, the more independent variables, the higher
 
the AICc and BIC, as including more parameters could lead to overfitting. Therefore, models with lower AICc and BIC are preferred as lower values indicating lower penalty term.

As we can observe, the result for AICc and BIC is contradictory (which is shown in Figure 1 and Figure 2). In our case, we will choose AICc instead of BIC, as AICc is not largely affected by the degree of freedom. 

![pic1](/images/figure1&2.png)

Secondly, we used the elbow method, one of the most common approaches to determine the value of the k. It involves running the algorithm multiple times over a loop, with an increasing number of k and then plotting a clustering score as a function of the number of clusters. The elbow is the point where the plot turns from steep to plain, where the marginal change of the deviance turns from high to low. However, the method is sometimes ambiguous and not very reliable. One possible reason for the unintuitive elbow point could be the nature of the dataset. It is hard to determine the optimal k based on the elbow method when the dataset itself is not clustered. As we can observe for our model, there is no significant evidence of the elbow point so the largest value of k, k = 25 is optimal.


![pic2](/images/figure3.png)
 
To sum up, both methods indicate that k = 25 is optimal for our model.

- Step 3:

The highest Bayes Factor corresponds to the model with 13 topics, so we chose parameter K = 13.

We choose Latent Dirichlet Allocation (LDA) model here to fit this topic model, which is different from previous K-means model that partitions N documents into K disjoint clusters or topics in this case. LDA model assigns a document to a mixture of topics, which can give more realistic results than k-means model.

We presented top 13 phrases ordered by “topic over aggregate” lift. This lift captures phrase in-topic probability normalized by its occurrence probability, which helps us to make sure that phrase frequency does not unduly influence its topic weight and thus more useful in constructing that topic.

We also present the top 10 phrases ordered by in-topic probability, which sometimes are more intuitive for us to define the topics.

Combining these two approaches, we can have a glimpse of these 13 topics. From the result above, we can see that these 13 topics covered the topics including immigration, race, social security, war, social welfare, natural resources, climate change, laws, trade, violence, etc.

- Step 4:

Based on previous question, we use K-means model with K=25. There are some non-partisan topics, for example, topic 1, topic 13 and topic 17, as they belong to both party Republican and party Democrat. Furthermore, we couldn’t tell which party is dominant among these topics, so they are non-partisan topics.

For topic regression on repshare, we can see from the result that for party Democrat if an extra 10% speech phrases come from topic 2 which covers race issues will drop an expected 0.0166 share voting for Bush in 2004. For party Republican if an extra 10% speech phrases come from topic 1 which covers immigration issues will increase an expected 0.002 share voting for Bush in 2004.

When compared to regression onto phrase percentages, whatever the party is the topic model does better with lower mean-squared error and higher r- squared.

![pic3](/images/table1.png)

