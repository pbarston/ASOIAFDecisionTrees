# ASOIAF-Decision-Trees

Decision Tree/Random Forest Analysis for George R.R. Martin's ASOIAF Universe

Can we use decision trees to predict survival in the A Song of Ice and Fire series? 

Previously, I used survival analysis techniques in R to visualize and predict character survival in ASOIAF.

You can check out the full walk-through here at my blog:https://peterbarston.blogspot.com/2021/08/collating-game-of-thrones-survival.html and also here on Github: https://github.com/pbarston/ASOIAFSurvival

This repository has the code and blog post links for my next project: using decision tree and random forest models to predict survival analysis, with particular focus on variable importance. 

You can follow along with the code (in this repository) as well as on my blog, where I've organized all the blog posts into one neat listing: https://peterbarston.blogspot.com/2021/09/collating-game-of-thrones-decision-tree.html

There are a few additional files in this repository:

#Data contains the primary dataset as well as two additional datasets. Special thanks to the work done by Myles O'Neill to compile these here (https://www.kaggle.com/mylesoneill/game-of-thrones).

The dataset we will primarily use is "character-deaths.csv", a list of the 917 characters identified in the first five books along with their house allegiances (if noted), information on their death (year, book, chapter), the chapter of their first appearance, and some biographical info (gender, nobility) as well as a boolean appear/not appear in each of the five books.

This is a direct grab of the "character-deaths.csv" from Erin Pierce and Ben Kahle. This dataset was created as a part of their Bayesian Survival Analysis which can be found here: http://allendowney.blogspot.com/2015/03/bayesian-survival-analysis-for-game-of.html. Thanks, Erin and Ben!

#Thrones Pre-Processing is the code used to compute just the chapter span data. If you want to branch out and conduct your own analysis, then this is the place for you.

#Thrones Final Code is my full code to follow along with my blog posts (https://peterbarston.blogspot.com/2021/09/collating-game-of-thrones-decision-tree.html). These highlight specific findings and generally are great practice in GGPLOT and Tidyverse.

Let me know what you think! 
