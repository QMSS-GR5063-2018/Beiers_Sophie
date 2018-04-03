# Kickstarter Campaigns
Kickstarter is an American public-benefit corporation based in Brooklyn, New York, that maintains a global crowd funding platform focused on creativity. The company’s stated mission is to “help bring creative projects to life”. Kickstarter has reportedly received more than $1.9 billion in pledges from 9.4 million backers to fund 257,000 creative projects, such as films, music, stage shows, comics, journalism, video games, technology and food-related projects. For this assignment, I analyze the descriptions of kickstarter projects to identify commonalities of successful (and unsuccessful projects) using text mining techniques. Visualizations were created primarily using ggplot and Highcharter in R. For code and all visualizations, please see the [Rmd](.hw3/docs/Beiers_Kickstarter.Rmd) file and the "docs" folder. For further explanation of each visualization, please see the [HTML](.hw3/docs/Beiers_Kickstarter.html) document. The present README.md file is only to serve as an outline of the repository and a summary of key visualizations.

This repository is organized as follows. Please see each subfolder for a README.md file.

```
proj/
├── docs/
├── data/
├── images/
└── output/
```

## Data
The dataset for this assignment is taken from [webroboto.io ‘s repository](https://webrobots.io/kickstarter-datasets/). They developed a scraper robot that crawls all Kickstarter projects monthly since 2009. We just took data from the most recent crawl on 2018-02-15.

The data was cleaned and is contained in the file `kickstarter_data.csv` -- it contains about 150,000 projects and about 20 variables.


## Highlights

*All interactive Highcarter maps are included in the HTML document.*

### Identifying Successful Projects
#### Success by Category

To define “success,” I chose to create a ratio of pledged amount of money over the monetary goal of the campaign. I removed any campaign with a goal of under $30 to ensure results were a little less skewed and eliminated any campaigns deemed unsuccessful since many of these campaigns dropped out or discontinued. I then broke out the results into categories of campaigns; we can see below that all campaign categories received an average of over 100% of their goal money, which is surprising! The technology category seems to have the most success while art campaigns were still highly successful (over 100% of goal money on average) but less exorbitantly successful than other campaign categories.

![](../hw3/images/unnamed-chunk-2-1.png)

#### Success in San Francisco
I was curious to visualize success in San Francisco because it’s a tech hub and a hot spot for Kickstarter campaigns. Most campaigns from San Francisco received over 100% of their goal from their backers, so “success” took on a new definition: popularity. Larger bubbles had more backers and bubbles placed higher up on the graph received the most pledged money. The scales of the x axis versus the y axis tell us that no projects asked for over a million dollars, yet one received up to 6.5 million and multiple received multi-millions of dollars! Scroll over each bubble for more information about the project.

![](../hw3/images/unnamed-chunk-3-1)

### Writing your success story
Each project contains a blurb – a short description of the project. While not the full description of the project, the short headline is arguably important for inducing interest in the project (and ultimately popularity and success). Let’s analyze the text.

#### Cleaning the Text and Word Cloud
I selected the top 1000 most successful projects and the bottom 1000 least successful projects, then removed stop words, punctuation and numbers. I chose to not stem the words as to leave more of the meaning in the word cloud. I then created a document-term-matrix and term-document-matrix and visualized the most frequent words among the 1000 most successful projects in a word cloud.

Most common words throughout all 2000 projects include “game,” “art,” “designed” and “homemade.” Many of the kickstarter projects must be game-related, and it’s not surprising that many projects are “homemade” and “artistically” “designed” due to many people’s hipster, DIY aesthetic these days.

![](../hw3/images/unnamed-chunk-4-1.png)

#### Success in Words
I then created a pyramid plot to show how the words between the most successful and unsuccessful projects differ in frequency. I selected the top 20 words used by the 1000 most successful and least successful campaigns.

As expected “art” and other art-related words like “painting” and “music” came up as some of the most common words unsuccessful campaigns use. We knew from our visualization earlier that art campaigns don’t tend to do as well as other categories of campaigns. Again unsurprisingly, we knew from our wordcloud that “game” is shown as a word commonly used by successful campaigns. Games seem to be attractive to backers! “World” tends to be an equally used word from both successful and unsuccessful campaigns.

![](../hw3/images/unnamed-chunk-5-1.png)


#### Simplicity as a virtue

These blurbs are short in length (max. 150 characters) but let’s see whether brevity and simplicity still matters. I calculated a readability measure (Flesh Kincaid) for the texts defined by the equation below (from Wikipedia):

![](../h3/images/fk.png)

Flesch Kincaid produces a number that defines how easy or difficult a chunk of text is based on what grade level in school should be able to read it. Anything that receives over a 90 is considered easy to read (a 5th grader could read it) and anything below a 30 is considered difficult and for college students to be able to read and understand. I then visualized the relationship between the Flesch Kincaid measure of the short blurb of the campaign and the number of backers each campaign had.

I chose a different measure of “success” for this visualization: number of backers. From the visualization, we can see that most heavily-backed projects were in between -13 to 80 in terms of Flesch Kincaid scores. That’s a very large range! Most projects in general were left-skewed, but a few blurbs were so difficult to read they received negative scores. Those projects didn’t have the most backers but still received over a thousand. The most popular project (35550 backers) had a readability score of around 80, or, between a 6th and 7th grade level of reading.

Hover over the circles for more information about each project.The larger the bubble, the more backers the project received.

![](../hw3/images/unnamed-chunk-6-1.png)


### Sentiment
Now, let’s check whether the use of positive / negative words or specific emotions helps a project to be successful.

#### Stay positive
I calculated the tone of each text using the AFINN lexicon based on the positive and negative words used in each blurb. AFINN assigns a positive or negative score to each word (between -5 and 5) depending on how positive or negative the sentiment attached to the word is. I calculated a cumulative measure of negativity or positivity per blurb by adding up the scores on each individual word. Below, I visualized the relationship between the tone of the blurb and success.

While quite similar in nature in terms of the number of and magnitude of positive and negative words used in each blurb, less successful projects used a larger range of extreme positive or extreme negative words. Both successful and unsuccessful projects mainly use positive words (with a score above 0 averaging around a score of 3) overall.

![](../hw3/images/unnamed-chunk-7-1.png)

#### Positive v. Negative
I segregated all 2,000 blurbs into positive and negative texts based on their polarity score calculated in step (a). Then, I created a term-document-matrix in order to generate a comparison cloud showing the most-frequent positive and negative words.

Negative words most frequently include the use of “limited,” which is funny only because often times a “limited edition” is seen as a good thing! Other frequent negative terms include “hard,” “battle,” “lost” and “fight.”

Frequent positive words include “inspired,” “love,” “easy” and “share.” These seem like lifestyle words, as in projects that might include making someone’s life easier.

![](../hw3/images/unnamed-chunk-8-1.png)

#### Get in their mind
Lastly, I used the NRC Word-Emotion Association Lexicon to identify a larger set of emotions (anger, anticipation, disgust, fear, joy, sadness, surprise, trust). I visualized the relationship between success and range of emotions in two different ways; first with a bar chart and second with a lollipop chart. We can see in both that successful campaigns tend to use more of every type of emotional word except for “joy,” for which unsuccessful campaigns use more.

![](../hw3/images/unnamed-chunk-9-1.png)

The lollipop chart makes it easy for us to view the true difference in number of words per emotion for successful and unsuccessful campaigns. There’s the largest difference in use of negative words; successful campaigns use plenty more of these than unsuccessful campaigns– maybe the use of negative words adds to the drama of the project and leads to more backers?

![](../hw3/images/unnamed-chunk-10-1.png)



### Interactivity
For interactive plots, please see the [HTML File](.hw3/output/Beiers_Kickstarter.html).


## References
Some ideas and code were borrowed from Thomas Brambor (Professor, Columbia University) as well as the [Text Mining with R](https://www.tidytextmining.com/tidytext.html) textbook (Silge & Robinson).
