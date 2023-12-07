## Datasets

### The datasets are from this link:
^[https://www.kaggle.com/datasets/abecklas/fifa-world-cup?select=WorldCups.csv](https://www.kaggle.com/datasets/abecklas/fifa-world-cup?select=WorldCups.csv)

The source is Fifa World Cup Archive

The raw datasets are in the raw folder.

I am using three datasets which I have cleaned:
- WorldCupsCleaned.csv 
- WorldCupMatchesCleaned.csv
- Cups_Matches_Joined.csv

I included codebooks for the variables:
- wc_codebook.rds
- wc_match_codebook.rds

The World Cups dataset contains information on the overall World Cup, with one row for each World Cup, and has variables of the year it took place, which country won, the total goals scored, and more. The World Cup Matches dataset contains information on each match played in each World Cup, and contains variables of the year it took place, the city and stadium where the match was held, home team name, and more. I have cleaned and stored the datasets, and joined them to create the Cups_Matches_Joined dataset.


I also have a subsetted dataset which I created:
- goals_bycountry_perWC.csv

This dataset provided information on the total number of goals scored by each country per World Cup.




