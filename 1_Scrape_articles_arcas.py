"""
This script is a modified version of the example script from:
https://github.com/ArcasProject/ArcasExamples/blob/initial_commits/scripts/scraping_num_articles.py  
The script demonstrates how to use arcas to collect a number of 
metadata of articles on a specific topic. In this example the five apis, Ieee,
Plos, Nature, arXiv and Springer and used. 
The keywords used to search articles are "sustainable software", 
"research software" and we are asking for a maximum number of 30 articles
from each api. 
In each search 10 articles are asked. 
"""
# pip  pip install arcas
import arcas
import pandas as pd

keywords = ["logistics"]
num_collect = 10
max_num = 100
dfs = []

"""
## INSTRUCTIONS FOR FIRST TIME RUNNING##
Only arcas.Arxiv and arcas.Plos work without API key
For the others you need to request annd set the API key in the installation folder of 
their respective API within Arcas. To find the location  of the installed arcis library,
1) run 'pip show arcas' 
2) go to the following directories and files to enter your
3) Enter your API keys in the files:
..\arcas\nature\api_key.py
..\arcas\IEEE\api_key.py
..\arcas\Ppringer\api_key.py

You first need to register an account to retrieveAPI keys at the API websites:
- https://dev.springernature.com/signup?cannot_be_converted_to_param
- https://developer.ieee.org/Quick_Start_Guide
- https://dev.springernature.com/signup
"""
for p in [arcas.Arxiv]:
    api = p()
    for key in keywords:
        start = 1
        switch = True
        while start < max_num and switch is True:
            parameters = api.parameters_fix(title=key, records=num_collect,
                                            abstract=key, start=start)

            url = api.create_url_search(parameters)

            request = api.make_request(url)
            root = api.get_root(request)
            raw_articles = api.parse(root)

            try:
                for art in raw_articles:
                    article = api.to_dataframe(art)
                    dfs.append(article)

            except:
                switch = False

            start += 10
df = pd.concat(dfs, ignore_index=True)
df.to_csv('arcas_search_results.tsv',sep="\t")
