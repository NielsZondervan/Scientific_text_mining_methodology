# Scientific_text_mining_methodology
An open-source scientific text mining methodology for reproducible text mining. We provide template code for analysing single and multiple groups of documents (such as articles and abstracts), or time-based analysis. The method includes mock data and can be used for scientific text mining or as educational material. Before analysing any text, the researcher/students need to search and collect articles based on pre-defined search criteria.

On a high level, the text mining method can be devided in three steps:
1)	Loading and cleaning of text from articles
2)	Processing, statistical analysis, and clustering 
3)	Presentation of results using generalized and tailor-made visualizations

Visualisation examples are provided for ready to publish heatmaps, network graphs, line plots, bar plots, bubble plots and dendrograms. This method supports both automated and manual grouping of either or both Terms and Documents, and provides examples for each.


# Code
The code is separated in tree example templates. It is recommended to always start with the first template code to analyse documents as a single group, to get a better understanding and feeling for the data before moving on to more advanced analysis.
![Code schematic](https://github.com/NielsZondervan/Scientific_text_mining_methadology/blob/main/GraphicalAbstract.png)

# Dependencies
Tested with R version 3.6.1 (2019-07-05)
| Library| Version| 
|--------|--------|
| viridis	| 0.5.1 |      
| viridisLite |	0.3.0 |
| dplyr	| 0.8.3 | 
| qgraph | 1.6.9 |
| dendextend	| 1.13.2 | 
| pheatmap	| 1.0.12 |
| wordcloud2 	| 0.2.1 | 
| ggplot2	| 3.2.1 |    
| DataCombine	| 0.2.21  |
| tidyr	| 1.0.0  |
| stringr	| 1.4.0  |     
| tm	| 0.7-8 |    
| pdftools	| 3.0.1 |  

# Data
This method can be used with text data such as articles as PDF's or with abstract information as BibTex format. Free to use mock data is provided which can be directly used with the code to generate all example figures. 

# References
This methodology was built by generalizing the code used for multiple peer reviewed articles and includes additional visualisation examples.
* http://dx.doi.org/10.3390/pr10122667
* http://dx.doi.org/10.1016/j.aquaculture.2022.739158
* http://dx.doi.org/10.1016/j.ocecoaman.2022.106149
* This methodology is submitted for review to MethodX, Elsevier. Reference to the published article will be provided upon acceptance.

# Licence
 Apache License 2.0

