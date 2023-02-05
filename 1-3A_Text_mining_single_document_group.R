#####################################################################################
## Template code for reproducible scientific text mining. This workflow can be 
## described in the following steps
## 1) Downloading articles as .pdf or abstract&reference data as BibTex
## 2) Loading and preprocessing documents and metadeta
## 3) Processing data
## 4) Analysis and visulation
## Author: N. A. Zondervan & F. T. Zondervan, contact nazondervan@gmail.com, or Github
#####################################################################################

## Uncomment to install all dependencies/packages
#install.packages("pdftools")
#install.packages("tm")
#install.packages ("stringr)
#install.packages ("tidyr)
#install.packages ("stringr")
#install.packages("DataCombine")
#install.packages("ggplot2")
#install.packages("wordcloud2")
#install.packages("pheatmap")
#install.packages("dendextend")
#install.packages("qgraph")
#install.packages("dplyr")
#install.packages("viridis")

## Load all libraries
library("pdftools")
library("tm")
library("stringr")
library("tidyr")
library("DataCombine")
library("ggplot2")
library('wordcloud2')
library("pheatmap")
library("dendextend")
library("qgraph")
library("dplyr")
library('viridis')

#####################################################################################
## 1A) Load files from multiple pdf's into a single text corpus using the tm library
## for loading abstracts from a dataframe/TSV, scroll down to 1B)
#####################################################################################

# ## Set Working directory containing articles/abstracts, if using RStudio, select in the menu:
# ## Session > SetWorkingDirecotory > SourceFileLocation
# articles_directory = 'articles/' # Directory to put your pdf articles in 
# files <- list.files(path = articles_directory, pattern = "pdf$")
# files <- paste(articles_directory,files,sep='')
# 
# ## Load all PDF's in the directory in a list and store them as as a Tm text corpus
# corpus <-  Corpus(URISource(files),
#                   readerControl = list(reader = readPDF))
# 
# ## Step 1B Load corpus, without filtering or altering text
# corpus_crude <- tm_map(corpus)


#####################################################################################
## 1B) Load text from a dataframe/Tab-Seperated-File, containing for example abstracts
## for loading PDF's scroll up to 1B)
#####################################################################################
## Tm VCorpus function, load a dataframe, col1 must be 'doc_id', col2 must be 'text', rest is metadata, must be utf-8
## load files
df <- read.csv(file='doi_abstract.tsv',sep='\t')

## reorder columns to start with doc_id and text, then rest
## rename and reorder columns to comply with tm package. Alternatively use the full title as 'doc_id'
df2 <- rename(df,doc_id=doi,text=abstract)
df2 <- df2 %>% select(doc_id, text,everything()) # Change order, to comply with tm

## Load df2 which is now in the right format for the tm package, as corpus
corpus <- VCorpus(DataframeSource(df2))


#####################################################################################
## 2) Loading & pre-processing data
## Load text from corpus into a Term-Document matrix
#####################################################################################
## Step 2, get TDM (Term Document Matrix), also cleans up the text

## Map and clean the corpus text
content.tdm <- TermDocumentMatrix(corpus, 
                                  control = 
                                    list(removePunctuation = TRUE,
                                         stopwords = TRUE,
                                         tolower = TRUE,
                                         stemming = TRUE,
                                         removeNumbers = TRUE,
                                         bounds = list(global = c(3, Inf),
                                                       removeWords, c("etc","ie", "eg", stopwords("english")), ## This line you can manually specify stopwords to drop
                                                       content_transformer(tolower),
                                                       tm_map(corpus, content_transformer(tolower))
                                         )))

## To get a feelling for the data, explore the data
## Have a look at a part or the whole of the Term Document Matrix
inspect(content.tdm[1:10,])
#View(as.matrix(content.tdm)) # For if you use RStudio

## In case one wants to autmatically determine the optimal number of clusters/groups in a number of articles
## https://stackoverflow.com/questions/15376075/cluster-analysis-in-r-determine-the-optimal-number-of-clusters#36729465

## We can normalize the data by making the TDM binary, this corrects for text being of different sizes
## Additionally it is convenient when looking at co-occurance of terms and allows us to perfrom binomial statistics later on
tdm.binary <- content.tdm # Create a copy to binarize
tdm.binary[content.tdm>=1] <- 1 # Replace the count of terms with a 1, only look at presence absence of terms



#####################################################################################
## 3) Processing: Frequency count and creating a Terms-Terms matrix
#####################################################################################
## Check frequent terms in the 'Term Document Matrix'. 
## If needed, use the word list from the frequency count to only select top 100 frequent terms 
## Play with the parameters below and document them in your metahdology
## These treshold should be adjusted to the amount of text you analyse

treshold_low = 100 # Lower treshold, document its value or share the code
treshold_up = Inf # Upper treshold, if there are common words with a high frequency still in your data set, you can filter them out
findFreqTerms(content.tdm, lowfreq = treshold_up, highfreq = treshold_up)

## define a function to get frequent terms from tdm, this time as dataframe
find_freq_terms_fun <- function(tdm){
  freq_terms <- findFreqTerms(content.tdm)[1:max(tdm$nrow)]
  terms_grouped <- tdm[freq_terms,] %>%
    as.matrix() %>%
    rowSums() %>%
    data.frame(Term=freq_terms, Frequency = .) %>%
    arrange(desc(Frequency)) %>%
    mutate(prop_term_to_total_terms=Frequency/nrow(.))
  return(data.frame(terms_grouped))
}

## run the find_freq_terms_fun, get a dataframe of the Frequent terms
df_terms_freq <- find_freq_terms_fun(tdm.binary)
## Optional extra Cleaning of frequent terms in case you find words that are not relevant for the analysis
remove <- c('can','will','make','also','use','may','implement','author', 'figur', 'includ', 'howev', 'journal',  'studi',"tabl","fig")
df_terms_freq <- df_terms_freq[!df_terms_freq$Term %in% remove, ]

## Inspect the results. The relative word frequency can be used to crate word clouds, discover trends, compare importance of Terms in multiple groups of articles
head(df_terms_freq)
tail(df_terms_freq)
#View(df_terms_freq)

## IMPORTANT, You can regenerate the TDM, only looking for words you selected based on their frequency,
## to filter you can use 'control = list(dictionary = words_selection)', with words_selection being the selection of words youy are interested in
words_selection <- head(df_terms_freq$Term,50) ## EXAMPLE, take top 50  terms. Youu can also use manually selected Terms, terms of interest to your study!
                     
## Filter both the normal and the binary TDM on words you are interestd in 
tdm.select <- content.tdm[rownames(content.tdm) %in% words_selection,]
tdm.binary.select <- tdm.binary[rownames(tdm.binary) %in% words_selection,]

## Remove NaN, remove rows and columns with only zero values, usefull in case you searched for a Term and it was not found
m <- as.matrix(tdm.binary.select)
m[is.nan(m)] = 0
m <- m[rowSums(m[])>0,]
m <- m[,colSums(m[])>0]



#####################################################################################
## 4) Final analysis and Visualisations
#####################################################################################

#### VISULATION 1 ####
## Create a word cloud with size based on frequency, nice for presentations
## WARNING: The Wordcloud package generates html code, you have to use R-Studio, click on export, to save as .png
treshold <- 50 ## Set this treshold and document it, all words with lower count will not be shown
wordcloud2(df_terms_freq,minSize = 50)
dev.off()

## Look at correlation between terms, first Convert to Term_Term matrix, to see which words are co-occuring a lot
m_backup <- m
term_term_matrix <- m %*% t(m) # Get Term-Term matrix, so how many documents terms co-occur
distance_matrix <- as.matrix(dist(term_term_matrix)) # Euclidian distance, nice for plotting the distance between terms in a heatmap
cor.matrix <- cor(term_term_matrix,method="pearson") # Get the correlation matrix based on correlation/co-occurance of Terms in the the binary matrix

#### VISULATION 2 ####
## Dendrogram
## Plot a dendogram from the hierarchically clustered terms based on the Euclidian distance
## Mostly redundant since dendogram is also present in the heatmap, but you can extract the clustering of terms from the horizontal clustering
m_h_clust_terms <- hclust(dist(m), method = "complete")
png("4A dendogram_horizontally_clustered_terms.png",width = 450, height = 600, units='mm', res = 300)
as.dendrogram(m_h_clust_terms) %>%
  plot(horiz = TRUE)
dev.off() 

#### VISULATION 2 ####
## Plot Terms-Terms matrix, get grouping as annotation from the dendrogram basd on the Euclidian distance
## NOTE: Heatmaps of clustering are one of the most usefull visualisations for accademic analysis and publishing
dist_mat_annotation  <- data.frame(cluster = as.factor(cutree(tree = as.dendrogram(m_h_clust_terms), k = 3)))
dist_mat_annotation$frequency  <-  df_terms_freq$Frequency[match(row.names(distance_matrix),df_terms_freq$Term)]
png("4A Heatmap with horizontal clustering of Terms euclidian distance.png",width = 300, height = 300, units='mm', res = 300)
  pheatmap(distance_matrix, annotation_row = dist_mat_annotation, color = viridis(n=100),fontsize=13)  
dev.off() 

#### VISULATION 2 ####
## Plot Terms-Terms matrix, get grouping as annotation from the dendrogram from the heatmap itself, so based on Pearson correlation
## NOTE: Heatmaps of clustering are one of the most usefull visualisations for accademic analysis and publishing 
result <-  pheatmap(cor.matrix,fontsize=13)
dev.off()
pearson_annotation <- data.frame(cluster =  as.factor(cutree(result$tree_row, k = 3)))
pearson_annotation$frequency  <-  df_terms_freq$Frequency[match(row.names(cor.matrix),df_terms_freq$Term)]
png("4A Heatmap with horizontal clustering of Terms pearson correlation.png",width = 300, height = 300, units='mm', res = 300)
  pheatmap(cor.matrix, annotation_row = pearson_annotation, color = viridis(n=100),fontsize=13)  
dev.off() 

#########################
## Below we will show a couple of example Network graphs
## NOTE, with minimum you can remove weaker nodes, but you have to document the setings for maximum reproducability
## Although network graphs look intersting, in many cases heatmaps are more insightfull and suited for publication
#########################

#### VISULATION 4 ####
## Network visualisation using qqgraph
cor.matrix2 <- cor.matrix

png("4A Network of Terms clustering.png",width = 600, height = 600, units='mm', res = 300)
  qgraph(cor.matrix,minimum=0.5,labels =rownames(cor.matrix), vsize=3,shape='ellipse',legend=TRUE,borders=FALSE,layout = "spring",theme="classic",normalize=TRUE,legend=TRUE)
dev.off() 

#### VISULATION 5 ####
## Network visualisation using qqgraph, with groups and using frequency for the node size
group_annotation <- pearson_annotation$cluster
group_annotation <- setNames(group_annotation, rownames(pearson_annotation)) # Create named list of Term as name and cluster as value
scaling_parameter <- 500 # Tweak this variable, higher for smaller nodes, to make your graph readable
size_annotation <- pearson_annotation$frequency**0.5/10 # Take the root value, so the surface area = frequency, keeps text readable
size_annotation<- setNames(size_annotation, rownames(pearson_annotation)) # Create named list of Term as name and cluster as value

## Plot using the group annotation for color and size annotation for node size. So similar to a WordCloud, but with clustering
png("4A Network of Terms clustering_groups_frequency_size1.png",width = 600, height = 600, units='mm', res = 300)
  qgraph(cor.matrix,minimum=0.5,labels =rownames(cor.matrix),groups=group_annotation, vsize=size_annotation,shape='ellipse',legend=TRUE,borders=FALSE,layout = "spring",theme="classic",normalize=TRUE,legend=TRUE)
dev.off() 

#### VISULATION 7 ####
## Same as above, but using different layout. We recommend trying a few layouts such as 'sprins', 'circle'
png("4A Network of Terms clustering_groups2.png",width = 600, height = 600, units='mm', res = 300)
  qgraph(cor.matrix,minimum=0.5,labels =rownames(cor.matrix),groups=group_annotation, vsize=3,shape='ellipse',legend=TRUE,borders=FALSE,layout = "circle",theme="classic",normalize=TRUE)
dev.off()        
       