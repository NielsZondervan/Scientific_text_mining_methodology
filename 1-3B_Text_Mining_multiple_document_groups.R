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

## Define a function to get frequent terms from TDM, output is a dataframe
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


#####################################################################################
## 1A) Load files from multiple pdf into a single text corpus using tm
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
# 
# ## Note, you can seperate articles on containing a keyword, or you can retrieve metadata from abstract
# ## See 1B for loading abstract information and the metadata abstracts provides with the example data :
# ## "abstract"  "address"   "booktitle" "doi"       "edition"   "isbn"      "issn"      "journal"   "keywords"  "note"      "number"   
# ## "pages"     "publisher" "series"    "title"     "url"       "volume"    "year"
# 


#####################################################################################
## 1B) Load text from a dataframe/Tab-Seperated-File, containing for example abstracts
## for loading PDF's scroll up to 1B)
#####################################################################################
## Tm VCorpus function, load a dataframe, col1 must be 'doc_id', col2 must be 'text', rest is metadata, must be utf-8
## load files
df <- read.csv(file='doi_abstract.tsv',sep='\t')

## rename and reorder columns to comply with tm package. Alternatively use the full title as 'doc_id'
df2 <- rename(df,doc_id=doi,text=abstract)
df2 <- df2 %>% select(doc_id, text,everything()) # Change order, to comply with tm

## Metadata you can use for grouping. In many cases it makes more sence to provide manual grouping, e.g. articles on
colnames(df2)

## In our example, we compare two time periods, pre-covid19 and covid19.
## Commented out is another example, using the top 10 journals as groups
df2 <- df2[which(df2$year %in% c(2016,2017,2018,2019,2020,2021)),]
df2$group_doc[df2$year %in% c(2016,2017,2018)] <- "pre-Covid19"
df2$group_doc[df2$year %in% c(2019,2020,2021)] <- "Covid19"
groups <- unique(df2$group_doc)


#####################################################################################
## 2) Loading & pre-processing data
## Load text from corpus into a Term-Document matrix
## 3) Processing
## - Step 2 and Step 3 are combined since both pre-processing and processing such as statistics
## - are performed in the same for loop
#####################################################################################
## Step 2, get TDM (Term Document Matrix), binary TDM, clean and do basic Statistics

## Map and clean the corpus text, load in a for loop per document group
df_terms_freq_all <- data.frame() #Also get frequency per group, add the long dataframe for plotting with ggplot
for (g in groups){

  ## Step 1A, PDF to text (corpus), uncomment this section if you want to load pdfs, you would need to have a dataframe with file_name and metadta
  ## fields to seperate on. We do not provide examples files for that, see article for instructions on how to obtani full article pdf's
  # files_select <- grep(g, df$journal,value=T) # Note, here we search for journal names in journal column, replace with any other column you want to group on
  # articles_text <- lapply(files, pdf_text)
  
  # corpus <-  Corpus(URISource(files_select),
  #                   readerControl = list(reader = readPDF))
  
  ## Step 1B, load abstracts from dataframe to text (corpus)
  ## Load df2 which is now in the right format, as corpus
  df_selected <- df2[as.character(df2$group_doc) == g,] # CHANGE THIS IF YOU WANT TO SELECT BASED ON ANOTHER FIELD!
  corpus <- VCorpus(DataframeSource(df_selected))
  
  ## Step 1B Reads the corpus text, but cleans it up as well
  content.tdm <- TermDocumentMatrix(corpus, 
                                             control = 
                                               list(removePunctuation = TRUE,
                                                    stopwords = TRUE,
                                                    tolower = TRUE,
                                                    stemming = TRUE,
                                                    removeNumbers = TRUE,
                                                    bounds = list(global = c(3, Inf),
                                                                  #removeWords, c("etc","ie", "eg", stopwords("english")),
                                                                  content_transformer(tolower),
                                                                  tm_map(corpus, content_transformer(tolower))
                                                    )))
  
  ## we can normalize by making the TDM binary, not counting multiple occurance of Tersm per document
  tdm.binary <- content.tdm # Create a copy to binarize
  tdm.binary[tdm.binary>=1] <- 1 # normalize/scale by countil multiple occurance only one, alternatively devide by column sum
  
  ## get frequency counts
  df_terms_freq <- find_freq_terms_fun(tdm.binary)
  df_terms_freq$group_doc <- g
  
  ## STATISTICS: Since we use the binary term document matrix, we can get confidence interval assuming a binomial distribution
  n <- dim(tdm.binary)[2] ## n is the total number of documents
  # Loop over terms, perform proption test, get confidence interval, store in df_terms_freq
  for (i in 1:dim(df_terms_freq)[1]){
    x <- df_terms_freq$Frequency[i]
    results <- prop.test(x=x, n=n,conf.level=0.95, correct = FALSE) # We chose a 95% confidence interval, adjust if needed
    conf_int <- results$conf.int
    ## Get foncidence interval for absolute count
    df_terms_freq$conf_low[i] <- conf_int[1]*n
    df_terms_freq$conf_up[i] <- conf_int[2]*n
    ## Get confidence interval for proportional term to total, just scaling
    df_terms_freq$conf_low_prop[i] <- conf_int[1]/results$estimate *df_terms_freq$prop_term_to_total_terms[i]
    df_terms_freq$conf_up_prop[i] <- conf_int[2]/results$estimate *df_terms_freq$prop_term_to_total_terms[i]
  }

  ## Combine data frame fo each period, end of for loop
  df_terms_freq_all <- rbind(df_terms_freq_all,df_terms_freq)
  ## END for loop
}


############
## OPTIONAL 1: Fixing trimmed/stemming of words

## Some usefull  code if you want to transform words back from their common 'stemming' root to something more readbale in a publication
## Make a dataframe with two collumns, words as they are due to stemming, and words as you would like to see them in your final analysis and plots
rename_words <- data_frame(not_correct = c("logist","use","model","studi","system","result","develop","transport","problem","cost","product","research","propos","can","suppli","time","paper","oper","chain","provid","method","optim","process","perform","manag","data","differ","patient","network","servic","base","increas","effect","improv","approach","vehicl","solut","analysi","industri","also","effici","deliveri","includ","show","impact","consid","algorithm","case","design","technolog","present","sustain","rout","signific","plan","distribut","new","environment","decis","order","identifi","area","inform","requir","import","level","implement","evalu","need","practic","reduc","compar","integr","one","factor","urban","locat","compani","demand","applic","two","relat","assess","potenti","high","aim","total","challeng","program","find","set","object","risk","citi","manufactur","howev","emiss","support","custom","wood","que","bioenergi","weee","pellet","lsps","ahp","rail","crane","biofuel","formal","pilot","europ","lifecycl","hous","surgeri","side","european","ewast","batch","chemic","soil","rfid","biomass","instal","layout","abstract","feedstock","pts","determinist","maker","reus","look","assembl","parti","ghg","skill","closedloop","acquisit","remanufactur","polit","repair","cope","largest","multicriteria","hub","medicin","iot","advers","dataset","cold","cohort","internet","serious","damag","thing","remot","accuraci","diagnosi","accid","therapi","demograph","financ","robot","digit","baselin","militari","hydrogen","lowcarbon","batteri","consult","infect","resili","donor","lung","cart","therapeut","lastmil","visit","circular","physician","theme","odd","split","fund","drone","compact","uav","honoraria","arctic","mortal","burn","graft","transfus","covid","pandem","blockchain"),
                           correct = c("logistic","use","model","study","system","result","developer","transport","problem","cost","product","research","propose","cane","supply","time","paper","operate","chain","provide","method","optimum","process","perform","manage","data","differ","patient","network","service","base","increase","effect","improve","approach","vehicle","solution","analysis","industrial","also","effective","delivery","include","show","impact","consider","algorithm","case","design","technology","present","sustain","rout","significant","plan","distribute","new","environment","decision","order","identify","area","inform","require","import","level","implement","evaluate","need","practice","reduce","compare","integrate","one","factor","urban","locate","company","demand","application","two","relate","assess","potential","high","aim","total","challenge","program","find","set","object","risk","city","manufacturer","however","emiss","support","custom","wood","que","bioenergy","weee","pellet","lsps","ahp","rail","crane","biofuel","formal","pilot","europ","lifecycle","house","surgery","side","european","ewast","batch","chemical","soil","rfid","biomass","install","layout","abstract","feedstock","pts","deterministic","maker","reuse","look","assemble","parti","ghg","skill","closedloop","acquisit","remanufactur","political","repair","cope","largest","multicriteria","hub","medicine","iot","adverse","dataset","cold","cohort","internet","serious","damage","thing","remote","accuracy","diagnosis","accident","therapy","demographics","finance","robot","digit","baseline","military","hydrogen","lowcarbon","battery","consult","infect","resilience","donor","lung","cart","therapeut","lastmile","visit","circular","physician","theme","odd","split","fund","drone","compact","uav","honoraria","arctic","mortal","burn","graft","transfus","covid","pandem","blockchain"))
df_terms_freq <- FindReplace(data = df_terms_freq, Var = "Term", replaceData = rename_words,from = "not_correct", to = "correct", exact = TRUE)
df_terms_freq_all_backup <- df_terms_freq_all

## END OPTIONAL 1: Fixing trimmed/stemming of words1
############

## Check if for every group there is a data point, if not add as 0 value
## This will make ploting of results more straightforward
for (term in unique(df_terms_freq_all$Term)){
  for (g in unique(df_terms_freq_all$group_doc)){
    if (!(term %in% df_terms_freq_all$Term[df_terms_freq_all$group_doc == g])){
      df_terms_freq_all <- rbind(df_terms_freq_all, c(term,0,0,g))
    }
  }
}

## Fix rbind messing up data types, if you add other ew columns you have to manualy specify their type!
df_terms_freq_all$Frequency <- as.integer(df_terms_freq_all$Frequency)
df_terms_freq_all$prop_term_to_total_terms <- as.double(df_terms_freq_all$prop_term_to_total_terms)
df_terms_freq_all$group_doc <- as.factor(df_terms_freq_all$group_doc)
df_terms_freq_all$Term_all <- as.factor(df_terms_freq_all$Term)
df_terms_freq_all$conf_low <- as.double(df_terms_freq_all$conf_low)
df_terms_freq_all$conf_up <- as.double(df_terms_freq_all$conf_up)
df_terms_freq_all$conf_low_prop <- as.double(df_terms_freq_all$conf_low_prop)
df_terms_freq_all$conf_up_prop <- as.double(df_terms_freq_all$conf_up_prop)

## Replace missing values for confidence interval with 0, otherwise they will be dropped when plotting
df_terms_freq_all[is.na(df_terms_freq_all)] <- 0



#####################################################################################
## 4) Processing and 
## Analysis and plotting
#####################################################################################

#####
## Example 1, plotting top 100 words over all groups
## Here we summ the Frequency and prop_term_to_total_term of all groups
## So basically we treat them again as a single corpus to get the most abundant ones
## After we get this informatin, we use this list of terms for plotting per group
unique_terms = unique(df_terms_freq_all$Term)
df_terms_freq = data.frame()
for (term in unique_terms){
  selected_rows <- df_terms_freq_all[as.character(df_terms_freq_all$Term)==term,]
  freq = sum(selected_rows$Frequency)
  prop_term_to_total_terms = sum(selected_rows$prop_term_to_total_terms*selected_rows$Frequency)/freq ## This  calculates back the original propo
  df_row <- data.frame(Term = term, Frequency =  freq, prop_term_to_total_term = prop_term_to_total_terms,stringsAsFactors = FALSE)
  df_terms_freq <- rbind(df_terms_freq,df_row)
}

## Get top 50 or top 100 most abundant terms
df_terms_freq <- df_terms_freq[order(df_terms_freq$Frequency, decreasing = TRUE), ]
top_100_terms <- df_terms_freq$Term[0:100]
top_50_terms <- df_terms_freq$Term[0:50] 

## Now retrieve information for these top 100 terms. First we order the Term by the factor column
df_terms_freq_all <- df_terms_freq_all[order(df_terms_freq_all$Frequency, decreasing =  TRUE), ]
df_terms_freq_all$Term <- factor(df_terms_freq_all$Term, levels=unique(as.character(df_terms_freq_all$Term))) 
df_terms_freq_long_top100 <- df_terms_freq_all[df_terms_freq_all$Term %in% top_100_terms,]
df_terms_freq_long_top50 <- df_terms_freq_all[df_terms_freq_all$Term %in% top_50_terms ,]

### VISUALISATION 1 BAR PLOT of the absolute term frequency (not so usefull since absolute while groups can be of different sizes)
## Note: Bar plots work great in pairwise comparison, not so great when comparing large groups. 
##       Better to use a heatmap when working with many groups

png("4B_bar_plot_term_frequency_per_group.png",width = 900, height = 400, units='mm', res = 300)
 ggplot(df_terms_freq_long_top50, aes(x=Term, y=Frequency, fill=group_doc)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90),
        axis.text=element_text(size=24,face="bold"), 
        axis.title=element_text(size=36,face="bold"))+
  geom_errorbar(aes(ymin=df_terms_freq_long_top50$conf_low, max=df_terms_freq_long_top50$conf_up), 
                width=.2, position=position_dodge(width=0.7)) 
dev.off()

### VISUALISATION 2 BAR PLOT of the proportional term frequency (usefull results, since it is normalized)
png("4B_bar_plot_term_proportional_frequency_per_group.png",width = 900, height = 400, units='mm', res = 300)
ggplot(df_terms_freq_long_top50, aes(x=Term, y=prop_term_to_total_terms, fill=group_doc)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90),
        axis.text=element_text(size=24,face="bold"), 
        axis.title=element_text(size=36,face="bold"))+
  geom_errorbar(aes(ymin=df_terms_freq_long_top50$conf_low_prop, max=df_terms_freq_long_top50$conf_up_prop), 
                width=.2, position=position_dodge(width=0.7)) 
dev.off()

#####################################################################################
## Finding Terms of interest
## - We find terms of interst by looking at their fold change between groups of documents
#####################################################################################

## As researcher you are interested in differnces between groups
## In this example we will look at keywords most different between two groups
## The code scales for any number of groups

## Set your relative group, calculate factor and p-value when comparing groups to the chosen relative gorup
groups_doc <- unique(df_terms_freq_all$group_doc)
nr_groups_doc <- length(groups_doc)
relative_to <- 1 # IMPORTANT, select here the reference group for the bionomial test, in our case devided by "pre-Covid19"
length_df <- dim(df_terms_freq_all)[1]
df_terms_freq_all <- df_terms_freq_all[order(df_terms_freq_all$Term),] # Make sure the same terms in different groups are next to eachother

## Loop over rows of same term perform binomial two sided test to get p-value for them not being equal
for(i in (0:((length_df-1)/nr_groups_doc)*nr_groups_doc+1)){
  ## Get factor for proportion in one group to the max
  for (j in (i:(i+nr_groups_doc-1))){
    ## Get p-value for groups being different, based on two-sided binomail test
    group_relative_to_p <- df_terms_freq_all$prop_term_to_total_terms[i+relative_to-1]
    group_test_n <- df_terms_freq_all$Frequency[j]
    group_test_total <- df_terms_freq_all$Frequency[j]/df_terms_freq_all$prop_term_to_total_terms[j]
    ## Try to get two sided binomial test, store p-value, if low numbers return 1 as p-value
    tryCatch(
      {
      binomial_test_results <- binom.test(group_test_n , group_test_total, group_relative_to_p)
      p_value <- binomial_test_results$null.value # We get the p-value for the null hypothesis: two samples being equal
      },error=function(err) 
      {1 # Meaning 1 is return as p-value if x or n = 0
      }) # Return a probability of 1 if we cannot determine the probability due to x or n being 0
    df_terms_freq_all$p_value[j] <- p_value
  }
}

## In this example we will filter on the factor with an addition filter of prop_term_to_total_terms > 0.005 # Play with this parameter
## Note: it is always advisable to use a minimal prop_term_to_total_terms when using the factor, to avoid low occurance Terms from popping up
min_prop_freq <- 0.005 # PARAMETER, should be tweaked to your data and reported

## We will filter out all terms that are rarely occuring, proportional frequency below min_prop_freq
filtered_terms <- unique(as.character(df_terms_freq_all$Term[which(df_terms_freq_all$prop_term_to_total_terms > min_prop_freq)])) ## Get terms with at least one group above 0.01
df_terms_freq_all_filtered <- df_terms_freq_all[which(df_terms_freq_all$Term %in% filtered_terms),]
## This code is for 2 groups, calculate factor. WARNING, for more than two groups you need to manually add code since calculatin a factor is unambigious
index_group1 <- which(df_terms_freq_all_filtered$group_doc == groups_doc[1])
index_group2 <- which(df_terms_freq_all_filtered$group_doc == groups_doc[2])
## Calculate the factor (the proportional frequency of a term in group devided by the proportional frequency in another group)
df_terms_freq_all_filtered$factor[index_group1] <- df_terms_freq_all_filtered$prop_term_to_total_terms[index_group1]/df_terms_freq_all_filtered$prop_term_to_total_terms[index_group2]
df_terms_freq_all_filtered$factor[index_group2] <- df_terms_freq_all_filtered$prop_term_to_total_terms[index_group2]/df_terms_freq_all_filtered$prop_term_to_total_terms[index_group1]
## Retrieve top 100 terms for group1 and for group2
df_terms_group1 <- df_terms_freq_all_filtered[index_group1,]
df_terms_group2 <- df_terms_freq_all_filtered[index_group2,]
## Get terms most dominant (highest factor, for group1 and group2 respectively)
top_group1 <- unique(as.character(df_terms_group1$Term[order(df_terms_group1$factor, decreasing = TRUE )]))
top_group2 <- unique(as.character(df_terms_group2$Term[order(df_terms_group2$factor, decreasing = TRUE )]))
## Get dataframe of top 100 for group1 and group2 for plotting
df_terms_group1_top_100 <- df_terms_freq_all_filtered[as.character(df_terms_freq_all_filtered$Term) %in% top_group1[0:100],]
df_terms_group1_top_100$Term <- factor(as.character(df_terms_group1_top_100$Term),levels=top_group1) #Order according to factor
df_terms_group2_top_100 <- df_terms_freq_all_filtered[as.character(df_terms_freq_all_filtered$Term) %in% top_group2[0:100],]
df_terms_group2_top_100$Term <- factor(as.character(df_terms_group2_top_100$Term),levels=top_group2) #Order according to factor

### VISUALISATION 3 BAR PLOT of the top 100 terms of group 1, proportional term frequency
png("4B_bar_plot_top_100_terms_group1.png",width = 900, height = 400, units='mm', res = 300)
ggplot(df_terms_group1_top_100, aes(x=Term, y=prop_term_to_total_terms, fill=group_doc)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90),
        axis.text=element_text(size=24,face="bold"), 
        axis.title=element_text(size=36,face="bold"))+
  geom_errorbar(aes(ymin=df_terms_group1_top_100$conf_low_prop, max=df_terms_group1_top_100$conf_up_prop), 
                width=.2, position=position_dodge(width=0.7)) 
dev.off()

### VISUALISATION 4 BAR PLOT of the top 100 terms of group 1, proportional term frequency
## Plot the bottom 100, so those Terms less frequent in group2 (Covid-19) compared to group1 (pre-Covid19)
png("4B_bar_plot_top_100_terms_group2.png",width = 900, height = 400, units='mm', res = 300)
ggplot(df_terms_group2_top_100, aes(x=Term, y=prop_term_to_total_terms, fill=group_doc)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90),
        axis.text=element_text(size=24,face="bold"), 
        axis.title=element_text(size=36,face="bold"))+
  geom_errorbar(aes(ymin=df_terms_group2_top_100$conf_low_prop, max=df_terms_group2_top_100$conf_up_prop), 
                width=.2, position=position_dodge(width=0.7)) 
dev.off()


#####################################################################################
## EXAMPLE 2 LOOKING AT GROUPS OF KEYWORDS OF INTEREST (for example keywords you identified to be important
## You can not only look at groups of articles, but also at groups of Terms/words, see example below
#####################################################################################

terms_resiliance <- c("resilience","adapt","robust","risk","disrupt","strateg","adverse","cope")
terms_digital <- c("iot","internet","dataset","digit","robot","drone","uav","rfid")
terms_sustainable <- c("wood","biofuel","bioenergy","biodiesel","biomass","battery","hydrogen","lowcarbon")
search_terms <-c(terms_resiliance,terms_digital,terms_sustainable)

## Add KEYWORD GROUPING to column 'group2', select only those in the search terms
df_terms_freq_all$group_term <- NA
df_terms_freq_all$group_term[which(df_terms_freq_all$Term %in% terms_resiliance)] <- "Resilience"
df_terms_freq_all$group_term[which(df_terms_freq_all$Term %in% terms_digital)] <- "Digitalisation"
df_terms_freq_all$group_term[which(df_terms_freq_all$Term %in% terms_sustainable)] <- "Sustainability"
df_terms_freq_keywords <- df_terms_freq_all[which(df_terms_freq_all$Term %in% search_terms),]

## In this example, we will look at the development of keyword groups, aggregated by summing their proportional frequency, per article group
## Convert long dataframe to wide matrix, Plot heatmap per group of articles and per group of terms <++++ PROBLEMS HERE BELOW
wide_dataframe <- acast(df_terms_freq_keywords, group~group_term, value.var="prop_term_to_total_terms",fun=sum) # go from long dataframe to matrix with as value proportional terms

### VISUALISATION 5 HEATMAP
## Heatmap of the sum of relative occurance of keywords groups (Resilience, Digitalisation, Sustainability) per article category (pre-Covid-19, Covid-19)
png("4B heatmap_keywords-group_per_article-group.png", width = 300, height = 200, units='mm', res = 300)
  pheatmap(wide_dataframe,display_numbers=TRUE) 
dev.off()

### VISUALISATION 6 BUBBLE PLOT
## Bubble plot keywords and their occurances in groups of articles.This works best with more than two article groups or in a time-series analysis
df_terms_freq_keywords <- df_terms_freq_keywords[order(df_terms_freq_keywords$group_term),] # Order so that keywords from the same group are next to each other
df_terms_freq_keywords$Term <- factor(df_terms_freq_keywords$Term,levels = unique(df_terms_freq_keywords$Term)) # Make the Term column a factor, else ggplot will reorder the dataframe
png("4B Bubble plot of groups of terms per document group.png",width=7,height=5,units="in",res=1200)
ggplot(df_terms_freq_keywords, aes(x=group_doc, y=Term, size = prop_term_to_total_terms,color= group_term),  group = group_doc)+
  geom_point()+
  theme(axis.text.y = element_text(face='bold'),
        axis.text.x = element_text(face="bold"),
        axis.title = element_text(face="bold",size=14))
dev.off()
