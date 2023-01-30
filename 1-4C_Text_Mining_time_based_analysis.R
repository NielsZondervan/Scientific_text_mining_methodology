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
#install.packages("resphape2")
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
library("resphape2")
library("stringr")
library("tidyr")
library("DataCombine")
library("ggplot2")
library('wordcloud2')
library("pheatmap")
library("dendextend")
library("qgraph")
library("dplyr")

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
# ## Note, you can seperate articles on containing a keyword, or you can retrieve metadata from loading their abstract information
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
## slightly different topics, e.g. articles with keywords 'ai' versus articles which contain keyword 'machine learning'
## To explore which 

## NOTE: As a method a timebased analysis is not much different from multiple groups, only now the order matters
## Hence we just select 'year' as group_doc, alternatively, month data can be parsed from the doi column with regular
## expressions if needed. Quarterly data can in turn be infered from the month data.
groups <- c(2016,2017,2018,2019,2020,2021)
df2 <- df2[which(df2$year %in% groups),] ## Filter to be sure there are no articles outside of our defined time period
df2$group_doc <- df2$year ## So we can use roughly the same code as for multiple groupings


#####################################################################################
## 2) Loading & pre-processing data
## Load text from corpus into a Term-Document matrix
## 3) Processing
## - Step 2 and Step 3 are combined since both pre-processing and processing such as statistics
## - happen in the same loop
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
  df_selected <- df2[df2$group_doc == g,] # CHANGE THIS IF YOU WANT TO SELECT BASED ON ANOTHER FIELD!
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
  
  ## we normalize by making the TDM by making it binary, so not counting multiple occurance of Tersm per document
  tdm.binary <- content.tdm # Create a copy to binarize
  tdm.binary[tdm.binary>=1] <- 1 # normalize/scale by countil multiple occurance only one, alternatively devide by column sum
  
  ## get frequency counts
  df_terms_freq <- find_freq_terms_fun(tdm.binary)
  df_terms_freq$group_doc <- g
  
  ## STATISTICS: Since we use the binary term document matrix, we can get confidence interval assuming a binomial distribution
  n <- dim(tdm.binary)[2] ## n is the total number of documents that could have a term
  # Loop over rows, perform proption test, get confidence interval, store in df_terms_freq
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

  ## combine data frame fo each period, end of for loop
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
df_terms_freq_all$Term <- as.factor(df_terms_freq_all$Term)
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


### VISUALISATION 1
#### One large PDF with plot of the trend forr the top 100 terms with a linear trendline and confidence interval

## order to prop_term_to_total_terms, since axis scaling per page are fixed and not adjusted per term
df_terms_freq_all_sorted <- df_terms_freq_all[order(df_terms_freq_all$prop_term_to_total_terms,decreasing = TRUE), ]
df_terms_freq_all_sorted_top_100 <- all_sorted[as.character(df_terms_freq_all_sorted$Term) %in% top_100_terms,]
terms_to_plot <- unique(df_terms_freq_all_sorted_top_100$Term)

## Get number of terms and make a fecet plot for each of those terms, 20 per page
## NOTE: you can adjust this parameter if desired so, but you also have to adjust the line with facet_wrap()
nr_plots_per_page <- 20 # 
nr_terms <- length(terms_to_plot)
pdf("4C_terms_yearly_trend_100_terms.pdf") #20  per page, round of +1 for page
for (i in 1:round(nr_terms/nr_plots_per_page)){
  # Basic line plot with points
  begin <- (i-1)*nr_plots_per_page+1
  end <- i*nr_plots_per_page
  print(begin)
  print(end)
  terms <- terms_to_plot[begin:end]
  data <- df_terms_freq_all[df_terms_freq_all$Term %in% terms,]
  data$Term=factor(data$Term,levels=unique(data$Term))  # To make sure the order is not changed, used in facegrid 
  p1 <- ggplot(data=data, aes(x=group_doc, y=prop_term_to_total_terms,group=Term)) +
    #geom_line()+
    geom_point()+
    ggtitle("term",)+ # for the main title
    ylab("relative frequency")+ # for the x axis label
    geom_smooth(method=lm,level=0.95,color='blue')+ #add linear trend line+
    #geom_smooth(method=loess,level=0.95,color='red')+ #add linear trend line+
    theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    #scale_y_continuous(breaks = seq(0, max(data$prop_term_to_total_terms), by=0.1), limits=c(0,1000)): 
    facet_wrap(~Term, ncol = 4, nrow=5)
  plot(p1)
}
dev.off()

#####################################################################################
## EXAMPLE 2 LOOKING AT GROUPS OF KEYWORDS OF INTEREST (for example keywords you identified to be important
## You can not only look at groups of articles, but also at groups of Terms/words, see example below
#####################################################################################

terms_resiliance <- c("resilience","adapt","robust","risk","disrupt","strateg","adverse","cope")
terms_digital <- c("iot","internet","dataset","digit","robot","drone","uav","rfid")
terms_sustainable <- c("wood","biofuel","bioenergy","biodiesel","biomass","battery","hydrogen","lowcarbon")
terms_medical <- c("surgery","cold","medicine","cohort","lung","infect", "donor","covid","vaccin")
search_terms <-c(terms_resiliance,terms_digital,terms_sustainable)

## Add KEYWORD GROUPING to column 'group2', select only those in the search terms
df_terms_freq_all$group_term <- NA
df_terms_freq_all$group_term[which(df_terms_freq_all$Term %in% terms_resiliance)] <- "Resilience"
df_terms_freq_all$group_term[which(df_terms_freq_all$Term %in% terms_digital)] <- "Digitalisation"
df_terms_freq_all$group_term[which(df_terms_freq_all$Term %in% terms_sustainable)] <- "Sustainability"
df_terms_freq_all$group_term[which(df_terms_freq_all$Term %in% terms_sustainable)] <- "Medical"
df_terms_freq_keywords <- df_terms_freq_all[which(df_terms_freq_all$Term %in% search_terms),]


## In this example, we will look at the development of keyword groups, aggregated by summing their proportional frequency, per article group
## Convert long dataframe to wide matrix, Plot heatmap per group of articles and per group of terms
wide_dataframe <- dcast(df_terms_freq_keywords, group_doc~group_term, value.var="prop_term_to_total_terms",fun=sum) # go from long dataframe to matrix with as value proportional terms

### VISUALISATION 2 BUBBLE PLOT
## Bubble plot keywords and their occurances in groups of articles.This works best with more than two article groups or in a time-series analysis
df_terms_freq_keywords$Term <- factor(df_terms_freq_keywords$Term, levels = search_terms) # Order terms in accordance with search terms, so terms from groups are together
png("4C Bubble plot of groups of terms per document group.png",width=7,height=5,units="in",res=1200)
ggplot(df_terms_freq_keywords, aes(x=group_doc, y=Term, size = prop_term_to_total_terms,color= group_term),  group = group_doc)+
  geom_point()+
  theme(axis.text.y = element_text(face='bold'),
        axis.text.x = element_text(face="bold"),
        axis.title = element_text(face="bold",size=14))
dev.off()


### VISUALISATION 3 Line graphs for all terms per group 
## Note(groups were identified with '1-4B_Text_Mining_template_multiple_text_corpus_groups.R'

#### Plot Group1 Resilience terms
terms_selected <-  terms_resiliance
df_terms_freq_select <- df_terms_freq_all[df_terms_freq_all$Term %in% terms_selected,]
df_terms_freq_select$Term < - factor(df_terms_freq_select$Term,level=df_terms_select)
df <- df_terms_freq_select

# Basic line plot with points
png("4C_group1_resilience_terms.png",width=7,height=3,units="in",res=1200)
ggplot(data=df, aes(x=group_doc, y=prop_term_to_total_terms,group=Term)) +
  #geom_line()+
  geom_point()+
  ggtitle("term",)+ # for the main title
  ylab("relative frequency")+ # for the x axis label
  geom_smooth(method=lm,level=0.95,color='orange')+ #add linear trend line+
  #geom_smooth(method=loess,level=0.95,color='brown')+ #add linear trend line+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~Term, ncol = 4, nrow=5,scales='free_y')
dev.off()


#############################
#### Plot Group2 Digital_Automation terms
terms_selected <-  terms_digital
df_terms_freq_select <- df_terms_freq_all[df_terms_freq_all$Term %in% terms_selected,]
df_terms_freq_select$Term <- factor(as.character(df_terms_freq_select$Term),level=terms_selected)
df <- df_terms_freq_select

# Basic line plot with points
png("4C_group2_digital_automation_terms.png",width=7,height=3,units="in",res=1200)
ggplot(data=df, aes(x=group_doc, y=prop_term_to_total_terms,group=Term)) +
  #geom_line()+
  geom_point()+
  ggtitle("term",)+ # for the main title
  ylab("relative frequency")+ # for the x axis label
  geom_smooth(method=lm,level=0.95,color='blue')+ #add linear trend line+
  #geom_smooth(method=loess,level=0.95,color='red')+ #add linear trend line+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~Term, ncol = 4, nrow=5,scales='free_y')
dev.off()


#############################
#### Group 3 Plot Sustainable terms
terms_selected <-  terms_sustainable
df_terms_freq_select <- df_terms_freq_all[df_terms_freq_all$Term %in% terms_selected,]
df_terms_freq_select$Term <- factor(as.character(df_terms_freq_select$Term),level=terms_selected)
df <- df_terms_freq_select

# Basic line plot with points
png("4C_group3_sustainability_terms.png",width=7,height=3,units="in",res=1200)
ggplot(data=df, aes(x=group_doc, y=prop_term_to_total_terms,group=Term)) +
  #geom_line()+
  geom_point()+
  ggtitle("term",)+ # for the main title
  ylab("relative frequency")+ # for the x axis label
  geom_smooth(method=lm,level=0.95,color='green')+ #add linear trend line+
  #geom_smooth(method=loess,level=0.95,color='red')+ #add linear trend line+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~Term, ncol = 4, nrow=5,scales='free_y')
dev.off()

#############################
#### Group 4  Plot Medical terms
terms_selected <-  terms_medical
df_terms_freq_select <- df_terms_freq_all[df_terms_freq_all$Term %in% terms_selected,]
df_terms_freq_select$Term <- factor(as.character(df_terms_freq_select$Term),level=terms_selected)
df <- df_terms_freq_select

# Basic line plot with points
png("4C_group4_medical_terms.png",width=7,height=3,units="in",res=1200)
ggplot(data=df, aes(x=group_doc, y=prop_term_to_total_terms,group=Term)) +
  #geom_line()+
  geom_point()+
  ggtitle("term",)+ # for the main title
  ylab("relative frequency")+ # for the x axis label
  geom_smooth(method=lm,level=0.95,color='red')+ #add linear trend line+
  #geom_smooth(method=loess,level=0.95,color='red')+ #add linear trend line+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~Term, ncol = 4, nrow=5,scales='free_y')
dev.off()
