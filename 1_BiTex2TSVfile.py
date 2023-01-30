'''
1) A simple Python script to load BibTex references in a dataframe and export
to a Tab Seperate Volume file (.tsv)
'''


#pip install pybtex
from pybtex.database.input import bibtex
import unicodedata
import pandas as pd
import os	  
parser = bibtex.Parser()

## Directory/folder containing all the downloaded paper reference including
## abstract information as file.bib
directory = 'BibTex/'

## Innitiate llist of dictionaries per article to be later converted to df
list_bibtex_dict = []
excluded = [] # list of abstracts that failed to loadd


for file_name in os.listdir(directory):
	if file_name.endswith(".bib"):
		try:
			bib_data = parser.parse_file(directory+file_name)
		except:
			print('skipped file',file_name)
			excluded.append(file_name)
			continue

		## Loop over all bib entries in the database
		for entry in bib_data.entries.keys():
			bib_dict = {}
			for field in bib_data.entries[entry].fields.keys():
				try:	 
					field_content = bib_data.entries[entry].fields[field]
					field_clean = unicodedata.normalize('NFKD', field).encode('ascii', 'ignore') # Cleaned of tabs, to avoid problems exporting to TSV
					field_content_clean = unicodedata.normalize('NFKD', field_content).encode('ascii', 'ignore') # Cleaned of tabs, to avoid problems exporting to TSV
					bib_dict[field_clean] = field_content_clean
				except:
					print('excluded',field,field_content,entry)
					pass
			if bool(bib_dict): # checks if not empty
				list_bibtex_dict.append(bib_dict)
			bib_dict = {}	
			

## Load list of dictionaries into a dataframe, drop duplicates based on doi
df = pd.DataFrame.from_records(list_bibtex_dict)
df.sort_values(by='doi',inplace=True)
df.drop_duplicates(subset=['doi'],inplace=True)

## Print summary of loading results
print('Successfully Loaded {} BibTex references out of {}, excluded {}.'.format(df.shape[0],
	  df.shape[0]+len(excluded),len(excluded)))

## Export the pandas dataframe as .tsv file
df.to_csv( 'doi_abstract.tsv',sep='\t')

## Write excluded files because they failed to load as proper bibtex to log
with open("log_excluded.txt", "w") as output:
    output.write(str(excluded))	
