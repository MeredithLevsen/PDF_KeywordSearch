# Search Journal Article PDFs for Keywords
## This R script was implemented as part of an initial screening process for articles being considered for a meta-analysis on the relationship of insomnia and BMI. 
Script written in Fall 2016

#### The Project: 
A systematic search of Ovid MEDLINE, PubMed, Scopus, and PsychInfo on topics related to insomnia and BMI returned a total of 3,985 articles. First, we needed to screen out articles that did not contain measures of both BMI and insomnia symptoms. The initial screening strategy was to have Research Assistants (like myself) read through each abstract and identify articles that might contain both measures. One concern with this strategy is that is biased in favor of studies with significant/non-null BMI-insomnia associations - if a study does report a null association (e.g., in a correlation table), they may not mention it in their abstract (especially if those variables were not the primary focus of the study).  

As I was helping with this screening process, it occurred to me that this tedious and inefficient process could be performed automatically. Additionally, the capability of searching the full text of the articles instead of just the abstracts would in part address the bias issue. I set to work writing an R script that could search the body text of all article .pdfs for the presence of both BMI and insomnia measures.  

I thought keyword search was the ideal approach for this project because this would optomize precision as opposed to recall. Many articles mentioned other sleep disorders other than insomnia (like sleep apnea), so I was concerned that employing a different method (i.e., one that didn't search for the exact words of interest) would return the sleep apnea articles.  

This script was employed after Research Assistants screened out case studies and commentaries. Ultimately, it screened out 942 articles (out of 2,221 remaining articles with searchable .pdfs), ultimately saving Research Assistants time. This left us with far fewer articles to manually review.  

#### Script Details: 
- **Input**: a folder containing all pdfs of the journal articles that you'd like to search
- **Output**: csv files with rows for each of the pdfs and a column for which keywords they contain (one csv file for rejected files, one for accepted ones, and one for pdfs that threw errors). 

## What I would have done differently if I were working on this project today:
This was my best attempt to automatize this manual process at the time. However, I was still learning the basics of R coding at the time. I learned a lot in a short amount of time while working on this project. Nonetheless, I still would have done a few things differently if I were completing the same project again today: 
1. *Better coding practices*: I would have broken this script up into multiple parts and defined functions. 
2. *Validation*: To validate this method of selecting articles, I would have manually determined which articles contained both BMI and insomnia measures out of a subset of articles. I would have compared the hand-labeled subset to the PDF search script, and looked at the confusion matrix (and precision, recall, and F1 score) to see how this script compared to the manual method. 
3. *More advanced NLP techniques*: I also would have considered other approaches, such as TF-IDF, instead of just a basic keyword search. However, at the time that I wrote these scripts, I was unaware that such methods existed. 
