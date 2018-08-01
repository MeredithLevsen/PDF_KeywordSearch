# First tell it where the pdf files are located: 
setwd("/Users/meredithjohnson/Box Sync/My Stuff/Sleep Lab/PDF Search R Code/All PDF Files") #from my laptop
# from Sleep Lab computer: 
#setwd("C:/Users/mpjvhd/Desktop/MizZzou Sleep Lab/PDF Search R Code/2012_Unsure Articles") #Local directory
#setwd("C:/Users/chanw/Desktop/PDFs") #from Dr. Chan's login
# From student work room computer at Thompson Center:
setwd("C:/Users/mpjvhd/Desktop/All PDF Files")

#Trouble-shooting: 
#setwd("C:/Users/chanw/Desktop/Articles_Errors")
# Directory on Server: 
#setwd("//gprs.ifs2.rnet.missouri.edu/SHP_GPRS_Groups/McCraeC/Lab Member Individual Folders/Chan/Insomnia and Obesity Review/Working files/Meredith_2012/2012_Unsure Articles")

#### Keywords Seach List: ####
# Note: Make sure words are listed in lowercase letters, 
# and abbreviations are all UPPERCASE letters. 
kwlist = c("BMI", "body mass index", "body mass", "height weight", "obesity",
           "insomnia", 
           "difficulty falling asleep", "falling asleep", "initiating sleep",
           "difficulty maintaining sleep", "maintaining sleep",
           "early waking", "night awakening", "night waking",
           "pittsburgh sleep quality index", "PSQI", 
           "insomnia severity index", "ISI",  
           "jenkins sleep questionnaire", "JSQ")


# load necessary packages to get the job done:
require(tm)
require(pdftools)
require(stringr)
require(dplyr)

# Next create a vector of PDF file names using the list.files function. 
# The pattern argument says to only grab those files ending with .pdf
# that are located in the working directory (specified above):
# Make sure there are only PDF files in this folder..
# Also, File Names should avoid "-" because sometimes it will throw errors.
# Also the file must end in ".pdf" - NOT ".PDF" - otherwise the script will skip it.
pdflist <- list.files(pattern = "pdf$")

# Establish the existence of the data frame:
RefSum = c()


#### PDF Loop-De-Loop #### 
# Loop through the pdf and keyword lists and add all of the variables to a data.frame. 
## There will be 2 variables per keyword: in the data.frame produced by this loop:  
# 1. "_count": Count of the number of total times keyword is mentioned, and 
# 2. "_SplitPgs": How many pages on which all keyphrase words appear (not necessarily consecutively).


for (i in 1: length(pdflist)) {

  #### Start with some basics about the article first: 
  file.name <- pdflist[i] # Variable with file name
  article <- pdf_text(file.name) # Read pdf to text using pdftools package.
  file.info <- pdf_info(file.name) # Assign the file info to a variable.
  article.lc <- tolower(article) # Convert article to all lower case letters.
  
  #### Deal with "no layout" articles: 
  file.info$keys$Title[file.info$layout=="no_layout"]=NA
  file.info$keys$`WPS-ARTICLEDOI`[file.info$layout=="no_layout"]=NA
  
  print(file.name) #Verbose output so that you know R is doing something. Also if it throws an error, 
      # this makes it easier to figure out how far the script got into the pdf folder.

  #### Start building new line of dataframe and record some variables:
  temp.df <- data.frame("Title"=file.info$keys$Title, 
                        "DOI"=file.info$keys$`WPS-ARTICLEDOI`,
                        "FileName"=file.name)
  
  ##### Loop through keywords/keyphrases list for each article: 
  for (k in 1: length(kwlist)) {
    
    #### For abbreviations, search regular version of article. For regular (lowercase) words/phrases, 
      ## search the version of article that has been converted to all lowercase letters (i.e., "article.lc").
    article.version = 0
    ifelse (test = grepl("^[[:upper:]]+$", kwlist[1]), # Checks to see if current KW is all uppercase (1st and last letter of KW)
            article.version <- article,  #If KW is uppercase, it sets article.version to the regular case format.
            article.version <- article.lc)  #If KW is not all uppercase, it sets article.version to lowercase format.
    
    #### Count total number of KW occurences:
    temp.count <- sum(str_count(article.version, kwlist[k])) # Counts # of times KW mentioned in article
    temp.kwvar <- str_replace_all(kwlist[k], pattern = " ", replacement = "_") # Replaces spaces in keyphrase with "_"
    temp.df[,paste0(temp.kwvar, "_Count")] <- temp.count  #  new count variable for this kw added to temp dataframe.
    
    
    #### Count number of pages that contain all words within KeyPhrase
        ##(to account for phrases being separated by line breaks):
    kp.split <- 
      str_split(kwlist[k], boundary(type = "word")) %>%  # Splits phrase into separate word items.
        unlist(.) # Makes variable not in list format. Because lists are kind of weird.
                  # Note: the %>% passes the previous argument to the next function. 
    # Now search for pages that contain each of the words in the phrase (separately):
    # Make a little data frame that contains a row for each word with columns representing each page.
    temp.pgNums = c()
    for (w in 1: length(kp.split)) {
      temp.split <- str_detect(article.version, kp.split[w]) # Detects whether each word appears on each page. 
      temp.pgNums <- rbind(temp.pgNums, temp.split) # Adds a row for each word to the mini data frame. 
    }
    #temp.pgNums <- temp.pgNums[-1,] #delete 0s in first row
    #Above code makes a dataframe "temp.pgNums" with a row for each word in the keyphrase that contains
        # the pages in which each word occurs. Variables represent each page. 1 = word was present
        # on that page, 0 = it was not.
    ### Now in the next steps, we're going to add up all of the pages on which all words in the keyphrase appear: 
        # (first we have to treat 1-word keyphrases differently from those with >1 word)
    if(nrow(temp.pgNums) > 1) {
          temp.splitPgs <-          # For Key Phrases with more than one word:
            colSums(temp.pgNums) %>% # Sums all of the columns
            str_count(., pattern = as.character(length(kp.split))) %>% # returns 1 for each column (page) 
                                                    # that contains all of the words in the keyphrase.
            sum(.)  # Gives the total number of pages that contain all keyphrase words 
    } else {
          # For Key Phrases with only one word/string:
          temp.splitPgs <- sum(temp.pgNums) #Sum up the number of pages that contain the Keyphrase word.
          }
    
    ### Now add the SplitPgs Variable to the temp data frame:
    temp.df[,paste0(temp.kwvar, "_SplitPgs")] <- temp.splitPgs  #  new count variable for this kw added to temp dataframe.
    
    ### Keyword Loop housecleaning:
    temp.count = 0
    temp.kwvar = 0
    temp.split = 0
    temp.splitPgs = 0
    temp.pgNums = 0
    w = 0
    kp.split = 0
    
    }
  
  #### Add temp data.frame to the bigger data.frame. 
  RefSum = rbind(RefSum, temp.df) 
  
  #### Housecleaning: Clear everything before starting on the next article in the folder.
  file.info$keys$Title=0 
  file.info$keys$`WPS-ARTICLEDOI`=0
  file.name=0
  temp.df = 0

}
# The end of the loop!

# Note: This script throws a bunch of errors for some of the articles (e.g., "error: Invalid Font Weight", etc.),
# but this doesn't seem to actually create an issue in terms of running the loop to completion.
# Also those articles seem to have been read into R just fine. It doesn't hinder our ability to obtain 
# the info we want from the pdf. It's still readable. 

# Leave the pdf file folder: (or not...)
#setwd("C:/Users/chanw/Desktop/PDF Search R Code") #Local directory
#If you don't leave, then the output will be stored in the same file where the PDFs are located. 


###### Make Summary Columns ########

# Here is the new list of search terms:
#   "Body Mass Index" OR 
#     "BMI" OR 
#     "Body Mass"
# AND
#   "insomnia" OR 
#     "difficulty falling asleep" OR 
#     "difficulty maintaining sleep" OR 
#         "falling asleep" OR 
#         "initiating sleep" OR
#         "maintaining sleep"
#     "early waking" OR 
#     "night awakening" OR 
#     "night waking" OR 
#     "Pittsburgh Sleep Quality Index" OR 
#        "PSQI" OR 
#     "Insomnia Severity Index" OR 
#         "ISI" OR 
#     "Jenkins Sleep Questionnaire" OR 
#         "JSQ" 
# #

# First do some housecleaning: 
RefSum = RefSum[,-1:-2] # get rid of title and doi. Those didn't really work out how I thought they might.

# Export as is. Without Summary Variables. #
write.csv(RefSum, "PDFSearchRScript_PrescreenedList_Round2_Jan2017.csv", row.names = FALSE)

#### Make summary Variables ####
# Make term lists. 
bmiList = c("BMI", "body mass index", "body mass", "height weight", "obesity")
insomList = c("insomnia", 
            "difficulty falling asleep", "falling asleep", "initiating sleep",
            "difficulty maintaining sleep", "maintaining sleep",
            "early waking", "night awakening", "night waking",
            "pittsburgh sleep quality index", "PSQI", 
            "insomnia severity index", "ISI",  
            "jenkins sleep questionnaire", "JSQ")

# make a loop that creates a list of all the variables names with bmi search terms. 
bmiList.vars = c()
for (b in bmiList) {
  temp.vars = grep(str_replace_all(b, " ", "_"), colnames(RefSum), value = TRUE)
  bmiList.vars = c(bmiList.vars, temp.vars)
}

RefSum$AnyBMI = apply(RefSum[,c(bmiList.vars)], 1, FUN = sum) #adds up values from the BMI columns for each row
RefSum$AnyBMI_Count = apply(RefSum[,c(grep("Count", bmiList.vars, value = TRUE))], 1, FUN = sum) # Adds up all the count BMI columns

# make a loop that creates a list of all the variables names with insomnia search terms. 
insomList.vars = c()
for (i in insomList) {
  temp.vars = grep(str_replace_all(i, " ", "_"), colnames(RefSum), value = TRUE)
  insomList.vars = c(insomList.vars, temp.vars)
}

RefSum$AnyInsomnia = apply(RefSum[,c(insomList.vars)], 1, FUN = sum) # adds up values from insomnia columns for each row
RefSum$AnyInsomnia_Count = apply(RefSum[,c(insomList.vars)], 1, FUN = sum) #Adds up insomnia count columns

# Now make new summary columns: 
  # BMI: 
# RefSum$AnyBMI = apply(RefSum[,2:9], 1, FUN = sum) #adds up values from the BMI columns (#2-9) for each row
# RefSum$AnyBMI_Count = apply(RefSum[,c(2,4,6,8)], 1, FUN = sum) # Adds up all the count BMI columns
#   # Insomnia
# RefSum$AnyInsomnia = apply(RefSum[,10:39], 1, FUN = sum) # adds up values from insomnia columns for each row
# RefSum$AnyInsomnia_Count = apply(RefSum[,seq(from = 10, to = 39, by = 2)], 1, FUN = sum) #Adds up insomnia count columns

# Make a column that identifies articles that have BOTH insomnia AND BMI. 1=both, 0=not both.
RefSum$BothInsomBMI=0
RefSum$BothInsomBMI[(RefSum$AnyBMI > 0) & (RefSum$AnyInsomnia > 0)]=1

#### Shorter List Summaries ####
bmiList = c("BMI", "body mass index", "body mass", "obesity")
insomList = c("insomnia")

# BMI: make a loop that creates a list of all the variables names with bmi search terms. 
bmiList.vars = c()
for (b in bmiList) {
  temp.vars = grep(str_replace_all(b, " ", "_"), colnames(RefSum), value = TRUE)
  bmiList.vars = c(bmiList.vars, temp.vars)
}
RefSum$AnyBMIObesity_4kw = apply(RefSum[,c(bmiList.vars)], 1, FUN = sum) #adds up values from the BMI columns for each row
#RefSum$AnyBMIObesity_4kw_Count = apply(RefSum[,c(grep("Count", bmiList.vars, value = TRUE))], 1, FUN = sum) # Adds up all the count BMI columns

# Insomnia: make a loop that creates a list of all the variables names with insomnia search terms. 
insomList.vars = c()
for (i in insomList) {
  temp.vars = grep(str_replace_all(i, " ", "_"), colnames(RefSum), value = TRUE)
  insomList.vars = c(insomList.vars, temp.vars)
}
RefSum$AnyInsomnia_1kw = apply(RefSum[,c(insomList.vars)], 1, FUN = sum) # adds up values from insomnia columns for each row
#RefSum$AnyInsomnia_Count = apply(RefSum[,c(insomList.vars)], 1, FUN = sum) #Adds up insomnia count columns

# Make a column that identifies articles that have BOTH insomnia AND BMI. 1=both, 0=not both.
RefSum$BothInsomBMI_5kw=0
RefSum$BothInsomBMI_5kw[(RefSum$AnyBMIObesity_4kw > 0) & (RefSum$AnyInsomnia_1kw > 0)]=1

#### Reorganize and export ####

# Rearrange the columns so the summary ones are in front (i.e., right afer filename)
RefSum.full = data.frame(RefSum[,c(1, grep("Both", colnames(RefSum)), grep("Any", colnames(RefSum)), 2:41)]) 

# Make a dataframe with just the rejected articles that don't have both: 
RefSum.0Both <- RefSum.full[RefSum.full$BothInsomBMI == 0,] #659 papers rejected
RefSum.0Both_shortlist <- RefSum.full[RefSum$BothInsomBMI_5kw == 0,] # 953 papers rejected

#### Export dataframes ####
# Export the merged dataframe to a tab-delimited txt file. 
write.csv(RefSum.full, file = "PDFSearchRScript_PreScreenedList_Round2_SummaryVariables.csv", row.names = FALSE)

# write.table(RefSum.full, file = "PDFSearchRScript_PreScreenedList.txt", sep = "\t", row.names = FALSE)
# write.table(RefSum.0Both, file = "PDFSearchRScript_PreScreenedList_RejectedArticles.txt", sep = "\t", row.names = FALSE)


#### Solving Mysteries: The Case of the Missing Articles:####
# Note: There are 2,241 articles in the folder. However, there are only 2,224 articles in the dataframe.
# 17 articles missing?? 

# It seems that there are non-pdf files in the folder.. 

# Here's how to make a list of files that are missing: 
allfiles <- list.files(pattern = "") # make a list of all files in the folder

art.noErrors <- as.data.frame(RefSum.full$FileName) # make a dataframe with the only the filename column from (pdfs dataframe)
art.folder <- as.data.frame(allfiles) # make a dataframe with the list of all files in the folder
colnames(art.noErrors) <- "FileName" # Make column names in both dataframes match
colnames(art.folder) <- "FileName"
art.noErrors[,1] <- as.character(art.noErrors[,1]) #make the columns characters, not factors.
art.folder[,1] <- as.character(art.folder[,1])

# Now make your list of items that appear in art.folder, but NOT in art.noErrors.
art.missing <- setdiff(art.folder, art.noErrors)
art.missing$FileName <- str_to_lower(art.missing$FileName) # make all the filenames all lowercase
art.missing$pdf[str_count(art.missing$FileName, ".pdf") == 1] = 1 #create extra column that checks for any pdfs left in folder
# If there are pdfs still missing, you might have to manually fix them (like if the extension is ".PDF" instead of ".pdf"). 
# Not going to do anything about the webarchives right now. 

# Export list of missing files: 
write.table(art.missing$FileName, file = "PDFSearchRScript_PreScreenedList_Missing Articles.txt", sep = "\t", row.names = FALSE)

#### To Do ####
# - Incorporate a way of noting if the keywords are mentioned near the Refs section...
# - better pdf to text method. (i.e., that preserves layout)

