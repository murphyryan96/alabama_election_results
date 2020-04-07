# Prepare the data set for the results of the Alabama state-wide elections
# Created by Ryan Murphy, 04-04-2020 | rjmurphy5@crimson.ua.edu

# Test for missing pacakges
list.of.packages <- c("pdftools", "stringr", "svDialogs")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Import libraries
library(pdftools)
library(stringr)
library(svDialogs)
options(stringsAsFactors = FALSE)

# Load in the file
dlg_message("Please select the election results PDF file.",
                       type = "ok")
file <- file.choose(new = FALSE)
new_file_name <- str_split(file, "/")
new_file_name <- str_split(new_file_name[[1]][length(new_file_name[[1]])], "\\.")[[1]][1]
pdf_document <- lapply(file, pdf_text)

# How many pages does the document have?
num_pages <- length(pdf_document[[1]])

# Main Function - Builds out one table per page
create_results_table <- function(doc, page_num) {
    
    # Locate the current page in the given document
    current_page <- doc[page_num]
    #current_page <- pdf_document[[1]][2]
    
    
    # Step 1. Create an empty data frame
    results_table <- data.frame("County", "District", "Candidate", "Party", 0)
    colnames(results_table) <- c("County", "District", "Candidate", "Party", "Votes")
    
    
    # Step 2. Make a table that matches candidates to their districts
    districts <- str_extract_all(current_page, "District [0-9]+")
    
    # Step 2.A Include district number for later sorting
    district_nums <- str_extract_all(districts, "[0-9]+")
    
    names <- paste(str_extract_all(current_page, "\n\\s*.*\\(.*\\)"), "Write-In")
    names <- str_extract(names, "\\S.*")
    names <- gsub("Write‐In", "Write-In (W)", names)
    names <- gsub("Write In", "Write-In (W)", names)
    
    Candidate <- trimws(unlist(str_split(names, "\\(\\S\\)")))
    Party <- c(str_extract_all(names, "\\S*\\(\\S\\)")[[1]], "(W)")
    cpd <- data.frame(Candidate, Party) # candidate & party & district
    
    iter <- 1
    for (i in 1:nrow(cpd)) {
        cpd$District[i] <- districts[[1]][iter]
        cpd$DistrictNum[i] <- as.numeric(district_nums[[1]][iter])
        if(substr(cpd$Party[i], 2, 2) == "W") {
            iter <- iter + 1
        }
    } # end for loop
    
    
    # Step 3. Build out a tabular representation of the document page
    tabular <- str_split(current_page, "\n")[[1]]
    tabular <- tabular[3:(length(tabular)-3)]       # get rid of headers and footers
    tabular <- str_replace_all(tabular, "‐", "0")   # change dashes ‐ to zeros
    
    
    # Step 4. Find the line with the list of districts
    district_line <- str_split(current_page, "\n")[[1]][3]
    district_col_boundaries <- str_locate_all(district_line, "D")[[1]] # find that first letter "D" to show where the text begins
    
    
    # Step. 5 Cut out all of the lines with counties that don't have votes in these districts
    ends_in_num <- !sapply(as.numeric(substr(tabular, nchar(tabular), nchar(tabular))), FUN = is.na)
    current_counties <- tabular[ends_in_num]
    current_counties <- str_replace_all(current_counties, ",", "")                          # eliminate thousands-place separators
    current_counties <- current_counties[!substr(current_counties, 2, 6) == "Total"] # eliminate the total row
    current_counties_names <- str_extract(current_counties[2:length(current_counties)], "[:alpha:]*\\S")
    
    # Step 6. Create vectors of the vote counts and match them to their county names
    vote_counts <- str_extract_all(current_counties[2:(length(current_counties))], "\\d+\\d*")
    names(vote_counts) <- current_counties_names
    
    
    # Step 7. Determine, based on column position, which district each vote count belongs to
    vote_count_positions <- str_locate_all(current_counties[2:length(current_counties)], "\\d+\\d*")
    votes_vs_districts <- c() # empty vector to match vote counts with corresponding districts
    for (x in 1:length(vote_count_positions)) {
        for(y in 1:nrow(vote_count_positions[[x]])) {
            # which_district asks "is it true that it's past the start of the text position
            # for a given district?" e.g. if it's under district 2, it'll be true for #1 & #2, but not for #3
            # so we want to find the maximum value for which it is true
            which_district <- districts[[1]][max(which(vote_count_positions[[x]][y,1] >= district_col_boundaries[,1]))]
            votes_vs_districts <- c(votes_vs_districts, which_district)
        }
    }
    
    
    # Step 8. Make a data frame which lines up the vote counts and the counties with the proper districts
    cvd <- data.frame(t(rbind(unlist(vote_counts), votes_vs_districts))) # counties/votes/districts
    cvd$County <- str_remove_all(rownames(cvd), "\\d")
    rownames(cvd) <- c()
    colnames(cvd) <- c("Votes", "District", "County")
    cvd <- cvd[ , c(3,2,1)]
    
    
    # Step 9. Add "Align" columns to the cpd and cvd data.frames to join on
    # For the cpd data.frame, the Align index restarts whenever the district goes up
    cpd_index <- 1
    for (i in 1:nrow(cpd)) {
        if (!is.na(cpd$District[i+1])) {
            cpd$align[i] <- cpd_index
            if (cpd$District[i] == cpd$District[i+1]) {
                cpd_index <- cpd_index + 1
            } else {
                cpd_index <- 1 # reset
            }
        } else {
            # last row
            cpd$align[i] <- cpd_index
        }
    } # end for loop
    
    # For the cvd data.frame, the Align index restarts for every new county-district combination
    cvd_index <- 1
    for (i in 1:nrow(cvd)) {
        if (!is.na(cvd$District[i+1])) {
            cvd$align[i] <- cvd_index
            
            if (paste(cvd$District[i], cvd$County[i]) == paste(cvd$District[i+1], cvd$County[i+1])) {
                cvd_index <- cvd_index + 1
            } else {
                cvd_index <- 1 # reset
            }
        } else {
            # last row
            cvd$align[i] <- cvd_index
        }
    } # end for loop
    
    
    # Step 10. Merge the two tables together and sort them in order
    temp_output <- merge(cvd, cpd, by = c("District", "align"))
    results_table <- temp_output[order(temp_output$County, temp_output$DistrictNum), c(3, 1, 7, 5, 6, 4)]
    return(results_table)
}

# Loop over Main Function
df <- data.frame("County", "District", "Candidate", "Party", 0)
colnames(df) <- c("County", "District", "Candidate", "Party", "Votes")
for (i in 1:num_pages) {
    if (i == 1){
        df <- rbind(create_results_table(pdf_document[[1]], i)) 
    } else {
        df <- rbind(df, create_results_table(pdf_document[[1]], i))
    }
}

df <- df[order(df$DistrictNum, df$County), ]
dlg_message("Please select where you would like to save the output file and give it a name.", type = "ok")
output_file_path <- file.choose(new = TRUE)
write.csv(df, file = paste0(output_file_path, ".csv"), row.names = FALSE)
dlg_message("Okay!  You're all set.", type = "ok")