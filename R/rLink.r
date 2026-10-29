# 02-11-2016

## R LINKAGE
# Performs deduplication on one dataset or linkage on two datasets. 
# Allows for multiple blocking strategies. 
# Includes Soundex, Jaro-Winkler and Damerau-Levenshtein Edit Distance.
# Allows comparisons that split strings using regular expressions.


## CURRENT LIMITATIONS
# Two datasets
# RAM

## IMPROVEMENT LIST
# More than 2 datasets - Not a path I will take this program down
# NLS probabilities
# Likelihood of match based on frequency
# BigMatch Jaro-Winkler variant
# Memory management -> written in another file, but it's slow and SQL is gross :( Not a path I will take this program down
# Alternatives to soundex - None seem to be any better
# Fellegi-Sunter
# Weighting the weights
# Whitespace permutations (SORT OF ADDED)
# Firstname/Surname flip (SORT OF ADDED)
# Intelligent handling of hyphens (ADDED)
# split, sort strings, reassemble
# Error reports (ADDED)


# Change/Update log
# All changes/updates go in here
# Of the same format as the first element in the list
updates <- list(
	 "2016-04-27" = c("Example 1."
					, "Example 2.")
	,"2016-04-29" = c("Fixed a bug which caused the unique values to overwrite missing values on the output when multiple blocks were used and missingPos was TRUE.")
	,"2016-06-06" = c("Introduced a changelog that only alerts users of new changes made to rLink since they last used it.")
	,"2016-06-24" = c("rLink will now identify variable names, as specified in parameters, that are not on the dataset. This is useful for correcting typos.")
	,"2016-07-14" = c("Performing a split comparison will return the value of the full comparison if it is larger. This is so names like \"VAN DER BEEK\" will match well with \"VANDERBEEK\"."
					, "When performing linkage or deduplication with multiple blocks, rLink will now report the expected pairs for each block before blocking rather than the expected total number of pairs. This should allow users to diagnose which blocks are problematic."
					, "The groupID function no longer fails to execute if you already have a variable named \"groupID\" on your input data file.")
	,"2016-07-27" = c("Data imported from csv is now read in completely verbatim. Previously rLink assessed whether variables should be treated as character, numeric or integer, which impacted on some formatting.")
	,"2016-10-11" = c("Fixed a bug which allowed record pairs to appear multiple times on the output where they matched under multiple blocking strategies when doing deduplication with identical blocking strategies.")
	,"2017-01-31" = c("The groupID function now includes an option to identify groups using ID information within and across all pairs. This is relevant for deduplication tasks such as identifying where we will call a single household multiple times (for example).")
)

# Import rLink user history
if (exists("SAS")) {
	if (SAS) {
		rLinkUserHistory <- read.csv("E:\\SAS\\Surveys\\R\\Programs\\R\\R Linkage\\rLinkUserHistory.txt", header=T)
	}
} else {
	rLinkUserHistory <- read.csv("S:\\Surveys\\R\\Programs\\R\\R Linkage\\rLinkUserHistory.txt", header=T)
}

# Identify when the current user last used rLink if at all
last <- as.Date(rLinkUserHistory[rLinkUserHistory$login==Sys.info()["login"], "lastUsed"])

# Determine whether the current user has used rLink before
if (length(last)>0) { 
	if (!is.na(last)) {
		# If the current user has used rLink before
		# Determine whether there are any new updates since the current user last used rLink
		if (any(as.Date(names(updates)) > last)) {
			# If there are any new updates since the current user last used rLink
			# Identify the new updates since the current user last used rLink
			newUpdates <- updates[as.Date(names(updates)) > last]
			
			# Manipulate the updates into a format desired for printing to the screen
			string <- ""
			for (i in 1:length(newUpdates)) {
				string <- paste0(string
								,names(newUpdates[i]),":\n"
								,paste(" - ",unlist(newUpdates[i]), collapse="\n"),"\n\n")
			}
			
			# Print the updates to the screen
			require(tcltk2)
			win3  <- tktoplevel()
			tktitle(win3) <- "Changes to rLink since last use"
			# Scrollbar must be defined first
			win3$env$scr <- tk2scrollbar(win3, orient = "vertical", command = function(...) tkyview(win3$env$txt, ...))
			win3$env$txt <- tk2text(win3, bg = "white", font = "Arial", width = 80, height = 10, wrap="word", yscrollcommand = function(...) tkset(win3$env$scr, ...))
			# Use grid manager, telling to occupy the whole area
			tkgrid(win3$env$txt, win3$env$scr, sticky = "nsew")
			# Indicate that win3$env$txt must spread in x and y on window resize
			tkgrid.rowconfigure(win3, win3$env$txt, weight = 1)
			tkgrid.columnconfigure(win3, win3$env$txt, weight = 1)
			# Populate the text area with many lines
			tkinsert(win3$env$txt, "end", string)
			tkconfigure(win3$env$txt, state = "disabled")
			tkfocus(win3$env$txt)
		}
		
		# Update the rLink user history
		rLinkUserHistory[rLinkUserHistory$login==Sys.info()["login"], "lastUsed"] <- as.character(Sys.Date())
	}
} else {
	# If the current user has not used rLink before
	# Update the rLink user history
	rLinkUserHistory <- rbind(rLinkUserHistory,data.frame(login=Sys.info()["login"],lastUsed=as.character(Sys.Date())))
}

# Write out the updated rLink user history
if (exists("SAS")) {
	if (SAS) {
		write.csv(rLinkUserHistory, "E:\\SAS\\Surveys\\R\\Programs\\R\\R Linkage\\rLinkUserHistory.txt", row.names=F)
	}
} else {
	write.csv(rLinkUserHistory, "S:\\Surveys\\R\\Programs\\R\\R Linkage\\rLinkUserHistory.txt", row.names=F)
}



## INPUT
# data1 = First dataset. Can be csv file location or already imported data.frame object. No default, input required.
# data2 = Optional second dataset. Can be csv file location or already imported data.frame object or empty (FALSE). Default is FALSE.
# blockStrategy1 = Vector (or list of vectors) of blocking variables from data1, listed in the same order as blockStrategy2. Can be character (variable name) or numeric (column number) or a list of these. Default is empty.
# blockStrategy2 = Vector (or list of vectors) of blocking variables from data2, listed in the same order as blockStrategy1. Can be character (variable name) or numeric (column number) or a list of these or empty. If empty blockStrategy2=blockStrategy1. Default is empty.
# soundexPos = Vector (or list of vectors) containing the position of the soundex variables in the blocking vector. Can be character (variable name) or numeric (vector index). Default is empty.
# missingPos = Boolean or vector (or list of vectors) containing the position of the variables in the blocking vector to be made unique. Can be boolean (TRUE or FALSE), character (variable name) or numeric (vector index). Default is FALSE.
# winkler1 = Vector of variables to be compared using Jaro-Winkler from data1, listed in the same order as winkler2. Must be character (variable name). Default is empty.
# winkler2 = Vector of variables to be compared using Jaro-Winkler from data2, listed in the same order as winkler1. Must be character (variable name) or empty. If empty winkler2=winkler1. Default is empty.
# winklerSplitVec1 = Vector of regular expressions used to split associated winkler1 variables. Specifying regular expressions will cause the function to compare each part of the winkler1 variable and return the maximum score. Default is empty.
# winklerSplitVec2 = Vector of regular expressions used to split associated winkler2 variables. Specifying regular expressions will cause the function to compare each part of the winkler2 variable and return the maximum score. Default is empty.
# damLev1 = Vector of variables to be compared using Damerau-Levenshtein Edit Distance from data1, listed in the same order as damLev2. Must be character (variable name). Default is empty.
# damLev2 = Vector of variables to be compared using Damerau-Levenshtein Edit Distance from data2, listed in the same order as damLev1. Must be character (variable name) or empty. If empty damLev2=damLev1. Default is empty.
# damLevSplitVec1 = Vector of regular expressions used to split associated damLev1 variables. Specifying regular expressions will cause the function to compare each part of the damLev1 variable and return the maximum score. Default is empty.
# damLevSplitVec2 = Vector of regular expressions used to split associated damLev2 variables. Specifying regular expressions will cause the function to compare each part of the damLev2 variable and return the maximum score. Default is empty.
# output = Optional destination for outputting results to csv. Must be a string for it to attempt to write. Must use "/" in place of "\". Default is FALSE.
# efficient = A toggle for taking extra steps to more efficiently manage memory by retaining only the final dataset. Must be boolean (TRUE or FALSE). Default is TRUE.
# forceUpperCase = A toggle for forcing blocking and comparison variables on both datasets to be upper case. Must be boolean (TRUE or FALSE). Default is TRUE.
# addPrefix = A toggle for adding a prefix to each variable name indicating which dataset the variable came from. Must be boolean (TRUE or FALSE). Default is TRUE.
# diagnostics = Allows you to specify how verbose you want the function to be. Can take the value 0 or 'quiet' for no print out, 1 or 'default' for a print of the first and last 5 rows of input, or 2 or 'verbose' for a full diagnostic of the import stage as well. Default is 'default'.

## OUTPUT
# blockPairs = Dataset containing:
#			   - All pairs identified through blocking; 
#			   - All variables from data1 and data2;
#			   - All Soundex, Jaro-Winkler and Damerau-Levenshtein Edit Distance results; and
#			   - A group ID similar to descriptions of the one in NLS



link <- function(data1, data2=FALSE, blockStrategy1=c(), blockStrategy2=c(), soundexPos=c(), missingPos=FALSE, winkler1=c(), winkler2=c(), winklerSplitVec1=c(), winklerSplitVec2=c(), damLev1=c(), damLev2=c(), damLevSplitVec1=c(), damLevSplitVec2=c(), output=FALSE, efficient=TRUE, forceUpperCase=TRUE, addPrefix=TRUE, diagnostics="default") {
	
	dummyBlock=blockKey=rown=pair=rown.data1=rown.data2=rowmin=rowmax=pairID=.=damLev=i.rown=blockNo=NULL
	
	# By default turn deduplication mode off
	dedup <- FALSE
	# By default assume one blocking strategy
	multB <- FALSE
	# Check for where blockPairs already exists and remove it ### THIS MAY NOT WORK
	if (exists("blockPairs", envir=currentEnvir)) rm(blockPairs, envir=currentEnvir)
	
	### DATA
	# Import/Assign the datasets
	# If data is a string assume that it is a file path and import using read.csv
	if (class(data1)=="character") {
		if (diagnostics %in% c(2,"verbose")) fd1 <- fread(data1, stringsAsFactors=F, verbose=T, colClasses="character")
		else fd1 <- fread(data1, stringsAsFactors=F, colClasses="character")
		if (!(diagnostics %in% c(0,"quiet"))) {
			cat("\ndata1\n")
			print(fd1)
		}
	# If data is a data frame just relabel it
	} else if ("data.frame" %in% class(data1)) {  
		fd1<-data.table(data1)
		if (!(diagnostics %in% c(0,"quiet"))) {
			cat("\ndata1\n")
			print(fd1)
		}
		# # # If efficiency is a concern then remove the original data
		# # if (efficient) rm(data1)
	# If neither of the above then print error
	} else stop("data1 missing/unexpected format")
	# If data is a string assume that it is a file path and import using read.csv
	if (class(data2)=="character") {
		if (diagnostics %in% c(2,"verbose")) fd2 <- fread(data2, stringsAsFactors=F, verbose=T, colClasses="character")
		else fd2 <- fread(data2, stringsAsFactors=F, colClasses="character")
		if (!(diagnostics %in% c(0,"quiet"))) {
			cat("\ndata2\n")
			print(fd2)
		}
	# If data is a data frame just relabel it
	} else if ("data.frame" %in% class(data2)) {  
		fd2<-data.table(data2)
		if (!(diagnostics %in% c(0,"quiet"))) {
			cat("\ndata2\n")
			print(fd2)
		}
		# # # If efficiency is a concern then remove the original data
		# # if (efficient) rm(data2)
	# If input is FALSE then convert to deduplication mode
	} else if (class(data2)=="logical") {
		dedup <- TRUE
	# If neither of the above then print error
	} else stop("data2 unexpected format")
	
	
	
	### INDEX
	# Blocking
	# Detect whether input is of the correct format
	# If the input is a list then we know we are doing multiple blocking
	if (class(blockStrategy1)=="list") {
		multB <- TRUE
		# If the input is not character or numeric, print error
		if (sum(!(sapply(blockStrategy1,class) %in% c("character","numeric")))>0) stop("blockStrategy1 unexpected format")
		# If the input is character, take it as is
		block1 <- blockStrategy1
		# If the input is numeric, assume that it is the column position and convert it to variable name
		if (sum(sapply(blockStrategy1,class)==c("numeric")) > 0) block1[sapply(blockStrategy1,class)=="numeric"] <- lapply(blockStrategy1[sapply(blockStrategy1,class)=="numeric"], function(x) names(fd1)[x])
	# If the input is just a vector
	# If the input is character, take it as is
	} else if (class(blockStrategy1)=="character") {
		block1 <- blockStrategy1
	# If the input is numeric, assume that it is the column position and convert it to variable name
	} else if (class(blockStrategy1)=="numeric") {
		block1 <- names(fd1)[blockStrategy1]
	# Sometimes we want to pair everything together in the same block
	} else if (class(blockStrategy1)=="NULL") {
		block1 <- "dummyBlock"
		fd1[,dummyBlock:="1"]
	# If not character then print error
	} else {
		stop("blockStrategy1 unexpected format")
	}
	# If there is a second dataset then we are doing linkage and not deduplication
	if (!dedup) {
		# If the input is a list then we know we are doing multiple blocking
		if (class(blockStrategy2)=="list" & multB) {
			# If the input is not character or numeric, print error
			if (sum(!(sapply(blockStrategy2,class) %in% c("character","numeric")))>0) stop("blockStrategy2 unexpected format")
			# If the input is character, take it as is
			block2 <- blockStrategy2
			# If the input is numeric, assume that it is the column position and convert it to variable name
			if (sum(sapply(blockStrategy2,class)==c("numeric"))>0) block2[sapply(blockStrategy2,class)=="numeric"] <- lapply(blockStrategy2[sapply(blockStrategy2,class)=="numeric"], function(x) names(fd2)[x])
		# If the input is just a vector
		# If the input is character, take it as is
		} else if (class(blockStrategy2)=="character") {
			block2 <- blockStrategy2
		# If the input is numeric, assume that it is the column position and convert it to variable name
		} else if (class(blockStrategy2)=="numeric") {
			block2 <- names(fd2)[blockStrategy2]
		# If the input is empty then assume the names of the blocking variables are the same as the first dataset
		# For multiple blocking
		} else if (multB & class(blockStrategy2)=="NULL") {
			# for (i in 1:length(blockStrategy1)) {
				# assign(paste("block2",i,sep=".",eval(parse(text=paste("block1",i,sep="")))))
			# } # NO IDEA WHAT I WAS DOING HERE
			block2 <- block1
		# For single blocking
		} else if (class(blockStrategy2)=="NULL") {
			block2 <- block1
			if (all(block2=="dummyBlock")) fd2[,dummyBlock:="1"]
		# If not character then print error
		} else {
			stop("blockStrategy2 unexpected format")
		}
	}
	# If there is not a second dataset then we are doing deduplication
	if (dedup) {
		# If the input is a list then we know we are doing multiple blocking
		if (class(blockStrategy2)=="list" & multB) {
			# If the input is not character or numeric, print error
			if (sum(!(sapply(blockStrategy2,class) %in% c("character","numeric")))>0) stop("blockStrategy2 unexpected format")
			# If the input is character, take it as is
			block2 <- blockStrategy2
			# If the input is numeric, assume that it is the column position and convert it to variable name
			if (sum(sapply(blockStrategy2,class)==c("numeric"))>0) block2[sapply(blockStrategy2,class)=="numeric"] <- lapply(blockStrategy2[sapply(blockStrategy2,class)=="numeric"], function(x) names(fd1)[x])
		# If the input is just a vector
		# If the input is character, take it as is
		} else if (class(blockStrategy2)=="character") {
			block2 <- blockStrategy2
		# If the input is numeric, assume that it is the column position and convert it to variable name
		} else if (class(blockStrategy2)=="numeric") {
			block2 <- names(fd1)[blockStrategy2]
		# If the input is empty then assume the names of the blocking variables are the same as the first dataset
		# For multiple blocking
		} else if (multB & class(blockStrategy2)=="NULL") {
			# for (i in 1:length(blockStrategy1)) {
				# assign(paste("block2",i,sep=".",eval(parse(text=paste("block1",i,sep="")))))
			# } # NO IDEA WHAT I WAS DOING HERE
			block2 <- block1
		# For single blocking
		} else if (class(blockStrategy2)=="NULL") {
			block2 <- block1
		# If not character then print error
		} else {
			stop("blockStrategy2 unexpected format")
		}
	}
	
	# browser();stop()
	# Check whether the user has selected blocking/comparison variables that exist on the datasets
	# It's easy for users to misspell or write them in the wrong case
	error <- FALSE
	# Linkage (single and multi blocking)
	if (!dedup) {
		# blocking
		tmp <- unique(unlist(block1))
		if (any(!tmp %in% names(fd1))) {
			incorrectVars <- tmp[!tmp %in% names(fd1)]
			cat(paste0("\nERROR: The following blockStrategy1 variables do not appear on data1:\n",paste(incorrectVars,collapse="\n"),"\n"))
			error <- TRUE
		}
		tmp <- unique(unlist(block2))
		if (any(!tmp %in% names(fd2))) {
			incorrectVars <- tmp[!tmp %in% names(fd2)]
			cat(paste0("\nERROR: The following blockStrategy2 variables do not appear on data2:\n",paste(incorrectVars,collapse="\n"),"\n"))
			error <- TRUE
		}
		# soundex
		tmp <- unique(unlist(soundexPos)[!grepl("^[0-9]+$",unlist(soundexPos))])
		if (any(!tmp %in% names(fd1))) {
			incorrectVars <- tmp[!tmp %in% names(fd1)]
			cat(paste0("\nERROR: The following soundexPos variables do not appear on data1:\n",paste(incorrectVars,collapse="\n"),"\n"))
			error <- TRUE
		}
		# missingPos
		if (class(missingPos)!="logical") {
			tmp <- unique(unlist(missingPos)[!grepl("^[0-9]+$",unlist(missingPos))])
			if (any(!tmp %in% names(fd1))) {
				incorrectVars <- tmp[!tmp %in% names(fd1)]
				cat(paste0("\nERROR: The following missingPos variables do not appear on data1:\n",paste(incorrectVars,collapse="\n"),"\n"))
				error <- TRUE
			}
		}
		# winkler
		tmp <- unique(winkler1)
		if (any(!tmp %in% names(fd1))) {
			incorrectVars <- tmp[!tmp %in% names(fd1)]
			cat(paste0("\nERROR: The following winkler1 variables do not appear on data1:\n",paste(incorrectVars,collapse="\n"),"\n"))
			error <- TRUE
		}
		tmp <- unique(winkler2)
		if (any(!tmp %in% names(fd2))) {
			incorrectVars <- tmp[!tmp %in% names(fd2)]
			cat(paste0("\nERROR: The following winkler2 variables do not appear on data2:\n",paste(incorrectVars,collapse="\n"),"\n"))
			error <- TRUE
		}
		# damLev
		tmp <- unique(damLev1)
		if (any(!tmp %in% names(fd1))) {
			incorrectVars <- tmp[!tmp %in% names(fd1)]
			cat(paste0("\nERROR: The following damLev1 variables do not appear on data1:\n",paste(incorrectVars,collapse="\n"),"\n"))
			error <- TRUE
		}
		tmp <- unique(damLev2)
		if (any(!tmp %in% names(fd2))) {
			incorrectVars <- tmp[!tmp %in% names(fd2)]
			cat(paste0("\nERROR: The following damLev2 variables do not appear on data2:\n",paste(incorrectVars,collapse="\n"),"\n"))
			error <- TRUE
		}
	# Deduplication
	} else {
		# blocking
		tmp <- unique(c(unlist(block1),unlist(block2)))
		if (any(!tmp %in% names(fd1))) {
			incorrectVars <- tmp[!tmp %in% names(fd1)]
			cat(paste0("\nERROR: The following blocking variables do not appear on data1:\n",paste(incorrectVars,collapse="\n"),"\n"))
			error <- TRUE
		}
		# soundex
		tmp <- unique(unlist(soundexPos)[!grepl("^[0-9]+$",unlist(soundexPos))])
		if (any(!tmp %in% names(fd1))) {
			incorrectVars <- tmp[!tmp %in% names(fd1)]
			cat(paste0("\nERROR: The following soundexPos variables do not appear on data1:\n",paste(incorrectVars,collapse="\n"),"\n"))
			error <- TRUE
		}
		# missingPos
		if (class(missingPos)!="logical") {
			tmp <- unique(unlist(missingPos)[!grepl("^[0-9]+$",unlist(missingPos))])
			if (any(!tmp %in% names(fd1))) {
				incorrectVars <- tmp[!tmp %in% names(fd1)]
				cat(paste0("\nERROR: The following missingPos variables do not appear on data1:\n",paste(incorrectVars,collapse="\n"),"\n"))
				error <- TRUE
			}
		}
		# winkler
		tmp <- unique(c(winkler1,winkler2))
		if (any(!tmp %in% names(fd1))) {
			incorrectVars <- tmp[!tmp %in% names(fd1)]
			cat(paste0("\nERROR: The following winkler variables do not appear on data1:\n",paste(incorrectVars,collapse="\n"),"\n"))
			error <- TRUE
		}
		# damLev
		tmp <- unique(c(damLev1,damLev2))
		if (any(!tmp %in% names(fd1))) {
			incorrectVars <- tmp[!tmp %in% names(fd1)]
			cat(paste0("\nERROR: The following damLev variables do not appear on data1:\n",paste(incorrectVars,collapse="\n"),"\n"))
			error <- TRUE
		}
	}
	# If any of the variables specified in the parameters were incorrect exit the function
	if (error) return(cat("\nPlease review your parameters\n"))

	
	# If addPrefix option is toggled on then add a meaningful prefix to the variable names
	if (addPrefix & !dedup) {
		# Datasets
		setnames(fd1,names(fd1),paste("d1",names(fd1),sep="."))
		setnames(fd2,names(fd2),paste("d2",names(fd2),sep="."))
		# Blocking lists or vectors
		if (multB) {
			block1 <- lapply(block1, function(x) paste("d1",x,sep="."))
			block2 <- lapply(block2, function(x) paste("d2",x,sep="."))
		} else {
			block1 <- paste("d1",block1,sep=".")
			block2 <- paste("d2",block2,sep=".")
		}
		# Soundex
		if (sum(sapply(soundexPos,class)==c("character"))>0) soundexPos <- paste("d1",soundexPos,sep=".")
		# Winkler
		if (length(winkler1)>0) winkler1 <- paste("d1",winkler1,sep=".")
		if (length(winkler2)>0) winkler2 <- paste("d2",winkler2,sep=".")
		# damLev
		if (length(damLev1)>0) damLev1 <- paste("d1",damLev1,sep=".")
		if (length(damLev2)>0) damLev2 <- paste("d2",damLev2,sep=".")
	}
	
	# Ensure all blocking variables are character
	if (!dedup) {
		blockAll1 <- unique(unlist(block1))
		fd1[,(blockAll1):=lapply(.SD, as.character), .SDcols=blockAll1]
		blockAll2 <- unique(unlist(block2))
		fd2[,(blockAll2):=lapply(.SD, as.character), .SDcols=blockAll2]
	} else {
		blockAll1 <- unique(c(unlist(block1),unlist(block2)))
		fd1[,(blockAll1):=lapply(.SD, as.character), .SDcols=blockAll1]
	}
	
	# If force upper case option toggled on then make sure all blocking and comparison variables are upper case
	if (forceUpperCase) {
		if (!dedup) {
			vars1 <- unique(c(unlist(block1),winkler1,damLev1))
			fd1[,(vars1):=lapply(.SD, toupper), .SDcols=vars1]
			vars2 <- unique(c(unlist(block2),winkler2,damLev2))
			fd2[,(vars2):=lapply(.SD, toupper), .SDcols=vars2]
		} else {
			vars1 <- unique(c(unlist(block1),unlist(block2),winkler1,damLev1))
			fd1[,(vars1):=lapply(.SD, toupper), .SDcols=vars1]
		}
	}
	
	# Soundex
	# Check if any variables are to be blocked using soundex (Are we doing soundex?)
	if (class(soundexPos)=="list" & multB) {
		# If the input is not character or numeric, print error
		if (sum(!(sapply(soundexPos,class) %in% c("character","numeric","NULL")))>0) stop("soundexPos unexpected format")
		# If the input is numeric, take it as is
		soundexList <- soundexPos
		# If the input is character, convert it to the position relative to the blocking list (Must assume that the names relate to the first dataset)
		if (sum(sapply(soundexPos,class)==c("character"))>0) soundexList[sapply(soundexPos,class)=="character"] <- mapply(match,soundexPos[sapply(soundexPos,class)=="character"],block1[sapply(soundexPos,class)=="character"],SIMPLIFY=F)
		
		# Select the blocking vars to apply soundex to
		soundexList1 <- mapply("[",block1,soundexList,SIMPLIFY=F)
		soundexList2 <- mapply("[",block2,soundexList,SIMPLIFY=F)
		# Create a unique vector containing the vars to apply soundex to
		soundexVec1 <- unique(unlist(soundexList1))
		soundexVec2 <- unique(unlist(soundexList2))
		if (dedup) soundexVec1 <- unique(c(soundexVec1,soundexVec2))
		# Create a vector containing the new variable names
		soundexVec1New <- paste(soundexVec1,"soundex",sep=".")
		soundexVec2New <- paste(soundexVec2,"soundex",sep=".")
		if (dedup) soundexVec1New <- unique(c(soundexVec1New,soundexVec2New))
		
		# Apply soundex to the first dataset and update the blocking vector
		# Calculate the soundex for each variable
		fd1[,(soundexVec1New):=lapply(.SD, soundex),.SDcols=soundexVec1]
		if (!dedup) fd2[,(soundexVec2New):=lapply(.SD, soundex),.SDcols=soundexVec2] ##### SLOW
		# Update the blocking vector with the names of the soundex variables
		for (i in 1:length(soundexList)) {
			block1[[i]][soundexList[[i]]] <- paste(block1[[i]][soundexList[[i]]],"soundex",sep=".")
			block2[[i]][soundexList[[i]]] <- paste(block2[[i]][soundexList[[i]]],"soundex",sep=".")
		}
	
	# If we don't have a list do we have a vector?
	} else if (length(soundexPos)>0 & !multB) {
		# If the vector is numeric assume vector position has been specified
		if (class(soundexPos)=="numeric") soundexVec <- soundexPos
		# If the vector is character assume variable names have been specified, are the same on each dataset and convert them to vector position
		else if (class(soundexPos)=="character") soundexVec <- which(block1 %in% soundexPos[[i]])
		# If neither of the above then print error
		else stop("soundexPos unexpected format")
		# Select the blocking vars to apply soundex to
		soundexVec1 <- block1[soundexVec]
		soundexVec2 <- block2[soundexVec]
		if (dedup) soundexVec1 <- unique(c(soundexVec1,soundexVec2))
		# Create a vector containing the new variable names
		soundexVec1New <- paste(soundexVec1,"soundex",sep=".")
		soundexVec2New <- paste(soundexVec2,"soundex",sep=".")
		if (dedup) soundexVec1New <- unique(c(soundexVec1New,soundexVec2New))
		
		# Apply soundex to the first dataset and update the blocking vector
		# Calculate the soundex for each variable
		fd1[,(soundexVec1New):=lapply(.SD, soundex),.SDcols=soundexVec1]
		
		# If there is a second dataset then we are doing linkage and not deduplication
		if (!dedup) {
			# Apply soundex to the second dataset and update the blocking vector
			# Calculate the soundex for each variable
			fd2[,(soundexVec2New):=lapply(.SD, soundex),.SDcols=soundexVec2]
		}
		
		# Update the blocking vector with the names of the soundex variables
		block1[soundexVec] <- soundexVec1New
		block2[soundexVec] <- soundexVec2New
		
	} else if (class(soundexPos)!="NULL") stop("soundexPos unexpected format")
	
	
	# Convert all missing to "" for consistency and to speed up the merge (probably)
	# Define a function to convert all missing to ""
	missing2blank <- function(x) {
		x[is.na(x)] <- ""
		x
	}
	# Run that function over all of the blocking variables on both datasets
	if (!dedup) {
		blockAll1 <- unique(unlist(block1))
		fd1[,(blockAll1):=lapply(.SD, missing2blank), .SDcols=blockAll1]
		blockAll2 <- unique(unlist(block2))
		fd2[,(blockAll2):=lapply(.SD, missing2blank), .SDcols=blockAll2]
	} else {
		blockAll1 <- unique(c(unlist(block1),unlist(block2)))
		fd1[,(blockAll1):=lapply(.SD, missing2blank), .SDcols=blockAll1]
	}
	
	
	# If missingPos has been specified then replace missings with unique values
	# For all blocking variables
	if (ifelse(class(missingPos)=="logical",ifelse(missingPos,TRUE,FALSE),FALSE)) {
		for (i in blockAll1) {
			setkeyv(fd1,i)
			fd1[.(""), (i):=paste0("1",i,1:nrow(fd1[.(""), (i), with=F])), with=F]
		}
		if (!dedup) {
			for (i in blockAll2) {
				setkeyv(fd2,i)
				fd2[.(""), (i):=paste0("2",i,1:nrow(fd2[.(""), (i), with=F])), with=F]
			}
		}
	# For some blocking variables (single block)
	} else if (!multB & ifelse(class(missingPos) %in% c("numeric","character"),TRUE,FALSE)) {
		# If the vector is numeric assume vector position has been specified
		if (class(missingPos)=="numeric") missingVec <- missingPos
		# If the vector is character assume variable names have been specified, are the same on each dataset and convert them to vector position
		else if (class(missingPos)=="character") missingVec <- which(block1 %in% missingPos[[i]])
		# If neither of the above then print error
		else stop("missingPos unexpected format")
		# Select the blocking vars to apply uniqueness to
		missingVec1 <- block1[missingVec]
		missingVec2 <- block2[missingVec]
		if (dedup) missingVec1 <- unique(c(missingVec1,missingVec2))
		# Apply uniqueness
		for (i in missingVec1) {
			setkeyv(fd1,i)
			fd1[.(""), (i):=paste0("1",i,1:nrow(fd1[.(""), (i), with=F])), with=F]
		}
		if (!dedup) {
			for (i in missingVec2) {
				setkeyv(fd2,i)
				fd2[.(""), (i):=paste0("2",i,1:nrow(fd2[.(""), (i), with=F])), with=F]
			}
		}
	# For some blocking variables (multiple blocks)
	} else if (multB & ifelse(class(missingPos) %in% c("list"),TRUE,FALSE)) {
		# If the input is not character or numeric, print error
		if (sum(!(sapply(missingPos,class) %in% c("character","numeric","NULL")))>0) stop("missingPos unexpected format")
		# If the input is numeric, take it as is
		missingList <- missingPos
		# If the input is character, convert it to the position relative to the blocking list (Must assume that the names relate to the first dataset)
		if (sum(sapply(missingPos,class)==c("character"))>0) missingList[sapply(missingPos,class)=="character"] <- mapply(match,missingPos[sapply(missingPos,class)=="character"],block1[sapply(missingPos,class)=="character"],SIMPLIFY=F)
		# Select the blocking vars to apply uniqueness to
		missingList1 <- mapply("[",block1,missingList,SIMPLIFY=F)
		missingList2 <- mapply("[",block2,missingList,SIMPLIFY=F)
		# Create a unique vector containing the vars to apply uniqueness to
		missingVec1 <- unique(unlist(missingList1))
		missingVec2 <- unique(unlist(missingList2))
		if (dedup) missingVec1 <- unique(c(missingVec1,missingVec2))
		# Apply uniqueness
		for (i in missingVec1) {
			setkeyv(fd1,i)
			fd1[.(""), (i):=paste0("1",i,1:nrow(fd1[.(""), (i), with=F])), with=F]
		}
		if (!dedup) {
			for (i in missingVec2) {
				setkeyv(fd2,i)
				fd2[.(""), (i):=paste0("2",i,1:nrow(fd2[.(""), (i), with=F])), with=F]
			}
		}
	# Else if anything is specified than it is in error
	} else if (ifelse(class(c)=="logical",ifelse(c,TRUE,FALSE),ifelse(class(c) %in% c("list","numeric","character"),TRUE,FALSE))) {
		stop("missingPos unexpected format")
	}
	# Else it's fine to match missings
	
	
	# For deduplication where the blocking variables (name and order) are not exactly the same across block1 and block2, create a second file as the index may differ
	if (dedup & !identical(block1,block2)) fd2 <- copy(fd1)
	
	
	# Create block key/s (what we will merge on) ##### Is this necessary? Can I use setkeyv() instead of creating blockKeys? Would that be faster? I think it would cause complications that aren't worth introducing
	if (multB) {
		for (i in 1:length(block1)) {
			fd1[,paste("blockKey",i,sep="."):=do.call(paste, c(fd1[,.SD,.SDcols=block1[[i]]], sep = "~"))]
			if (!dedup | !identical(block1,block2)) fd2[,paste("blockKey",i,sep="."):=do.call(paste, c(fd2[,.SD,.SDcols=block2[[i]]], sep = "~"))]
		}
	} else {
		fd1[,blockKey:=do.call(paste, c(fd1[,.SD,.SDcols=block1], sep = "~"))]
		if (!dedup | !identical(block1,block2)) fd2[,blockKey:=do.call(paste, c(fd2[,.SD,.SDcols=block2], sep = "~"))]
	}
	
	# browser();stop()
	
	# It's useful to know how many pairs one can expect given the blocking strategy
	# It's also important to break this up blocks if there are multiple blocks
	# That way if it falls over the user has some idea of why
	if (multB) {
		cat("\n")
		if (!dedup) {
			for (i in 1:length(block1)) {
				tmpFreqs <- merge(data.frame(table(fd1[,paste("blockKey",i,sep="."),with=F])),data.frame(table(fd2[,paste("blockKey",i,sep="."),with=F])),"Var1")
				cat(paste("A total of",sum(tmpFreqs$Freq.x*tmpFreqs$Freq.y),"pairs were found with block",i,"\n"))
				if (i==1) blockKeyFreqs <- tmpFreqs
				else blockKeyFreqs <- rbind(blockKeyFreqs,tmpFreqs)
			}
			expectedPairs <- sum(as.numeric(blockKeyFreqs$Freq.x*blockKeyFreqs$Freq.y))
		} else {
			for (i in 1:length(block1)) {
				if (identical(block1[[i]],block2[[i]])) {
					tmpFreqs <- data.frame(table(fd1[,paste("blockKey",i,sep="."),with=F]))
					# cat(paste("A total of",sum(factorial(tmpFreqs$Freq[tmpFreqs$Freq>1])/(factorial(2)*factorial(tmpFreqs$Freq[tmpFreqs$Freq>1]-2))),"pairs were found with block",i,"\n"))
					cat(paste("A total of",sum(as.numeric(tmpFreqs$Freq*(tmpFreqs$Freq-1)/2)),"pairs were found with block",i,"\n"))
					if (!exists("blockKeyFreqs1")) blockKeyFreqs1 <- tmpFreqs
					else blockKeyFreqs1 <- rbind(blockKeyFreqs1,tmpFreqs)
				} else {
					tmpFreqs <- merge(data.frame(table(fd1[,paste("blockKey",i,sep="."),with=F])),data.frame(table(fd2[,paste("blockKey",i,sep="."),with=F])),"Var1")
					cat(paste("A total of",sum(tmpFreqs$Freq.x*tmpFreqs$Freq.y),"pairs were found with block",i,"\n"))
					if (!exists("blockKeyFreqs2")) blockKeyFreqs2 <- tmpFreqs
					else blockKeyFreqs2 <- rbind(blockKeyFreqs2,tmpFreqs)
				}
			}
			if (exists("blockKeyFreqs1")) {
				blockKeyFreqs1 <- blockKeyFreqs1[blockKeyFreqs1$Freq>1,]
				# expectedPairs1 <- sum(factorial(blockKeyFreqs1$Freq)/(factorial(2)*factorial(blockKeyFreqs1$Freq-2)))
				expectedPairs1 <- sum(as.numeric(blockKeyFreqs1$Freq*(blockKeyFreqs1$Freq-1)/2))
			} else {
				expectedPairs1 <- 0
			}
			if (exists("blockKeyFreqs2")) {
				expectedPairs2 <- sum(blockKeyFreqs2$Freq.x*blockKeyFreqs2$Freq.y)
			} else {
				expectedPairs2 <- 0
			}
			expectedPairs = expectedPairs1 + expectedPairs2
		}
		if (expectedPairs==0) return(cat("\nNo pairs were found with this blocking strategy\n"))
		else cat(paste("Note: The total number of pairs may be reduced when duplicate pairs are removed\n"))
	} else {
		if (!dedup | !identical(block1,block2)) {
			blockKeyFreqs <- merge(data.frame(table(fd1[,"blockKey",with=F])),data.frame(table(fd2[,"blockKey",with=F])),"Var1")
			expectedPairs <- sum(as.numeric(blockKeyFreqs$Freq.x*blockKeyFreqs$Freq.y))
		} else {
			blockKeyFreqs <- data.frame(table(fd1[,"blockKey",with=F]))
			blockKeyFreqs <- blockKeyFreqs[blockKeyFreqs$Freq>1,]
			# expectedPairs <- sum(as.numeric(factorial(blockKeyFreqs$Freq)/(factorial(2)*factorial(blockKeyFreqs$Freq-2))))
			expectedPairs <- sum(as.numeric(blockKeyFreqs$Freq*(blockKeyFreqs$Freq-1)/2))
		}
		if (expectedPairs==0) return(cat("\nNo pairs were found with this blocking strategy\n"))
		else cat(paste("\nA total of",expectedPairs,"pairs were found with this blocking strategy\n"))
	}
	
	
	# I'm not sure if this is worth doing. The analyst probably prefers it, but it is possibly additional calculations that don't need to be forced upon them
	# Convert all "" to missing
	# blank2missing <- function(x) {
		# x[x==""] <- NA
		# x
	# }
	# fd1[,(block1):=lapply(.SD, blank2missing), .SDcols=block1]
	# if (!dedup) fd2[,(block2):=lapply(.SD, blank2missing), .SDcols=block2]
	
	# browser()
	# stop()
	
	
	# browser();stop()
	
	# Merge the two files by the block key/s
	if (multB) {
		fd1[,rown:=1:nrow(fd1)]
		if (!dedup | !identical(block1,block2)) fd2[,rown:=1:nrow(fd2)]
		for (i in 1:length(block1)) {
			# Linkage
			if (!dedup) {
				tmp <- fd1[fd2,on=paste0("blockKey.",i), allow.cartesian=T, nomatch=0]
				tmp[,pair:=paste(rown,i.rown,sep="~")] ##### SLOW
				tmp[,blockNo:=i]
				
				setnames(tmp, paste0("blockKey.",i), "blockKey")
				setcolorder(tmp, c("blockKey",setdiff(names(tmp), "blockKey")))
				
				if (i==1) {
					blockPairs <- tmp
				} else {
					setkey(tmp, pair)
					blockPairs <- rbindlist(list(blockPairs,tmp[!.(blockPairs[,pair])]))
				}
			# Deduplication
			} else if (identical(block1,block2)) {
				tmp <- fd1[fd1, on=paste0("blockKey.",i), allow.cartesian=T, nomatch=0]
				tmp[,blockNo:=i]
				
				tmp[,rowmin:=pmin(rown, i.rown)]
				tmp[,rowmax:=pmax(rown, i.rown)]
				tmp <- tmp[!duplicated(tmp[,list(rowmin,rowmax)]) & tmp[,rowmin]!=tmp[,rowmax]]
				
				if (i==1) {
					blockPairs <- tmp
				} else blockPairs <- rbindlist(list(blockPairs,tmp[!(paste(tmp$rowmin,tmp$rowmax,sep="~") %in% paste(blockPairs$rowmin,blockPairs$rowmax,sep="~")),]))
				
			} else {
				tmp <- fd1[fd2,on=paste0("blockKey.",i), allow.cartesian=T, nomatch=0]
				tmp[,blockNo:=i]
				
				tmp[,rowmin:=pmin(rown, i.rown)]
				tmp[,rowmax:=pmax(rown, i.rown)]
				tmp <- tmp[!duplicated(tmp[,list(rowmin,rowmax)]) & tmp[,rowmin]!=tmp[,rowmax]]
				
				if (i==1) {
					blockPairs <- tmp
				# Not sure which is faster
				} else {
					blockPairs <- rbindlist(list(blockPairs,tmp[!(paste(tmp$rowmin,tmp$rowmax,sep="~") %in% paste(blockPairs$rowmin,blockPairs$rowmax,sep="~")),]))
				}
			}
			rm(tmp)
		}
	} else {
		# Linkage
		if (!dedup) {
			blockPairs <- fd1[fd2,on="blockKey", allow.cartesian=T, nomatch=0]
		# Deduplication
		} else if (identical(block1,block2)) {
			fd1[,rown:=1:nrow(fd1)]
			blockPairs <- fd1[fd1,on="blockKey", allow.cartesian=T, nomatch=0]
			
			blockPairs[,rowmin:=pmin(rown, i.rown)]
			blockPairs[,rowmax:=pmax(rown, i.rown)]
			blockPairs <- blockPairs[!duplicated(blockPairs[,list(rowmin,rowmax)]) & blockPairs[,rowmin]!=blockPairs[,rowmax]]

		} else {
			fd1[,rown:=1:nrow(fd1)]
			fd2[,rown:=1:nrow(fd2)]
			blockPairs <- fd1[fd2,on="blockKey", allow.cartesian=T, nomatch=0]
			
			blockPairs[,rowmin:=pmin(rown, i.rown)]
			blockPairs[,rowmax:=pmax(rown, i.rown)]
			blockPairs <- blockPairs[!duplicated(blockPairs[,list(rowmin,rowmax)]) & blockPairs[,rowmin]!=blockPairs[,rowmax]]

		}
	}
	
	# Store the names from the original datasets to check for commonality later
	n1 <- names(fd1)
	if (!dedup | !identical(block1,block2)) n2 <- names(fd2)
	else n2 <- n1
	# If efficiency is a concern then remove the original data
	if (efficient) {
		rm(fd1)
		if (!dedup | !identical(block1,block2)) rm(fd2)
	}
	
	
	# If didn't allow missings then replace the unique values created earlier with blanks
	# browser();stop()
	# All
	if (ifelse(class(missingPos)=="logical",ifelse(missingPos,TRUE,FALSE),FALSE)) {
		block1u <- block1
		block2u <- block2
		same1 <- block1u %in% n2
		same2 <- block2u %in% n1
		# # block1u[same1] <- paste(block1u[same1],".data1",sep="")
		# # block2u[same2] <- paste(block2u[same2],".data2",sep="")
		block2u[same2] <- paste0("i.",block2u[same2])
		
		if (!dedup) {
			blockAll1 <- unique(unlist(block1u))
			blockAll2 <- unique(unlist(block2u))
		} else {
			blockAll1 <- unique(c(unlist(block1u),unlist(block2u)))
		}
		
		
		for (i in blockAll1) {
			blockPairs[grepl(paste0("^1",i),blockPairs[[i]]), (i):="", with=F]
		}
		if (!dedup) {
			for (i in blockAll2) {
				blockPairs[grepl(paste0("^2",gsub("i[.]","",i)),blockPairs[[i]]), (i):="", with=F]
			}
		}
	# Some
	} else if (ifelse(class(missingPos) %in% c("list","numeric","character"),TRUE,FALSE)) {
		same1 <- missingVec1 %in% n2
		same2 <- missingVec2 %in% n1
		# Update the winkler vectors where appropriate
		# # missingVec1[same1] <- paste(missingVec1[same1],".data1",sep="")
		# # missingVec2[same2] <- paste(missingVec2[same2],".data2",sep="")
		missingVec2[same2] <- paste0("i.",missingVec2[same2])
		for (i in missingVec1) {
			# blockPairs[grepl(paste0("^1",gsub("[.]data[12]","",i)),blockPairs[[i]]), (i):="", with=F]
			blockPairs[grepl(paste0("^1",i),blockPairs[[i]]), (i):="", with=F]
		}
		if (!dedup) {
			for (i in missingVec2) {
				# blockPairs[grepl(paste0("^2",gsub("[.]data[12]","",i)),blockPairs[[i]]), (i):="", with=F]
				blockPairs[grepl(paste0("^2",gsub("i[.]","",i)),blockPairs[[i]]), (i):="", with=F]
			}
		}
	# Else none to change
	}
	
	
	# browser();stop()
	# For deduplication, if the addPrefix thing is toggled, change the variable names
	if (addPrefix & dedup) {
		# blockPairs
		# setnames(blockPairs,grep("^i[.]",names(blockPairs)),paste0("d1.",gsub("^i[.]","",names(blockPairs)[grepl("^i[.]",names(blockPairs))])))
		setnames(blockPairs,grep("^i[.]",names(blockPairs)),paste0("d2.",gsub("^i[.]","",names(blockPairs)[grepl("^i[.]",names(blockPairs))])))
		setnames(blockPairs,
			names(blockPairs)[names(blockPairs) %in% gsub("^d2[.]","",names(blockPairs)[grepl("^d2[.]",names(blockPairs))])],
			paste0("d1.",names(blockPairs)[names(blockPairs) %in% gsub("^d2[.]","",names(blockPairs)[grepl("^d2[.]",names(blockPairs))])]))
		# Blocking lists or vectors
		if (multB) {
			block1 <- lapply(block1, function(x) paste("d1",x,sep="."))
			block2 <- lapply(block2, function(x) paste("d2",x,sep="."))
		} else {
			block1 <- paste("d1",block1,sep=".")
			block2 <- paste("d2",block2,sep=".")
		}
		# Soundex
		if (sum(sapply(soundexPos,class)==c("character"))>0) soundexPos <- paste("d1",soundexPos,sep=".")
		# Winkler
		if (length(winkler1)>0) winkler1 <- paste("d1",winkler1,sep=".")
		if (length(winkler2)>0) winkler2 <- paste("d2",winkler2,sep=".")
		# damLev
		if (length(damLev1)>0) damLev1 <- paste("d1",damLev1,sep=".")
		if (length(damLev2)>0) damLev2 <- paste("d2",damLev2,sep=".")
	}
	
	
	
	
	
	# If no pairs were found given the blocking strategy, report this and exit the function
	# if (nrow(blockPairs)==0) {
		# return("No pairs were found with this blocking strategy")
	# }
	
	# browser();stop();
	
	### COMPARE
	# Jaro-Winkler
	if (length(winkler1)>0) {
		# Detect whether input is of the correct format
		if (class(winkler1)=="character") {
			winklerVec1 <- winkler1
		# If not character then print error
		} else {
			stop("winkler1 unexpected format")
		}
		# Detect whether input is of the correct format
		if (class(winkler2)=="character") {
			winklerVec2 <- winkler2
		# If the input is empty then assume the names of the winkler variables are the same as the first dataset
		} else if (class(winkler2)=="NULL") {
			winklerVec2 <- gsub("^d1[.]","d2.",winklerVec1)
		# If not character then print error
		} else {
			stop("winkler2 unexpected format")
		}
		
		if (class(winklerSplitVec1)!="NULL" & class(winklerSplitVec2)=="NULL") {
			winklerSplitVec2 <- rep(NA,length(winklerSplitVec1))
		}
		if (class(winklerSplitVec2)!="NULL" & class(winklerSplitVec1)=="NULL") {
			winklerSplitVec1 <- rep(NA,length(winklerSplitVec2))
		}
		
		# Check that the user has referred to the correct variable names
		if (addPrefix) {
			if (sum(!(winklerVec1 %in% names(blockPairs)))>0) {
				print("ERROR: There is an inconsistency between the user specified variables in the parameter 'winkler1' and the variable names that result from importing")
				print("The variable names that result from importing are:")
				print(n1)
			}
			if (sum(!(winklerVec2 %in% names(blockPairs)))>0) {
				print("ERROR: There is an inconsistency between the user specified variables in the parameter 'winkler2' and the variable names that result from importing")
				print("The variable names that result from importing are:")
				print(n1)
			}
		} else {
			if (sum(!(winklerVec1 %in% n1))>0) {
				print("ERROR: There is an inconsistency between the user specified variables in the parameter 'winkler1' and the variable names that result from importing")
				print("The variable names that result from importing are:")
				print(n1)
			}
			if (sum(!(winklerVec2 %in% n2))>0) {
				print("ERROR: There is an inconsistency between the user specified variables in the parameter 'winkler2' and the variable names that result from importing")
				print("The variable names that result from importing are:")
				print(n2)
			}
		}
		
		# If a variable name was common across datasets then it would have received a suffix in the merge
		# Check the winkler variables for commonality across datasets
		same1 <- winklerVec1 %in% n2
		same2 <- winklerVec2 %in% n1
		# Update the winkler vectors where appropriate
		# # winklerVec1[same1] <- paste(winklerVec1[same1],".data1",sep="")
		# # winklerVec2[same2] <- paste(winklerVec2[same2],".data2",sep="")
		winklerVec2[same2] <- paste0("i.",winklerVec2[same2])
		# Ensure all winkler variables are character
		blockPairs[,(unique(c(winklerVec1,winklerVec2))):=lapply(.SD, as.character), .SDcols=unique(c(winklerVec1,winklerVec2))]
		
		# Convert all "" to missing for consistency and maybe speed
		# Define a function to convert all "" to missing
		blank2missing <- function(x) {
			x[x==""] <- NA
			x
		}
		# Run that function over all of the blocking variables on both datasets
		blockPairs[,(unique(c(winklerVec1,winklerVec2))):=lapply(.SD, blank2missing), .SDcols=unique(c(winklerVec1,winklerVec2))]
		
		# Perform the comparison
		# Loop through each of the comparison variables in the the winkler vector
		for (i in 1:length(winklerVec1)) {
			# Perform a jarowinkler comparison of the full strings
			# As split comparisons also include a full comparison this is not inefficient
			fc <- jarowinkler(blockPairs[[winklerVec1[i]]],blockPairs[[winklerVec2[i]]])
			# Determine whether there are any partial comparisons
			if (length(winklerSplitVec1)>0 | length(winklerSplitVec2)>0) {
				# Determine whether we will be doing partial comparisons on the given comparison variables
				if (is.na(winklerSplitVec1[i]) & is.na(winklerSplitVec2[i])) {
					# No partial comparison on the given comparison variables
					# Store the full comparison under a variable of the format "winkler.varNameData1.varNameData2"
					blockPairs[,(paste("winkler",winklerVec1[i],winklerVec2[i],sep=".")):=fc]
				} else {
					# Partial comparison on the given comparison variables
					# If we are splitting the comparison variable from data1...
					if (!is.na(winklerSplitVec1[i])) {
						# Store the split up components of the string
						split1 <- strsplit(blockPairs[[winklerVec1[i]]],winklerSplitVec1[i])
					# If we are not splitting the comparison variable from data1...
					} else {
						# Store the full string
						split1 <- blockPairs[[winklerVec1[i]]]
					}
					# If we are splitting the comparison variable from data2...
					if (!is.na(winklerSplitVec2[i])) {
						# Store the split up components of the string
						split2 <- strsplit(blockPairs[[winklerVec2[i]]],winklerSplitVec2[i])
					# If we are not splitting the comparison variable from data1...
					} else {
						# Store the full string
						split2 <- blockPairs[[winklerVec2[i]]]
					}
					# Perform jarowinkler comparisons on each combination of components of each string
					pc <- mapply(function(x,y) max(outer(x,y,jarowinkler)), split1, split2)
					# Store the max of the full and partial comparisons under a variable of the format "winkler.varNameData1.varNameData2"
					blockPairs[,(paste("winkler",winklerVec1[i],winklerVec2[i],sep=".")):=mapply(function(x,y) max(x,y), fc, pc)]
				}
			} else {
				# All full comparisons
				# Store the full comparison under a variable of the format "winkler.varNameData1.varNameData2"
				blockPairs[,(paste("winkler",winklerVec1[i],winklerVec2[i],sep=".")):=fc]
			}
		}
	}
	
	# Damerau-Levenshtein Edit Distance
	if (length(damLev1)>0) {
		# Detect whether input is of the correct format
		if (class(damLev1)=="character") {
			damLevVec1 <- damLev1
		# If not character then print error
		} else {
			stop("damLev1 unexpected format")
		}
		# Detect whether input is of the correct format
		if (class(damLev2)=="character") {
			damLevVec2 <- damLev2
		# If the input is empty then assume the names of the damLev variables are the same as the first dataset
		} else if (class(winkler2)=="NULL") {
			damLevVec2 <- gsub("^d1[.]","d2.",damLevVec1)
		# If not character then print error
		} else {
			stop("damLev2 unexpected format")
		}
		
		if (class(damLevSplitVec1)!="NULL" & class(damLevSplitVec2)=="NULL") {
			damLevSplitVec2 <- rep(NA,length(damLevSplitVec1))
		}
		if (class(damLevSplitVec2)!="NULL" & class(damLevSplitVec1)=="NULL") {
			damLevSplitVec1 <- rep(NA,length(damLevSplitVec2))
		}
		
		# Check that the user has referred to the correct variable names
		if (addPrefix) {
			if (sum(!(damLevVec1 %in% names(blockPairs)))>0) {
				print("ERROR: There is an inconsistency between the user specified variables in the parameter 'damLev1' and the variable names that result from importing")
				print("The variable names that result from importing are:")
				print(n1)
			}
			if (sum(!(damLevVec2 %in% names(blockPairs)))>0) {
				print("ERROR: There is an inconsistency between the user specified variables in the parameter 'damLev2' and the variable names that result from importing")
				print("The variable names that result from importing are:")
				print(n1)
			}
		} else {
			if (sum(!(damLevVec1 %in% n1))>0) {
				print("ERROR: There is an inconsistency between the user specified variables in the parameter 'damLev1' and the variable names that result from importing")
				print("The variable names that result from importing are:")
				print(n1)
			}
			if (sum(!(damLevVec2 %in% n2))>0) {
				print("ERROR: There is an inconsistency between the user specified variables in the parameter 'damLev2' and the variable names that result from importing")
				print("The variable names that result from importing are:")
				print(n2)
			}
		}
		
		# If a variable name was common across datasets then it would have received a suffix in the merge
		# Check the damLev variables for commonality across datasets
		same1 <- damLevVec1 %in% n2
		same2 <- damLevVec2 %in% n1
		# Update the damLev vectors where appropriate
		# # damLevVec1[same1] <- paste(damLevVec1[same1],".data1",sep="")
		# # damLevVec2[same2] <- paste(damLevVec2[same2],".data2",sep="")
		damLevVec2[same2] <- paste0("i.",damLevVec2[same2])
		
		# Ensure all damLev variables are character
		blockPairs[,(unique(c(damLevVec1,damLevVec2))):=lapply(.SD, as.character), .SDcols=unique(c(damLevVec1,damLevVec2))]
		
		# Convert all "" to missing for consistency and maybe speed
		# Define a function to convert all "" to missing
		blank2missing <- function(x) {
			x[x==""] <- NA
			x
		}
		# Run that function over all of the blocking variables on both datasets
		blockPairs[,(unique(c(damLevVec1,damLevVec2))):=lapply(.SD, blank2missing), .SDcols=unique(c(damLevVec1,damLevVec2))]
		
		# Define the Damerau-Levenshtein edit distance measure
		damLev <- function(m,n) 1-stringdist(m,n,"dl")/max(nchar(c(m,n)))
		
		# Perform the comparison
		# Loop through each of the comparison variables in the the damLeV vector
		for (i in 1:length(damLevVec1)) {
			# Perform the damLev comparison of the full strings
			# As split comparisons also include a full comparison this is not inefficient
			fc <- damLev(blockPairs[[damLevVec1[i]]],blockPairs[[damLevVec2[i]]])
			# Determine whether there are any partial comparisons
			if (length(damLevSplitVec1)>0 | length(damLevSplitVec2)>0) {
				# Determine whether we will be doing partial comparisons on the given comparison variables
				if (is.na(damLevSplitVec1[i]) & is.na(damLevSplitVec2[i])) {
					# No partial comparison on the given comparison variables
					# Store the full comparison under a variable of the format "damLev.varNameData1.varNameData2"
					blockPairs[,(paste("damLev",damLevVec1[i],damLevVec2[i],sep=".")):=fc]
				} else {
					# Partial comparison on the given comparison variables
					# If we are splitting the comparison variable from data1...
					if (!is.na(damLevSplitVec1[i])) {
						# Store the split up components of the string
						split1 <- strsplit(blockPairs[[damLevVec1[i]]],damLevSplitVec1[i])
					# If we are not splitting the comparison variable from data1...
					} else {
						# Store the full string
						split1 <- blockPairs[[damLevVec1[i]]]
					}
					# If we are splitting the comparison variable from data2...
					if (!is.na(damLevSplitVec2[i])) {
						# Store the split up components of the string
						split2 <- strsplit(blockPairs[[damLevVec2[i]]],damLevSplitVec2[i])
					# If we are not splitting the comparison variable from data1...
					} else {
						# Store the full string
						split2 <- blockPairs[[damLevVec2[i]]]
					}
					# Perform damLev comparisons on each combination of components of each string
					pc <- mapply(function(x,y) max(outer(x,y,function(a,b) 1-stringdist(a,b,"dl")/max(nchar(c(a,b))))), split1, split2)
					# Store the max of the full and partial comparisons under a variable of the format "damLev.varNameData1.varNameData2"
					blockPairs[,(paste("damLev",damLevVec1[i],damLevVec2[i],sep=".")):=mapply(function(x,y) max(x,y), fc, pc)]
				}
			} else {
				# All full comparisons
				# Store the full comparison under a variable of the format "damLev.varNameData1.varNameData2"
				blockPairs[,(paste("damLev",damLevVec1[i],damLevVec2[i],sep=".")):=fc]
			}
		}
	}
	
	# Create a pairID variable
	blockPairs[,pairID:=1:nrow(blockPairs)]
	# Move it to the front
	setcolorder(blockPairs, c("pairID",setdiff(names(blockPairs), "pairID")))
	
	### RESULT
	# Make the result globally accessible
	assign("blockPairs", blockPairs, envir=currentEnvir)
	
	# If a destination has been specified for the output, export blockPairs as a csv
	if (class(output)=="character") {
		# Assume that the reason it is being exported is so that it can be read into SAS
		# therefore it is necessary to replace "." with "_" in variable names
		setnames(blockPairs,names(blockPairs),gsub("[.]","_",names(blockPairs)))
		# SAS also truncates long variable names (horrible program) so let's try make some of them shorter
		setnames(blockPairs,names(blockPairs),gsub("winkler","wink",names(blockPairs)))
		setnames(blockPairs,names(blockPairs),gsub("damLev","dl",names(blockPairs)))
		# Assuming that the other software doesn't interpret NAs, change them to blanks
		# write.csv(blockPairs, output, na="", row.names=F)
		fwrite(blockPairs, output, na="")
	}
	
	# Report the resulting number of pairs
	if (multB) return(cat(paste("\nA total of",nrow(blockPairs),"unique pairs were found with this blocking strategy\n")))
}

# Store the current environment of the workspace
currentEnvir <- environment()
# Load required packages if they have not already been loaded
require(RecordLinkage, quietly=TRUE)
require(data.table, quietly=TRUE)
require(stringdist, quietly=TRUE)
require(bit64, quietly=TRUE)
# require(plyr, quietly=TRUE)
# Compile the code
require(compiler, quietly=TRUE)
rLink <- cmpfun(link)

