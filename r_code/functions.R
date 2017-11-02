# Functions for the analysis of yelloweye rockfish 

# age error ----
f.age_error <- function(x, max){
	# x = data
	# max = max age (aka plus group)
	# data format
	# age 	2nd_read_age		n
	
	# the output produces a file for admb with the following structure:
	# number of obs
	# age vector 
	# percent agreement vector
	# sample size vector
	
	min_age = min(x$age)
	
	x %>% 
		mutate(agree = case_when(age==read_age ~ 1)) %>% 
		group_by(age) %>% 
		summarise(agree = sum(agree, na.rm = T)/sum(n),
					 sample_size = sum(n)) %>% 
		full_join(data.frame(age = min_age:max)) %>% 
		mutate(agree = ifelse(is.na(sample_size), -9, agree),
				 sample_size = ifelse(is.na(sample_size), -9, sample_size)) %>% 
		arrange(age) -> age_error_to_admb
	
	c("# Number of obs", length(age_error_to_admb$age),
	  "# Age vector", age_error_to_admb$age,
	  "# Percent agreement vector", age_error_to_admb$agree,
	  "# Sample size vector", age_error_to_admb$sample_size) 
	
	# the output can be saved in a tabular format e.g.
	# f.age_error(age_error, 75) %>% 
	# 		write.table(,file="admb/age_error/age_error.dat", 
	#							quote=F, row.names=F, col.names=F)
}	


