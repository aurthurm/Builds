
WestgardRules <- function(x){
    x          <- as.data.frame(x)
    x$color    = "green"
    x$rule     = " "
    x_mean     = mean(x$x)
    x_sd       = sd(x$x)
    x_sds      = -3:3 * x_sd + x_mean
    minus_3_sd = x_sds[1]
    minus_2_sd = x_sds[2]
    minus_1_sd = x_sds[3]
    plus_1_sd  = x_sds[5]
    plus_2_sd  = x_sds[6]
    plus_3_sd  = x_sds[7]
    x_len      = 1:length(x$x)
    
    for (i in x_len) {
	    # 1_3s Rule:
	    # 1 control result outside 3sd
        if(x$x[i] > plus_3_sd | x$x[i] < minus_3_sd){
            x$color[i] = "red"
            x$rule[i]  = "1_3s" 
        }
        # 1_2s Rule : Warning rule
    	# one of two control results fall outside +- 2SD
    	if(x$x[i] <= plus_3_sd & x$x[i] > plus_2_sd |
    		x$x[i] >= minus_3_sd & x$x[i] < minus_2_sd) {
    		x$color[i] = "orange"
    		x$rule[i]  = "1_2s"
    		# 2_2s Rule:
		    # two control results fall outside 2sd in the same direction
		    # or both controls in the same run exceeded 2sd
    		if(i > 1) {
    		    if(x$x[i-1] <= plus_3_sd & x$x[i-1] > plus_2_sd |
    		       x$x[i-1] >= minus_3_sd & x$x[i-1] < minus_2_sd) {
        		        x$color[i] = "red"
        		        x$rule[i]  = "2_2s"
    		    }
    		}
    	}
	    # R_4s  Rule:
	    # Difference between two runs equals or exceeds 4sd	    
    	if(i > 1){
    		if(abs(x$x[i] - x$x[i -1]) >= x_sd * 4){
    			x$color[i] = "red"
    			x$rule[i]  = "R_4s"
    		}
    	}
    	# 4_1s:
    	# 4 consecutive controls outside 1sd
    	if(i > 3){
    		if(x$x[i] > x_mean & x$x[i] <= plus_1_sd &
    			x$x[i - 1] > x_mean & x$x[i - 1] <= plus_1_sd &
    			x$x[i - 2] > x_mean & x$x[i - 2] <= plus_1_sd &
    			x$x[i - 3] > x_mean & x$x[i - 3] <= plus_1_sd | 
    			x$x[i] < x_mean & x$x[i] >= plus_1_sd &
    			x$x[i - 1] < x_mean & x$x[i - 1] >= minus_1_sd &
    			x$x[i - 2] < x_mean & x$x[i - 2] >= minus_1_sd &
    			x$x[i - 3] < x_mean & x$x[i - 3] >= minus_1_sd){
    			    x$color[i] = "red"
    				  x$rule[i]  = "4_1s"
                    # A correction to make sure that even if the fifth control lies in the same 
                    # range it will not be regarded as a 4_1s.
    				if (x$rule[i-1] == "4_1s"){
    				    x$rule[i]  = "!!!"
    				}
                    if (x$rule[i-2] == "4_1s"){
                        x$rule[i]  = "!!!"
                    }
                    if (x$rule[i-3] == "4_1s"){
                        x$rule[i]  = "!!!"
                    }
                    if (x$rule[i-4] == "4_1s"){
                        x$rule[i]  = "!!!"
                    }
    		}
    	}
    	# 10_x Rule:
    	# 10 consecutive control results are on the same sideof the mean
    	# Not Done. Yu can create it and share with me
    }
    return (x)
}