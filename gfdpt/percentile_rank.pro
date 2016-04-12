function percentile_rank,array,p
;+
; PURPOSE:    compute the value in ARRAY which is at a given 
;             percentile ranking within ARRAY.  For example,
;             median_of_array=percentile_rank(array,50)
;
; USEAGE:     result=percentile_rank(array,percentile)
;
;             percentile can be a scalar or vector
;-
nn = n_elements(array)
ip =  long(float(p)*nn/100.)

ii = sort(array)
return,array(ii(ip))
END
