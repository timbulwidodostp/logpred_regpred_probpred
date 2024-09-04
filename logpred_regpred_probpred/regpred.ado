/*  Program to plot predicted values for linear regression models          */
/*  02/20/95  Joanne M. Garrett                                            */
/*  Form:  regpred y x, from(#) to(#) inc(#) adj(cov_list) options         */
/*  Options required: from(#), to(#), increment(#)                         */
/*  Options allowed: poly, adjust, nomodel, noplot, xlab, ylab, nolist     */
/*  Note:  X variable must be continuous (interval or ordinal)             */
/*  sg42: STB-26                                                           */

program define regpred
  version 3.1
  #delimit ;
  local options "From(real 1) To(real 1) Inc(real 1) Poly(real 0)
                 NOModel Adjust(string) NOList NOPlot T1title(string)
                 L1title(string) *" ;
  #delimit cr
  local varlist "req ex min(2) max(2)"
  local if "opt"
  parse "`*'"
  parse "`varlist'", parse(" ")
  preserve
  capture keep `if'
  keep `varlist' `adjust'
  local varlbly : variable label `1'
  local yvar="`1'"
  local varlblx : variable label `2'
  local xvar="`2'"
  quietly drop if `xvar'==. | `yvar'==.

* If there are covariates, drop obs. with missing, then set them to means 
  parse "`adjust'", parse(" ")
  local numcov=0
  local i=1
  while "`1'"~=""  {
    local cov`i'="`1'"
    quietly drop if `cov`i''==.
    local i=`i'+1
    macro shift
    local numcov=`i'-1
    }
  local i=1
  while `i'<=`numcov'  {
    quietly sum `cov`i''
    local mcov`i'=_result(3)
    local i=`i'+1
    }

* If polynomial terms are requested, create them
  if `poly'==2  {
     gen x_sq=`xvar'^2
     local polylst="x_sq"
     }
  if `poly'==3  {
     gen x_sq=`xvar'^2
     gen x_cube=`xvar'^3
     local polylst="x_sq x_cube"
     }
  
* Run regression model
  if "`nomodel'"~="nomodel"  {
     reg `yvar' `xvar' `polylst' `adjust'
     more
     }
  if "`nomodel'"=="nomodel"  {
     quietly reg `yvar' `xvar' `polylst' `adjust'
     }
  local newn=_result(1)

* Generate the values of x to calculate the predicted values
  drop _all
  local i=`from'
  while `i'<`to'  {
    local i=`i'+`inc'
    }
  if `i'>`to'  {
    local to=`i'-`inc'
    }
  local newobs=((`to'-`from')/`inc')+1
  local newobs=round(`newobs',1)
  quietly range `xvar' `from' `to' `newobs'
  label var `xvar' "`varlblx'"
  if `poly'==2  {
     gen x_sq=`xvar'^2
     }
  if `poly'==3  {
     gen x_sq=`xvar'^2
     gen x_cube=`xvar'^3
     }
  local i=1
  while `i'<=`numcov'  {
    quietly gen `cov`i''=`mcov`i''
    local i=`i'+1
    }
  
* Calculate the predicted values and 95% confidence intervals
  tempvar se
  predict pred_y
  predict `se', stdp
  gen lower=pred_y-1.96*`se'
  gen upper=pred_y+1.96*`se'
    
* Plot and list results
  if "`noplot'"~="noplot"  {
    if "`t1title'"=="" {
       local t1title "Predicted Values for `varlbly' -- $S_E_depv"
       }
    if "`l1title'"=="" {local l1title "Predicted Values and 95% CI"}
    #delimit ;
    graph pred_y upper lower `xvar', sort c(sss) s(Oii) t1("`t1title'")
      l1("`l1title'") `options' ;
    #delimit  cr
    }
  if "`nolist'"~="nolist"  {
     display "  "
     display in green "Predicted Values and 95% Confidence Intervals"
     display "  "
     display "  Outcome Variable:     `varlbly' -- $S_E_depv"
     display "  Independent Variable: `varlblx' -- `xvar'"
     if `poly'==2 | `poly'==3  {
        display "  Polynomial Terms:     `polylst'
        }
     display "  Covariates:           `adjust'"
     display "  Total Observations:   `newn'"
     list `xvar' pred lower upper
     }
end

