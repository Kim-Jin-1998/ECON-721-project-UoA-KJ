import delimited "data_f.csv", clear 
gen logprince = log(price_eur_m_whe)
gen loggwh = log(gwh)
gen logcp_meur_nsa_b1g = log(cp_meur_nsa_b1g)
gen logcp_meur_nsa_d1 = log(cp_meur_nsa_d1)
gen logcp_meur_nsa_d11 = log(cp_meur_nsa_d11)
gen logcp_meur_nsa_p6 = log(cp_meur_nsa_p6)
gen logcp_meur_nsa_p7 = log(cp_meur_nsa_p7)
gen logths_hw_b_e_nsa_emp_dc = log(ths_hw_b_e_nsa_emp_dc)
gen logths_hw_c_nsa_emp_dc = log(ths_hw_c_nsa_emp_dc)
gen logths_hw_j_nsa_emp_dc = log(ths_hw_j_nsa_emp_dc)
gen logths_hw_g_i_nsa_emp_dc = log(ths_hw_g_i_nsa_emp_dc)
gen logths_hw_total_nsa_emp_dc = log(ths_hw_total_nsa_emp_dc)
gen logexp_e7000_gwh = log(exp_e7000_gwh)

*ssc install estout, replace

encode country, gen(id)
encode date , gen(time)
gen Y = .
global cx = "logths_hw_b_e_nsa_emp_dc-logexp_e7000_gwh"
*y: price_eur_m_whe logcp_meur_nsa_b1g logcp_meur_nsa_d1 logcp_meur_nsa_d11 logcp_meur_nsa_p6 logcp_meur_nsa_p7
*x: loggwh 
*c: logcp_meur_nsa_b1g-logexp_e7000_gwh
xtset id time // set panel data

local varlist "price_eur_m_whe- exp_e7000_gwh"
estpost summarize `varlist', detail
esttab using test1.rtf, ///
cells("count mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))") ///
b(%8.3f) p(%8.3f) noobs compress replace title(esttab_Table: Descriptive statistics)


* robust test
local vv "price_eur_m_whe loggwh-logexp_e7000_gwh"
foreach v of varlist `vv'{
xtunitroot llc  `v' , demean
} 

*regression
replace Y = logcp_meur_nsa_b1g
reg Y l.Y loggwh $cx
est store m01
xtreg Y l.Y loggwh $cx, fe
est store m02
ivregress 2sls Y (loggwh=l.Y) $cx
est store m03
xtabond2 Y l.Y loggwh $cx, gmm(l.Y) iv(l.Y) robust // system GMM
est store m04

esttab  m01 m02 m03 m04 using model1.rtf

xtabond2 Y l.Y loggwh $cx, gmm(l.Y) iv(l.Y) robust 
est store m01
xtabond2 Y l.Y loggwh $cx, gmm(l.Y) iv(l.Y) robust nolevel
est store m02
xtabond2 Y l.Y loggwh $cx, gmm(l.Y) iv(l.Y) robust twostep
est store m03
xtabond2 Y l.Y loggwh $cx, gmm(l.Y) iv(l.Y) robust twostep nolevel
est store m04

esttab  m01 m02 m03 m04 using model2.rtf

xtabond2 logcp_meur_nsa_d1 l.logcp_meur_nsa_d1 loggwh $cx, gmm(l.logcp_meur_nsa_d1) iv(l.logcp_meur_nsa_d1) robust
est store m01
xtabond2 logcp_meur_nsa_d11 l.logcp_meur_nsa_d11 loggwh $cx, gmm(l.logcp_meur_nsa_d11) iv(l.logcp_meur_nsa_d11) robust 
est store m02
xtabond2 logcp_meur_nsa_p6 l.logcp_meur_nsa_p6 loggwh $cx, gmm(l.logcp_meur_nsa_p6) iv(l.logcp_meur_nsa_p6) robust 
est store m03
xtabond2 logcp_meur_nsa_p7 l.logcp_meur_nsa_p7 loggwh $cx, gmm(l.logcp_meur_nsa_p7) iv(l.logcp_meur_nsa_p7) robust 
est store m04

esttab  m01 m02 m03 m04 using model3.rtf