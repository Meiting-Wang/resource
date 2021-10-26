*! Stata course
*! Meiting Wang
*! Oct 26, 2021


/*-------------------------------------------------
Contents
1. Stata initial understanding
	1.1 Interface
	1.2 System directory
	1.3 profile.do
	1.4 Mata in Stata
2. Get a rough idea of the data set
3. slides_intro
4. slides_simplereg
	4.1 calculate SST, SSE, SSR, R^2 by hand
	4.2 Units of Measurement and Functional Form
	4.3 Unbiasedness of the OLS estimators(under assumptions SLR.1 through SLR.4)
	4.4 Standard error(under assumptions SLR.1 through SLR.5)
5. slides_multreg
	5.1 Estimate the population parameters
	5.2 Omitted variable bias
	5.3 Multi-step regression
	5.4 Standard error(under assumptions MLR.1 through MLR.5)
	5.5 hypothesis testing
6. Implement multiple regression by hand
	6.1 Stata programming
	6.2 Mata programming
-------------------------------------------------*/



*--------- 1. Stata initial understanding ---------
* 1.1 Interface
cd "E:\学习资料\暨南大学\2021-2022学年上学期\高计助教\高计I\Stata_course"
clear all
cls

* 1.2 System directory
* net install open, from("https://raw.githubusercontent.com/Meiting-Wang/open/main")
sysdir
open stata
open base
open plus
open personal

* 1.3 profile.do
doedit "`c(sysdir_stata)'profile.do"

* 1.4 Mata in Stata
* Stata
sysuse auto, clear
summarize
help summarize
* Mata
mata: 1+1
mata:
A = (1,2,3\4,5,6\7,8,9)
B = sqrt(A)
A
B
end
help mata sqrt
help mata
* Stata and Mata
viewsource logit.ado



*--------- 2. Get a rough idea of the data set ---------
cls
use ..\Data_Sets\WAGE2.DTA, clear
describe
summarize
misstable summarize
misstable pattern



*--------- 3. slides_intro ---------
* cross-sectional Data
cls
clear
set seed 123456
set obs 20
gen id = _n
gen chinese = runiform(0,100)
gen math = runiform(0,100)
gen english = runiform(0,100)
format %9.2f chinese-english
list, sep(20)

* time series data
cls
clear
set seed 123456
set obs 10
gen year = 2000 + _n
gen id = 1
gen chinese = runiform(0,100)
gen math = runiform(0,100)
gen english = runiform(0,100)
format %9.2f chinese-english
list, sep(10)

* panel data
cls
clear
set seed 123456
set obs 20
gen id = _n
expand 10
bysort id: gen year = _n + 2000
gen chinese = runiform(0,100)
gen math = runiform(0,100)
gen english = runiform(0,100)
format %9.2f chinese-english
list in 1/50, sep(10)



*--------- 4. slides_simplereg ---------
/* y = beta0 + beta1*x + u, where E(u|x) = 0 */
cls
use ..\Data_Sets\WAGE2.DTA, clear
d wage educ
sum wage educ
reg wage educ

* 4.1 calculate SST, SSE, SSR, R^2 by hand
qui reg wage educ
predict wage_hat, xb
predict u_hat, residuals

estpost summarize wage
gen temp1 = (wage - e(mean)[1,1])^2
gen temp2 = (wage_hat - e(mean)[1,1])^2
gen temp3 = u_hat^2

estpost summarize temp1-temp3
scalar SST = e(sum)[1,1]
scalar SSE = e(sum)[1,2]
scalar SSR = e(sum)[1,3]
scalar R2 = SSE/SST
drop temp*

dis "SST = " %9.0f SST
dis "SSE = " %10.1f SSE
dis "SSR = " %9.0f SSR
dis "R2 = " %6.4f R2

reg wage educ

* 4.2 Units of Measurement and Functional Form
cls
use ..\Data_Sets\WAGE2.DTA, clear
gen educ_d_100 = educ / 100
gen wage_m_100 = wage * 100

eststo clear
qui eststo: reg wage educ
qui eststo: reg wage educ_d_100
qui eststo: reg wage_m_100 educ
qui eststo: reg lwage educ
esttab *, b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) r2 obslast nogaps

* 4.3 Unbiasedness of the OLS estimators(under assumptions SLR.1 through SLR.4)
* case1: y = 6 + 4x + u, x~N(0,1) u~N(0,1)
cls
clear
set obs 100
set seed 123456
gen id = _n
gen x = rnormal()
gen u = rnormal()
gen y = 6 + 4*x + u
list in 1/20, sep(20)
reg y x

* case2: y = 6 + 4x + u, x~N(0,1) u = 0.5x + N(0,1)
cls
clear
set obs 100
set seed 123456
gen id = _n
gen x = rnormal()
gen u = 0.5*x + rnormal()
gen y = 6 + 4*x + u
list in 1/20, sep(20)
reg y x

* 4.4 Standard error(under assumptions SLR.1 through SLR.5)
* Standard error of the regression(also called root mean squared error in Stata)
* Standard error of the OLS estimators
cls
use ..\Data_Sets\WAGE2.DTA, clear
reg wage educ
local RMSE = sqrt(e(rss)/(e(N)-2))

qui estpost summarize educ
gen temp1 = (educ - e(mean)[1,1])^2
gen temp2 = educ^2
qui estpost summarize temp1 temp2
local se_educ = `RMSE'/sqrt(e(sum)[1,1])
local se_cons = `RMSE'*sqrt(e(count)[1,1]^(-1)*e(sum)[1,2]/e(sum)[1,1])
drop temp*

dis _n _s(5)"RMSE = `RMSE'" _n ///
	_s(1)"se(educ) = `se_educ'" _n ///
	"se(_cons) = `se_cons'"

reg wage educ



*--------- 5. slides_multreg ---------
* 5.1 Estimate the population parameters
* y = 6 + 4x1 + 3x2 + 2x3 + u, x1~N(0,1) x2~N(0,1) x3~N(0,1) u~N(0,1)
cls
clear
set obs 10000
set seed 123456
gen id = _n
gen x1 = rnormal()
gen x2 = rnormal()
gen x3 = rnormal()
gen u = rnormal()
gen y = 6 + 4*x1 + 3*x2 + 2*x3 + u
list in 1/20, sep(20)
reg y x1 x2 x3

* calculate OLS estimators by hand in Stata
matrix accum XTX = x1 x2 x3
matrix vecaccum yTX = y x1 x2 x3
matrix beta = invsym(XTX)*(yTX)'
matrix list beta, format(%11.9f)

* calculate OLS estimators by hand in Mata
mata:
X = st_data(.,tokens("x1 x2 x3"))
y = st_data(.,"y")
X = X, J(rows(X),1,1)
cholinv(cross(X,X))*cross(X,y)
end

* 5.2 Omitted variable bias
cls
clear
set obs 10000
set seed 123456
gen id = _n
gen x1 = rnormal()
gen x2 = 2 + 5*x1 + rnormal()
gen y = 6 + 4*x1 + 3*x2 + rnormal()

reg y x1 x2
local beta1_hat = _b[x1]
local beta2_hat = _b[x2]
reg x2 x1
local delta1_tilde = _b[x1]

local beta1_tilde = `beta1_hat' + `beta2_hat'*`delta1_tilde'
dis "beta1_tilde = `beta1_tilde'"

reg y x1

* 5.3 Multi-step regression
cls
use ..\Data_Sets\WAGE2.DTA, clear
regress lwage educ exper

qui regress educ exper
qui predict resid_educ, residuals
regress lwage resid_educ

* 5.4 Standard error(under assumptions MLR.1 through MLR.5)
* Standard error of the regression(also called root mean squared error in Stata)
* Standard error of the OLS estimators

* Formula I
cls
use ..\Data_Sets\WAGE2.DTA, clear
regress lwage educ exper black
local RMSE = sqrt(e(rss)/e(df_r))

qui regress educ black exper
local se_educ = `RMSE'/sqrt(e(rss))
qui regress exper black educ
local se_exper = `RMSE'/sqrt(e(rss))
qui regress black educ exper
local se_black = `RMSE'/sqrt(e(rss))

dis _n _s(5)"RMSE = `RMSE'" _n ///
	_s(1)"se(educ) = `se_educ'" _n ///
	"se(exper) = `se_exper'" _n ///
	"se(black) = `se_black'"

regress lwage educ exper black

* Formula II
cls
use ..\Data_Sets\WAGE2.DTA, clear
regress lwage educ exper black
local RMSE = sqrt(e(rss)/e(df_r))

mata:
X = st_data(.,tokens("educ exper black"))
X = X, J(rows(X),1,1)
se_beta = `RMSE'*sqrt(diagonal(cholinv(cross(X,X))))
st_matrix("se_beta",se_beta)
end

mat rown se_beta = educ exper black _cons
mat coln se_beta = "Std. err."
dis "RMSE = `RMSE'"
mat list se_beta

regress lwage educ exper black

* 5.5 hypothesis testing
* the relationship between t test and F test
cls
use ..\Data_Sets\WAGE2.DTA, clear
regress lwage educ exper black south sibs
mat temp = r(table)[1..6,5]'
mat list temp

test sibs=0
dis sqrt(r(F))

* calculate t, p, CI by hand(take sibs as an example)
cls
use ..\Data_Sets\WAGE2.DTA, clear
regress lwage educ exper black south sibs
mat temp = r(table)[1..6,5]'
mat list temp

local t = _b[sibs] / _se[sibs]
local p = 2*ttail(e(df_r),abs(`t'))
local z = invttail(e(df_r),0.025)
local ll = _b[sibs] - `z'*_se[sibs]
local ul = _b[sibs] + `z'*_se[sibs]

dis _s(1) "t = `t'" _n ///
	_s(1) "p = `p'" _n ///
	"ll = `ll'" _n ///
	"ul = `ul'"

* calculate F test by hand
cls
use ..\Data_Sets\WAGE2.DTA, clear
regress lwage educ exper black south sibs
test south sibs
ret list

qui regress lwage educ exper black south sibs
local SSR_ur = e(rss)
local R2_ur = e(r2)
qui regress lwage educ exper black
local SSR_r = e(rss)
local R2_r = e(r2)
local F = ((`SSR_r'-`SSR_ur')/2) / (`SSR_ur'/(935-5-1))
// local F = ((`R2_ur'-`R2_r')/2) / ((1-`R2_ur')/(935-5-1))
local p = Ftail(2,935-5-1,`F')

dis "F = `F'" _n ///
	"p = `p'"

* calculate other statistics displayed on the Stata interface after regression by hand
cls
use ..\Data_Sets\WAGE2.DTA, clear
regress lwage educ exper black south sibs
eret list

local F = (e(r2)/5) / ((1-e(r2))/(935-5-1))
local Adj_R2 = 1 - (1-e(r2))*(935-1)/(935-5-1)
dis _s(5)"F = `F'" _n ///
	"Adj_R2 = `Adj_R2'"

* Comparison of test and lincom commands
cls
use ..\Data_Sets\WAGE2.DTA, clear
regress lwage educ exper black south sibs
test sibs
lincom sibs

test educ+exper=0.07
lincom educ+exper-0.07

test (educ+exper=0.07) (south=-0.1)



*--------- 6. Implement multiple regression by hand ---------
* 6.1 Stata programming
cls
clear all
use ..\Data_Sets\WAGE2.DTA, clear
mat accum XTX = educ exper black south sibs
mat vecaccum yTX = lwage educ exper black south sibs
mat accum yTy = lwage, noconstant
mat b = (invsym(XTX)*(yTX)')' //estimated coefficient

local k = rowsof(XTX)
qui count if ~missing(lwage,educ,exper,black,south,sibs)
local n = r(N)
local dof = `n' - `k'
mat s2 = (yTy-b*XTX*b') / `dof'
mat V = s2*invsym(XTX) //Variance covariance matrix of estimated coefficients

eret post b V, obs(`n') dof(`dof')
eret display
mat temp1 = r(table)[1..6,....]'

regress lwage educ exper black south sibs, noheader
mat temp2 = r(table)[1..6,....]'

mat diff = temp1 - temp2
mat list temp1
mat list temp2
mat list diff

* 6.2 Mata programming
cls
clear all
use ..\Data_Sets\WAGE2.DTA, clear

local dependent "lwage"
local independent "educ exper black south sibs"

mata:
X = st_data(.,tokens("`independent'"))
y = st_data(.,"`dependent'")
n = rows(X)
X = X, J(n,1,1)
k = cols(X)
dof = n - k
b = cholinv(cross(X,X))*cross(X,y) //estimated coefficient
e = y - X*b
s2 = cross(e,e)/dof
V = s2*cholinv(cross(X,X))
st_numscalar("r(n)",n)
st_numscalar("r(dof)",dof)
st_matrix("b",b')
st_matrix("V",V)
end

local n = r(n)
local dof = r(dof)
mat coln b = `independent' _cons
mat rown V = `independent' _cons
mat coln V = `independent' _cons

eret post b V, obs(`n') dof(`dof')
eret display
mat temp1 = r(table)[1..6,....]'

regress lwage educ exper black south sibs, noheader
mat temp2 = r(table)[1..6,....]'

mat diff = temp1 - temp2
mat list temp1
mat list temp2
mat list diff
