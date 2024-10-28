global path = "/Users/shmuelsan/Dropbox/HUJI/misc/twitter/cars"

** load and merge datasets
* registry
import delimited "$path/0866573c-40cd-4ca8-91d2-9dd2d7a492e5.csv", delimiter("|") varnames(1) encoding(UTF-8) clear
keep mispar_rechev tozeret_cd degem_cd sug_degem
save "$path/cars.dta", replace

* disableds
import delimited "$path/c8b9f9c8-4612-4068-934f-d4acd2e3c06e.csv", delimiter("|") varnames(1) encoding(UTF-8) clear
rename misparrechev mispar_rechev
keep mispar_rechev
duplicates drop
gen disable = 1
merge 1:1 mispar_rechev using "$path/cars.dta", keep(2 3) nogen
replace disable = 0 if disable ==.
save "$path/cars.dta", replace

* car characteristics

import delimited "$path/142afde2-6228-49f9-8a29-9b6c3a0cbe40.csv", delimiter("|") varnames(1) encoding(UTF-8) clear
keep tozeret_cd degem_cd mishkal_kolel shnat_yitzur sug_degem
destring degem_cd, replace force
drop if tozeret_cd ==. | degem_cd ==. | shnat_yitzur ==.
duplicates drop tozeret_cd degem_cd sug_degem , force
merge 1:m tozeret_cd degem_cd sug_degem using "$path/cars.dta", keep(2 3) nogen
save "$path/cars.dta", replace


* price

import delimited "$path/39f455bf-6db0-4926-859d-017f34eacbcb.csv", delimiter("|") bindquote(nobind) varnames(1) stripquote(no) encoding(UTF-8) clear


replace tozeret_cd = subinstr(tozeret_cd, `"""',  "", .)
replace sug_degem = subinstr(sug_degem, `"""',  "", .)
replace degem_cd = subinstr(degem_cd, `"""',  "", .)
replace mehir = subinstr(mehir, `"""',  "", .)
replace shnat_yitzur = subinstr(shnat_yitzur, `"""',  "", .)


destring tozeret_cd degem_cd mehir shnat_yitzur, replace
keep tozeret_cd degem_cd sug_degem mehir shnat_yitzur
merge 1:m tozeret_cd degem_cd sug_degem shnat_yitzur using "$path/cars.dta", keep(2 3) nogen
save "$path/cars.dta", replace

* kilometrage
import delimited "$path/56063a99-8a3e-4ff4-912e-5966c0279bad.csv", delimiter("|") varnames(1) encoding(UTF-8) clear

keep mispar_rechev kilometer_test_aharon
merge 1:1 mispar_rechev using "$path/cars.dta", keep(2 3) nogen
replace kilometer_test_aharon = . if kilometer_test_aharon == 0
save "$path/cars.dta", replace

** clean and make groups

use "$path/cars.dta", replace

keep if sug_degem == "P"
egen mehir_b = cut(mehir), group(30)
egen mishkal_kolel_b = cut(mishkal_kolel), group(30)


save "$path/cars.dta", replace

* export to csv
export delimited using "$path/cars.csv", replace



** histograms
use "$path/cars.dta", replace
hist mehir if inrange(mehir,50000,400000),   frequency xtitle("Car price (NIS)")
graph export "$path/hist_price.png", replace

hist mishkal_kolel if inrange(mishkal_kolel,500,3500),   frequency xtitle("Car weight (kg)")
graph export "$path/hist_weight.png", replace

hist shnat_yitzur if inrange(shnat_yitzur,1996,2024) ,   frequency xtitle("Production Year") discrete  
graph export "$path/hist_year.png", replace

gen kilometer_year = kilometer_test_aharon/(2024-shnat_yitzur)
collapse (mean) kilometer_year, by(disable shnat_yitzur)
twoway (scatter shnat_yitzur kilometer_year if disable == 0) ///
(scatter shnat_yitzur kilometer_year if disable == 1), xtitle("Car weight (kg)") ytitle("Disabled mark share")

** means
use "$path/cars.dta", replace
collapse (mean) disable mehir , by(mehir_b)
twoway (scatter disable mehir if mehir<400000), xtitle("Car price (NIS)") ytitle("Disabled mark share")
graph export "$path/price.png", replace

use "$path/cars.dta", replace
collapse (mean) disable mishkal_kolel , by(mishkal_kolel_b)
twoway (scatter disable mishkal_kolel), xtitle("Car weight (kg)") ytitle("Disabled mark share")
graph export "$path/weight.png", replace

use "$path/cars.dta", replace
collapse (mean) disable, by(shnat_yitzur)
keep if shnat_yitzur>= 1996 & shnat_yitzur<= 2024 
twoway (scatter disable shnat_yitzur), xtitle("Production Year") ytitle("Disabled mark share")
graph export "$path/year.png", replace


use "$path/cars.dta", replace
collapse (mean) mishkal_kolel, by(shnat_yitzur)
keep if shnat_yitzur>= 1996 & shnat_yitzur<= 2024 
twoway (scatter mishkal_kolel shnat_yitzur), xtitle("Production Year") ytitle("Car weight (kg)")
graph export "$path/weight_year.png", replace



** regressions
use "$path/cars.dta", replace

reghdfe disable, absorb(mehir_fe = mehir_b mishkal_kolel_fe  = mishkal_kolel_b shnat_yitzur_fe  = shnat_yitzur)
save "$path/reg.dta", replace

use "$path/reg.dta", replace
keep mehir_b mehir_fe mehir 
collapse (mean) mehir (first) mehir_fe, by(mehir_b)
twoway (scatter mehir_fe mehir if mehir<400000), xtitle("Price") ytitle("Disabled mark share")
graph export "$path/price_reg.png", replace

use "$path/reg.dta", replace
keep mishkal_kolel_b mishkal_kolel_fe mishkal_kolel
collapse (mean) mishkal_kolel (first) mishkal_kolel_fe, by(mishkal_kolel_b)
twoway (scatter mishkal_kolel_fe mishkal_kolel), xtitle("Weight") ytitle("Disabled mark share")
graph export "$path/weight_reg.png", replace

use "$path/reg.dta", replace
keep shnat_yitzur shnat_yitzur_fe
collapse (first) shnat_yitzur_fe, by(shnat_yitzur)
keep if shnat_yitzur>= 1996 & shnat_yitzur<= 2024 
twoway (scatter shnat_yitzur_fe shnat_yitzur), xtitle("Producation Year") ytitle("Disabled mark share")
graph export "$path/year_reg.png", replace


  

