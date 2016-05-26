
/*collapsing total deaths*/

	use "&outdir/mort_clean2/mort`1'clean", clear
	gen agecat = age
	
	recode agecat 0=0 1/4=1 5/9=5 10/14=10 15/19=15 20/24=20 25/29=25 30/34=30 35/39=35 40/44=40 45/49=45 50/54=50 55/59=55 60/64=60 65/69=65 70/74=70 75/80=75 80/84=80 85/150=85 999=999
	**ATTN**open-ended catch for 85+ age**
	replace agecat=999 if agecat==.							
	
	cap gen number = 1
	
	collapse (sum) number, by(mcounty sex race4 agecat)
	fillin mcounty sex race4 agecat
	
	drop _fillin
	replace number = 0 if number==.
	rename number death`1'
	rename race4 race

	sort mcounty race agecat sex
	compress

	exit