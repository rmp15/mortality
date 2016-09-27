pause on

// age.do

// converts from "000" format, where the first character is coded 0-6. Values 2-6 reflect infant mortality). 
	
// For use with "format_mort.do"

		cap drop temp1 temp2 
		cap drop newage sqnewage



	if `1'==1962 | `1'==1963 {		/*Years 1962 and 1963 apparently used age categories rather than individual years of age*/

	#delimit;
	
		cap drop age;
		destring agerecode, replace;
		rename agerecode age;
		recode age

		1/2 = 0
		3/6 = 1
		7 = 5
		8 = 10
		9 = 15
		10 = 20
		11 = 25
		12 = 30
		13 = 35
		14 = 40
		15 = 45
		16 = 50
		17 = 55
		18 = 60
		19 = 65
		20 = 70
		21 = 75
		22 = 80
		23 = 85
		24 = 90
		25 = 95
		26 = 100
		27 = 999;
		
	#delimit cr

		}
	
		else if `1'>=2003 {													/* deals with age as four-digit format*/
			cap rename age agedetail
			cap ren age_detail agedetail
			tostring agedetail, replace		
			gen temp1=substr(agedetail,1,1)				
			gen temp2=substr(agedetail,2,3)
			gen temp3=real(temp2) if temp1=="1"
			replace temp3=0 if temp1=="2" | temp1=="4" | temp1=="5" | temp1=="6"
			replace temp3 = 999 if agedetail == "9999" | temp1=="9"

			compress
			drop temp1 temp2 age*
			rename temp3 age
			}
			
		else {																				/* deals with age as three-digit format*/
			cap rename age agedetail
			cap ren age_detail agedetail
			tostring agedetail, replace					
			gen temp1=substr(agedetail,1,1)				
			gen temp2=substr(agedetail,2,2)
			gen temp3=real(temp2) if temp1=="0"
					replace temp3=100+real(temp2) if temp1=="1"
					replace temp3=0 if temp1=="2" | temp1=="3" | temp1=="4" | temp1=="5" | temp1=="6"
					replace temp3 = 999 if agedetail == "9999" | temp1=="9"

			compress
			drop temp1 temp2 age*
			rename temp3 age	
			}	
			
	destring age, replace
	
		{
		    cap drop obs
			gen obs = 1
			sum obs if age==.
				if r(sum)>0 {
					di "ERROR: age has missing values"
					BREAK
					}
			drop obs
		}
	
	/*AGE Categories*/	****************************************************

	#delimit;

	gen agecat = age;
		
		recode agecat 	0=0 1/4=1 5/9=5 10/14=10 15/19=15 20/24=20 25/29=25 30/34=30 35/39=35 40/44=40 45/49=45 50/54=50
		55/59=55 60/64=60 65/69=65 70/74=70 75/79=75 80/84=80 85/89=85 90/94=90 95/99=95 100/150=100 .=999;

	#delimit cr

************************************************************	
	
	
	
	
	exit