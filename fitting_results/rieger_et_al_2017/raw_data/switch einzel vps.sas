/* -------------------------------------------------------------- */

/* -------------------------------------------------------------- */
LIBNAME switch 'c:\switch\sasdat';
OPTIONS PAGESIZE=200 LINESIZE=150 NUMBER;

/* -------------------------------------------------------------- */
/* Rohdaten einlesen                                              */
/* -------------------------------------------------------------- */
DATA switch.temp1;
INFILE 'c:\switch\rohdat\vp10_exp.log';
INPUT   #1 zeile1 temp1 $ cnd1 $  time1_1 temp 
        #2 temp temp1 $ r1_1 $   time1_2 temp
 	    #3 temp temp1 $ r1_2 $   time1_3 temp
		#4 temp temp1 $ r1_3 $   time1_4 temp
		#5 temp temp1 $ r1_4 $   time1_5 temp
        #6 zeile2 temp1 $ cnd2 $  time2_1 temp 
        #7 temp temp1 $ r2_1 $   time2_2 temp
 	    #8 temp temp1 $ r2_2 $   time2_3 temp
		#9 temp temp1 $ r2_3 $   time2_4 temp
		#10 temp temp1 $ r2_4 $   time2_5 temp
; 
DROP temp1 temp; 
RUN; 

DATA switch.temp2;
INFILE 'c:\switch\rohdat\vp10_exp2.log';
INPUT   #1 zeile1 temp1 $ cnd1 $  time1_1 temp 
        #2 temp temp1 $ r1_1 $   time1_2 temp
 	    #3 temp temp1 $ r1_2 $   time1_3 temp
		#4 temp temp1 $ r1_3 $   time1_4 temp
		#5 temp temp1 $ r1_4 $   time1_5 temp
        #6 zeile2 temp1 $ cnd2 $  time2_1 temp 
        #7 temp temp1 $ r2_1 $   time2_2 temp
 	    #8 temp temp1 $ r2_2 $   time2_3 temp
		#9 temp temp1 $ r2_3 $   time2_4 temp
		#10 temp temp1 $ r2_4 $   time2_5 temp
; 
DROP temp1 temp; 
RUN; 

DATA switch.temp3; 
SET switch.temp1 switch.temp2; 
RUN; 

DATA switch.data; 
SET switch.temp3; 

if cnd1 = 'Break' then delete;
if cnd2 = 'Break' then delete; 

/* Blockbedingung feststellen */
if cnd1 = 'mVHliFau' then block ='mixed'; 
if cnd1 = 'mVHliFin' then block ='mixed';
if cnd1 = 'mVHreFin' then block ='mixed';
if cnd1 = 'mVHreFau' then block ='mixed';
if cnd1 = 'mAHliFau' then block ='mixed';
if cnd1 = 'mAHliFin' then block ='mixed';
if cnd1 = 'mAHreFin' then block ='mixed';
if cnd1 = 'mAHreFau' then block ='mixed';

if cnd1 = 'VHliFau' then block ='singl'; 
if cnd1 = 'VHliFin' then block ='singl';
if cnd1 = 'VHreFin' then block ='singl';
if cnd1 = 'VHreFau' then block ='singl';
if cnd1 = 'AHliFau' then block ='singl';
if cnd1 = 'AHliFin' then block ='singl';
if cnd1 = 'AHreFin' then block ='singl';
if cnd1 = 'AHreFau' then block ='singl';
 
/* action conntion feststellen */

if cnd1 = 'mVHliFau' then action1 = 'ima'; 
if cnd1 = 'mVHliFin' then action1 = 'ima';
if cnd1 = 'mVHreFin' then action1 = 'ima';
if cnd1 = 'mVHreFau' then action1 = 'ima';
if cnd1 = 'mAHliFau' then action1 = 'exe';
if cnd1 = 'mAHliFin' then action1 = 'exe';
if cnd1 = 'mAHreFin' then action1 = 'exe';
if cnd1 = 'mAHreFau' then action1 = 'exe';

if cnd1 = 'VHliFau' then action1 = 'ima'; 
if cnd1 = 'VHliFin' then action1 = 'ima';
if cnd1 = 'VHreFin' then action1 = 'ima';
if cnd1 = 'VHreFau' then action1 = 'ima';
if cnd1 = 'AHliFau' then action1 = 'exe';
if cnd1 = 'AHliFin' then action1 = 'exe';
if cnd1 = 'AHreFin' then action1 = 'exe';
if cnd1 = 'AHreFau' then action1 = 'exe';

if cnd2 = 'mVHliFau' then action2 = 'ima'; 
if cnd2 = 'mVHliFin' then action2 = 'ima';
if cnd2 = 'mVHreFin' then action2 = 'ima';
if cnd2 = 'mVHreFau' then action2 = 'ima';
if cnd2 = 'mAHliFau' then action2 = 'exe';
if cnd2 = 'mAHliFin' then action2 = 'exe';
if cnd2 = 'mAHreFin' then action2 = 'exe';
if cnd2 = 'mAHreFau' then action2 = 'exe';

if cnd2 = 'VHliFau' then action2 = 'ima'; 
if cnd2 = 'VHliFin' then action2 = 'ima';
if cnd2 = 'VHreFin' then action2 = 'ima';
if cnd2 = 'VHreFau' then action2 = 'ima';
if cnd2 = 'AHliFau' then action2 = 'exe';
if cnd2 = 'AHliFin' then action2 = 'exe';
if cnd2 = 'AHreFin' then action2 = 'exe';
if cnd2 = 'AHreFau' then action2 = 'exe';

/* tatsächeliche action condition */
actact1 = 'exe'; 
actact2 = 'exe'; 
if r1_2 = 'default' then actact1 = 'ima';
if r2_2 = 'default' then actact2 = 'ima';

if action1 = 'ima' and action2 = 'ima' then act_ord = 'imaima';
if action1 = 'exe' and action2 = 'exe' then act_ord = 'exeexe'; 
if action1 = 'ima' and action2 = 'exe' then act_ord = 'imaexe';
if action1 = 'exe' and action2 = 'ima' then act_ord = 'exeima'; 


/*compare action conditions */
/* codes:  1 = correct; 2 = execution instead of imagery; 3 = imagery instead of execution */

if action1 = 'ima' and actact1 = 'ima' then actcomp1 =1;
if action2 = 'ima' and actact2 = 'ima' then actcomp2 =1; 
if action1 = 'exe' and actact1 = 'exe' then actcomp1 =1;
if action2 = 'exe' and actact2 = 'exe' then actcomp2 =1; 

if action1 = 'ima' and actact1 = 'exe' then actcomp1 =2;
if action2 = 'ima' and actact2 = 'exe' then actcomp2 =2; 

if action1 = 'exe' and actact1 = 'ima' then actcomp1 =3;
if action2 = 'exe' and actact2 = 'ima' then actcomp2 =3; 



/* Bedingungen definieren - Hand */
hand = 'diff'; 
if cnd1 = 'mAHliFau' and cnd2 = 'mAHliFau' then hand = 'same'; 
if cnd1 = 'mAHliFau' and cnd2 = 'mAHliFin' then hand = 'same'; 
if cnd1 = 'mAHliFau' and cnd2 = 'mVHliFau' then hand = 'same'; 
if cnd1 = 'mAHliFau' and cnd2 = 'mVHliFin' then hand = 'same'; 

if cnd1 = 'mAHliFin' and cnd2 = 'mAHliFau' then hand = 'same'; 
if cnd1 = 'mAHliFin' and cnd2 = 'mAHliFin' then hand = 'same'; 
if cnd1 = 'mAHliFin' and cnd2 = 'mVHliFau' then hand = 'same'; 
if cnd1 = 'mAHliFin' and cnd2 = 'mVHliFin' then hand = 'same'; 

if cnd1 = 'mVHliFau' and cnd2 = 'mAHliFau' then hand = 'same'; 
if cnd1 = 'mVHliFau' and cnd2 = 'mAHliFin' then hand = 'same'; 
if cnd1 = 'mVHliFau' and cnd2 = 'mVHliFau' then hand = 'same'; 
if cnd1 = 'mVHliFau' and cnd2 = 'mVHliFin' then hand = 'same'; 

if cnd1 = 'mVHliFin' and cnd2 = 'mAHliFau' then hand = 'same'; 
if cnd1 = 'mVHliFin' and cnd2 = 'mAHliFin' then hand = 'same'; 
if cnd1 = 'mVHliFin' and cnd2 = 'mVHliFau' then hand = 'same'; 
if cnd1 = 'mVHliFin' and cnd2 = 'mVHliFin' then hand = 'same'; 


if cnd1 = 'mAHreFau' and cnd2 = 'mAHreFau' then hand = 'same'; 
if cnd1 = 'mAHreFau' and cnd2 = 'mAHreFin' then hand = 'same'; 
if cnd1 = 'mAHreFau' and cnd2 = 'mVHreFau' then hand = 'same'; 
if cnd1 = 'mAHreFau' and cnd2 = 'mVHreFin' then hand = 'same'; 

if cnd1 = 'mAHreFin' and cnd2 = 'mAHreFau' then hand = 'same'; 
if cnd1 = 'mAHreFin' and cnd2 = 'mAHreFin' then hand = 'same'; 
if cnd1 = 'mAHreFin' and cnd2 = 'mVHreFau' then hand = 'same'; 
if cnd1 = 'mAHreFin' and cnd2 = 'mVHreFin' then hand = 'same'; 

if cnd1 = 'mVHreFau' and cnd2 = 'mAHreFau' then hand = 'same'; 
if cnd1 = 'mVHreFau' and cnd2 = 'mAHreFin' then hand = 'same'; 
if cnd1 = 'mVHreFau' and cnd2 = 'mVHreFau' then hand = 'same'; 
if cnd1 = 'mVHreFau' and cnd2 = 'mVHreFin' then hand = 'same'; 

if cnd1 = 'mVHreFin' and cnd2 = 'mAHreFau' then hand = 'same'; 
if cnd1 = 'mVHreFin' and cnd2 = 'mAHreFin' then hand = 'same'; 
if cnd1 = 'mVHreFin' and cnd2 = 'mVHreFau' then hand = 'same'; 
if cnd1 = 'mVHreFin' and cnd2 = 'mVHreFin' then hand = 'same'; 


if cnd1 = 'AHliFau' and cnd2 = 'AHliFau' then hand = 'same'; 
if cnd1 = 'AHliFau' and cnd2 = 'AHliFin' then hand = 'same'; 

if cnd1 = 'AHliFin' and cnd2 = 'AHliFau' then hand = 'same'; 
if cnd1 = 'AHliFin' and cnd2 = 'AHliFin' then hand = 'same'; 

if cnd1 = 'VHliFau' and cnd2 = 'VHliFau' then hand = 'same'; 
if cnd1 = 'VHliFau' and cnd2 = 'VHliFin' then hand = 'same'; 

if cnd1 = 'VHliFin' and cnd2 = 'VHliFau' then hand = 'same'; 
if cnd1 = 'VHliFin' and cnd2 = 'VHliFin' then hand = 'same'; 


if cnd1 = 'AHreFau' and cnd2 = 'AHreFau' then hand = 'same'; 
if cnd1 = 'AHreFau' and cnd2 = 'AHreFin' then hand = 'same'; 

if cnd1 = 'AHreFin' and cnd2 = 'AHreFau' then hand = 'same'; 
if cnd1 = 'AHreFin' and cnd2 = 'AHreFin' then hand = 'same'; 

if cnd1 = 'VHreFau' and cnd2 = 'VHreFau' then hand = 'same'; 
if cnd1 = 'VHreFau' and cnd2 = 'VHreFin' then hand = 'same'; 

if cnd1 = 'VHreFin' and cnd2 = 'VHreFau' then hand = 'same'; 
if cnd1 = 'VHreFin' and cnd2 = 'VHreFin' then hand = 'same'; 

/* Bedingungen definieren - Richtung */

if cnd1 = 'mVHliFau' then richtung1 = 'au'; 
if cnd1 = 'mVHliFin' then richtung1 = 'in'; 
if cnd1 = 'mVHreFau' then richtung1 = 'au'; 
if cnd1 = 'mVHreFin' then richtung1 = 'in'; 
 
if cnd1 = 'mAHliFau' then richtung1 = 'au';
if cnd1 = 'mAHliFin' then richtung1 = 'in'; 
if cnd1 = 'mAHreFau' then richtung1 = 'au';
if cnd1 = 'mAHreFin' then richtung1 = 'in'; 

if cnd1 = 'VHliFau' then richtung1 = 'au'; 
if cnd1 = 'VHliFin' then richtung1 = 'in'; 
if cnd1 = 'VHreFau' then richtung1 = 'au'; 
if cnd1 = 'VHreFin' then richtung1 = 'in'; 
 
if cnd1 = 'AHliFau' then richtung1 = 'au';
if cnd1 = 'AHliFin' then richtung1 = 'in'; 
if cnd1 = 'AHreFau' then richtung1 = 'au';
if cnd1 = 'AHreFin' then richtung1 = 'in';


if cnd2 = 'mVHliFau' then richtung2 = 'au'; 
if cnd2 = 'mVHliFin' then richtung2 = 'in'; 
if cnd2 = 'mVHreFau' then richtung2 = 'au'; 
if cnd2 = 'mVHreFin' then richtung2 = 'in'; 
 
if cnd2 = 'mAHliFau' then richtung2 = 'au';
if cnd2 = 'mAHliFin' then richtung2 = 'in'; 
if cnd2 = 'mAHreFau' then richtung2 = 'au';
if cnd2 = 'mAHreFin' then richtung2 = 'in'; 

if cnd2 = 'VHliFau' then richtung2 = 'au'; 
if cnd2 = 'VHliFin' then richtung2 = 'in'; 
if cnd2 = 'VHreFau' then richtung2 = 'au'; 
if cnd2 = 'VHreFin' then richtung2 = 'in'; 
 
if cnd2 = 'AHliFau' then richtung2 = 'au';
if cnd2 = 'AHliFin' then richtung2 = 'in'; 
if cnd2 = 'AHreFau' then richtung2 = 'au';
if cnd2 = 'AHreFin' then richtung2 = 'in';

if richtung1 = 'au' and richtung2 = 'au' then richtung = 'same'; 
if richtung1 = 'in' and richtung2 = 'in' then richtung = 'same'; 
if richtung1 = 'au' and richtung2 = 'in' then richtung = 'diff'; 
if richtung1 = 'in' and richtung2 = 'au' then richtung = 'diff'; 


/* correct und incorrect definieren */
/* korrekte definieren */
corr1=0; 
corr2=0;

/* codes:  1 = correct; 2 = execution instead of imagery; 3 = imagery instead of execution */
/* erstes trials */
if cnd1 = 'mAHliFau' and r1_1 = '2' and r1_2 = '5' and r1_3 = '6' and r1_4 = '1' then corr1 = 1; 
if cnd1 = 'mAHliFin' and r1_1 = '2' and r1_2 = '7' and r1_3 = '8'  and r1_4 = '1' then corr1 = 1; 
if cnd1 = 'mAHreFau' and r1_1 = '4' and r1_2 = '11' and r1_3 = '12' and r1_4 = '3' then corr1 = 1; 
if cnd1 = 'mAHreFin' and r1_1 = '4' and r1_2 = '9' and r1_3 = '10' and r1_4 = '3' then corr1 = 1;

if cnd1 = 'AHliFau' and r1_1 = '2' and r1_2 = '5' and r1_3 = '6' and r1_4 = '1' then corr1 = 1; 
if cnd1 = 'AHliFin' and r1_1 = '2' and r1_2 = '7' and r1_3 = '8'  and r1_4 = '1' then corr1 = 1; 
if cnd1 = 'AHreFau' and r1_1 = '4' and r1_2 = '11' and r1_3 = '12' and r1_4 = '3' then corr1 = 1; 
if cnd1 = 'AHreFin' and r1_1 = '4' and r1_2 = '9' and r1_3 = '10' and r1_4 = '3' then corr1 = 1;


if cnd1 = 'mVHliFau' and r1_1 = 'default' and r1_2 = 'default' and r1_3 = '2' and r1_4 = '1' then corr1 = 1; 
if cnd1 = 'mVHliFin' and r1_1 = 'default' and r1_2 = 'default' and r1_3 = '2'  and r1_4 = '1' then corr1 = 1; 
if cnd1 = 'mVHreFau' and r1_1 = 'default' and r1_2 = 'default' and r1_3 = '4' and r1_4 = '3' then corr1 = 1; 
if cnd1 = 'mVHreFin' and r1_1 = 'default' and r1_2 = 'default' and r1_3 = '4' and r1_4 = '3' then corr1 = 1;

if cnd1 = 'VHliFau' and r1_1 = 'default' and r1_2 = 'default' and r1_3 = '2' and r1_4 = '1' then corr1 = 1; 
if cnd1 = 'VHliFin' and r1_1 = 'default' and r1_2 = 'default' and r1_3 = '2'  and r1_4 = '1' then corr1 = 1; 
if cnd1 = 'VHreFau' and r1_1 = 'default' and r1_2 = 'default' and r1_3 = '4' and r1_4 = '3' then corr1 = 1; 
if cnd1 = 'VHreFin' and r1_1 = 'default' and r1_2 = 'default' and r1_3 = '4' and r1_4 = '3' then corr1 = 1;


if cnd1 = 'mVHliFau' and r1_1 = '2' and r1_2 = '5' and r1_3 = '6' and r1_4 = '1' then corr1 = 2; 
if cnd1 = 'mVHliFin' and r1_1 = '2' and r1_2 = '7' and r1_3 = '8'  and r1_4 = '1' then corr1 = 2; 
if cnd1 = 'mVHreFau' and r1_1 = '4' and r1_2 = '11' and r1_3 = '12' and r1_4 = '3' then corr1 = 2; 
if cnd1 = 'mVHreFin' and r1_1 = '4' and r1_2 = '9' and r1_3 = '10' and r1_4 = '3' then corr1 = 2;

if cnd1 = 'VHliFau' and r1_1 = '2' and r1_2 = '5' and r1_3 = '6' and r1_4 = '1' then corr1 = 2; 
if cnd1 = 'VHliFin' and r1_1 = '2' and r1_2 = '7' and r1_3 = '8'  and r1_4 = '1' then corr1 = 2; 
if cnd1 = 'VHreFau' and r1_1 = '4' and r1_2 = '11' and r1_3 = '12' and r1_4 = '3' then corr1 = 2; 
if cnd1 = 'VHreFin' and r1_1 = '4' and r1_2 = '9' and r1_3 = '10' and r1_4 = '3' then corr1 = 2;


if cnd1 = 'mAHliFau' and r1_1 = 'default' and r1_2 = 'default' and r1_3 = '2' and r1_4 = '1' then corr1 = 3; 
if cnd1 = 'mAHliFin' and r1_1 = 'default' and r1_2 = 'default' and r1_3 = '2'  and r1_4 = '1' then corr1 = 3; 
if cnd1 = 'mAHreFau' and r1_1 = 'default' and r1_2 = 'default' and r1_3 = '4' and r1_4 = '3' then corr1 = 3; 
if cnd1 = 'mAHreFin' and r1_1 = 'default' and r1_2 = 'default' and r1_3 = '4' and r1_4 = '3' then corr1 = 3;

if cnd1 = 'AHliFau' and r1_1 = 'default' and r1_2 = 'default' and r1_3 = '2' and r1_4 = '1' then corr1 = 3; 
if cnd1 = 'AHliFin' and r1_1 = 'default' and r1_2 = 'default' and r1_3 = '2'  and r1_4 = '1' then corr1 = 3; 
if cnd1 = 'AHreFau' and r1_1 = 'default' and r1_2 = 'default' and r1_3 = '4' and r1_4 = '3' then corr1 = 3; 
if cnd1 = 'AHreFin' and r1_1 = 'default' and r1_2 = 'default' and r1_3 = '4' and r1_4 = '3' then corr1 = 3;

/* zweites trials */
if cnd2 = 'mAHliFau' and r2_1 = '2' and r2_2 = '5' and r2_3 = '6' and r2_4 = '1' then corr2 = 1; 
if cnd2 = 'mAHliFin' and r2_1 = '2' and r2_2 = '7' and r2_3 = '8'  and r2_4 = '1' then corr2 = 1; 
if cnd2 = 'mAHreFau' and r2_1 = '4' and r2_2 = '11' and r2_3 = '12' and r2_4 = '3' then corr2 = 1; 
if cnd2 = 'mAHreFin' and r2_1 = '4' and r2_2 = '9' and r2_3 = '10' and r2_4 = '3' then corr2 = 1;

if cnd2 = 'AHliFau' and r2_1 = '2' and r2_2 = '5' and r2_3 = '6' and r2_4 = '1' then corr2 = 1; 
if cnd2 = 'AHliFin' and r2_1 = '2' and r2_2 = '7' and r2_3 = '8'  and r2_4 = '1' then corr2 = 1; 
if cnd2 = 'AHreFau' and r2_1 = '4' and r2_2 = '11' and r2_3 = '12' and r2_4 = '3' then corr2 = 1; 
if cnd2 = 'AHreFin' and r2_1 = '4' and r2_2 = '9' and r2_3 = '10' and r2_4 = '3' then corr2 = 1;


if cnd2 = 'mVHliFau' and r2_1 = 'default' and r2_2 = 'default' and r2_3 = '2' and r2_4 = '1' then corr2 = 1; 
if cnd2 = 'mVHliFin' and r2_1 = 'default' and r2_2 = 'default' and r2_3 = '2'  and r2_4 = '1' then corr2 = 1; 
if cnd2 = 'mVHreFau' and r2_1 = 'default' and r2_2 = 'default' and r2_3 = '4' and r2_4 = '3' then corr2 = 1; 
if cnd2 = 'mVHreFin' and r2_1 = 'default' and r2_2 = 'default' and r2_3 = '4' and r2_4 = '3' then corr2 = 1;

if cnd2 = 'VHliFau' and r2_1 = 'default' and r2_2 = 'default' and r2_3 = '2' and r2_4 = '1' then corr2 = 1; 
if cnd2 = 'VHliFin' and r2_1 = 'default' and r2_2 = 'default' and r2_3 = '2'  and r2_4 = '1' then corr2 = 1; 
if cnd2 = 'VHreFau' and r2_1 = 'default' and r2_2 = 'default' and r2_3 = '4' and r2_4 = '3' then corr2 = 1; 
if cnd2 = 'VHreFin' and r2_1 = 'default' and r2_2 = 'default' and r2_3 = '4' and r2_4 = '3' then corr2 = 1;


if cnd2 = 'mVHliFau' and r2_1 = '2' and r2_2 = '5' and r2_3 = '6' and r2_4 = '1' then corr2 = 2; 
if cnd2 = 'mVHliFin' and r2_1 = '2' and r2_2 = '7' and r2_3 = '8'  and r2_4 = '1' then corr2 = 2; 
if cnd2 = 'mVHreFau' and r2_1 = '4' and r2_2 = '11' and r2_3 = '12' and r2_4 = '3' then corr2 = 2; 
if cnd2 = 'mVHreFin' and r2_1 = '4' and r2_2 = '9' and r2_3 = '10' and r2_4 = '3' then corr2 = 2;

if cnd2 = 'VHliFau' and r2_1 = '2' and r2_2 = '5' and r2_3 = '6' and r2_4 = '1' then corr2 = 2; 
if cnd2 = 'VHliFin' and r2_1 = '2' and r2_2 = '7' and r2_3 = '8'  and r2_4 = '1' then corr2 = 2; 
if cnd2 = 'VHreFau' and r2_1 = '4' and r2_2 = '11' and r2_3 = '12' and r2_4 = '3' then corr2 = 2; 
if cnd2 = 'VHreFin' and r2_1 = '4' and r2_2 = '9' and r2_3 = '10' and r2_4 = '3' then corr2 = 2;

corr = 0; 
if corr1 = 1 and corr2 = 1 then corr =1; 

if action2 = 'exe' then rt =  (time2_2 - time2_1)/10;
if action2 = 'exe' then mt1 = (time2_3 - time2_2)/10; 
if action2 = 'exe' then rest = (time2_4 - time2_3)/10; 
if action2 = 'exe' then mt2 = (time2_5 - time2_4)/10; 
if action2 = 'exe' then mt = mt1 + rest + mt2; 

if action2 = 'ima' then rt =  (time2_4 - time2_1)/10;
if action2 = 'ima' then mt = (time2_5 - time2_4)/10; 



RUN; 
PROC PRINT; RUN; 

/* -------------------------------------------------------------- */
/* Kennwerte berechnen                                            */
/* -------------------------------------------------------------- */

/* -------------------------------------------------------------- */
/* korrekt inkorrekt                                              */


DATA switch.temp3; 
SET switch.data; 
if block = 'mixed' and corr = 1 then valid = 1; 
else valid = 0;  
if valid = 0 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res1  N=NM_corec;
RUN;


DATA switch.temp3; 
SET switch.data; 
if block = 'mixed' and corr = 0 then valid = 1; 
else valid = 0;  
if valid = 0 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res2  N=NM_incor;
RUN;


DATA switch.temp3; 
SET switch.data; 
if block = 'singl' and corr = 1 then valid = 1; 
else valid = 0;  
if valid = 0 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res3  N=NS_corec;
RUN;


DATA switch.temp3; 
SET switch.data; 
if block = 'singl' and corr = 0 then valid = 1; 
else valid = 0;  
if valid = 0 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res4  N=NS_incor;
RUN;

/* -------------------------------------------------------------- */
/* falsche action                                                 */

DATA switch.temp3; 
SET switch.data; 
if block = 'mixed' and actcomp2 = 2 then valid = 1; 
else valid = 0;  
if valid = 0 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res5  N=NM_ima_statt_exe;
RUN;

DATA switch.temp3; 
SET switch.data; 
if block = 'mixed' and actcomp2 = 3 then valid = 1; 
else valid = 0;  
if valid = 0 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res6  N=NM_exe_statt_ima;
RUN;

DATA switch.temp3; 
SET switch.data; 
if block = 'singl' and actcomp2 = 2 then valid = 1; 
else valid = 0;  
if valid = 0 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res7  N=NS_ima_statt_exe;
RUN;

DATA switch.temp3; 
SET switch.data; 
if block = 'singl' and actcomp2 = 3 then valid = 1; 
else valid = 0;  
if valid = 0 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res8  N=NS_exe_statt_ima;
RUN;


/* -------------------------------------------------------------- */
/* RT mixed blocks                                                */

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'imaima' and corr = 1 and hand = 'same' and richtung = 'same' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zRT = RT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zRT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zRT > 3 then delete;
if zRT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res11  N=NM_II_hsrs MEAN=RTM_II_hsrs ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'imaima' and corr = 1 and hand = 'same' and richtung = 'diff' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zRT = RT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zRT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zRT > 3 then delete;
if zRT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res12  N=NM_II_hsrd MEAN=RTM_II_hsrd ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'imaima' and corr = 1 and hand = 'diff' and richtung = 'same' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zRT = RT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zRT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zRT > 3 then delete;
if zRT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res13  N=NM_II_hdrs MEAN=RTM_II_hdrs ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'imaima' and corr = 1 and hand = 'diff' and richtung = 'diff' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zRT = RT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zRT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zRT > 3 then delete;
if zRT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res14  N=NM_II_hdrd MEAN=RTM_II_hdrd ;
RUN;


DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'exeexe' and corr = 1 and hand = 'same' and richtung = 'same' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zRT = RT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zRT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zRT > 3 then delete;
if zRT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res15  N=NM_EE_hsrs MEAN=RTM_EE_hsrs ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'exeexe' and corr = 1 and hand = 'same' and richtung = 'diff' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zRT = RT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zRT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zRT > 3 then delete;
if zRT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res16  N=NM_EE_hsrd MEAN=RTM_EE_hsrd ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'exeexe' and corr = 1 and hand = 'diff' and richtung = 'same' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zRT = RT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zRT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zRT > 3 then delete;
if zRT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res17  N=NM_EE_hdrs MEAN=RTM_EE_hdrs ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'exeexe' and corr = 1 and hand = 'diff' and richtung = 'diff' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zRT = RT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zRT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zRT > 3 then delete;
if zRT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res18  N=NM_EE_hdrd MEAN=RTM_EE_hdrd ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'imaexe' and corr = 1 and hand = 'same' and richtung = 'same' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zRT = RT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zRT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zRT > 3 then delete;
if zRT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res21  N=NM_IE_hsrs MEAN=RTM_IE_hsrs ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'imaexe' and corr = 1 and hand = 'same' and richtung = 'diff' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zRT = RT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zRT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zRT > 3 then delete;
if zRT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res22  N=NM_IE_hsrd MEAN=RTM_IE_hsrd ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'imaexe' and corr = 1 and hand = 'diff' and richtung = 'same' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zRT = RT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zRT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zRT > 3 then delete;
if zRT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res23  N=NM_IE_hdrs MEAN=RTM_IE_hdrs ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'imaexe' and corr = 1 and hand = 'diff' and richtung = 'diff' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zRT = RT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zRT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zRT > 3 then delete;
if zRT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res24  N=NM_IE_hdrd MEAN=RTM_IE_hdrd ;
RUN;


DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'exeima' and corr = 1 and hand = 'same' and richtung = 'same' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zRT = RT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zRT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zRT > 3 then delete;
if zRT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res25  N=NM_EI_hsrs MEAN=RTM_EI_hsrs ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'exeima' and corr = 1 and hand = 'same' and richtung = 'diff' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zRT = RT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zRT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zRT > 3 then delete;
if zRT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res26  N=NM_EI_hsrd MEAN=RTM_EI_hsrd ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'exeima' and corr = 1 and hand = 'diff' and richtung = 'same' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zRT = RT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zRT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zRT > 3 then delete;
if zRT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res27  N=NM_EI_hdrs MEAN=RTM_EI_hdrs ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'exeima' and corr = 1 and hand = 'diff' and richtung = 'diff' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zRT = RT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zRT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zRT > 3 then delete;
if zRT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res28  N=NM_EI_hdrd MEAN=RTM_EI_hdrd ;
RUN;



/* -------------------------------------------------------------- */
/* RT single blocks                                                */

DATA switch.temp2; 
SET switch.data; 
If block = 'singl' and act_ord = 'imaima' and corr = 1 and hand = 'same' and richtung = 'same' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zRT = RT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zRT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zRT > 3 then delete;
if zRT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res31  N=NS_II_hsrs MEAN=RTS_II_hsrs ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'singl' and act_ord = 'imaima' and corr = 1 and hand = 'same' and richtung = 'diff' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zRT = RT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zRT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zRT > 3 then delete;
if zRT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res32  N=NS_II_hsrd MEAN=RTS_II_hsrd ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'singl' and act_ord = 'imaima' and corr = 1 and hand = 'diff' and richtung = 'same' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zRT = RT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zRT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zRT > 3 then delete;
if zRT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res33  N=NS_II_hdrs MEAN=RTS_II_hdrs ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'singl' and act_ord = 'imaima' and corr = 1 and hand = 'diff' and richtung = 'diff' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zRT = RT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zRT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zRT > 3 then delete;
if zRT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res34  N=NS_II_hdrd MEAN=RTS_II_hdrd ;
RUN;


DATA switch.temp2; 
SET switch.data; 
If block = 'singl' and act_ord = 'exeexe' and corr = 1 and hand = 'same' and richtung = 'same' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zRT = RT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zRT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zRT > 3 then delete;
if zRT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res35  N=NS_EE_hsrs MEAN=RTS_EE_hsrs ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'singl' and act_ord = 'exeexe' and corr = 1 and hand = 'same' and richtung = 'diff' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zRT = RT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zRT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zRT > 3 then delete;
if zRT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res36  N=NS_EE_hsrd MEAN=RTS_EE_hsrd ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'singl' and act_ord = 'exeexe' and corr = 1 and hand = 'diff' and richtung = 'same' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zRT = RT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zRT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zRT > 3 then delete;
if zRT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res37  N=NS_EE_hdrs MEAN=RTS_EE_hdrs ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'singl' and act_ord = 'exeexe' and corr = 1 and hand = 'diff' and richtung = 'diff' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zRT = RT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zRT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zRT > 3 then delete;
if zRT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR rt;
     OUTPUT OUT=switch.res38  N=NS_EE_hdrd MEAN=RTS_EE_hdrd ;
RUN;


/* -------------------------------------------------------------- */
/* MT mixed blocks                                                */

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'imaima' and corr = 1 and hand = 'same' and richtung = 'same' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zMT = MT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zMT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zMT > 3 then delete;
if zMT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR mt;
     OUTPUT OUT=switch.res111 MEAN=MTM_II_hsrs ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'imaima' and corr = 1 and hand = 'same' and richtung = 'diff' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zMT = MT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zMT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zMT > 3 then delete;
if zMT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR mt;
     OUTPUT OUT=switch.res112 MEAN=MTM_II_hsrd ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'imaima' and corr = 1 and hand = 'diff' and richtung = 'same' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zMT = MT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zMT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zMT > 3 then delete;
if zMT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR mt;
     OUTPUT OUT=switch.res113 MEAN=MTM_II_hdrs ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'imaima' and corr = 1 and hand = 'diff' and richtung = 'diff' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zMT = MT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zMT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zMT > 3 then delete;
if zMT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR mt;
     OUTPUT OUT=switch.res114  MEAN=MTM_II_hdrd ;
RUN;


DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'exeexe' and corr = 1 and hand = 'same' and richtung = 'same' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zMT = MT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zMT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zMT > 3 then delete;
if zMT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR mt;
     OUTPUT OUT=switch.res115  MEAN=MTM_EE_hsrs ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'exeexe' and corr = 1 and hand = 'same' and richtung = 'diff' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zMT = MT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zMT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zMT > 3 then delete;
if zMT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR mt;
     OUTPUT OUT=switch.res116  MEAN=MTM_EE_hsrd ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'exeexe' and corr = 1 and hand = 'diff' and richtung = 'same' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zMT = MT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zMT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zMT > 3 then delete;
if zMT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR mt;
     OUTPUT OUT=switch.res117  MEAN=MTM_EE_hdrs ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'exeexe' and corr = 1 and hand = 'diff' and richtung = 'diff' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zMT = MT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zMT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zMT > 3 then delete;
if zMT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR mt;
     OUTPUT OUT=switch.res118  MEAN=MTM_EE_hdrd ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'imaexe' and corr = 1 and hand = 'same' and richtung = 'same' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zMT = MT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zMT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zMT > 3 then delete;
if zMT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR mt;
     OUTPUT OUT=switch.res121  MEAN=MTM_IE_hsrs ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'imaexe' and corr = 1 and hand = 'same' and richtung = 'diff' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zMT = MT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zMT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zMT > 3 then delete;
if zMT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR mt;
     OUTPUT OUT=switch.res122  MEAN=MTM_IE_hsrd ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'imaexe' and corr = 1 and hand = 'diff' and richtung = 'same' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zMT = MT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zMT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zMT > 3 then delete;
if zMT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR mt;
     OUTPUT OUT=switch.res123  MEAN=MTM_IE_hdrs ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'imaexe' and corr = 1 and hand = 'diff' and richtung = 'diff' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zMT = MT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zMT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zMT > 3 then delete;
if zMT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR mt;
     OUTPUT OUT=switch.res124  MEAN=MTM_IE_hdrd ;
RUN;


DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'exeima' and corr = 1 and hand = 'same' and richtung = 'same' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zMT = MT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zMT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zMT > 3 then delete;
if zMT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR mt;
     OUTPUT OUT=switch.res125  MEAN=MTM_EI_hsrs ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'exeima' and corr = 1 and hand = 'same' and richtung = 'diff' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zMT = MT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zMT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zMT > 3 then delete;
if zMT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR mt;
     OUTPUT OUT=switch.res126  MEAN=MTM_EI_hsrd ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'exeima' and corr = 1 and hand = 'diff' and richtung = 'same' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zMT = MT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zMT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zMT > 3 then delete;
if zMT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR mt;
     OUTPUT OUT=switch.res127  MEAN=MTM_EI_hdrs ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'mixed' and act_ord = 'exeima' and corr = 1 and hand = 'diff' and richtung = 'diff' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zMT = MT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zMT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zMT > 3 then delete;
if zMT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR mt;
     OUTPUT OUT=switch.res128  MEAN=MTM_EI_hdrd ;
RUN;



/* -------------------------------------------------------------- */
/* MT single blocks                                                */

DATA switch.temp2; 
SET switch.data; 
If block = 'singl' and act_ord = 'imaima' and corr = 1 and hand = 'same' and richtung = 'same' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zMT = MT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zMT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zMT > 3 then delete;
if zMT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR mt;
     OUTPUT OUT=switch.res131  MEAN=MTS_II_hsrs ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'singl' and act_ord = 'imaima' and corr = 1 and hand = 'same' and richtung = 'diff' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zMT = MT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zMT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zMT > 3 then delete;
if zMT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR mt;
     OUTPUT OUT=switch.res132  MEAN=MTS_II_hsrd ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'singl' and act_ord = 'imaima' and corr = 1 and hand = 'diff' and richtung = 'same' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zMT = MT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zMT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zMT > 3 then delete;
if zMT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR mt;
     OUTPUT OUT=switch.res133  MEAN=MTS_II_hdrs ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'singl' and act_ord = 'imaima' and corr = 1 and hand = 'diff' and richtung = 'diff' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zMT = MT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zMT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zMT > 3 then delete;
if zMT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR mt;
     OUTPUT OUT=switch.res134  MEAN=MTS_II_hdrd ;
RUN;


DATA switch.temp2; 
SET switch.data; 
If block = 'singl' and act_ord = 'exeexe' and corr = 1 and hand = 'same' and richtung = 'same' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zMT = MT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zMT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zMT > 3 then delete;
if zMT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR mt;
     OUTPUT OUT=switch.res135  MEAN=MTS_EE_hsrs ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'singl' and act_ord = 'exeexe' and corr = 1 and hand = 'same' and richtung = 'diff' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zMT = MT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zMT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zMT > 3 then delete;
if zMT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR mt;
     OUTPUT OUT=switch.res136  MEAN=MTS_EE_hsrd ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'singl' and act_ord = 'exeexe' and corr = 1 and hand = 'diff' and richtung = 'same' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zMT = MT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zMT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zMT > 3 then delete;
if zMT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR mt;
     OUTPUT OUT=switch.res137  MEAN=MTS_EE_hdrs ;
RUN;

DATA switch.temp2; 
SET switch.data; 
If block = 'singl' and act_ord = 'exeexe' and corr = 1 and hand = 'diff' and richtung = 'diff' then valid = 1; 
else valid = 0; 
If valid = 0 then delete; 
zMT = MT; 
RUN; 
PROC STANDARD MEAN = 0 STD = 1 OUT = switch.ztemp1; 
VAR zMT; 
RUN; 
DATA switch.temp2; 
SET switch.ztemp1; 
if zMT > 3 then delete;
if zMT < -3 then delete; 
RUN; 
PROC UNIVARIATE;
     VAR mt;
     OUTPUT OUT=switch.res138  MEAN=MTS_EE_hdrd ;
RUN;


DATA switch.vpnr; 
vpnr = 10; 
RUN; 


DATA switch.vp10; 
MERGE switch.vpnr 
switch.res1 switch.res2 switch.res3 switch.res4 
switch.res5 switch.res6 switch.res7 switch.res8
switch.res11 switch.res12 switch.res13 switch.res14 switch.res15 switch.res16 switch.res17 switch.res18
switch.res21 switch.res22 switch.res23 switch.res24 switch.res25 switch.res26 switch.res27 switch.res28
switch.res31 switch.res32 switch.res33 switch.res34 switch.res35 switch.res36 switch.res37 switch.res38
switch.res111 switch.res112 switch.res113 switch.res114 switch.res115 switch.res116 switch.res117 switch.res118
switch.res121 switch.res122 switch.res123 switch.res124 switch.res125 switch.res126 switch.res127 switch.res128
switch.res131 switch.res132 switch.res133 switch.res134 switch.res135 switch.res136 switch.res137 switch.res138
; 
if NM_ima_statt_exe = . then NM_ima_statt_exe = 0; 
if NM_exe_statt_ima = . then NM_exe_statt_ima = 0;
if NS_ima_statt_exe = . then NS_ima_statt_exe = 0; 
if NS_exe_statt_ima = . then NS_exe_statt_ima = 0;
NM_incor2 = NM_incor-(NM_ima_statt_exe + NM_exe_statt_ima);
NS_incor2 = NS_incor-(NS_ima_statt_exe + NS_exe_statt_ima);
NM_outlier = NM_corec-(NM_II_hsrs + NM_II_hsrd + NM_II_hdrs + NM_II_hdrd + NM_EE_hsrs + NM_EE_hsrd + NM_EE_hdrs + NM_EE_hdrd +
NM_IE_hsrs + NM_IE_hsrd + NM_IE_hdrs + NM_IE_hdrd +NM_EI_hsrs + NM_EI_hsrd + NM_EI_hdrs + NM_EI_hdrd); 
NS_outlier = NS_corec-(NS_II_hsrs + NS_II_hsrd + NS_II_hdrs + NS_II_hdrd + NS_EE_hsrs + NS_EE_hsrd + NS_EE_hdrs + NS_EE_hdrd); 
NM_total = NM_incor+NM_corec; 
NS_total = NS_incor+NS_corec;
RUN; 
PROC PRINT; RUN; 




/* -------------------------------------------------------------- */
/* Erstellung der Versuchspersonendatei                           */
/* -------------------------------------------------------------- */


/* -------------------------------------------------------------- */
/* ENDE                                                           */
/* -------------------------------------------------------------- */
