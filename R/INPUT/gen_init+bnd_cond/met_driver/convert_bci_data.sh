#!/bin/bash

# WARNING: This script perform the conversion from bci data provided by Tom Powell to ED:script compliant format.
# It is highly tailored to a specific case so don't use on any random file.

# WARNING: Marcos script has a skip=49 lines, we fill up the first 49 lines with usless stuff to leave the script untouched

echo "============================================================="             > filled_bci.txt
echo "File          : filled_bci.txt"                                           >> filled_bci.txt
echo "Header length : 47"                                                       >> filled_bci.txt
echo "Site          : Barro Colorado Island"                                    >> filled_bci.txt
echo "Longitude     : -79.846"                                                  >> filled_bci.txt
echo "Latitude      : 9.154"                                                    >> filled_bci.txt
echo "   "                                                                      >> filled_bci.txt
echo "Updated 11/10/2017 by: "                                                  >> filled_bci.txt
echo "Manfredo di Porcia <manfredo.diporciaebrugnera@ugent.be"                  >> filled_bci.txt
echo " "                                                                        >> filled_bci.txt
echo "Based on the data originally processed by:"                               >> filled_bci.txt
echo "Natalia Coupe  <nataliacoupe@gmail.com>"                                  >> filled_bci.txt
echo "                                            "                             >> filled_bci.txt
echo "   "                                                                      >> filled_bci.txt
echo "Disclaimer: QAQC on these data are a work in progress."                   >> filled_bci.txt
echo "Version for public use subject to changes."                               >> filled_bci.txt
echo "   "                                                                      >> filled_bci.txt
echo "Fair use policy: These are not Harvard data and they are"                 >> filled_bci.txt
echo "subject to the LBA policy.  If you would"                                 >> filled_bci.txt
echo "like to use these data for any research,"                                 >> filled_bci.txt
echo "please contact the PI responsible for the site"                           >> filled_bci.txt
echo "and get an authorisation from them first."                                >> filled_bci.txt
echo "   "                                                                      >> filled_bci.txt
echo "PI: Scott Miller  <smiller@albany.edu>"                                   >> filled_bci.txt
echo "URL: http://daac.ornl.gov/LBA/guides/CD04_Meteorology_Fluxes.html"        >> filled_bci.txt
echo "   "                                                                      >> filled_bci.txt
echo "Note: time stamp is the end of the hour average"                          >> filled_bci.txt
echo "time in UTC"                                                              >> filled_bci.txt
echo "   "                                                                      >> filled_bci.txt
echo "   Variable         Description                  Unit        "            >> filled_bci.txt
echo "  -----------------------------------------------------------"            >> filled_bci.txt
echo "   year             Year                         [       yr]"             >> filled_bci.txt
echo "   month            Month                        [      mon]"             >> filled_bci.txt
echo "   day              Day                          [      day]"             >> filled_bci.txt
echo "   hour             Hour                         [       hr]"             >> filled_bci.txt
echo "   min              Minute                       [      min]"             >> filled_bci.txt
echo "   sec              Second                       [        s]"             >> filled_bci.txt
echo "   atm.prss         Pressure                     [       Pa]"             >> filled_bci.txt
echo "   atm.tmp          Temperature                  [        K]"             >> filled_bci.txt
echo "   atm.shv          Specific humidity            [    kg/kg]"             >> filled_bci.txt
echo "   atm.vels         Wind speed                   [      m/s]"             >> filled_bci.txt
echo "   atm.vdir         Wind direction               [      deg]"             >> filled_bci.txt
echo "   rshort.in        Incoming shortwave radiation [     W/m2]"             >> filled_bci.txt
echo "   rlong.in         Incoming longwave radiation  [     W/m2]"             >> filled_bci.txt
echo "   rain             Rainfall                     [  kg/m2/s]"             >> filled_bci.txt
echo "============================================================="            >> filled_bci.txt
echo "   "                                                                      >> filled_bci.txt
#echo "when,atm.prss,atm.tmp,atm.rhv,atm.vels,atm.vdir,rshort.in,rlong.in,rain"  >> filled_bci.txt
echo "year,month,day,hour,min,sec,atm.prss,atm.tmp,atm.rhv,atm.vels,atm.vdir,rshort.in,rlong.in,rain" >> filled_bci.txt


# Conversion constants
#Pascal2mmHg=0.00750062
awk -F ':|-|,| ' '{if (NR!=1) printf("%s,%s,%s,%s,%s,%s,%f,%f,%s,%s,0.0,%s,NA,%s\n"),$2,$3,$4,$5,$6,$7,$34 / 0.00750062,$28 + 273.16,$30 / 100.0 ,$36,$26,$32 / 3600.0 }' bci_raw.txt >> filled_bci.txt
#awk -F ':|-|,| ' '{if (NR!=1) printf("(%s/%s/%s %s:%s:%s),%f,%f,%s,%s,0.0,%s,NA,%s\n"),$4,$3,$2,$5,$6,$7,$34 / 0.00750062,$28 + 273.16,$30,$36,$26,$32}' bci_raw.txt >> filled_bci.txt

# month=$(head -2 bci.txt | tail -1  | awk -F ':|-|,' '{print $3}')
# day=$(head -2 bci.txt | tail -1  | awk -F ':|-|,' '{print $4}')
# hour=$(head -2 bci.txt | tail -1  | awk -F ':|-|,' '{print $5}')
# min=$(head -2 bci.txt | tail -1  | awk -F ':|-|,' '{print $6}')
# sec=$(head -2 bci.txt | tail -1  | awk -F ':|-|,' '{print $7}')
# atm.prss=$(head -2 bci.txt | tail -1  | awk -F ':|-|,' '{print $30}')
# atm.tmp
# atm.shv
# atm.vels
# atm.vdir
# rshort.in
# rlong.in
# rain







