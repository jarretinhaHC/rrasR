#!/bin/bash

#Download shapefiles
mkdir IBGE
cd IBGE
wget -ndH ftp://geoftp.ibge.gov.br/malhas_digitais/municipio_2010/*

#Uncompress 
for F in *.zip
do
	unzip $F
done

#Copy to a single folder
rm *.zip
rename 'y/a-z/A-Z/' *
mkdir Brazil
cp `find . -name "*.*"` ./Brazil

