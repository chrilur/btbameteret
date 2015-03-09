BTBA-METERET
============

Dette er en liten nettside som viser trafikkutviklingen på <a href="http://bt.no" target="blank">bt.no</a> og <a href="http://ba.no" target="blank">ba.no</a> over tid.

R-scriptet btba.R kan kjøres for manuelt å oppdatere uketall fra <a href="http://tnslistene.no/" target="blank">TNS Gallup</a>.

Scriptet genererer en .js-fil og en .csv-fil med data som oppdateres for hver gang R-scriptet mates med nye tall.

R-scriptet laster inn csv-tabellen, legger til og regner ut prosenter, og spytter ut oppdatert csv og oppdatert js.

js-filen brukes deretter til å oppdatere nettsidens <a href="http://www.highcharts.com/" target="blank">Highcharts</a>-grafikker.

