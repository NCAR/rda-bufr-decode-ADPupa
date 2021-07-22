# CONFIGURATION FILE GUIDE
```
IDX VALUES                                  DESCRIPTION
---------------------------------------------------------------------------------------------------------------------------
041 y/n                                     y (default) = gets the report ID, a basic set of parameters and
                                              any extra parameters selected below.
                                            n = gets the report ID, and only those parameters selected below.
041 AAAA BBBB CCCC DDDD ..                  extra parameter mnemonics;  lengths from four to eight characters, 
041 .. WWWW XXXX YYYY ZZZZ                    separated by blanks, up to 75 characters per line, for a list of 
                                              up to 100 mnemonics.  basic mnemonics are: PRLC PSAL GP10 GP07
                                              FLVL VSIG WDIR WSPD TMDB TMDP REHU. NCEP's table of mnemonics is 
                                              available on the web, here:
                                              http://www.emc.ncep.noaa.gov/mmb/data_processing/bufrtab_tableb.htm
051    xxx   xxx  xxxx  xxxx                window boundaries (4i6) latitude (-90 to 90), longitude (-180 to 180)
                                              i.e., west side then the east side.
061 iii                                     circle radius, km (i3)
062   xxx.xx  xxx.xx  xxx.xx  xxx.xx ..     circle loci, four latitude, longitude pairs; 4(2f8.2) per line
                                              subsequent lines for up to 100 pairs
071 bbsss bbsss bbsss bbsss ..              specific WMO numeric station numbers (five digit); 10(A5,1X) per line
                                              subsequent lines for up to 100 stations
076 bb bb bb ..                             any WMO station numbers with these two digit block numbers (10(a2,1x)); per line
                                              subsequent lines for up to 100 blocks
081  eeeee eeeee                            elevation range (-1000 to 12000 m) (2i6)
086   pppp  pppp                            pressure level range (1100 to 1 mb) (2i6)
091 c or k                                  temperature units: c/k  for  Kelvin/Celsius (default is 'c')
999 STOP                                    END OF CONFIGURATION (MUST BE PRESENT)
```
ADDITIONAL COMMENTS
041 to 099: simply omit the line(s) if you do not want to filter by this means
