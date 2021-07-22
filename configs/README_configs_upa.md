# Configuration file user guide and templates

The "configs" directory contains sample configuration files which can
be provided to the bufrupperair.f executable.

The parameters to be extracted are identified by mnemonics.  If the set of
default parameters defined in bufrupperair.f is sufficient, you will not
need to know other mnemonics.  To add more parameters, please refer
to a list of mnemonics, some of which are shown in `..bufrdecode/docs/mnemonics.txt`.  
A complete list is available in the NCEP BUFR document:
https://www.emc.ncep.noaa.gov/emc/pages/infrastructure/bufrlib/tables/bufrtab_tableb.html.

Following is a list and description of the sample configurations.
```
CONFIGURATION
FILE                            DESCRIPTION
bufrupprair_config_Guide_user   A template with a description of the options and
                                  how to enter them.

bufrupprair_config_all          Gets everything from the listed file

bufrupprair_config_circles      Gets only the stations within a couple of circles

bufrupprair_config_gmeq         Gets only the stations inside a latitude-
                                  longitude window including the place where the
                                  Greenwich Meridian meets the equator.

bufrupprair_config_high         Gets only the stations within the specified range
                                  of elevation

bufrupprair_config_idl          Gets only the stations inside a latitude-
                                  longitude window for the Bering Strait region,
                                  on either side of the International Date Line.
                                  Note that this includes where the longitude
                                  changes from maximum west (-179.9) to maximum
                                  east (179.9)

bufrupprair_config_US_def       Gets a latitude-longitude window which includes
                                  the 48 contiguous United States

bufrupprair_config_US_extra     Gets a latitude-longitude window which includes
                                  the 48 contiguous United States, and extracts
                                  some additional parameters

bufrupprair_config_US_selpr     Gets a latitude-longitude window which includes
                                  the 48 contiguous United States, extracts some
                                  additional parameters at pressure levels between
                                  800 and 100mb.
                    
bufrupprair_config_wbb          Gets only the SYNOP stations from the given
                                  WMO blocks

bufrupprair_config_wmo          Gets only the specified SYNOP stations
```

NOTE: All these configuration files use data file gdas.adpupa.t00z.20100901.bufr
      as an input.  Others may use one or more of the additional gdas files.  All
      files are already in the ../bufrobs directory, placed there as examples 
      when you untarred this package.
