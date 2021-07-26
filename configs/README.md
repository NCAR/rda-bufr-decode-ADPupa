# Configuration file user guide and templates

The `/configs` directory contains sample configuration files which can
be provided to the `bufrupperair.f` executable.

The parameters to be extracted are identified by mnemonics.  If the set of
default parameters defined in `bufrupperair.f` is sufficient, you will not
need to know other mnemonics.  To add more parameters, please refer
to a list of mnemonics, some of which are shown in `..bufrdecode/docs/mnemonics.txt`.  

A complete list of mnemonics is available in the NCEP BUFR document:
https://www.emc.ncep.noaa.gov/emc/pages/infrastructure/bufrlib/tables/bufrtab_tableb.html.

Following is a list and description of the sample configurations.
```
CONFIGURATION FILE               DESCRIPTION
-------------------------------------------------------------------------------------------
bufrupperair_config_template     A complete template with a description of the options and
                                 how to enter them

bufrupperair_config_all          Retrieves everything from the input BUFR file

bufrupperair_config_circles      Retrieves stations within a couple of circles of a 
                                 given radius

bufrupperair_config_gmeq         Retrieves stations inside a latitude-longitude boundary 
                                 including the location where the Greenwich Meridian meets 
                                 the equator.

bufrupperair_config_elev         Retrieves stations within the specified elevation range,
                                 specified in meters

bufrupperair_config_idl          Retrieves stations inside a latitude-longitude boundary 
                                 for the Bering Strait region, on either side of the 
                                 International Date Line.  Note that this includes where 
                                 the longitude changes from maximum west (-179.9) to maximum
                                 east (179.9)

bufrupperair_config_US_def       Retrieves observations within a latitude-longitude boundary
                                 that includes the 48 contiguous United States

bufrupperair_config_US_extra     Retrieves observations within a latitude-longitude boundary 
                                 that includes the 48 contiguous United States, and extracts
                                 some additional parameters

bufrupperair_config_US_selpr     Retrieves observations within a latitude-longitude boundary 
                                 that includes the 48 contiguous United States, and extracts 
                                 some additional parameters at pressure levels between 800 
                                 and 100mb.
                    
bufrupperair_config_wbb          Retrieves observations from SYNOP stations from the given WMO blocks

bufrupperair_config_wmo          Retrieves observations from the specified SYNOP stations
```
