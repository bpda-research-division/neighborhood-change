This website is a project of the [BPDA Research Division](https://www.bostonplans.org/research/). 

For further analysis, see our [Historical Trends in Boston's Neighborhoods](https://www.bostonplans.org/research/research-publications?search=historical+&amp;sortby=date&amp;sortdirection=DESC) report, where most of the data on this website were originally published.

### Data

Data on this website are drawn from 1950-2020 Decennial Censuses and the 2006-2010 and 2016-2020 American Community Surveys. 

The smallest available geography for some variables is the Census tract. Therefore, neighborhood boundaries are approximated using 2020 Census tract boundaries. Older data are crosswalked to the 2020 tracts to provide a consistent geography over the decades (see the "Methodology" section below for more details).

Because they cannot be separately identified at the Census tract level, the Leather District is combined with Downtown and Bay Village is combined with the South End.

Census Tract 9815.02 is partially in East Boston and partially in Revere, but almost the entire population is in Revere so it is excluded from the East Boston approximation.

### Methodology

To create neighborhood-level data, we use U.S. Census Tracts to define neighborhood boundaries. The Census Bureau, however, has
changed the boundaries of its Census Tracts across the decades. We use the most recent map of Census Tracts (from the 2020
Census), then where necessary, we interpolate data from earlier years’ Census Tracts to conform to the tracts from 2020.

When Census Tracts change, we use that year’s population by Census Block, the smallest geographic unit available in the Census, to
create population weights that are used to apportion populations from earlier years to 2020 Census Tracts. In cases where Census
Blocks overlap the boundaries of 2020 Census Tracts, the share of block land area on either side of the 2020 tract boundary is
incorporated into the weights. In earlier years, when Census Block boundaries are not available, we use a modified method that
employs 1990 Census Block geography, the oldest year available.

While there were a fair number of tract boundary changes over the full time period we study, the majority of these boundary changes
occur within the larger neighborhood geographies, rather than between them. In these cases, the boundary changes do not affect the
numbers shown in this report, as both portions of the divided tract are aggregated into the same neighborhood.

The methods used here are similar to those used in the commercially available Neighborhood Change Database (NCDB), produced by
the Urban Institute and Geolytics, and the public use Longitudinal Tract Database (LTDB) produced by the American Communities
Project. Because our geographic scope is limited to Boston, we are able to extend our crosswalk back over a longer time period than
either of these sources, and to cover a larger set of variables than is available from the NCDB. Our method also uses block-level
population weighting for all years, whereas the LTDB uses area-based weighting for earlier years. Creation of our crosswalk was greatly
aided by the availability of historic tract and block-level GIS shapefiles from the National Historical Geographic Information System
(NHGIS) at the Minnesota Population Center.