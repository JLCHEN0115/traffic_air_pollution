===================================================================================================

California TAMS VC fixed-width format data

See section 4.2 and 4.5.2 in 2022_TMG_Final_Report for details
See `Improved California Truck Traffic Census Reporting and Spatial Activity Measurement` for VC codes

For example, the record
C06087600312510200001099000000073800225000010002200007000000000500090000000000100001000000000100000
should be interpretated as:

C	Record Type (RT)
06	FIPS state code (SFIPS)
087600	Station ID (ID)
3	Direction of Travel (DIR)
1	Lane of Travel (LN)
25	Year of Data (YR)
10	Month of Year (MOY)
20	Day of Month (DOM)
00	Hour of Day (HOD)
01099	Total Interval Volume (TVOL)
00000	FHWA1
00738	FHWA2
00225	FHWA3
00001	FHWA4
00022	FHWA5
00007	FHWA6
00000	FHWA7
00005	FHWA8
00090	FHWA9
00000	FHWA10
00001	FHWA11
00001	FHWA12
00000	FHWA13
00001	FHWA14
00000	FHWA15

===================================================================================================
Traffic Census Program

Also called Traffic Census Stations. Traffic census stations are managed through the
Caltrans Census Program. PeMS receives counts from mainline and traffic census
stations on a periodic basis (not in real-time). All census stations in PeMS contain counts,
and there are also some specialized census stations that report counts by vehicle class
(see Vehicle Classification Stations) and truck weight information (see WIM stations). 

===================================================================================================
Note that the PEMS data clearinghouse has both `Census V-Class Hour` data and `Census Trucks Hour` data
## Each of them seems to contain Vehicle Classification data we want
## But by inspection, the `Census Trucks Hour` data is unreliable
## See the testing code for further information.


===  `Census Trucks Hour` data variables ===============================================================

Timestamp	The date and time of the beginning of the summary interval. For example, a time of 08:00:00 indicates that the aggregate(s) contain measurements collected between 08:00:00 and 08:59:59. Note that minute and second values are always 0 for hourly aggregations. The format is MM/DD/YYYY HH24:MI:SS.	 
Census Station Identifier	The unique number that identifies this census station within PeMS.	 
Census Substation Identifier	The unique number that identifies this census substation within PeMS.	 
Freeway Identifier	The unique number that identifies this freeway within PeMS.	 
Freeway Direction	A string indicating the freeway direction.	 
City Identifier	The unique number that identifies the city that contains this census station within PeMS.	 
County Identifier	The unique number that identifies the county that contains this census station within PeMS.	

Federal Information Processing Standard (FIPS) code

    06000        California
    06001        Alameda County
    06003        Alpine County
    06005        Amador County
    06007        Butte County
    06009        Calaveras County
    06011        Colusa County
    06013        Contra Costa County
    06015        Del Norte County
    06017        El Dorado County
    06019        Fresno County
    06021        Glenn County
    06023        Humboldt County
    06025        Imperial County
    06027        Inyo County
    06029        Kern County
    06031        Kings County
    06033        Lake County
    06035        Lassen County
    06037        Los Angeles County
    06039        Madera County
    06041        Marin County
    06043        Mariposa County
    06045        Mendocino County
    06047        Merced County
    06049        Modoc County
    06051        Mono County
    06053        Monterey County
    06055        Napa County
    06057        Nevada County
    06059        Orange County
    06061        Placer County
    06063        Plumas County
    06065        Riverside County
    06067        Sacramento County
    06069        San Benito County
    06071        San Bernardino County
    06073        San Diego County
    06075        San Francisco County
    06077        San Joaquin County
    06079        San Luis Obispo County
    06081        San Mateo County
    06083        Santa Barbara County
    06085        Santa Clara County
    06087        Santa Cruz County
    06089        Shasta County
    06091        Sierra County
    06093        Siskiyou County
    06095        Solano County
    06097        Sonoma County
    06099        Stanislaus County
    06101        Sutter County
    06103        Tehama County
    06105        Trinity County
    06107        Tulare County
    06109        Tuolumne County
    06111        Ventura County
    06113        Yolo County
    06115        Yuba County
 
District Identifier	The unique number that identifies the Caltrans distrcit that contains this census station within PeMS.	 
Absolute Postmile	The postmile where this census station is located.
	Special care here: Data included here are ABSOLUTE postmiles. They reflect the actual distance along a freeway from its beginning to its terminus.
	Different from the jurisdictional (Caltrans) postmiles, which re-set to zero at every county line.
	Caltrans call absolute postmile "Odometer": 
		The Statewide Odometer is an alternate indicator of position within the linear reference system. The Statewide Odometer measures the distance of the location along a route from 		the beginning of the route, without regard for county boundaries.

		The Postmile specification uses measures that are local to each county (and Postmile Prefix codes) so that realigning a route (modifying the path that a route takes) has 			minimal impact on the specifications of locations downstream of the segment that was changed. This is important because of the cost to change all the physical postmile paddles 		that line the routes. In the Statewide Odometer, on the other hand, any change in the path of the route affects the odometer specification for all points downstream of the 			change, across the entire state. We convert postmile information to exact latitude and longitude using State Highway Network Lines (See below). You can verify the accuracy at 			https://postmile.dot.ca.gov/PMQT/PostmileQueryTool.html?

Station Type	A string indicating the type of station. Possible values (and their meaning are:
CD (Coll/Dist)
CH (Conventional Highway)
FF (Fwy-Fwy connector)
FR (Off Ramp)
HV (HOV)
ML (Mainline)
OR (On Ramp)
 
Census Station Set ID	The unique number that identifies the census station set within PeMS.	 
Lane Number	The lane number	 
	0 = aggregate

Vehicle Class	The vehicle class	
	See FHWA 13 VEHICLE CATEGORY CLASSIFICATION: https://www.fhwa.dot.gov/publications/research/infrastructure/pavements/ltpp/13091/002.cfm
 
Vehicle Count	The vehicle count	 
Average Speed	The average speed observed for this vehicle class during the summary interval.	MPH
Violation Count	The number of violations observed during his summary interval.	 
Violation Codes	A set of tuples of the form 'violation code' = 'count': ...	 
Single Axle Count	The single axle count	 
Single Axle Count	The single axle count	 
Tandem Axle Count	Tandem axle count	 
Tridem Axle Count	The tridem axle count	 
Quad Axle Count	The quad axle count	 
Aveage Gross Weight	The average gross weight	unknown
Gross Weight Distribution	The gross weight distribution	 
Averaege Single Weight	The average single weight	 
Averaege Tandem Weight	The average tandem weight	 
Averaege Tridem Weight	The average tridem weight	 
Averaege Quad Weight	The average quad weight	 
Averaege Vehicle Length	The average vehicle length	 
Vehicle Length Distribution	The vehicle length distribution	 
Average Tandem Spacing	The average tandem spacing	 
Average Tridem Spacing	The average tridem spacing	 
Average Quad Spacing	The average quad spacing	 
Average Wheelbase/td>	The average wheelbase	 
Wheelbase Distribution	The wheelbase distribution	 
Total Flex ESAL 300	Total Flex ESAL 300	 
Total Flex ESAL 285	Total Flex ESAL 285	 
Total Rigid ESAL 300	Total Rigid ESAL 300	 
Total Rigid ESAL 285	Total Rigid ESAL 285

===== `Census V-Class Hour` data variables ==========================================
Also see above. 

The following VC codes is inferred by data matching. See the testing code for more information.

# class_1 is 0-8 ft
# class_2 is 8-20 ft
# class_3 is 2 Axle, 4T SU
# class_4 is Bus
# class_5 is 2 Axle,6T SU
# class_6 is 3 Axle SU
# class_7 is 4+ Axle SU
# class_8 is < 4 Axle ST
# class_9 is 5 Axle ST
# class_10 is 6+ Axle ST
# class_11 is < 5 Axle MT
# class_12 is 6 Axle MT
# class_13 is 7+ Axle MT
# class_14 is User-Def
# class_15 is Unknown


observations with total_flow with 0 are removed in processing the data.

observations with freeway_ID = 944 are removed.

Observation with multiple LRS segments or relevant LINESTRING are removed. (deal with this in future version!)

=====================================================================================
CA State Highway Network linear referencing system (LRS)
https://gisdata-caltrans.opendata.arcgis.com/datasets/77f2d7ba94e040a78bfbe36feb6279da_0/about

# Note that we are currently using this LRS for all years traffic data.
# Change this in the future versions if possible.


