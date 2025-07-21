package com.sgm.navi.service.tts;


import com.autonavi.gbl.guide.model.RangeType;

import java.util.Arrays;
import java.util.List;

public class TTSPlayHelper {

    private static final List<Integer> soundTypeEvenNopOpen = Arrays.asList(
            RangeType.RANGETYPE_BRIDGE_PROMPT,//1010
            RangeType.RANGETYPE_BRIDGE_PASS,//1020
            RangeType.RANGETYPE_SERVICE_REMOTE,//1070
            RangeType.RANGETYPE_SERVICE_CLOSE,//1080
            RangeType.RANGETYPE_SERVICE_CONFIRM,//1090
            RangeType.RANGETYPE_CARE_FATIGUE,//1100
            RangeType.RANGETYPE_FORBIDDEN_REMOTE,//1120
            RangeType.RANGETYPE_FORBIDDEN_CLOSE,//1130
            RangeType.RANGETYPE_CURVE_PROMPT,//1140
            RangeType.RANGETYPE_CURVE_PASS,//1150
            RangeType.RANGETYPE_PATHABNORMAL,//1360
            RangeType.RANGETYPE_LEFT_LANE_MERGE_PROMPT,//1500
            RangeType.RANGETYPE_RIGHT_LANE_MERGE_PROMPT,//1501
            RangeType.RANGETYPE_SHARP_CURVE_PROMPT,//1502
            RangeType.RANGETYPE_CURVES_AHEAD_PROMPT,//1503
            RangeType.RANGETYPE_FALLING_ROCKS_PROMPT,//1504
            RangeType.RANGETYPE_SLIPPERY_SURFACE_PROMPT,//1505
            RangeType.RANGETYPE_VILLAGE_PROMPT,//1506
            RangeType.RANGETYPE_RAILWAY_CROSSING_PROMPT,//1507
            RangeType.RANGETYPE_ACCIDENT_BLACK_SPOT_PROMPT,//1508
            RangeType.RANGETYPE_ROAD_NARROWS_LEFT_PROMPT,//1509
            RangeType.RANGETYPE_ROAD_NARROWS_RIGHT_PROMPT,//1510
            RangeType.RANGETYPE_ROAD_NARROWS_BOTHSIDES_PROMPT,//1511
            RangeType.RANGETYPE_CROSSWIND_PROMPT,//1512
            RangeType.RANGETYPE_SCHOOL_PROMPT,//1513
            RangeType.RANGETYPE_UP_STEEP_SLOP_PROMPT,//1514
            RangeType.RANGETYPE_DOWN_STEEP_SLOP_PROMPT,//1515
            RangeType.RANGETYPE_FACILITY_BUSLANE,//1516
            RangeType.RANGETYPE_PARK_GUIDE,//1580
            RangeType.RANGETYPE_WEATHERCLOSE,//2030
            RangeType.RANGETYPE_WEATHERMIDDLE,//2040
            RangeType.RANGETYPE_WEATHERFAR,//2050
            RangeType.RANGETYPE_MULTITRAFFIC,//2060
            RangeType.RANGETYPE_TRAFFICIN,//2070
            RangeType.RANGETYPE_TRAFFICFIRST,//2080
            RangeType.RANGETYPE_TRAFFICCLOSE,//2090
            RangeType.RANGETYPE_TRAFFICFAR,//2100
            RangeType.RANGETYPE_TRAFFICEVENTFAR,//2150
            RangeType.RANGETYPE_TRAFFICEVENTCLOSE,//2160
            RangeType.RANGETYPE_TRAFFICEVENTGAODA,//2170
            RangeType.RANGETYPE_PASS_VIA,//3013
            RangeType.RANGETYPE_NAVIGATION_START,//3016
            RangeType.RANGETYPE_NAVIGATION_END,//3017
            RangeType.RANGETYPE_NAVIGATION_START_REPLACE,//3018
            RangeType.RANGETYPE_REFRESH_TURN,//3024
            RangeType.RANGETYPE_ETC_CAMERA_EDUCATION//4000
    );

    public static boolean allowToPlayWithNopOpen(int soundType){
        return soundTypeEvenNopOpen.contains(soundType);
    }

    public static int RANGETYPE_NAVIGATION_START() {
        return RangeType.RANGETYPE_NAVIGATION_START;
    }
}
