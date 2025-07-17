package com.sgm.navi.service.tts;


import com.autonavi.gbl.guide.model.RangeType;

import java.util.Arrays;
import java.util.List;

public class TTSPlayHelper {

    private static final List<Integer> soundTypeEvenNopOpen = Arrays.asList(
            RangeType.RANGETYPE_BRIDGE_PROMPT,
            RangeType.RANGETYPE_BRIDGE_PASS,
            RangeType.RANGETYPE_SERVICE_REMOTE,
            RangeType.RANGETYPE_SERVICE_CLOSE,
            RangeType.RANGETYPE_SERVICE_CONFIRM,
            RangeType.RANGETYPE_CARE_FATIGUE,
            RangeType.RANGETYPE_FORBIDDEN_REMOTE,
            RangeType.RANGETYPE_FORBIDDEN_CLOSE,
            RangeType.RANGETYPE_CURVE_PROMPT,
            RangeType.RANGETYPE_CURVE_PASS,
            RangeType.RANGETYPE_PATHABNORMAL,
            RangeType.RANGETYPE_LEFT_LANE_MERGE_PROMPT,
            RangeType.RANGETYPE_RIGHT_LANE_MERGE_PROMPT,
            RangeType.RANGETYPE_SHARP_CURVE_PROMPT,
            RangeType.RANGETYPE_CURVES_AHEAD_PROMPT,
            RangeType.RANGETYPE_FALLING_ROCKS_PROMPT,
            RangeType.RANGETYPE_SLIPPERY_SURFACE_PROMPT,
            RangeType.RANGETYPE_VILLAGE_PROMPT,
            RangeType.RANGETYPE_RAILWAY_CROSSING_PROMPT,
            RangeType.RANGETYPE_ACCIDENT_BLACK_SPOT_PROMPT,
            RangeType.RANGETYPE_ROAD_NARROWS_LEFT_PROMPT,
            RangeType.RANGETYPE_ROAD_NARROWS_RIGHT_PROMPT,
            RangeType.RANGETYPE_ROAD_NARROWS_BOTHSIDES_PROMPT,
            RangeType.RANGETYPE_CROSSWIND_PROMPT,
            RangeType.RANGETYPE_SCHOOL_PROMPT,
            RangeType.RANGETYPE_UP_STEEP_SLOP_PROMPT,
            RangeType.RANGETYPE_DOWN_STEEP_SLOP_PROMPT,
            RangeType.RANGETYPE_FACILITY_BUSLANE,
            RangeType.RANGETYPE_PARK_GUIDE,
            RangeType.RANGETYPE_WEATHERCLOSE,
            RangeType.RANGETYPE_WEATHERMIDDLE,
            RangeType.RANGETYPE_WEATHERFAR,
            RangeType.RANGETYPE_MULTITRAFFIC,
            RangeType.RANGETYPE_TRAFFICIN,
            RangeType.RANGETYPE_TRAFFICFIRST,
            RangeType.RANGETYPE_TRAFFICCLOSE,
            RangeType.RANGETYPE_TRAFFICFAR,
            RangeType.RANGETYPE_TRAFFICEVENTFAR,
            RangeType.RANGETYPE_TRAFFICEVENTCLOSE,
            RangeType.RANGETYPE_TRAFFICEVENTGAODA,
            RangeType.RANGETYPE_PASS_VIA,
            RangeType.RANGETYPE_NAVIGATION_START,
            RangeType.RANGETYPE_NAVIGATION_END,
            RangeType.RANGETYPE_NAVIGATION_START_REPLACE,
            RangeType.RANGETYPE_REFRESH_TURN,
            RangeType.RANGETYPE_ETC_CAMERA_EDUCATION
    );

    public static boolean allowToPlayWithNopOpen(int soundType){
        return soundTypeEvenNopOpen.contains(soundType);
    }

    public static int RANGETYPE_NAVIGATION_START() {
        return RangeType.RANGETYPE_NAVIGATION_START;
    }
}
