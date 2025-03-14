package com.fy.navi.vrbridge;

import android.util.Log;

import java.util.HashMap;

import org.json.JSONObject;

import com.android.utils.gson.GsonUtils;
import com.baidu.bridge.BridgeSdk;
import com.baidu.oneos.protocol.IStateManager;
import com.baidu.oneos.protocol.SystemStateCons.NaviStateCons;
import com.fy.navi.vrbridge.bean.MapLocation;
import com.fy.navi.vrbridge.bean.MapState;

import androidx.annotation.NonNull;

public class AmapStateUtils {

    private static final double X_PI = 3.14159265358979324 * 3000.0 / 180.0;

    /**
     * 将高德的视图模式 转换成 语音状态的视图模式
     * @param mode 高德的视图模式(0-2D正北向上，1-2D车头向上，2-3D车头向上)
     */
    public static int convertMapViewMode(int mode) {
        switch (mode) {
            case 0:
                return 1;
            case 1:
                return 0;
            case 2:
                return 2;
            default:
                return -1;
        }
    }

    /**
      * 将高德的播报模式 转换成 语音状态的播报模式
      * @param mode 高德的播报模式(2-详细； 4-简洁 6-极简)
      */
    public static int convertBroadcastMode(int mode) {
        switch (mode) {
            case 2:
                return 0;
            case 4:
                return 1;
            case 6:
                return 5;
            default:
                return -1;
        }
    }

    /**
     * 将高德的路线偏好 转换成 语音状态的路线偏好
     * @param preference 高德的路线偏好(0 高德推荐 2：躲避拥堵； 4：避免收费； 8：不走高速； 16：高速优先 32：速度最快  64：大路优先)
     */
    public static int convertRoadPreference(int preference) {
        switch (preference) {
            case 0:
                return 1;
            case 2:
                return 16;
            case 4:
                return 8;
            case 8:
                return 4;
            case 16:
                return 512;
            case 32:
                return 256;
            case 64:
                return 64;
            default:
                return -1;
        }
    }

    public static void saveMapState(MapState mapState) {
        if (null == mapState) {
            return;
        }

        JSONObject data = null;
        try {
            String mapStateStr = GsonUtils.toJson(mapState);
            data = new JSONObject(mapStateStr);
        } catch (Exception exception) {
            Log.e(IVrBridgeConstant.TAG, "parse mapState error");
        }
        if (null == data) {
            return;
        }
        HashMap<String, Object> map = new HashMap<>();
        map.put(NaviStateCons.KEY_CURRENT_MAP_TYPE, "A_MAP");
        if (data.has("openStatus")) {
            map.put(NaviStateCons.KEY_NAVI_APP_OPENED, data.optBoolean("openStatus"));
        }
        if (data.has("isFront")) {
            map.put(NaviStateCons.KEY_NAVI_APP_FOREGROUND, data.optBoolean("isFront"));
        }
        if (data.has("isGPSNavi")) {
            map.put(NaviStateCons.KEY_IN_NAVIGATION, data.optBoolean("isGPSNavi"));
        }
        if (data.has("isCruiseNavi")) {
            map.put(NaviStateCons.KEY_IN_CRUISE, data.optBoolean("isCruiseNavi"));
        }
        if (data.has("currMapMode")) {
            map.put(NaviStateCons.KEY_PERSPECTIVE_MODE, data.optInt("currMapMode"));
        }

        boolean isMute = false;
        if (data.has("isMute")) {
            isMute = data.optBoolean("isMute");
            if (isMute) {
                map.put(NaviStateCons.KEY_BROADCAST_MODE, 2);
            }
        }
        if (data.has("broadcastMode")) {
            map.put(NaviStateCons.KEY_BROADCAST_MODE, isMute ? 2
                    : data.optInt("broadcastMode"));
        }

        if (data.has("isRoutePage")) {
            map.put(NaviStateCons.KEY_HAS_PLANE_ROUTE, data.optBoolean("isRoutePage"));
        }
        if (data.has("pathCount")) {
            map.put(NaviStateCons.KEY_PLANE_ROUTE_NUM, data.optInt("pathCount"));
        }
        if (data.has("viaPointsCount")) {
            map.put(NaviStateCons.KEY_PASSING_POINT_NUM, data.optInt("viaPointsCount"));
        }
        if (data.has("viaPointsMaxCount")) {
            map.put(NaviStateCons.KEY_PASSING_POINT_MAX_COUNT,
                    data.optInt("viaPointsMaxCount"));
        }
        if (data.has("isSetHome")) {
            map.put(NaviStateCons.KEY_SETTED_HOME_ADDRESS, data.optBoolean("isSetHome"));
        }
        if (data.has("isSetCompany")) {
            map.put(NaviStateCons.KEY_SETTED_WORK_ADDRESS, data.optBoolean("isSetCompany"));
        }
        if (data.has("isRoadEvent")) {
            map.put(NaviStateCons.KEY_TRAFFIC_CONDITION_OPENED, data.optBoolean("isRoadEvent"));
        }
        if (data.has("isOverView")) {
            map.put(NaviStateCons.KEY_FULL_VIEW_OPENED, data.optBoolean("isOverView"));
        }
        if (data.has("isParallelFlagMain")) {
            map.put(NaviStateCons.KEY_ON_MAIN_ROAD, data.optInt("isParallelFlagMain"));
        }
        if (data.has("switchParallelFlag")) {
            map.put(NaviStateCons.KEY_MAIN_ROAD_TOGGLEABLE,
                    data.optBoolean("switchParallelFlag"));
        }
        if (data.has("isParallelBridge")) {
            map.put(NaviStateCons.KEY_ON_BRIDGE, data.optInt("isParallelBridge"));
        }
        if (data.has("switchParallelBridge")) {
            map.put(NaviStateCons.KEY_BRIDGE_TOGGLEABLE, data.optBoolean("switchParallelBridge"));
        }
        if (data.has("isListPage")) {
            map.put(NaviStateCons.KEY_IS_POI_LIST_SHOW, data.optBoolean("isListPage"));
        }
        if (data.has("currPlanPref")) {
            map.put(NaviStateCons.KEY_ROAD_PREFERENCE, data.optInt("currPlanPref"));
        }
        int maxMapSize = -1;
        if (data.has("maxZoomLevel")) {
            maxMapSize = data.optInt("maxZoomLevel");
            map.put(NaviStateCons.KEY_MAX_MAP_SIZE, maxMapSize);
        }
        int minMapSize = -1;
        if (data.has("minZoomLevel")) {
            minMapSize = data.optInt("minZoomLevel");
            map.put(NaviStateCons.KEY_MIN_MAP_SIZE, minMapSize);
        }
        int currMapSize = -1;
        if (data.has("currZoomLevel")) {
            currMapSize = data.optInt("currZoomLevel");
            map.put(NaviStateCons.KEY_CURRENT_MAP_SIZE, currMapSize);
        }
        if (currMapSize != -1) {
            if (maxMapSize != -1) {
                map.put(NaviStateCons.KEY_MAX_MAP, currMapSize == maxMapSize);
            }
            if (minMapSize != -1) {
                map.put(NaviStateCons.KEY_MIN_MAP, currMapSize == minMapSize);
            }
        }
        if (data.has("endPoiName")) {
            map.put(NaviStateCons.KEY_DEST_NAME, data.optString("endPoiName"));
        }
        if (data.has("endPoiCity")) {
            map.put(NaviStateCons.KEY_DEST_CITY, data.optString("endPoiCity"));
        }
        if (data.has("isDayWithMapStyle")) {
            map.put(NaviStateCons.KEY_DAY_MODE, data.optBoolean("isDayWithMapStyle"));
        }
        if (data.has("hasPrivacyPermission")) {
            map.put(NaviStateCons.KEY_PRIVACY_ACCEPTED,
                    data.optBoolean("hasPrivacyPermission"));
        }
        if (data.has("maxVolumeLevel")) {
            map.put(NaviStateCons.KEY_MAX_VOLUME, data.optInt("maxVolumeLevel"));
        }
        if (data.has("currentVolumeLevel")) {
            map.put(NaviStateCons.KEY_CURRENT_VOLUME, data.optInt("currentVolumeLevel"));
        }
        if (data.has("isLogin")) {
            map.put(NaviStateCons.KEY_IS_LOGIN, data.optBoolean("isLogin"));
        }
        if (data.has("isAgreeTeamAgreement")) {
            map.put(NaviStateCons.KEY_ACCEPTED_TEAM_AGREEMENT, data.optBoolean("isAgreeTeamAgreement"));
        }
        if (data.has("isInTeam")) {
            map.put(NaviStateCons.KEY_IN_TEAM, data.optBoolean("isInTeam"));
        }
        if (data.has("isTeamLeader")) {
            map.put(NaviStateCons.KEY_TEAM_LEADER, data.optBoolean("isTeamLeader"));
        }
        map.put(NaviStateCons.KEY_CURRENT_MAP_TYPE, "A_MAP");
        try {
            BridgeSdk.getInstance().getRemote(IStateManager.class).updateNaviState(map);
        } catch (Exception e) {
            Log.e(IVrBridgeConstant.TAG, "updateNaviState: " + Log.getStackTraceString(e));
        }
    }

    /**
     * 保存定位信息
     *
     * @param mapLocation 示例：{"gpsTickCount":0,"sysTickCount":1720598461888,"vaccuracy":0,"provider":"test",
     *             "bearing":0,"resultCode":10000,"accuracy":0,"lon":116.39747,"type":0,"sourceFlag":1,
     *             "speed":0,"lat":39.9088229}
     */
    public static void saveMapLocation(MapLocation mapLocation) {
        if (null == mapLocation) {
            return;
        }

        JSONObject data = null;
        try {
            String mapLocationStr = GsonUtils.toJson(mapLocation);
            data = new JSONObject(mapLocationStr);
        } catch (Exception exception) {
            Log.e(IVrBridgeConstant.TAG, "parse mapLocation error");
        }
        if (null == data) {
            return;
        }
        HashMap<String, Object> map = new HashMap<>();
        map.put("provider", data.optString("provider"));
        map.put("speed", data.optInt("speed"));
        map.put("bearing", data.optInt("bearing"));
        double lon = data.optDouble("lon"); // 经度
        double lat = data.optDouble("lat"); // 纬度
        double[] doubles = gcj02ToBd09(lat, lon);
        map.put("lon", doubles[1]); // 返回Bd09坐标系的经纬度
        map.put("lat", doubles[0]);
        try {
            BridgeSdk.getInstance().getRemote(IStateManager.class).updateNaviLocation(map);
        } catch (Exception e) {
            Log.e(IVrBridgeConstant.TAG, "updateNaviLocation: " + Log.getStackTraceString(e));
        }
    }

    private static double[] gcj02ToBd09(double lat, double lon) {
        double x = lon;
        double y = lat;
        double z = Math.sqrt(x * x + y * y) + 0.00002 * Math.sin(y * X_PI);
        double theta = Math.atan2(y, x) + 0.000003 * Math.cos(x * X_PI);
        double tempLon = z * Math.cos(theta) + 0.0065;
        double tempLat = z * Math.sin(theta) + 0.006;
        return new double[]{tempLat, tempLon};
    }
}
