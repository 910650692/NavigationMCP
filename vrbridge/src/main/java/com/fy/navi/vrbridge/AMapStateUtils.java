package com.fy.navi.vrbridge;

import java.util.HashMap;

import org.json.JSONException;
import org.json.JSONObject;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.baidu.bridge.BridgeSdk;
import com.baidu.oneos.protocol.IStateManager;
import com.baidu.oneos.protocol.SystemStateCons.NaviStateCons;
import com.fy.navi.vrbridge.bean.MapLocation;
import com.fy.navi.vrbridge.bean.MapState;

final public class AMapStateUtils {

    private static final double X_PI = 3.14159265358979324 * 3000.0 / 180.0;

    private AMapStateUtils() {

    }

    /**
     * 传端状态.
     *
     * @param mapState MapState 端状态.
     */
    public static void saveMapState(final MapState mapState) {
        if (null == mapState) {
            return;
        }

        JSONObject data = null;
        try {
            final String mapStateStr = GsonUtils.toJson(mapState);
            data = new JSONObject(mapStateStr);
        } catch (JSONException exception) {
            Logger.e(IVrBridgeConstant.TAG, "parse mapState error");
        }
        if (null == data) {
            return;
        }

        final HashMap<String, Object> map = new HashMap<>();
        map.put(NaviStateCons.KEY_CURRENT_MAP_TYPE, "A_MAP");
        saveMapStateBoolean(map, data);
        if (data.has("mCurrMapMode")) {
            map.put(NaviStateCons.KEY_PERSPECTIVE_MODE, data.optInt("mCurrMapMode"));
        }
        boolean isMute = false;
        if (data.has("mIsMute")) {
            isMute = data.optBoolean("mIsMute");
            if (isMute) {
                map.put(NaviStateCons.KEY_BROADCAST_MODE, 2);
            }
        }
        if (data.has("mBroadcastMode")) {
            map.put(NaviStateCons.KEY_BROADCAST_MODE, isMute ? 2
                    : data.optInt("mBroadcastMode"));
        }
        if (data.has("mPathCount")) {
            map.put(NaviStateCons.KEY_PLANE_ROUTE_NUM, data.optInt("mPathCount"));
        }
        if (data.has("mViaPointsCount")) {
            map.put(NaviStateCons.KEY_PASSING_POINT_NUM, data.optInt("mViaPointsCount"));
        }
        if (data.has("mViaPointsMaxCount")) {
            map.put(NaviStateCons.KEY_PASSING_POINT_MAX_COUNT,
                    data.optInt("mViaPointsMaxCount"));
        }
        if (data.has("mIsParallelFlagMain")) {
            map.put(NaviStateCons.KEY_ON_MAIN_ROAD, data.optInt("mIsParallelFlagMain"));
        }
        if (data.has("mIsParallelBridge")) {
            map.put(NaviStateCons.KEY_ON_BRIDGE, data.optInt("mIsParallelBridge"));
        }
        if (data.has("mCurrPlanPref")) {
            map.put(NaviStateCons.KEY_ROAD_PREFERENCE, data.optInt("mCurrPlanPref"));
        }
        int maxMapSize = -1;
        if (data.has("mMaxZoomLevel")) {
            maxMapSize = data.optInt("mMaxZoomLevel");
            map.put(NaviStateCons.KEY_MAX_MAP_SIZE, maxMapSize);
        }
        int minMapSize = -1;
        if (data.has("mMinZoomLevel")) {
            minMapSize = data.optInt("mMinZoomLevel");
            map.put(NaviStateCons.KEY_MIN_MAP_SIZE, minMapSize);
        }
        int currMapSize = -1;
        if (data.has("mCurrZoomLevel")) {
            currMapSize = data.optInt("mCurrZoomLevel");
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
        if (data.has("mEndPoiName")) {
            map.put(NaviStateCons.KEY_DEST_NAME, data.optString("mEndPoiName"));
        }
        if (data.has("mEndPoiCity")) {
            map.put(NaviStateCons.KEY_DEST_CITY, data.optString("mEndPoiCity"));
        }
        if (data.has("mMaxVolumeLevel")) {
            map.put(NaviStateCons.KEY_MAX_VOLUME, data.optInt("mMaxVolumeLevel"));
        }
        if (data.has("mCurrentVolumeLevel")) {
            map.put(NaviStateCons.KEY_CURRENT_VOLUME, data.optInt("mCurrentVolumeLevel"));
        }
        try {
            final IStateManager bdStateManager = BridgeSdk.getInstance().getRemote(IStateManager.class);
            if (null != bdStateManager) {
                bdStateManager.updateNaviState(map);
            }
        } catch (ClassCastException | NullPointerException exception) {
            Logger.e(IVrBridgeConstant.TAG, "updateNaviState: " + exception.getMessage());
        }
    }

    /**
     * saveMapStateBoolean
     *
     * @param map  map
     * @param data data
     */
    private static void saveMapStateBoolean(final HashMap<String, Object> map, final JSONObject data) {
        if (data.has("mOpenStatus")) {
            map.put(NaviStateCons.KEY_NAVI_APP_OPENED, data.optBoolean("mOpenStatus"));
        }
        if (data.has("mIsFront")) {
            map.put(NaviStateCons.KEY_NAVI_APP_FOREGROUND, data.optBoolean("mIsFront"));
        }
        if (data.has("mIsGPSNavi")) {
            map.put(NaviStateCons.KEY_IN_NAVIGATION, data.optBoolean("mIsGPSNavi"));
        }
        if (data.has("mIsCruiseNavi")) {
            map.put(NaviStateCons.KEY_IN_CRUISE, data.optBoolean("mIsCruiseNavi"));
        }
        if (data.has("mIsRoutePage")) {
            map.put(NaviStateCons.KEY_HAS_PLANE_ROUTE, data.optBoolean("mIsRoutePage"));
        }
        if (data.has("mIsSetHome")) {
            map.put(NaviStateCons.KEY_SETTED_HOME_ADDRESS, data.optBoolean("mIsSetHome"));
        }
        if (data.has("mIsSetCompany")) {
            map.put(NaviStateCons.KEY_SETTED_WORK_ADDRESS, data.optBoolean("mIsSetCompany"));
        }
        if (data.has("mIsRoadEvent")) {
            map.put(NaviStateCons.KEY_TRAFFIC_CONDITION_OPENED, data.optBoolean("mIsRoadEvent"));
        }
        if (data.has("mIsOverView")) {
            map.put(NaviStateCons.KEY_FULL_VIEW_OPENED, data.optBoolean("mIsOverView"));
        }
        if (data.has("mSwitchParallelFlag")) {
            map.put(NaviStateCons.KEY_MAIN_ROAD_TOGGLEABLE, data.optBoolean("mSwitchParallelFlag"));
        }
        if (data.has("mSwitchParallelBridge")) {
            map.put(NaviStateCons.KEY_BRIDGE_TOGGLEABLE, data.optBoolean("mSwitchParallelBridge"));
        }
        if (data.has("mIsListPage")) {
            map.put(NaviStateCons.KEY_IS_POI_LIST_SHOW, data.optBoolean("mIsListPage"));
        }
        if (data.has("mIsLogin")) {
            map.put(NaviStateCons.KEY_IS_LOGIN, data.optBoolean("mIsLogin"));
        }
        if (data.has("mIsAgreeTeamAgreement")) {
            map.put(NaviStateCons.KEY_ACCEPTED_TEAM_AGREEMENT, data.optBoolean("mIsAgreeTeamAgreement"));
        }
        if (data.has("mIsInTeam")) {
            map.put(NaviStateCons.KEY_IN_TEAM, data.optBoolean("mIsInTeam"));
        }
        if (data.has("mIsTeamLeader")) {
            map.put(NaviStateCons.KEY_TEAM_LEADER, data.optBoolean("mIsTeamLeader"));
        }
        if (data.has("mIsDayWithMapStyle")) {
            map.put(NaviStateCons.KEY_DAY_MODE, data.optBoolean("mIsDayWithMapStyle"));
        }
        if (data.has("mHasPrivacyPermission")) {
            map.put(NaviStateCons.KEY_PRIVACY_ACCEPTED, data.optBoolean("mHasPrivacyPermission"));
        }
    }

    /**
     * 保存定位信息
     *
     * @param mapLocation 示例：{"gpsTickCount":0,"sysTickCount":1720598461888,"vaccuracy":0,"provider":"test",
     *                    "bearing":0,"resultCode":10000,"accuracy":0,"lon":116.39747,"type":0,"sourceFlag":1,
     *                    "speed":0,"lat":39.9088229}
     */
    public static void saveMapLocation(final MapLocation mapLocation) {
        if (null == mapLocation) {
            return;
        }

        JSONObject data = null;
        try {
            final String mapLocationStr = GsonUtils.toJson(mapLocation);
            data = new JSONObject(mapLocationStr);
        } catch (JSONException exception) {
            Logger.e(IVrBridgeConstant.TAG, "parse mapLocation error");
        }
        if (null == data) {
            return;
        }
        final HashMap<String, Object> map = new HashMap<>();
        map.put("provider", data.optString("provider"));
        map.put("speed", data.optInt("speed"));
        map.put("bearing", data.optInt("bearing"));
        final double lon = data.optDouble("lon"); // 经度
        final double lat = data.optDouble("lat"); // 纬度
        final double[] doubles = gcj02ToBd09(lat, lon);
        map.put("lon", doubles[1]); // 返回Bd09坐标系的经纬度
        map.put("lat", doubles[0]);
        try {
            BridgeSdk.getInstance().getRemote(IStateManager.class).updateNaviLocation(map);
        } catch (ClassCastException | NullPointerException exception) {
            Logger.e(IVrBridgeConstant.TAG, "updateNaviLocation: " + exception.getMessage());
        }
    }

    /**
     * gcj02ToBd09
     *
     * @param lat 纬度
     * @param lon 经度
     * @return double
     */
    private static double[] gcj02ToBd09(final double lat, final double lon) {
        final double z = Math.sqrt(lon * lon + lat * lat) + 0.00002 * Math.sin(lat * X_PI);
        final double theta = Math.atan2(lat, lon) + 0.000003 * Math.cos(lon * X_PI);
        final double tempLon = z * Math.cos(theta) + 0.0065;
        final double tempLat = z * Math.sin(theta) + 0.006;
        return new double[]{tempLat, tempLon};
    }

}