package com.sgm.navi.service.logicpaket.navi;


import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.path.option.PathInfo;
import com.sgm.navi.service.R;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.layer.refix.DynamicLevelMode;
import com.sgm.navi.service.define.layer.refix.LayerItemRouteEndPoint;
import com.sgm.navi.service.define.layer.refix.LayerPointItemType;
import com.sgm.navi.service.define.map.MapMode;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.route.RouteParam;
import com.sgm.navi.service.define.search.ETAInfo;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.search.SearchResultCallback;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * 获取其他模块接口的辅助类，帮助快速查找Navi中使用到的其他模块的接口
 * @author sgm
 * @version $Revision.*$
 */
public final class OpenApiHelper {

    public static final String TAG = "OpenApiHelper";

    // 沿途搜
    public static final int ALONG_WAY = 0;
    // 终点搜
    public static final int DESTINATION = 1;
    // 周边搜
    public static final int ARROUND = 2;
    private static final RoutePackage ROUTE_PACKAGE = RoutePackage.getInstance();

    private static final CalibrationPackage CALIBRATION_PACKAGE = CalibrationPackage.getInstance();

    private static List<PathInfo> CURRENT_PATH_INFOS = new CopyOnWriteArrayList<>();

    private static final SearchPackage SEARCH_PACKAGE = SearchPackage.getInstance();
    private static final NaviPackage NAVI_PACKAGE = NaviPackage.getInstance();
    private static final MapPackage MAP_PACKAGE = MapPackage.getInstance();
    private static final LayerPackage LAYER_PACKAGE = LayerPackage.getInstance();

    private static int CURRENT_SEARCH_TYPE = -1;

    private static PathInfo CURRENT_PATH_INFO = null;

    private OpenApiHelper() {
    }

    /**
     * 获取当前导航的路径信息
     * @param mapTypeId 屏幕id
     * @return 路径信息
     */
    public static PathInfo getCurrentPathInfo(final MapType mapTypeId) {
        if (null == CURRENT_PATH_INFO) {
            return ROUTE_PACKAGE.getCurrentPathInfo(mapTypeId) == null ? null :
                    (PathInfo) ROUTE_PACKAGE.getCurrentPathInfo(mapTypeId).getMPathInfo();
        } else {
            return CURRENT_PATH_INFO;
        }
    }

    public static void removePathById(final long pathId) {
        if (!ConvertUtils.isEmpty(CURRENT_PATH_INFOS)) {
            CURRENT_PATH_INFOS.removeIf(pathInfo -> pathId == pathInfo.getPathID());
        }
    }

    public static PathInfo getPathInfo(final MapType mapType, final long pathId) {
        if (!ConvertUtils.isEmpty(CURRENT_PATH_INFOS)) {
            for (PathInfo pathInfo : CURRENT_PATH_INFOS) {
                if (pathId == pathInfo.getPathID()) {
                    return pathInfo;
                }
            }
        }
        return null;
    }

    /**
     * @param pathInfo 设置当前的路径信息
     */
    public static void setCurrentPathInfo(final PathInfo pathInfo) {
        CURRENT_PATH_INFO = pathInfo;
    }

    /**
     * @param mapTypeId 屏幕id
     * @return 返回当前的路径id
     */
    public static long getCurrentPathId(final MapType mapTypeId) {
        PathInfo pathInfo = null;
        if (null == CURRENT_PATH_INFO) {
            pathInfo = getCurrentPathInfo(mapTypeId);
        } else {
            pathInfo = CURRENT_PATH_INFO;
        }
        if (pathInfo != null) {
            return pathInfo.getPathID();
        }
        return -1;
    }

    /**
     * 获取当前算路出的导航线路的所有路径信息
     * @return 返回当前的路径信息列表
     */
    public static List<PathInfo> getCurrentPathInfos() {
        return CURRENT_PATH_INFOS;
    }

    /**
     * @param currentPathInfos 设置当前的路径信息列表
     */
    public static void setCurrentPathInfos(final List<PathInfo> currentPathInfos) {
        if (ConvertUtils.isEmpty(currentPathInfos)) {
            Logger.e(TAG, "setCurrentPathInfos is null");
            return;
        }
        Logger.i(TAG, "setCurrentPathInfos size = " + currentPathInfos.size());
        CURRENT_PATH_INFOS = new CopyOnWriteArrayList<>(currentPathInfos);
    }

    /**
     * 动力类型标定
     * -1 无效值
     * 0 汽油车
     * 1 纯电动车
     * 2 插电式混动汽车
     * @return 车辆动力类型
     */
    public static int powerType() {
        int type = CALIBRATION_PACKAGE.powerType();
        Logger.i(TAG, "powerType = " + type);
        return type;
    }

    /**
     * @param keyWord 搜索关键字
     * @param searchType 搜索类型
     * @return 返回搜索的taskId
     */
    public static int getSearchResult(final String keyWord, final int searchType) {
        CURRENT_SEARCH_TYPE = searchType;
        int taskId = -1;
        if (searchType == 0) {
            taskId = SEARCH_PACKAGE.enRouteKeywordSearch(keyWord);
        } else if (searchType == 1) {
            final RouteParam endPoint = ROUTE_PACKAGE.getEndPoint(MapType.MAIN_SCREEN_MAIN_MAP);
            taskId = SEARCH_PACKAGE.aroundSearch(1, keyWord,
                    new GeoPoint(endPoint.getRealPos().getLon(), endPoint.getRealPos().getLat()), false);
        } else {
            taskId = SEARCH_PACKAGE.aroundSearch(1, keyWord);
        }
        return taskId;
    }

    public static int getSearchType() {
        return CURRENT_SEARCH_TYPE;
    }

    /**
     * @param mapTypeId 屏幕id
     * @return 获取路线上所有的点
     */
    public static List<RouteParam> getAllPoiParamList(final MapType mapTypeId) {
        return ROUTE_PACKAGE.getAllPoiParamList(mapTypeId);
    }

    /**
     * @param callBackId 回调id
     * @param resultCallback 回调接口
     */
    public static void registerSearchResultCallback(final String callBackId,
                                                    final SearchResultCallback resultCallback) {
        SEARCH_PACKAGE.registerCallBack(callBackId, resultCallback);
    }

    public static void unRegisterSearchResultCallback(final String callBackId) {
        SEARCH_PACKAGE.unRegisterCallBack(callBackId);
    }

    private static MapType mMapType;
    private static MapMode mMapMode;
    /**
     * 切换视角
     * @param mapTypeId 屏幕id
     * @param mapMode 视角类型
     */
    public static void switchMapMode(MapType mapTypeId, MapMode mapMode) {
        mMapType = mapTypeId;
        mMapMode = mapMode;
        if (!NAVI_PACKAGE.getPreviewStatus()) {
            MAP_PACKAGE.switchMapMode(mapTypeId, mapMode, true);
        } else {
            Logger.i(TAG, "switchMapMode: getPreviewStatus true");
        }
    }

    /**
     * 进入全览
     * @param mapTypeId 屏幕id
     */
    public static void enterPreview(final MapType mapTypeId) {
        Logger.i(TAG, "enterPreview");
        mMapType = null;
        mMapMode = null;
        NAVI_PACKAGE.setPreviewStatus(true);
        LAYER_PACKAGE.setFollowMode(mapTypeId, false);
        // 关闭自动比例尺
        NAVI_PACKAGE.closeDynamicLevel(mapTypeId);
        ROUTE_PACKAGE.showPreview(mapTypeId, DynamicLevelMode.DYNAMIC_LEVEL_GUIDE);
        LayerItemRouteEndPoint endPoint = NAVI_PACKAGE.getEndPoint();
        if (endPoint != null) {
            ROUTE_PACKAGE.updateRouteEndPoint(MapType.MAIN_SCREEN_MAIN_MAP, endPoint);
        }
        NAVI_PACKAGE.setRouteEnergyEmptyPointVisible(MapType.MAIN_SCREEN_MAIN_MAP, true);
    }

    /**
     * 退出全览
     * @param mapTypeId 屏幕id
     */
    public static void exitPreview(final MapType mapTypeId) {
        if (NAVI_PACKAGE.getFixedOverViewStatus() || NAVI_PACKAGE.getClusterFixOverViewStatus()) {
            Logger.i(TAG, "exitPreview: 固定全览状态，不能退出全览");
            return;
        }
        Logger.i(TAG, "exitPreview");
        NAVI_PACKAGE.setPreviewStatus(false);
        // 退出全览
        MAP_PACKAGE.exitPreview(mapTypeId, DynamicLevelMode.DYNAMIC_LEVEL_GUIDE);
        // 回到当前位置
        MAP_PACKAGE.goToCarPosition(mapTypeId, false, false);
        LAYER_PACKAGE.setFollowMode(mapTypeId, true);
        // bugID：1023666 导航中缩放地图然后点击继续导航，恢复到导航跟随态的过程时间太长
        setCurrentZoomLevel(mapTypeId);
        final boolean isAutoScale = SettingPackage.getInstance().getAutoScale();
        // 如果自动比例尺打开了才开启自动比例尺
        if (isAutoScale) {
            LAYER_PACKAGE.openDynamicLevel(mapTypeId, DynamicLevelMode.DYNAMIC_LEVEL_GUIDE);
        }
        // 如果全览时，视角和按键设置的有，需要设置一下视角
        if (mMapType != null && mMapMode != null &
                MAP_PACKAGE.getCurrentMapMode(mMapType) != mMapMode) {
            MAP_PACKAGE.switchMapMode(mMapType, mMapMode, true);
        }
        LayerItemRouteEndPoint endPoint = new LayerItemRouteEndPoint();
        endPoint.setEndPointType(LayerPointItemType.ROUTE_POINT_END);
        ROUTE_PACKAGE.updateRouteEndPoint(MapType.MAIN_SCREEN_MAIN_MAP, endPoint);
        NAVI_PACKAGE.setRouteEnergyEmptyPointVisible(MapType.MAIN_SCREEN_MAIN_MAP, false);
    }

    /**
     * 设置当前车标模式的默认比例尺
     * @param mapType 屏幕类型
     */
    public static void setCurrentZoomLevel(MapType mapType) {
        MapMode currentMapMode = MAP_PACKAGE.getCurrentMapMode(mapType);
        switch (currentMapMode) {
            case UP_2D, NORTH_2D:
                MAP_PACKAGE.setZoomLevel(mapType, 15);
                break;
            case UP_3D:
                MAP_PACKAGE.setZoomLevel(mapType, 17);
                break;
            default:
                break;
        }
    }

    /**
     * 清除搜索扎标标记
     */
    public static void clearSearchLabelMark() {
        SEARCH_PACKAGE.clearLabelMark();
    }

    /**
     * @param energyConsumption 到达目的地消耗的能量，单位为百分之一瓦时
     * @param currentEnergy 当前剩余电量，单位为百分之一瓦时
     * @param maxEnergy 最大电池量，单位为百分之一瓦时
     * @return 获取到达电量剩余百分比
     */
    public static int calculateRemainingOrNeededEnergyPercent(long energyConsumption, long currentEnergy, long maxEnergy) {
        Logger.i(TAG, "calculateRemainingOrNeededEnergyPercent","energyConsumption: ",
                energyConsumption, ", currentEnergy: ", currentEnergy, ", maxEnergy: " + maxEnergy);
        if (energyConsumption < 0 || currentEnergy < 0 || maxEnergy <= 0) {
            return -1; // 返回-1表示无效的电量数据
        }
        // 验证当前剩余电量是否足够到达目的地
        if (currentEnergy >= energyConsumption) {
            // 如果电量足够，计算到达目的地后的剩余电量百分比
            double remainingEnergyPercent = ((double)(
                    currentEnergy - energyConsumption) * 100) / maxEnergy;
            return (int)Math.round(remainingEnergyPercent);
        } else {
            // 如果电量不足，计算到达目的地还需要多少电量
            double neededEnergyPercent = ((double)(
                    energyConsumption - currentEnergy) * 100) / maxEnergy;
            // 将需要的额外电量转换为相对于最大电量的百分比，并返回负数，进行四舍五入
            return (int)Math.round(-neededEnergyPercent);
        }
    }

    /**
     * @param geoPoint 经纬度点
     * @return ETAInfo 包含距离，剩余时间和剩余电量
     */
    public static CompletableFuture<ETAInfo> getTravelTimeFutureIncludeChargeLeft(
            final GeoPoint geoPoint) {
        return SEARCH_PACKAGE.getTravelTimeFutureIncludeChargeLeft(geoPoint);
    }
}
