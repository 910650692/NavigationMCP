package com.fy.navi.service.logicpaket.navi;


import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.path.option.PathInfo;
import com.fy.navi.service.R;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;
import com.fy.navi.service.logicpaket.setting.SettingPackage;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * 获取其他模块接口的辅助类，帮助快速查找Navi中使用到的其他模块的接口
 * @author fy
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

    private OpenApiHelper() {
    }

    /**
     * 获取当前导航的路径信息
     * @param mapTypeId 屏幕id
     * @return 路径信息
     */
    public static PathInfo getCurrentPathInfo(final MapType mapTypeId) {
        return (PathInfo) ROUTE_PACKAGE.getCurrentPathInfo(mapTypeId).getMPathInfo();
    }

    /**
     * @param mapTypeId 屏幕id
     * @return 返回当前的路径id
     */
    public static long getCurrentPathId(final MapType mapTypeId) {
        final PathInfo pathInfo = getCurrentPathInfo(mapTypeId);
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
                    new GeoPoint(endPoint.getRealPos().getLon(), endPoint.getRealPos().getLat()));
        } else {
            taskId = SEARCH_PACKAGE.aroundSearch(1, keyWord);
        }
        return taskId;
    }

    /**
     * @param poiId poiId
     * @return 返回搜索的taskId
     */
    public static int poiIdSearch(String poiId) {
        return SEARCH_PACKAGE.poiIdSearch(poiId);
    }

    /**
     * 终点停车场专用搜索接口
     * @return 返回搜索结果
     */
    public static int getParkSearchResult() {
        int taskId = -1;
        final RouteParam endPoint = ROUTE_PACKAGE.getEndPoint(MapType.MAIN_SCREEN_MAIN_MAP);
        taskId = SEARCH_PACKAGE.aroundSearch(1, ResourceUtils.Companion.getInstance().
                getString(R.string.navi_parking_list),
                new GeoPoint(endPoint.getRealPos().getLon(), endPoint.getRealPos().getLat()));
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

    /**
     * 进入全览
     * @param mapTypeId 屏幕id
     */
    public static void enterPreview(final MapType mapTypeId) {
        NAVI_PACKAGE.setPreviewStatus(true);
        ROUTE_PACKAGE.naviShowPreview(mapTypeId);
    }

    /**
     * 退出全览
     * @param mapTypeId 屏幕id
     */
    public static void exitPreview(final MapType mapTypeId) {
        NAVI_PACKAGE.setPreviewStatus(false);
        // 退出全览
        MAP_PACKAGE.exitPreview(mapTypeId);
        // 回到当前位置
        MAP_PACKAGE.goToCarPosition(mapTypeId, false, false);
        LAYER_PACKAGE.setFollowMode(mapTypeId, true);
        LAYER_PACKAGE.openDynamicLevel(mapTypeId, SettingPackage.getInstance().
                getAutoScale());
    }

    /**
     * 清除搜索扎标标记
     */
    public static void clearLabelMark() {
        SEARCH_PACKAGE.clearLabelMark();
    }

    // 个性化道路接口
//    public static void test() {
//        Logger.i("shisong", "CURRENT_PATH_INFOS size = " + CURRENT_PATH_INFOS.size());
//        List<PathInfo> list = CURRENT_PATH_INFOS;
//        if (!ConvertUtils.isEmpty(list)) {
//            for (PathInfo pathInfo : list) {
//                long segmentCount = pathInfo.getSegmentCount();
//                for (long i = 0; i < segmentCount; i++) {
//                    SegmentInfo segment = pathInfo.getSegmentInfo(i);
//                    long linkCount = segment.getLinkCount();
//                    long curPathId = -1;
//                    String roadName = "";
//                    for (long j = 0; j < linkCount; j++) {
//                        LinkInfo link = segment.getLinkInfo(j);
//                        if (curPathId != pathInfo.getPathID() ||
//                                !roadName.equals(link.getRoadName())) {
//                            curPathId = pathInfo.getPathID();
//                            roadName = link.getRoadName();
//                            Logger.i("shisong", "pathId = " + curPathId,
//                                    "roadName = " + roadName);
//                        }
//
//                    }
//                }
//            }
//        }
//    }
}
