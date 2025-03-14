package com.fy.navi.service.adapter.layer.bls.parser;

import android.graphics.Rect;

import com.android.utils.ConvertUtils;
import com.android.utils.file.ParseJsonUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.guide.model.TrafficLight;
import com.autonavi.gbl.layer.GuideTrafficSignalLightLayerItem;
import com.autonavi.gbl.layer.model.TrafficLightCountDownStatus;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.model.QuadrantType;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.NaviAdapter;
import com.fy.navi.service.define.layer.bls.MarkerInfoBean;
import com.fy.navi.service.define.layer.bls.RasterImageBean;
import com.fy.navi.service.define.layer.bls.VectorCrossBean;
import com.fy.navi.service.define.map.MapTypeId;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * @Description 图层json解析util类, 以便其它位置使用.
 * @Author lvww
 * @date 2024/12/20
 */
public class ParserStyleLayerUtils {
    private static final String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;
    private static JSONObject mLayerItemInfo;
    private static JSONObject mRouteLayerJson;
    private static JSONObject mCrossLayerJson;
    private static JSONObject mRoadFacilityLayerJson;

    /*** 因为搜索图层比较多，所以抽出一个json文件单独作为搜索图层配置 **/
    private static JSONObject mSearchResultLayer;
    private static JSONObject searchResultParentPoint;
    private static JSONObject searchParkingResultPoint;
    private static JSONObject roadFacilityItemInfo;

    private static JSONObject mMarkerInfo;

    public static void initStyleJsonFile(long engineId) {
        try {
            String layerSearchResult = ParseJsonUtils.parseJsonFile("bls/style" + engineId + "/layer_search_result_point.json");
            JSONObject searhcLayer = new JSONObject(layerSearchResult);
            mSearchResultLayer = searhcLayer.getJSONObject("search_result_layer");

            String layerInfo = ParseJsonUtils.parseJsonFile("bls/style" + engineId + "/layer_item_info.json");
            if (mLayerItemInfo == null) mLayerItemInfo = new JSONObject(layerInfo);
            mRouteLayerJson = mLayerItemInfo.getJSONObject("route_json_layer");
            mCrossLayerJson = mLayerItemInfo.getJSONObject("cross_json_layer");

            String markerInfo = ParseJsonUtils.parseJsonFile("bls/style" + engineId + "/marker_info.json");
            if (mMarkerInfo == null) mMarkerInfo = new JSONObject(markerInfo);

            String roadFacilityInfo = ParseJsonUtils.parseJsonFile("bls/style" + engineId + "/layer_road_facility_info.json");
            if (mRoadFacilityLayerJson == null)
                mRoadFacilityLayerJson = new JSONObject(roadFacilityInfo);
        } catch (JSONException jsonException) {
            Logger.i(TAG, "engineId: " + engineId, jsonException.toString());
        }
    }

    /*** 获取纹理json文件 **/
    public static JSONObject getLayerItemInfo() {
        return mLayerItemInfo;
    }

    /*** 获取锚点json文件 **/
    public static JSONObject getMarkerInfo() {
        return mMarkerInfo;
    }

    /*** 获取搜索父节点结果的扎标点图元 **/
    public static String getSearchResultPointLayer(String key) {
        try {
            if (ConvertUtils.isEmpty(searchResultParentPoint))
                searchResultParentPoint = mSearchResultLayer.getJSONObject("search_result_parent_point");
            return ParseJsonUtils.getStyleBeanJson(searchResultParentPoint, key);
        } catch (JSONException e) {
            return "EMPTY";
        }
    }

    public static String getSearchParkingResultPointLayer(String key) {
        try {
            if (ConvertUtils.isEmpty(searchParkingResultPoint))
                searchParkingResultPoint = mSearchResultLayer.getJSONObject("search_result_parking_point");
            return ParseJsonUtils.getStyleBeanJson(searchParkingResultPoint, key);
        } catch (JSONException e) {
            return "EMPTY";
        }
    }

    public static String getTrafficLightLayer(LayerItem layerItem) {
        try {
            if (ConvertUtils.isEmpty(roadFacilityItemInfo))
                roadFacilityItemInfo = mRoadFacilityLayerJson.getJSONObject("layer_item_info");
            String overlayType = "point_guide_traffic_signal_light_";
            GuideTrafficSignalLightLayerItem trafficLightItem = (GuideTrafficSignalLightLayerItem) layerItem;
            int trafficLightStatus = trafficLightItem.getTrafficLightStatus();
            int quadrantType = trafficLightItem.getQuadrantType();
            if (quadrantType == QuadrantType.QuadrantTypeLeft) {
                overlayType = overlayType + "left";
            } else if (quadrantType == QuadrantType.QuadrantTypeRight) {
                overlayType = overlayType + "right";
            }
//            if (trafficLightStatus == TrafficLightCountDownStatus.RedLight && quadrantType == QuadrantType.QuadrantTypeLeft) {
//
//            } else if (trafficLightStatus == TrafficLightCountDownStatus.RedLight && quadrantType == QuadrantType.QuadrantTypeRight) {
//            }
            Logger.d(TAG, "导航红绿灯信息已透出 trafficLightStatus " + trafficLightStatus + ",quadrantType："
                    + quadrantType + ",overlayType：" + overlayType);
            return ParseJsonUtils.getStyleBeanJson(roadFacilityItemInfo, overlayType);
        } catch (Exception e) {
            return "EMPTY";
        }
    }

    /*** 获取路线扎标图元纹理 **/
    public static String getRouteTypePointJson(String key) {
        return getRouteTypePointJson(mRouteLayerJson, key);
    }

    /*** 获取路线扎标图元纹理 **/
    public static String getRouteTypePointJson(JSONObject jsonObject, String key) {
        return ParseJsonUtils.getStyleBeanJson(jsonObject, key);
    }

    /*** 获取POI点击扎标图元纹理 **/
    public static String getPoiClickTypePointJson(String key) {
        return ParseJsonUtils.getStyleBeanJson(getMarkerInfo(), key);
    }

    /*** 获取扎标图元锚点 **/
    public static MarkerInfoBean getPointMarkerInfo(String markInfo) {
        if (ConvertUtils.isEmpty(markInfo)) return null;
        String json = ParseJsonUtils.getStyleBeanJson(mMarkerInfo, markInfo);
        return GsonUtils.fromJson(json, MarkerInfoBean.class);
    }

    /*** 获取栅格图 ***/
    public static String getLayerItemRasterImageType() {
        String rasterImageStyle = ParseJsonUtils.getStyleBeanJson(mCrossLayerJson, "raster_image_style");
        if (ConvertUtils.isEmpty(rasterImageStyle)) {
            Logger.e(TAG, "rasterImageStyle is null");
            return null;
        }
        //根据屏幕适配栅格大图宽高
        RasterImageBean rasterImageBean = GsonUtils.fromJson(rasterImageStyle, RasterImageBean.class);
        if (rasterImageBean != null) {
            RasterImageBean.RasterImageLayerItemStyleBean rasterImageLayerItemStyleBean = rasterImageBean.getRaster_image_layer_item_style();
            Rect rect = NaviAdapter.getInstance().gettRoadCrossRect(MapTypeId.MAIN_SCREEN_MAIN_MAP);
            if (rect != null) {
                rasterImageLayerItemStyleBean.setWinx(rect.left);
                rasterImageLayerItemStyleBean.setWiny(rect.top);
                rasterImageLayerItemStyleBean.setWidth(rect.width());
                rasterImageLayerItemStyleBean.setHeight(rect.height());
                Logger.d(TAG, "CrossImage_tag RasterImage rect: " + rect);
                return GsonUtils.toJson(rasterImageBean);
            } else {
                Logger.e(TAG, "rect == null ");
            }
        }
        Logger.e(TAG, "CrossImage_tag rasterImage is null ");
        return rasterImageStyle;
    }


    /*** 获取矢量图 ***/
    public static String getLayerItemVectorCrossType() {
        String vectorCrossStyle = ParseJsonUtils.getStyleBeanJson(mCrossLayerJson, "vector_cross_style");
        if (ConvertUtils.isEmpty(vectorCrossStyle)) {
            Logger.e(TAG, "矢量图 vectorCrossStyle is null");
            return null;
        }
        //根据屏幕适配路口大图宽高
        VectorCrossBean vectorCrossBean = GsonUtils.fromJson(vectorCrossStyle, VectorCrossBean.class);
        if (vectorCrossBean != null) {
            VectorCrossBean.VectorCrossLayerStyleBean.VectorCrossAttrBean.RectBean rectBean = vectorCrossBean.getVector_cross_layer_style().getVector_cross_attr().getRect();
            Rect rect = NaviAdapter.getInstance().gettRoadCrossRect(MapTypeId.MAIN_SCREEN_MAIN_MAP);
//            Rect rect = new Rect(248, 116, 248, 116);
            if (rect != null) {
                rectBean.setX_min(rect.left);
                rectBean.setY_min(rect.top);
                rectBean.setX_max(rect.right);
                rectBean.setY_max(rect.bottom);
                Logger.d(TAG, "CrossImage_tag vectorCrossStyle  rect: " + rect);
                return GsonUtils.toJson(vectorCrossBean);
            } else {
                Logger.e(TAG, "rect == null ");
            }
        }
        Logger.e(TAG, "CrossImage_tag vectorCrossBean is null");
        return vectorCrossStyle;
    }
}