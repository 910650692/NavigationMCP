package com.fy.navi.service.adapter.layer.bls.manager;

import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.map.MapService;
import com.autonavi.gbl.map.MapView;
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.fy.navi.service.define.layer.LayerType;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.logicpaket.engine.EnginePackage;

import java.util.Hashtable;

/**
 * 所有屏幕的图层管理器
 *
 * @Description 所有图层操作都要来自这里
 * @Author lvww
 * @date 2024/12/9
 */
public class LayerManager {
    private static final String TAG = "LayerManager";
    private Hashtable<MapTypeId, LayerBean> layerBeanHashtable;
    private BizControlService mBizService;
    private MapService mMapService;

    public LayerManager(BizControlService bizService, MapService mapService) {
        mBizService = bizService;
        mMapService = mapService;
        layerBeanHashtable = new Hashtable<>();
    }

    /**
     * 获取底图图层控制类.
     *
     * @param mapTypeId 视图ID
     * @return 底图图层控制实例
     */
    public MapLayerStyle getMapLayer(MapTypeId mapTypeId) {
        return (MapLayerStyle) getLayer(mapTypeId, LayerType.MAP_LAYER);
    }

    /**
     * 交通设施图层控制.
     *
     * @param mapTypeId 视图ID
     * @return 底图图层控制实例
     */
    public RoadFacilityLayerStyle getRoadFacilityLayer(MapTypeId mapTypeId) {
        return (RoadFacilityLayerStyle) getLayer(mapTypeId, LayerType.ROAD_FACILITY_LAYER);
    }

    /**
     * 获取搜索图层控制类.
     *
     * @param mapTypeId 视图ID
     * @return 搜索图层控制实例
     */
    public SearchLayerStyle getSearchLayer(MapTypeId mapTypeId) {
        return (SearchLayerStyle) getLayer(mapTypeId, LayerType.SEARCH_LAYER);
    }

    /**
     * 获取路线图层控制类.
     *
     * @param mapTypeId 视图ID
     * @return 路线图层控制实例
     */
    public RouteLayerStyle getRouteLayer(MapTypeId mapTypeId) {
        return (RouteLayerStyle) getLayer(mapTypeId, LayerType.ROUTE_LAYER);
    }

    /**
     * 获取引导图层控制类.
     *
     * @param mapTypeId 视图ID
     * @return 引导图层控制实例
     */
    public NaviLayerStyle getNaviLayer(MapTypeId mapTypeId) {
        return (NaviLayerStyle) getLayer(mapTypeId, LayerType.NAVI_LAYER);
    }

    /**
     * 获取路口大图图层控制类.
     *
     * @param mapTypeId 视图ID
     * @return 路口大图图层控制实例
     */
    public CrossLayerStyle getCrossLayer(MapTypeId mapTypeId) {
        return (CrossLayerStyle) getLayer(mapTypeId, LayerType.CROSS_LAYER);
    }

    /**
     * 获取鹰眼图层控制类.
     *
     * @param mapTypeId 视图ID
     * @return 鹰眼图层控制实例
     */
    public GuideEagleEyeStyle getEagleEyeLayer(MapTypeId mapTypeId) {
        return (GuideEagleEyeStyle) getLayer(mapTypeId, LayerType.EAGLE_EYE_LAYER);
    }

    /**
     * 获取区域图层控制类.
     *
     * @param mapTypeId 视图ID
     * @return 鹰眼图层控制实例
     */
    public AreaLayerStyle getAreaLayer(MapTypeId mapTypeId) {
        return (AreaLayerStyle) getLayer(mapTypeId, LayerType.AREA_LAYER);
    }

    /**
     * 获取图层控制类.
     *
     * @param mapTypeId 视图ID
     * @param layerType 图层类型
     * @return 图层控制实例
     */
    private BaseLayerStyle getLayer(MapTypeId mapTypeId, @LayerType.LayerId int layerType) {
        int engineId = EnginePackage.getInstance().getEngineID(mapTypeId);
        MapView mapView = mMapService.getMapView(engineId);
        LayerBean layerBean = ConvertUtils.containToValue(layerBeanHashtable, mapTypeId);
        if (ConvertUtils.isNull(layerBeanHashtable)) {
            layerBeanHashtable = new Hashtable<>();
        }
        if (ConvertUtils.isEmpty(layerBean)) {
            layerBean = new LayerBean();
            layerBeanHashtable.put(mapTypeId, layerBean);
        }
        switch (layerType) {
            case LayerType.MAP_LAYER:
                MapLayerStyle mapLayer = layerBean.mapLayerStyle;
                if (ConvertUtils.isEmpty(mapLayer)) {
                    mapLayer = new MapLayerStyle(mBizService, mapView);
                    layerBean.mapLayerStyle = mapLayer;
                }
                return mapLayer;
            case LayerType.SEARCH_LAYER:
                SearchLayerStyle searchLayerStyle = layerBean.searchLayerStyle;
                if (ConvertUtils.isEmpty(searchLayerStyle)) {
                    searchLayerStyle = new SearchLayerStyle(mBizService, mapView);
                    layerBean.searchLayerStyle = searchLayerStyle;
                    searchLayerStyle.addLayerClickListener();
                }
                return searchLayerStyle;

            case LayerType.ROUTE_LAYER:
                RouteLayerStyle routeLayerStyle = layerBean.routeLayerStyle;
                if (ConvertUtils.isEmpty(routeLayerStyle)) {
                    routeLayerStyle = new RouteLayerStyle(mBizService, mapView);
                    layerBean.routeLayerStyle = routeLayerStyle;
                    routeLayerStyle.addLayerClickListener();
                }
                return routeLayerStyle;

            case LayerType.NAVI_LAYER:
                NaviLayerStyle naviLayerStyle = layerBean.naviLayerStyle;
                if (ConvertUtils.isEmpty(naviLayerStyle)) {
                    naviLayerStyle = new NaviLayerStyle(mBizService, mapView);
                    layerBean.naviLayerStyle = naviLayerStyle;
                }
                return naviLayerStyle;
            case LayerType.CROSS_LAYER:
                CrossLayerStyle crossLayerStyle = layerBean.crossLayerStyle;
                if (ConvertUtils.isEmpty(crossLayerStyle)) {
                    crossLayerStyle = new CrossLayerStyle(mBizService, mapView);
                    layerBean.crossLayerStyle = crossLayerStyle;
                }
                return crossLayerStyle;
            case LayerType.EAGLE_EYE_LAYER:
                GuideEagleEyeStyle eagleEyeStyle = layerBean.eagleEyeStyle;
                if (ConvertUtils.isEmpty(eagleEyeStyle)) {
                    eagleEyeStyle = new GuideEagleEyeStyle(mBizService, mapView);
                    layerBean.eagleEyeStyle = eagleEyeStyle;
                }
                return eagleEyeStyle;
            case LayerType.AREA_LAYER:
                AreaLayerStyle areaLayerStyle = layerBean.areaLayerStyle;
                if (ConvertUtils.isEmpty(areaLayerStyle)) {
                    areaLayerStyle = new AreaLayerStyle(mBizService, mapView);
                    layerBean.areaLayerStyle = areaLayerStyle;
                }
                return areaLayerStyle;
            case LayerType.ROAD_FACILITY_LAYER:
                RoadFacilityLayerStyle roadFacilityLayerStyle = layerBean.roadFacilityLayerStyle;
                if (ConvertUtils.isEmpty(roadFacilityLayerStyle)) {
                    roadFacilityLayerStyle = new RoadFacilityLayerStyle(mBizService, mapView);
                    layerBean.roadFacilityLayerStyle = roadFacilityLayerStyle;
                }
                return roadFacilityLayerStyle;
            default:
                return new BaseLayerStyle(mBizService, mapView);
        }
    }

    public void unInitLayerManager() {
        for (LayerBean layerBean : layerBeanHashtable.values()) {
            layerBean.unInitLayer();
        }
        layerBeanHashtable.clear();
        layerBeanHashtable = null;
    }

    public void registerLayerClickObserver(MapTypeId mapTypeId, @LayerType.LayerId int layerId, ILayerAdapterCallBack observer) {
        BaseLayerStyle baseLayerStyle = getLayerStyleById(mapTypeId, layerId);
        Logger.d(TAG, "registerLayerClickObserver:" + layerId);
        if (baseLayerStyle != null) {
            Logger.d(TAG, "registerLayerClickObserver success!");
            baseLayerStyle.registerLayerObserver(mapTypeId, observer);
        } else {
            Logger.d(TAG, "registerLayerClickObserver failed!");
        }
    }

    public void unRegisterLayerClickObserver(MapTypeId mapTypeId, @LayerType.LayerId int layerId, ILayerAdapterCallBack observer) {
        Logger.d(TAG, "unRegisterLayerClickObserver:" + layerId);
        BaseLayerStyle baseLayerStyle = getLayerStyleById(mapTypeId, layerId);
        if (baseLayerStyle != null) {
            baseLayerStyle.unRegisterLayerObserver(mapTypeId, observer);
        }
    }

    private static final class LayerBean {
        private MapLayerStyle mapLayerStyle;
        private SearchLayerStyle searchLayerStyle;
        private RouteLayerStyle routeLayerStyle;
        private NaviLayerStyle naviLayerStyle;
        private CrossLayerStyle crossLayerStyle;
        private GuideEagleEyeStyle eagleEyeStyle;
        private AreaLayerStyle areaLayerStyle;
        private RoadFacilityLayerStyle roadFacilityLayerStyle;

        private void unInitLayer() {
            if (!ConvertUtils.isEmpty(mapLayerStyle)) mapLayerStyle.unInit();
            if (!ConvertUtils.isEmpty(searchLayerStyle)) searchLayerStyle.unInit();
            if (!ConvertUtils.isEmpty(routeLayerStyle)) routeLayerStyle.unInit();
            if (!ConvertUtils.isEmpty(naviLayerStyle)) naviLayerStyle.unInit();
            if (!ConvertUtils.isEmpty(crossLayerStyle)) crossLayerStyle.unInit();
            if (!ConvertUtils.isEmpty(eagleEyeStyle)) eagleEyeStyle.unInit();
            if (!ConvertUtils.isEmpty(areaLayerStyle)) areaLayerStyle.unInit();
            if (!ConvertUtils.isEmpty(roadFacilityLayerStyle)) roadFacilityLayerStyle.unInit();
        }
    }

    @Nullable
    public BaseLayerStyle getLayerStyleById(MapTypeId mapTypeId, @LayerType.LayerId int layerId) {
       /* if (ConvertUtils.isEmpty(layerBeanHashtable)) {
            Logger.w(TAG, "getLayerStyleById but layerBeanHashtable is null or empty!");
            return null;
        }
        LayerBean bean = layerBeanHashtable.get(mapTypeId);
        if (bean == null) {
            Logger.w(TAG, "getLayerStyleById but bean is null!");
            return null;
        }*/
        switch (layerId) {
            case LayerType.MAP_LAYER -> {
                return getMapLayer(mapTypeId);
            }
            case LayerType.SEARCH_LAYER -> {
                return getSearchLayer(mapTypeId);
            }
            case LayerType.ROUTE_LAYER -> {
                return getRouteLayer(mapTypeId);
            }
            case LayerType.NAVI_LAYER -> {
                return getNaviLayer(mapTypeId);
            }
            case LayerType.CROSS_LAYER -> {
                return getCrossLayer(mapTypeId);
            }
            case LayerType.EAGLE_EYE_LAYER -> {
                return getEagleEyeLayer(mapTypeId);
            }
            case LayerType.AREA_LAYER -> {
                return getAreaLayer(mapTypeId);
            }
            default -> {
                return null;
            }
        }
    }
}
