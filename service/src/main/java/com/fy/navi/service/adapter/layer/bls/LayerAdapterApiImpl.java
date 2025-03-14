package com.fy.navi.service.adapter.layer.bls;

import static com.fy.navi.service.adapter.layer.bls.impl.PrepareLayerStyleInnerImpl.ROUTE_END_POINT_TEXTURE;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.Coord2DDouble;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.model.BizLayerUtil;
import com.autonavi.gbl.layer.model.InnerStyleParam;
import com.autonavi.gbl.layer.observer.PrepareLayerParam;
import com.autonavi.gbl.layer.observer.PrepareLayerParamInner;
import com.autonavi.gbl.map.MapService;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.layer.model.LayerIconAnchor;
import com.autonavi.gbl.map.layer.model.LayerIconType;
import com.autonavi.gbl.map.layer.model.LayerTexture;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.util.model.BinaryStream;
import com.autonavi.gbl.util.model.ServiceInitStatus;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.fy.navi.service.adapter.layer.ILayerApi;
import com.fy.navi.service.adapter.layer.bls.impl.PrepareLayerStyleImpl;
import com.fy.navi.service.adapter.layer.bls.impl.PrepareLayerStyleInnerImpl;
import com.fy.navi.service.adapter.layer.bls.manager.AreaLayerStyle;
import com.fy.navi.service.adapter.layer.bls.manager.CrossLayerStyle;
import com.fy.navi.service.adapter.layer.bls.manager.LayerManager;
import com.fy.navi.service.adapter.layer.bls.manager.MapLayerStyle;
import com.fy.navi.service.adapter.layer.bls.manager.RoadFacilityLayerStyle;
import com.fy.navi.service.adapter.layer.bls.manager.RouteLayerStyle;
import com.fy.navi.service.adapter.layer.bls.manager.SearchLayerStyle;
import com.fy.navi.service.adapter.layer.bls.parser.ParserStyleLayerUtils;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.adapter.navi.bls.NaviDataFormatHelper;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.layer.CarModeType;
import com.fy.navi.service.define.layer.GemLayerClickBusinessType;
import com.fy.navi.service.define.layer.LayerType;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.layer.SearchResultLayer;
import com.fy.navi.service.define.layer.bls.CarLocation;
import com.fy.navi.service.define.map.GmBizUserFavoritePoint;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.NaviLayerTexture;
import com.fy.navi.service.define.navi.NaviParkingEntity;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.engine.EnginePackage;
import com.fy.navi.service.logicpaket.position.PositionPackage;

import java.util.ArrayList;
import java.util.List;

/**
 * @Description TODO
 * @Author lww
 * @date 2024/12/8
 */
public class LayerAdapterApiImpl implements ILayerApi {
    private static final String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;
    private BizControlService mBizService;
    private MapService mMapService;
    private final LayerManager layerManager;

    public LayerAdapterApiImpl() {
        mBizService = (BizControlService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.BizControlSingleServiceID);
        mMapService = (MapService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.MapSingleServiceID);
        layerManager = new LayerManager(mBizService, mMapService);
    }

    @Override
    public void initLayerService() {
        MapTypeId[] mapTypeIds = MapTypeId.values();
        String styleBlPath = GBLCacheFilePath.BLS_ASSETS_LAYER_PATH + "style_bl.json";
        for (MapTypeId mapTypeId : mapTypeIds) {
            int engineId = EnginePackage.getInstance().getEngineID(mapTypeId);
            int eagleEyeEngineId = EnginePackage.getInstance().getEagleEyeEngineID(mapTypeId);
            boolean result = mBizService.init(engineId, styleBlPath);
            mBizService.init(eagleEyeEngineId, styleBlPath);
            Logger.i(TAG, "地图  " + engineId + "  ; Layer init result -> " + result);
            ParserStyleLayerUtils.initStyleJsonFile(engineId);
        }
    }

    @Override
    public void initInnerStyle() {
        if (ServiceInitStatus.ServiceInitDone != mMapService.isInit()) {
            Logger.i(TAG, "MapService un init");
            return;
        }
        MapTypeId[] mapTypeIds = MapTypeId.values();
        Logger.i(TAG, "Inner Style Layer init start");
        for (MapTypeId mapTypeId : mapTypeIds) {
            int engineId = EnginePackage.getInstance().getEngineID(mapTypeId);
            MapView mapView = mMapService.getMapView(engineId);
            PrepareLayerParamInner paramInner = new PrepareLayerParamInner(engineId);
            InnerStyleParam innerStyleParam = getInnerStyleParam();
            PrepareLayerStyleInnerImpl prepareLayerStyleInner = new PrepareLayerStyleInnerImpl(mapView, paramInner, innerStyleParam, mapTypeId);
            mBizService.setStyle(engineId, prepareLayerStyleInner);
        }
        Logger.i(TAG, "Inner Style Layer init end");
    }

    @Override
    public void setCustomLayerStyle() {
        if (ServiceInitStatus.ServiceInitDone != mMapService.isInit()) {
            Logger.i(TAG, "MapService un init");
            return;
        }
        MapTypeId[] mapTypeIds = MapTypeId.values();
        Logger.i(TAG, "Custom Style Layer init start");
        for (MapTypeId mapTypeId : mapTypeIds) {
            int engineId = EnginePackage.getInstance().getEngineID(mapTypeId);
            MapView mapView = mMapService.getMapView(engineId);
            PrepareLayerParam customParam = new PrepareLayerParamInner(engineId);
            InnerStyleParam innerStyleParam = getInnerStyleParam();
            PrepareLayerStyleImpl prepareLayerStyle = new PrepareLayerStyleImpl(mapTypeId, mapView, customParam, innerStyleParam);
            mBizService.setStyle(engineId, prepareLayerStyle);
        }
        Logger.i(TAG, "Custom Style Layer init end");
    }

    @Override
    public void unInitLayerService() {
        mMapService = null;
        layerManager.unInitLayerManager();
        for (MapTypeId mapTypeId : MapTypeId.values()) {
            int engineId = EnginePackage.getInstance().getEngineID(mapTypeId);
            mBizService.setStyle(engineId, null);
            mBizService.unInit(engineId);
        }
        mBizService = null;
        ServiceMgr.getServiceMgrInstance().removeBLService(SingleServiceID.BizControlSingleServiceID);
    }

    @Override
    public void setDefaultCarMode(MapTypeId mapTypeId) {
        CarLocation carLocation = new CarLocation();
        CarLocation.PathMatchInfos pathMatchInfos = new CarLocation.PathMatchInfos(PositionPackage.getInstance().getLastCarLocation().getLongitude(),
                PositionPackage.getInstance().getLastCarLocation().getLatitude(), 0);
        carLocation.vecPathMatchInfo.add(pathMatchInfos);
        setCarPosition(mapTypeId, carLocation);
        setCarModeVisible(mapTypeId, true);
        setCarModeClickable(mapTypeId, false);
        setCarMode(mapTypeId, CarModeType.CAR_MODEL_TYPE_SKELETON);
    }

    @Override
    public void setCarModeVisible(MapTypeId mapTypeId, boolean isVisible) {
        MapLayerStyle mapLayerStyle = layerManager.getMapLayer(mapTypeId);
        mapLayerStyle.setCarModeVisible(isVisible);
    }

    @Override
    public void setCarModeClickable(MapTypeId mapTypeId, boolean bClickable) {
        MapLayerStyle mapLayerStyle = layerManager.getMapLayer(mapTypeId);
        mapLayerStyle.setCarModeClickable(bClickable);
    }

    @Override
    public void setCarMode(MapTypeId mapTypeId, int carMode) {
        MapLayerStyle mapLayerStyle = layerManager.getMapLayer(mapTypeId);
        mapLayerStyle.setCarMode(carMode);
    }

    @Override
    public int getCarMode(MapTypeId mapTypeId) {
        MapLayerStyle mapLayerStyle = layerManager.getMapLayer(mapTypeId);
        return mapLayerStyle.getCarMode();
    }

    @Override
    public int getCurrentCarModeType(MapTypeId mapTypeId) {
        MapLayerStyle mapLayerStyle = layerManager.getMapLayer(mapTypeId);
        return mapLayerStyle.getCurrentCarModeType();
    }

    @Override
    public void setCarUpMode(MapTypeId mapTypeId, boolean bCarUp) {
        MapLayerStyle mapLayerStyle = layerManager.getMapLayer(mapTypeId);
        mapLayerStyle.setCarUpMode(bCarUp);
    }

    @Override
    public boolean setCarScaleByMapLevel(MapTypeId mapTypeId, float[] vScales) {
        MapLayerStyle mapLayerStyle = layerManager.getMapLayer(mapTypeId);
        return mapLayerStyle.setCarScaleByMapLevel(vScales);
    }

    @Override
    public void setCarPosition(MapTypeId mapTypeId, CarLocation carLocation) {
        MapLayerStyle mapLayerStyle = layerManager.getMapLayer(mapTypeId);
        mapLayerStyle.setCarPosition(carLocation);
    }

    @Override
    public void updateCarPosition(MapTypeId mapTypeId, CarLocation carLocation) {
        MapLayerStyle mapLayerStyle = layerManager.getMapLayer(mapTypeId);
        mapLayerStyle.updateCarPosition(carLocation);
    }

    @Override
    public int setFollowMode(MapTypeId mapTypeId, boolean bFollow) {
        MapLayerStyle mapLayerStyle = layerManager.getMapLayer(mapTypeId);
        return mapLayerStyle.setFollowMode(bFollow);
    }

    @Override
    public void addSearchPointMarker(MapTypeId mapTypeId, SearchResultLayer searchResultLayer) {
        SearchLayerStyle searchLayerStyle = layerManager.getSearchLayer(mapTypeId);
        searchLayerStyle.updateSearchPoiLayer(searchResultLayer);
    }

    @Override
    public boolean addSearchLabelMarker(MapTypeId mapTypeId, SearchResultLayer.ChildPoint childPoint) {
        SearchLayerStyle searchLayerStyle = layerManager.getSearchLayer(mapTypeId);
        return searchLayerStyle.updateSearchPoiLabel(childPoint);
    }

    @Override
    public void clearSearchAllItemLayer(MapTypeId mapTypeId) {
        SearchLayerStyle searchLayerStyle = layerManager.getSearchLayer(mapTypeId);
        searchLayerStyle.clearSearchAllItemLayer();
    }

    @Override
    public PreviewParams getPathResultBound(MapTypeId mapTypeId, ArrayList<?> pathResult) {
        RouteLayerStyle routeLayerStyle = layerManager.getRouteLayer(mapTypeId);
        return routeLayerStyle.getPathResultBound(pathResult);
    }

    @Override
    public void drawRouteLine(RouteLineLayerParam routeLineLayer) {
        // 这里要给所有屏幕都加上线路绘制，如果不需要请自己处理
        MapTypeId[] ids = MapTypeId.values();
        if (!ConvertUtils.isEmpty(ids)) {
            for (MapTypeId mapTypeId : ids) {
                RouteLayerStyle routeLayerStyle = layerManager.getRouteLayer(mapTypeId);
                routeLayerStyle.drawRoute(routeLineLayer);
            }
        }
    }

    @Override
    public void setSelectedPathIndex(MapTypeId mapTypeId, int routeIndex) {
        MapTypeId[] ids = MapTypeId.values();
        if (!ConvertUtils.isEmpty(ids)) {
            for (MapTypeId typeId : ids) {
                RouteLayerStyle routeLayerStyle = layerManager.getRouteLayer(typeId);
                routeLayerStyle.setSelectedPathIndex(routeIndex);
            }
        }
    }

    @Override
    public void clearRouteLine(MapTypeId mapTypeId) {
        // 这里要给所有屏幕都加上，如果不需要请自己处理
        MapTypeId[] ids = MapTypeId.values();
        if (!ConvertUtils.isEmpty(ids)) {
            for (MapTypeId typeId : ids) {
                RouteLayerStyle routeLayerStyle = layerManager.getRouteLayer(typeId);
                routeLayerStyle.clearPaths();
                //释放终点动态纹理
                int engineId = EnginePackage.getInstance().getEngineID(mapTypeId);
                MapView mapView = mMapService.getMapView(engineId);
                mapView.destroyTexture(getDynamicsMarkerId(ROUTE_END_POINT_TEXTURE));
            }
        }
    }

    /**
     * @return markerId
     * @brief 获取动态markerId
     */
    private int getDynamicsMarkerId(int index) {
        return 0x660000 + 0x60000 + index;
    }

    @Override
    public void showRestArea(MapTypeId mapTypeId, ArrayList<?> pathInfoList, int index) {
        // 这里要给所有屏幕都加上，如果不需要请自己处理
        MapTypeId[] ids = MapTypeId.values();
        if (!ConvertUtils.isEmpty(ids)) {
            for (MapTypeId typeId : ids) {
                RouteLayerStyle routeLayerStyle = layerManager.getRouteLayer(typeId);
                routeLayerStyle.showRestArea(pathInfoList, index);
            }
        }
    }

    @Override
    public void showWeatherView(MapTypeId mapTypeId, ArrayList<?> weatherLabelItem) {
        // 这里要给所有屏幕都加上，如果不需要请自己处理
        MapTypeId[] ids = MapTypeId.values();
        if (!ConvertUtils.isEmpty(ids)) {
            for (MapTypeId typeId : ids) {
                RouteLayerStyle routeLayerStyle = layerManager.getRouteLayer(typeId);
                routeLayerStyle.showWeatherView(weatherLabelItem);
            }
        }
    }

    @Override
    public void showRestrictionView(MapTypeId mapTypeId, Object object, int position) {
        // 这里要给所有屏幕都加上，如果不需要请自己处理
        MapTypeId[] ids = MapTypeId.values();
        if (!ConvertUtils.isEmpty(ids)) {
            for (MapTypeId typeId : ids) {
                AreaLayerStyle areaLayerStyle = layerManager.getAreaLayer(typeId);
                areaLayerStyle.showRestrictionView(object, position);
            }
        }
    }

    @Override
    public boolean switchSelectedPath(MapTypeId mapTypeId, int index) {
        RouteLayerStyle routeLayerStyle = layerManager.getRouteLayer(mapTypeId);
        return routeLayerStyle.switchSelectedPath(index);
    }

    @Override
    public void updatePathArrow(MapTypeId mapTypeId) {
        // 这里要给所有屏幕都加上，如果不需要请自己处理
        MapTypeId[] ids = MapTypeId.values();
        if (!ConvertUtils.isEmpty(ids)) {
            for (MapTypeId typeId : ids) {
                RouteLayerStyle routeLayerStyle = layerManager.getRouteLayer(typeId);
                routeLayerStyle.updatePathArrow();
            }
        }
    }

    @Override
    public String getCurrentRouteTime(MapTypeId mapTypeId) {
        RouteLayerStyle routeLayerStyle = layerManager.getRouteLayer(mapTypeId);
        return routeLayerStyle.getCurrentRouteTime();
    }

    @Override
    public void setPathArrowSegment(MapTypeId mapTypeId, ArrayList<Long> segmentsIndexs) {
        // 这里要给所有屏幕都加上，如果不需要请自己处理
        MapTypeId[] ids = MapTypeId.values();
        if (!ConvertUtils.isEmpty(ids)) {
            for (MapTypeId typeId : ids) {
                RouteLayerStyle routeLayerStyle = layerManager.getRouteLayer(typeId);
                routeLayerStyle.setPathArrowSegment(segmentsIndexs);
            }
        }
    }

    @Override
    public boolean showCross(MapTypeId mapTypeId, CrossImageEntity crossInfo) {
        boolean ret = false;
        CrossLayerStyle crossLayer = layerManager.getCrossLayer(mapTypeId);
        if (crossInfo.getType() == NaviConstant.CrossType.CrossTypeVector || crossInfo.getType() == NaviConstant.CrossType.CrossType3D) {
            //矢量图或者三维图
            ret = crossLayer.updateCross(crossInfo.getDataBuf(), crossInfo.getType());
        } else if (crossInfo.getType() == NaviConstant.CrossType.CrossTypeGrid) {
            ret = crossLayer.setRasterImageData(getArrowRoadImage(true, crossInfo), getArrowRoadImage(false, crossInfo));
        }
        Logger.i(TAG, "showCross ret：" + ret);
        crossLayer.showCross(crossInfo.getType());
        return ret;
    }

    @Override
    public void hideCross(MapTypeId mapTypeId, int type) {
        CrossLayerStyle crossLayer = layerManager.getCrossLayer(mapTypeId);
        crossLayer.hideCross(type);
    }

    @Override
    public void setVisible(MapTypeId mapTypeId, int type, boolean bVisible) {
        CrossLayerStyle crossLayer = layerManager.getCrossLayer(mapTypeId);
        crossLayer.setVisible(type, bVisible);
    }

    @Override
    public void setCrossImageInfo(MapTypeId mapTypeId, CrossImageEntity crossInfo) {
        CrossLayerStyle crossLayer = layerManager.getCrossLayer(mapTypeId);
        crossLayer.setCrossImageInfo(NaviDataFormatHelper.getCrossImageInfo(crossInfo));
    }

    @Override
    public boolean updateCross(MapTypeId mapTypeId, byte[] buff, int crossType) {
        CrossLayerStyle crossLayer = layerManager.getCrossLayer(mapTypeId);
        return crossLayer.updateCross(buff, crossType);
    }

    @Override
    public boolean setRasterImageData(MapTypeId mapTypeId, NaviLayerTexture arrowImage, NaviLayerTexture roadImage) {
        CrossLayerStyle crossLayer = layerManager.getCrossLayer(mapTypeId);
        return crossLayer.setRasterImageData(getLayerTexture(arrowImage), getLayerTexture(roadImage));
    }

    @Override
    public void clearSearchMarks(MapTypeId mapTypeId) {
        SearchLayerStyle searchLayerStyle = layerManager.getSearchLayer(mapTypeId);
        searchLayerStyle.clearSearchMarks();
    }

    private LayerTexture getArrowRoadImage(boolean isArrow, CrossImageEntity info) {
        int RES_ID = 888888888;
        LayerTexture image = new LayerTexture();
        image.dataBuff = new BinaryStream(isArrow ? info.getArrowDataBuf() : info.getDataBuf());
        //栅格图箭头png
        image.iconType = isArrow ? LayerIconType.LayerIconTypePNG : LayerIconType.LayerIconTypeJPG;
        image.resID = RES_ID;
        image.isGenMipmaps = false;
        image.isPreMulAlpha = true;
        image.isRepeat = false;
        image.anchorType = LayerIconAnchor.LayerIconAnchorLeftTop;
        return image;
    }

    private LayerTexture getLayerTexture(NaviLayerTexture naviLayerTexture) {
        LayerTexture layerTexture = new LayerTexture();
        layerTexture.resID = naviLayerTexture.resID;
        layerTexture.dataBuff = new BinaryStream(naviLayerTexture.dataBuff);
        layerTexture.anchorType = naviLayerTexture.anchorType;
        layerTexture.width = naviLayerTexture.width;
        layerTexture.height = naviLayerTexture.height;
        layerTexture.xRatio = naviLayerTexture.xRatio;
        layerTexture.yRatio = naviLayerTexture.yRatio;
        layerTexture.iconType = naviLayerTexture.iconType;
        layerTexture.isGenMipmaps = naviLayerTexture.isGenMipmaps;
        layerTexture.isRepeat = naviLayerTexture.isRepeat;
        layerTexture.errorCode = naviLayerTexture.errorCode;
        layerTexture.name = naviLayerTexture.name;
        layerTexture.isPreMulAlpha = naviLayerTexture.isPreMulAlpha;
        return layerTexture;
    }

    private InnerStyleParam getInnerStyleParam() {
        InnerStyleParam param = new InnerStyleParam();
        String path = GBLCacheFilePath.BLS_ASSETS_LAYER_PATH;
        param.layerAssetPath = path;
        param.cardCmbPaths.add(path);
        return param;
    }

    @Override
    public void openDynamicLevel(MapTypeId mapTypeId, boolean isOpen) {
        RouteLayerStyle routeLayerStyle = layerManager.getRouteLayer(mapTypeId);
        routeLayerStyle.openDynamicLevel(isOpen);
    }

    @Override
    public void updateSearchParkPoi(MapTypeId mapTypeId, ArrayList<NaviParkingEntity> parkList) {
        SearchLayerStyle searchLayerStyle = layerManager.getSearchLayer(mapTypeId);
        searchLayerStyle.updateSearchParkPoi(parkList);
    }

    @Override
    public void clearSearchParkPoi(MapTypeId mapTypeId) {
        SearchLayerStyle searchLayerStyle = layerManager.getSearchLayer(mapTypeId);
        searchLayerStyle.clearSearchParkPoi();
    }

    @Override
    public void setParkFocus(MapTypeId mapTypeId, String strID, boolean bFocus) {
        SearchLayerStyle searchLayerStyle = layerManager.getSearchLayer(mapTypeId);
        searchLayerStyle.setParkFocus(strID, bFocus);
    }

    @Override
    public void updateGuideCarStyle(MapTypeId mapTypeId) {
        MapLayerStyle mapLayerStyle = layerManager.getMapLayer(mapTypeId);
        mapLayerStyle.updateGuideCarStyle();
    }

    @Override
    public void setVisibleCruiseSignalLight(MapTypeId mapTypeId, boolean isVisible) {
        RoadFacilityLayerStyle layerStyle = layerManager.getRoadFacilityLayer(mapTypeId);
        layerStyle.setVisibleCruiseSignalLight(isVisible);
    }

    @Override
    public void setVisibleGuideSignalLight(MapTypeId mapTypeId, boolean isVisible) {
        RoadFacilityLayerStyle layerStyle = layerManager.getRoadFacilityLayer(mapTypeId);
        layerStyle.setVisibleGuideSignalLight(isVisible);
    }

    @Override
    public double calcStraightDistance(GeoPoint startPoint, GeoPoint endPoint) {
        Coord2DDouble startP = new Coord2DDouble(startPoint.getLon(), startPoint.getLat());
        Coord2DDouble endP = new Coord2DDouble(endPoint.getLon(), endPoint.getLat());
        return BizLayerUtil.calcDistanceBetweenPoints(startP, endP);
    }

    @Override
    public void selectSearchPoi(MapTypeId mapTypeId, GemLayerClickBusinessType type, String strID, boolean bFocus) {
        SearchLayerStyle searchLayerStyle = layerManager.getSearchLayer(mapTypeId);
        searchLayerStyle.setSelect(type, strID, bFocus);
    }

    @Override
    public void registerLayerClickObserver(MapTypeId mapTypeId, @LayerType.LayerId int layerId, ILayerAdapterCallBack observer) {
        layerManager.registerLayerClickObserver(mapTypeId, layerId, observer);
    }

    @Override
    public void unRegisterLayerClickObserver(MapTypeId mapTypeId, @LayerType.LayerId int layerId, ILayerAdapterCallBack observer) {
        layerManager.unRegisterLayerClickObserver(mapTypeId, layerId, observer);
    }

    @Override
    public void updateFavoriteMain(MapTypeId mapTypeId, List<GmBizUserFavoritePoint> list) {
        layerManager.getMapLayer(mapTypeId).updateFavoriteMain(list);
    }

    @Override
    public void clearFavoriteMain(MapTypeId mapTypeId) {
        layerManager.getMapLayer(mapTypeId).clearFavoriteMain();
    }

    @Override
    public int setDynamicLevelLock(MapTypeId mapTypeId, boolean isLock, int type) {
        return layerManager.getRouteLayer(mapTypeId).setDynamicLevelLock(isLock, type);
    }

    @Override
    public void resetDynamicLevel(MapTypeId mapTypeId, int type) {
        layerManager.getRouteLayer(mapTypeId).resetDynamicLevel(type);
    }

    @Override
    public boolean getDynamicLevelLock(MapTypeId mapTypeId, int type) {
        return layerManager.getRouteLayer(mapTypeId).getDynamicLevelLock(type);
    }

    @Override
    public float getDynamicLevelMapHeadDegree(MapTypeId mapTypeId, int type) {
        return layerManager.getRouteLayer(mapTypeId).getDynamicLevelMapHeadDegree(type);
    }

    @Override
    public int openDynamicCenter(MapTypeId mapTypeId, boolean changeCenter) {
        return layerManager.getRouteLayer(mapTypeId).openDynamicCenter(changeCenter);
    }
}
