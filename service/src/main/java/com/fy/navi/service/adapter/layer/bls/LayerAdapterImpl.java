package com.fy.navi.service.adapter.layer.bls;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.Coord2DDouble;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.model.BizLayerUtil;
import com.autonavi.gbl.map.MapService;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.util.model.ServiceInitStatus;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.engine.EngineAdapter;
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.fy.navi.service.adapter.layer.ILayerApi;
import com.fy.navi.service.adapter.layer.bls.refix.LayersWrapper;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.layer.CarModeType;
import com.fy.navi.service.define.layer.GemLayerClickBusinessType;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.layer.SearchResultLayer;
import com.fy.navi.service.define.layer.bls.CarLocation;
import com.fy.navi.service.define.layer.refix.LayerItemSearchAlongWay;
import com.fy.navi.service.define.layer.refix.LayerItemSearchAlongWayPop;
import com.fy.navi.service.define.layer.refix.LayerItemSearchBegin;
import com.fy.navi.service.define.layer.refix.LayerItemSearchChargeStation;
import com.fy.navi.service.define.layer.refix.LayerItemSearchChild;
import com.fy.navi.service.define.layer.refix.LayerItemSearchEnd;
import com.fy.navi.service.define.layer.refix.LayerItemSearchEntrance;
import com.fy.navi.service.define.layer.refix.LayerItemSearchExit;
import com.fy.navi.service.define.layer.refix.LayerItemSearchLabel;
import com.fy.navi.service.define.layer.refix.LayerItemSearchParent;
import com.fy.navi.service.define.layer.refix.LayerItemSearchPark;
import com.fy.navi.service.define.layer.refix.LayerItemSearchVia;
import com.fy.navi.service.define.map.GmBizUserFavoritePoint;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.NaviLayerTexture;
import com.fy.navi.service.define.navi.NaviParkingEntity;
import com.fy.navi.service.logicpaket.position.PositionPackage;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * @Description TODO
 * @Author lww
 * @date 2024/12/8
 */
public class LayerAdapterImpl implements ILayerApi {
    private static final String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;

    private BizControlService mBizControlService;

    private MapService mMapService;

    private HashMap<MapTypeId, LayersWrapper> mLayersWrapper = new HashMap<>();

    public LayerAdapterImpl() {
        mBizControlService = (BizControlService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.BizControlSingleServiceID);
        mMapService = (MapService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.MapSingleServiceID);
        ConvertUtils.isNullRequire(mBizControlService, "Init layer load fail");
        ConvertUtils.isNullRequire(mMapService, "Init MapService load fail");
    }

    @Override
    public boolean initLayerService(MapTypeId mapTypeId) {
        String styleBlPath = EngineAdapter.getInstance().styleBlPath(mapTypeId);
        int engineId = EngineAdapter.getInstance().engineID(mapTypeId);
        int eagleEyeEngineId = EngineAdapter.getInstance().eagleEyeEngineID(mapTypeId);
        boolean result = mBizControlService.init(engineId, styleBlPath);
        Logger.d(TAG, "engineId :" + engineId + " ;result :" + result);
        result = mBizControlService.init(eagleEyeEngineId, styleBlPath);
        Logger.d(TAG, "eagleEyeEngineId :" + eagleEyeEngineId + " ;result :" + result);
        if (mLayersWrapper.get(mapTypeId) == null) {
            mLayersWrapper.put(mapTypeId, new LayersWrapper(mBizControlService, mMapService.getMapView(engineId), AppContext.getInstance().getMContext()));
        }
        result = mBizControlService.isInit() == ServiceInitStatus.ServiceInitDone;
        Logger.d(TAG, "initLayerServiceresult :" + result);
        return result;
    }

    @Override
    public void initInnerStyle() {

    }

    @Override
    public void setCustomLayerStyle() {

    }

    @Override
    public void unInitLayerService() {

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
        mLayersWrapper.get(mapTypeId).getLayerCar().setCarModeVisible(isVisible);
    }

    @Override
    public void setCarModeClickable(MapTypeId mapTypeId, boolean bClickable) {

        mLayersWrapper.get(mapTypeId).getLayerCar().setCarModeClickable(bClickable);
    }

    @Override
    public void setCarMode(MapTypeId mapTypeId, int carMode) {
        mLayersWrapper.get(mapTypeId).getLayerCar().setCarMode(carMode);
    }

    @Override
    public int getCarMode(MapTypeId mapTypeId) {
        return mLayersWrapper.get(mapTypeId).getLayerCar().getCarMode();
    }

    @Override
    public int getCurrentCarModeType(MapTypeId mapTypeId) {
        return mLayersWrapper.get(mapTypeId).getLayerCar().getCurrentCarModeType();
    }

    @Override
    public void setCarUpMode(MapTypeId mapTypeId, boolean bCarUp) {
        mLayersWrapper.get(mapTypeId).getLayerCar().setCarUpMode(bCarUp);
    }

    @Override
    public boolean setCarScaleByMapLevel(MapTypeId mapTypeId, float[] vScales) {
        return mLayersWrapper.get(mapTypeId).getLayerCar().setCarScaleByMapLevel(vScales);
    }

    @Override
    public void setCarPosition(MapTypeId mapTypeId, CarLocation carLocation) {
        mLayersWrapper.get(mapTypeId).getLayerCar().setCarPosition(carLocation);
    }

    @Override
    public void updateCarPosition(MapTypeId mapTypeId, CarLocation carLocation) {
        mLayersWrapper.get(mapTypeId).getLayerCar().updateCarPosition(carLocation);
    }

    @Override
    public int setFollowMode(MapTypeId mapTypeId, boolean bFollow) {
        return mLayersWrapper.get(mapTypeId).getLayerCar().setFollowMode(bFollow);
    }

    @Override
    public void addSearchPointMarker(MapTypeId mapTypeId, SearchResultLayer searchResultLayer) {
    }

    @Override
    public boolean addSearchLabelMarker(MapTypeId mapTypeId, SearchResultLayer.ChildPoint childPoint) {
        return true;
    }

    @Override
    public void clearSearchAllItemLayer(MapTypeId mapTypeId) {
    }

    @Override
    public PreviewParams getPathResultBound(MapTypeId mapTypeId, ArrayList<?> pathResult) {
        return new PreviewParams();
    }

    @Override
    public void drawRouteLine(MapTypeId mapTypeId, RouteLineLayerParam routeLineLayer) {
        // 这里要给所有屏幕都加上线路绘制，如果不需要请自己处理
    }

    @Override
    public void setSelectedPathIndex(MapTypeId mapTypeId, int routeIndex) {
    }

    @Override
    public void clearRouteLine(MapTypeId mapTypeId) {
        // 这里要给所有屏幕都加上，如果不需要请自己处理
    }

    @Override
    public void showRestArea(MapTypeId mapTypeId, ArrayList<?> pathInfoList, int index) {
        // 这里要给所有屏幕都加上，如果不需要请自己处理
    }

    @Override
    public void showWeatherView(MapTypeId mapTypeId, ArrayList<?> weatherLabelItem) {
        // 这里要给所有屏幕都加上，如果不需要请自己处理
    }

    @Override
    public void showRestrictionView(MapTypeId mapTypeId, Object object, int position) {
        // 这里要给所有屏幕都加上，如果不需要请自己处理
        mLayersWrapper.get(mapTypeId).getLayerArea().showRestrictionView(object, position);
    }

    @Override
    public boolean switchSelectedPath(MapTypeId mapTypeId, int index) {
        return true;
    }

    @Override
    public void updatePathArrow(MapTypeId mapTypeId) {
        // 这里要给所有屏幕都加上，如果不需要请自己处理
        mLayersWrapper.get(mapTypeId).getLayerGuideRoute().updatePathArrow();
    }

    @Override
    public String getCurrentRouteTime(MapTypeId mapTypeId) {
        return null;
    }

    @Override
    public void setPathArrowSegment(MapTypeId mapTypeId, ArrayList<Long> segmentsIndexs) {
        // 这里要给所有屏幕都加上，如果不需要请自己处理
    }

    @Override
    public boolean showCross(MapTypeId mapTypeId, CrossImageEntity crossInfo) {
        return true;
    }

    @Override
    public void hideCross(MapTypeId mapTypeId, int type) {
    }

    @Override
    public void setVisible(MapTypeId mapTypeId, int type, boolean bVisible) {
    }

    @Override
    public void setCrossImageInfo(MapTypeId mapTypeId, CrossImageEntity crossInfo) {
    }

    @Override
    public boolean updateCross(MapTypeId mapTypeId, byte[] buff, int crossType) {
        return true;
    }

    @Override
    public boolean setRasterImageData(MapTypeId mapTypeId, NaviLayerTexture arrowImage, NaviLayerTexture roadImage) {
        return true;
    }

    @Override
    public void clearSearchMarks(MapTypeId mapTypeId) {
    }

    @Override
    public void openDynamicLevel(MapTypeId mapTypeId, boolean isOpen) {
    }

    @Override
    public void registerLayerClickObserver(MapTypeId mapTypeId, int layerId, ILayerAdapterCallBack observer) {

    }

    @Override
    public void unRegisterLayerClickObserver(MapTypeId mapTypeId, int layerId, ILayerAdapterCallBack observer) {

    }

    @Override
    public void updateSearchParkPoi(MapTypeId mapTypeId, ArrayList<NaviParkingEntity> parkList) {
    }

    @Override
    public void clearSearchParkPoi(MapTypeId mapTypeId) {
    }

    @Override
    public void setParkFocus(MapTypeId mapTypeId, String strID, boolean bFocus) {
    }

    @Override
    public void updateGuideCarStyle(MapTypeId mapTypeId) {
        mLayersWrapper.get(mapTypeId).getLayerCar().updateGuideCarStyle();
    }

    @Override
    public void setVisibleCruiseSignalLight(MapTypeId mapTypeId, boolean isVisible) {
    }

    @Override
    public void setVisibleGuideSignalLight(MapTypeId mapTypeId, boolean isVisible) {
    }

    @Override
    public double calcStraightDistance(GeoPoint startPoint, GeoPoint endPoint) {
        Coord2DDouble startP = new Coord2DDouble(startPoint.getLon(), startPoint.getLat());
        Coord2DDouble endP = new Coord2DDouble(endPoint.getLon(), endPoint.getLat());
        return BizLayerUtil.calcDistanceBetweenPoints(startP, endP);
    }

    @Override
    public void selectSearchPoi(MapTypeId mapTypeId, GemLayerClickBusinessType type, String strID, boolean bFocus) {
        mLayersWrapper.get(mapTypeId).getLayerSearch().setSelect(type, strID, bFocus);
    }


    @Override
    public void updateFavoriteMain(MapTypeId mapTypeId, List<GmBizUserFavoritePoint> list) {
        mLayersWrapper.get(mapTypeId).getLayerUser().updateFavoriteMain(list);
    }

    @Override
    public void clearFavoriteMain(MapTypeId mapTypeId) {
        mLayersWrapper.get(mapTypeId).getLayerUser().clearFavoriteMain();
    }

    @Override
    public int setDynamicLevelLock(MapTypeId mapTypeId, boolean isLock, int type) {
        return 0;
    }

    @Override
    public void resetDynamicLevel(MapTypeId mapTypeId, int type) {
    }

    @Override
    public boolean getDynamicLevelLock(MapTypeId mapTypeId, int type) {
        return true;
    }

    @Override
    public float getDynamicLevelMapHeadDegree(MapTypeId mapTypeId, int type) {
        return 0;
    }

    @Override
    public boolean addLayerItemOfSearchResult(MapTypeId mapTypeId, ArrayList<LayerItemSearchParent> parentPoints, ArrayList<LayerItemSearchChild> childPoints, ArrayList<LayerItemSearchExit> exitPoints, ArrayList<LayerItemSearchEntrance> entrancePoints, boolean clearOtherLayerItem) {
        return false;
    }

    @Override
    public boolean addLayerItemOfBeginEnd(MapTypeId mapTypeId, LayerItemSearchBegin beginPoint, ArrayList<LayerItemSearchVia> viaPoints, LayerItemSearchEnd endPoint, boolean clearOtherLayerItem) {
        return false;
    }

    @Override
    public boolean addLayerItemOfAlongRoute(MapTypeId mapTypeId, ArrayList<LayerItemSearchAlongWay> alongWayPoints, ArrayList<LayerItemSearchAlongWayPop> alongWayPops, boolean clearOtherLayerItem) {
        return false;
    }

    @Override
    public boolean addLayerItemOfChargeStation(MapTypeId mapTypeId, ArrayList<LayerItemSearchChargeStation> points, boolean clearOtherLayerItem) {
        return false;
    }

    @Override
    public boolean addLayerItemOfPark(MapTypeId mapTypeId, ArrayList<LayerItemSearchPark> points, boolean clearOtherLayerItem) {
        return false;
    }

    @Override
    public boolean addLayerItemOfLabel(MapTypeId mapTypeId, LayerItemSearchLabel label, boolean clearOtherLayerItem) {
        return false;
    }

    @Override
    public int openDynamicCenter(MapTypeId mapTypeId, boolean changeCenter) {
        return 0;
    }

}
