package com.sgm.navi.service.adapter.layer.bls;

import android.graphics.Rect;

import com.sgm.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.sgm.navi.service.adapter.layer.ILayerApi;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.bean.PreviewParams;
import com.sgm.navi.service.define.layer.refix.CarModeType;
import com.sgm.navi.service.define.layer.refix.DynamicLevelMode;
import com.sgm.navi.service.define.layer.refix.LayerItemCrossEntity;
import com.sgm.navi.service.define.layer.refix.LayerItemLabelResult;
import com.sgm.navi.service.define.layer.refix.LayerItemRouteEndPoint;
import com.sgm.navi.service.define.layer.refix.LayerItemRouteOdd;
import com.sgm.navi.service.define.layer.refix.LayerItemSearchResult;
import com.sgm.navi.service.define.layer.refix.LayerItemUserFavorite;
import com.sgm.navi.service.define.layer.refix.LayerItemUserTrackDepth;
import com.sgm.navi.service.define.layer.refix.LayerPointItemType;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.route.RequestRouteResult;
import com.sgm.navi.service.define.route.RouteAlterChargeStationInfo;
import com.sgm.navi.service.define.route.RouteChargeStationParam;
import com.sgm.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;
import java.util.List;

/**
 * 空实现
 */
public class LayerAdapterImplD implements ILayerApi {
    @Override
    public boolean initLayerService() {
        return true;
    }

    @Override
    public boolean initLayer(MapType mapType) {
        return true;
    }

    @Override
    public boolean unInitLayer(MapType mapType) {
        return true;
    }

    @Override
    public void unInitLayerService() {

    }

    /* 路线全览 */
    public void showPreviewView(MapType mapTypeId) {

    }

    @Override
    public void drawRouteLine(MapType mapTypeId, RequestRouteResult routeResult) {

    }

    @Override
    public void updateRouteReplaceChargePoints(MapType mapTypeId, ArrayList<RouteAlterChargeStationInfo> chargeStationInfos) {

    }

    @Override
    public void updateRouteEndPoint(MapType mapTypeId, LayerItemRouteEndPoint endPoint) {

    }

    @Override
    public void updateRouteChargeStation(MapType mapTypeId, RouteChargeStationParam routeChargeStation) {

    }

    @Override
    public void updateViaPointList(MapType mapTypeId, List<PoiInfoEntity> viaPointList) {

    }

    @Override
    public void updateOddInfo(MapType mapTypeId, ArrayList<LayerItemRouteOdd> oddInfoList, long pathId) {

    }

    @Override
    public void setSelectedPathIndex(MapType mapTypeId, int routeIndex) {

    }

    @Override
    public void clearRouteLine(MapType mapTypeId) {

    }

    @Override
    public void clearRouteItemByType(MapType mapTypeId, LayerPointItemType type) {

    }

    @Override
    public void showRestArea(MapType mapTypeId, ArrayList<?> pathInfoList, int index) {

    }

    @Override
    public void showWeatherView(MapType mapTypeId, ArrayList<?> weatherLabelItem) {

    }

    @Override
    public void showRestrictionView(MapType mapTypeId, Object object, int position) {

    }

    @Override
    public boolean switchSelectedPath(MapType mapTypeId, int index) {
        return true;
    }

    @Override
    public void updatePathArrow(MapType mapTypeId) {

    }

    @Override
    public void setPathArrowSegment(MapType mapTypeId, ArrayList<Long> segmentsIndexs) {

    }

    @Override
    public boolean setRouteJamBubblesVisible(MapType mapTypeId, boolean isShow) {
        return true;
    }

    @Override
    public void setPathStyle(MapType mapTypeId, boolean isStartNavi, boolean isOffLine, boolean isMultipleMode) {

    }

    @Override
    public boolean setPathVisible(MapType mapTypeId, int index, boolean isVisible) {
        return true;
    }

    @Override
    public boolean setPathVisible(MapType mapTypeId, long pathId, boolean isVisible) {
        return true;
    }

    @Override
    public boolean updatePathInfo(MapType mapTypeId, ArrayList<?> pathInfoList, int selectIndex) {
        return true;
    }

    @Override
    public void removeViaPoint(MapType mapTypeId, String pid) {

    }

    @Override
    public void setStartPointVisible(MapType mapTypeId, boolean visible) {

    }

    @Override
    public void setRouteViaPointSelectStatus(MapType mapTypeId, boolean isSelect, int index) {

    }

    @Override
    public void setRouteEnergyEmptyPointVisible(MapType mapTypeId, boolean isShow) {

    }

    @Override
    public void registerLayerClickObserver(MapType mapTypeId, ILayerAdapterCallBack observer) {

    }

    @Override
    public void unRegisterLayerClickObserver(MapType mapTypeId, ILayerAdapterCallBack observer) {

    }

    @Override
    public void updateGuideCarStyle(MapType mapTypeId) {

    }

    @Override
    public void setVisibleCruiseSignalLight(MapType mapTypeId, boolean isVisible) {

    }

    @Override
    public void setVisibleGuideSignalLight(MapType mapTypeId, boolean isVisible) {

    }

    @Override
    public double calcStraightDistance(GeoPoint startPoint, GeoPoint endPoint) {
        return 0;
    }

    @Override
    public void clearFavoriteMain(MapType mapTypeId) {

    }

    @Override
    public void openDynamicLevel(MapType mapTypeId, DynamicLevelMode dynamicLevelMode) {

    }

    @Override
    public void closeDynamicLevel(MapType mapTypeId) {

    }

    @Override
    public void setDynamicLevelLock(MapType mapTypeId, DynamicLevelMode dynamicLevelMode, boolean isLock) {

    }

    @Override
    public void openDynamicCenter(MapType mapTypeId, boolean isDynaCenterLock) {

    }

    @Override
    public void selectSearchPoi(MapType mapTypeId, LayerPointItemType type, int index) {

    }

    @Override
    public void clearFocus(MapType mapTypeId, LayerPointItemType type) {

    }

    @Override
    public boolean updateSearchMarker(MapType mapTypeId, LayerPointItemType type, LayerItemSearchResult searchResult, boolean clearOtherLayerItem) {
        return true;
    }

    @Override
    public void updateSearchResult(MapType mapTypeId, LayerPointItemType type, LayerItemSearchResult result) {

    }

    @Override
    public void clearAllSearchLayerItems(MapType mapTypeId) {

    }

    @Override
    public void clearSearchPOILayerItems(MapType mapTypeId, LayerPointItemType searchItemType) {

    }

    @Override
    public void setDefaultCarMode(MapType mapTypeId) {

    }

    @Override
    public void setCarMode(MapType mapTypeId, CarModeType carMode) {

    }

    @Override
    public void initCarLogoByFlavor(MapType mapTypeId, String flavor) {

    }

    @Override
    public CarModeType getCarModeType(MapType mapTypeId) {
        return null;
    }

    @Override
    public void setCarLogoVisible(MapType mapTypeId, boolean visible) {

    }

    @Override
    public void setPreviewMode(MapType mapTypeId, boolean bPreview) {

    }

    @Override
    public void setLockMapRollAngle(MapType mapTypeId, boolean isLock) {
        
    }

    @Override
    public void setCarPosition(MapType mapTypeId, GeoPoint geoPoint) {

    }

    @Override
    public int setFollowMode(MapType mapTypeId, boolean bFollow) {
        return 0;
    }

    @Override
    public void setSkeletonBaseScale(MapType mapTypeId, float f) {

    }

    @Override
    public void setModelScale(MapType mapTypeId, float f) {

    }

    @Override
    public void addLayerItemOfUserTrackDepth(MapType mapTypeId, LayerItemUserTrackDepth userTrackDepth, boolean clearOtherLayerItem) {

    }

    @Override
    public void addLayerItemOfFavorite(MapType mapTypeId, LayerItemUserFavorite favorites) {

    }

    @Override
    public void removeFavoriteMain(MapType mapTypeId, PoiInfoEntity poiInfoEntity) {

    }

    @Override
    public void setFavoriteVisible(MapType mapTypeId, boolean visible) {

    }

    @Override
    public boolean showCross(MapType mapTypeId, LayerItemCrossEntity crossEntity) {
        return true;
    }

    @Override
    public boolean hideCross(MapType mapTypeId, int type) {
        return true;
    }

    @Override
    public void updateRoadCrossRect(MapType mapTypeId, Rect rect) {

    }

    @Override
    public Rect getRoadCrossRect(MapType mapTypeId) {
        return new Rect();
    }

    @Override
    public void openFlyLine(MapType mapTypeId, boolean visible) {

    }

    @Override
    public boolean updatePopSearchPointInfo(MapType mapTypeId, LayerItemLabelResult labelResult) {
        return true;
    }

    @Override
    public void clearLabelItem(MapType mapTypeId) {

    }
}
