package com.fy.navi.service.adapter.layer;

import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.layer.CarModeType;
import com.fy.navi.service.define.layer.GemLayerClickBusinessType;
import com.fy.navi.service.define.layer.LayerType;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.layer.SearchResultLayer;
import com.fy.navi.service.define.layer.bls.CarLocation;
import com.fy.navi.service.define.layer.refix.LayerItemSearchChild;
import com.fy.navi.service.define.layer.refix.LayerItemSearchParent;
import com.fy.navi.service.define.map.GmBizUserFavoritePoint;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.NaviLayerTexture;
import com.fy.navi.service.define.navi.NaviParkingEntity;

import java.util.ArrayList;
import java.util.List;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/8
 */
public class LayerAdapter {
    private static final String LAYER_PKG_NAME = LayerAdapter.class.getPackage().getName();
    private static final String LAYER_CLS_NAME = "LayerAdapterApiImpl";

    private ILayerApi mLayerApi;

    public static LayerAdapter getInstance() {
        return Helper.ra;
    }

    private static final class Helper {
        private static final LayerAdapter ra = new LayerAdapter();
    }

    private LayerAdapter() {
        mLayerApi = (ILayerApi) AdapterConfig.getObject(LAYER_PKG_NAME, LAYER_CLS_NAME);
    }

    public boolean initLayerService(MapTypeId mapTypeId) {
        return mLayerApi.initLayerService(mapTypeId);
    }

    public void initInnerStyle() {
        mLayerApi.initInnerStyle();
    }

    public void setCustomLayerStyle() {
        mLayerApi.setCustomLayerStyle();
    }

    public void setDefaultCarMode(MapTypeId mapTypeId) {
        mLayerApi.setDefaultCarMode(mapTypeId);
    }

    public void setCarModeVisible(MapTypeId mapTypeId, boolean isVisible) {
        mLayerApi.setCarModeVisible(mapTypeId, isVisible);
    }

    public void setCarModeClickable(MapTypeId mapTypeId, boolean bClickable) {
        mLayerApi.setCarModeClickable(mapTypeId, bClickable);
    }

    public void setCarMode(MapTypeId mapTypeId, @CarModeType.CarModelTypeId int carMode) {
        mLayerApi.setCarMode(mapTypeId, carMode);
    }

    public int getCarMode(MapTypeId mapTypeId) {
        return mLayerApi.getCarMode(mapTypeId);
    }

    public int getCurrentCarModeType(MapTypeId mapTypeId) {
        return mLayerApi.getCurrentCarModeType(mapTypeId);
    }

    public void setCarUpMode(MapTypeId mapTypeId, boolean bCarUp) {
        mLayerApi.setCarUpMode(mapTypeId, bCarUp);
    }

    public boolean setCarScaleByMapLevel(MapTypeId mapTypeId, float[] vScales) {
        return mLayerApi.setCarScaleByMapLevel(mapTypeId, vScales);
    }

    public void setCarPosition(MapTypeId mapTypeId, CarLocation carLocation) {
        mLayerApi.setCarPosition(mapTypeId, carLocation);
    }

    public void updateCarPosition(MapTypeId mapTypeId, CarLocation carLocation) {
        mLayerApi.updateCarPosition(mapTypeId, carLocation);
    }

    public int setFollowMode(MapTypeId mapTypeId, boolean bFollow) {
        return mLayerApi.setFollowMode(mapTypeId, bFollow);
    }

    public void addSearchPointMarker(MapTypeId mapTypeId, SearchResultLayer searchResultLayer) {
        mLayerApi.addSearchPointMarker(mapTypeId, searchResultLayer);
    }

    public boolean addSearchLabelMarker(MapTypeId mapTypeId, SearchResultLayer.ChildPoint childPoint) {
        return mLayerApi.addSearchLabelMarker(mapTypeId, childPoint);
    }

    public void clearSearchAllItemLayer(MapTypeId mapTypeId) {
        mLayerApi.clearSearchAllItemLayer(mapTypeId);
    }

    public PreviewParams getPathResultBound(MapTypeId mapTypeId, ArrayList<?> pathResult) {
        return mLayerApi.getPathResultBound(mapTypeId, pathResult);
    }

    public void drawRouteLine(MapTypeId mapTypeId, RouteLineLayerParam routeLineLayer) {
        mLayerApi.drawRouteLine(mapTypeId, routeLineLayer);
    }

    public boolean showCross(MapTypeId mapTypeId, CrossImageEntity crossInfo) {
        return mLayerApi.showCross(mapTypeId, crossInfo);
    }

    public void hideCross(MapTypeId mapTypeId, int type) {
        mLayerApi.hideCross(mapTypeId, type);
    }

    public void setVisible(MapTypeId mapTypeId, int type, boolean bVisible) {
        mLayerApi.setVisible(mapTypeId, type, bVisible);
    }

    public void setCrossImageInfo(MapTypeId mapTypeId, CrossImageEntity crossImageEntity) {
        mLayerApi.setCrossImageInfo(mapTypeId, crossImageEntity);
    }

    public boolean updateCross(MapTypeId mapTypeId, byte[] buff, int crossType) {
        return mLayerApi.updateCross(mapTypeId, buff, crossType);
    }

    public boolean setRasterImageData(MapTypeId mapTypeId, NaviLayerTexture arrowImage, NaviLayerTexture roadImage) {
        return mLayerApi.setRasterImageData(mapTypeId, arrowImage, roadImage);
    }

    public boolean switchSelectedPath(MapTypeId mapTypeId, int index) {
        return mLayerApi.switchSelectedPath(mapTypeId, index);
    }

    public void updatePathArrow(MapTypeId mapTypeId) {
        mLayerApi.updatePathArrow(mapTypeId);
    }

    public void setPathArrowSegment(MapTypeId mapTypeId, ArrayList<Long> segmentsIndexs) {
        mLayerApi.setPathArrowSegment(mapTypeId, segmentsIndexs);
    }

    public String getCurrentRouteTime(MapTypeId mapTypeId) {
        return mLayerApi.getCurrentRouteTime(mapTypeId);
    }

    public void unInitLayerService() {
        mLayerApi.unInitLayerService();
    }


    public void setSelectedPathIndex(MapTypeId mapTypeId, int routeIndex) {
        mLayerApi.setSelectedPathIndex(mapTypeId, routeIndex);
    }

    public void clearRouteLine(MapTypeId mapTypeId) {
        mLayerApi.clearRouteLine(mapTypeId);
    }

    public void showRestArea(MapTypeId mapTypeId, ArrayList<?> pathInfoList, int index) {
        mLayerApi.showRestArea(mapTypeId, pathInfoList, index);
    }

    public void showWeatherView(MapTypeId mapTypeId, ArrayList<?> weatherLabelItem) {
        mLayerApi.showWeatherView(mapTypeId, weatherLabelItem);
    }

    public void showRestrictionView(MapTypeId mapTypeId, Object object) {
        showRestrictionView(mapTypeId, object, 0);
    }

    public void showRestrictionView(MapTypeId mapTypeId, Object object, int position) {
        mLayerApi.showRestrictionView(mapTypeId, object, position);
    }

    public void clearSearchMarks(MapTypeId mapTypeId) {
        mLayerApi.clearSearchMarks(mapTypeId);
    }

    public void registerLayerClickObserver(MapTypeId mapTypeId, @LayerType.LayerId int layerId, ILayerAdapterCallBack observer) {
        mLayerApi.registerLayerClickObserver(mapTypeId, layerId, observer);
    }

    public void unRegisterLayerClickObserver(MapTypeId mapTypeId, @LayerType.LayerId int layerId, ILayerAdapterCallBack observer) {
        mLayerApi.unRegisterLayerClickObserver(mapTypeId, layerId, observer);
    }

    public void openDynamicLevel(MapTypeId mapTypeId, boolean isOpen) {
        mLayerApi.openDynamicLevel(mapTypeId, isOpen);
    }

    public void updateSearchParkPoi(MapTypeId mapTypeId, ArrayList<NaviParkingEntity> parkList) {
        mLayerApi.updateSearchParkPoi(mapTypeId, parkList);
    }

    public void clearSearchParkPoi(MapTypeId mapTypeId) {
        mLayerApi.clearSearchParkPoi(mapTypeId);
    }

    public void setParkFocus(MapTypeId mapTypeId, String strID, boolean bFocus) {
        mLayerApi.setParkFocus(mapTypeId, strID, bFocus);
    }

    public void updateGuideCarStyle(MapTypeId mapTypeId) {
        mLayerApi.updateGuideCarStyle(mapTypeId);
    }

    public void setVisibleCruiseSignalLight(MapTypeId mapTypeId, boolean isVisible) {
        mLayerApi.setVisibleCruiseSignalLight(mapTypeId, isVisible);
    }

    public void setVisibleGuideSignalLight(MapTypeId mapTypeId, boolean isVisible) {
        mLayerApi.setVisibleGuideSignalLight(mapTypeId, isVisible);
    }

    public double calcStraightDistance(GeoPoint startPoint, GeoPoint endPoint) {
        return mLayerApi.calcStraightDistance(startPoint, endPoint);
    }

    public void setSearchSelect(MapTypeId mapTypeId, GemLayerClickBusinessType type, String strID, boolean bFocus) {
        mLayerApi.selectSearchPoi(mapTypeId, type, strID, bFocus);
    }

    public void updateFavoriteMain(MapTypeId mapTypeId, List<GmBizUserFavoritePoint> list) {
        mLayerApi.updateFavoriteMain(mapTypeId, list);
    }

    public void clearFavoriteMain(MapTypeId mapTypeId) {
        mLayerApi.clearFavoriteMain(mapTypeId);
    }

    public int setDynamicLevelLock(MapTypeId mapTypeId, boolean isLock, int type) {
        return mLayerApi.setDynamicLevelLock(mapTypeId, isLock, type);
    }

    public void resetDynamicLevel(MapTypeId mapTypeId, int type) {
        mLayerApi.resetDynamicLevel(mapTypeId, type);
    }

    public boolean getDynamicLevelLock(MapTypeId mapTypeId, int type) {
        return mLayerApi.getDynamicLevelLock(mapTypeId, type);
    }

    public float getDynamicLevelMapHeadDegree(MapTypeId mapTypeId, int type) {
        return mLayerApi.getDynamicLevelMapHeadDegree(mapTypeId, type);
    }

    public int openDynamicCenter(MapTypeId mapTypeId, boolean changeCenter) {
        return mLayerApi.openDynamicCenter(mapTypeId, changeCenter);
    }

}
