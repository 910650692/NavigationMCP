package com.fy.navi.service.logicpaket.layer;

import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.fy.navi.service.adapter.layer.LayerAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.layer.CarModeType;
import com.fy.navi.service.define.layer.LayerType;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.layer.SearchResultLayer;
import com.fy.navi.service.define.layer.bls.CarLocation;
import com.fy.navi.service.define.layer.GemBaseLayer;
import com.fy.navi.service.define.layer.GemLayerItem;
import com.fy.navi.service.define.layer.refix.LayerItemSearchChild;
import com.fy.navi.service.define.layer.refix.LayerItemSearchParent;
import com.fy.navi.service.define.map.GmBizUserFavoritePoint;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.NaviLayerTexture;
import com.fy.navi.service.define.navi.NaviParkingEntity;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/8
 */
public class LayerPackage implements ILayerAdapterCallBack {
    private LayerAdapter mLayerAdapter;
    private final Hashtable<MapTypeId, List<ILayerPackageCallBack>> mLayerPackageCallBacks = new Hashtable<>();

    private static final class Helper {
        private static final LayerPackage lPackage = new LayerPackage();
    }

    private LayerPackage() {
        mLayerAdapter = LayerAdapter.getInstance();
    }

    public static LayerPackage getInstance() {
        return Helper.lPackage;
    }

    public boolean initLayerService() {
        return initLayerService(MapTypeId.MAIN_SCREEN_MAIN_MAP);
    }

    public boolean initLayerService(MapTypeId mapTypeId) {
        return mLayerAdapter.initLayerService(mapTypeId);
    }

    public void registerCallBack(MapTypeId mapTypeId, ILayerPackageCallBack callBack, @LayerType.LayerId int layerId) {
        List<ILayerPackageCallBack> layerPackageCallBacks = mLayerPackageCallBacks.get(mapTypeId);
        if (layerPackageCallBacks == null) {
            layerPackageCallBacks = new ArrayList<>();
            mLayerPackageCallBacks.put(mapTypeId, layerPackageCallBacks);
        }
        if (!layerPackageCallBacks.contains(callBack)) {
            layerPackageCallBacks.add(callBack);
        }
        mLayerAdapter.registerLayerClickObserver(mapTypeId, layerId, this);
    }

    public void unRegisterCallBack(MapTypeId mapTypeId, ILayerPackageCallBack callBack, @LayerType.LayerId int layerId) {
        List<ILayerPackageCallBack> layerPackageCallBacks = mLayerPackageCallBacks.get(mapTypeId);
        if (layerPackageCallBacks != null) {
            layerPackageCallBacks.remove(callBack);
        }
        mLayerAdapter.unRegisterLayerClickObserver(mapTypeId, layerId, this);
    }

    public void setDefaultCarMode(MapTypeId mapTypeId) {
        mLayerAdapter.setDefaultCarMode(mapTypeId);
    }


    public void setCarMode(MapTypeId mapTypeId, @CarModeType.CarModelTypeId int carMode) {
        mLayerAdapter.setCarMode(mapTypeId, carMode);
    }

    public int getCarMode(MapTypeId mapTypeId) {
        return mLayerAdapter.getCarMode(mapTypeId);
    }

    public int getCurrentCarModeType(MapTypeId mapTypeId) {
        return mLayerAdapter.getCurrentCarModeType(mapTypeId);
    }

    public void setCarUpMode(MapTypeId mapTypeId, boolean bCarUp) {
        mLayerAdapter.setCarUpMode(mapTypeId, bCarUp);
    }

    public boolean setCarScaleByMapLevel(MapTypeId mapTypeId, float[] vScales) {
        return mLayerAdapter.setCarScaleByMapLevel(mapTypeId, vScales);
    }

    public void setCarPosition(MapTypeId mapTypeId, CarLocation carLocation) {
        mLayerAdapter.setCarPosition(mapTypeId, carLocation);
    }

    public void updateCarPosition(MapTypeId mapTypeId, CarLocation carLocation) {
        mLayerAdapter.updateCarPosition(mapTypeId, carLocation);
    }

    public int setFollowMode(MapTypeId mapTypeId, boolean bFollow) {
        return mLayerAdapter.setFollowMode(mapTypeId, bFollow);
    }

    public void addSearchPointMarker(MapTypeId mapTypeId, SearchResultLayer searchResultLayer) {
        mLayerAdapter.addSearchPointMarker(mapTypeId, searchResultLayer);
    }

    public void clearSearchAllItemLayer(MapTypeId mapTypeId) {
        mLayerAdapter.clearSearchAllItemLayer(mapTypeId);
    }

    public boolean addSearchLabelMarker(MapTypeId mapTypeId, SearchResultLayer.ChildPoint childPoint) {
        return mLayerAdapter.addSearchLabelMarker(mapTypeId, childPoint);
    }

    public PreviewParams getPathResultBound(MapTypeId mapTypeId, ArrayList<?> pathResult) {
        return mLayerAdapter.getPathResultBound(mapTypeId, pathResult);
    }

    public void drawRouteLine(MapTypeId mapTypeId, RouteLineLayerParam routeLineLayer) {
        if (!routeLineLayer.isMIsDrawLineLayer()) return;
        mLayerAdapter.drawRouteLine(mapTypeId, routeLineLayer);
    }

    public boolean showCross(MapTypeId mapTypeId, CrossImageEntity crossInfo) {
        return mLayerAdapter.showCross(mapTypeId, crossInfo);
    }

    public void hideCross(MapTypeId mapTypeId, int type) {
        mLayerAdapter.hideCross(mapTypeId, type);
    }

    public void setVisible(MapTypeId mapTypeId, int type, boolean bVisible) {
        mLayerAdapter.setVisible(mapTypeId, type, bVisible);
    }

    public void setCrossImageInfo(MapTypeId mapTypeId, CrossImageEntity crossImageEntity) {
        mLayerAdapter.setCrossImageInfo(mapTypeId, crossImageEntity);
    }

    public boolean updateCross(MapTypeId mapTypeId, byte[] buff, int crossType) {
        return mLayerAdapter.updateCross(mapTypeId, buff, crossType);
    }

    public boolean setRasterImageData(MapTypeId mapTypeId, NaviLayerTexture arrowImage, NaviLayerTexture roadImage) {
        return mLayerAdapter.setRasterImageData(mapTypeId, arrowImage, roadImage);
    }

    public void unInitLayerService() {
        mLayerAdapter.unInitLayerService();
    }

    public void setSelectedPathIndex(MapTypeId mapTypeId, int routeIndex) {
        mLayerAdapter.setSelectedPathIndex(mapTypeId, routeIndex);
    }

    public void switchSelectedPath(MapTypeId mapTypeId, int index) {
        mLayerAdapter.switchSelectedPath(mapTypeId, index);
    }

    public void updatePathArrow(MapTypeId mapTypeId) {
        mLayerAdapter.updatePathArrow(mapTypeId);
    }

    public void setPathArrowSegment(MapTypeId mapTypeId, ArrayList<Long> segmentsIndexs) {
        mLayerAdapter.setPathArrowSegment(mapTypeId, segmentsIndexs);
    }

    public void setVisibleCruiseSignalLight(MapTypeId mapTypeId, boolean isVisible) {
        mLayerAdapter.setVisibleCruiseSignalLight(mapTypeId, isVisible);
    }

    public void setVisibleGuideSignalLight(MapTypeId mapTypeId, boolean isVisible) {
        mLayerAdapter.setVisibleGuideSignalLight(mapTypeId, isVisible);
    }

    public void openDynamicLevel(MapTypeId mapTypeId, boolean isOpen) {
        mLayerAdapter.openDynamicLevel(mapTypeId, isOpen);
    }

    @Override
    public void onBeforeNotifyClick(MapTypeId mapTypeId, GemBaseLayer layer, GemLayerItem pItem) {
        mLayerPackageCallBacks.forEach((key, packageCallBacks) -> {
            packageCallBacks.forEach(packageCallBack -> {
                packageCallBack.onBeforeNotifyClick(mapTypeId, layer, pItem);
            });
        });
    }

    @Override
    public void onNotifyClick(MapTypeId mapTypeId, GemBaseLayer layer, GemLayerItem pItem) {
        mLayerPackageCallBacks.forEach((key, packageCallBacks) -> {
            packageCallBacks.forEach(packageCallBack -> {
                packageCallBack.onNotifyClick(mapTypeId, layer, pItem);
            });
        });
    }

    @Override
    public void onAfterNotifyClick(MapTypeId mapTypeId, GemBaseLayer layer, GemLayerItem pItem) {
        mLayerPackageCallBacks.forEach((key, packageCallBacks) -> {
            packageCallBacks.forEach(packageCallBack -> {
                packageCallBack.onAfterNotifyClick(mapTypeId, layer, pItem);
            });
        });
    }

    public void updateSearchParkPoi(MapTypeId mapTypeId, ArrayList<NaviParkingEntity> parkList) {
        mLayerAdapter.updateSearchParkPoi(mapTypeId, parkList);
    }

    public void clearSearchParkPoi(MapTypeId mapTypeId) {
        mLayerAdapter.clearSearchParkPoi(mapTypeId);
    }

    public void setParkFocus(MapTypeId mapTypeId, String strID, boolean bFocus) {
        mLayerAdapter.setParkFocus(mapTypeId, strID, bFocus);
    }

    /**
     * 计算两点之间的直线距离.
     *
     * @param startPoint 起点.
     * @param endPoint   终点.
     * @return 距离.
     */
    public double calcStraightDistance(GeoPoint startPoint, GeoPoint endPoint) {
        return mLayerAdapter.calcStraightDistance(startPoint, endPoint);
    }

    public void updateFavoriteMain(MapTypeId mapTypeId, List<GmBizUserFavoritePoint> list) {
        mLayerAdapter.updateFavoriteMain(mapTypeId, list);
    }

    public void clearFavoriteMain(MapTypeId mapTypeId) {
        mLayerAdapter.clearFavoriteMain(mapTypeId);
    }

    public int setDynamicLevelLock(MapTypeId mapTypeId, boolean isLock, int type) {
        return mLayerAdapter.setDynamicLevelLock(mapTypeId, isLock, type);
    }

    public void resetDynamicLevel(MapTypeId mapTypeId, int type) {
        mLayerAdapter.resetDynamicLevel(mapTypeId, type);
    }

    public boolean getDynamicLevelLock(MapTypeId mapTypeId, int type) {
        return mLayerAdapter.getDynamicLevelLock(mapTypeId, type);
    }

    public float getDynamicLevelMapHeadDegree(MapTypeId mapTypeId, int type) {
        return mLayerAdapter.getDynamicLevelMapHeadDegree(mapTypeId, type);
    }

    public int openDynamicCenter(MapTypeId mapTypeId, boolean changeCenter) {
        return mLayerAdapter.openDynamicCenter(mapTypeId, changeCenter);
    }


}
