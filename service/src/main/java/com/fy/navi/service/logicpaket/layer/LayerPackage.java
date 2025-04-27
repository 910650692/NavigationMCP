package com.fy.navi.service.logicpaket.layer;

import android.car.Car;

import com.android.utils.log.Logger;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.fy.navi.service.adapter.layer.LayerAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.refix.CarModeType;
import com.fy.navi.service.define.layer.refix.DynamicLevelMode;
import com.fy.navi.service.define.layer.refix.LayerItemCrossEntity;
import com.fy.navi.service.define.layer.refix.LayerItemRoutePointClickResult;
import com.fy.navi.service.define.layer.refix.LayerItemUserFavorite;
import com.fy.navi.service.define.layer.refix.LayerItemUserTrackDepth;
import com.fy.navi.service.define.layer.refix.LayerPointItemType;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.NaviParkingEntity;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/8
 */
public class LayerPackage implements ILayerAdapterCallBack {
    protected String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;
    private LayerAdapter mLayerAdapter;

    private final Hashtable<MapType, List<ILayerPackageCallBack>> callbacks = new Hashtable<>();

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
        return initLayerService(MapType.MAIN_SCREEN_MAIN_MAP);
    }

    public boolean initLayerService(MapType mapTypeId) {
        return mLayerAdapter.initLayerService(mapTypeId);
    }

    public void registerCallBack(MapType mapTypeId, ILayerPackageCallBack callback) {
        if (!callbacks.containsKey(mapTypeId)) {
            callbacks.put(mapTypeId, new CopyOnWriteArrayList<>());
            mLayerAdapter.registerLayerClickObserver(mapTypeId, this);
        }
        if (!callbacks.get(mapTypeId).contains(callback)) {
            callbacks.get(mapTypeId).add(callback);
        }
    }

    public void unRegisterCallBack(MapType mapTypeId, ILayerPackageCallBack callBack) {
        if (callbacks.containsKey(mapTypeId)) {
            if (callbacks.get(mapTypeId).contains(callBack)) {
                callbacks.get(mapTypeId).remove(callBack);
            }
        }
        if (callbacks.get(mapTypeId).size() <= 0) {
            mLayerAdapter.unRegisterLayerClickObserver(mapTypeId, this);
        }
    }

    public void setDefaultCarMode(MapType mapTypeId) {
        mLayerAdapter.setDefaultCarMode(mapTypeId);
    }

    public void setCarMode(MapType mapTypeId, CarModeType carMode) {
        mLayerAdapter.setCarMode(mapTypeId, carMode);
    }

    public CarModeType getCarModeType(MapType mapTypeId) {
        return mLayerAdapter.getCarModeType(mapTypeId);
    }

    public void setPreviewMode(MapType mapTypeId, boolean bPreview) {
        mLayerAdapter.setPreviewMode(mapTypeId, bPreview);
    }

    public int setFollowMode(MapType mapTypeId, boolean bFollow) {
        return mLayerAdapter.setFollowMode(mapTypeId, bFollow);
    }

    public void unInitLayerService() {
        mLayerAdapter.unInitLayerService();
    }


    public void updatePathArrow(MapType mapTypeId) {
        mLayerAdapter.updatePathArrow(mapTypeId);
    }

    public void setPathArrowSegment(MapType mapTypeId, ArrayList<Long> segmentsIndexs) {
        mLayerAdapter.setPathArrowSegment(mapTypeId, segmentsIndexs);
    }

    public void setVisibleGuideSignalLight(MapType mapTypeId, boolean isVisible) {
        mLayerAdapter.setVisibleGuideSignalLight(mapTypeId, isVisible);
    }

    // 此接口后续废弃
    public void openDynamicLevel(MapType mapTypeId, boolean isOpen) {
        mLayerAdapter.openDynamicLevel(mapTypeId, isOpen);
    }

    /* 是否打开动态比例尺功能，type区分巡航动态比例尺还是导航动态比例尺 */
    public void openDynamicLevel(MapType mapTypeId, DynamicLevelMode dynamicLevelMode) {
        mLayerAdapter.openDynamicLevel(mapTypeId, dynamicLevelMode);
    }

    /**
     * 搜索图层Item点击回调
     *
     */
    @Override
    public void onSearchItemClick(MapType mapTypeId, LayerPointItemType type, int index) {
        callbacks.forEach((key, packageCallBacks) -> {
            packageCallBacks.forEach(packageCallBack -> {
                packageCallBack.onSearchItemClick(mapTypeId, type, index);
            });
        });
    }

    /**
     * 路线图层Item点击回调
     */
    @Override
    public void onRouteItemClick(MapType mapTypeId, LayerPointItemType type, LayerItemRoutePointClickResult result) {
        callbacks.forEach((key, packageCallBacks) -> {
            packageCallBacks.forEach(packageCallBack -> {
                Logger.d(TAG, "onRouteItemClick");
                packageCallBack.onRouteItemClick(mapTypeId, type, result);
            });
        });
    }

    @Override
    public void onFavoriteClick(MapType mapTypeId,PoiInfoEntity poiInfo) {
        callbacks.forEach((key, packageCallBacks) -> {
            packageCallBacks.forEach(packageCallBack -> {
                packageCallBack.onFavoriteClick(mapTypeId,poiInfo);
            });
        });
    }

    @Override
    public void onFlyLineMoveEnd(MapType mapTypeId, GeoPoint descPoint) {
        callbacks.forEach((key, packageCallBacks) -> {
            packageCallBacks.forEach(packageCallBack -> {
                Logger.e(TAG, "onMapMoveEnd-LayerPackage:");
                packageCallBack.onFlyLineMoveEnd(mapTypeId, descPoint);
            });
        });
    }

    @Override
    public void onCarClick(MapType mapType, GeoPoint geoPoint) {
        callbacks.forEach((key, packageCallBacks) -> {
            packageCallBacks.forEach(packageCallBack -> {
                Logger.e(TAG, "onCarClick :");
                packageCallBack.onCarClick(mapType, geoPoint);
            });
        });
    }

    public void updateSearchParkPoi(MapType mapTypeId, ArrayList<NaviParkingEntity> parkList) {
        mLayerAdapter.updateSearchParkPoi(mapTypeId, parkList);
    }

    public void clearSearchParkPoi(MapType mapTypeId) {
        mLayerAdapter.clearSearchParkPoi(mapTypeId);
    }

    public void setParkFocus(MapType mapTypeId, String strID, boolean bFocus) {
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

    public void clearFavoriteMain(MapType mapTypeId) {
        mLayerAdapter.clearFavoriteMain(mapTypeId);
    }


    /*========================================= 搜索图层接口定义=========================================*/


    /*========================================= 搜索图层接口定义=========================================*/


    /*========================================= 用户图层接口定义=========================================*/
    public void addLayerItemOfUserTrackDepth(MapType mapTypeId, LayerItemUserTrackDepth userTrackDepth, boolean clearOtherLayerItem) {
        mLayerAdapter.addLayerItemOfUserTrackDepth(mapTypeId, userTrackDepth, clearOtherLayerItem);
    }

    public void addLayerItemOfFavorite(MapType mapTypeId, LayerItemUserFavorite favorites) {
        mLayerAdapter.addLayerItemOfFavorite(mapTypeId, favorites);
    }

    public void removeFavoriteMain(MapType mapTypeId, PoiInfoEntity poiInfoEntity) {
        mLayerAdapter.removeFavoriteMain(mapTypeId,poiInfoEntity);
    }

    public void setFavoriteVisible(MapType mapTypeId, boolean visible) {
        mLayerAdapter.setFavoriteVisible(mapTypeId,visible);
    }



    /*========================================= 用户图层接口定义=========================================*/

    /*========================================= 路口大图 =========================================*/

    /* 设置栅格图图片数据 */
    public boolean showCross(MapType mapTypeId, LayerItemCrossEntity crossEntity) {
        return mLayerAdapter.showCross(mapTypeId, crossEntity);
    }

    /* 根据放大路口类型隐藏对应的路口大图 */
    public boolean hideCross(MapType mapTypeId, int type) {
        return mLayerAdapter.hideCross(mapTypeId, type);
    }

    /*========================================= 路口大图 =========================================*/


    /*=========================================飞线=========================================*/
    public void openFlyLine(MapType mapTypeId, boolean visible) {
        mLayerAdapter.openFlyLine(mapTypeId, visible);
    }

    /* 设置动态比例尺是否锁住状态，type区分巡航动态比例尺还是导航动态比例尺 */
    public void setDynamicLevelLock(MapType mapTypeId, DynamicLevelMode dynamicLevelMode, boolean isLock) {
        mLayerAdapter.setDynamicLevelLock(mapTypeId, dynamicLevelMode, isLock);
    }

    public void setPassGray(MapType mapTypeId, boolean isGray) {
        mLayerAdapter.setPassGray(mapTypeId, isGray);
    }
}
