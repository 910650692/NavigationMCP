package com.sgm.navi.service.logicpaket.layer;


import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.sgm.navi.service.adapter.layer.LayerAdapter;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.layer.refix.CarModeType;
import com.sgm.navi.service.define.layer.refix.DynamicLevelMode;
import com.sgm.navi.service.define.layer.refix.LayerItemCrossEntity;
import com.sgm.navi.service.define.layer.refix.LayerItemRoutePointClickResult;
import com.sgm.navi.service.define.layer.refix.LayerItemUserFavorite;
import com.sgm.navi.service.define.layer.refix.LayerItemUserTrackDepth;
import com.sgm.navi.service.define.layer.refix.LayerPointItemType;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/8
 */
public class LayerPackage implements ILayerAdapterCallBack {
    protected String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;
    private LayerAdapter mLayerAdapter;

    private final ConcurrentHashMap<MapType, List<ILayerPackageCallBack>> callbacks = new ConcurrentHashMap<>();

    public void clearRouteLine(MapType mapType) {
        mLayerAdapter.clearRouteLine(mapType);
    }

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
        return mLayerAdapter.initLayerService();
    }

    public boolean initLayer(MapType mapTypeId) {
        return mLayerAdapter.initLayer(mapTypeId);
    }

    public void unInitLayer(MapType mapType) {
        mLayerAdapter.unInitLayer(mapType);
    }

    public void unInitLayerService() {
        mLayerAdapter.unInitLayerService();
    }


    public void registerCallBack(MapType mapTypeId, ILayerPackageCallBack callback) {
        if (!callbacks.containsKey(mapTypeId)) {
            callbacks.put(mapTypeId, new ArrayList<>());
            mLayerAdapter.registerLayerClickObserver(mapTypeId, this);
        }
        if (!callbacks.get(mapTypeId).contains(callback)) {
            callbacks.get(mapTypeId).add(callback);
        }
        if (callbacks.get(mapTypeId).size() == 1) {
            mLayerAdapter.registerLayerClickObserver(mapTypeId, this);
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

    public void updateMapLevel(MapType mapTypeId, float mapLevel) {
        mLayerAdapter.updateMapLevel(mapTypeId, mapLevel);
    }

    public void setDefaultCarMode(MapType mapTypeId) {
        mLayerAdapter.setDefaultCarMode(mapTypeId);
    }

    public void setCarPosition(MapType mapTypeId, GeoPoint geoPoint) {
        mLayerAdapter.setCarPosition(mapTypeId, geoPoint);
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

    public void setLockMapRollAngle(MapType mapTypeId, boolean isLock) {
        mLayerAdapter.setLockMapRollAngle(mapTypeId, isLock);
    }

    public void initCarLogoByFlavor(MapType mapTypeId, String flavor) {
        mLayerAdapter.initCarLogoByFlavor(mapTypeId, flavor);
    }

    public int setFollowMode(MapType mapTypeId, boolean bFollow) {
        return mLayerAdapter.setFollowMode(mapTypeId, bFollow);
    }

    public void setVisibleGuideSignalLight(MapType mapTypeId, boolean isVisible) {
        mLayerAdapter.setVisibleGuideSignalLight(mapTypeId, isVisible);
    }

    /* 是否打开动态比例尺功能，type区分巡航动态比例尺还是导航动态比例尺 */
    public void openDynamicLevel(MapType mapTypeId, DynamicLevelMode dynamicLevelMode) {
        mLayerAdapter.openDynamicLevel(mapTypeId, dynamicLevelMode);
    }

    /* 关闭动态比例尺 */
    public void closeDynamicLevel(MapType mapTypeId) {
        mLayerAdapter.closeDynamicLevel(mapTypeId);
    }

    /*清除扎标*/
    public void clearLabelItem(MapType mapTypeId) {
        mLayerAdapter.clearLabelItem(mapTypeId);
    }

    /* 设置起点扎标是否显示 */
    public void setStartPointVisible(MapType mapTypeId, boolean visible) {
        mLayerAdapter.setStartPointVisible(mapTypeId, visible);
    }

    /* HUD样式初始化 */
    public void initGuideRouteHUDMode(MapType mapTypeId) {
        mLayerAdapter.initGuideRouteHUDMode(mapTypeId);
    }

    /* 设置路线图层扎标是否选中 */
    public void setRoutePointSelect(MapType mapTypeId, LayerPointItemType type, boolean isSelect, int index) {
        mLayerAdapter.setRoutePointSelect(mapTypeId, type, isSelect, index);
    }

    /* 清除路线图层扎标focus */
    public void clearRoutePointFocus(MapType mapTypeId, LayerPointItemType type) {
        mLayerAdapter.clearRoutePointFocus(mapTypeId, type);
    }

    /**
     * 搜索图层Item点击回调
     */
    @Override
    public void onSearchItemClick(MapType mapTypeId, LayerPointItemType type, int index) {
        if (callbacks.containsKey(mapTypeId)) {
            List<ILayerPackageCallBack> callBacks = callbacks.get(mapTypeId);
            if (callBacks != null && !callBacks.isEmpty()) {
                callBacks.forEach(new Consumer<ILayerPackageCallBack>() {
                    @Override
                    public void accept(ILayerPackageCallBack callBack) {
                        callBack.onSearchItemClick(mapTypeId, type, index);
                    }
                });
            }
        }
    }

    /**
     * 路线图层Item点击回调
     */
    @Override
    public void onRouteItemClick(MapType mapTypeId, LayerPointItemType type, LayerItemRoutePointClickResult result) {
        if (callbacks.containsKey(mapTypeId)) {
            List<ILayerPackageCallBack> callBacks = callbacks.get(mapTypeId);
            if (callBacks != null && !callBacks.isEmpty()) {
                callBacks.forEach(new Consumer<ILayerPackageCallBack>() {
                    @Override
                    public void accept(ILayerPackageCallBack callBack) {
                        callBack.onRouteItemClick(mapTypeId, type, result);
                    }
                });
            }
        }
    }

    @Override
    public void onFavoriteClick(MapType mapTypeId, PoiInfoEntity poiInfo) {
        if (callbacks.containsKey(mapTypeId)) {
            List<ILayerPackageCallBack> callBacks = callbacks.get(mapTypeId);
            if (callBacks != null && !callBacks.isEmpty()) {
                callBacks.forEach(new Consumer<ILayerPackageCallBack>() {
                    @Override
                    public void accept(ILayerPackageCallBack callBack) {
                        callBack.onFavoriteClick(mapTypeId, poiInfo);
                    }
                });
            }
        }
    }

    @Override
    public void onFlyLineMoveEnd(MapType mapTypeId, GeoPoint descPoint) {
        if (callbacks.containsKey(mapTypeId)) {
            List<ILayerPackageCallBack> callBacks = callbacks.get(mapTypeId);
            if (callBacks != null && !callBacks.isEmpty()) {
                callBacks.forEach(new Consumer<ILayerPackageCallBack>() {
                    @Override
                    public void accept(ILayerPackageCallBack callBack) {
                        callBack.onFlyLineMoveEnd(mapTypeId, descPoint);
                    }
                });
            }
        }
    }

    @Override
    public void onCarClick(MapType mapTypeId, GeoPoint geoPoint) {
        if (callbacks.containsKey(mapTypeId)) {
            List<ILayerPackageCallBack> callBacks = callbacks.get(mapTypeId);
            if (callBacks != null && !callBacks.isEmpty()) {
                callBacks.forEach(new Consumer<ILayerPackageCallBack>() {
                    @Override
                    public void accept(ILayerPackageCallBack callBack) {
                        if (Logger.openLog) {
                            Logger.e(TAG, mapTypeId, " onCarClick :");
                        }
                        callBack.onCarClick(mapTypeId, geoPoint);
                    }
                });
            }
        }
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
        mLayerAdapter.removeFavoriteMain(mapTypeId, poiInfoEntity);
    }

    public void hideOrShowFavoriteMain(MapType mapTypeId, boolean isShow) {
        mLayerAdapter.hideOrShowFavoriteMain(mapTypeId, isShow);
    }

    public void setFavoriteVisible(MapType mapTypeId, boolean visible) {
        mLayerAdapter.setFavoriteVisible(mapTypeId, visible);
    }



    /*========================================= 用户图层接口定义=========================================*/

    /*========================================= 路口大图 =========================================*/

    /* 设置栅格图图片数据 */
    public boolean showCross(MapType mapTypeId, LayerItemCrossEntity crossEntity) {
        notifyCrossImageVisibleChanged(mapTypeId, true);
        return mLayerAdapter.showCross(mapTypeId, crossEntity);
    }

    /* 根据放大路口类型隐藏对应的路口大图 */
    public boolean hideCross(MapType mapTypeId, int type) {
        notifyCrossImageVisibleChanged(mapTypeId, false);
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

    private void notifyCrossImageVisibleChanged(MapType mapTypeId, boolean visible) {
        if (callbacks.containsKey(mapTypeId)) {
            List<ILayerPackageCallBack> callBacks = callbacks.get(mapTypeId);
            if (ConvertUtils.isEmpty(callBacks)) {
                if (Logger.openLog) {
                    Logger.e(TAG, mapTypeId, " callBacks is empty, visible ", visible);
                }
                return;
            }
            //创建副本并遍历 不影响原始集合数据
            for (ILayerPackageCallBack callBack : new ArrayList<>(callBacks)) {
                callBack.onCrossImageVisibleChanged(mapTypeId, visible);
            }
        }
    }
}
