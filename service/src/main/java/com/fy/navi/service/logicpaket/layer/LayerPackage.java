package com.fy.navi.service.logicpaket.layer;

import com.autonavi.gbl.common.model.RectInt;
import com.autonavi.gbl.guide.model.CrossType;
import com.autonavi.gbl.guide.model.NaviInfo;
import com.autonavi.gbl.map.layer.model.RealCityTmcParam;
import com.autonavi.gbl.map.layer.model.VectorCrossViewPostureEvent;
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.fy.navi.service.adapter.layer.LayerAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.refix.CarModeType;
import com.fy.navi.service.define.layer.refix.LayerItemCrossEntity;
import com.fy.navi.service.define.layer.refix.LayerType;
import com.fy.navi.service.define.layer.GemBaseLayer;
import com.fy.navi.service.define.layer.GemLayerItem;
import com.fy.navi.service.define.layer.refix.LayerItemUserFavorite;
import com.fy.navi.service.define.layer.refix.LayerItemUserTrackDepth;
import com.fy.navi.service.define.map.GmBizUserFavoritePoint;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.CrossImageEntity;
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

    private static final String TAG = LayerPackage.class.getSimpleName();
    private LayerAdapter mLayerAdapter;
    private final Hashtable<MapType, List<ILayerPackageCallBack>> mLayerPackageCallBacks = new Hashtable<>();

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

    public void registerCallBack(MapType mapTypeId, ILayerPackageCallBack callBack, LayerType layerId) {
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

    public void unRegisterCallBack(MapType mapTypeId, ILayerPackageCallBack callBack, LayerType layerId) {
        List<ILayerPackageCallBack> layerPackageCallBacks = mLayerPackageCallBacks.get(mapTypeId);
        if (layerPackageCallBacks != null) {
            layerPackageCallBacks.remove(callBack);
        }
        mLayerAdapter.unRegisterLayerClickObserver(mapTypeId, layerId, this);
    }

    public void setDefaultCarMode(MapType mapTypeId) {
        mLayerAdapter.setDefaultCarMode(mapTypeId);
    }


    public void setCarMode(MapType mapTypeId, CarModeType carMode) {
        mLayerAdapter.setCarMode(mapTypeId, carMode);
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

    public void openDynamicLevel(MapType mapTypeId, boolean isOpen) {
        mLayerAdapter.openDynamicLevel(mapTypeId, isOpen);
    }


    @Override
    public void onNotifyClick(MapType mapTypeId, GemBaseLayer layer, GemLayerItem pItem) {
        mLayerPackageCallBacks.forEach((key, packageCallBacks) -> {
            packageCallBacks.forEach(packageCallBack -> {
                packageCallBack.onNotifyClick(mapTypeId, layer, pItem);
            });
        });
    }

    @Override
    public void onFavorite(double lat,double lon) {
        mLayerPackageCallBacks.forEach((key, packageCallBacks) -> {
            packageCallBacks.forEach(packageCallBack -> {
                packageCallBack.onFavorite(lat,lon);
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

    public void updateFavoriteMain(MapType mapTypeId, List<GmBizUserFavoritePoint> list) {
        mLayerAdapter.updateFavoriteMain(mapTypeId, list);
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

    public void addLayerItemOfFavorite(MapType mapTypeId, LayerItemUserFavorite favorites, boolean clearOtherLayerItem) {
        mLayerAdapter.addLayerItemOfFavorite(mapTypeId, favorites, clearOtherLayerItem);
    }



    /*========================================= 用户图层接口定义=========================================*/

    /*========================================= 路口大图 =========================================*/

    /* 根据放大路口图层类型更新样式 */
    public boolean updateCrossStyle(MapType mapTypeId, int crossType) {
        return mLayerAdapter.updateCrossStyle(mapTypeId, crossType);
    }

    /* 根据放大路口类型进行显示隐藏控制 */
    public boolean setCrossVisible(MapType mapTypeId, int type, boolean bVisible) {
        return mLayerAdapter.setCrossVisible(mapTypeId, type, bVisible);
    }

    /* 设置栅格图图片数据 */
    public boolean setRasterImageData(MapType mapTypeId, LayerItemCrossEntity crossEntity) {
        return mLayerAdapter.setRasterImageData(mapTypeId, crossEntity);
    }

    /* 根据放大路口类型填充数据 */
    public boolean updateCross(MapType mapTypeId, LayerItemCrossEntity crossEntity) {
        return mLayerAdapter.updateCross(mapTypeId, crossEntity);
    }

    /* 根据放大路口类型隐藏对应的路口大图 */
    public boolean hideCross(MapType mapTypeId, int type) {
        return mLayerAdapter.hideCross(mapTypeId, type);
    }

    /* 设置导航车首上还是北上模式 */
    public boolean set3DCrossCarMode(MapType mapTypeId, boolean isCarUp) {
        return mLayerAdapter.set3DCrossCarMode(mapTypeId, isCarUp);
    }

    /* 设置3D飞线的路况信息 */
    public boolean setFlyTmc(MapType mapTypeId, byte[] buffer, ArrayList<RealCityTmcParam> param) {
        return mLayerAdapter.setFlyTmc(mapTypeId, buffer, param);
    }

    /* 更新3D精品大图引导信息 */
    public boolean updateNaviInfo(MapType mapTypeId, NaviInfo naviInfo) {
        return mLayerAdapter.updateNaviInfo(mapTypeId, naviInfo);
    }

    /* 设置路口栅格图信息
     *1、设置路口大图信息，使用自带近接/混淆矢量大图显隐策略，如果没有设置数据，用户可自定义策略并调用SetViewPostureEvent触发功能
     *2、本接口暂时只对混淆\近接路口生效，当设置路口大图信息，用户调用SetViewPostureEvent无效（内部策略自动调用
     */
    public boolean setCrossImageInfo(MapType mapTypeId, int type, boolean useCustom) {
        return mLayerAdapter.setCrossImageInfo(mapTypeId, type, useCustom);
    }

    /* 设置近接/混淆矢量大图的姿态事件, 目前只有type = CrossTypeVector才有实现才有实现 */
    public boolean setViewPostureEvent(MapType mapTypeId, CrossType type, VectorCrossViewPostureEvent postureEvent) {
        return mLayerAdapter.setViewPostureEvent(mapTypeId, type, postureEvent);
    }

    /* 设置放大路口显示区域 */
    public boolean setRoadCrossRect(MapType mapTypeId, int crossType, RectInt viewRect) {
        return mLayerAdapter.setRoadCrossRect(mapTypeId, crossType, viewRect);
    }

    /*========================================= 路口大图 =========================================*/



    /*=========================================飞线=========================================*/
    public void flyLineVisible(MapType mapTypeId, boolean visible) {
        mLayerAdapter.flyLineVisible(mapTypeId, visible);
    }

    public void flyLineHideOnce(MapType mapTypeId) {
        mLayerAdapter.flyLineHideOnce(mapTypeId);
    }


}
