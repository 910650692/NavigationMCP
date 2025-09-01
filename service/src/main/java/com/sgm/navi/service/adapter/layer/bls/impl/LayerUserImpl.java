package com.sgm.navi.service.adapter.layer.bls.impl;

import android.content.Context;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.Coord3DDouble;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.FavoritePointLayerItem;
import com.autonavi.gbl.layer.model.BizUserFavoritePoint;
import com.autonavi.gbl.layer.model.BizUserType;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.model.ClickViewIdInfo;
import com.autonavi.gbl.user.behavior.model.FavoriteType;
import com.autonavi.gbl.user.usertrack.model.GpsTrackDepthInfo;
import com.autonavi.gbl.user.usertrack.model.GpsTrackPoint;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.adapter.layer.bls.style.LayerUserStyleAdapter;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.layer.refix.LayerItemUserFavorite;
import com.sgm.navi.service.define.layer.refix.LayerItemUserTrackDepth;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.FavoriteInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.user.usertrack.GpsTrackDepthBean;
import com.sgm.navi.service.define.user.usertrack.GpsTrackPointBean;

import java.util.ArrayList;

public class LayerUserImpl extends BaseLayerImpl<LayerUserStyleAdapter> {

    public LayerUserImpl(BizControlService bizService, MapView mapView, Context context, MapType mapType) {
        super(bizService, mapView, context, mapType);
        this.className = "LayerUserImpl";
        getLayerUserControl().setStyle(this);
        getLayerUserControl().addClickObserver(this);
        Logger.d(TAG, "LayerUserImpl init");
    }

    @Override
    protected LayerUserStyleAdapter createStyleAdapter() {
        return new LayerUserStyleAdapter(getEngineId(), getLayerUserControl());
    }

    public void clearFavoriteMain() {
        getLayerUserControl().clearAllItems();
    }

    /* 绘制用户历史行程轨迹(包括轨迹点和线) */
    public void updateGpsTrack(LayerItemUserTrackDepth depthInfo) {
        //进行数据的转换
        if (ConvertUtils.isEmpty(depthInfo)) {
            getLayerUserControl().getUserLayer(BizUserType.BizUserTypeGpsTrack).clearAllItems();
            getLayerUserControl().getUserLayer(BizUserType.BizUserTypeGpsTrackLine).clearAllItems();
            Logger.e(TAG, "updateGpsTrack null");
            return;
        }

        final GpsTrackDepthInfo info = getGpsTrackDepthInfo(depthInfo.getGpsTrackDepthBean());
        //显示用户历史轨迹相关图层
        getLayerUserControl().updateGpsTrack(info);
        getLayerUserControl().setVisible(BizUserType.BizUserTypeGpsTrack, true);
        getLayerUserControl().setVisible(BizUserType.BizUserTypeGpsTrackLine, true);
        Logger.d(TAG, "updateGpsTrack");
    }

//    /* 删除绘制用户历史行程轨迹(包括轨迹点和线) */
//    public void cleanGpsTrack() {
//        getLayerUserControl().clearAllItems(BizUserType.BizUserTypeGpsTrack);
//        getLayerUserControl().clearAllItems(BizUserType.BizUserTypeGpsTrackLine);
//        getLayerUserControl().setVisible(BizUserType.BizUserTypeGpsTrack,false);
//        getLayerUserControl().setVisible(BizUserType.BizUserTypeGpsTrackLine,false);
//    }

//    /* 更新SendToCar图层对象的气泡 */
//    public void updateSendToCar(LayerItemUserReceive sendToCarInfo) {
//        //todo
//        //构造SendToCar点
//        BizPointBusinessInfo sendToCar = new BizCustomPointInfo();
//        sendToCar.mPos3D.lon = 116.342024;
//        sendToCar.mPos3D.lat = 39.952446;
//        sendToCar.mPos3D.z = 0;
//        // 扎标位置与屏幕中心比往下偏移100个像素点
//        int x = 0;
//        int y = 100;
//        // 显示SendToCar图层
//        getLayerUserControl().updateSendToCar(sendToCar, x, y);
//        Logger.d("updateSendToCar","updateSendToCar");
//    }

    /* 更新收藏夹中主图查看模式的收藏点 */
    public void updateFavoriteMain(LayerItemUserFavorite userFavorite) {
        ArrayList<PoiInfoEntity> mSimpleFavoriteList = userFavorite.getMSimpleFavoriteList();
        ArrayList<BizUserFavoritePoint> favoriteList = new ArrayList<>();
        if (!ConvertUtils.isEmpty(mSimpleFavoriteList)) {
            mSimpleFavoriteList.forEach((entity) -> {
                //防止已添加的收藏点更换为家/公司类型
                removeFavoriteMain(entity);
                BizUserFavoritePoint point = new BizUserFavoritePoint();
                //FavoriteInfo 根据 mCommonName  1家，2公司，0普通收藏点）
                FavoriteInfo favoriteInfo = entity.getFavoriteInfo();
                if (!ConvertUtils.isEmpty(favoriteInfo)) {
                    point.id = favoriteInfo.getItemId();
                    int commonName = favoriteInfo.getCommonName();
                    if (commonName == 0) {
                        point.favoriteType = FavoriteType.FavoriteTypePoi;
                    } else if (commonName == 1) {
                        point.favoriteType = FavoriteType.FavoriteTypeHome;
                    } else if (commonName == 2) {
                        point.favoriteType = FavoriteType.FavoriteTypeCompany;
                    }
                }
                GeoPoint geoPoint = entity.getMPoint();
                if (!ConvertUtils.isEmpty(geoPoint)) {
                    point.mPos3D.lat = geoPoint.getLat();
                    point.mPos3D.lon = geoPoint.getLon();
                }
                favoriteList.add(point);
            });
        }

        if (ConvertUtils.isEmpty(favoriteList)) {
            Logger.e(TAG, "updateFavoriteMain null");
            return;
        }

        getLayerUserControl().updateFavoriteMain(favoriteList);
        Logger.d(TAG, "updateFavoriteMain size" + favoriteList.size());
    }

    public void removeFavoriteMain(PoiInfoEntity poiInfoEntity) {
        if (!ConvertUtils.isEmpty(poiInfoEntity)){
            FavoriteInfo favoriteInfo = poiInfoEntity.getFavoriteInfo();
            if (!ConvertUtils.isEmpty(favoriteInfo)) {
                Logger.d(TAG, "removeFavoriteMain remove id " + favoriteInfo.getItemId());
                getLayerUserControl().getUserLayer(BizUserType.BizUserTypeFavoriteMain).removeItem(favoriteInfo.getItemId());
            } else {
                Logger.e(TAG, "favoriteInfo is null");
            }
        } else {
            Logger.e(TAG, "poiInfoEntity is null");
        }
    }

    public void hideOrShowFavoriteMain(boolean isShow){
        getLayerUserControl().setVisible(BizUserType.BizUserTypeFavoriteMain, isShow);
        getLayerUserControl().updateStyle(BizUserType.BizUserTypeFavoriteMain);
        Logger.d(TAG,"hideOrShowFavoriteMain ", isShow);
    }

//    public void cleanFavoriteToMain(){
//        getLayerUserControl().clearAllItems(BizUserType.BizUserTypeFavoriteMain);
//        getLayerUserControl().setVisible(BizUserType.BizUserTypeFavoriteMain,false);
//        Logger.d(TAG,"cleanFavoriteToMain");
//    }

    /* 清除*/
    public void clearAllItems() {
        getLayerUserControl().clearAllItems();
        getLayerUserControl().setVisible(false);
    }

    /* 设置是否显示*/
    public void setFavoriteVisible(boolean visible) {
        getLayerUserControl().setVisible(BizUserType.BizUserTypeFavoriteMain, visible);
    }

    /**
     * 数据转换深度信息
     *
     * @param depInfo 深度信息
     * @return 深度信息
     */
    private GpsTrackDepthInfo getGpsTrackDepthInfo(final GpsTrackDepthBean depInfo) {

        if (ConvertUtils.isEmpty(depInfo)) {
            return null;
        }
        final GpsTrackDepthInfo info = new GpsTrackDepthInfo();
        info.fileName = depInfo.getFileName();
        info.filePath = depInfo.getFilePath();
        info.fastestIndex = depInfo.getFastestIndex();
        info.distance = depInfo.getDistance();
        info.duration = depInfo.getDuration();
        info.averageSpeed = depInfo.getAverageSpeed();

        final ArrayList<GpsTrackPoint> getGpsTrackPointBean = new ArrayList<>();
        for (GpsTrackPointBean point : depInfo.getTrackPoints()) {
            getGpsTrackPointBean.add(getGpsTrackPointBean(point));
        }
        info.trackPoints = getGpsTrackPointBean;
        return info;
    }

    /**
     * 数据转换Gps轨迹点信息
     *
     * @param point Gps轨迹点信息
     * @return Gps轨迹点信息
     */
    private GpsTrackPoint getGpsTrackPointBean(final GpsTrackPointBean point) {

        final GpsTrackPoint pointBean = new GpsTrackPoint();
        pointBean.f64Latitude = point.getF64Latitude();
        pointBean.f64Longitude = point.getF64Longitude();
        pointBean.f32Accuracy = point.getF32Accuracy();
        pointBean.f32Speed = point.getF32Speed();
        pointBean.f32Course = point.getF32Course();
        pointBean.n64TickTime = point.getN64TickTime();
        pointBean.n32SateliteTotal = point.getN32SateliteTotal();
        pointBean.nSectionId = point.getSectionId();
        return pointBean;
    }

    @Override
    protected void dispatchItemClickEvent(LayerItem item, ClickViewIdInfo clickViewIds) {
        if (ConvertUtils.isEmpty(item)) {
            Logger.e(TAG, "dispatchItemClickEvent item is null");
            return;
        }
        switch (item.getBusinessType()) {
            case BizUserType.BizUserTypeFavoriteMain: {
                if ((item instanceof FavoritePointLayerItem) && getCallBack() != null) {
                    FavoritePointLayerItem favoritePointLayerItem = (FavoritePointLayerItem) item;
                    Coord3DDouble coord3DDouble = favoritePointLayerItem.getPosition();
                    //进行数据转换
                    GeoPoint geoPoint = new GeoPoint();
                    geoPoint.setLat(coord3DDouble.lat);
                    geoPoint.setLon(coord3DDouble.lon);
                    PoiInfoEntity poiInfo = new PoiInfoEntity();
                    poiInfo.setPoiType(AutoMapConstant.SearchType.GEO_SEARCH);
                    poiInfo.setPoint(geoPoint);
                    poiInfo.setPid(favoritePointLayerItem.getID());
                    getCallBack().onFavoriteClick(getMapType(), poiInfo);
                }
                break;
            }
        }
    }
}
