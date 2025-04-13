package com.fy.navi.service.adapter.layer.bls.impl;

import static com.fy.navi.service.MapDefaultFinalTag.LAYER_SERVICE_TAG;

import android.content.Context;
import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.Coord3DDouble;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.FavoritePointLayerItem;
import com.autonavi.gbl.layer.model.BizUserFavoritePoint;
import com.autonavi.gbl.layer.model.BizUserType;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.layer.BaseLayer;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.model.ClickViewIdInfo;
import com.autonavi.gbl.user.behavior.model.FavoriteType;
import com.autonavi.gbl.user.usertrack.model.GpsTrackDepthInfo;
import com.autonavi.gbl.user.usertrack.model.GpsTrackPoint;
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.fy.navi.service.adapter.layer.bls.style.LayerUserStyleAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.refix.LayerItemUserFavorite;
import com.fy.navi.service.define.layer.refix.LayerItemUserTrackDepth;
import com.fy.navi.service.define.map.GmBizUserFavoritePoint;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.user.usertrack.GpsTrackDepthBean;
import com.fy.navi.service.define.user.usertrack.GpsTrackPointBean;
import java.util.ArrayList;
import java.util.List;

public class LayerUserImpl extends BaseLayerImpl<LayerUserStyleAdapter> {

    public LayerUserImpl(BizControlService bizService, MapView mapView, Context context, MapType mapType) {
        super(bizService, mapView, context,mapType);
        getLayerUserControl().setStyle(this);
        getLayerUserControl().addClickObserver(this);
        getLayerUserControl().addFocusChangeObserver(this);
    }

    @Override
    protected LayerUserStyleAdapter createStyleAdapter() {
        return new LayerUserStyleAdapter(getEngineId(),getLayerUserControl());
    }

    public void updateFavoriteMain(List<GmBizUserFavoritePoint> list) {
        ArrayList<BizUserFavoritePoint> points = new ArrayList<>();
        list.forEach((entity) -> {
            BizUserFavoritePoint point = new BizUserFavoritePoint();
            point.favoriteType = entity.favoriteType;
            point.mPos3D.lat = entity.lat;
            point.mPos3D.lon = entity.lon;
            points.add(point);
        });
        clearFavoriteMain();
        getLayerUserControl().updateFavoriteMain(points);
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
            Logger.e(LAYER_SERVICE_TAG,"updateGpsTrack null");
            return;
        }

        final GpsTrackDepthInfo info = getGpsTrackDepthInfo(depthInfo.getGpsTrackDepthBean());
        //显示用户历史轨迹相关图层
        getLayerUserControl().updateGpsTrack(info);
        getLayerUserControl().setVisible(BizUserType.BizUserTypeGpsTrack,true);
        getLayerUserControl().setVisible(BizUserType.BizUserTypeGpsTrackLine,true);
        Logger.d(LAYER_SERVICE_TAG,"updateGpsTrack");
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
        ArrayList<BizUserFavoritePoint> favoriteList = new ArrayList<>();
        ArrayList<PoiInfoEntity> mSimpleFavoriteList =  userFavorite.getMSimpleFavoriteList();
        PoiInfoEntity mHomeFavorite = userFavorite.getMHomeFavoriteList();
        PoiInfoEntity mCompanyFavorite = userFavorite.getMCompanyFavoriteList();
        if(!ConvertUtils.isEmpty(mSimpleFavoriteList)){
                for (int i = 0; i < mSimpleFavoriteList.size(); i++) {
                    BizUserFavoritePoint point = new BizUserFavoritePoint();
                    point.favoriteType = FavoriteType.FavoriteTypePoi;
                    GeoPoint geoPoint = mSimpleFavoriteList.get(i).getMPoint();
                    if(!ConvertUtils.isEmpty(geoPoint)){
                        point.mPos3D.lat = geoPoint.getLat();
                        point.mPos3D.lon = geoPoint.getLon();
                    }
                    favoriteList.add(point);
            }
        }
        if(!ConvertUtils.isEmpty(mHomeFavorite)){
            BizUserFavoritePoint point = new BizUserFavoritePoint();
            point.favoriteType = FavoriteType.FavoriteTypeHome;
            GeoPoint geoPoint = mHomeFavorite.getMPoint();
            if(!ConvertUtils.isEmpty(geoPoint)){
                point.mPos3D.lat = geoPoint.getLat();
                point.mPos3D.lon = geoPoint.getLon();
            }
            favoriteList.add(point);
        }
        if(!ConvertUtils.isEmpty(mCompanyFavorite)){
            BizUserFavoritePoint point = new BizUserFavoritePoint();
            point.favoriteType = FavoriteType.FavoriteTypeCompany;
            GeoPoint geoPoint = mCompanyFavorite.getMPoint();
            if(!ConvertUtils.isEmpty(geoPoint)){
                point.mPos3D.lat = geoPoint.getLat();
                point.mPos3D.lon = geoPoint.getLon();
            }
            favoriteList.add(point);
        }
        if(ConvertUtils.isEmpty(favoriteList)){
            Logger.e(LAYER_SERVICE_TAG,"updateFavoriteMain null");
            return;
        }

        getLayerUserControl().updateFavoriteMain(favoriteList);
        getLayerUserControl().setVisible(BizUserType.BizUserTypeFavoriteMain,true);
        Logger.d(LAYER_SERVICE_TAG,"updateFavoriteMain size"+favoriteList.size());
    }

//    public void cleanFavoriteToMain(){
//        getLayerUserControl().clearAllItems(BizUserType.BizUserTypeFavoriteMain);
//        getLayerUserControl().setVisible(BizUserType.BizUserTypeFavoriteMain,false);
//        Logger.d(LAYER_SERVICE_TAG,"cleanFavoriteToMain");
//    }

    /* 清除*/
    public void clearAllItems() {
        getLayerUserControl().clearAllItems();
        getLayerUserControl().setVisible(false);
    }

    /**
     * 数据转换深度信息
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
     * @param point Gps轨迹点信息
     * @return Gps轨迹点信息
     */
    private GpsTrackPoint getGpsTrackPointBean(final GpsTrackPointBean point) {

        final GpsTrackPoint pointBean = new GpsTrackPoint();
        pointBean.f64Latitude = point.getF64Latitude();
        pointBean.f64Longitude = point.getF64Longitude();
        pointBean.f64Altitude = point.getF64Altitude();
        pointBean.f32Accuracy = point.getF32Accuracy();
        pointBean.f32Speed = point.getF32Speed();
        pointBean.f32Course = point.getF32Course();
        pointBean.n64TickTime = point.getN64TickTime();
        pointBean.n32SateliteTotal = point.getN32SateliteTotal();
        pointBean.nSectionId = point.getSectionId();
        return pointBean;
    }

    @Override
    public void onNotifyClick(BaseLayer layer, LayerItem pItem, ClickViewIdInfo clickViewIds) {
        Logger.d(LAYER_SERVICE_TAG, "onNotifyClick");
        int itemType = pItem.getBusinessType();
        if(itemType == BizUserType.BizUserTypeFavoriteMain){
            if(pItem instanceof FavoritePointLayerItem){
                FavoritePointLayerItem favoritePointLayerItem = (FavoritePointLayerItem) pItem;
                Coord3DDouble coord3DDouble = favoritePointLayerItem.getPosition();
                //进行数据转换
                GeoPoint geoPoint = new GeoPoint();
                geoPoint.setLat(coord3DDouble.lat);
                geoPoint.setLon(coord3DDouble.lon);
                for (ILayerAdapterCallBack callBack : getCallBacks())
                    callBack.onFavoriteClick(geoPoint);
                }
            }
        }
}
