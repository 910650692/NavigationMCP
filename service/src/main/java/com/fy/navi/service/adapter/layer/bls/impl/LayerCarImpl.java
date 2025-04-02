package com.fy.navi.service.adapter.layer.bls.impl;

import android.content.Context;

import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.model.BizCarType;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.layer.model.CarLoc;
import com.autonavi.gbl.map.layer.model.CarMode;
import com.autonavi.gbl.map.layer.model.PathMatchInfo;
import com.autonavi.gbl.map.layer.model.SkeletonAnimationInfo;
import com.autonavi.gbl.map.layer.model.SkeletonDataInfoBase;
import com.autonavi.gbl.map.layer.model.SkeletonDataType;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.adapter.layer.bls.style.CarLayerStyleAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.refix.CarModeType;
import com.fy.navi.service.define.layer.refix.LayerItemCar;


public class LayerCarImpl extends BaseLayerImpl<CarLayerStyleAdapter> {

    public LayerCarImpl(BizControlService bizService, MapView mapView, Context context) {
        super(bizService, mapView, context);
        getLayerCarControl().setStyle(this);
        getLayerCarControl().setVisible(true);
        getLayerCarControl().setClickable(true);
        getLayerCarControl().addCarObserver(this);
        initSkeletonCarModel();
    }

    @Override
    protected CarLayerStyleAdapter createStyleAdapter() {
        return new CarLayerStyleAdapter();
    }


    /* 设置骨骼车标 暂不支持骨骼车标设置*/ //TODO
    private void initSkeletonCarModel() {
        SkeletonDataInfoBase dataInfo = new SkeletonDataInfoBase();
        dataInfo.type = SkeletonDataType.FBX;//格式由UE提供的资源模型决定
        dataInfo.skeletonDataPath = GBLCacheFilePath.COPY_ASSETS_DIR + "bls/style1/car_skeleton_logo/11/carLogo.dat";//旧接口HMI读取资源，新接口直接设置资源路径
        int a = getLayerCarControl().setSkeletonDataInfo(dataInfo);
        //默认不播动画，需要动画，要再调用动画接口
        SkeletonAnimationInfo animationInfo = new SkeletonAnimationInfo();
        animationInfo.animationName = "StatusMove"; // StatusStatic、StatusMove、StatusSpeed
        getLayerCarControl().setSkeletonAnimation(animationInfo);
    }

    /* 设置车标模式，2D车标/3D车标/骨骼车标/车速车标 */
    public void setCarMode(LayerItemCar carMode) {
        int carModeType;
        switch (carMode.getCarModeType()) {
            case CAR_MODEL_TYPE_3D -> carModeType = CarMode.CarMode3D;
            case CAR_MODEL_TYPE_SKELETON -> carModeType = CarMode.CarModeSkeleton;
            case CAR_MODEL_TYPE_SPEED -> carModeType = CarMode.CarModeSpeed;
            default -> carModeType = CarMode.CarMode2D;
        }
        Logger.d(TAG, "CarMode.CarMode2D :" + carModeType);
        getLayerCarControl().setCarAnimationSwitch(true);
        getLayerCarControl().setCarMode(carModeType, true);
    }

    /* 设置车标位置信息。通常用于单次设置车标位置，频次低 */
    public void setCarPosition(GeoPoint geoPoint) {
        CarLoc carLocation = new CarLoc();
        PathMatchInfo info = new PathMatchInfo();
        info.longitude = geoPoint.getLon();
        info.latitude = geoPoint.getLat();
        info.carDir = 90;
        carLocation.vecPathMatchInfo.add(info);
        getLayerCarControl().setCarPosition(carLocation);
    }

    /* 设置设置跟随模式、自由模式 */
    public int setFollowMode(boolean bFollow) {
        return getLayerCarControl().setFollowMode(bFollow);
    }

    public void updateGuideCarStyle() {
        getLayerCarControl().updateStyle(BizCarType.BizCarTypeGuide);
    }

}
