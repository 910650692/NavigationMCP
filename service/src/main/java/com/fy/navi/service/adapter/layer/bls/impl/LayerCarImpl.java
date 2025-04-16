package com.fy.navi.service.adapter.layer.bls.impl;

import android.content.Context;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
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
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.fy.navi.service.adapter.layer.bls.style.LayerCarStyleAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.refix.CarModeType;
import com.fy.navi.service.define.map.MapType;

import java.util.function.Consumer;

public class LayerCarImpl extends BaseLayerImpl<LayerCarStyleAdapter> {

    private static final float[] CAR_SCALE = { 1.0f, 1.0f, 1.0f, 0.6f, 0.6f, 0.6f, 0.6f, 0.6f, 0.6f, 0.8f, 0.8f, 0.8f, 0.8f, 0.8f, 0.8f, 0.8f, 0.8f, 0.8f, 0.8f, 0.8f, 1.0f };

    public LayerCarImpl(BizControlService bizService, MapView mapView, Context context, MapType mapType) {
        super(bizService, mapView, context, mapType);
        getLayerCarControl().setStyle(this);
        getLayerCarControl().setVisible(true);
        getLayerCarControl().setClickable(true);
        getLayerCarControl().addCarObserver(this);
        initSkeletonCarModel();
        initCarScaleByMapLevel();
    }

    @Override
    protected LayerCarStyleAdapter createStyleAdapter() {
        return new LayerCarStyleAdapter(getEngineId(), getLayerCarControl());
    }

    @Override
    public void onCarLocChange(CarLoc carLoc) {
        getStyleAdapter().updateCarSpeed(carLoc.speed);
    }

    @Override
    public void onCarClick(CarLoc carLoc) {
        getCallBacks().forEach(new Consumer<ILayerAdapterCallBack>() {
            @Override
            public void accept(ILayerAdapterCallBack callBack) {
                if (callBack != null) {
                    GeoPoint geoPoint = new GeoPoint();
                    if (!carLoc.vecPathMatchInfo.isEmpty()) {
                        PathMatchInfo pathMatchInfo = carLoc.vecPathMatchInfo.get(0);
                        geoPoint.setLat(pathMatchInfo.latitude);
                        geoPoint.setLon(pathMatchInfo.longitude);
                    }
                    ThreadManager.getInstance().postUi(new Runnable() {
                        @Override
                        public void run() {
                            Logger.d(TAG, getMapType() + " onCarClick =" + geoPoint.toString());
                            callBack.onCarClick(getMapType(), geoPoint);
                        }
                    });
                }
            }
        });
    }

    /* 设置骨骼车标 暂不支持骨骼车标设置*/ //TODO
    private void initSkeletonCarModel() {
        SkeletonDataInfoBase dataInfo = new SkeletonDataInfoBase();
        dataInfo.type = SkeletonDataType.FBX;//格式由UE提供的资源模型决定
        dataInfo.skeletonDataPath = new StringBuffer(GBLCacheFilePath.BLS_ASSETS_CUSTOM_PATH)
                .append(getEngineId())
                .append("/carSkeleton/carLogo.dat").toString();
        int a = getLayerCarControl().setSkeletonDataInfo(dataInfo);
        //默认不播动画，需要动画，要再调用动画接口
        SkeletonAnimationInfo animationInfo = new SkeletonAnimationInfo();
        animationInfo.animationName = "StatusMove"; // StatusStatic、StatusMove、StatusSpeed
        getLayerCarControl().setSkeletonAnimation(animationInfo);
    }

    /* 设置车标模式，2D车标/3D车标/骨骼车标/车速车标 */
    public void setCarMode(CarModeType carModeType) {
        int carMode;
        switch (carModeType) {
            case CAR_MODEL_BRAND -> carMode = CarMode.CarModeSkeleton;
            case CAR_MODEL_SPEED -> carMode = CarMode.CarModeSpeed;
            default -> carMode = CarMode.CarMode2D;
        }
        Logger.d(TAG, "setCarMode:" + carMode);
        getLayerCarControl().setCarAnimationSwitch(true);
        getLayerCarControl().setCarMode(carMode, true);

    }

    /* 设置车标位置信息。通常用于单次设置车标位置，频次低 */
    public void setCarPosition(GeoPoint geoPoint) {
        CarLoc carLocation = new CarLoc();
        PathMatchInfo info = new PathMatchInfo();
        info.longitude = geoPoint.getLon();
        info.latitude = geoPoint.getLat();
        info.carDir = geoPoint.getCourse();
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

    /**
     * 车标预览模式
     *
     * @param bPreview
     */
    public void setPreviewMode(boolean bPreview) {
        getLayerCarControl().setPreviewMode(bPreview);
    }

    /* 设置车标缩放系数和比例尺对应关系 */
    private void initCarScaleByMapLevel() {
        boolean result = getLayerCarControl().setCarScaleByMapLevel(CAR_SCALE);
        Logger.d(TAG, "setCarScaleByMapLevel result " + result);
    }

}
