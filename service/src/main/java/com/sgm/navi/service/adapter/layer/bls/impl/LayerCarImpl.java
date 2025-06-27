package com.sgm.navi.service.adapter.layer.bls.impl;

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
import com.autonavi.gbl.util.errorcode.common.Service;
import com.sgm.navi.service.GBLCacheFilePath;
import com.sgm.navi.service.adapter.layer.bls.style.LayerCarStyleAdapter;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.layer.refix.CarModeType;
import com.sgm.navi.service.define.map.MapType;

import java.io.File;

public class LayerCarImpl extends BaseLayerImpl<LayerCarStyleAdapter> {

    private static final String CAR_SKELETON_ROOT = "/carSkeleton/";

    private static final float[] CAR_SCALE = {1.0f, 1.0f, 1.0f, 0.6f, 0.6f, 0.6f, 0.6f, 0.6f, 0.6f, 0.8f, 0.8f, 0.8f, 0.8f, 0.8f, 0.8f, 0.8f, 0.8f, 0.8f, 0.8f, 0.8f, 1.0f};

    public LayerCarImpl(BizControlService bizService, MapView mapView, Context context, MapType mapType) {
        super(bizService, mapView, context, mapType);
        getLayerCarControl().setStyle(this);
        getLayerCarControl().setVisible(true);
        getLayerCarControl().setClickable(true);
        getLayerCarControl().addCarObserver(this);
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
    protected void dispatchCarClick(CarLoc carLoc) {
        if (getCallBack() != null) {
            GeoPoint geoPoint = new GeoPoint();
            if (!carLoc.vecPathMatchInfo.isEmpty()) {
                PathMatchInfo pathMatchInfo = carLoc.vecPathMatchInfo.get(0);
                geoPoint.setLat(pathMatchInfo.latitude);
                geoPoint.setLon(pathMatchInfo.longitude);
            }
            Logger.d(TAG, getMapType() + " onCarClick =" + geoPoint.toString());
            getCallBack().onCarClick(getMapType(), geoPoint);
        }
    }

    public void initCarLogoByFlavor(String flavor) {
        Logger.d(TAG, "flavor is" + flavor);
        String carModel = switch (flavor) {
            case "cadi" -> "Cadi";
            default -> "Buick";
        };
        initSkeletonCarModel(carModel);
    }

    private void initSkeletonCarModel(String carModel) {
        SkeletonDataInfoBase dataInfo = new SkeletonDataInfoBase();
        dataInfo.type = SkeletonDataType.FBX;//格式由UE提供的资源模型决定
        dataInfo.skeletonDataPath = new StringBuffer(GBLCacheFilePath.BLS_ASSETS_CUSTOM_PATH)
                .append(getEngineId())
                .append(CAR_SKELETON_ROOT)
                .append(carModel).append(File.separator)
                .append("carLogo.dat").toString();
        int result = getLayerCarControl().setSkeletonDataInfo(dataInfo);
        if (result != Service.ErrorCodeOK) {
            dataInfo.skeletonDataPath = new StringBuffer(GBLCacheFilePath.BLS_ASSETS_CUSTOM_PATH)
                    .append(MapType.MAIN_SCREEN_MAIN_MAP.getMapType())
                    .append(CAR_SKELETON_ROOT)
                    .append(carModel).append(File.separator)
                    .append("carLogo.dat").toString();
            result = getLayerCarControl().setSkeletonDataInfo(dataInfo);
            Logger.d(TAG, "使用主图资源");
        }
        Logger.d(TAG, "初始化骨骼车标 ：" + result);
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
        Logger.d(TAG, "setCarPosition:");
        getLayerCarControl().setCarPosition(carLocation);
    }

    /* 设置设置跟随模式、自由模式 */
    public int setFollowMode(boolean bFollow) {
        if(Logger.openLog) Logger.d(TAG, "setFollowMode: " + bFollow);
        return getLayerCarControl().setFollowMode(bFollow);
    }

    public void updateGuideCarStyle() {
        getLayerCarControl().updateStyle(BizCarType.BizCarTypeGuide);
    }

    /* 设置车标是否显示 */
    public void setCarLogoVisible(boolean visible) {
        Logger.d(TAG, "setCarLogoVisible visible " + visible);
        getLayerCarControl().setVisible(visible);
    }

    /**
     * 车标预览模式
     *
     * @param bPreview
     */
    public void setPreviewMode(boolean bPreview) {
        getLayerCarControl().setPreviewMode(bPreview);
        if(Logger.openLog) Logger.d(TAG, "setPreviewMode bPreview " + bPreview);
    }

    /* 设置车标缩放系数和比例尺对应关系 */
    private void initCarScaleByMapLevel() {
        boolean result = getLayerCarControl().setCarScaleByMapLevel(CAR_SCALE);
        Logger.d(TAG, "setCarScaleByMapLevel result " + result);
    }

    public CarModeType getCarModeType() {
        return switch (getLayerCarControl().getCarMode()) {
            case CarMode.CarModeSpeed -> CarModeType.CAR_MODEL_SPEED;
            case CarMode.CarModeSkeleton -> CarModeType.CAR_MODEL_BRAND;
            default -> CarModeType.CAR_MODE_DEFAULT;
        };
    }

    /* 设置骨骼车标的基础缩放值 */
    public void setSkeletonBaseScale(float f) {
        Logger.d(TAG, "setSkeletonBaseScale f " + f);
        getLayerCarControl().setSkeletonBaseScale(f);
    }

    /* 设置3D车模缩放比例 */
    public void setModelScale(float f) {
        Logger.d(TAG, "setModelScale f " + f);
        getLayerCarControl().setModelScale(f);
    }
}
