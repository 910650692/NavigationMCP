package com.fy.navi.service.adapter.layer.bls.refix;

import android.content.Context;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.model.BizCarType;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.layer.model.CarLoc;
import com.autonavi.gbl.map.layer.model.CarMode;
import com.autonavi.gbl.map.layer.model.SkeletonAnimationInfo;
import com.autonavi.gbl.map.layer.model.SkeletonDataInfoBase;
import com.autonavi.gbl.map.layer.model.SkeletonDataType;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.adapter.layer.bls.refix.style.CarLayerStyleAdapter;
import com.fy.navi.service.define.layer.CarModeType;
import com.fy.navi.service.define.layer.bls.CarLocation;
import com.fy.navi.service.define.layer.refix.LayerItemCar;


public class LayerCar extends BaseLayerImpl<CarLayerStyleAdapter> {

    public LayerCar(BizControlService bizService, MapView mapView, Context context) {
        super(bizService, mapView, context);
        getLayerCarControl().setStyle(this);
        getLayerCarControl().setClickable(true);
        getLayerCarControl().addCarObserver(this);
        initSkeletonCarModel();
    }

    @Override
    protected CarLayerStyleAdapter createStyleAdapter() {
        return new CarLayerStyleAdapter();
    }

    /* 控制车标图层显隐 */
    public void setCarModeVisible(boolean isVisible) {
        getLayerCarControl().setVisible(isVisible);
    }

    /* 控制车标图层是否可点击 */
    public void setCarModeClickable(boolean bClickable) {
        getLayerCarControl().setClickable(bClickable);
    }

    /* 设置车标模式，2D车标/3D车标/骨骼车标/车速车标 */
    public void setCarMode(@CarModeType.CarModelTypeId int carMode) {
        int carModeType;
        switch (carMode) {
            case CarModeType.CAR_MODEL_TYPE_3D -> carModeType = CarMode.CarMode3D;
            case CarModeType.CAR_MODEL_TYPE_SKELETON -> carModeType = CarMode.CarModeSkeleton;
            case CarModeType.CAR_MODEL_TYPE_SPEED -> carModeType = CarMode.CarModeSpeed;
            default -> carModeType = CarMode.CarMode2D;
        }
        Logger.d(TAG, "CarMode.CarMode2D :" + carModeType);
        getLayerCarControl().setCarAnimationSwitch(true);
        LayerItemCar itemCar = new LayerItemCar();
        itemCar.setSpeed(90);
        getStyleAdapter().updateLayerItemCar(itemCar);
        getLayerCarControl().setCarMode(carModeType, true);
    }

    /* 获取车标模式，2D车标/3D车标/骨骼车标/车速车标 */
    public int getCarMode() {
        int carModeType = getLayerCarControl().getCarMode();
        return switch (carModeType) {
            case CarMode.CarMode3D -> CarModeType.CAR_MODEL_TYPE_3D;
            case CarMode.CarModeSkeleton -> CarModeType.CAR_MODEL_TYPE_SKELETON;
            case CarMode.CarModeSpeed -> CarModeType.CAR_MODEL_TYPE_SPEED;
            case CarMode.CarMode2D -> CarModeType.CAR_MODEL_TYPE_2D;
            case CarMode.AUTO_UNKNOWN_ERROR -> CarModeType.CAR_MODEL_TYPE_2D;
            default -> CarModeType.CAR_MODEL_TYPE_2D;
        };
    }

    /* 获取当前车标模式，2D车标/3D车标/骨骼车标/车速车标 */
    public int getCurrentCarModeType() {
        int carModeType = getLayerCarControl().getCarMode();
        switch (carModeType) {
            case CarMode.CarMode3D -> {
                return CarModeType.CAR_MODEL_TYPE_3D;
            }
            case CarMode.CarModeSkeleton -> {
                return CarModeType.CAR_MODEL_TYPE_SKELETON;
            }
            case CarMode.CarModeSpeed -> {
                return CarModeType.CAR_MODEL_TYPE_SPEED;
            }
            default -> {
                return CarModeType.CAR_MODEL_TYPE_2D;
            }
        }
    }

    /* 设置车头朝上模式 */
    public void setCarUpMode(boolean bCarUp) {
        getLayerCarControl().setCarUpMode(bCarUp);
    }

    /* 设置车标位置信息。通常用于单次设置车标位置，频次低 */
    public void setCarPosition(CarLocation carLocation) {
        CarLoc carLoc = GsonUtils.convertToT(carLocation, CarLoc.class);
        getLayerCarControl().setCarPosition(carLoc);
    }

    /* 更新车标位置信息，用于定位引擎下发定位消息驱动使用，频次高 */
    public void updateCarPosition(CarLocation carLocation) {
        CarLoc carLoc = GsonUtils.convertToT(carLocation, CarLoc.class);
        getLayerCarControl().setCarPosition(carLoc);
    }

    /* 设置车标缩放系数和比例尺对应关系 */
    public boolean setCarScaleByMapLevel(float[] vScales) {
        return getLayerCarControl().setCarScaleByMapLevel(vScales);
    }

    /* 设置设置跟随模式、自由模式 */
    public int setFollowMode(boolean bFollow) {
        return getLayerCarControl().setFollowMode(bFollow);
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

    public void updateGuideCarStyle() {
        getLayerCarControl().updateStyle(BizCarType.BizCarTypeGuide);
    }

}
