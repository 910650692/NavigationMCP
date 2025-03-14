package com.fy.navi.service.adapter.layer.bls.manager;

import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.Coord3DDouble;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.model.BizCarType;
import com.autonavi.gbl.layer.model.BizPopPointBusinessInfo;
import com.autonavi.gbl.layer.model.BizUserFavoritePoint;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.layer.BaseLayer;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.model.CarLoc;
import com.autonavi.gbl.map.layer.model.CarMode;
import com.autonavi.gbl.map.layer.model.ClickViewIdInfo;
import com.autonavi.gbl.map.layer.model.SkeletonAnimationInfo;
import com.autonavi.gbl.map.layer.model.SkeletonDataInfoBase;
import com.autonavi.gbl.map.layer.model.SkeletonDataType;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.define.layer.CarModeType;
import com.fy.navi.service.define.layer.bls.CarLocation;
import com.fy.navi.service.define.map.GmBizUserFavoritePoint;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;
import java.util.List;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/9
 */
public class MapLayerStyle extends BaseLayerStyle {

    public MapLayerStyle(BizControlService bizService, MapView mapView) {
        super(bizService, mapView);
    }

    /* 控制车标图层显隐 */
    public void setCarModeVisible(boolean isVisible) {
        mBziCarControl.setVisible(isVisible);
    }

    /* 控制车标图层是否可点击 */
    public void setCarModeClickable(boolean bClickable) {
        mBziCarControl.setClickable(bClickable);
    }

    /* 设置车标模式，2D车标/3D车标/骨骼车标/车速车标 */
    public void setCarMode(@CarModeType.CarModelTypeId int carMode) {
        int carModeType;
        switch (carMode) {
            case CarModeType.CAR_MODEL_TYPE_2D -> carModeType = CarMode.CarMode2D;
            case CarModeType.CAR_MODEL_TYPE_3D -> carModeType = CarMode.CarMode3D;
            case CarModeType.CAR_MODEL_TYPE_SKELETON -> {
                setSkeletonCarModel();
                carModeType = CarMode.CarModeSkeleton;
            }
            case CarModeType.CAR_MODEL_TYPE_SPEED -> carModeType = CarMode.CarModeSpeed;
            default -> carModeType = CarMode.AUTO_UNKNOWN_ERROR;
        }
        mBziCarControl.setCarAnimationSwitch(true);
        mBziCarControl.setCarMode(carModeType, true);
    }

    /* 获取车标模式，2D车标/3D车标/骨骼车标/车速车标 */
    public int getCarMode() {
        int carModeType = mBziCarControl.getCarMode();
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
        int carModeType = mBziCarControl.getCarMode();
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
        mBziCarControl.setCarUpMode(bCarUp);
    }

    /* 设置车标位置信息。通常用于单次设置车标位置，频次低 */
    public void setCarPosition(CarLocation carLocation) {
        CarLoc carLoc = GsonUtils.convertToT(carLocation, CarLoc.class);
        Logger.i("lvww", "设置车标位置");
        mBziCarControl.setCarPosition(carLoc);
    }

    /* 更新车标位置信息，用于定位引擎下发定位消息驱动使用，频次高 */
    public void updateCarPosition(CarLocation carLocation) {
        CarLoc carLoc = GsonUtils.convertToT(carLocation, CarLoc.class);
        mBziCarControl.updateCarPosition(carLoc);
    }

    /* 设置车标缩放系数和比例尺对应关系 */
    public boolean setCarScaleByMapLevel(float[] vScales) {
        return mBziCarControl.setCarScaleByMapLevel(vScales);
    }

    /* 设置设置跟随模式、自由模式 */
    public int setFollowMode(boolean bFollow) {
        return mBziCarControl.setFollowMode(bFollow);
    }

    /* 设置骨骼车标 暂不支持骨骼车标设置*/
    private void setSkeletonCarModel() {
        SkeletonDataInfoBase dataInfo = new SkeletonDataInfoBase();
        dataInfo.type = SkeletonDataType.FBX;//格式由UE提供的资源模型决定
        dataInfo.skeletonDataPath = GBLCacheFilePath.COPY_ASSETS_DIR + "bls/style1/car_skeleton_logo/11/carLogo.dat";//旧接口HMI读取资源，新接口直接设置资源路径
        int a = mBziCarControl.setSkeletonDataInfo(dataInfo);
        //默认不播动画，需要动画，要再调用动画接口
        SkeletonAnimationInfo animationInfo = new SkeletonAnimationInfo();
        animationInfo.animationName = "StatusMove"; // StatusStatic、StatusMove、StatusSpeed
        int b = mBziCarControl.setSkeletonAnimation(animationInfo);
    }

    public void setPoiClickMark(MapTypeId mapTypeId, @Nullable PoiInfoEntity entity) {
        final ArrayList<BizPopPointBusinessInfo> bizPopPointBusinessInfos = new ArrayList<>();
        if (entity != null) {
            final BizPopPointBusinessInfo bizPopPointBusinessInfo = new BizPopPointBusinessInfo();
            bizPopPointBusinessInfo.id = entity.getPid();
            bizPopPointBusinessInfo.text = entity.getName();
            bizPopPointBusinessInfo.mPos3D = new Coord3DDouble(
                    entity.getPoint().getLon(),
                    entity.getPoint().getLat(),
                    entity.getPoint().getZ()
            );
            bizPopPointBusinessInfos.add(bizPopPointBusinessInfo);
            mBizLabelControl.updatePopEndAreaPointBoxInfo(bizPopPointBusinessInfos);
        }
    }

    public void updateGuideCarStyle() {
        if (null != mBziCarControl) {
            mBziCarControl.updateStyle(BizCarType.BizCarTypeGuide);
        }
    }

    @Override
    protected void unInit() {
        if (!ConvertUtils.isEmpty(mBziCarControl)) mBziCarControl = null;
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
        mBizUserControl.updateFavoriteMain(points);
    }

    public void clearFavoriteMain() {
        mBizUserControl.clearAllItems();
    }
}
