package com.sgm.navi.scene.impl.navi;

import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.text.TextUtils;

import androidx.databinding.ObservableField;
import androidx.fragment.app.Fragment;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.RoutePath;
import com.sgm.navi.scene.impl.imersive.ImersiveStatus;
import com.sgm.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.sgm.navi.scene.impl.search.SearchFragmentFactory;
import com.sgm.navi.scene.ui.navi.SceneNaviViaDetailView;
import com.sgm.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.NaviViaEntity;
import com.sgm.navi.service.define.search.ChargeInfo;
import com.sgm.navi.service.define.search.GasStationInfo;
import com.sgm.navi.service.define.search.ParkingInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.define.utils.NumberUtils;
import com.sgm.navi.service.logicpaket.navi.OpenApiHelper;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.search.SearchResultCallback;
import com.sgm.navi.ui.base.BaseFragment;

import java.util.List;
import java.util.Objects;


public class SceneNaviViaDetailImpl extends BaseSceneModel<SceneNaviViaDetailView> implements SearchResultCallback {

    public static final String TAG = MapDefaultFinalTag.NAVI_SCENE_VIA_DETAIL_IMPL;
    private NaviViaEntity mCurrentNaviViaEntity;
    private int mSearchId;
    private int powerType = NumberUtils.NUM_ERROR;

    public ObservableField<Drawable> mViaIcon;
    public ObservableField<String> mViaTitle;
    // 0:有空闲数据的充电站，1:无空闲数据的充电站，2:有空闲数据的停车场，3:无空闲数据的停车场，4:加油站
    public ObservableField<Integer> mViaContent;
    public ObservableField<Integer> mGas92Num;
    public ObservableField<Integer> mGas95Num;
    public ObservableField<Integer> mGas98Num;
    public ObservableField<String> mParkingFree;
    public ObservableField<String> mParkingTotal;
    public ObservableField<String> mSlowChargeFree;
    public ObservableField<String> mSlowChargeTotal;
    public ObservableField<String> mFastChargeFree;
    public ObservableField<String> mFastChargeTotal;
    // 1:显示 0:不显示 是否显示慢充
    public ObservableField<Integer> mSlowChargeShow;
    public ObservableField<Integer> mFastChargeShow;
    // 1:显示 0:不显示 是否显示慢充空闲车位
    public ObservableField<Integer> mSlowChargeShowFreeSpace;
    public ObservableField<Integer> mFastChargeShowFreeSpace;
    // 1:显示内容和名称 0:只显示名称
    public ObservableField<Integer> mContentShow;
    private int mCurrentPoiType = NumberUtils.NUM_ERROR;
    private boolean mIsNeedShow;
    public SceneNaviViaDetailImpl(SceneNaviViaDetailView mScreenView) {
        super(mScreenView);
        mViaIcon = new ObservableField<>(ResourceUtils.Companion.getInstance().
                getDrawable(R.drawable.img_refuel_rim_58));
        mViaTitle = new ObservableField<>("");
        mViaContent = new ObservableField<>(0);
        mGas92Num = new ObservableField<>(0);
        mGas95Num = new ObservableField<>(0);
        mGas98Num = new ObservableField<>(0);
        mParkingFree = new ObservableField<>("");
        mParkingTotal = new ObservableField<>("");
        mSlowChargeFree = new ObservableField<>("");
        mSlowChargeTotal = new ObservableField<>("");
        mFastChargeFree = new ObservableField<>("");
        mFastChargeTotal = new ObservableField<>("");
        mSlowChargeShow = new ObservableField<>(0);
        mFastChargeShow = new ObservableField<>(0);
        mContentShow = new ObservableField<>(0);
        mSlowChargeShowFreeSpace = new ObservableField<>(0);
        mFastChargeShowFreeSpace = new ObservableField<>(0);
    }

    @Override
    protected void onCreate() {
        super.onCreate();
        SearchPackage.getInstance().registerCallBack(TAG, this);
        powerType = OpenApiHelper.powerType();
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        SearchPackage.getInstance().unRegisterCallBack(TAG);
        ThreadManager.getInstance().removeHandleTask(mSearchPoi);
    }

    private Runnable mSearchPoi = new Runnable() {
        @Override
        public void run() {
            try {
                if (mCurrentNaviViaEntity != null) {
                    String poiID = mCurrentNaviViaEntity.getPid();
                    Logger.i(TAG, poiID);
                    if (!TextUtils.isEmpty(poiID) && !poiID.contains(".") && poiID.startsWith("B")) {
                        mSearchId = SearchPackage.getInstance().poiIdSearch(poiID, true);
                        Logger.i(TAG, "run", "mSearchId:", mSearchId);
                    } else if (!ConvertUtils.isNull(mCurrentNaviViaEntity.getRealPos())) {
                        mSearchId = SearchPackage.getInstance().geoSearch(
                                mCurrentNaviViaEntity.getRealPos(), true);
                        Logger.i(TAG, "run", "mSearchId:", mSearchId);
                    } else {
                        Logger.e(TAG, "poiId无效，地址为空，无法进行搜索");
                    }
                }
                ThreadManager.getInstance().postDelay(mSearchPoi,
                        NumberUtils.NUM_3 * NumberUtils.NUM_1000 * NumberUtils.NUM_60);
            } catch (Exception e) {
                Logger.e(TAG, e.getMessage());
            }
        }
    };

    public void showViaDetail(boolean b) {
        mIsNeedShow = b;
        // 纯电车不显示加油站，油车不显示充电站
        boolean isCanShow = ((mCurrentPoiType == AutoMapConstant.PointTypeCode.GAS_STATION &&
                powerType != NumberUtils.NUM_1) ||
                (mCurrentPoiType == AutoMapConstant.PointTypeCode.CHARGING_STATION &&
                        powerType == NumberUtils.NUM_1) ||
                mCurrentPoiType == AutoMapConstant.PointTypeCode.PARKING_LOT);
        Logger.d(TAG, "setShowViaDetail", "isCanShow:", isCanShow, " isNeedShow = ",
                mIsNeedShow);
        updateSceneVisible(b && isCanShow);
    }

    public void updateNewestViaPoint(NaviViaEntity naviViaEntity) {
        if (null != naviViaEntity) {
            // 不搜索重复的点,只有POIID和POS都一样才看作同一个点
            if (Objects.equals(mCurrentNaviViaEntity, naviViaEntity)) {
                return;
            }
            mCurrentNaviViaEntity = naviViaEntity;
            ThreadManager.getInstance().removeHandleTask(mSearchPoi);
            ThreadManager.getInstance().postUi(mSearchPoi);
        }
    }

    /**
     * @param isVisible visible
     */
    public void updateSceneVisible(final boolean isVisible) {
        Logger.d(TAG, "SceneNaviViaDetailImpl", "isVisible:", isVisible, "currentVis:",
                mScreenView.isVisible());
        if(mScreenView.isVisible() == isVisible) return;
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ?
                        INaviSceneEvent.SceneStateChangeType.SceneShowState :
                        INaviSceneEvent.SceneStateChangeType.SceneCloseState), NaviSceneId.NAVI_SCENE_VIA_DETAIL);
    }

    @Override
    public void onSilentSearchResult(int taskId, int errorCode, String message,
                                     SearchResultEntity searchResultEntity) {
        Logger.i(TAG, "onSilentSearchResult", "taskId:", taskId, "errorCode:",
                errorCode, "message:", message);
        if (mSearchId == taskId) {
            Logger.i(TAG, "搜索结果");
            if (!ConvertUtils.isEmpty(searchResultEntity)) {
                List<PoiInfoEntity> poiInfos = searchResultEntity.getPoiList();
                if (!ConvertUtils.isEmpty(poiInfos)) {
                    PoiInfoEntity poiInfo = poiInfos.get(0);
                    updateUi(poiInfo);
                }
            }
        }
    }

    private void updateUi(PoiInfoEntity poiInfo) {
        Logger.i(TAG, "poiInfo:", poiInfo);
        if (poiInfo == null) {
            return;
        }
        if (null != mScreenView) {
            mViaTitle.set(poiInfo.getName());
        }
        String poiTypeCode = poiInfo.getPointTypeCode();
        int poiType = SearchPackage.getInstance().getPointTypeCode(poiTypeCode);
        mCurrentPoiType = poiType;
        Logger.i(TAG, "poiType:", poiType);
        // 加油站
        if (poiType == AutoMapConstant.PointTypeCode.GAS_STATION) {
            List<GasStationInfo> gasStationInfos = poiInfo.getStationList();
            if (!ConvertUtils.isEmpty(gasStationInfos)) {
                showGasStationDetail(gasStationInfos);
            }
        } else if (poiType == AutoMapConstant.PointTypeCode.PARKING_LOT) {
            List<ParkingInfo> parkingInfos = poiInfo.getParkingInfoList();
            if (!ConvertUtils.isEmpty(parkingInfos)) {
                ParkingInfo parkingInfo = parkingInfos.get(0);
                showParkingDetail(parkingInfo);
            }
        } else if (poiType == AutoMapConstant.PointTypeCode.CHARGING_STATION) {
            List<ChargeInfo> chargeInfos = poiInfo.getChargeInfoList();
            if (!ConvertUtils.isEmpty(chargeInfos)) {
                ChargeInfo chargeInfo = chargeInfos.get(0);
                showChargeStationDetail(chargeInfo);
            }
        } else {
            updateSceneVisible(false);
        }
    }

    private void showGasStationDetail(List<GasStationInfo> gasStationInfos) {
        if (null != mScreenView) {
            mViaIcon.set(ResourceUtils.Companion.getInstance().
                    getDrawable(R.drawable.img_refuel_rim_58));
        }
        mViaContent.set(NumberUtils.NUM_4);
        boolean isGas92 = false;
        boolean isGas95 = false;
        boolean isGas98 = false;
        for (GasStationInfo gasStationInfo : gasStationInfos) {
            String gasType = gasStationInfo.getType();
            Logger.i(TAG, "showGasStationDetail: ", gasType);
            if ("92#".equals(gasType)) {
                isGas92 = true;
            } else if ("95#".equals(gasType)) {
                isGas95 = true;
            } else if ("98#".equals(gasType)) {
                isGas98 = true;
            }
        }
        mGas92Num.set(isGas92 ? 1 : 0);
        mGas95Num.set(isGas95 ? 1 : 0);
        mGas98Num.set(isGas98 ? 1 : 0);
        boolean showGasContent = isGas92 || isGas95 || isGas98;
        if (showGasContent) {
            mContentShow.set(NumberUtils.NUM_1);
        } else {
            mContentShow.set(NumberUtils.NUM_0);
        }
        Logger.i(TAG, "showGasStationDetail = ", mViaContent.get());
    }

    private void showParkingDetail(ParkingInfo parkingInfo) {
        if (null != mScreenView) {
            mViaIcon.set(ResourceUtils.Companion.getInstance().
                    getDrawable(R.drawable.img_end_point_58));
        }
        int spaceFree = parkingInfo.getSpaceFree();
        // 视为未获取到有效数据
        if (spaceFree <= 0) {
            mViaContent.set(NumberUtils.NUM_3);
        } else {
            mViaContent.set(NumberUtils.NUM_2);
            mParkingFree.set(parkingInfo.getMSpaceFree() + "");
        }
        int spaceTotal = Math.max(parkingInfo.getSpaceTotal(), 0);
        mParkingTotal.set(spaceTotal + "");
        if (spaceTotal == 0) {
            mContentShow.set(NumberUtils.NUM_0);
        } else {
            mContentShow.set(NumberUtils.NUM_1);
        }
        Logger.i(TAG, "showParkingDetail = ", mViaContent.get(),
                " spaceFree = ", spaceFree, " spaceTotal = ", spaceTotal);
    }

    private void showChargeStationDetail(ChargeInfo chargeInfo) {
        if (null != mScreenView) {
            mViaIcon.set(ResourceUtils.Companion.getInstance().
                    getDrawable(R.drawable.img_lightning_58));
        }
        int slowSpaceFree = chargeInfo.getSlow_free();
        int fastSpaceFree = chargeInfo.getFast_free();
        int slowSpaceTotal = chargeInfo.getSlow_total();
        int fastSpaceTotal = chargeInfo.getFast_total();
        mViaContent.set(NumberUtils.NUM_0);
        boolean isShowSlowCharge = slowSpaceTotal > 0;
        boolean isShowSLowChargeFree = slowSpaceFree > 0;
        boolean isShowFastCharge = fastSpaceTotal > 0;
        boolean isShowFastChargeFree = fastSpaceFree > 0;
        if (!isShowSlowCharge && !isShowSLowChargeFree && !isShowFastCharge &&
                !isShowFastChargeFree) {
            mContentShow.set(NumberUtils.NUM_0);
            return;
        } else {
            mContentShow.set(NumberUtils.NUM_1);
        }
        if (isShowSlowCharge && isShowSLowChargeFree) {
            mSlowChargeShow.set(NumberUtils.NUM_1);
            mSlowChargeShowFreeSpace.set(NumberUtils.NUM_1);
        } else if (isShowSlowCharge) {
            mSlowChargeShow.set(NumberUtils.NUM_1);
            mSlowChargeShowFreeSpace.set(NumberUtils.NUM_0);
        } else {
            mSlowChargeShow.set(NumberUtils.NUM_0);
        }
        if (isShowFastCharge && isShowFastChargeFree) {
            mFastChargeShow.set(NumberUtils.NUM_1);
            mFastChargeShowFreeSpace.set(NumberUtils.NUM_1);
        } else if (isShowFastCharge) {
            mFastChargeShow.set(NumberUtils.NUM_1);
            mFastChargeShowFreeSpace.set(NumberUtils.NUM_0);
        } else {
            mFastChargeShow.set(NumberUtils.NUM_0);
        }
        Logger.i(TAG, "showChargeStationDetail = ", mViaContent.get(),
                " slowSpaceFree = ", slowSpaceFree, " slowSpaceTotal = ", slowSpaceTotal,
                " fastSpaceFree = ", fastSpaceFree, " fastSpaceTotal = ", fastSpaceTotal);
        mSlowChargeTotal.set(slowSpaceTotal + "");
        mFastChargeTotal.set(fastSpaceTotal + "");
        mSlowChargeFree.set(slowSpaceFree + "");
        mFastChargeFree.set(fastSpaceFree + "");
    }

    public void skipToPoiFragment() {
        Logger.d(TAG, "skipToPoiFragment");
        if (mCurrentNaviViaEntity == null) {
            return;
        }
        final Fragment fragment = (Fragment) ARouter.getInstance()
                .build(RoutePath.Search.POI_DETAILS_FRAGMENT)
                .navigation();
        final PoiInfoEntity poiInfo = new PoiInfoEntity();
        poiInfo.setPid(mCurrentNaviViaEntity.getPid());
        poiInfo.setPoint(mCurrentNaviViaEntity.getRealPos());
        Bundle bundle = SearchFragmentFactory.
                createPoiDetailsFragment(
                        AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT,
                        AutoMapConstant.PoiType.POI_DELETE_AROUND, poiInfo);
        bundle.putInt(NaviConstant.NAVI_CONTROL, 1);
        bundle.putInt(NaviConstant.VIA_POSITION, NumberUtils.NUM_0);
        bundle.putBoolean(NaviConstant.VIA_IS_USER_ADD, mCurrentNaviViaEntity.isUserAdd());
        addFragment((BaseFragment) fragment, bundle, false);
        ImmersiveStatusScene.getInstance().setImmersiveStatus(
                MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.TOUCH);
        if (mCallBack != null) {
            mCallBack.hideNaviContent();
        }
    }

}
