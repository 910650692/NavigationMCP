package com.fy.navi.scene.impl.navi;

import android.graphics.drawable.Drawable;
import android.os.Handler;
import android.os.Looper;

import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.R;
import com.fy.navi.scene.ui.navi.SceneNaviViaDetailView;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.navi.NaviViaEntity;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.GasStationInfo;
import com.fy.navi.service.define.search.ParkingInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.define.utils.NumberUtils;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;

import java.util.List;


public class SceneNaviViaDetailImpl extends BaseSceneModel<SceneNaviViaDetailView> implements SearchResultCallback {

    public static final String TAG = "SceneNaviViaDetailImpl";
    private String mCurrentPoiId = null;
    private int mSearchId;

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
                if (null != mCurrentPoiId) {
                    mSearchId = SearchPackage.getInstance().poiIdSearch(mCurrentPoiId, true);
                    Logger.i(TAG, "run", "mSearchId:", mSearchId);
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
        boolean isCanShow = (mCurrentPoiType == AutoMapConstant.PointTypeCode.GAS_STATION ||
                mCurrentPoiType == AutoMapConstant.PointTypeCode.CHARGING_STATION ||
                mCurrentPoiType == AutoMapConstant.PointTypeCode.PARKING_LOT);
        Logger.i(TAG, "setShowViaDetail", "isCanShow:" + isCanShow + " isNeedShow = " +
                mIsNeedShow);
        updateSceneVisible(b && isCanShow);
    }

    public void updateNewestViaPoint(NaviViaEntity naviViaEntity) {
        if (null != naviViaEntity) {
            mCurrentPoiId = naviViaEntity.getPid();
            if (null != mCurrentPoiId) {
                ThreadManager.getInstance().removeHandleTask(mSearchPoi);
                ThreadManager.getInstance().postUi(mSearchPoi);
            }
        }
    }

    /**
     * @param isVisible visible
     */
    public void updateSceneVisible(final boolean isVisible) {
        Logger.i(TAG, "SceneNaviViaListImpl", "isVisible:"+ isVisible, "currentVis:" +
                mScreenView.isVisible());
        if(mScreenView.isVisible() == isVisible) return;
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ?
                        INaviSceneEvent.SceneStateChangeType.SceneShowState :
                        INaviSceneEvent.SceneStateChangeType.SceneCloseState),
                NaviSceneId.NAVI_SCENE_VIA_DETAIL);
    }

    @Override
    public void onSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {

    }

    @Override
    public void onSilentSearchResult(int taskId, int errorCode, String message,
                                     SearchResultEntity searchResultEntity) {
        Logger.i(TAG, "onSilentSearchResult", "taskId:" + taskId, "errorCode:" +
                errorCode, "message:" + message);
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
        Logger.i(TAG, "poiInfo:" + poiInfo);
        if (poiInfo == null) {
            return;
        }
        if (null != mScreenView) {
            mViaTitle.set(poiInfo.getName());
        }
        String poiTypeCode = poiInfo.getPointTypeCode();
        int poiType = SearchPackage.getInstance().getPointTypeCode(poiTypeCode);
        mCurrentPoiType = poiType;
        Logger.i(TAG, "poiType:" + poiType);
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
            Logger.i(TAG, "showGasStationDetail: " + gasType);
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
        Logger.i(TAG, "showGasStationDetail = " + mViaContent.get());
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
        Logger.i(TAG, "showParkingDetail = " + mViaContent.get() +
                " spaceFree = " + spaceFree + " spaceTotal = " + spaceTotal);
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
        Logger.i(TAG, "showChargeStationDetail = " + mViaContent.get() +
                " slowSpaceFree = " + slowSpaceFree + " slowSpaceTotal = " + slowSpaceTotal +
                " fastSpaceFree = " + fastSpaceFree + " fastSpaceTotal = " + fastSpaceTotal);
        mSlowChargeTotal.set(slowSpaceTotal + "");
        mFastChargeTotal.set(fastSpaceTotal + "");
        mSlowChargeFree.set(slowSpaceFree + "");
        mFastChargeFree.set(fastSpaceFree + "");
    }

}
