package com.sgm.navi.hmi.splitscreen;

import android.app.Application;
import android.graphics.Rect;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.SplitScreenManager;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.hmi.map.MapActivity;
import com.sgm.navi.service.define.cruise.CruiseInfoEntity;
import com.sgm.navi.service.utils.ExportIntentParam;
import com.sgm.navi.hmi.launcher.FloatViewManager;
import com.sgm.navi.mapservice.bean.INaviConstant;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.impl.imersive.ImersiveStatus;
import com.sgm.navi.service.define.calibration.PowerType;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.ui.action.Action;
import com.sgm.navi.ui.base.BaseViewModel;

import java.util.concurrent.ScheduledFuture;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/6/12
 * Description: [在这里描述文件功能]
 */
public class BaseSplitViewModel extends BaseViewModel<SplitFragment, SplitModel> {
    private static final String TAG = "BaseSplitViewModel";

    public BaseSplitViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected SplitModel initModel() {
        return new SplitModel();
    }

    public ObservableField<Boolean> mTopNaviBarVisibility = new ObservableField<>(false);
    // 导航态按钮组
    public ObservableField<Boolean> mNaviActionBarVisibility = new ObservableField<>(false);
    public ObservableField<Boolean> mNaviBroadIsMute = new ObservableField<>(false);
    public ObservableField<Integer> mNaviVoicePic = new ObservableField<>(com.sgm.navi.scene.R.drawable.img_mute_broadcast_black_58);
    public ObservableField<Boolean> mCrossImageVisibility = new ObservableField<>(false);
    public ObservableField<Boolean> mIsGasCar = new ObservableField<>(false);// true代表油车
    public ObservableField<Boolean> mIsOnTouch = new ObservableField<>(false);// 继续导航按钮
    public ObservableField<Boolean> mIsOnShowPreview = new ObservableField<>(false);
    public ObservableField<Boolean> mLanesVisibility = new ObservableField<>(false);
    public ObservableField<Boolean> mNextManeuverVisible = new ObservableField<>(false);
    public ObservableField<Boolean> mCruiseLanesVisibility = new ObservableField<>(false);
    public ObservableField<Boolean> mCruisebg = new ObservableField<>(false);
    public ObservableField<Boolean> mSlStationVisibility = new ObservableField<>(true);
    public ObservableField<Boolean> muteVisibility = new ObservableField<>(true);
    public ObservableField<Boolean> cruiseUiVisibility = new ObservableField<>(true);

    // 触摸态后开启倒计时，8秒后进入沉浸态
    private final long TOTAL_TIME = 8;
    private ScheduledFuture immersiveScheduledFuture;
    // 全览后开启倒计时，8秒退出全览
    private ScheduledFuture previewScheduledFuture;

    @Override
    public void onCreate() {
        super.onCreate();
        cruiseUiVisibility.set(TextUtils.equals(mModel.getCurrentNaviStatus(), NaviStatus.NaviStatusType.CRUISE));
    }

    public void initView() {
        if (ConvertUtils.isNull(mView) || ConvertUtils.isNull(mModel)) return;
        mTopNaviBarVisibility.set(!mModel.isOnNavigating());
        mNaviActionBarVisibility.set(mModel.isOnNavigating());
        mNaviBroadIsMute.set(mModel.isMute());
        mNaviVoicePic.set(mModel.isMute() ? R.drawable.img_mute_broadcast_black_58 : R.drawable.img_navi_broadcast);
        mIsGasCar.set(mModel.getPowerType() == PowerType.E_VEHICLE_ENERGY_FUEL);
        mIsOnTouch.set(mModel.isOnTouch() && mModel.isOnNavigating());
        if (mModel.isOnNavigating()) {
            onNaviInfo(mModel.getCurrentNaviEtaInfo());
        }
        if (mModel.isOnTouch() && mModel.isOnNavigating()) {
            startImmersiveSchedule();
        }
        mIsOnShowPreview.set(mModel.getPreviewStatus());
    }

    /***
     * 回家
     */
    public Action goHome = () -> {
        Logger.i(TAG, "goHome");
        startMapActivity(INaviConstant.OpenIntentPage.GO_HOME);
        SplitScreenManager.getInstance().switchNaviToFullScreen();
    };

    /***
     * 去公司
     */
    public Action goCompany = () -> {
        Logger.i(TAG, "goCompany");
        startMapActivity(INaviConstant.OpenIntentPage.GO_COMPANY);
        SplitScreenManager.getInstance().switchNaviToFullScreen();
    };

    /***
     * 搜索
     */
    public Action doSearch = () -> {
        Logger.i(TAG, "doSearch");
        startMapActivity(INaviConstant.OpenIntentPage.SEARCH_PAGE);
        SplitScreenManager.getInstance().switchNaviToFullScreen();
    };

    /***
     * 充电站/加油站
     */
    public Action chargeOrGas = () -> {
        Logger.i(TAG, "chargeOrGas");
        startMapActivity(INaviConstant.OpenIntentPage.SEARCH_RESULT_PAGE);
        SplitScreenManager.getInstance().switchNaviToFullScreen();
    };

    /***
     * 切到智能驾驶，交换位置和大小
     */
    public Action switchAiDriver = () -> {
        Logger.i(TAG, "switchAiDriver");
        SplitScreenManager.getInstance().switchPositionAndSize();
    };

    /***
     * 退出导航
     *
     */
    public Action stopNavi = () -> {
        Logger.i(TAG, "stopNavi");
        mModel.stopNavi();
        SplitScreenManager.getInstance().switchSRToFullScreen();
    };

    /***
     * 看全览
     *
     */
    public Action showOrClosePreview = () -> {
        mModel.showOrClosePreview();
    };

    public void startPreviewSchedule() {
        try {
            stopPreviewSchedule();
            mIsOnShowPreview.set(true);
            previewScheduledFuture = ThreadManager.getInstance().asyncDelayWithResult(() -> {
                mModel.closePreview();
                mIsOnShowPreview.set(false);
            }, TOTAL_TIME);
        } catch (Exception e) {
            Logger.e(TAG, "startPreviewSchedule failed:" + e.getMessage());
        }
    }

    public void stopPreviewSchedule() {
        try {
            if (!ConvertUtils.isNull(previewScheduledFuture) && !previewScheduledFuture.isDone()) {
                boolean cancelResult = previewScheduledFuture.cancel(true);
                Logger.i(TAG, "stopPreviewSchedule:" + cancelResult);
            } else {
                Logger.i(TAG, "stopPreviewSchedule not need do, preiveScheduledFuture is null or has completed!");
            }
        } catch (Exception e) {
            Logger.e(TAG, "stopPreviewSchedule failed:" + e.getMessage());
        } finally {
            previewScheduledFuture = null;
            mIsOnShowPreview.set(false);
        }
    }

    /***
     * 静音或者取消静音
     *
     */
    public Action muteOrUnMute = () -> {
        Logger.i(TAG, "muteOrUnMute");
        mNaviBroadIsMute.set(!mNaviBroadIsMute.get());
        mModel.setCruiseVoice(Boolean.FALSE.equals(muteVisibility.get()));
    };

    public void setCruiseMuteOrUnMute(boolean isOpen) {
        muteVisibility.set(isOpen);
        mView.cruiseMuteOrUnMute(isOpen);
    }

    // 更新巡航态下的车道信息
    public void updateCruiseLanInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {
        Logger.d(TAG,"updateCruiseLanInfo");
        mView.updateCruiseLanInfo(isShowLane, laneInfoEntity);
        mCruiseLanesVisibility.set(!ConvertUtils.isNull(laneInfoEntity) && !ConvertUtils.isEmpty(laneInfoEntity.getBackLane()));
    }

    /***
     * 判断巡航UI是否正在显示
     * @return
     */
    public boolean isCruiseUiVisible() {
        return mCruiseLanesVisibility.get();
    }

    /***
     * 继续导航
     *
     */
    public Action naviContinue = () -> {
        Logger.i(TAG, "naviContinue");
        mModel.openOrCloseImmersive(true);
        // 如果处于全览要退出全览
        mModel.exitPreviewIfNeeded();
    };

    public void onNaviInfo(NaviEtaInfo naviETAInfo) {
        if (ConvertUtils.isNull(mView)) return;
        if (!ConvertUtils.isNull(naviETAInfo)) {
            mView.onNaviInfo(naviETAInfo);
        }
    }

    public void onLaneInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {
        if (ConvertUtils.isNull(mView)) return;
        mView.onLaneInfo(isShowLane, laneInfoEntity);
        mLanesVisibility.set(isShowLane);
    }

    /**
     * 更新TMC灯光条（路况信息）
     *
     * @param naviTmcInfo navi tmc info
     */
    public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo, final boolean isShow) {
        mView.onUpdateTMCLightBar(naviTmcInfo, isShow);
    }

    /***
     * 启动Navi_App
     */
    public void startMapActivity(int pageCode) {
        if (FloatViewManager.getInstance().isNaviDeskBg()) {
            ExportIntentParam.setIntentPage(pageCode);
        } else {
            MapActivity activity = (MapActivity) mView.getActivity();
            assert activity != null;
            activity.savePageCode(pageCode);
        }
    }

    private void startImmersiveSchedule() {
        try {
            stopImmersiveSchedule();
            immersiveScheduledFuture = ThreadManager.getInstance().asyncDelayWithResult(() -> {
                if (mModel.isOnTouch()) {
                    mModel.openOrCloseImmersive(true);
                }
            }, TOTAL_TIME);
        } catch (Exception e) {
            Logger.i(TAG, "startImmersiveSchedule failed:" + e.getMessage());
        }
    }

    private void stopImmersiveSchedule() {
        try {
            if (!ConvertUtils.isNull(immersiveScheduledFuture) && !immersiveScheduledFuture.isDone()) {
                final boolean cancelResult = immersiveScheduledFuture.cancel(true);
                Logger.i(TAG, "stopImmersiveSchedule:" + cancelResult);
            } else {
                Logger.i(TAG, "stopImmersiveSchedule not need do, immersiveScheduledFuture is null or has completed!");
            }
        } catch (Exception e) {
            Logger.i(TAG, "stopImmersiveSchedule failed:" + e.getMessage());
        } finally {
            immersiveScheduledFuture = null;
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        stopPreviewSchedule();
        stopImmersiveSchedule();
    }

    public Rect getPreviewRect() {
        return mView.getPreviewRect();
    }

    /***
     * 沉浸式状态改变后更新UI状态
     */
    public void updateUiStateAfterImmersiveChanged(ImersiveStatus currentImersiveStatus) {
        mIsOnTouch.set(currentImersiveStatus == ImersiveStatus.TOUCH && mModel.isOnNavigating());
        if (currentImersiveStatus == ImersiveStatus.TOUCH) {
            startImmersiveSchedule();
        } else {
            stopImmersiveSchedule();
        }
    }

    /***
     * 导航状态改变后更新UI状态
     */
    public void updateUiStateAfterNaviStatusChanged(String naviStatus) {
        Logger.d(TAG, "updateUiStateAfterNaviStatusChanged:" + naviStatus);
        mNaviActionBarVisibility.set(naviStatus.equals(NaviStatus.NaviStatusType.NAVING));
        mTopNaviBarVisibility.set(!naviStatus.equals(NaviStatus.NaviStatusType.CRUISE));

        mCruisebg.set(naviStatus.equals(NaviStatus.NaviStatusType.CRUISE));
        mSlStationVisibility.set(!naviStatus.equals(NaviStatus.NaviStatusType.CRUISE));
        mTopNaviBarVisibility.set(!naviStatus.equals(NaviStatus.NaviStatusType.CRUISE));
    }

    public void updateCruiseCameraInfo(CruiseInfoEntity cruiseInfoEntity) {
        Logger.d(TAG,"updateCruiseCameraInfo = " + cruiseInfoEntity);
        mView.updateCruiseCameraInfo(cruiseInfoEntity);
        if (TextUtils.equals(mModel.getCurrentNaviStatus(), NaviStatus.NaviStatusType.CRUISE)){
            cruiseUiVisibility.set(cruiseInfoEntity != null);
        }
    }

    /***
     * 桌面背景改变后更新UI状态
     */
    public void updateUiStateAfterDeskBackgroundChanged(FloatViewManager.DesktopMode desktopMode) {

    }
}
