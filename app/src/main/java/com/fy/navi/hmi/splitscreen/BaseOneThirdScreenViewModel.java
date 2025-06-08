package com.fy.navi.hmi.splitscreen;

import android.app.ActivityOptions;
import android.app.Application;
import android.content.Intent;
import android.graphics.Rect;
import android.os.Bundle;
import android.os.CountDownTimer;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.databinding.ObservableField;

import com.android.utils.log.Logger;
import com.fy.navi.hmi.map.MapActivity;
import com.fy.navi.mapservice.bean.INaviConstant;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.service.AppCache;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.ThemeType;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviManeuverInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navi.NextManeuverEntity;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.calibration.PowerType;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/5/21
 * Description: [在这里描述文件功能]
 */
public class BaseOneThirdScreenViewModel extends BaseViewModel<OneThirdScreenMapActivity, OneThirdScreenModel> {
    public BaseOneThirdScreenViewModel(@NonNull Application application) {
        super(application);
    }

    private static final String TAG = "BaseOneThirdScreenViewModel";
    public ObservableField<Boolean> mTopNaviBarVisibility = new ObservableField<>(false);
    public ObservableField<Boolean> mNaviActionBarVisibility = new ObservableField<>(false);
    public ObservableField<Boolean> mNaviBroadIsMute = new ObservableField<>(false);
    public ObservableField<Integer> mNaviVoicePic = new ObservableField<>(com.fy.navi.scene.R.drawable.img_mute_broadcast_black_58);
    public ObservableField<Boolean> mCrossImageVisibility = new ObservableField<>(false);
    public ObservableField<Boolean> mIsGasCar = new ObservableField<>(false);// true代表油车
    public ObservableField<Boolean> mIsOnTouch = new ObservableField<>(false);
    public ObservableField<Boolean> mIsOnShowPreview = new ObservableField<>(false);
    public ObservableField<Boolean> mLanesVisibility = new ObservableField<>(false);
    public ObservableField<Boolean> mNextManeuverVisible = new ObservableField<>(false);
    private boolean mIsSetCrossRect = false; // 是否设置过路口大图显示区域
    // 触摸态后开启倒计时，8秒后进入沉浸态
    private CountDownTimer immersiveCountDownTimer = new CountDownTimer(8000, 1000) {
        @Override
        public void onTick(long millisUntilFinished) {

        }

        @Override
        public void onFinish() {
            if (!mModel.isOnImmersive()) {
                mModel.openOrCloseImmersive(true);
            }
        }
    };
    // 全览后开启倒计时，8秒退出全览
    private CountDownTimer preiveCountDownTimer = new CountDownTimer(8000, 1000) {
        @Override
        public void onTick(long millisUntilFinished) {

        }

        @Override
        public void onFinish() {
            Logger.i(TAG, "exit preview!", "mIsOnShowPreview:" + mIsOnShowPreview.get());
            if (mIsOnShowPreview.get()) {
                mModel.closePreview();
                mIsOnShowPreview.set(false);
            }
        }
    };

    @Override
    public void onCreate() {
        super.onCreate();
        mTopNaviBarVisibility.set(mModel.isOnNavigating() ? false : true);
        mNaviActionBarVisibility.set((mModel.isOnNavigating() && mModel.isOnImmersive()) ? true : false);
        mNaviBroadIsMute.set(mModel.isMute());
        mNaviVoicePic.set(mModel.isMute() ? com.fy.navi.scene.R.drawable.img_mute_broadcast_black_58 : com.fy.navi.scene.R.drawable.img_navi_broadcast);
        mIsGasCar.set(mModel.getPowerType() == PowerType.E_VEHICLE_ENERGY_FUEL);
        mIsOnTouch.set(!mModel.isOnImmersive() && mModel.isOnNavigating());
        if (mModel.isOnNavigating()) {
            onNaviInfo(mModel.getCurrentNaviEtaInfo());
            onCrossImageInfo(mModel.getCrossIsShowing());
            mModel.showOrHideCross(mModel.getLastCrossEntity());
        }
    }

    @Override
    protected OneThirdScreenModel initModel() {
        return new OneThirdScreenModel();
    }

    public IBaseScreenMapView getMapView() {
        return mView.getMapView();
    }

    public void loadMapView() {
        mModel.loadMapView();
    }

    public int[] getLogoPosition() {
        return mView.getCarSelfPosition();
    }


    /*-------------Action--------------*/
    public Action testCloseActivity = () -> {
        mView.finish();
    };

    /***
     * 回家
     */
    public Action goHome = () -> {
        Logger.i(TAG, "goHome");
        startMapActivity(INaviConstant.OpenIntentPage.GO_HOME, null);
    };

    /***
     * 去公司
     */
    public Action goCompany = () -> {
        Logger.i(TAG, "goCompany");
        startMapActivity(INaviConstant.OpenIntentPage.GO_COMPANY, null);
    };

    /***
     * 搜索
     */
    public Action doSearch = () -> {
        Logger.i(TAG, "doSearch");
        startMapActivity(INaviConstant.OpenIntentPage.SEARCH_PAGE, null);
    };

    /***
     * 充电站/加油站
     */
    public Action chargeOrGas = () -> {
        Logger.i(TAG, "chargeOrGas");
        startMapActivity(INaviConstant.OpenIntentPage.SEARCH_RESULT_PAGE, null);
    };

    /***
     * 切到智能驾驶
     * TODO
     */
    public Action switchAiDriver = () -> {
        Logger.i(TAG, "switchAiDriver");
    };

    /***
     * 退出导航
     *
     */
    public Action stopNavi = () -> {
        Logger.i(TAG, "stopNavi");
        mModel.stopNavi();
    };

    /***
     * 看全览
     *
     */
    public Action showOrClosePreview = () -> {
        Logger.i(TAG, "showOrClosePreview:" + mIsOnShowPreview.get());
        preiveCountDownTimer.cancel();
        if (mIsOnShowPreview.get()) {
            mModel.closePreview();
            mIsOnShowPreview.set(false);
        } else {
            mModel.showPreview();
            mIsOnShowPreview.set(true);
            preiveCountDownTimer.start();
        }
    };

    /***
     * 静音或者取消静音
     *
     */
    public Action muteOrUnMute = () -> {
        Logger.i(TAG, "muteOrUnMute");
        mModel.muteOrUnMute();
        mNaviBroadIsMute.set(mModel.isMute());
        mNaviVoicePic.set(mModel.isMute() ? com.fy.navi.scene.R.drawable.img_mute_broadcast_black_58 : com.fy.navi.scene.R.drawable.img_navi_broadcast);
    };

    /***
     * 继续导航
     *
     */
    public Action naviContinue = () -> {
        Logger.i(TAG, "naviContinue");
        mModel.openOrCloseImmersive(true);
    };

    public void onNaviInfo(NaviEtaInfo naviETAInfo) {
        mTopNaviBarVisibility.set(false);
        mNaviActionBarVisibility.set(true);
        mView.onNaviInfo(naviETAInfo);
        if (!mIsSetCrossRect) {
            mView.setCrossRect();
        }
    }

    public void onNaviStop() {
        mTopNaviBarVisibility.set(true);
        mNaviActionBarVisibility.set(false);
        mIsOnTouch.set(false);
    }

    public void onLaneInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {
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
    public void startMapActivity(int pageCode, @Nullable PoiInfoEntity poiInfo) {
        Logger.i(TAG, "startMapActivity:" + pageCode);
        Intent intent = new Intent(AppCache.getInstance().getMContext(), MapActivity.class);
        ActivityOptions options = ActivityOptions.makeBasic();
        options.setLaunchDisplayId(0);
        Bundle bundle = new Bundle();
        bundle.putInt(INaviConstant.PAGE_EXTRA, pageCode);
        if (poiInfo != null) {
            bundle.putParcelable(INaviConstant.POI_INFO_EXTRA, poiInfo);
        }
        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        intent.putExtras(bundle);
        AppCache.getInstance().getMContext().startActivity(intent, options.toBundle());
    }

    public void onImmersiveStatusChange(ImersiveStatus lastImersiveStatus) {
        immersiveCountDownTimer.cancel();
        mIsOnTouch.set(lastImersiveStatus == ImersiveStatus.TOUCH && mModel.isOnNavigating());
        mIsOnShowPreview.set(lastImersiveStatus == ImersiveStatus.TOUCH && mModel.isOnNavigating());
        if (!mModel.isOnImmersive()) {
            immersiveCountDownTimer.start();
        }
    }

    public Rect getPreviewRect() {
        return mView.getPreviewRect();
    }


    public void onCrossImageInfo(boolean isShow) {
        mCrossImageVisibility.set(isShow);
    }

    public void setCrossRect(Rect rect) {
        mIsSetCrossRect = true;
        mModel.setCrossRect(rect);
    }

    public void onManeuverInfo(NaviManeuverInfo info) {
        mView.onManeuverInfo(info);
    }

    public void showNextManeuver(boolean isSuccess, NextManeuverEntity mNextManeuverEntity) {
        if (isSuccess) {
            mNextManeuverVisible.set(mView.onNextManeuverInfo(mNextManeuverEntity));
        } else {
            mNextManeuverVisible.set(false);
        }
    }

    public boolean isOnNaviGating() {
        return mModel.isOnNavigating();
    }

    public void onConfigurationChanged(ThemeType type) {
        mModel.onConfigurationChanged(type);
    }
}
