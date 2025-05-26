package com.fy.navi.hmi.splitscreen;

import android.app.Application;
import android.graphics.Rect;

import androidx.annotation.NonNull;
import androidx.databinding.ObservableField;

import com.android.utils.log.Logger;
import com.fy.navi.hmi.R;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
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
    @Override
    public void onCreate() {
        super.onCreate();
        mTopNaviBarVisibility.set(mModel.isOnNavigating() ? false : true);
        mNaviActionBarVisibility.set((mModel.isOnNavigating() && mModel.isOnImmersive()) ? true : false);
        mNaviBroadIsMute.set(mModel.isMute());
        mNaviVoicePic.set(mModel.isMute() ? com.fy.navi.scene.R.drawable.img_mute_broadcast_black_58 : com.fy.navi.scene.R.drawable.img_navi_broadcast);
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
     * TODO ·点击【家/公司/搜索】icon，打开2/3屏导航APP，后续条件判断与动作执行同导航APP内点击【家/公司/搜索】icon.
     */
    public Action goHome = () -> {
        Logger.i(TAG, "goHome");

    };

    /***
     * 去公司
     * TODO ·点击【家/公司/搜索】icon，打开2/3屏导航APP，后续条件判断与动作执行同导航APP内点击【家/公司/搜索】icon.
     */
    public Action goCompany = () -> {
        Logger.i(TAG, "goCompany");

    };

    /***
     * 搜索
     * TODO ·点击【家/公司/搜索】icon，打开2/3屏导航APP，后续条件判断与动作执行同导航APP内点击【家/公司/搜索】icon.
     */
    public Action doSearch = () -> {
        Logger.i(TAG, "doSearch");

    };

    /***
     * 充电站/加油站
     * TODO ·点击【充电站/加油站】icon，打开全屏2/3屏导航APP，展示充电站/加油站列表。
     */
    public Action chargeOrGas = () -> {
        Logger.i(TAG, "chargeOrGas");
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
        Logger.i(TAG, "showOrClosePreview");
        if (mModel.isOnImmersive()) {
            mModel.showPreview();
        } else {
            mModel.naviContinue();
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
        mModel.naviContinue();
    };

    public void onNaviInfo(NaviEtaInfo naviETAInfo) {
        mTopNaviBarVisibility.set(false);
        mNaviActionBarVisibility.set(true);
        mView.onNaviInfo(naviETAInfo);
    }

    public void onNaviStop() {
        mTopNaviBarVisibility.set(true);
        mNaviActionBarVisibility.set(false);
    }

    public void onLaneInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {
        mView.onLaneInfo(isShowLane, laneInfoEntity);
    }

    /**
     * 更新TMC灯光条（路况信息）
     *
     * @param naviTmcInfo navi tmc info
     */
    public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo, final boolean isShow) {
        mView.onUpdateTMCLightBar(naviTmcInfo, isShow);
    }

    public void onCrossImageInfo(boolean isShowImage, CrossImageEntity naviImageInfo) {
        mCrossImageVisibility.set(isShowImage);
    }

    public void setRoadCrossRect(Rect rect) {
        Logger.i(TAG, "rect:" + rect.toShortString());
        mModel.setRoadCrossRect(rect);
    }
}
