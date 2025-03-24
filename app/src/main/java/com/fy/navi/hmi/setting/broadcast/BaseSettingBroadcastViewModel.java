package com.fy.navi.hmi.setting.broadcast;


import android.app.Application;
import com.android.utils.log.Logger;
import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.fy.navi.hmi.setting.broadcast.voice.SettingVoiceBroadcastFragment;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.logicpaket.cruise.CruisePackage;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseSettingBroadcastViewModel extends BaseViewModel<SettingBroadcastFragment, SettingBroadcastModel> {

    public final MutableLiveData<Boolean> mIsNaviBroadcastDetail = new MutableLiveData<>(true);
    public final MutableLiveData<Boolean> mIsNaviBroadcastConcise = new MutableLiveData<>(false);
    public final MutableLiveData<Boolean> mIsNaviBroadcastSimple = new MutableLiveData<>(false);
    public final MutableLiveData<Boolean> mIsCruiseBroadcast = new MutableLiveData<>(true);
    public final MutableLiveData<Boolean> mIsCruiseBroadcastRoadCondition = new MutableLiveData<>(true);
    public final MutableLiveData<Boolean> mIsCruiseBroadcastCamera = new MutableLiveData<>(true);
    public final MutableLiveData<Boolean> mIsCruiseBroadcastSafe = new MutableLiveData<>(true);
    private static final String TAG = BaseSettingBroadcastViewModel.class.getName();

    public BaseSettingBroadcastViewModel(final @NonNull Application application) {
        super(application);
    }

    @Override
    protected SettingBroadcastModel initModel() {
        return new SettingBroadcastModel();
    }

    @Override
    public void onCreate() {
        super.onCreate();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    /**
     * 初始化
     */
    public void initView() {
        mModel.initView();
    }

    /**
     * 设置巡航播报选项
     * @param key 设置项
     * @param isTrue 是否选中
     */
    public void dualChoiceControl(final String key, final boolean isTrue) {
        switch (key) {
            case SettingController.KEY_SETTING_CRUISE_BROADCAST:
                mIsCruiseBroadcast.setValue(isTrue);
                mView.updateCruiseBroadcastEnable(isTrue);
                break;
            case SettingController.KEY_SETTING_BROADCAST_ROAD_CONDITIONS:
                mIsCruiseBroadcastRoadCondition.setValue(isTrue);
                break;
            case SettingController.KEY_SETTING_BROADCAST_ELECTRONIC_EYE:
                mIsCruiseBroadcastCamera.setValue(isTrue);
                break;
            case SettingController.KEY_SETTING_BROADCAST_SAFE_REMINDER:
                mIsCruiseBroadcastSafe.setValue(isTrue);
                break;
            default:
                Logger.d(TAG," Invalid value, key = " + key);
                break;
        }
    }

    public Action mSwitchNaviBroadcastDetailClick = () -> {
        NaviPackage.getInstance().updateBroadcastParam(2, true);
    };
    public Action mSwitchNaviBroadcastConciseClick = () -> {
        NaviPackage.getInstance().updateBroadcastParam(1, true);
    };

    public Action mSwitchNaviBroadcastSimpleClick = () -> {
        NaviPackage.getInstance().updateBroadcastParam(3, true);
    };

    public Action mSwitchCruiseBroadcastClick = () -> {
        final boolean value = Boolean.FALSE.equals(mIsCruiseBroadcast.getValue());
        if (value) {
            mIsCruiseBroadcast.setValue(true);
            mModel.setCruiseBroadcastOpen(true);
            CruisePackage.getInstance().setConfigKeyDriveWarn(Boolean.TRUE.equals(mIsCruiseBroadcastRoadCondition.getValue()));
            CruisePackage.getInstance().setConfigKeySafeBroadcast(Boolean.TRUE.equals(mIsCruiseBroadcastCamera.getValue()));
            CruisePackage.getInstance().setConfigKeyRoadWarn(Boolean.TRUE.equals(mIsCruiseBroadcastSafe.getValue()));
            mModel.setConfigKeyRoadWarn(Boolean.TRUE.equals(mIsCruiseBroadcastRoadCondition.getValue()));
            mModel.setConfigKeySafeBroadcast(Boolean.TRUE.equals(mIsCruiseBroadcastCamera.getValue()));
            mModel.setConfigKeyDriveWarn(Boolean.TRUE.equals(mIsCruiseBroadcastSafe.getValue()));
            mView.updateCruiseBroadcastEnable(true);
        } else {
            mIsCruiseBroadcast.setValue(false);
            mModel.setCruiseBroadcastOpen(false);
            CruisePackage.getInstance().setConfigKeyDriveWarn(false);
            CruisePackage.getInstance().setConfigKeySafeBroadcast(false);
            CruisePackage.getInstance().setConfigKeyRoadWarn(false);
            mModel.setConfigKeyRoadWarn(false);
            mModel.setConfigKeySafeBroadcast(false);
            mModel.setConfigKeyDriveWarn(false);
            mView.updateCruiseBroadcastEnable(false);
        }
    };

    public Action mSwitchCruiseBroadcastRoadConditionClick = () -> {
        final boolean value = Boolean.FALSE.equals(mIsCruiseBroadcastRoadCondition.getValue());
        mIsCruiseBroadcastRoadCondition.setValue(value);
        mModel.setConfigKeyRoadWarn(value);
    };

    public Action mSwitchCruiseBroadcastCameraClick = () -> {
        final boolean value = Boolean.FALSE.equals(mIsCruiseBroadcastCamera.getValue());
        mIsCruiseBroadcastCamera.setValue(value);
        mModel.setConfigKeySafeBroadcast(value);

    };

    public Action mSwitchCruiseBroadcastSafeClick = () -> {
        final boolean value = Boolean.FALSE.equals(mIsCruiseBroadcastSafe.getValue());
        mIsCruiseBroadcastSafe.setValue(value);
        mModel.setConfigKeyDriveWarn(value);
    };

    /**
     * 导航播报回调
     * @param isBroadcastDetail 是否播报详细信息
     * @param isBroadcastConcise 是否播报简略信息
     * @param isBroadcastSimple 是否播报简单信息
     */
    public void onNaviBroadcastChange(final boolean isBroadcastDetail, final boolean isBroadcastConcise,
                                      final boolean isBroadcastSimple) {
        mIsNaviBroadcastDetail.setValue(isBroadcastDetail);
        mIsNaviBroadcastConcise.setValue(isBroadcastConcise);
        mIsNaviBroadcastSimple.setValue(isBroadcastSimple);
    }


    public Action mOpenBroadcastVoiceListPage = () -> {
        addFragment(new SettingVoiceBroadcastFragment(), null);
    };
}
