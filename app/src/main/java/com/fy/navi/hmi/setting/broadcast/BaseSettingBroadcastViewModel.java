package com.fy.navi.hmi.setting.broadcast;


import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.fy.navi.hmi.setting.broadcast.voice.SettingVoiceBroadcastFragment;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.logicpaket.cruise.CruisePackage;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseSettingBroadcastViewModel extends BaseViewModel<SettingBroadcastFragment, SettingBroadcastModel> {

    public final MutableLiveData<Boolean> isNaviBroadcastDetail = new MutableLiveData<>(true);
    public final MutableLiveData<Boolean> isNaviBroadcastConcise = new MutableLiveData<>(false);
    public final MutableLiveData<Boolean> isNaviBroadcastSimple = new MutableLiveData<>(false);
    public final MutableLiveData<Boolean> isCruiseBroadcast = new MutableLiveData<>(true);
    public final MutableLiveData<Boolean> isCruiseBroadcastRoadCondition = new MutableLiveData<>(true);
    public final MutableLiveData<Boolean> isCruiseBroadcastCamera = new MutableLiveData<>(true);
    public final MutableLiveData<Boolean> isCruiseBroadcastSafe = new MutableLiveData<>(true);

    public BaseSettingBroadcastViewModel(@NonNull Application application) {
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
    public void initView() {
        mModel.initView();
    }
    public void dualChoiceControl(String key, boolean isTrue) {
        switch (key) {
            case SettingController.KEY_SETTING_CRUISE_BROADCAST:
                isCruiseBroadcast.setValue(isTrue);
                mView.updateCruiseBroadcastEnable(isTrue);
                break;
            case SettingController.KEY_SETTING_BROADCAST_ROAD_CONDITIONS:
                isCruiseBroadcastRoadCondition.setValue(isTrue);
                break;
            case SettingController.KEY_SETTING_BROADCAST_ELECTRONIC_EYE:
                isCruiseBroadcastCamera.setValue(isTrue);
                break;
            case SettingController.KEY_SETTING_BROADCAST_SAFE_REMINDER:
                isCruiseBroadcastSafe.setValue(isTrue);
                break;
        }
    }

    public Action switchNaviBroadcastDetailClick = () -> {
        NaviPackage.getInstance().updateBroadcastParam(2, true);
    };
    public Action switchNaviBroadcastConciseClick = () -> {
        NaviPackage.getInstance().updateBroadcastParam(1, true);
    };

    public Action switchNaviBroadcastSimpleClick = () -> {
        NaviPackage.getInstance().updateBroadcastParam(3, true);
    };

    public Action switchCruiseBroadcastClick = () -> {
        boolean value = Boolean.FALSE.equals(isCruiseBroadcast.getValue());
        if (value) {
            isCruiseBroadcast.setValue(true);
            mModel.setCruiseBroadcastOpen(true);
            CruisePackage.getInstance().setConfigKeyDriveWarn(Boolean.TRUE.equals(isCruiseBroadcastRoadCondition.getValue()));
            CruisePackage.getInstance().setConfigKeySafeBroadcast(Boolean.TRUE.equals(isCruiseBroadcastCamera.getValue()));
            CruisePackage.getInstance().setConfigKeyRoadWarn(Boolean.TRUE.equals(isCruiseBroadcastSafe.getValue()));
            mModel.setConfigKeyRoadWarn(Boolean.TRUE.equals(isCruiseBroadcastRoadCondition.getValue()));
            mModel.setConfigKeySafeBroadcast(Boolean.TRUE.equals(isCruiseBroadcastCamera.getValue()));
            mModel.setConfigKeyDriveWarn(Boolean.TRUE.equals(isCruiseBroadcastSafe.getValue()));
            mView.updateCruiseBroadcastEnable(true);
        } else {
            isCruiseBroadcast.setValue(false);
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

    public Action switchCruiseBroadcastRoadConditionClick = () -> {
        boolean value = Boolean.FALSE.equals(isCruiseBroadcastRoadCondition.getValue());
        isCruiseBroadcastRoadCondition.setValue(value);
        mModel.setConfigKeyRoadWarn(value);
    };

    public Action switchCruiseBroadcastCameraClick = () -> {
        boolean value = Boolean.FALSE.equals(isCruiseBroadcastCamera.getValue());
        isCruiseBroadcastCamera.setValue(value);
        mModel.setConfigKeySafeBroadcast(value);

    };

    public Action switchCruiseBroadcastSafeClick = () -> {
        boolean value = Boolean.FALSE.equals(isCruiseBroadcastSafe.getValue());
        isCruiseBroadcastSafe.setValue(value);
        mModel.setConfigKeyDriveWarn(value);
    };

    public void onNaviBroadcastChange(boolean isBroadcastDetail, boolean isBroadcastConcise, boolean isBroadcastSimple) {
        isNaviBroadcastDetail.setValue(isBroadcastDetail);
        isNaviBroadcastConcise.setValue(isBroadcastConcise);
        isNaviBroadcastSimple.setValue(isBroadcastSimple);
    }


    public Action openBroadcastVoiceListPage = () -> {
        addFragment(new SettingVoiceBroadcastFragment(), null);
    };
}
