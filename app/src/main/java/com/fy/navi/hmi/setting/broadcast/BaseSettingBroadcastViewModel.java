package com.fy.navi.hmi.setting.broadcast;


import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.bean.BuryProperty;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.setting.broadcast.voice.SettingVoiceBroadcastFragment;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.logicpaket.cruise.CruisePackage;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

import java.util.List;

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

    public List<String> setVoiceIcons() {
        return mModel.getRecommendVoiceList();
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

        sendBuryPointForSettingBroadcast(BuryConstant.BroadcastOption.VOICE_STYLE, BuryConstant.VoiceStyle.DETAILED);
    };
    public Action mSwitchNaviBroadcastConciseClick = () -> {
        NaviPackage.getInstance().updateBroadcastParam(1, true);

        sendBuryPointForSettingBroadcast(BuryConstant.BroadcastOption.VOICE_STYLE, BuryConstant.VoiceStyle.CONCISE);
    };

    public Action mSwitchNaviBroadcastSimpleClick = () -> {
        NaviPackage.getInstance().updateBroadcastParam(3, true);

        sendBuryPointForSettingBroadcast(BuryConstant.BroadcastOption.VOICE_STYLE, BuryConstant.VoiceStyle.SIMPLE);
    };

    public Action mSwitchCruiseBroadcastClick = () -> {
        final boolean value = Boolean.FALSE.equals(mIsCruiseBroadcast.getValue());
        if (value) {
            mIsCruiseBroadcast.setValue(true);
            CruisePackage.getInstance().setConfigKeyDriveWarn(Boolean.TRUE.equals(mIsCruiseBroadcastRoadCondition.getValue()));
            CruisePackage.getInstance().setConfigKeySafeBroadcast(Boolean.TRUE.equals(mIsCruiseBroadcastCamera.getValue()));
            CruisePackage.getInstance().setConfigKeyRoadWarn(Boolean.TRUE.equals(mIsCruiseBroadcastSafe.getValue()));
            mModel.setConfigKeyRoadWarn(Boolean.TRUE.equals(mIsCruiseBroadcastRoadCondition.getValue()));
            mModel.setConfigKeySafeBroadcast(Boolean.TRUE.equals(mIsCruiseBroadcastCamera.getValue()));
            mModel.setConfigKeyDriveWarn(Boolean.TRUE.equals(mIsCruiseBroadcastSafe.getValue()));
            mView.updateCruiseBroadcastEnable(true);
            mModel.setCruiseBroadcastOpen(true);
        } else {
            mIsCruiseBroadcast.setValue(false);
            mIsCruiseBroadcastRoadCondition.setValue(false);
            mIsCruiseBroadcastCamera.setValue(false);
            mIsCruiseBroadcastSafe.setValue(false);
            CruisePackage.getInstance().setConfigKeyDriveWarn(false);
            CruisePackage.getInstance().setConfigKeySafeBroadcast(false);
            CruisePackage.getInstance().setConfigKeyRoadWarn(false);
            mModel.setConfigKeyRoadWarn(false);
            mModel.setConfigKeySafeBroadcast(false);
            mModel.setConfigKeyDriveWarn(false);
            mView.updateCruiseBroadcastEnable(false);
            mModel.setCruiseBroadcastOpen(false);

            sendBuryPointForSettingBroadcast(BuryConstant.BroadcastOption.CRUISE_VOICE, BuryConstant.CruiseVoice.CLOSE);
        }
    };

    public Action mSwitchCruiseBroadcastRoadConditionClick = () -> {
        final boolean value = Boolean.FALSE.equals(mIsCruiseBroadcastRoadCondition.getValue());
        mIsCruiseBroadcastRoadCondition.setValue(value);
        mModel.setConfigKeyRoadWarn(value);

        sendBuryPointForSettingBroadcast(BuryConstant.BroadcastOption.CRUISE_VOICE, BuryConstant.CruiseVoice.ROAD_CONDITION);
    };

    public Action mSwitchCruiseBroadcastCameraClick = () -> {
        final boolean value = Boolean.FALSE.equals(mIsCruiseBroadcastCamera.getValue());
        mIsCruiseBroadcastCamera.setValue(value);
        mModel.setConfigKeySafeBroadcast(value);

        sendBuryPointForSettingBroadcast(BuryConstant.BroadcastOption.CRUISE_VOICE, BuryConstant.CruiseVoice.CAMERA);

    };

    public Action mSwitchCruiseBroadcastSafeClick = () -> {
        final boolean value = Boolean.FALSE.equals(mIsCruiseBroadcastSafe.getValue());
        mIsCruiseBroadcastSafe.setValue(value);
        mModel.setConfigKeyDriveWarn(value);

        sendBuryPointForSettingBroadcast(BuryConstant.BroadcastOption.CRUISE_VOICE, BuryConstant.CruiseVoice.SAFE);
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

    public void setCurrentVoice(){
        mView.setCurrentVoice();
    }


    public Action mOpenBroadcastVoiceListPage = () -> {
        if(!Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())){
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.setting_broadcast_voice_no_net));
            return;
        }
        addFragment(new SettingVoiceBroadcastFragment(), null);
    };

    @HookMethod
    private void sendBuryPointForSettingBroadcast(String option, String value){
        String eventName = switch (option){
            case BuryConstant.BroadcastOption.VOICE_PACKAGE ->
                    BuryConstant.EventName.AMAP_SETTING_VOICEPACKAGE;
            case BuryConstant.BroadcastOption.VOICE_STYLE ->
                    BuryConstant.EventName.AMAP_SETTING_VOICESTYLE;
            case BuryConstant.BroadcastOption.CRUISE_VOICE ->
                    BuryConstant.EventName.AMAP_SETTING_CRUISEVOICE;
            default -> BuryConstant.EventName.AMAP_UNKNOWN;
        };
        BuryPointController.getInstance().setEventName(eventName);
        BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_SETTING_CONTENT, value)
                .build();
        BuryPointController.getInstance().setBuryProps(buryProperty);
    }
}
