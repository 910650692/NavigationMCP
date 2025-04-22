package com.fy.navi.hmi.setting.others.privacy;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.bean.BuryProperty;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

import java.util.Objects;

public class BaseSettingOthersPrivacyViewModel extends BaseViewModel<SettingOthersPrivacyFragment, SettingOthersPrivacyModel> {


    public MutableLiveData<Boolean> mIsOneYearPrivacy = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> mIsNeverPrivacy = new MutableLiveData<>(true);
    public MutableLiveData<String> mEndDate = new MutableLiveData<>("");


    public BaseSettingOthersPrivacyViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected SettingOthersPrivacyModel initModel() {
        return new SettingOthersPrivacyModel();
    }

    /**
     * 设置隐私设置
     * @param key 设置key
     * @param isTrue true表示一年，false永不
     */
    public void dualChoiceControl(final String key, final boolean isTrue) {
        if (key.equals(SettingController.KEY_SETTING_PRIVACY_STATUS)) {
            mIsNeverPrivacy.setValue(!isTrue);
            mIsOneYearPrivacy.setValue(isTrue);
            mView.setEndDate(mModel.getEndDate(), isTrue);
        }
    }

    /**
     * 初始化各设置项状态值
     */
    public void initView() {
        mModel.initView();
    }

    /**
     * 设置隐私到期时间
     * @param endDateTime 时间
     */
    public void setEndDate(final String endDateTime) {
        mEndDate.setValue(endDateTime);
    }

    public Action mFinishPrivacy = () -> {
        closeFragment(true);
    };

    public Action mOneYearPrivacy = () -> {
        mView.setEndDate(mModel.getFormattedDate(), true);
        mModel.setPrivacyStatus(true);
        mModel.setEndDate(mModel.getFormattedDate());
        SettingUpdateObservable.getInstance().notifySettingChanged(SettingController.KEY_SETTING_PRIVACY_STATUS, true);
        sendBuryPointForSetPrivacyTime(BuryConstant.Number.ONE);
    };

    public Action mNeverPrivacy = () -> {
        mView.setEndDate(mModel.getFormattedDate(), false);
        mModel.setPrivacyStatus(false);
        mModel.setEndDate("");
        SettingUpdateObservable.getInstance().notifySettingChanged(SettingController.KEY_SETTING_PRIVACY_STATUS, false);
        sendBuryPointForSetPrivacyTime(BuryConstant.Number.SECOND);
        //非导航状态直接退出应用，导航状态则等导航结束再退出
        if (!Objects.equals(NaviStatusPackage.getInstance().getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING)) {
            System.exit(0);
        }
    };

    @HookMethod(eventName = BuryConstant.EventName.AMAP_PRIVACY_SET)
    private void sendBuryPointForSetPrivacyTime(String isOneYearPrivacy){
        BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_HOME_PREDICTION, isOneYearPrivacy)
                .build();
        BuryPointController.getInstance().setBuryProps(buryProperty);
    }

}
