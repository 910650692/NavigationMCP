package com.fy.navi.hmi.setting.others.privacy;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

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
    };

    public Action mNeverPrivacy = () -> {
        mView.setEndDate(mModel.getFormattedDate(), false);
        mModel.setPrivacyStatus(false);
        mModel.setEndDate("");
        SettingUpdateObservable.getInstance().notifySettingChanged(SettingController.KEY_SETTING_PRIVACY_STATUS, false);
    };

}
