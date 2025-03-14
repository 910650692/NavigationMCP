package com.fy.navi.hmi.setting.others.privacy;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseSettingOthersPrivacyViewModel extends BaseViewModel<SettingOthersPrivacyFragment, SettingOthersPrivacyModel> {


    public MutableLiveData<Boolean> isOneYearPrivacy = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> isNeverPrivacy = new MutableLiveData<>(true);
    public MutableLiveData<String> endDate = new MutableLiveData<>("");


    public BaseSettingOthersPrivacyViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected SettingOthersPrivacyModel initModel() {
        return new SettingOthersPrivacyModel();
    }

    public void dualChoiceControl(String key, boolean isTrue) {
        if (key.equals(SettingController.KEY_SETTING_PRIVACY_STATUS)) {
            isNeverPrivacy.setValue(!isTrue);
            isOneYearPrivacy.setValue(isTrue);
            mView.setEndDate(mModel.getEndDate(), isTrue);
        }
    }

    public void initView() {
        mModel.initView();
    }

    public void setEndDate(String endDateTime) {
        endDate.setValue(endDateTime);
    }

    public Action finishPrivacy = () -> {
        closeFragment(true);
    };

    public Action oneYearPrivacy = () -> {
        mView.setEndDate(mModel.getFormattedDate(), true);
        mModel.setPrivacyStatus(true);
        mModel.setEndDate(mModel.getFormattedDate());
        SettingUpdateObservable.getInstance().notifySettingChanged(SettingController.KEY_SETTING_PRIVACY_STATUS, true);
    };

    public Action neverPrivacy = () -> {
        mView.setEndDate(mModel.getFormattedDate(), false);
        mModel.setPrivacyStatus(false);
        mModel.setEndDate("");
        SettingUpdateObservable.getInstance().notifySettingChanged(SettingController.KEY_SETTING_PRIVACY_STATUS, false);
    };

}
