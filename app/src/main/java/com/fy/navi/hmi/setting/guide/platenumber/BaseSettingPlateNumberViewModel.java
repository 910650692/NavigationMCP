package com.fy.navi.hmi.setting.guide.platenumber;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseSettingPlateNumberViewModel extends BaseViewModel<SettingPlateNumberFragment, SettingPlateNumberModel> {

    public MutableLiveData<Boolean> isFocus = new MutableLiveData<>(false);

    public BaseSettingPlateNumberViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected SettingPlateNumberModel initModel() {
        return new SettingPlateNumberModel();
    }

    public Action showProvinceKeyboard = () -> {
        mView.showProvinceKeyboard();
        isFocus.setValue(true);
    };

    public Action hideKeyboard = () -> {
        mView.hideKeyboard();
    };

    public Action closePlateNumber = () -> {
        closeFragment(true);
    };

    public Action showPlateNumberKeyboard = () -> {
        mView.showPlateNumberKeyboard();
    };
     public Action plateNumberInputFinish = () -> {
        mView.plateNumberInputFinish();
    };

     public Action clearPlateNumber = () -> {
        mView.clearPlateNumber();
    };

     public String getPlateNumber() {
        return mModel.getPlateNumber();
    }

    public void setIsFocus(boolean isFocus) {
        this.isFocus.setValue(isFocus);
    }
}
