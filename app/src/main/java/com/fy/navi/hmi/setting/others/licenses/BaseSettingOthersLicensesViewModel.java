package com.fy.navi.hmi.setting.others.licenses;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseSettingOthersLicensesViewModel extends BaseViewModel<SettingOthersLicensesFragment, SettingOthersLicensesModel> {

    public BaseSettingOthersLicensesViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected SettingOthersLicensesModel initModel() {
        return new SettingOthersLicensesModel();
    }

    public Action finishLicenses = () -> closeFragment(true);
}
