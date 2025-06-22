package com.sgm.navi.hmi.setting.others.licenses;

import android.app.Application;

import androidx.annotation.NonNull;

import com.sgm.navi.ui.action.Action;
import com.sgm.navi.ui.base.BaseViewModel;

public class BaseSettingOthersLicensesViewModel extends BaseViewModel<SettingOthersLicensesFragment, SettingOthersLicensesModel> {

    public BaseSettingOthersLicensesViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected SettingOthersLicensesModel initModel() {
        return new SettingOthersLicensesModel();
    }

    public Action mFinishLicenses = () -> closeFragment(true);
}
