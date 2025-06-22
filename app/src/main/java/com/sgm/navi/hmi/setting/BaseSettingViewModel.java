package com.sgm.navi.hmi.setting;

import android.app.Application;

import androidx.annotation.NonNull;

import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.ui.action.Action;
import com.sgm.navi.ui.base.BaseViewModel;

public class BaseSettingViewModel extends BaseViewModel<SettingFragment, SettingModel> {
    private static final String TAG = MapDefaultFinalTag.SETTING_HMI_TAG;

    public BaseSettingViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected SettingModel initModel() {
        return new SettingModel();
    }

    @Override
    public void onCreate() {
        super.onCreate();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    public Action mFinishSetting = () -> closeFragment(true);
}
