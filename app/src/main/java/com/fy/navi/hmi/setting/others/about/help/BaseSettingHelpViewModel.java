package com.fy.navi.hmi.setting.others.about.help;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseSettingHelpViewModel extends BaseViewModel<SettingHelpFragment, SettingHelpModel> {

    public MutableLiveData<Integer> selectPosition = new MutableLiveData<>(0);

    public BaseSettingHelpViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected SettingHelpModel initModel() {
        return new SettingHelpModel();
    }

    public void setSelectPosition(int position) {
        selectPosition.setValue(position);
    }

    public Action clickBack = () -> {
        closeFragment(true);
    };

}
