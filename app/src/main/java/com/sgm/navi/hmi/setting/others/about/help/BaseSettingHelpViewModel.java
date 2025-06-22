package com.sgm.navi.hmi.setting.others.about.help;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.sgm.navi.ui.action.Action;
import com.sgm.navi.ui.base.BaseViewModel;

public class BaseSettingHelpViewModel extends BaseViewModel<SettingHelpFragment, SettingHelpModel> {

    public MutableLiveData<Integer> mSelectPosition = new MutableLiveData<>(0);

    public BaseSettingHelpViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected SettingHelpModel initModel() {
        return new SettingHelpModel();
    }

    /**
     * 设置选中位置
     * @param position 选中位置
     */
    public void setSelectPosition(final int position) {
        mSelectPosition.setValue(position);
    }

    public Action mClickBack = () -> {
        closeFragment(true);
    };

}
