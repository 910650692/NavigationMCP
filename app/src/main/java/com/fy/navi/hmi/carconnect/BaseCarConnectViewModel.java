package com.fy.navi.hmi.carconnect;

import android.app.Application;
import android.os.Bundle;

import androidx.annotation.NonNull;

import com.fy.navi.hmi.carconnect.help.CarConnectHelpFragment;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseCarConnectViewModel extends BaseViewModel<CarConnectFragment, CarConnectModel> {

    private static final String TAG = MapDefaultFinalTag.ACCOUNT_HMI_TAG;

    public BaseCarConnectViewModel(final @NonNull Application application) {
        super(application);
    }

    @Override
    protected CarConnectModel initModel() {
        return new CarConnectModel();
    }

    //返回上一页
    public Action mCarConnectBack = this::closeCurrentFragment;

    public Action mOpenHelpFirst = () -> {
        openCommonHelp(0);
    };

    public Action mOpenHelpSecond = () -> {
        openCommonHelp(1);
    };

    public Action mOpenHelpThird = () -> {
        openCommonHelp(2);
    };

    public Action mOpenHelpForth = () -> {
        openCommonHelp(3);
    };

    public Action mLastOneMile = () -> {
        final boolean isChecked = SettingManager.getInstance().getValueByKey(
                SettingController.KEY_SETTING_IS_SEND_DESTINATION_LAST_MILE).equals(CarConnectFragment.DESTINATION_SEND);
        mView.setLastOneMile(!isChecked);
        mView.setLastOneMileChecked(!isChecked);
    };

    /**
     * open common help
     * @param position
     */
    public void openCommonHelp(final int position){
        final Bundle bundle = new Bundle();
        bundle.putInt("position", position);
        addFragment(new CarConnectHelpFragment(), bundle);
    }

    /**
     * clear use info
     */
    public void clearUserInfo(){
        mView.clearUserInfo();
    }

    /**
     * close current fragment
     */
    public void closeCurrentFragment(){
        closeFragment(true);
    }

}
