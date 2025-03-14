package com.fy.navi.hmi.carconnect;

import android.app.Application;
import android.os.Bundle;

import androidx.annotation.NonNull;

import com.fy.navi.hmi.carconnect.help.CarConnectHelpFragment;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

/**
 * @Description TODO
 * @Author fh
 * @date 2024/12/24
 */
public class BaseCarConnectViewModel extends BaseViewModel<CarConnectFragment, CarConnectModel> {

    private static final String TAG = MapDefaultFinalTag.ACCOUNT_HMI_TAG;

    public BaseCarConnectViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected CarConnectModel initModel() {
        return new CarConnectModel();
    }

    //返回上一页
    public Action carConnectBack = this::closeCurrentFragment;

    public Action openHelpFirst = () -> {
        openCommonHelp(0);
    };

    public Action openHelpSecond = () -> {
        openCommonHelp(1);
    };

    public Action openHelpThird = () -> {
        openCommonHelp(2);
    };

    public Action openHelpForth = () -> {
        openCommonHelp(3);
    };

    public void openCommonHelp(int position){
        Bundle bundle = new Bundle();
        bundle.putInt("position", position);
        addFragment(new CarConnectHelpFragment(), bundle);
    }

    public void clearUserInfo(){
        mView.clearUserInfo();
    }

    public void closeCurrentFragment(){
        closeFragment(true);
    }

}
