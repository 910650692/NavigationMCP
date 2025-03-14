package com.fy.navi.hmi.setting.others.about;

import android.app.Application;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.fy.navi.hmi.setting.others.about.help.SettingHelpFragment;
import com.fy.navi.hmi.setting.others.licenses.SettingOthersLicensesFragment;
import com.fy.navi.hmi.setting.others.protocol.SettingOthersProtocolCenterFragment;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseSettingOthersAboutViewModel extends BaseViewModel<SettingOthersAboutFragment, SettingOthersAboutModel> {

    public MutableLiveData<String> channelID = new MutableLiveData<>("");
    public MutableLiveData<String> mapDataVersion = new MutableLiveData<>("");
    public MutableLiveData<String> sdkVersion = new MutableLiveData<>("");

    public BaseSettingOthersAboutViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected SettingOthersAboutModel initModel() {
        return new SettingOthersAboutModel();
    }

    @Override
    public void onCreate() {
        super.onCreate();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    public void initView() {
        mModel.initView();
    }

    public Action finishAbout = () -> {
        closeFragment(true);
    };

    public Action openProtocolCenter = () -> {
        addFragment(new SettingOthersProtocolCenterFragment(), null);
    };

    public Action openLicensesInfo = () -> {
        addFragment(new SettingOthersLicensesFragment(), null);
    };

    public Action openHelp = () -> {
        Bundle bundle = new Bundle();
        bundle.putInt("HelpIndex", 0);
        addFragment(new SettingHelpFragment(), bundle);
    };

    public void setChannelID(String channelID) {
        this.channelID.setValue(channelID);
    }

    public void setMapDataVersion(String mapDataVersion) {
        this.mapDataVersion.setValue(mapDataVersion);
    }

    public void setSdkVersion(String sdkVersion) {
        this.sdkVersion.setValue(sdkVersion);
    }
}
