package com.sgm.navi.hmi.setting.others.about;

import android.app.Application;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.android.utils.thread.ThreadManager;
import com.sgm.navi.hmi.BuildConfig;
import com.sgm.navi.hmi.setting.others.about.help.SettingHelpFragment;
import com.sgm.navi.hmi.setting.others.licenses.SettingOthersLicensesFragment;
import com.sgm.navi.hmi.setting.others.protocol.SettingOthersProtocolCenterFragment;
import com.sgm.navi.ui.action.Action;
import com.sgm.navi.ui.base.BaseViewModel;

public class BaseSettingOthersAboutViewModel extends BaseViewModel<SettingOthersAboutFragment, SettingOthersAboutModel> {

    public MutableLiveData<String> mChannelID = new MutableLiveData<>("");
    public MutableLiveData<String> mMapDataVersion = new MutableLiveData<>("");
    public MutableLiveData<String> mSdkVersion = new MutableLiveData<>("");
    public MutableLiveData<String> mVersion = new MutableLiveData<>("");

    public BaseSettingOthersAboutViewModel(@NonNull final Application application) {
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

    /**
     * 初始化视图
     */
    public void initView() {
        mModel.initView();
    }

    public Action mFinishAbout = () -> {
        closeFragment(true);
    };

    public Action mOpenProtocolCenter = () -> {
        addFragment(new SettingOthersProtocolCenterFragment(), null);
    };

    public Action mOpenLicensesInfo = () -> {
        addFragment(new SettingOthersLicensesFragment(), null);
    };

    public Action mOpenHelp = () -> {
        final Bundle bundle = new Bundle();
        bundle.putInt("HelpIndex", 0);
        addFragment(new SettingHelpFragment(), bundle);
    };

    /**
     * 设置渠道ID
     * @param channelID 渠道ID
     */
    public void setChannelID(final String channelID) {
        this.mChannelID.setValue(channelID);
    }

    /**
     * 设置地图数据版本
     * @param mapDataVersion 地图数据版本号
     */
    public void setMapDataVersion(final String mapDataVersion) {
        this.mMapDataVersion.setValue(mapDataVersion);
    }

    /**
     * 设置SDK版本
     * @param sdkVersion SDK版本号
     */
    public void setSdkVersion(final String sdkVersion) {
        this.mSdkVersion.setValue(sdkVersion);
    }

    /**
     * 获取VersionName
     */
    public String getVersionName() {
        return BuildConfig.VERSION_NAME;
    }

    /**
     * 设置互联网审图号
     * @param version 互联网审图号
     */
    public void setVersion(final String version) {
        ThreadManager.getInstance().postUi(() -> {
            this.mVersion.setValue(version);
        });
    }
}
