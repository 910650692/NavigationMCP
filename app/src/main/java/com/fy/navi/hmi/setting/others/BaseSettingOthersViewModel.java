package com.fy.navi.hmi.setting.others;

import android.app.Application;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.account.AccountQRCodeLoginFragment;
import com.fy.navi.hmi.carconnect.CarConnectFragment;
import com.fy.navi.hmi.drivingrecord.DrivingRecordFragment;
import com.fy.navi.hmi.drivingrecord.recordlogin.DrivingRecordLoginFragment;
import com.fy.navi.hmi.setting.others.about.SettingOthersAboutFragment;
import com.fy.navi.hmi.setting.others.privacy.SettingOthersPrivacyFragment;
import com.fy.navi.hmi.wechat.WeChatFragment;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseSettingOthersViewModel extends BaseViewModel<SettingOthersFragment, SettingOthersModel> {

    public MutableLiveData<String> sdkVersion = new MutableLiveData<>("");


    public BaseSettingOthersViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected SettingOthersModel initModel() {
        return new SettingOthersModel();
    }

    @Override
    public void onCreate() {
        super.onCreate();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    // 跳转到行程历史页
    public Action openDivingRecord = () -> {
        if (mModel.getIsLogin()) { // 是否已登录高德账号
            addFragment(new DrivingRecordFragment(), null);
        } else {
            // 未登录高德账号时，需判断当前是否存在行程历史数据
            if (mModel.getValueByType() > 0) { // 未登录存在历史数据
                addFragment(new DrivingRecordFragment(), null);
            } else {
                addFragment(new DrivingRecordLoginFragment(), null);
            }
        }
    };

    public Action openCarConnect = () -> {
        if (mModel.getIsLogin()) {
            Bundle bundle = new Bundle();
            bundle.putString("userName", mView.getUserName());
            bundle.putString("userIcon", mView.getUserIcon());
            addFragment(new CarConnectFragment(), bundle);
        } else {
            ToastUtils.Companion.getInstance().showCustomToastView("登录高德账号才能使用手车互联服务");
        }
    };

    public Action openWeChat= () -> {
        if (mModel.getIsLogin()) {
            addFragment(new WeChatFragment(), null);
        } else {
            ToastUtils.Companion.getInstance().showCustomToastView("登录高德账号才能使用微信互联服务");
        }
    };

    public Action openAccount = () -> {
        if (!mModel.getIsLogin()) {
            addFragment(new AccountQRCodeLoginFragment(), null);
        } else {
            mView.showLogoutAccountDialog();
        }
    };

    public Action openAbout = () -> {
        addFragment(new SettingOthersAboutFragment(), null);
    };

    public Action openPrivacy = () -> {
        addFragment(new SettingOthersPrivacyFragment(), null);
    };

    public Action clearMemory = () -> {
        mView.showClearMemoryDialog();
    };

    public Action resetSetting = () -> {
        mView.showResetSettingDialog();
    };

    public void updatePrivacyStatus(boolean status) {
        Logger.d("updatePrivacyStatus status = " + status);
        mView.updatePrivacyStatus(status);
    }

    public void initView() {
        mModel.initView();
    }

    public void updateUserInfo(String userName, String url) {
        mView.updateUserInfo(userName, url);
    }

    public void clearUserInfo() {
        mView.clearUserInfo();
    }

    public void logoutAccount() {
        mModel.logoutAccount();
    }

    public void setWeChatStatus(boolean status) {
        mView.setWeChatStatus(status);
    }

    public boolean getIsLogin() {
        return mModel.getIsLogin();
    }

    public boolean getWechatStatus() {
        return mModel.getWechatStatus();
    }

    public void setSdkVersion(String sdkVersion) {
        String defaultString = ResourceUtils.Companion.getInstance().getString(R.string.setting_others_about_version);
        this.sdkVersion.setValue(defaultString + sdkVersion);
    }
}
