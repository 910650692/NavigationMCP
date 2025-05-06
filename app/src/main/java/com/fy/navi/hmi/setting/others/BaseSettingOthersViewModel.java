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

    public MutableLiveData<String> mSdkVersion = new MutableLiveData<>("");
    public MutableLiveData<String> mTotalSize = new MutableLiveData<>("0KB");

    public MutableLiveData<Boolean> mClearMemoryDialogShown = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> mResetSettingDialogShown = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> mLogoutAccountDialogShown = new MutableLiveData<>(false);


    public BaseSettingOthersViewModel(@NonNull final Application application) {
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
    public Action mOpenDivingRecord = () -> {
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

    public Action mOpenCarConnect = () -> {
        if (mModel.getIsLogin()) {
            final Bundle bundle = new Bundle();
            bundle.putString("userName", mView.getUserName());
            bundle.putString("userIcon", mView.getUserIcon());
            addFragment(new CarConnectFragment(), bundle);
        } else {
            ToastUtils.Companion.getInstance().showCustomToastView("登录高德账号才能使用手车互联服务");
        }
    };

    public Action mOpenWeChat = () -> {
        if (mModel.getIsLogin()) {
            addFragment(new WeChatFragment(), null);
        } else {
            ToastUtils.Companion.getInstance().showCustomToastView("登录高德账号才能使用微信互联服务");
        }
    };

    public Action mOpenAccount = () -> {
        if (!mModel.getIsLogin()) {
            addFragment(new AccountQRCodeLoginFragment(), null);
        } else {
            mView.showLogoutAccountDialog();
            setLogoutAccountDialogShown(true);
        }
    };


//    public Action mOpenAbout = () -> {
//        Logger.d("mOpenAbout");
//        addFragment(new SettingOthersAboutFragment(), null);
//    };


    public Action mOpenAbout = new Action() {

        @Override
        public void call() {
            addFragment(new SettingOthersAboutFragment(), null);
        }
    };

    public Action mOpenPrivacy = () -> {
        addFragment(new SettingOthersPrivacyFragment(), null);
    };

    public Action mClearMemory = () -> {
        mView.showClearMemoryDialog();
        setMemoryDialogShown(true);
    };

    public Action mResetSetting = () -> {
        mView.showResetSettingDialog();
        setResetSettingDialogShown(true);
    };

    /**
     * 更新隐私协议状态
     * @param status true:已授权，false:未授权
     */
    public void updatePrivacyStatus(final boolean status) {
        Logger.d("updatePrivacyStatus status = " + status);
        mView.updatePrivacyStatus(status);
    }

    /**
     * 初始化视图
     */
    public void initView() {
        mModel.initView();
    }

    /**
     * 更新用户信息
     * @param userName 用户名
     * @param url 用户头像地址
     */
    public void updateUserInfo(final String userName, final String url) {
        mView.updateUserInfo(userName, url);
    }

    /**
     * 清除用户信息
     */
    public void clearUserInfo() {
        mView.clearUserInfo();
    }

    /**
     * 退出登录
     */
    public void logoutAccount() {
        mModel.logoutAccount();
    }

    /**
     * 设置微信状态
     * @param status true 已绑定，false 未绑定
     */
    public void setWeChatStatus(final boolean status) {
        mView.setWeChatStatus(status);
    }

    /**
     * 获取是否登录状态
     * @return true 已登录，false 未登录
     */
    public boolean getIsLogin() {
        return mModel.getIsLogin();
    }

    /**
     * 获取微信绑定状态
     * @return  true 已绑定，false 未绑定
     */
    public boolean getWechatStatus() {
        return mModel.getWechatStatus();
    }

    /**
     * 设置SDK版本
     * @param sdkVersion SDK版本号
     */
    public void setSdkVersion(final String sdkVersion) {
        final String defaultString = ResourceUtils.Companion.getInstance().getString(R.string.setting_others_about_version);
        this.mSdkVersion.setValue(defaultString + sdkVersion);
    }

    /**
     * 清除数据库中所有设置相关记录
     */
    public void clearAll() {
        mModel.clearAll();
    }

    /**
     *重置设置
     */
    public void resetSetting() {
        mModel.resetSetting();
    }

    /**
     * 删除指定文件夹列表下的所有文件和子文件夹
     */
    public void deleteFilesInDirectories() {
        mModel.deleteFilesInDirectories();
    }

    /**
     * 设置指定文件夹列表下的所有文件和子文件夹的总大小
     * @param totalSize 总大小
     */
    public void setTotalSizeOfDirectories(final String totalSize) {
        mTotalSize.setValue(totalSize);
    }

    /**
     * 获取指定文件夹列表下的所有文件的总大小
     * @return 文件夹总大小（字节）
     */
    public String getTotalSizeOfDirectories() {
        return mModel.getTotalSizeOfDirectories();
    }

    /**
     * 设置清除内存弹窗状态
     * @param isClearMemoryDialogShown true:显示，false:隐藏
     */
    public void setMemoryDialogShown(final boolean isClearMemoryDialogShown){
        mClearMemoryDialogShown.setValue(isClearMemoryDialogShown);
    }

    /**
     * @return true:显示，false:隐藏
     */
    public boolean getIsClearMemoryDialogShown() {
        return Boolean.TRUE.equals(mClearMemoryDialogShown.getValue());
    }

    /**
     * 设置重置设置弹窗状态
     * @param isResetSettingDialogShown true:显示，false:隐藏
     */
    public void setResetSettingDialogShown(final boolean isResetSettingDialogShown) {
        mResetSettingDialogShown.setValue(isResetSettingDialogShown);
    }

    /**
     * 设置重置设置弹窗状态
     * @return true:显示，false:隐藏
     */
    public boolean getIsResetSettingDialogShown() {
        return Boolean.TRUE.equals(mResetSettingDialogShown.getValue());
    }

    public boolean getIsLogoutAccountDialogShown() {
        return Boolean.TRUE.equals(mLogoutAccountDialogShown.getValue());
    }

    /**
     * 设置退出登录弹窗状态
     * @param isLogoutAccountDialogShown true:显示，false:隐藏
     */
    public void setLogoutAccountDialogShown(final boolean isLogoutAccountDialogShown) {
        mLogoutAccountDialogShown.setValue(isLogoutAccountDialogShown);
    }
}
