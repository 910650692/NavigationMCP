package com.fy.navi.hmi.setting.others;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.define.user.account.AccountUserInfo;
import com.fy.navi.service.define.user.wechat.BLResponseBean;
import com.fy.navi.service.greendao.history.History;
import com.fy.navi.service.greendao.history.HistoryManager;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.service.logicpaket.engine.EnginePackage;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.service.logicpaket.user.account.AccountCallBack;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;
import com.fy.navi.service.logicpaket.user.wechat.WeChatCallBack;
import com.fy.navi.service.logicpaket.user.wechat.WeChatPackage;
import com.fy.navi.ui.base.BaseModel;

import java.util.List;

public class SettingOthersModel extends BaseModel<SettingOthersViewModel> implements SettingUpdateObservable.SettingUpdateObserver, AccountCallBack, WeChatCallBack {

    private final AccountPackage accountPackage;
    private final SettingPackage settingPackage;
    private final WeChatPackage weChatPackage;
    private final SettingManager settingManager;
    private final HistoryManager historyManager;

    public SettingOthersModel() {
        accountPackage = AccountPackage.getInstance();
        settingPackage = SettingPackage.getInstance();
        weChatPackage = WeChatPackage.getInstance();
        settingManager = SettingManager.getInstance();
        settingManager.init();
        historyManager = HistoryManager.getInstance();
        historyManager.init();
    }
    @Override
    public void onCreate() {
        super.onCreate();
        SettingUpdateObservable.getInstance().addObserver("SettingOthersModel", this);
        accountPackage.registerCallBack("SettingOthersModel",this);
        weChatPackage.registerCallBack("SettingOthersModel", this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        weChatPackage.unRegisterCallBack("SettingOthersModel");
    }

    public void initView() {
        mViewModel.updatePrivacyStatus(settingPackage.getPrivacyStatus());
        mViewModel.updateUserInfo(accountPackage.getUserInfo().nickname, accountPackage.getUserInfo().avatar);
        String weChatStatus = settingManager.getValueByKey(SettingController.KEY_SETTING_IS_WE_CHAT_BIND);
        mViewModel.setWeChatStatus(weChatStatus.equals(SettingController.VALUE_GENERIC_TRUE));
        getSdkVersion();
    }

    @Override
    public void onUpdateSetting(String key, boolean value) {
        if (key.equals(SettingController.KEY_SETTING_PRIVACY_STATUS)) {
            mViewModel.updatePrivacyStatus(value);
        }
    }

    public void logoutAccount() {
        accountPackage.accountLogoutRequest();
    }

    /**
     * 退出登录后，刷新UI
     */
    @Override
    public void notifyAccountLogout(int errCode, int taskId, AccountUserInfo result) {
        if (result != null && result.code == 1) {
            mViewModel.clearUserInfo();
        }
    }

    /**
     * 手机号登录成功，刷新UI
     */
    @Override
    public void notifyMobileLogin(int errCode, int taskId, AccountUserInfo result) {
        if (result != null && result.code == 1) {
            if (result.profileInfo != null) {
                mViewModel.updateUserInfo(result.profileInfo.nickname, result.profileInfo.avatar);
            }
        }
    }

    @Override
    public void notifyQRCodeLoginConfirm(int errCode, int taskId, AccountUserInfo result) {
        if (result != null && result.code == 1) {
            if (result.profileInfo != null) {
                mViewModel.updateUserInfo(result.profileInfo.nickname, result.profileInfo.avatar);
            }
        }
    }

    public boolean getIsLogin() {
        return accountPackage.isLogin();
    }

    @Override
    public void notifyWeixinStatus(BLResponseBean result) {
        if (result != null) {
            if (result.getCode() == 1) {
                mViewModel.setWeChatStatus(true);
            } else {
                mViewModel.setWeChatStatus(false);
            }
        }
    }

    /**
     * @param result 回调数据
     */
    @Override
    public void notifyGQRCodeConfirm(BLResponseBean result) {
        if (result != null && result.getCode() == 1) {
            mViewModel.setWeChatStatus(true);
        } else {
            mViewModel.setWeChatStatus(false);
        }
    }

    /**
     * 通过type查找其对应行程历史信息
     * @return
     */
    public int getValueByType() {
        int size = 0;
        List<History> list = historyManager.getValueByType("行程历史");
        if (list != null && !list.isEmpty()) {
            size = list.size();
        }
        return size;
    }

    public void getSdkVersion(){
        String sdkVersion = EnginePackage.getInstance().getSdkVersion();
        mViewModel.setSdkVersion(sdkVersion);
    }

    public boolean getWechatStatus() {
        String isBind = settingManager.getValueByKey(SettingController.KEY_SETTING_IS_WE_CHAT_BIND);
        return isBind.equals(SettingController.VALUE_GENERIC_TRUE);
    }
}
