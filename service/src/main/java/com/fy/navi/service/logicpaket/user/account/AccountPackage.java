package com.fy.navi.service.logicpaket.user.account;

import android.text.TextUtils;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.adapter.user.account.AccountAdapter;
import com.fy.navi.service.adapter.user.account.AccountAdapterCallBack;
import com.fy.navi.service.define.code.UserDataCode;
import com.fy.navi.service.define.user.account.AccountProfileInfo;
import com.fy.navi.service.define.user.account.AccountUserInfo;
import com.fy.navi.service.greendao.CommonManager;
import com.fy.navi.service.logicpaket.user.msgpush.MsgPushPackage;

import java.util.Hashtable;

/**
 * @Description
 * @Author fh
 * @date 2024/12/18
 */
public class AccountPackage implements AccountAdapterCallBack {
    private final AccountAdapter mAccountAdapter;
    private final Hashtable<String, AccountCallBack> callBacks;
    private CommonManager commonManager;
    private boolean isLogin = false; // 判断当前用户是否已登录。  默认false，未登录

    private AccountPackage() {
        callBacks = new Hashtable<>();
        mAccountAdapter = AccountAdapter.getInstance();
        commonManager = CommonManager.getInstance();
        commonManager.init();
    }

    public void initAccountService(){
        mAccountAdapter.initAccountService();
        mAccountAdapter.registerCallBack("AccountPackage", this);
    }

    /**
     * 从本地数据库中获取用户信息
     */
    public AccountProfileInfo getUserInfo() {
        AccountProfileInfo info = new AccountProfileInfo();
        String valueJson = commonManager.getValueByKey(UserDataCode.SETTING_GET_USERINFO);
        Logger.i("getUserInfo valueJson = " + valueJson);
        if (!TextUtils.isEmpty(valueJson)) {
           info = GsonUtils.fromJson(valueJson, AccountProfileInfo.class);
            if (info != null) {
                if (!TextUtils.isEmpty(info.uid)) {
                    isLogin = true;
                } else {
                    isLogin = false;
                }
            }
        }
        return info;
    }

    public boolean isLogin() {
        return isLogin;
    }

    @Override
    public void notifyVerificationCode(int errCode, int taskId, AccountUserInfo result) {
        for (AccountCallBack observer : callBacks.values()) {
            observer.notifyVerificationCode(errCode, taskId, result);
        }
    }

    @Override
    public void notifyAccountCheck(int errCode, int taskId, AccountUserInfo result) {
        for (AccountCallBack observer : callBacks.values()) {
            observer.notifyAccountCheck(errCode, taskId, result);
        }
    }

    @Override
    public void notifyAccountRegister(int errCode, int taskId, AccountUserInfo result) {
        for (AccountCallBack observer : callBacks.values()) {
            observer.notifyAccountRegister(errCode, taskId, result);
        }
    }

    @Override
    public void notifyMobileLogin(int errCode, int taskId, AccountUserInfo result) {

        if (result != null && result.code == 1) {
            // 手机号登录成功后，用户登录信息保存到数据库
            isLogin = true;
            AccountProfileInfo info = GsonUtils.convertToT(result.profileInfo, AccountProfileInfo.class);
            commonManager.insertOrReplace(UserDataCode.SETTING_GET_USERINFO, GsonUtils.toJson(info));
        }

        for (AccountCallBack observer : callBacks.values()) {
            observer.notifyMobileLogin(errCode, taskId, result);
        }
    }

    @Override
    public void notifyQRCodeLogin(int errCode, int taskId, AccountUserInfo result) {
        for (AccountCallBack observer : callBacks.values()) {
            observer.notifyQRCodeLogin(errCode, taskId, result);
        }
    }
    @Override
    public void notifyQRCodeLoginConfirm(int errCode, int taskId, AccountUserInfo result) {
        if (result != null && result.code == 1) {
            isLogin = true;
            MsgPushPackage.getInstance().startListen(result.uid);
            AccountProfileInfo info = GsonUtils.convertToT(result.profileInfo, AccountProfileInfo.class);
            commonManager.insertOrReplace(UserDataCode.SETTING_GET_USERINFO, GsonUtils.toJson(info));
        }
        for (AccountCallBack observer : callBacks.values()) {
            observer.notifyQRCodeLoginConfirm(errCode, taskId, result);
        }
    }

    @Override
    public void notifyAccountProfile(int errCode, int taskId, AccountUserInfo result) {
        for (AccountCallBack observer : callBacks.values()) {
            observer.notifyAccountProfile(errCode, taskId, result);
        }
    }

    @Override
    public void notifyAvatar(int errCode, int taskId, AccountUserInfo result) {
        for (AccountCallBack observer : callBacks.values()) {
            observer.notifyAvatar(errCode, taskId, result);
        }
    }

    @Override
    public void notifyAccountLogout(int errCode, int taskId, AccountUserInfo result) {

        if (result != null && result.code == 1) {
            Logger.i("AccountModel", "退出登录成功");
            // 退出登录后，清除数据库中的用户信息
            commonManager.deleteValue(UserDataCode.SETTING_GET_USERINFO);
            isLogin = false;
            MsgPushPackage.getInstance().stopListen();
        }

        for (AccountCallBack observer : callBacks.values()) {
            observer.notifyAccountLogout(errCode, taskId, result);
        }
    }

    @Override
    public void notifyAccountUnRegister(int errCode, int taskId, AccountUserInfo result) {
        for (AccountCallBack observer : callBacks.values()) {
            observer.notifyAccountUnRegister(errCode, taskId, result);
        }
    }

    public synchronized void registerCallBack(String key, AccountCallBack callback) {
        if (callback != null && !callBacks.contains(callback)) {
            callBacks.put(key,callback);
        }
    }

    public int verificationCodeRequest(String mobileNum) {
        return mAccountAdapter.verificationCodeRequest(mobileNum);
    }

    public int accountCheckRequest() {
        return mAccountAdapter.accountCheckRequest();
    }

    public int accountRegisterRequest(String codeInput, String mobileInput) {
        return mAccountAdapter.accountRegisterRequest(codeInput, mobileInput);
    }

    public int mobileLoginRequest(String codeInput, String mobileInput) {
        return mAccountAdapter.mobileLoginRequest(codeInput, mobileInput);
    }

    public int qRCodeLoginRequest(int qrType) {
        return mAccountAdapter.qRCodeLoginRequest(qrType);
    }

    public int qRCodeLoginConfirmRequest(String qrCodeId) {
        return mAccountAdapter.qRCodeLoginConfirmRequest(qrCodeId);
    }

    public int accountProfileRequest() {
        return mAccountAdapter.accountProfileRequest();
    }

    public int avatarRequest() {
        return mAccountAdapter.avatarRequest();
    }

    public int accountLogoutRequest() {
        return mAccountAdapter.accountLogoutRequest();
    }

    public int accountUnRegisterRequest() {
        return mAccountAdapter.accountUnRegisterRequest();
    }

    public void unInitAccountService(){
        mAccountAdapter.unInitAccountService();
    }

    public static AccountPackage getInstance() {
        return Helper.ep;
    }

    private static final class Helper {
        private static final AccountPackage ep = new AccountPackage();
    }
}
