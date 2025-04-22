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
import com.fy.navi.service.greendao.history.HistoryManager;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.service.logicpaket.user.msgpush.MsgPushPackage;

import java.util.Hashtable;


public final class AccountPackage implements AccountAdapterCallBack {
    private final AccountAdapter mAccountAdapter;
    private final Hashtable<String, AccountCallBack> mCallBacks;
    private final CommonManager mCommonManager;
    private boolean mIsLogin = false; // 判断当前用户是否已登录。  默认false，未登录
    private final HistoryManager mHistoryManager;

    private AccountPackage() {
        mCallBacks = new Hashtable<>();
        mAccountAdapter = AccountAdapter.getInstance();
        mCommonManager = CommonManager.getInstance();
        mCommonManager.init();
        mHistoryManager = HistoryManager.getInstance();
        mHistoryManager.init();
    }

    /**
     * 初始化服务
     */
    public void initAccountService(){
        mAccountAdapter.initAccountService();
        mAccountAdapter.registerCallBack("AccountPackage", this);
    }

    /**
     * 从本地数据库中获取用户信息
     * @return 用户信息
     */
    public AccountProfileInfo getUserInfo() {
        AccountProfileInfo info = new AccountProfileInfo();
        final String valueJson = mCommonManager.getValueByKey(UserDataCode.SETTING_GET_USERINFO);
        Logger.i("getUserInfo valueJson = " + valueJson);
        if (!TextUtils.isEmpty(valueJson)) {
           info = GsonUtils.fromJson(valueJson, AccountProfileInfo.class);
            if (info != null) {
                if (!TextUtils.isEmpty(info.getUid())) {
                    mIsLogin = true;
                } else {
                    mIsLogin = false;
                }
            }
        }
        return info;
    }


    /**
     * 是否真正登录  用此方法
     * @return 用户信息
     */
    public boolean reallyLogin() {
        AccountProfileInfo info = new AccountProfileInfo();
        final String valueJson = mCommonManager.getValueByKey(UserDataCode.SETTING_GET_USERINFO);
        Logger.i("getUserInfo valueJson = " + valueJson);
        if (!TextUtils.isEmpty(valueJson)) {
            info = GsonUtils.fromJson(valueJson, AccountProfileInfo.class);
            if (info != null) {
                return !TextUtils.isEmpty(info.getUid());
            }
        }
        return false;
    }


    public boolean isLogin() {
        return mIsLogin;
    }

    @Override
    public void notifyVerificationCode(final int errCode, final int taskId, final AccountUserInfo result) {
        for (AccountCallBack observer : mCallBacks.values()) {
            observer.notifyVerificationCode(errCode, taskId, result);
        }
    }

    @Override
    public void notifyAccountCheck(final int errCode, final int taskId, final AccountUserInfo result) {
        for (AccountCallBack observer : mCallBacks.values()) {
            observer.notifyAccountCheck(errCode, taskId, result);
        }
    }

    @Override
    public void notifyAccountRegister(final int errCode, final int taskId, final AccountUserInfo result) {
        for (AccountCallBack observer : mCallBacks.values()) {
            observer.notifyAccountRegister(errCode, taskId, result);
        }
    }

    @Override
    public void notifyMobileLogin(final int errCode, final int taskId, final AccountUserInfo result) {

        if (result != null && result.getCode() == 1) {
            // 手机号登录成功后，用户登录信息保存到数据库
            mIsLogin = true;
            final AccountProfileInfo info = GsonUtils.convertToT(result.getProfileInfo(), AccountProfileInfo.class);
            mCommonManager.insertOrReplace(UserDataCode.SETTING_GET_USERINFO, GsonUtils.toJson(info));
        }

        for (AccountCallBack observer : mCallBacks.values()) {
            observer.notifyMobileLogin(errCode, taskId, result);
        }
    }

    @Override
    public void notifyQRCodeLogin(final int errCode, final int taskId, final AccountUserInfo result) {
        for (AccountCallBack observer : mCallBacks.values()) {
            observer.notifyQRCodeLogin(errCode, taskId, result);
        }
    }
    @Override
    public void notifyQRCodeLoginConfirm(final int errCode, final int taskId, final AccountUserInfo result) {
        if (result != null && result.getCode() == 1) {
            mIsLogin = true;
            MsgPushPackage.getInstance().startListen(result.getUid());
            final AccountProfileInfo info = GsonUtils.convertToT(result.getProfileInfo(), AccountProfileInfo.class);
            mCommonManager.insertOrReplace(UserDataCode.SETTING_GET_USERINFO, GsonUtils.toJson(info));
            BehaviorPackage.getInstance().setLoginInfo();
        }
        for (AccountCallBack observer : mCallBacks.values()) {
            observer.notifyQRCodeLoginConfirm(errCode, taskId, result);
        }
    }

    @Override
    public void notifyAccountProfile(final int errCode, final int taskId, final AccountUserInfo result) {
        for (AccountCallBack observer : mCallBacks.values()) {
            observer.notifyAccountProfile(errCode, taskId, result);
        }
    }

    @Override
    public void notifyAvatar(final int errCode, final int taskId, final AccountUserInfo result) {
        for (AccountCallBack observer : mCallBacks.values()) {
            observer.notifyAvatar(errCode, taskId, result);
        }
    }

    @Override
    public void notifyAccountLogout(final int errCode, final int taskId, final AccountUserInfo result) {

        if (result != null && result.getCode() == 1) {
            Logger.i("AccountModel", "退出登录成功");
            // 退出登录后，清除数据库中的用户信息
            mCommonManager.deleteValue(UserDataCode.SETTING_GET_USERINFO);
            mIsLogin = false;
            MsgPushPackage.getInstance().stopListen();
            BehaviorPackage.getInstance().setLoginInfo();
            mHistoryManager.deleteValueByKey(2);
        }

        for (AccountCallBack observer : mCallBacks.values()) {
            observer.notifyAccountLogout(errCode, taskId, result);
        }
    }

    @Override
    public void notifyAccountUnRegister(final int errCode, final int taskId, final AccountUserInfo result) {
        for (AccountCallBack observer : mCallBacks.values()) {
            observer.notifyAccountUnRegister(errCode, taskId, result);
        }
    }

    /**
     * 注册回调
     * @param key 回调key
     * @param callback 回调
     */
    public synchronized void registerCallBack(final String key, final AccountCallBack callback) {
        if (callback != null && !mCallBacks.contains(callback)) {
            mCallBacks.put(key,callback);
        }
    }

    /**
     * 发送验证码
     * @param mobileNum 手机号
     * @return 结果值
     */
    public int verificationCodeRequest(final String mobileNum) {
        return mAccountAdapter.verificationCodeRequest(mobileNum);
    }

    /**
     * 手机号登录
     * @param codeInput 验证码
     * @param mobileInput 手机号
     * @return 结果值
     */
    public int mobileLoginRequest(final String codeInput, final String mobileInput) {
        return mAccountAdapter.mobileLoginRequest(codeInput, mobileInput);
    }

    /**
     * 二维码登录
     * @param qrType 二维码类型
     * @return 结果值
     */
    public int qrcodeloginrequest(final int qrType) {
        return mAccountAdapter.qrCodeLoginRequest(qrType);
    }

    /**
     * 退出登录
     * @return 结果值
     */
    public int accountLogoutRequest() {
        return mAccountAdapter.accountLogoutRequest();
    }

    public static AccountPackage getInstance() {
        return Helper.EP;
    }

    private static final class Helper {
        private static final AccountPackage EP = new AccountPackage();
    }
}
