package com.sgm.navi.service.logicpaket.user.account;

import android.accounts.Account;
import android.content.Context;
import android.content.Intent;
import android.text.TextUtils;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.adapter.user.account.AccountAdapter;
import com.sgm.navi.service.adapter.user.account.AccountAdapterCallBack;
import com.sgm.navi.service.define.code.UserDataCode;
import com.sgm.navi.service.define.position.LocInfoBean;
import com.sgm.navi.service.define.user.account.AccessTokenParam;
import com.sgm.navi.service.define.user.account.AccountProfileInfo;
import com.sgm.navi.service.define.user.account.AccountUserInfo;
import com.sgm.navi.service.define.user.msgpush.MsgCarInfo;
import com.sgm.navi.service.greendao.CommonManager;
import com.sgm.navi.service.greendao.history.HistoryManager;
import com.sgm.navi.service.logicpaket.position.IPositionPackageCallback;
import com.sgm.navi.service.logicpaket.position.PositionPackage;
import com.sgm.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.sgm.navi.service.logicpaket.user.msgpush.MsgPushPackage;

import java.util.concurrent.ConcurrentHashMap;


public final class AccountPackage implements AccountAdapterCallBack {
    private final AccountAdapter mAccountAdapter;
    private final ConcurrentHashMap<String, AccountCallBack> mCallBacks;
    private final CommonManager mCommonManager;
    private boolean mIsLogin = false; // 判断当前用户是否已登录。  默认false，未登录
    private final HistoryManager mHistoryManager;

    private AccountPackage() {
        mCallBacks = new ConcurrentHashMap<>();
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
        mAccountAdapter.refreshUserInfo();
        AccountProfileInfo info = new AccountProfileInfo();
        final String valueJson = mCommonManager.getValueByKey(UserDataCode.SETTING_GET_USERINFO);
        Logger.i("getUserInfo valueJson = " + valueJson);
        if (!TextUtils.isEmpty(valueJson)) {
           info = GsonUtils.fromJson(valueJson, AccountProfileInfo.class);
            if (info != null) {
                if (!TextUtils.isEmpty(info.getUid())) {
                    mIsLogin = true;
                    Logger.i("getUserInfo mIsLogin = true");
                    PositionPackage.getInstance().registerCallBack(mIPositionPackageCallback);
                } else {
                    mIsLogin = false;
                    Logger.i("getUserInfo mIsLogin = false");
                    PositionPackage.getInstance().unregisterCallBack(mIPositionPackageCallback);
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
            Logger.i("notifyMobileLogin mIsLogin = true");
            PositionPackage.getInstance().registerCallBack(mIPositionPackageCallback);
            final AccountProfileInfo info = GsonUtils.convertToT(result.getProfileInfo(), AccountProfileInfo.class);
            mCommonManager.insertUserInfo(UserDataCode.SETTING_GET_USERINFO, GsonUtils.toJson(info));
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

    IPositionPackageCallback mIPositionPackageCallback = new IPositionPackageCallback() {
        @Override
        public void onLocationInfo(LocInfoBean locationInfo) {
            MsgCarInfo msgCarInfo = new MsgCarInfo();
            msgCarInfo.naviLocInfo.lon = locationInfo.getLongitude();
            msgCarInfo.naviLocInfo.lat = locationInfo.getLatitude();
            MsgPushPackage.getInstance().sendReqWsTserviceInternalLinkAutoReport(msgCarInfo);
        }
    };

    @Override
    public void notifyQRCodeLoginConfirm(final int errCode, final int taskId, final AccountUserInfo result) {
        if (result != null && result.getCode() == 1) {
            mIsLogin = true;
            Logger.i("notifyQRCodeLoginConfirm mIsLogin = true",
                    "result.getProfileInfo().getUid() = " + result.getProfileInfo().getUid());
            final AccountProfileInfo info = GsonUtils.convertToT(result.getProfileInfo(), AccountProfileInfo.class);
            mCommonManager.insertUserInfo(UserDataCode.SETTING_GET_USERINFO, GsonUtils.toJson(info));
            PositionPackage.getInstance().registerCallBack(mIPositionPackageCallback);
            MsgPushPackage.getInstance().startListen(result.getProfileInfo().getUid());
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
            Logger.i("notifyAccountLogout mIsLogin = false");
            PositionPackage.getInstance().unregisterCallBack(mIPositionPackageCallback);
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
    public void registerCallBack(final String key, final AccountCallBack callback) {
        if (callback != null && !mCallBacks.contains(callback)) {
            mCallBacks.put(key,callback);
        }
    }

    public void unRegisterCallBack(final String key) {
        mCallBacks.remove(key);
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
     * 获取账户accessToken
     * @param param 详细说明见AccessTokenParam
     * @return accessToken 获取失败返回空串
     */
    public String getAccessToken(final AccessTokenParam param) {
        return mAccountAdapter.getAccessToken(param);
    }

    /**
     * 获取SGM账户UserId
     * @return userId
     */
    public String getUserId() {
        Account[] accounts = getAccounts();
        if (accounts != null && accounts.length > 0) {
            return mAccountAdapter.getUserData(accounts[0], AutoMapConstant.AccountInfoKey.IDP_USER_ID);
        }
        return "";
    }

    /**
     *获取SGM账户Nickname
     * @return nickname
     */
    public String getNickname() {
        Account[] accounts = getAccounts();
        if (accounts != null && accounts.length > 0) {
            return mAccountAdapter.getUserData(accounts[0], AutoMapConstant.AccountInfoKey.NICK_NAME);
        }
        return "";
    }

    /**
     * 获取SGM账户手机号码
     * @return phone
     */
    public String getPhone() {
        Account[] accounts = getAccounts();
        if (accounts != null && accounts.length > 0) {
            return mAccountAdapter.getUserData(accounts[0], AutoMapConstant.AccountInfoKey.PHONE);
        }
        return "";
    }

    /**
     * 判断SGM账号是否登录
     * @return 是否登录
     */
    public boolean isSGMLogin() {
        return !TextUtils.isEmpty(getUserId());
    }

    /**
     * SGM请求登录
     */
    public void sendSGMLoginRequest(Context context) {
        final Intent intent = new Intent(AutoMapConstant.AccountLogin.ACTION);
        intent.setPackage(AutoMapConstant.AccountLogin.PACKAGE);
        context.startService(intent);
    }

    /**
     * 获取SGM账户列表
     * @return Account[]
     */
    public Account[] getAccounts() {
        return mAccountAdapter.getAccounts();
    }

    /**
     * 退出登录
     * @return 结果值
     */
    public int accountLogoutRequest() {
        return mAccountAdapter.accountLogoutRequest();
    }


    /**
     * 存储UUID
     * @param uuid
     */
    public void saveUuid(String uuid) {
        mCommonManager.insertOrReplace(UserDataCode.SETTING_ACCOUNT_UUID, uuid);
    }

    /**
     * 获取UUID
     * @return uuid
     */
    public String getUuid() {
        return mCommonManager.getValueByKey(UserDataCode.SETTING_ACCOUNT_UUID);
    }

    /**
     * 存储app key
     * @param key
     */
    public void saveAppKey(String key) {
        mCommonManager.insertOrReplace(UserDataCode.SETTING_ACCOUNT_APP_KEY, key);
    }

    /**
     * 获取app key
     * @return
     */
    public String getAppKey() {
        return mCommonManager.getValueByKey(UserDataCode.SETTING_ACCOUNT_APP_KEY);
    }

    public static AccountPackage getInstance() {
        return Helper.EP;
    }

    private static final class Helper {
        private static final AccountPackage EP = new AccountPackage();
    }
}
