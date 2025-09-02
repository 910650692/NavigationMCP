package com.sgm.navi.service.adapter.user.account;

import android.accounts.Account;

import com.sgm.navi.service.AdapterConfig;
import com.sgm.navi.service.define.user.account.AccessTokenParam;

import java.util.Objects;


public final class AccountAdapter {
    private static final String CLASS_API_PKG = Objects.requireNonNull(AccountAdapter.class.getPackage()).getName();
    private static final String CLASS_API_NAME = "AccountAdapterImpl";
    private final IAccountApi mAccountApi;

    private AccountAdapter() {
        mAccountApi = (IAccountApi) AdapterConfig.getObject(CLASS_API_PKG, CLASS_API_NAME);
    }

    /**
     * 初始化账户服务
     */
    public void initAccountService() {
        mAccountApi.initAccountService();
    }

    /**
     * 获取绑定状态请求
     *
     * @param sourceInput   高德分配网络请求源ID
     * @param authInput     车企账号ID，必传
     * @return 接口调用结果
     */
    public int carCheckBindRequest(String sourceInput,String authInput) {
        return mAccountApi.carCheckBindRequest(sourceInput,authInput);
    }

    /**
     * 车企账号绑定
     *
     * @param sourceInput   高德分配网络请求源ID
     * @param authInput     车企账号ID，必传
     * @param deviceInput   唯一设备ID
     * @return 接口调用结果
     */
    public int carBindRequest(String sourceInput,String authInput,String deviceInput) {
        return mAccountApi.carBindRequest(sourceInput,authInput,deviceInput);
    }

    /**
     * 车企账号快速登录
     *
     * @param sourceInput   高德分配网络请求源ID
     * @param authInput     车企账号ID，必传
     * @param userId        高德账号ID，必传
     * @return 接口调用结果
     */
    public int carLoginRequest(String sourceInput,String authInput,String userId) {
        return mAccountApi.carLoginRequest(sourceInput,authInput,userId);
    }

    /**
     * 车企账号token检查
     *
     * @param sourceInput   高德分配网络请求源ID
     * @param token         访问车企服务token
     * @param authInput     车企账号ID，必传
     * @param deviceCode    车机设备唯一标识
     * @return 接口调用结果
     */
    public int carCheckTokenRequest(String sourceInput, String token, String authInput, String deviceCode) {
        return mAccountApi.carCheckTokenRequest(sourceInput,token,authInput,deviceCode);
    }

    /**
     * 车企账号快速登录
     *
     * @param sourceInput   高德分配网络请求源ID
     * @param authInput     车企账号ID，必传
     * @return 接口调用结果
     */
    public int carQLoginRequest(String sourceInput, String authInput) {
        return mAccountApi.carQLoginRequest(sourceInput,authInput);
    }

    /**
     * 车企帐号解绑请求
     *
     * @param sourceInput   高德分配网络请求源ID
     * @param authInput     车企账号ID，必传
     * @param deviceCode    车机设备唯一标识
     * @return 接口调用结果
     */
    public int carUnBindRequest(String sourceInput,String authInput,String deviceCode) {
        return mAccountApi.carUnBindRequest(sourceInput,authInput,deviceCode);
    }

    /**
     * 车企账号信息请求
     *
     * @param sourceInput   高德分配网络请求源ID
     * @param uid
     * @return 接口调用结果
     */
    public int carAuthInfoRequest(String sourceInput,String uid) {
        return mAccountApi.carAuthInfoRequest(sourceInput,uid);
    }

    /**
     * 从SDK获取用户信息到本地
     */
    public void refreshUserInfo() {
        mAccountApi.refreshUserInfo();
    }

    /**
     * 获取手机验证码
     * @param mobileNum 手机号
     * @return 错误码 0：成功 其他：失败
     */
    public int verificationCodeRequest(final String mobileNum) {
        return mAccountApi.verificationCodeRequest(mobileNum);
    }

    /**
     * 请求检测账户是否存在
     * @return 错误码 0：成功 其他：失败
     */
    public int accountCheckRequest() {
        return mAccountApi.accountCheckRequest();
    }

    /**
     * 请求注册账户
     * @param codeInput 验证码
     * @param mobileInput 手机号
     * @return 错误码 0：成功 其他：失败
     */
    public int accountRegisterRequest(final String codeInput, final String mobileInput) {
        return mAccountApi.accountRegisterRequest(codeInput, mobileInput);
    }

    /**
     * 手机验证码登录
     * @param codeInput 验证码
     * @param mobileInput 手机号
     * @return 错误码 0：成功 其他：失败
     */
    public int mobileLoginRequest(final String codeInput, final String mobileInput) {
        return mAccountApi.mobileLoginRequest(codeInput, mobileInput);
    }

    /**
     * 请求获取扫码登录二维码
     * @param qrType 二维码类型
     * @return 错误码 0：成功 其他：失败
     */
    public int qrCodeLoginRequest(final int qrType) {
        return mAccountApi.qrCodeLoginRequest(qrType);
    }

    /**
     * 请求扫码登录确认
     * @param qrCodeId 二维码id
     * @return 错误码 0：成功 其他：失败
     */
    public int qrCodeLoginConfirmRequest(final String qrCodeId) {
        return mAccountApi.qrCodeLoginConfirmRequest(qrCodeId);
    }

    /**
     * 请求获取用户信息
     * @return 错误码 0：成功 其他：失败
     */
    public int accountProfileRequest() {
        return mAccountApi.accountProfileRequest();
    }

    /**
     * 请求获取头像数据
     * @return 错误码 0：成功 其他：失败
     */
    public int avatarRequest() {
        return mAccountApi.avatarRequest();
    }

    /**
     * 请求注销登录
     * @return 错误码 0：成功 其他：失败
     */
    public int accountLogoutRequest() {
        return mAccountApi.accountLogoutRequest();
    }

    /**
     * 请求注销账户
     * @return 错误码 0：成功 其他：失败
     */
    public int accountUnRegisterRequest() {
        return mAccountApi.accountUnRegisterRequest();
    }

    /**
     * 获取账户accessToken
     * @param param 详细说明见AccessTokenParam
     * @return accessToken 获取失败返回空串
     */
    public String getAccessToken(final AccessTokenParam param) {
        return mAccountApi.getAccessToken(param);
    }

    /**
     * 获取idpUserId
     * @param availableAccount 账户对象
     * @param key 账户信息的key值 ; id 用 AutoMapConstant.AccountInfoKey.IDP_USER_ID
     * @return userId
     */
    public String getUserData(final Account availableAccount, final String key) {
        return mAccountApi.getUserData(availableAccount, key);
    }

    /**
     * 获取账户列表
     * @return Account[]
     */
    public Account[] getAccounts() {
        return mAccountApi.getAccounts();
    }

    /**
     * 注册回调
     * @param key 回调key
     * @param callBack 回调
     */
    public void registerCallBack(final String key, final AccountAdapterCallBack callBack) {
        mAccountApi.registerCallBack(key, callBack);
    }

    /**
     * 注销回调
     * @param key 回调key
     */
    public void removeCallBack(final String key) {
        mAccountApi.unRegisterCallback(key);
    }

    /**
     * 注销账户服务
     */
    public void unInitAccountService() {
        mAccountApi.unInitAccountService();
    }

    public static AccountAdapter getInstance() {
        return Helper.RA;
    }

    private static final class Helper {
        private static final AccountAdapter RA = new AccountAdapter();
    }
}
