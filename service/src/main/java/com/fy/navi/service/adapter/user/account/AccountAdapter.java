package com.fy.navi.service.adapter.user.account;

import com.fy.navi.service.AdapterConfig;

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
