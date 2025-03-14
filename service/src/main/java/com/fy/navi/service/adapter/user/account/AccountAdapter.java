package com.fy.navi.service.adapter.user.account;

import com.fy.navi.service.AdapterConfig;

import java.util.Objects;

/**
 * @Description
 * @Author fh
 * @date 2024/12/18
 */
public class AccountAdapter {
    private static final String CLASS_API_PKG = Objects.requireNonNull(AccountAdapter.class.getPackage()).getName();
    private static final String CLASS_API_NAME = "AccountAdapterImpl";
    private IAccountApi mAccountApi;

    private AccountAdapter() {
        mAccountApi = (IAccountApi) AdapterConfig.getObject(CLASS_API_PKG, CLASS_API_NAME);
    }

    public void initAccountService() {
        mAccountApi.initAccountService();
    }

    public int verificationCodeRequest(String mobileNum) {
        return mAccountApi.verificationCodeRequest(mobileNum);
    }

    public int accountCheckRequest() {
        return mAccountApi.accountCheckRequest();
    }

    public int accountRegisterRequest(String codeInput, String mobileInput) {
        return mAccountApi.accountRegisterRequest(codeInput, mobileInput);
    }

    public int mobileLoginRequest(String codeInput, String mobileInput) {
        return mAccountApi.mobileLoginRequest(codeInput, mobileInput);
    }

    public int qRCodeLoginRequest(int qrType) {
        return mAccountApi.qRCodeLoginRequest(qrType);
    }

    public int qRCodeLoginConfirmRequest(String qrCodeId) {
        return mAccountApi.qRCodeLoginConfirmRequest(qrCodeId);
    }

    public int accountProfileRequest() {
        return mAccountApi.accountProfileRequest();
    }

    public int avatarRequest() {
        return mAccountApi.avatarRequest();
    }

    public int accountLogoutRequest() {
        return mAccountApi.accountLogoutRequest();
    }

    public int accountUnRegisterRequest() {
        return mAccountApi.accountUnRegisterRequest();
    }

    public void registerCallBack(String key, AccountAdapterCallBack callBack) {
        mAccountApi.registerCallBack(key, callBack);
    }

    public void removeCallBack(String key) {
        mAccountApi.unRegisterCallback(key);
    }

    public void unInitAccountService() {
        mAccountApi.unInitAccountService();
    }

    public static AccountAdapter getInstance() {
        return Helper.ra;
    }

    private static final class Helper {
        private static final AccountAdapter ra = new AccountAdapter();
    }
}
