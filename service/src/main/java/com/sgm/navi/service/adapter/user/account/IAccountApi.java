package com.sgm.navi.service.adapter.user.account;


import android.accounts.Account;

import com.sgm.navi.service.define.user.account.AccessTokenParam;

public interface IAccountApi {

    /**
     * 初始化账户服务
     */
    void initAccountService();

    /**
     * 获取绑定状态请求
     *
     * @param sourceInput   高德分配网络请求源ID
     * @param authInput     车企账号ID，必传
     * @return 接口调用结果
     */
    int carCheckBindRequest(String sourceInput,String authInput);

    /**
     * 车企账号绑定
     *
     * @param sourceInput   高德分配网络请求源ID
     * @param authInput     车企账号ID，必传
     * @param deviceInput   唯一设备ID
     * @return 接口调用结果
     */
    int carBindRequest(String sourceInput,String authInput,String deviceInput);

    /**
     * 车企账号快速登录
     *
     * @param sourceInput   高德分配网络请求源ID
     * @param authInput     车企账号ID，必传
     * @param userId        高德账号ID，必传
     * @return 接口调用结果
     */
    int carLoginRequest(String sourceInput,String authInput,String userId);

    /**
     * 车企账号token检查
     *
     * @param sourceInput   高德分配网络请求源ID
     * @param token         访问车企服务token
     * @param authInput     车企账号ID，必传
     * @param deviceCode    车机设备唯一标识
     * @return 接口调用结果
     */
    int carCheckTokenRequest(String sourceInput, String token, String authInput, String deviceCode);

    /**
     * 车企账号快速登录
     *
     * @param sourceInput   高德分配网络请求源ID
     * @param authInput     车企账号ID，必传
     * @return 接口调用结果
     */
    int carQLoginRequest(String sourceInput, String authInput);

    /**
     * 车企帐号解绑请求
     *
     * @param sourceInput   高德分配网络请求源ID
     * @param authInput     车企账号ID，必传
     * @param deviceCode    车机设备唯一标识
     * @return 接口调用结果
     */
    int carUnBindRequest(String sourceInput,String authInput,String deviceCode);

    /**
     * 车企账号信息请求
     *
     * @param sourceInput   高德分配网络请求源ID
     * @param uid
     * @return 接口调用结果
     */
    int carAuthInfoRequest(String sourceInput,String uid);

    /**
     * 从SDK获取用户信息到本地
     */
    void refreshUserInfo();

    /**
     * 注册回调
     * @param key 回调key
     * @param callBack 回调
     */
    void registerCallBack(String key, AccountAdapterCallBack callBack);

    /**
     * 注销回调
     * @param key 回调key
     */
    void unRegisterCallback(String key);

    /**
     * 注销账户服务
     */
    void unInitAccountService();

    /**
     * 获取手机验证码
     * @param mobileNum 手机号
     * @return 错误码 0：成功 其他：失败
     */
    int verificationCodeRequest(String mobileNum);

    /**
     * 请求检测账户是否存在
     * @return 错误码 0：成功 其他：失败
     */
    int accountCheckRequest();

    /**
     * 请求注册账户
     * @param codeInput 验证码
     * @param mobileInput 手机号
     * @return 错误码 0：成功 其他：失败
     */
    int accountRegisterRequest(String codeInput, String mobileInput);

    /**
     * 手机验证码登录
     * @param codeInput 验证码
     * @param mobileInput 手机号
     * @return 错误码 0：成功 其他：失败
     */
    int mobileLoginRequest(String codeInput, String mobileInput);

    /**
     * 请求获取扫码登录二维码
     * @param qrType 二维码类型
     * @return 错误码 0：成功 其他：失败
     */
    int qrCodeLoginRequest(int qrType);

    /**
     * 请求扫码登录确认
     * @param qrCodeId 二维码id
     * @return 错误码 0：成功 其他：失败
     */
    int qrCodeLoginConfirmRequest(String qrCodeId);

    /**
     * 请求获取用户信息
     * @return 错误码 0：成功 其他：失败
     */
    int accountProfileRequest();

    /**
     * 请求获取头像数据
     * @return 错误码 0：成功 其他：失败
     */
    int avatarRequest();

    /**
     * 请求注销登录
     * @return 错误码 0：成功 其他：失败
     */
    int accountLogoutRequest();

    /**
     * 请求注销账户
     * @return 错误码 0：成功 其他：失败
     */
    int accountUnRegisterRequest();

    /**
     * 获取账户accessToken
     * @param param 详细说明见AccessTokenParam
     * @return accessToken 获取失败返回空串
     */
    String getAccessToken(final AccessTokenParam param);

    /**
     * 获取账户信息
     * @param availableAccount 账户对象
     * @param key 账户信息的key值
     * @return userId
     */
    String getUserData(final Account availableAccount, final String key);

    /**
     * 获取账户列表
     * @return Account[]
     */
    Account[] getAccounts();

}
