package com.fy.navi.service.adapter.user.account;

/**
 * @Description TODO
 * @Author fh
 * @date 2024/12/09
 */
public interface IAccountApi {

    void initAccountService();

    void registerCallBack(String key, AccountAdapterCallBack callBack);

    void unRegisterCallback(String key);

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
     * @return 错误码 0：成功 其他：失败
     */
    int qRCodeLoginRequest(int qrType);

    int qRCodeLoginConfirmRequest(String qrCodeId);

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

}
