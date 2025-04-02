package com.fy.navi.service.adapter.user.account;

import com.fy.navi.service.define.user.account.AccountUserInfo;


public interface AccountAdapterCallBack {

    /**
     * 获取验证码回调通知
     * @param errCode 错误码
     * @param taskId task id
     * @param result 获取验证码返回结果数据
     */
    void notifyVerificationCode(int errCode, int taskId, AccountUserInfo result);

    /**
     * 检测账户是否存在回调通知
     * @param errCode 错误码
     * @param taskId task id
     * @param result 检测账户是否存在回调通知
     */
    void notifyAccountCheck(int errCode, int taskId, AccountUserInfo result);

    /**
     * 注册账户回调通知
     * @param errCode 错误码
     * @param taskId task id
     * @param result 注册账户返回结果数据
     */
    void notifyAccountRegister(int errCode, int taskId, AccountUserInfo result);

    /**
     * 通过手机验证码登录回调通知
     * @param errCode 错误码
     * @param taskId task id
     * @param result 通过手机验证码登录返回结果数据
     */
    void notifyMobileLogin(int errCode, int taskId, AccountUserInfo result);

    /**
     * 获取扫码登录二维码回调通知
     * @param errCode 错误码
     * @param taskId task id
     * @param result 获取扫码登录二维码返回结果数据
     */
    void notifyQRCodeLogin(int errCode, int taskId, AccountUserInfo result);

    /**
     * 长轮询是否扫码登录回调通知
     * @param errCode 错误码
     * @param taskId task id
     * @param result 长轮询是否扫码登录返回结果数据
     */
    void notifyQRCodeLoginConfirm(int errCode, int taskId, AccountUserInfo result);

    /**
     * 获取用户信息回调通知
     * @param errCode 错误码
     * @param taskId task id
     * @param result 获取用户信息返回结果数据
     */
    void notifyAccountProfile(int errCode, int taskId, AccountUserInfo result);

    /**
     * 获取头像数据回调通知
     * @param errCode 错误码
     * @param taskId task id
     * @param result 获取头像数据返回结果数据
     */
    void notifyAvatar(int errCode, int taskId, AccountUserInfo result);

    /**
     * 注销登录回调通知
     * @param errCode 错误码
     * @param taskId task id
     * @param result 注销登录返回结果数据
     */
    void notifyAccountLogout(int errCode, int taskId, AccountUserInfo result);

    /**
     * 注销账户回调通知
     * @param errCode 错误码
     * @param taskId task id
     * @param result 注销账户返回结果数据
     */
    void notifyAccountUnRegister(int errCode, int taskId, AccountUserInfo result);
}
