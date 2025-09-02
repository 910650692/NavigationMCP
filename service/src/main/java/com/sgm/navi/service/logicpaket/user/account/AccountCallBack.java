package com.sgm.navi.service.logicpaket.user.account;

import com.sgm.navi.service.define.user.account.AccountUserInfo;
import com.sgm.navi.service.define.user.account.CarCheckBindInfo;


public interface AccountCallBack {
    /**
     * 获取验证码回调通知
     *
     * @param errCode 错误码
     * @param taskId  task id
     * @param result  获取验证码返回结果数据
     */
    default void notifyVerificationCode(int errCode, int taskId, AccountUserInfo result) {

    }

    /**
     * 检测账户是否存在回调通知
     *
     * @param errCode 错误码
     * @param taskId  task id
     * @param result  检测账户是否存在回调通知
     */
    default void notifyAccountCheck(int errCode, int taskId, AccountUserInfo result) {

    }

    /**
     * 注册账户回调通知
     *
     * @param errCode 错误码
     * @param taskId  task id
     * @param result  注册账户返回结果数据
     */
    default void notifyAccountRegister(int errCode, int taskId, AccountUserInfo result) {

    }

    /**
     * 通过手机验证码登录回调通知
     *
     * @param errCode 错误码
     * @param taskId  task id
     * @param result  通过手机验证码登录返回结果数据
     */
    default void notifyMobileLogin(int errCode, int taskId, AccountUserInfo result) {

    }

    /**
     * 获取扫码登录二维码回调通知
     *
     * @param errCode 错误码
     * @param taskId  task id
     * @param result  获取扫码登录二维码返回结果数据
     */
    default void notifyQRCodeLogin(int errCode, int taskId, AccountUserInfo result) {

    }

    /**
     * 长轮询是否扫码登录回调通知
     *
     * @param errCode 错误码
     * @param taskId  task id
     * @param result  长轮询是否扫码登录返回结果数据
     */
    default void notifyQRCodeLoginConfirm(int errCode, int taskId, AccountUserInfo result) {

    }

    /**
     * 获取用户信息回调通知
     *
     * @param errCode 错误码
     * @param taskId  task id
     * @param result  获取用户信息返回结果数据
     */
    default void notifyAccountProfile(int errCode, int taskId, AccountUserInfo result) {

    }

    /**
     * 获取头像数据回调通知
     *
     * @param errCode 错误码
     * @param taskId  task id
     * @param result  获取头像数据返回结果数据
     */
    default void notifyAvatar(int errCode, int taskId, AccountUserInfo result) {

    }

    /**
     * 注销登录回调通知
     *
     * @param errCode 错误码
     * @param taskId  task id
     * @param result  注销登录返回结果数据
     */
    default void notifyAccountLogout(int errCode, int taskId, AccountUserInfo result) {

    }

    /**
     * 注销账户回调通知
     *
     * @param errCode 错误码
     * @param taskId  task id
     * @param result  注销账户返回结果数据
     */
    default void notifyAccountUnRegister(int errCode, int taskId, AccountUserInfo result) {

    }

    /**
     * 检查车企账号绑定状态回调通知
     * @param errCode 错误码
     * @param taskId task id
     * @param result 绑定状态回调结果数据
     */
    default void notifyCarltdCheckBindResult(int errCode, int taskId, CarCheckBindInfo result) {

    }

    /**
     * 车企账号绑定返回结果处理
     * @param errCode 错误码
     * @param taskId task id
     * @param result 车企账号绑定回调结果数据
     */
    default void notifyCarltdBindResult(int errCode, int taskId, CarCheckBindInfo result) {

    }

    /**
     * 车企账号登录返回结果处理
     * @param errCode 错误码
     * @param taskId task id
     * @param result 车企账号登录回调结果数据
     */
    default void notifyCarltdLoginResult(int errCode, int taskId, CarCheckBindInfo result) {

    }

    /**
     * 车企账号Token检查返回结果处理
     * @param errCode 错误码
     * @param taskId task id
     * @param result 车企账号Token检查回调结果数据
     */
    default void notifyCarltdCheckTokenResult(int errCode, int taskId, CarCheckBindInfo result) {

    }

    /**
     * 车企账号快速登录返回结果处理
     * @param errCode 错误码
     * @param taskId task id
     * @param result 车企账号快速登录回调结果数据
     */
    default void notifyCarltdQLoginResult(int errCode, int taskId, CarCheckBindInfo result) {

    }

    /**
     * 车企账号解绑回调结果处理
     * @param errCode 错误码
     * @param taskId task id
     * @param result 车企账号解绑回调结果数据
     */
    default void notifyCarltdUnBindResult(int errCode, int taskId, CarCheckBindInfo result) {

    }

    /**
     * 车企账号信息回调结果处理
     * @param errCode 错误码
     * @param taskId task id
     * @param result 车企账号信息回调结果数据
     */
    default void notifyCarltdAuthInfoResult(int errCode, int taskId, CarCheckBindInfo result) {

    }
}