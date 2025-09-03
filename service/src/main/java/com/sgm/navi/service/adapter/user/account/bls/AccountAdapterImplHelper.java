package com.sgm.navi.service.adapter.user.account.bls;

import android.accounts.Account;
import android.accounts.AccountManager;
import android.accounts.AccountManagerFuture;
import android.accounts.AuthenticatorException;
import android.accounts.OperationCanceledException;
import android.os.Bundle;
import android.text.TextUtils;

import com.android.utils.ConvertUtils;
import com.android.utils.file.FileUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.aosclient.BLAosServiceManager;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.user.account.AccountService;
import com.autonavi.gbl.user.account.model.AccountCheckResult;
import com.autonavi.gbl.user.account.model.AccountLogoutResult;
import com.autonavi.gbl.user.account.model.AccountProfile;
import com.autonavi.gbl.user.account.model.AccountProfileResult;
import com.autonavi.gbl.user.account.model.AccountProvider;
import com.autonavi.gbl.user.account.model.AccountRegisterResult;
import com.autonavi.gbl.user.account.model.AccountRequestType;
import com.autonavi.gbl.user.account.model.AccountServiceParam;
import com.autonavi.gbl.user.account.model.AccountUnRegisterResult;
import com.autonavi.gbl.user.account.model.AvatarResult;
import com.autonavi.gbl.user.account.model.CarltdAuthInfoRequest;
import com.autonavi.gbl.user.account.model.CarltdAuthInfoResult;
import com.autonavi.gbl.user.account.model.CarltdBindRequest;
import com.autonavi.gbl.user.account.model.CarltdBindResult;
import com.autonavi.gbl.user.account.model.CarltdCheckBindRequest;
import com.autonavi.gbl.user.account.model.CarltdCheckBindResult;
import com.autonavi.gbl.user.account.model.CarltdCheckTokenRequest;
import com.autonavi.gbl.user.account.model.CarltdCheckTokenResult;
import com.autonavi.gbl.user.account.model.CarltdLoginRequest;
import com.autonavi.gbl.user.account.model.CarltdLoginResult;
import com.autonavi.gbl.user.account.model.CarltdQLoginRequest;
import com.autonavi.gbl.user.account.model.CarltdQLoginResult;
import com.autonavi.gbl.user.account.model.CarltdUnBindRequest;
import com.autonavi.gbl.user.account.model.CarltdUnBindResult;
import com.autonavi.gbl.user.account.model.MobileLoginResult;
import com.autonavi.gbl.user.account.model.QRCodeLoginConfirmRequest;
import com.autonavi.gbl.user.account.model.QRCodeLoginConfirmResult;
import com.autonavi.gbl.user.account.model.QRCodeLoginResult;
import com.autonavi.gbl.user.account.model.UserCar;
import com.autonavi.gbl.user.account.model.VerificationCodeResult;
import com.autonavi.gbl.user.account.observer.IAccountServiceObserver;
import com.autonavi.gbl.util.model.ServiceInitStatus;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.GBLCacheFilePath;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.user.account.AccountAdapterCallBack;
import com.sgm.navi.service.define.code.UserDataCode;
import com.sgm.navi.service.define.user.account.AccessTokenParam;
import com.sgm.navi.service.define.user.account.AccountProfileCarBind;
import com.sgm.navi.service.define.user.account.AccountProfileInfo;
import com.sgm.navi.service.define.user.account.AccountProviderCarBind;
import com.sgm.navi.service.define.user.account.AccountUserInfo;
import com.sgm.navi.service.define.user.account.CarCheckBindInfo;
import com.sgm.navi.service.define.user.account.CarCheckBindResult;
import com.sgm.navi.service.define.user.account.UserCarBindInfo;
import com.sgm.navi.service.greendao.CommonManager;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;


public class AccountAdapterImplHelper implements IAccountServiceObserver {
    private static final String TAG = MapDefaultFinalTag.ACCOUNT_SERVICE_TAG;
    private AccountService mAccountService;
    private BLAosServiceManager mBlAosServiceManager;
    private final AccountManager mAccountManager;
    private final Hashtable<String, AccountAdapterCallBack> mAccountResultHashtable;
    private String mEmail;
    private final CommonManager mCommonManager;

    protected AccountAdapterImplHelper(final AccountService accountService) {
        mAccountResultHashtable = new Hashtable<>();
        mAccountService = accountService;
        mAccountManager = AccountManager.get(AppCache.getInstance().getMContext());
        mCommonManager = CommonManager.getInstance();
        mCommonManager.init();
    }

    protected void initAccountService() {
        if (null == mAccountService)
            mAccountService = (AccountService) ServiceMgr.getServiceMgrInstance()
                    .getBLService(SingleServiceID.AccountSingleServiceID);
        if (null == mBlAosServiceManager)
            mBlAosServiceManager = (BLAosServiceManager) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.AosSingleServiceID);

        // 构造初始化参数
        final AccountServiceParam param = new AccountServiceParam();
        param.dataPath = GBLCacheFilePath.ACCOUNT_PATH;
        // 保证传入目录存在
        FileUtils.getInstance().createDir(param.dataPath);
        //FileUtils.createDIR(param.dataPath);
        // 服务初始化
        final int res = mAccountService.init(param);
        Logger.i(TAG, "initAccountService: initSetting=", res);
        // 添加回调观察者
        mAccountService.addObserver(this);
        refreshUserInfo();
    }

    /**
     * 获取绑定状态请求
     *
     * @param sourceInput   高德分配网络请求源ID
     * @param authInput     车企账号ID，必传
     * @return 接口调用结果
     */
    public int carCheckBindRequest(String sourceInput,String authInput){
        CarltdCheckBindRequest carltdCheckBindRequest = new CarltdCheckBindRequest();
        carltdCheckBindRequest.reqType = AccountRequestType.AccountTypeCarltdCheckBind;
        carltdCheckBindRequest.sourceId = sourceInput; // 高德分配网络请求源ID
        carltdCheckBindRequest.authId = authInput; // 车企账号ID，必传
        int res = mAccountService.executeRequest(carltdCheckBindRequest);
        Logger.d(TAG, "carltdCheckBindRequest: result = " + res,sourceInput,authInput);
        return res;
    }

    /**
     * 车企账号绑定
     *
     * @param sourceInput   高德分配网络请求源ID
     * @param authInput     车企账号ID，必传
     * @param deviceInput   唯一设备ID
     * @return 接口调用结果
     */
    public int carBindRequest(String sourceInput,String authInput,String deviceInput){
        CarltdBindRequest carltdBindRequest = new CarltdBindRequest();
        carltdBindRequest.reqType = AccountRequestType.AccountTypeCarltdBind;
        carltdBindRequest.sourceId = sourceInput; // 高德分配网络请求源ID
        carltdBindRequest.authId = authInput; // 车企账号ID，必传
        carltdBindRequest.deviceCode = deviceInput; // 唯一设备ID
        int res = mAccountService.executeRequest(carltdBindRequest);
        Logger.d(TAG, "carBindRequest: result = " + res,sourceInput,authInput,deviceInput);
        return res;
    }

    /**
     * 车企账号快速登录
     *
     * @param sourceInput   高德分配网络请求源ID
     * @param authInput     车企账号ID，必传
     * @param userId        高德账号ID，必传
     * @return 接口调用结果
     */
    public int carLoginRequest(String sourceInput,String authInput,String userId){
        CarltdLoginRequest carltdLoginRequest = new CarltdLoginRequest();
        carltdLoginRequest.reqType = AccountRequestType.AccountTypeCarltdLogin;
        carltdLoginRequest.sourceId = sourceInput; // 高德分配网络请求源ID
        carltdLoginRequest.authId = authInput; // 车企账号ID，必传
        carltdLoginRequest.userId = userId; // 高德账号ID，必传
        int res = mAccountService.executeRequest(carltdLoginRequest);
        Logger.d(TAG, "carLoginRequest: result = " + res,sourceInput,authInput,userId);
        return res;
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
    public int carCheckTokenRequest(String sourceInput, String token, String authInput, String deviceCode){
        CarltdCheckTokenRequest carltdCheckTokenRequest = new CarltdCheckTokenRequest();
        carltdCheckTokenRequest.reqType = AccountRequestType.AccountTypeCarltdCheckToken;
        carltdCheckTokenRequest.sourceId = sourceInput; // 高德分配网络请求源ID
        carltdCheckTokenRequest.token = token; // 访问车企服务token
        carltdCheckTokenRequest.authId = authInput; // 车企账号ID，必传
        carltdCheckTokenRequest.deviceCode = deviceCode; // 车机设备唯一标识
        int res = mAccountService.executeRequest(carltdCheckTokenRequest);
        Logger.d(TAG, "carCheckTokenRequest: result = " + res,sourceInput,token,authInput,deviceCode);
        return res;
    }

    /**
     * 车企账号快速登录
     *
     * @param sourceInput   高德分配网络请求源ID
     * @param authInput     车企账号ID，必传
     * @return 接口调用结果
     */
    public int carQLoginRequest(String sourceInput, String authInput){
        CarltdQLoginRequest carltdCheckTokenRequest = new CarltdQLoginRequest();
        carltdCheckTokenRequest.reqType = AccountRequestType.AccountTypeCarltdQLogin;
        carltdCheckTokenRequest.sourceId = sourceInput; // 高德分配网络请求源ID
        carltdCheckTokenRequest.authId = authInput; // 车企账号ID，必传
        int res = mAccountService.executeRequest(carltdCheckTokenRequest);
        Logger.d(TAG, "carQLoginRequest: result = " + res,sourceInput,authInput);
        return res;
    }

    /**
     * 车企帐号解绑请求
     *
     * @param sourceInput   高德分配网络请求源ID
     * @param authInput     车企账号ID，必传
     * @param deviceCode    车机设备唯一标识
     * @return 接口调用结果
     */
    public int carUnBindRequest(String sourceInput, String authInput, String deviceCode){
        CarltdUnBindRequest carltdUnBindRequest = new CarltdUnBindRequest();
        carltdUnBindRequest.reqType = AccountRequestType.AccountTypeCarltdUnBind;
        carltdUnBindRequest.sourceId = sourceInput; // 高德分配网络请求源ID
        carltdUnBindRequest.authId = authInput; // 车企账号ID，必传
        carltdUnBindRequest.deviceCode = deviceCode; // 车机设备唯一标识
        int res = mAccountService.executeRequest(carltdUnBindRequest);
        Logger.d(TAG, "carUnBindRequest: result = " + res,sourceInput,authInput);
        return res;
    }

    /**
     * 车企账号信息请求
     *
     * @param sourceInput   高德分配网络请求源ID
     * @param uid
     * @return 接口调用结果
     */
    public int carAuthInfoRequest(String sourceInput, String uid){
        CarltdAuthInfoRequest carltdAuthInfoRequest = new CarltdAuthInfoRequest();
        carltdAuthInfoRequest.sourceId = sourceInput;// 高德分配网络请求源ID，必传
        carltdAuthInfoRequest.uid = uid;
        int res = mAccountService.executeRequest(carltdAuthInfoRequest);
        Logger.d(TAG, "carAuthInfoRequest: result = " + res,sourceInput,uid);
        return res;
    }

    /**
     * 通过SDK刷新用户信息
     */
    public void refreshUserInfo() {
        if (mAccountService == null ||
                ServiceInitStatus.ServiceInitDone != mAccountService.isInit()) {
            Logger.i(TAG, "refreshUserInfo: mAccountService is not OK");
            return;
        }
        if (mAccountService.getUserData() == null || TextUtils.isEmpty(mAccountService.getUserData().uid)) {
            mCommonManager.insertUserInfo(UserDataCode.SETTING_GET_USERINFO,
                    GsonUtils.toJson(convertAccountProfile(new AccountProfile())));
        } else {
            mCommonManager.insertUserInfo(UserDataCode.SETTING_GET_USERINFO,
                    GsonUtils.toJson(convertAccountProfile(mAccountService.getUserData())));
        }
    }

    /**
     * 注册监听
     * @param key 注册的key
     * @param callBack 回调
     */
    public void registerCallBack(final String key, final AccountAdapterCallBack callBack) {
        mAccountResultHashtable.put(key, callBack);
    }

    /**
     * 注销监听
     * @param key 注册的key
     */
    public void unRegisterCallBack(final String key) {
        mAccountResultHashtable.remove(key);
    }

    /**
     * 清除监听
     */
    public void removeCallback() {
        mAccountResultHashtable.clear();
    }

    /**
     * 获取验证码请求回调
     * @param errCode 错误码
     * @param taskId 任务ID
     * @param result 验证码结果
     */
    @Override
    public void notify(final int errCode, final int taskId, final VerificationCodeResult result) {
        notifyAccountRequestSuccess(errCode, taskId, result);
    }

    /**
     * 检查账号是否存在回调
     * @param errCode 错误码
     * @param taskId 任务ID
     * @param result 检查结果
     */
    @Override
    public void notify(final int errCode, final int taskId, final AccountCheckResult result) {
        notifyAccountRequestSuccess(errCode, taskId, result);
    }

    /**
     * 异步注册结果处理
     * @param errCode 错误码
     * @param taskId 任务ID
     * @param result 检查结果
     */
    @Override
    public void notify(final int errCode, final int taskId, final AccountRegisterResult result) {

        notifyAccountRequestSuccess(errCode, taskId, result);
    }

    /**
     * 手机号登录结果回调
     * @param errCode 错误码
     * @param taskId 任务ID
     * @param result 检查结果
     */
    @Override
    public void notify(final int errCode, final int taskId, final MobileLoginResult result) {
        if (result != null && result.code == 1) {
            Logger.i(TAG, "MobileLoginResult notify: res=" , result.code);
            Logger.i(TAG, "userId=" + result.profile.uid);
            Logger.i("状态:" + result.profile.username + "已登录," + "id = " + result.profile.uid);
        } else {
            Logger.w(TAG, "登录失败:" + errCode);
        }
        notifyAccountRequestSuccess(errCode,taskId,result);
    }
    /**
     * 获取二维码回调
     * @param errCode 错误码
     * @param taskId 任务ID
     * @param result 检查结果
     */
    @Override
    public void notify(final int errCode, final int taskId, final QRCodeLoginResult result) {
        Logger.i(TAG, "QRCodeLoginResult notify: res=" + (result == null ? -9527 : result.code) + "; taskId=" + taskId);

        notifyAccountRequestSuccess(errCode,taskId,result);

        // 长轮询是否扫码
        final QRCodeLoginConfirmRequest req = new QRCodeLoginConfirmRequest();
        if (result == null) {
            return;
        }
        req.qrcodeId = result.qrcode.id;
        mAccountService.executeRequest(req);
    }

    /**
     * 获取用户信息回调
     * @param errCode 错误码
     * @param taskId 任务ID
     * @param result 检查结果
     */
    @Override
    public void notify(final int errCode, final int taskId, final AccountProfileResult result) {

        notifyAccountRequestSuccess(errCode,taskId,result);
    }

    /**
     * 头像获取请求异步回调处理
     * @param errCode 错误码
     * @param taskId 任务ID
     * @param result 检查结果
     */
    @Override
    public void notify(final int errCode, final int taskId, final AvatarResult result) {
        notifyAccountRequestSuccess(errCode,taskId,result);
    }

    /**
     * 退出登录回调
     * @param errCode 错误码
     * @param taskId 任务ID
     * @param result 检查结果
     */
    @Override
    public void notify(final int errCode, final int taskId, final AccountLogoutResult result) {
        if (result != null && result.code == 1) {
            Logger.i(TAG, "AccountLogoutResult: 已登出 res=" + result.code + "; taskId=" + taskId);
            mBlAosServiceManager.setUid("");
        } else {
            Logger.i(TAG, "AccountLogoutResult: 登出失败 ", result.code);
        }
        notifyAccountRequestSuccess(errCode,taskId,result);
    }

    /**
     * 账号注销
     * @param errCode 错误码
     * @param taskId 任务ID
     * @param result 检查结果
     */
    @Override
    public void notify(final int errCode, final int taskId, final AccountUnRegisterResult result) {

        notifyAccountRequestSuccess(errCode,taskId,result);
    }

    /**
     * 是否手机二维码扫码登陆
     * @param errCode 错误码
     * @param taskId 任务ID
     * @param result 检查结果
     */
    @Override
    public void notify(final int errCode, final int taskId, final QRCodeLoginConfirmResult result) {
        if (result != null) {
            if (Logger.openLog) {
                Logger.i(TAG, "QRCodeLoginConfirmResult notify: res=", result);
            }
        }
        notifyAccountRequestSuccess(errCode,taskId,result);
    }

    /**
     * 检查车企账号绑定状态回调通知
     * @param errCode 错误码
     * @param taskId task id
     * @param result 绑定状态回调结果数据
     */
    @Override
    public void notify(int errCode, int taskId, CarltdCheckBindResult result) {
        if (result != null) {
            if (Logger.openLog) {
                Logger.i(TAG, "CarltdCheckBindResult notify: res=", result.result,result.data.hasBind);
            }
        }
        notifyAccountRequestSuccess(errCode,taskId,result);
    }

    /**
     * 车企账号绑定返回结果处理
     * @param errCode 错误码
     * @param taskId task id
     * @param result 车企账号绑定回调结果数据
     */
    @Override
    public void notify(int errCode, int taskId, CarltdBindResult result) {
        if (result != null) {
            if (Logger.openLog) {
                Logger.i(TAG, "CarltdBindResult notify: res=", result.result);
            }
        }
        notifyAccountRequestSuccess(errCode,taskId,result);
    }

    /**
     * 车企账号登录返回结果处理
     * @param errCode 错误码
     * @param taskId task id
     * @param result 车企账号登录回调结果数据
     */
    @Override
    public void notify(int errCode, int taskId, CarltdLoginResult result) {
        if (result != null) {
            if (Logger.openLog) {
                Logger.i(TAG, "CarltdLoginResult notify: res=", result.result,result.data.uid,
                        result.data.nickname);
            }
        }
        notifyAccountRequestSuccess(errCode,taskId,result);
    }

    /**
     * 车企账号Token检查返回结果处理
     * @param errCode 错误码
     * @param taskId task id
     * @param result 车企账号Token检查回调结果数据
     */
    @Override
    public void notify(int errCode, int taskId, CarltdCheckTokenResult result) {
        if (result != null) {
            if (Logger.openLog) {
                Logger.i(TAG, "CarltdCheckTokenResult notify: res=", result.result,result.authId,
                        result.mobile,result.profile.uid,result.profile.nickname);
            }
        }
        notifyAccountRequestSuccess(errCode,taskId,result);
    }

    /**
     * 车企账号快速登录返回结果处理
     * @param errCode 错误码
     * @param taskId task id
     * @param result 车企账号快速登录回调结果数据
     */
    @Override
    public void notify(int errCode, int taskId, CarltdQLoginResult result) {
        if (result != null) {
            if (Logger.openLog) {
                Logger.i(TAG, "CarltdQLoginResult notify: res=", result.result,result.userId,
                        result.token);
            }
        }
        notifyAccountRequestSuccess(errCode,taskId,result);
    }

    /**
     * 车企账号解绑回调结果处理
     * @param errCode 错误码
     * @param taskId task id
     * @param result 车企账号解绑回调结果数据
     */
    @Override
    public void notify(int errCode, int taskId, CarltdUnBindResult result) {
        if (result != null) {
            if (Logger.openLog) {
                Logger.i(TAG, "CarltdQLoginResult notify: res=", result.result);
            }
        }
        notifyAccountRequestSuccess(errCode,taskId,result);
    }

    /**
     * 车企账号信息回调结果处理
     * @param errCode 错误码
     * @param taskId task id
     * @param result 车企账号信息回调结果数据
     */
    @Override
    public void notify(int errCode, int taskId, CarltdAuthInfoResult result) {
        if (result != null) {
            if (Logger.openLog) {
                Logger.i(TAG, "CarltdAuthInfoResult notify: res=", result.result);
            }
        }
        notifyAccountRequestSuccess(errCode,taskId,result);
    }

    /***
     * 账号请求相关结果统一处理.
     * @param errCode 错误码
     * @param taskId 任务ID
     * @param result  请求结果
     * @param <T> GBL 请求结构类型
     */
    public <T> void notifyAccountRequestSuccess(final int errCode, final int taskId, final T result) {
        if (ConvertUtils.isEmpty(mAccountResultHashtable)) {
            return;
        }
        for (AccountAdapterCallBack callBack : mAccountResultHashtable.values()) {
            if (callBack == null) {
                continue;
            }
            if (result instanceof VerificationCodeResult) {
                callBack.notifyVerificationCode(errCode, taskId, convertVerificationCodeInfo((VerificationCodeResult)result));
            } else if (result instanceof AccountCheckResult) {
                AccountCheckResult resultTemp = (AccountCheckResult) result;
                callBack.notifyAccountCheck(errCode, taskId, getAccountBaseInfo(resultTemp.code, resultTemp.result,
                        resultTemp.message, resultTemp.timestamp,resultTemp.version));
            } else if (result instanceof AccountRegisterResult) {
                callBack.notifyAccountRegister(errCode, taskId, convertAccountUserInfo((AccountRegisterResult)result));
            } else if (result instanceof MobileLoginResult) {
                callBack.notifyMobileLogin(errCode, taskId, convertMobileLoginInfo((MobileLoginResult)result));
            } else if (result instanceof QRCodeLoginResult) {
                callBack.notifyQRCodeLogin(errCode, taskId, convertQRCodeLoginInfo((QRCodeLoginResult)result));
            } else if (result instanceof QRCodeLoginConfirmResult) {
                callBack.notifyQRCodeLoginConfirm(errCode, taskId, convertQRCodeLoginConfirmInfo((QRCodeLoginConfirmResult)result));
            }else if (result instanceof AccountProfileResult) {
                callBack.notifyAccountProfile(errCode, taskId, convertAccountProfileInfo((AccountProfileResult)result));
            } else if (result instanceof AvatarResult) {
                callBack.notifyAvatar(errCode, taskId, convertAvatarInfo((AvatarResult)result));
            } else if (result instanceof AccountLogoutResult) {
                callBack.notifyAccountLogout(errCode, taskId, convertAccountLogoutInfo((AccountLogoutResult)result));
            } else if (result instanceof AccountUnRegisterResult) {
                callBack.notifyAccountUnRegister(errCode, taskId, convertAccountUnRegisterInfo((AccountUnRegisterResult)result));
            } else if (result instanceof CarltdCheckBindResult) {
                callBack.notifyCarltdCheckBindResult(errCode, taskId, convertCarltdCheckBindInfo((CarltdCheckBindResult)result));
            } else if (result instanceof CarltdBindResult) {
                CarltdCheckBindResult resultTemp = (CarltdCheckBindResult)result;
                callBack.notifyCarltdBindResult(errCode, taskId, getCarltdCheckBindInfo(resultTemp.code,resultTemp.result,
                        resultTemp.message,resultTemp.timestamp,resultTemp.version));
            } else if (result instanceof CarltdLoginResult) {
                callBack.notifyCarltdLoginResult(errCode, taskId, convertCarltdLoginInfo((CarltdLoginResult)result));
            } else if (result instanceof CarltdCheckTokenResult) {
                callBack.notifyCarltdCheckTokenResult(errCode, taskId, convertCarltdCheckTokenInfo((CarltdCheckTokenResult)result));
            } else if (result instanceof CarltdQLoginResult) {
                callBack.notifyCarltdQLoginResult(errCode, taskId, convertCarltdQLoginInfo((CarltdQLoginResult)result));
            } else if (result instanceof CarltdUnBindResult) {
                CarltdUnBindResult resultTemp = (CarltdUnBindResult)result;
                callBack.notifyCarltdUnBindResult(errCode, taskId, getCarltdCheckBindInfo(resultTemp.code,resultTemp.result,
                        resultTemp.message,resultTemp.timestamp,resultTemp.version));
            } else if (result instanceof CarltdAuthInfoResult) {
                callBack.notifyCarltdAuthInfoResult(errCode, taskId, convertCarltdAuthInfo((CarltdAuthInfoResult)result));
            }
        }
    }

    /**
     * 获取基本信息
     * @param code 错误码
     * @param result 结果
     * @param message 消息
     * @param timestamp 时间戳
     * @param version 版本
     * @return 基本信息
     */
    private AccountUserInfo getAccountBaseInfo(final int code, final String result,
                                               final String message, final String timestamp, final String version) {
        if (result == null) {
            return null;
        }
        final AccountUserInfo info = new AccountUserInfo();
        info.setCode(code);
        info.setResult(result);
        info.setMessage(message);
        info.setTimestamp(timestamp);
        info.setVersion(version);
        return info;
    }

    /**
     * 转换验证码信息
     * @param result 验证码结果
     * @return 验证码信息
     */
    private AccountUserInfo convertVerificationCodeInfo(final VerificationCodeResult result) {
        if (result == null) {
            return null;
        }
        final AccountUserInfo info = getAccountBaseInfo(result.code,result.result,result.message,result.timestamp,result.version);
        info.setStatus(result.status);
        return info;
    }

    /**
     * 转换注册信息
     * @param result 注册结果
     * @return 注册信息
     */
    private AccountUserInfo convertAccountUserInfo(final AccountRegisterResult result) {
        if (result == null) {
            return null;
        }
        final AccountUserInfo info = getAccountBaseInfo(result.code,result.result,result.message,result.timestamp,result.version);
        if (result.profile != null) {
            mEmail = result.profile.email;
            info.setProfileInfo(convertAccountProfileInfo(result.profile.uid, result.profile.username,result.profile.nickname,
                    result.profile.avatar,result.profile.mobile));
        }
        return info;
    }

    /**
     * 转换SDK的用户信息
     * @param result 登录结果
     * @return 登录信息
     */
    private AccountUserInfo convertMobileLoginInfo(final MobileLoginResult result) {
        if (result == null) {
            return null;
        }
        final AccountUserInfo info = getAccountBaseInfo(result.code,result.result,result.message,result.timestamp,result.version);
        if (result.profile != null) {
            mEmail = result.profile.email;
            info.setProfileInfo(convertAccountProfileInfo(result.profile.uid, result.profile.username,result.profile.nickname,
                    result.profile.avatar,result.profile.mobile));
        }
        return info;
    }

    /**
     * 转换SDK的用户信息
     * @param result 登录结果
     * @return 登录信息
     */
    private AccountProfileInfo convertAccountProfile(final AccountProfile result) {
        if (result != null) {
            Logger.i(TAG, "convertAccountProfile: nickname=", result.nickname);
            mEmail = result.email;
            return convertAccountProfileInfo(result.uid, result.username, result.nickname,
                    result.avatar, result.mobile);
        }
        return new AccountProfileInfo();
    }

    /**
     * 转换登录二维码信息
     * @param result 登录结果
     * @return 登录二维码信息
     */
    private AccountUserInfo convertQRCodeLoginInfo(final QRCodeLoginResult result) {
        if (result == null) {
            return null;
        }
        final AccountUserInfo info = getAccountBaseInfo(result.code,result.result,result.message,result.timestamp,result.version);
        if (result.qrcode != null) {
            info.setId(result.qrcode.id);
            info.setTimeout(result.qrcode.timeout);
            if (result.qrcode.data != null) {
                info.setBuffer(result.qrcode.data.buffer);
            }
        }
        return info;
    }

    /**
     * 转换扫码登录结果
     * @param result 扫码登录结果
     * @return 扫码登录结果
     */
    private AccountUserInfo convertQRCodeLoginConfirmInfo(final QRCodeLoginConfirmResult result) {
        if (result == null) {
            return null;
        }
        final AccountUserInfo info = getAccountBaseInfo(result.code,result.result,result.message,result.timestamp,result.version);
        if (result.profile != null) {
            mEmail = result.profile.email;
            info.setProfileInfo(convertAccountProfileInfo(result.profile.uid, result.profile.username,result.profile.nickname,
                    result.profile.avatar,result.profile.mobile));
        }
        return info;
    }

    /**
     * 转换用户信息
     * @param result 获取用户信息结果
     * @return 用户信息
     */
    private AccountUserInfo convertAccountProfileInfo(final AccountProfileResult result) {
        if (result == null) {
            return null;
        }
        final AccountUserInfo info = getAccountBaseInfo(result.code,result.result,result.message,result.timestamp,result.version);
        if (result.profile != null) {
            mEmail = result.profile.email;
            info.setProfileInfo(convertAccountProfileInfo(result.profile.uid, result.profile.username,result.profile.nickname,
                    result.profile.avatar,result.profile.mobile));
        }
        return info;
    }

    /**
     * 转换用户信息
     * @param uid 用户ID
     * @param username 用户名
     * @param nickname 昵称
     * @param avatar 头像
     * @param mobile 手机号
     * @return 用户信息
     */
    private AccountProfileInfo convertAccountProfileInfo(final String uid, final String username, final String nickname,
                                                         final String avatar, final String mobile) {
        final AccountProfileInfo info = new AccountProfileInfo();
        info.setUid(uid);
        info.setUsername(username);
        info.setNickname(nickname);
        info.setAvatar(avatar);
        info.setMobile(mobile);
        info.setEmail(mEmail);
        return info;
    }

    /**
     * 转换头像信息
     * @param result 头像结果
     * @return 头像信息
     */
    private AccountUserInfo convertAvatarInfo(final AvatarResult result) {
        if (result == null) {
            return null;
        }
        final AccountUserInfo info = getAccountBaseInfo(result.code,result.result,result.message,result.timestamp,result.version);
        if (result.data != null) {
            info.setBuffer(result.data.buffer);
        }
        return info;
    }

    /**
     * 转换退出登录信息
     * @param result 退出登录结果
     * @return 退出登录信息
     */
    private AccountUserInfo convertAccountLogoutInfo(final AccountLogoutResult result) {
        if (result == null) {
            return null;
        }
        return getAccountBaseInfo(result.code,result.result,result.message,result.timestamp,result.version);
    }

    /**
     * 转换注销信息
     * @param result 注销结果
     * @return 注销信息
     */
    private AccountUserInfo convertAccountUnRegisterInfo(final AccountUnRegisterResult result) {
        if (result == null) {
            return null;
        }
        final AccountUserInfo info = getAccountBaseInfo(result.code,result.result,result.message,result.timestamp,result.version);
        if (result.data != null) {
            info.setRemain(result.data.remain);
            info.setMobile(result.data.mobile);
            info.setReason(result.data.reason);
        }
        return info;
    }

    /**
     * 获取绑定结果基本信息
     * @param code 错误码
     * @param result 结果
     * @param message 消息
     * @param timestamp 时间戳
     * @param version 版本
     * @return 基本信息
     */
    private CarCheckBindInfo getCarltdCheckBindInfo(final int code, final String result,
                                                    final String message, final String timestamp, final String version) {
        if (result == null) {
            return null;
        }
        CarCheckBindInfo info = new CarCheckBindInfo();
        info.setCode(code);
        info.setResult(result);
        info.setMessage(message);
        info.setTimestamp(timestamp);
        info.setVersion(version);
        return info;
    }

    /**
     * 转换车企账号绑定返回结果
     * @param result 绑定结果
     * @return 帐号绑定信息
     */
    private CarCheckBindInfo convertCarltdCheckBindInfo(final CarltdCheckBindResult result) {
        if (result == null) {
            return null;
        }
        final CarCheckBindInfo info = getCarltdCheckBindInfo(result.code,result.result,
                result.message,result.timestamp,result.version);
        CarCheckBindResult carCheckBindResult = new CarCheckBindResult();
        carCheckBindResult.setHasBind(result.data.hasBind);
        info.setCarCheckBindResult(carCheckBindResult);
        return info;
    }

    /**
     * 转换车企账号登录返回结果
     * @param result 登录结果
     * @return 帐号登录信息
     */
    private CarCheckBindInfo convertCarltdLoginInfo(final CarltdLoginResult result) {
        if (result == null) {
            return null;
        }
        final CarCheckBindInfo info = getCarltdCheckBindInfo(result.code,result.result,
                result.message,result.timestamp,result.version);
        if (result.data != null) {
            mEmail = result.data.email;
            info.setAccountProfileInfo(convertAccountProfileInfo(result.data.uid, result.data.username,result.data.nickname,
                    result.data.avatar,result.data.mobile));
        }
        return info;
    }

    /**
     * 转换Token检查返回结果
     * @param result Token检查结果
     * @return Token检查信息
     */
    private CarCheckBindInfo convertCarltdCheckTokenInfo(final CarltdCheckTokenResult result) {
        if (result == null) {
            return null;
        }
        final CarCheckBindInfo info = getCarltdCheckBindInfo(result.code, result.result,
                result.message, result.timestamp, result.version);
        AccountProfile accountProfile = result.profile;
        if (accountProfile != null) {
            mEmail = accountProfile.email;
            info.setAccountProfileInfo(convertAccountProfileInfo(accountProfile.uid,
                    accountProfile.username, accountProfile.nickname,
                    accountProfile.avatar, accountProfile.mobile));
            ArrayList<AccountProviderCarBind> accountProviderCarBinds = new ArrayList<>();
            ArrayList<AccountProvider> accountProviders = accountProfile.providers;
            if (!ConvertUtils.isEmpty(accountProviders)) {
                for (AccountProvider provider : accountProviders) {
                    accountProviderCarBinds.add(new AccountProviderCarBind(provider.provider,
                            provider.auth_id, provider.auth_username));
                }
            }
            UserCar userCar = accountProfile.car;
            UserCarBindInfo userCarBindInfo = new UserCarBindInfo(userCar.model, userCar.device,
                    userCar.manufacture);

            AccountProfileCarBind accountProfileCarBind = new AccountProfileCarBind(accountProfile.level,
                    accountProfile.gender, accountProfile.checkin_count, accountProfile.birthday,
                    accountProfile.description, accountProviderCarBinds, accountProfile.carLoginFlag,
                    userCarBindInfo);
            info.setAccountProfileCarBind(accountProfileCarBind);
        }
        return info;
    }

    /**
     * 转换车企账号快速登录返回结果
     * @param result 车企账号快速登录检查结果
     * @return 快速登录信息
     */
    private CarCheckBindInfo convertCarltdQLoginInfo(final CarltdQLoginResult result) {
        if (result == null) {
            return null;
        }
        final CarCheckBindInfo info = getCarltdCheckBindInfo(result.code,result.result,
                result.message,result.timestamp,result.version);
        info.setUserId(result.userId);
        info.setToken(result.token);
        return info;
    }

    /**
     * 转换车企账号信息返回结果
     * @param result 车企账号信息检查结果
     * @return 车企账号信息
     */
    private CarCheckBindInfo convertCarltdAuthInfo(final CarltdAuthInfoResult result) {
        if (result == null) {
            return null;
        }
        final CarCheckBindInfo info = getCarltdCheckBindInfo(result.code,result.result,
                result.message,result.timestamp,result.version);
        info.setAuthId(result.authId);
        info.setToken(result.token);
        info.setMobileCode(result.mobileCode);
        info.setDeviceCode(result.deviceCode);
        return info;
    }

    /**
     * 校验账号数据服务状态.
     */
    protected void checkoutAccountServer() {
        if (ServiceInitStatus.ServiceNotInit == mAccountService.isInit()) {
            initAccountService();
        }
    }

    /**
     * 获取账户accessToken
     * @param param 详细说明见AccessTokenParam
     * @return accessToken 获取失败返回空串
     */
    public String getAccessToken(final AccessTokenParam param) {
        Logger.d(TAG, "getAccessToken :");
        String authToken = "";
        if (ConvertUtils.isEmpty(mAccountManager)) {
            Logger.d(TAG, "AccountManager 为空");
            return "";
        }
        Account[] accounts = mAccountManager.getAccountsByType(AutoMapConstant.AccountTokenParamType.ACCOUNT_TYPE_PATAC_HMI);
        Logger.d(TAG, "accounts number :" , accounts.length);
        if (accounts.length == 0) {
            Logger.d(TAG, "无可用账户，将触发添加账户界面");
        }
        try {
            final AccountManagerFuture<Bundle> future =
//                    mAccountManager.getAuthTokenByFeatures(
//                            param.getMAccountType(),
//                            param.getMAuthTokenType(),
//                            param.getMFeatures(),
//                            param.getMActivity(),
//                            param.getMAddAccountOption(),
//                            param.getMGetAuthTokenOption(),
//                            param.getMCallback(),
//                            param.getMHandler()
//                    );
                    mAccountManager.getAuthToken(
                            accounts[0],
                            param.getMAuthTokenType(),
                            null,
                            param.getMActivity(),
                            null,
                            null
                    );
            Logger.d(TAG, "getAccessToken : future get 1");
            final Bundle bnd = future.getResult();
            Logger.d(TAG, "getAccessToken : future get 2");
            authToken = bnd.getString(AccountManager.KEY_AUTHTOKEN);
            Logger.i(TAG, "authToken : " + authToken);
        } catch (OperationCanceledException | AuthenticatorException | IOException e) {
            Logger.e(TAG, e.getMessage());
        }
        return authToken;
    }

    /**
     * 获取账户信息
     * @param availableAccount 账户对象
     * @param key 账户信息的key值 ;  AutoMapConstant.AccountInfoKey
     * @return userId
     */
    public String getUserData(final Account availableAccount, final String key) {
        if (ConvertUtils.isEmpty(mAccountManager)) {
            Logger.d(TAG, "mAccountManager 为空");
            return "";
        }
        return mAccountManager.getUserData(availableAccount, key);
    }

    /**
     * 获取账户列表
     * @return Account[]
     */
    public Account[] getAccounts() {
        return mAccountManager.getAccountsByType(AutoMapConstant.AccountInfoKey.ACCOUNT);
    }

}
