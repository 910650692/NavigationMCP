package com.fy.navi.service.adapter.user.account.bls;

import android.accounts.Account;
import android.accounts.AccountManager;
import android.accounts.AccountManagerFuture;
import android.accounts.AuthenticatorException;
import android.accounts.OperationCanceledException;
import android.os.Bundle;

import com.android.utils.ConvertUtils;
import com.android.utils.file.FileUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.user.account.AccountService;
import com.autonavi.gbl.user.account.model.AccountCheckResult;
import com.autonavi.gbl.user.account.model.AccountLogoutResult;
import com.autonavi.gbl.user.account.model.AccountProfileResult;
import com.autonavi.gbl.user.account.model.AccountRegisterResult;
import com.autonavi.gbl.user.account.model.AccountServiceParam;
import com.autonavi.gbl.user.account.model.AccountUnRegisterResult;
import com.autonavi.gbl.user.account.model.AvatarResult;
import com.autonavi.gbl.user.account.model.MobileLoginResult;
import com.autonavi.gbl.user.account.model.QRCodeLoginConfirmRequest;
import com.autonavi.gbl.user.account.model.QRCodeLoginConfirmResult;
import com.autonavi.gbl.user.account.model.QRCodeLoginResult;
import com.autonavi.gbl.user.account.model.VerificationCodeResult;
import com.autonavi.gbl.user.account.observer.IAccountServiceObserver;
import com.autonavi.gbl.util.model.ServiceInitStatus;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.user.account.AccountAdapterCallBack;
import com.fy.navi.service.define.user.account.AccessTokenParam;
import com.fy.navi.service.define.user.account.AccountProfileInfo;
import com.fy.navi.service.define.user.account.AccountUserInfo;

import java.io.IOException;
import java.util.Hashtable;


public class AccountAdapterImplHelper implements IAccountServiceObserver {
    private static final String TAG = MapDefaultFinalTag.ACCOUNT_SERVICE_TAG;
    private final AccountService mAccountService;
    private final AccountManager mAccountManager;
    private final Hashtable<String, AccountAdapterCallBack> mAccountResultHashtable;
    private String mEmail;

    protected AccountAdapterImplHelper(final AccountService accountService) {
        mAccountResultHashtable = new Hashtable<>();
        mAccountService = accountService;
        mAccountManager = AccountManager.get(AppContext.getInstance().getMContext());
    }

    protected void initAccountService() {
        // 构造初始化参数
        final AccountServiceParam param = new AccountServiceParam();
        param.dataPath = GBLCacheFilePath.ACCOUNT_PATH;
        // 保证传入目录存在
        FileUtils.getInstance().createDir(param.dataPath);
        //FileUtils.createDIR(param.dataPath);
        // 服务初始化
        final int res = mAccountService.init(param);
        Logger.i(TAG, "initAccountService: initSetting=" + res);
        // 添加回调观察者
        mAccountService.addObserver(this);
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
            Logger.i(TAG, "MobileLoginResult notify: res=" + result.code);
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
            Logger.i(TAG, "AccountLogoutResult notify: res=" + result.code + "; taskId=" + taskId);
            Logger.i(TAG, "状态: 已登出 ");
        } else {
            Logger.i(TAG, "登出失败 ");
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
     * 是否扫码登陆
     * @param errCode 错误码
     * @param taskId 任务ID
     * @param result 检查结果
     */
    @Override
    public void notify(final int errCode, final int taskId, final QRCodeLoginConfirmResult result) {
        if (result != null) {
            Logger.i(TAG, "QRCodeLoginConfirmResult notify: res=" + GsonUtils.toJson(result));
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
                callBack.notifyAccountCheck(errCode, taskId, getAccountBaseInfo(((AccountCheckResult) result).code,
                        ((AccountCheckResult) result).result, ((AccountCheckResult) result).message,
                        ((AccountCheckResult) result).timestamp,((AccountCheckResult) result).version));
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
     * 转换登录信息
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
     * @return accessToken 获取失败返回空串, "-1"为无效值需要稍后重试
     */
    public String getAccessToken(final AccessTokenParam param) {
        String authTokenToken = "";
        try {
            final AccountManagerFuture<Bundle> future =
                    mAccountManager.getAuthTokenByFeatures(
                            param.getMAccountType(),
                            param.getMAuthTokenType(),
                            param.getMFeatures(),
                            param.getMActivity(),
                            param.getMAddAccountOption(),
                            param.getMGetAuthTokenOption(),
                            param.getMCallback(),
                            param.getMHandler());
            final Bundle bnd = future.getResult();
            authTokenToken = bnd.getString(AccountManager.KEY_AUTHTOKEN);
            Logger.i(TAG, "authTokenToken : " + authTokenToken);
        } catch (OperationCanceledException | AuthenticatorException | IOException e) {
            Logger.e(TAG, e.getMessage());
        }
        if (ConvertUtils.equals(authTokenToken, "-1")) {
            Logger.e(TAG, "authTokenToken 无效值，请稍后再试");
        }
        return authTokenToken;
    }

    /**
     * 获取idpUserId
     * @param availableAccount 账户对象
     * @param key 账户信息的key值 ; id 用 AutoMapConstant.AccountInfoKey.IDP_USER_ID
     * @return userId
     */
    public String getIdpUserId(final Account availableAccount, final String key) {
        return mAccountManager.getUserData(availableAccount, key);
    }

}
