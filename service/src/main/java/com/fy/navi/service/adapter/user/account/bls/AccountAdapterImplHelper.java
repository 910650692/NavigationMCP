package com.fy.navi.service.adapter.user.account.bls;

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
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.user.account.AccountAdapterCallBack;
import com.fy.navi.service.define.user.account.AccountProfileInfo;
import com.fy.navi.service.define.user.account.AccountUserInfo;

import java.util.Hashtable;

/**
 * AccountService辅助类.
 *
 * @Description Helper类只做对象及数据转换，不做原子能力调用
 * @Author fh
 * @date 2024/12/18
 */
public class AccountAdapterImplHelper implements IAccountServiceObserver {
    private static final String TAG = MapDefaultFinalTag.ACCOUNT_SERVICE_TAG;
    private final AccountService mAccountService;
    private final Hashtable<String, AccountAdapterCallBack> accountResultHashtable;

    protected AccountAdapterImplHelper(AccountService accountService) {
        accountResultHashtable = new Hashtable<>();
        mAccountService = accountService;
    }

    protected void initAccountService() {
        // 构造初始化参数
        AccountServiceParam param = new AccountServiceParam();
        param.dataPath = GBLCacheFilePath.ACCOUNT_PATH;
        // 保证传入目录存在
        FileUtils.getInstance().createDir(param.dataPath);
        //FileUtils.createDIR(param.dataPath);
        // 服务初始化
        int res = mAccountService.init(param);
        Logger.i(TAG, "initAccountService: initSetting=" + res);
        // 添加回调观察者
        mAccountService.addObserver(this);
    }

    public void registerCallBack(String key, AccountAdapterCallBack callBack) {
        accountResultHashtable.put(key, callBack);
    }

    public void unRegisterCallBack(String key) {
        accountResultHashtable.remove(key);
    }

    public void removeCallback() {
        accountResultHashtable.clear();
    }

    /**
     * 获取验证码请求回调
     * @param errCode
     * @param taskId
     * @param result
     */
    @Override
    public void notify(int errCode, int taskId, VerificationCodeResult result) {
        /*//HMI根据业务进行后续处理
        int resultCode = result == null ? AutoConstant.DEFAULT_ERR_CODE : result.code;
        boolean sendSuccess = errCode == Service.ErrorCodeOK && resultCode == 1;
        String msg = "发送验证码:" + (sendSuccess ? "成功" : "失败") + " " + errCode + "\n" + GsonUtils.toJson(result);
        Logger.d(TAG, msg);
        tv_msg.setText(msg);*/
        notifyAccountRequestSuccess(errCode, taskId, result);
    }

    /**
     * 检查账号是否存在回调
     * @param errCode
     * @param taskId
     * @param result
     */
    @Override
    public void notify(int errCode, int taskId, AccountCheckResult result) {
        notifyAccountRequestSuccess(errCode, taskId, result);
    }

    /**
     * 异步注册结果处理
     * @param errCode
     * @param taskId
     * @param result
     */
    @Override
    public void notify(int errCode, int taskId, AccountRegisterResult result) {
        /*if (result != null && result.code == 1) {
            Logger.i(TAG, "AccountRegisterResult notify: res=" + result.code + "; taskId=" + taskId);
            Logger.i("注册成功,状态: " + result.profile.username + "已登录");
        } else {
            Logger.i("注册失败");
        }*/
        notifyAccountRequestSuccess(errCode, taskId, result);
    }

    /**
     * 手机号登录结果回调
     * @param errCode
     * @param taskId
     * @param result
     */
    @Override
    public void notify(int errCode, int taskId, MobileLoginResult result) {
        if (result != null && result.code == 1) {
            Logger.i(TAG, "MobileLoginResult notify: res=" + result.code + "; taskId=" + taskId);
            Logger.i(TAG, "userId=" + result.profile.uid);
            Logger.i("状态:" + result.profile.username + "已登录," + "id = " + result.profile.uid);
        } else {
            Logger.w(TAG, "登录失败:" + errCode);
        }
        notifyAccountRequestSuccess(errCode,taskId,result);
    }
    /**
     * 获取二维码回调
     * @param errCode
     * @param taskId
     * @param result
     */
    @Override
    public void notify(int errCode, int taskId, QRCodeLoginResult result) {
        Logger.i(TAG, "QRCodeLoginResult notify: res=" + (result == null ? -9527 : result.code) + "; taskId=" + taskId);
       /* int resultCde = result == null ? 0 : result.code;
        QRCodeInfo qrcode = result == null ? null : result.qrcode;
        BinaryStream data = qrcode == null ? null : qrcode.data;
        byte[] buffer = data == null ? null : data.buffer;

        if (resultCde == 1 && buffer != null) {
            //将字节数组转换为ImageView可调用的Bitmap对象
            Logger.i(TAG, "生成二维码成功");
//            Bitmap myBitmap = getPicFromBytes(buffer, null);
//            tv_msg.setText("生成了二维码" + qrcode.id);
//            iv_qrcode.setImageBitmap(myBitmap);
        } else {
            Logger.i(TAG, "生成二维码失败");
//            tv_msg.setText("生成二维码失败");
        }*/
        notifyAccountRequestSuccess(errCode,taskId,result);

        // 长轮询是否扫码
        QRCodeLoginConfirmRequest req = new QRCodeLoginConfirmRequest();
        if (result == null) {
            return;
        }
        req.qrcodeId = result.qrcode.id;
        mAccountService.executeRequest(req);
    }

    /**
     * 获取用户信息回调
     * @param errCode
     * @param taskId
     * @param result
     */
    @Override
    public void notify(int errCode, int taskId, AccountProfileResult result) {
       /* Logger.i(TAG, "获取用户信息回调通知: " + errCode + "," + taskId);
        if (result != null && result.code == 1) {
            Logger.i(TAG, "AccountProfileResult notify: res=" + result.code + "; taskId=" + taskId);
//            tv_msg.setText("信息: 用户名=" + result.profile.username
//                    + "; ID=" + result.profile.uid
//                    + "; 昵称=" + result.profile.nickname
//                    + "; 头像URL" + result.profile.avatar
//            );
//            AutoConstant.userld = result.profile.uid;// 保存
//            AosManager. getInstance().setUid (AutoConstant.userId);

        } else {
            Logger.i(TAG, "获取用户信息失败 ");
        }*/
        notifyAccountRequestSuccess(errCode,taskId,result);
    }

    /**
     * 头像获取请求异步回调处理
     * @param errCode
     * @param taskId
     * @param result
     */
    @Override
    public void notify(int errCode, int taskId, AvatarResult result) {
        notifyAccountRequestSuccess(errCode,taskId,result);
    }

    /**
     * 退出登录回调
     * @param errCode
     * @param taskId
     * @param result
     */
    @Override
    public void notify(int errCode, int taskId, AccountLogoutResult result) {
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
     * @param errCode
     * @param taskId
     * @param result
     */
    @Override
    public void notify(int errCode, int taskId, AccountUnRegisterResult result) {
        // 回调结果处理
        if (result.code == 0) {
            // 删除对应同步库数据
            // mUserTrackService.deleteLocalSyncData(userId);
        }
        notifyAccountRequestSuccess(errCode,taskId,result);
    }

    /**
     * 是否扫码登陆
     * @param errCode
     * @param taskId
     * @param result
     */
    @Override
    public void notify(int errCode, int taskId, QRCodeLoginConfirmResult result) {
        if (result != null) {
            Logger.i(TAG, "QRCodeLoginConfirmResult notify: res=" + GsonUtils.toJson(result));
        }
        notifyAccountRequestSuccess(errCode,taskId,result);
    }

    /***
     * 账号请求相关结果统一处理.
     * @param result  请求结果
     * @param <T> GBL 请求结构类型
     */
    public <T> void notifyAccountRequestSuccess(int errCode, int taskId, T result) {
        if (ConvertUtils.isEmpty(accountResultHashtable)) return;
        for (AccountAdapterCallBack callBack : accountResultHashtable.values()) {
            if (callBack == null) continue;
            if (result instanceof VerificationCodeResult) {
                callBack.notifyVerificationCode(errCode, taskId, convertVerificationCodeInfo((VerificationCodeResult)result));
            } else if (result instanceof AccountCheckResult) {
                callBack.notifyAccountCheck(errCode, taskId, getAccountBaseInfo(((AccountCheckResult) result).code, ((AccountCheckResult) result).result,
                        ((AccountCheckResult) result).message,((AccountCheckResult) result).timestamp,((AccountCheckResult) result).version));
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

    private AccountUserInfo getAccountBaseInfo(int code, String result, String message, String timestamp, String version) {
        if (result == null) return null;
        AccountUserInfo info = new AccountUserInfo();
        info.setCode(code);
        info.setResult(result);
        info.setMessage(message);
        info.setTimestamp(timestamp);
        info.setVersion(version);
        return info;
    }

    private AccountUserInfo convertVerificationCodeInfo(VerificationCodeResult result) {
        if (result == null) return null;
        AccountUserInfo info = getAccountBaseInfo(result.code,result.result,result.message,result.timestamp,result.version);
        info.setStatus(result.status);
        return info;
    }

    private AccountUserInfo convertAccountUserInfo(AccountRegisterResult result) {
        if (result == null) return null;
        AccountUserInfo info = getAccountBaseInfo(result.code,result.result,result.message,result.timestamp,result.version);
        if (result.profile != null) {
            info.setProfileInfo(convertAccountProfileInfo(result.profile.uid, result.profile.username,result.profile.nickname,
                    result.profile.avatar,result.profile.mobile,result.profile.email));
        }
        return info;
    }

    private AccountUserInfo convertMobileLoginInfo(MobileLoginResult result) {
        if (result == null) return null;
        AccountUserInfo info = getAccountBaseInfo(result.code,result.result,result.message,result.timestamp,result.version);
        if (result.profile != null) {
            info.setProfileInfo(convertAccountProfileInfo(result.profile.uid, result.profile.username,result.profile.nickname,
                    result.profile.avatar,result.profile.mobile,result.profile.email));
        }
        return info;
    }

    private AccountUserInfo convertQRCodeLoginInfo(QRCodeLoginResult result) {
        if (result == null) return null;
        AccountUserInfo info = getAccountBaseInfo(result.code,result.result,result.message,result.timestamp,result.version);
        if (result.qrcode != null) {
            info.setId(result.qrcode.id);
            info.setTimeout(result.qrcode.timeout);
            if (result.qrcode.data != null) {
                info.setBuffer(result.qrcode.data.buffer);
            }
        }
        return info;
    }

    private AccountUserInfo convertQRCodeLoginConfirmInfo(QRCodeLoginConfirmResult result) {
        if (result == null) return null;
        AccountUserInfo info = getAccountBaseInfo(result.code,result.result,result.message,result.timestamp,result.version);
        if (result.profile != null) {
            info.setProfileInfo(convertAccountProfileInfo(result.profile.uid, result.profile.username,result.profile.nickname,
                    result.profile.avatar,result.profile.mobile,result.profile.email));
        }
        return info;
    }

    private AccountUserInfo convertAccountProfileInfo(AccountProfileResult result) {
        if (result == null) return null;
        AccountUserInfo info = getAccountBaseInfo(result.code,result.result,result.message,result.timestamp,result.version);
        if (result.profile != null) {
            info.setProfileInfo(convertAccountProfileInfo(result.profile.uid, result.profile.username,result.profile.nickname,
                    result.profile.avatar,result.profile.mobile,result.profile.email));
        }
        return info;
    }

    private AccountProfileInfo convertAccountProfileInfo(String uid, String username, String nickname, String avatar, String mobile, String email) {
        AccountProfileInfo info = new AccountProfileInfo();
        info.setUid(uid);
        info.setUsername(username);
        info.setNickname(nickname);
        info.setAvatar(avatar);
        info.setMobile(mobile);
        info.setEmail(email);
        return info;
    }

    private AccountUserInfo convertAvatarInfo(AvatarResult result) {
        if (result == null) return null;
        AccountUserInfo info = getAccountBaseInfo(result.code,result.result,result.message,result.timestamp,result.version);
        if (result.data != null) {
            info.setBuffer(result.data.buffer);
        }
        return info;
    }

    private AccountUserInfo convertAccountLogoutInfo(AccountLogoutResult result) {
        if (result == null) return null;
        return getAccountBaseInfo(result.code,result.result,result.message,result.timestamp,result.version);
    }

    private AccountUserInfo convertAccountUnRegisterInfo(AccountUnRegisterResult result) {
        if (result == null) return null;
        AccountUserInfo info = getAccountBaseInfo(result.code,result.result,result.message,result.timestamp,result.version);
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

}
