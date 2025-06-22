package com.sgm.navi.service.adapter.user.account.bls;

import android.accounts.Account;

import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.user.account.AccountService;
import com.autonavi.gbl.user.account.model.AccountCheckRequest;
import com.autonavi.gbl.user.account.model.AccountLogoutRequest;
import com.autonavi.gbl.user.account.model.AccountProfileRequest;
import com.autonavi.gbl.user.account.model.AccountRegisterRequest;
import com.autonavi.gbl.user.account.model.AccountUnRegisterRequest;
import com.autonavi.gbl.user.account.model.AvatarRequest;
import com.autonavi.gbl.user.account.model.MobileLoginRequest;
import com.autonavi.gbl.user.account.model.QRCodeLoginConfirmRequest;
import com.autonavi.gbl.user.account.model.QRCodeLoginRequest;
import com.autonavi.gbl.user.account.model.VerificationCodeRequest;
import com.autonavi.gbl.user.account.model.VerificationCodeType;
import com.autonavi.gbl.user.account.model.VerificationTargetType;
import com.autonavi.gbl.util.model.ServiceInitStatus;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.sgm.navi.service.adapter.user.account.AccountAdapterCallBack;
import com.sgm.navi.service.adapter.user.account.IAccountApi;
import com.sgm.navi.service.define.user.account.AccessTokenParam;


public class AccountAdapterImpl implements IAccountApi {
    private final AccountService mAccountService;
    private final AccountAdapterImplHelper mAdapterImplHelper;

    public AccountAdapterImpl() {
        mAccountService = (AccountService) ServiceMgr.getServiceMgrInstance()
                .getBLService(SingleServiceID.AccountSingleServiceID);
        mAdapterImplHelper = new AccountAdapterImplHelper(mAccountService);
    }

    @Override
    public void initAccountService() {
        mAdapterImplHelper.initAccountService();
    }

    @Override
    public void registerCallBack(final String key, final AccountAdapterCallBack callBack) {
        mAdapterImplHelper.registerCallBack(key, callBack);
    }

    @Override
    public void unRegisterCallback(final String key) {
        mAdapterImplHelper.unRegisterCallBack(key);
    }

    @Override
    public void unInitAccountService() {
        if (mAccountService != null) {
            mAdapterImplHelper.removeCallback();
            if (ServiceInitStatus.ServiceInitDone != mAccountService.isInit()) {
                mAccountService.unInit();
            }
        }
    }

    /**
     * 获取验证码请求
     */
    @Override
    public int verificationCodeRequest(final String mobileNum) {
        if (mAccountService != null) {
            //构造验证码请求参数
            final VerificationCodeRequest loginVerifyReq = new VerificationCodeRequest();
            //一般场景下，取值为VerificationCodeTypeRegister 或 VerificationCodeTypeMobileLogin
            loginVerifyReq.codeType = VerificationCodeType.VerificationCodeTypeMobileLogin;// 手机号登录
            loginVerifyReq.targetType = VerificationTargetType.VerificationTargetTypeSms;
            loginVerifyReq.targetValue = mobileNum; // “请填写需要发送验证码的手机号”;
            loginVerifyReq.bindMode = true;
            loginVerifyReq.skipNew = true;
            return mAccountService.executeRequest(loginVerifyReq);
        } else {
            return  -1;
        }
    }

    /**
     * 检查账号是否存在
     */
    @Override
    public int accountCheckRequest() {
        if (mAccountService != null) {
            final AccountCheckRequest checkReq = new AccountCheckRequest();
            return mAccountService.executeRequest(checkReq);
        } else {
            return  -1;
        }
    }

    /**
     * 账号注册
     * @return -1 表示注册失败，其他表示注册成功
     */
    @Override
    public int accountRegisterRequest(final String codeInput, final String mobileInput) {
        if (mAccountService != null) {
            final AccountRegisterRequest registerReq = new AccountRegisterRequest();
            registerReq.code = codeInput;
            registerReq.mobileNum = mobileInput;
            final int res = mAccountService.executeRequest(registerReq);
            return res;
        } else {
            return -1;
        }
    }

    /**
     * 手机验证码登录
     * @param codeInput 验证码
     * @param mobileInput 手机号
     * @return -1 表示登录失败，其他表示登录成功
     */
    @Override
    public int mobileLoginRequest(final String codeInput, final String mobileInput) {
        // 登录请求
        if (mAccountService != null) {
            final MobileLoginRequest loginReq = new MobileLoginRequest();
            loginReq.code = codeInput;  // 验证码
            loginReq.mobileNum = mobileInput; // 手机号
            return mAccountService.executeRequest(loginReq);
        } else {
            return -1;
        }
    }

    /**
     * 获取登录二维码
     * @return -1 表示获取二维码失败，其他表示获取二维码成功
     */
    @Override
    public int qrCodeLoginRequest(final int qrType) {
        if (mAccountService != null) {
            final QRCodeLoginRequest qrReq = new QRCodeLoginRequest();
            qrReq.codeType = qrType;
            return  mAccountService.executeRequest(qrReq);
        } else {
            return -1;
        }
    }

    @Override
    public int qrCodeLoginConfirmRequest(final String qrCodeId) {
        if (mAccountService != null){
            final QRCodeLoginConfirmRequest req = new QRCodeLoginConfirmRequest();
            req.qrcodeId = qrCodeId;
            return mAccountService.executeRequest(req);
        } else {
            return -1;
        }

    }

    /**
     * 获取账号信息
     * @return -1 表示获取账号信息失败，其他表示获取账号信息成功
     */
    @Override
    public int accountProfileRequest() {
        if (mAccountService != null) {
            final AccountProfileRequest profileReq = new AccountProfileRequest();
            profileReq.mode = 0; //获取用户基本信息
            return mAccountService.executeRequest(profileReq);
        } else {
            return -1;
        }
    }

    /**
     * 获取用户头像
     * @return -1 表示获取用户头像失败，其他表示获取用户头像成功
     */
    @Override
    public int avatarRequest() {
        if (mAccountService != null) {
            final AvatarRequest avatarReq = new AvatarRequest();
            return mAccountService.executeRequest(avatarReq);
        } else {
            return -1;
        }
    }

    /**
     * 退出登录请求
     * @return -1 表示退出登录失败，其他表示退出登录成功
     */
    @Override
    public int accountLogoutRequest() {
        if (mAccountService != null) {
            final AccountLogoutRequest logoutReq = new AccountLogoutRequest();
            return mAccountService.executeRequest(logoutReq);
        } else {
            return -1;
        }
    }

    /**
     * 请求注销账号
     * @return -1 表示注销账号失败，其他表示注销账号成功
     */
    @Override
    public int accountUnRegisterRequest() {
        if (mAccountService != null) {
            final AccountUnRegisterRequest request = new AccountUnRegisterRequest();
            return mAccountService.executeRequest(request);
        } else {
            return -1;
        }
    }

    @Override
    public String getAccessToken(final AccessTokenParam param) {
        return mAdapterImplHelper.getAccessToken(param);
    }

    @Override
    public String getUserData(final Account availableAccount, final String key) {
        return mAdapterImplHelper.getUserData(availableAccount, key);
    }

    @Override
    public Account[] getAccounts() {
        return mAdapterImplHelper.getAccounts();
    }
}
