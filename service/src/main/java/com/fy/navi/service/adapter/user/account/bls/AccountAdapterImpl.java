package com.fy.navi.service.adapter.user.account.bls;

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
import com.fy.navi.service.adapter.user.account.AccountAdapterCallBack;
import com.fy.navi.service.adapter.user.account.IAccountApi;

/**
 * 高德账号数据服务.
 * @Description Impl类只做SDK的原子能力封装，不做对象及数据转换
 * @Author fh
 * @date 2024/12/18
 */
public class AccountAdapterImpl implements IAccountApi {
    private AccountService mAccountService;
    private AccountAdapterImplHelper adapterImplHelper;

    public AccountAdapterImpl() {
        mAccountService = (AccountService) ServiceMgr.getServiceMgrInstance()
                .getBLService(SingleServiceID.AccountSingleServiceID);
        adapterImplHelper = new AccountAdapterImplHelper(mAccountService);
    }

    @Override
    public void initAccountService() {
        adapterImplHelper.initAccountService();
    }

    @Override
    public void registerCallBack(String key, AccountAdapterCallBack callBack) {
        adapterImplHelper.registerCallBack(key, callBack);
    }

    @Override
    public void unRegisterCallback(String key) {
        adapterImplHelper.unRegisterCallBack(key);
    }

    @Override
    public void unInitAccountService() {
        if (mAccountService != null) {
            adapterImplHelper.removeCallback();
            if (ServiceInitStatus.ServiceInitDone != mAccountService.isInit()) {
                mAccountService.unInit();
            }
        }
    }

    /**
     * 获取验证码请求
     */
    @Override
    public int verificationCodeRequest(String mobileNum) {
        if (mAccountService != null) {
            //构造验证码请求参数
            VerificationCodeRequest loginVerifyReq = new VerificationCodeRequest();
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
            AccountCheckRequest checkReq = new AccountCheckRequest();
            return mAccountService.executeRequest(checkReq);
        } else {
            return  -1;
        }
    }

    /**
     * 账号注册
     * @return
     */
    @Override
    public int accountRegisterRequest(String codeInput, String mobileInput) {
        if (mAccountService != null) {
            AccountRegisterRequest registerReq = new AccountRegisterRequest();
            registerReq.code = codeInput;
            registerReq.mobileNum = mobileInput;
            int res = mAccountService.executeRequest(registerReq);
            return res;
        } else {
            return -1;
        }
    }

    /**
     * 手机验证码登录
     * @param codeInput
     * @param mobileInput
     * @return
     */
    @Override
    public int mobileLoginRequest(String codeInput, String mobileInput) {
        // 登录请求
        if (mAccountService != null) {
            MobileLoginRequest loginReq = new MobileLoginRequest();
            loginReq.code = codeInput;  // 验证码
            loginReq.mobileNum = mobileInput; // 手机号
            return mAccountService.executeRequest(loginReq);
        } else {
            return -1;
        }
    }

    /**
     * 获取登录二维码
     * @return
     */
    @Override
    public int qRCodeLoginRequest(int qrType) {
        if (mAccountService != null) {
            QRCodeLoginRequest qrReq = new QRCodeLoginRequest();
            qrReq.codeType = qrType;
            return  mAccountService.executeRequest(qrReq);
        } else {
            return -1;
        }
    }

    @Override
    public int qRCodeLoginConfirmRequest(String qrCodeId) {
        if (mAccountService != null){
            QRCodeLoginConfirmRequest req = new QRCodeLoginConfirmRequest();
            req.qrcodeId = qrCodeId;
            return mAccountService.executeRequest(req);
        } else {
            return -1;
        }

    }

    /**
     * 获取账号信息
     * @return
     */
    @Override
    public int accountProfileRequest() {
        if (mAccountService != null) {
            AccountProfileRequest profileReq = new AccountProfileRequest();
            profileReq.mode = 0; //获取用户基本信息
            return mAccountService.executeRequest(profileReq);
        } else {
            return -1;
        }
    }

    /**
     * 获取用户头像
     * @return
     */
    @Override
    public int avatarRequest() {
        if (mAccountService != null) {
            AvatarRequest avatarReq = new AvatarRequest();
            return mAccountService.executeRequest(avatarReq);
        } else {
            return -1;
        }
    }

    /**
     * 退出登录请求
     * @return
     */
    @Override
    public int accountLogoutRequest() {
        if (mAccountService != null) {
            AccountLogoutRequest logoutReq = new AccountLogoutRequest();
            return mAccountService.executeRequest(logoutReq);
        } else {
            return -1;
        }
    }

    /**
     * 请求注销账号
     * @return
     */
    @Override
    public int accountUnRegisterRequest() {
        if (mAccountService != null) {
            AccountUnRegisterRequest request = new AccountUnRegisterRequest();
            return mAccountService.executeRequest(request);
        } else {
            return -1;
        }
    }

}
