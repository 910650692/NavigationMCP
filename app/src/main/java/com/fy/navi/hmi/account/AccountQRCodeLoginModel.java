package com.fy.navi.hmi.account;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.user.account.AccountRequestType;
import com.fy.navi.service.define.user.account.AccountUserInfo;
import com.fy.navi.service.logicpaket.user.account.AccountCallBack;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;
import com.fy.navi.ui.base.BaseModel;

public class AccountQRCodeLoginModel extends BaseModel<AccountQRCodeLoginViewModel> implements AccountCallBack {

    private static final String TAG = MapDefaultFinalTag.ACCOUNT_HMI_TAG;

    private final AccountPackage accountPackage;
    public static final int ErrorCodeLoginSuccess = 1073807360;

    public AccountQRCodeLoginModel() {
        accountPackage = AccountPackage.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        accountPackage.registerCallBack("AccountQRCodeLoginModel",this);
    }
    @Override
    public void notifyQRCodeLogin(int errCode, int taskId, AccountUserInfo result) {
        if (result != null && result.code == 1) {
            Logger.i(TAG,"notifyQRCodeLogin AccountUserInfo = " + GsonUtils.toJson(result));
            Bitmap bitmap = BitmapFactory.decodeByteArray(result.buffer, 0, result.buffer.length);
            mViewModel.updateQRCode(bitmap);
        }
    }

    @Override
    public void notifyQRCodeLoginConfirm(int errCode, int taskId, AccountUserInfo result) {
        if (result != null && errCode == ErrorCodeLoginSuccess) {
            if (result.code == 7) {
                // 超时
                qRCodeLoginRequest(AccountRequestType.AccountTypeQRCodeLogin);
            } else if (result.code == 1) {
                Logger.i(TAG,"QRCodeLogin Success");
                mViewModel.close();
            }
        }
    }

    public void qRCodeLoginRequest(int qrType) {
        accountPackage.qRCodeLoginRequest(qrType);
    }

    public void qRCodeLoginConfirmRequest(String qrCodeId) {
        accountPackage.qRCodeLoginConfirmRequest(qrCodeId);
    }
}
