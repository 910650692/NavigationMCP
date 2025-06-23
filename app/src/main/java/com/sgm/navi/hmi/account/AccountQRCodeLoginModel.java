package com.sgm.navi.hmi.account;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;

import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.hmi.R;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.user.account.AccountUserInfo;
import com.sgm.navi.service.define.user.account.QRCodeType;
import com.sgm.navi.service.logicpaket.user.account.AccountCallBack;
import com.sgm.navi.service.logicpaket.user.account.AccountPackage;
import com.sgm.navi.ui.base.BaseModel;

public class AccountQRCodeLoginModel extends BaseModel<AccountQRCodeLoginViewModel> implements AccountCallBack {

    private static final String TAG = MapDefaultFinalTag.ACCOUNT_HMI_TAG;

    private final AccountPackage mAccountPackage;
    public static final int ERROR_CODE_LOGIN_SUCCESS = 1073807360;

    public AccountQRCodeLoginModel() {
        mAccountPackage = AccountPackage.getInstance();
        NetWorkUtils.Companion.getInstance().registerNetworkObserver(mNetworkObserver);
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mAccountPackage.registerCallBack("AccountQRCodeLoginModel",this);
    }

    @Override
    public void onDestroy() {
        NetWorkUtils.Companion.getInstance().unRegisterNetworkObserver(mNetworkObserver);
        mAccountPackage.unRegisterCallBack("AccountQRCodeLoginModel");
    }

    @Override
    public void notifyQRCodeLogin(final int errCode, final int taskId, final AccountUserInfo result) {
        if (result != null && result.getCode() == 1) {
            Logger.i(TAG,"notifyQRCodeLogin AccountUserInfo = " + GsonUtils.toJson(result));
            final Bitmap bitmap = BitmapFactory.decodeByteArray(result.getBuffer(), 0, result.getBuffer().length);
            mViewModel.updateLoadingVisible(false, false, true);
            mViewModel.updateQRCode(bitmap);
        } else {
            mViewModel.updateLoadingVisible(false, true, false);
            mViewModel.updateQRCode(null);
        }
    }

    @Override
    public void notifyQRCodeLoginConfirm(final int errCode, final int taskId, final AccountUserInfo result) {
        if (result != null) {
            if (errCode == 0 && result.getCode() == 7) {
                // 超时
                mViewModel.startAnimation();
                qrCodeLoginRequest(QRCodeType.QR_CODE_TYPE_DEFAULT);
            } else if (errCode == ERROR_CODE_LOGIN_SUCCESS && result.getCode() == 1) {
                Logger.i(TAG,"QRCodeLogin Success");
                closeFragment(true);
            }
        }
    }

    /**
     * 获取二维码
     * @param qrType 二维码类型
     */
    public void qrCodeLoginRequest(final int qrType) {
        mAccountPackage.qrcodeloginrequest(qrType);
    }

    private final NetWorkUtils.NetworkObserver mNetworkObserver = new NetWorkUtils.NetworkObserver() {
        @Override
        public void onNetConnectSuccess() {

        }

        @Override
        public void onNetDisConnect() {
            ToastUtils.Companion.getInstance().showCustomToastView(
                    ResourceUtils.Companion.getInstance().getString(R.string.setting_qr_code_load_offline_toast));
            mViewModel.stopAnimation();
            mViewModel.updateLoadingVisible(false, true, false);
        }

        @Override
        public void onNetUnavailable() {

        }

        @Override
        public void onNetBlockedStatusChanged() {

        }

        @Override
        public void onNetLosing() {

        }

        @Override
        public void onNetLinkPropertiesChanged() {

        }
    };
}
