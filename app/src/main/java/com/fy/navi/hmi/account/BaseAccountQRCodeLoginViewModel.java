package com.fy.navi.hmi.account;

import android.app.Application;
import android.graphics.Bitmap;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.R;
import com.fy.navi.service.define.user.account.QRCodeType;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseAccountQRCodeLoginViewModel extends BaseViewModel<AccountQRCodeLoginFragment, AccountQRCodeLoginModel> {

    public MutableLiveData<Boolean> mQRCodeLoadingVisible = new MutableLiveData<>(true);
    public MutableLiveData<Boolean> mQRCodeLoadFailedVisible = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> mQRCodeVisible = new MutableLiveData<>(false);

    public BaseAccountQRCodeLoginViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected AccountQRCodeLoginModel initModel() {
        return new AccountQRCodeLoginModel();
    }

    public Action mAccountBack = () -> {
        mView.stopAnimation();
        closeFragment(true);
    };

    public Action mClickPhoneNumberLogin = () -> {
        addFragment(new AccountPhoneNumberLoginFragment(), null);
    };

    public Action mRetry = () -> {
        if (mView.getNetworkState()) {
            startAnimation();
            qrCodeLoginRequest(QRCodeType.QR_CODE_TYPE_DEFAULT);
        } else {
            ToastUtils.Companion.getInstance().showCustomToastView(
                    ResourceUtils.Companion.getInstance().getString(R.string.setting_qr_code_load_offline_toast));
        }
    };

    /**
     * 更新二维码
     * @param bitmap 二维码图片
     */
    public void updateQRCode(final Bitmap bitmap) {
        mView.updateQRCode(bitmap);
    }

    /**
     * 请求二维码登录
     * @param qrType 二维码类型
     */
    public void qrCodeLoginRequest(final int qrType){
        mModel.qrCodeLoginRequest(qrType);
    }

    /**
     * 更新二维码加载状态
     * @param isVisibleLoading 是否显示加载中
     * @param isVisibleFailed 是否显示加载失败
     * @param isVisibleQRCode 是否显示二维码
     */
    public void updateLoadingVisible(final boolean isVisibleLoading, final boolean isVisibleFailed, final boolean isVisibleQRCode){
        ThreadManager.getInstance().postUi(() -> {
            mQRCodeLoadingVisible.setValue(isVisibleLoading);
            mQRCodeLoadFailedVisible.setValue(isVisibleFailed);
            mQRCodeVisible.setValue(isVisibleQRCode);
        });
    }

    /**
     * 开始动画
     */
    public void startAnimation(){
        mView.startAnimation();
    }

    /**
     * 停止动画
     */
    public void stopAnimation(){
        mView.stopAnimation();
    }
}
