package com.fy.navi.hmi.wechat;

import android.app.Application;
import android.graphics.Bitmap;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.R;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;


public class BaseWeChatViewModel extends BaseViewModel<WeChatFragment, WeChatModel> {

    public MutableLiveData<Boolean> mQRCodeLoadingVisible = new MutableLiveData<>(true);
    public MutableLiveData<Boolean> mQRCodeLoadFailedVisible = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> mQRCodeVisible = new MutableLiveData<>(false);


    public MutableLiveData<Boolean> mIsBind = new MutableLiveData<>(false);

    public BaseWeChatViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected WeChatModel initModel() {
        return new WeChatModel();
    }

    //返回上一页
    public Action mChatConnectBack = () -> {
        closeFragment(true);
    };

    public Action mHowToBind = () -> {
        mView.showUnbindDialog();
    };

    public Action mRetry = () -> {
        if (mModel.getNetworkState()) {
            startAnimation();
            mModel.sendReqWsPpAutoWeixinQrcode();
        } else {
            ToastUtils.Companion.getInstance().showCustomToastView(
                    ResourceUtils.Companion.getInstance().getString(R.string.setting_qr_code_load_offline_toast));
        }
    };

    /**
     * 设置是否绑定
     * @param isBind 是否绑定
     */
    public void setIsBind(final boolean isBind) {
        this.mIsBind.postValue(isBind);
        mView.updateTitle(isBind);
    }


    /**
     * 初始化view
     */
    public void initView() {
        mModel.getBindStatus();
    }

    /**
     * 更新二维码
     * @param bitmap 二维码图片
     */
    public void updateQRCode(final Bitmap bitmap) {
        mView.updateQRCode(bitmap);
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
