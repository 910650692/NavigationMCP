package com.sgm.navi.hmi.drivingrecord.recordlogin;

import android.app.Application;
import android.graphics.Bitmap;
import android.os.CountDownTimer;
import android.os.Handler;
import android.os.Looper;

import androidx.annotation.NonNull;
import androidx.databinding.ObservableBoolean;
import androidx.lifecycle.MutableLiveData;

import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.drivingrecord.recordsetting.RecordSettingFragment;
import com.sgm.navi.service.define.user.account.QRCodeType;
import com.sgm.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.sgm.navi.ui.action.Action;
import com.sgm.navi.ui.base.BaseViewModel;

import java.util.ArrayList;


public class BaseDrivingRecordLoginViewModel extends BaseViewModel<DrivingRecordLoginFragment, DrivingRecordLoginModel> {

    public ObservableBoolean mQRCodeLoadingVisible = new ObservableBoolean(true);
    public ObservableBoolean mQRCodeLoadFailedVisible = new ObservableBoolean(false);
    public ObservableBoolean mQRCodeVisible = new ObservableBoolean(false);
    public ObservableBoolean mLoginVisible = new ObservableBoolean(false);
    private ObservableBoolean mIsMergeDivingRecordDialog = new ObservableBoolean(false);
    private boolean isLoading;
    private CountDownTimer mCountDownTimer = new CountDownTimer(10000, 1000) {
        public void onTick(long millisUntilFinished) {

        }

        public void onFinish() {
            if (isLoading) {
                updateLoadingVisible(false, true, false);
                isLoading = false;
            }
        }
    }.start();

    public BaseDrivingRecordLoginViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected DrivingRecordLoginModel initModel() {
        return new DrivingRecordLoginModel();
    }

    //返回上一页
    public Action mDrivingRecordBack = () -> {
        mView.stopAnimation();
        closeFragment(true);
    };

    public Action mToRecordSetting = () -> {
        addFragment(new RecordSettingFragment(), null);
    };

    // 关闭当前页
    public Action mToClose = () -> {
        mView.stopAnimation();
        closeFragment(true);
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
     * 初始化页面
     */
    public void initView() {
        mModel.initView();
    }

    /**
     * 通过type查找其对应行程历史信息大小
     */
    public void getValueByType() {
         mView.updateNoDataView(mModel.getValueByType());
    }

    /**
     * 请求二维码登录
     * @param qrType 二维码类型
     */
    public void qrCodeLoginRequest(final int qrType){
        mModel.qrCodeLoginRequest(qrType);
        mCountDownTimer.start();
        isLoading = true;
    }

    /**
     * 更新二维码
     * @param bitmap 二维码图片
     */
    public void updateQRCode(final Bitmap bitmap) {
        mView.updateQRCode(bitmap);
    }

    /**
     * 关闭当前页
     */
    public void close() {
        new Handler(Looper.getMainLooper()).post(() -> {
            closeFragment(true);
        });
    }

    /**
     * 更新二维码加载状态
     * @param isVisibleLoading 是否显示加载中
     * @param isVisibleFailed 是否显示加载失败
     * @param isVisibleQRCode 是否显示二维码
     */
    public void updateLoadingVisible(final boolean isVisibleLoading, final boolean isVisibleFailed, final boolean isVisibleQRCode){
        ThreadManager.getInstance().postUi(() -> {
            mQRCodeLoadingVisible.set(isVisibleLoading);
            mQRCodeLoadFailedVisible.set(isVisibleFailed);
            mQRCodeVisible.set(isVisibleQRCode);
        });
        if (isVisibleFailed || isVisibleQRCode) {
            mCountDownTimer.cancel();
            isLoading = false;
        }
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

    /**
     * 展示合并记录对话框
     */
    public void showMergeDivingRecordDialog() {
        mView.showMergeDivingRecordDialog();
        setIsMergeDivingRecordDialog(true);
    }

    /**
     * 获取当前是否在行程历史登录页
     * @return true：在行程历史登录页
     */
    public boolean isRecordLoginFragment() {
        return mView.isRecordLoginFragment();
    }

    /**
     * 设置登录按钮是否显示
     * @param isVisible 是否显示登录按钮
     */
    public void setLoginVisible(final boolean isVisible) {
        ThreadManager.getInstance().postUi(() -> {
            mLoginVisible.set(isVisible);
        });
    }

    /**
     * 获取需要同步行程历史数据列表
     * @return 需要同步行程历史数据列表
     */
    public ArrayList<DrivingRecordDataBean> getDrivingRecordDataList() {
        return mModel.getDrivingRecordDataList();
    }

    /**
     * 设置是否显示合并记录对话框的值
     * 此方法用于更新观察者模式中观察的值，以通知订阅者关于是否应该显示合并记录对话框
     *
     * @param isMergeDivingRecordDialog 一个布尔值，指示是否应该显示合并记录对话框
     */
    public void setIsMergeDivingRecordDialog(final boolean isMergeDivingRecordDialog) {
        mIsMergeDivingRecordDialog.set(isMergeDivingRecordDialog);
    }

    /**
     * 获取是否显示合并记录对话框的值
     *
     * @return 一个布尔值，指示是否应该显示合并记录对话框
     */
    public boolean getIsMergeDivingRecordDialog() {
        return Boolean.TRUE.equals(mIsMergeDivingRecordDialog.get());
    }
}
