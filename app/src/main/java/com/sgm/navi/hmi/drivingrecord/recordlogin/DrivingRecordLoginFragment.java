package com.sgm.navi.hmi.drivingrecord.recordlogin;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.view.View;
import android.view.Window;
import android.view.animation.Animation;
import android.view.animation.RotateAnimation;

import androidx.core.graphics.drawable.RoundedBitmapDrawable;
import androidx.core.graphics.drawable.RoundedBitmapDrawableFactory;

import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentDrivingRecordLoginBinding;
import com.sgm.navi.hmi.setting.SettingCheckDialog;
import com.sgm.navi.service.GBLCacheFilePath;
import com.sgm.navi.service.define.user.account.QRCodeType;
import com.sgm.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.sgm.navi.service.logicpaket.user.usertrack.UserTrackPackage;
import com.sgm.navi.ui.base.BaseFragment;
import com.sgm.navi.ui.base.StackManager;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;

import java.util.ArrayList;


public class DrivingRecordLoginFragment extends BaseFragment<FragmentDrivingRecordLoginBinding, DrivingRecordLoginViewModel> {

    private RotateAnimation mRotateAnimation;
    private SettingCheckDialog mMergeDivingRecordDialog;
    @Override
    public int onLayoutId() {
        return R.layout.fragment_driving_record_login;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        initAnimation();
        initDialog();
        mViewModel.initView();
        if (getNetworkState()) {
            startAnimation();
            mViewModel.qrCodeLoginRequest(QRCodeType.QR_CODE_TYPE_DEFAULT);
        } else {
            ToastUtils.Companion.getInstance().showCustomToastView(
                    ResourceUtils.Companion.getInstance().getString(R.string.setting_qr_code_load_offline_toast));
            mViewModel.updateLoadingVisible(false, true, false);
        }
    }

    @Override
    public void onInitData() {
        ThreadManager.getInstance().postDelay(() -> {
            mViewModel.getValueByType();
        },0);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        clearDialog();
    }

    @Override
    public void onReStoreFragment() {
        super.onReStoreFragment();
        restoreFragment();
    }

    /**
     * 恢复Fragment的状态
     */
    private void restoreFragment(){
        if(mViewModel.getIsMergeDivingRecordDialog()){
            mMergeDivingRecordDialog.show();
        }
    }

    /**
     * 清除Dialog
     */
    private void clearDialog(){
        if(mMergeDivingRecordDialog.isShowing()){
            mMergeDivingRecordDialog.dismiss();
        }
        mMergeDivingRecordDialog = null;
    }

    /**
     * 更新二维码
     * @param bitmap 二维码图片
     */
    public void updateQRCode(final Bitmap bitmap) {
        ThreadManager.getInstance().postUi(() -> {
            RoundedBitmapDrawable roundedBitmapDrawable =
                    getRoundedBitmapDrawable(getContext(), bitmap, 20);

            mBinding.codeImg.setImageDrawable(roundedBitmapDrawable);
            mBinding.qrcodeImg.setImageDrawable(roundedBitmapDrawable);

        });
    }

    /**
     * 更新未登录高德账号时，需判断当前是否存在行程历史数据
     * @param size 历史数据大小
     */
    public void updateNoDataView(final int size) {
        ThreadManager.getInstance().postUi(() -> {
            // 未登录高德账号时，需判断当前是否存在行程历史数据
            if (size > 0) { // 未登录存在历史数据
                mBinding.dataLoginView.setVisibility(View.VISIBLE);
                mBinding.noDataLoginView.setVisibility(View.GONE);
            } else {
                mBinding.dataLoginView.setVisibility(View.GONE);
                mBinding.noDataLoginView.setVisibility(View.VISIBLE);
            }
        });
    }

    /**
     * 初始化动画
     */
    private void initAnimation() {
        // 设置图片
        // 创建旋转动画
        mRotateAnimation = new RotateAnimation(
                0, 360, Animation.RELATIVE_TO_SELF, 0.5f, Animation.RELATIVE_TO_SELF, 0.5f);
        mRotateAnimation.setDuration(1000); // 1秒完成一次旋转
        mRotateAnimation.setRepeatCount(Animation.INFINITE); // 无限循环
    }

    /**
     * 开始动画
     */
    public void startAnimation() {
        ThreadManager.getInstance().postUi(() -> {
            mBinding.drivingRecordQrcodeLoading.startAnimation(mRotateAnimation);
            mBinding.drivingRecordQrcodeLoadingNoData.startAnimation(mRotateAnimation);
        });
    }

    /**
     * 结束动画
     */
    public void stopAnimation() {
        ThreadManager.getInstance().postUi(() -> {
            mBinding.drivingRecordQrcodeLoading.clearAnimation();
            mBinding.drivingRecordQrcodeLoadingNoData.clearAnimation();
        });
    }

    /**
     * 初始化Dialog
     */
    public void initDialog() {

        mMergeDivingRecordDialog = new SettingCheckDialog.Build(getContext())
                .setTitle(ResourceUtils.Companion.getInstance().getString(R.string.driving_record_setting_dialog_merge_title))
                .setContent("")
                .setConfirmText(ResourceUtils.Companion.getInstance().getString(R.string.driving_record_setting_dialog_merge_confirm))
                .setDialogObserver(new IBaseDialogClickListener() {
                    @Override
                    public void onCommitClick() {
                        mViewModel.setIsMergeDivingRecordDialog(false);
                        final ArrayList<DrivingRecordDataBean> dataBeanList = mViewModel.getDrivingRecordDataList();
                        if (dataBeanList == null || dataBeanList.isEmpty()) {
                            return;
                        }
                        for (DrivingRecordDataBean dataBean : dataBeanList) {
                            UserTrackPackage.getInstance().obtainGpsTrackDepInfo(GBLCacheFilePath.SYNC_PATH + "/403", dataBean.getTrackFileName());
                        }
                    }

                    @Override
                    public void onCancelClick() {
                        mViewModel.setIsMergeDivingRecordDialog(false);
                    }
                }).build();
        clearBackground(mMergeDivingRecordDialog.getWindow());
    }

    /**
     * 清除背景色
     * @param window 窗口
     */
    private void clearBackground(final Window window) {
        if (window != null) {
            window.setDimAmount(0f);
        }
    }

    /**
     * 展示合并记录对话框
     */
    public void showMergeDivingRecordDialog() {
        ThreadManager.getInstance().postUi(() -> {
            mMergeDivingRecordDialog.show();
        });

    }

    /**
     * 获取当前是否在行程历史登录页
     * @return true：在行程历史登录页
     */
    public boolean isRecordLoginFragment() {
        final BaseFragment fragment =  StackManager.getInstance().getCurrentFragment(mScreenId);
        return fragment instanceof DrivingRecordLoginFragment;
    }


    /**
     * 获取网络状态
     * @return 网络状态
     */
    public boolean getNetworkState() {
        return Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork());
    }

    /**
     * 将bitmap添加圆角
     * @return drawable
     */
    public static RoundedBitmapDrawable getRoundedBitmapDrawable(Context context, Bitmap bitmap, float cornerRadius) {
        RoundedBitmapDrawable drawable = RoundedBitmapDrawableFactory.create(context.getResources(), bitmap);
        drawable.setCornerRadius(cornerRadius);
        drawable.setAntiAlias(true);
        return drawable;
    }
}
