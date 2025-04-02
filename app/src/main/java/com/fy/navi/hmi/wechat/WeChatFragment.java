package com.fy.navi.hmi.wechat;

import android.graphics.Bitmap;
import android.view.Window;
import android.view.animation.Animation;
import android.view.animation.RotateAnimation;

import com.android.utils.ResourceUtils;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentWeChatBinding;
import com.fy.navi.hmi.setting.SettingCheckDialog;

import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;


public class WeChatFragment extends BaseFragment<FragmentWeChatBinding, WeChatViewModel> {

    private final static String TAG = WeChatFragment.class.getSimpleName();
    private RotateAnimation mRotateAnimation;
    private SettingCheckDialog mUnbindDialog;

    @Override
    public int onLayoutId() {
        return R.layout.fragment_we_chat;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        mViewModel.initView();
        initDialog();
        initAnimation();
    }

    @Override
    public void onInitData() {

    }

    /**
     * 更新二维码
     * @param bitmap 二维码图片
     */
    public void updateQRCode(final Bitmap bitmap) {
        ThreadManager.getInstance().postUi(() -> {
            mBinding.chatContentImg.setImageBitmap(bitmap);
        });
    }

    /**
     * 更新标题
     * @param isLogin 是否登录
     */
    public void updateTitle(final boolean isLogin) {
        ThreadManager.getInstance().postUi(() -> {
            mBinding.weChatTitle.setText(isLogin ?
                    ResourceUtils.Companion.getInstance().getString(R.string.chat_connect_title) :
                    ResourceUtils.Companion.getInstance().getString(R.string.account_chat_connect));
        });
    }

    /**
     * 初始化dialog
     */
    private void initDialog() {

        mUnbindDialog = new SettingCheckDialog.Build(getContext())
                .setTitle(ResourceUtils.Companion.getInstance().getString(R.string.chat_connect_unbind_title))
                .setContent(ResourceUtils.Companion.getInstance().getString(R.string.chat_connect_unbind_tip))
                .setConfirmText(ResourceUtils.Companion.getInstance().getString(R.string.chat_connect_unbind_confirm))
                .setShowCancel(false)
                .setDialogObserver(new IBaseDialogClickListener() {

                }).build();
        clearBackground(mUnbindDialog.getWindow());
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
     * 显示解绑对话框
     */
    public void showUnbindDialog() {
        if (mUnbindDialog == null) {
            initDialog();
        }
        mUnbindDialog.show();
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
            mBinding.accountQrcodeLoading.startAnimation(mRotateAnimation);
        });
    }

    /**
     * 结束动画
     */
    public void stopAnimation() {
        ThreadManager.getInstance().postUi(() -> {
            mBinding.accountQrcodeLoading.clearAnimation();
        });
    }
}
