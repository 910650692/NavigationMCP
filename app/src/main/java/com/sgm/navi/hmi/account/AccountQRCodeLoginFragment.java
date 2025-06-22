package com.sgm.navi.hmi.account;

import android.graphics.Bitmap;
import android.view.View;
import android.view.animation.Animation;
import android.view.animation.RotateAnimation;

import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.account.adapter.SlideAdapter;
import com.sgm.navi.hmi.account.adapter.SlideItem;
import com.sgm.navi.hmi.databinding.FragmentQrcodeLoginBinding;
import com.sgm.navi.service.define.user.account.QRCodeType;
import com.sgm.navi.ui.base.BaseFragment;
import com.google.android.material.tabs.TabLayoutMediator;

import java.util.ArrayList;
import java.util.List;

public class AccountQRCodeLoginFragment extends BaseFragment<FragmentQrcodeLoginBinding, AccountQRCodeLoginViewModel> {

    private RotateAnimation mRotateAnimation;

    @Override
    public int onLayoutId() {
        return R.layout.fragment_qrcode_login;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        initAnimation();
        initView();
        initSlidePage();
    }

    @Override
    public void onInitData() {

    }

    /**
     * 初始化view
     */
    private void initView() {
        if (getNetworkState()) {
            startAnimation();
            mViewModel.qrCodeLoginRequest(QRCodeType.QR_CODE_TYPE_DEFAULT);
        } else {
            ToastUtils.Companion.getInstance().showCustomToastView(
                    ResourceUtils.Companion.getInstance().getString(R.string.setting_qr_code_load_offline_toast));
            mViewModel.updateLoadingVisible(false, true, false);
        }

    }

    /**
     * 更新二维码图片
     * @param bitmap 二维码图片
     */
    public void updateQRCode(final Bitmap bitmap) {
        ThreadManager.getInstance().postUi(() -> {
            stopAnimation();
            if (bitmap != null) {
                mBinding.accountQrcodeContentImg.setImageBitmap(bitmap);
            } else {
                mViewModel.updateLoadingVisible(false, true, false);
            }
        });
    }

    /**
     * 初始化滑动页面
     */
    private void initSlidePage() {
        final List<SlideItem> slideItems = new ArrayList<>();
        slideItems.add(new SlideItem(R.mipmap.icon_account_car_connect,
                ResourceUtils.Companion.getInstance().getString(R.string.car_connect_unLogin_function_1),
                ResourceUtils.Companion.getInstance().getString(R.string.car_connect_unLogin_function_1_tip)));
        slideItems.add(new SlideItem(R.mipmap.icon_account_connect,
                ResourceUtils.Companion.getInstance().getString(R.string.car_connect_unLogin_function_2),
                ResourceUtils.Companion.getInstance().getString(R.string.car_connect_unLogin_function_2_tip)));
        slideItems.add(new SlideItem(R.mipmap.icon_account_find_car,
                ResourceUtils.Companion.getInstance().getString(R.string.car_connect_unLogin_function_3),
                ResourceUtils.Companion.getInstance().getString(R.string.car_connect_unLogin_function_3_tip)));
        slideItems.add(new SlideItem(R.mipmap.icon_account_sync_data,
                ResourceUtils.Companion.getInstance().getString(R.string.car_connect_unLogin_function_4),
                ResourceUtils.Companion.getInstance().getString(R.string.car_connect_unLogin_function_4_tip)));

        final SlideAdapter adapter = new SlideAdapter(slideItems);
        mBinding.accountQrcodeViewpager.setAdapter(adapter);

        new TabLayoutMediator(mBinding.accountQrcodeTab, mBinding.accountQrcodeViewpager,true,true,
                (tab, position) -> {
                    final View view = getLayoutInflater().inflate(R.layout.item_slider_tab, null);
                    tab.setCustomView(view);
                }).attach();
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

    /**
     * 获取网络状态
     * @return 网络状态
     */
    public boolean getNetworkState() {
        return Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork());
    }
}
