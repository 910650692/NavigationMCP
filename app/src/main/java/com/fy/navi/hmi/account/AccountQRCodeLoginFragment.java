package com.fy.navi.hmi.account;

import android.graphics.Bitmap;
import android.view.View;

import com.android.utils.ResourceUtils;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.account.adapter.SlideAdapter;
import com.fy.navi.hmi.account.adapter.SlideItem;
import com.fy.navi.hmi.databinding.FragmentQrcodeLoginBinding;
import com.fy.navi.service.define.user.account.QRCodeType;
import com.fy.navi.ui.base.BaseFragment;
import com.google.android.material.tabs.TabLayoutMediator;

import java.util.ArrayList;
import java.util.List;

public class AccountQRCodeLoginFragment extends BaseFragment<FragmentQrcodeLoginBinding, AccountQRCodeLoginViewModel> {
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
        initView();
        initSlidePage();
    }

    @Override
    public void onInitData() {

    }

    private void initView() {
        mViewModel.qRCodeLoginRequest(QRCodeType.QRCodeTypeDefault);
    }

    public void updateQRCode(Bitmap bitmap) {
        ThreadManager.getInstance().postUi(() -> {
            mBinding.accountQrcodeContentImg.setImageBitmap(bitmap);
        });
    }

    private void initSlidePage() {
        List<SlideItem> slideItems = new ArrayList<>();
        slideItems.add(new SlideItem(R.mipmap.icon_account_car_connect, ResourceUtils.Companion.getInstance().getString(R.string.car_connect_unLogin_function_1),
                ResourceUtils.Companion.getInstance().getString(R.string.car_connect_unLogin_function_tip)));
        slideItems.add(new SlideItem(R.mipmap.icon_account_car_connect, ResourceUtils.Companion.getInstance().getString(R.string.car_connect_unLogin_function_2),
                ResourceUtils.Companion.getInstance().getString(R.string.car_connect_unLogin_function_tip)));
        slideItems.add(new SlideItem(R.mipmap.icon_account_car_connect, ResourceUtils.Companion.getInstance().getString(R.string.car_connect_unLogin_function_3),
                ResourceUtils.Companion.getInstance().getString(R.string.car_connect_unLogin_function_tip)));
        slideItems.add(new SlideItem(R.mipmap.icon_account_car_connect, ResourceUtils.Companion.getInstance().getString(R.string.car_connect_unLogin_function_4),
                ResourceUtils.Companion.getInstance().getString(R.string.car_connect_unLogin_function_tip)));

        SlideAdapter adapter = new SlideAdapter(slideItems);
        mBinding.accountQrcodeViewpager.setAdapter(adapter);

        new TabLayoutMediator(mBinding.accountQrcodeTab, mBinding.accountQrcodeViewpager,true,true,
                (tab, position) -> {
                    View view = getLayoutInflater().inflate(R.layout.item_slider_tab, null);
                    tab.setCustomView(view);
                }).attach();
    }
}
