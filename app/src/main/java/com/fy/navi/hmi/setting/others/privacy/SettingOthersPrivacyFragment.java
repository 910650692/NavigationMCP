package com.fy.navi.hmi.setting.others.privacy;

import android.view.View;

import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentSettingPrivacyBinding;
import com.fy.navi.ui.base.BaseFragment;

public class SettingOthersPrivacyFragment extends BaseFragment<FragmentSettingPrivacyBinding, SettingOthersPrivacyViewModel> {
    @Override
    public int onLayoutId() {
        return R.layout.fragment_setting_privacy;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        mViewModel.initView();
    }

    @Override
    public void onInitData() {

    }

    public void setEndDate(String endDate, boolean isShow) {
        Logger.d("SettingOthersPrivacyFragment", "setEndDate：" + endDate);
        ThreadManager.getInstance().postUi(() -> {
            if (isShow) {
                mBinding.settingOthersPrivacyEndDateLayout.setVisibility(View.VISIBLE);
                mBinding.settingOthersPrivacyEndDate.setText(endDate);
                mBinding.settingOthersPrivacyOneYear.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.white));
                mBinding.settingOthersPrivacyNever.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.black));
            } else {
                mBinding.settingOthersPrivacyEndDateLayout.setVisibility(View.GONE);
                mBinding.settingOthersPrivacyOneYear.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.black));
                mBinding.settingOthersPrivacyNever.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.white));
            }
        });
    }
}
