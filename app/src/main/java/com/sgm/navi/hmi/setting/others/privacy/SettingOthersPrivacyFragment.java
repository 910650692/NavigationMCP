package com.sgm.navi.hmi.setting.others.privacy;

import android.view.View;
import android.widget.CompoundButton;

import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentSettingPrivacyBinding;
import com.sgm.navi.ui.base.BaseFragment;

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

    }

    @Override
    public void onResume() {
        super.onResume();
        mViewModel.initView();
    }

    @Override
    public void onInitData() {
        updateCheckBoxTextColor();
    }

    /**
     * 设置隐私状态
     * @param isShow 是否显示
     */
    public void setEndDate(final boolean isShow) {
        Logger.d("SettingOthersPrivacyFragment", "isShow：" + isShow);
        ThreadManager.getInstance().postUi(() -> {
            if (isShow) {
                mBinding.settingOthersPrivacyEndDateLayout.setVisibility(View.VISIBLE);
//                mBinding.settingOthersPrivacyOneYear.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_white));
//                mBinding.settingOthersPrivacyNever.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.color_black_70));
            } else {
                mBinding.settingOthersPrivacyEndDateLayout.setVisibility(View.GONE);
//                mBinding.settingOthersPrivacyOneYear.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.color_black_70));
//                mBinding.settingOthersPrivacyNever.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_white));
            }
        });
    }


    public void updateCheckBoxTextColor() {
        mBinding.settingOthersPrivacyNever.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mBinding.settingOthersPrivacyOneYear.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
    }

    public void updateCheckBoxTextColor(final CompoundButton compoundButton, final boolean isSelected) {
        if (isSelected) {
            compoundButton.setTextColor(getResources().getColor(com.sgm.navi.scene.R.color.setting_white));
        } else {
            compoundButton.setTextColor(getResources().getColor(com.sgm.navi.scene.R.color.setting_preference_text_gray));
        }
    }
}
