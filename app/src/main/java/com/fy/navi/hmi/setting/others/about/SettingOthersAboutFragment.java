package com.fy.navi.hmi.setting.others.about;

import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentSettingAboutBinding;
import com.fy.navi.ui.base.BaseFragment;

public class SettingOthersAboutFragment extends BaseFragment<FragmentSettingAboutBinding, SettingOthersAboutViewModel> {
    @Override
    public int onLayoutId() {
        return R.layout.fragment_setting_about;
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
}
