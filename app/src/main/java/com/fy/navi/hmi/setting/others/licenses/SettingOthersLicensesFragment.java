package com.fy.navi.hmi.setting.others.licenses;

import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentSettingLicensesBinding;
import com.fy.navi.ui.base.BaseFragment;

public class SettingOthersLicensesFragment extends BaseFragment<FragmentSettingLicensesBinding, SettingOthersLicensesViewModel> {
    @Override
    public int onLayoutId() {
        return R.layout.fragment_setting_licenses;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {

    }

    @Override
    public void onInitData() {

    }
}
