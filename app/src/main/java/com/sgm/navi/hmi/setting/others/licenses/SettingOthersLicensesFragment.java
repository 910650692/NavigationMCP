package com.sgm.navi.hmi.setting.others.licenses;

import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentSettingLicensesBinding;
import com.sgm.navi.ui.base.BaseFragment;

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
