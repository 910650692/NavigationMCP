package com.sgm.navi.hmi.navi;

import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentNaviDrivereportBinding;
import com.sgm.navi.ui.base.BaseFragment;

public class NaviDriveReportFragment extends BaseFragment<FragmentNaviDrivereportBinding, NaviDriveReportViewModel> {

    @Override
    public int onLayoutId() {
        return R.layout.fragment_navi_drivereport;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {

    }

    @Override
    public void onInitData() {

    }
}
