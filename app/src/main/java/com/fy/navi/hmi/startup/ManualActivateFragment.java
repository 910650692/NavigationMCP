package com.fy.navi.hmi.startup;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentManualActivateBinding;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.ui.base.BaseFragment;

@Route(path = RoutePath.Activate.MANUAL_ACTIVATE_FRAGMENT)
public class ManualActivateFragment extends BaseFragment<FragmentManualActivateBinding, ManualActivateViewModel> {


    @Override
    public int onLayoutId() {
        return R.layout.fragment_manual_activate;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {

    }

    @Override
    public void onInitObserver() {
        super.onInitObserver();
    }

    @Override
    public void onInitData() {

    }
}