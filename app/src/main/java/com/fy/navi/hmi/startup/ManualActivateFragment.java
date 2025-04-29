package com.fy.navi.hmi.startup;

import android.view.View;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentManualActivateBinding;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.service.logicpaket.activate.ActivatePackage;
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
        mBinding.btnConfirm.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(final View v) {
                ActivatePackage.getInstance().manualActivate(
                        "PETAW9KTKQS79GZQBCXA984B", "UR32YH4SP4S9SQ57SJZ9ZUK9");
            }
        });
    }

    @Override
    public void onInitObserver() {
        super.onInitObserver();
    }

    @Override
    public void onInitData() {

    }
}