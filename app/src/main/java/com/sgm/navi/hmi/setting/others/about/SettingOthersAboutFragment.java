package com.sgm.navi.hmi.setting.others.about;

import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentSettingAboutBinding;
import com.sgm.navi.hmi.test.TestWindow;
import com.sgm.navi.ui.base.BaseFragment;

public class SettingOthersAboutFragment extends BaseFragment<FragmentSettingAboutBinding, SettingOthersAboutViewModel> {

    private long mLastClickTime = 0;
    private int mTestClickNum = 0 ;

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
        initTestWindow();
    }

    @Override
    public void onInitData() {

    }

    private void initTestWindow() {
        mBinding.settingOthersAboutQq.setOnClickListener(v -> {
            final long now = System.currentTimeMillis();
            if (now - mLastClickTime < 500) {
                mLastClickTime = now;
                mTestClickNum++;
            } else {
                mTestClickNum = 0;
                mLastClickTime = now;
            }
            if (mTestClickNum >= 4) {
                mTestClickNum = 0;
                TestWindow.getInstance().show(getActivity());
            }
        });
    }
}
