package com.fy.navi.hmi.setting.others.about.help;

import android.os.Bundle;
import android.view.View;
import android.widget.TextView;

import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentSettingHelpBinding;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.view.SkinLinearLayout;
import com.google.android.material.tabs.TabLayout;

import java.util.Objects;

public class SettingHelpFragment extends BaseFragment<FragmentSettingHelpBinding, SettingHelpViewModel> {

    private final int[] mTabIds = new int[]{
            R.string.setting_others_help_tab_question,
            R.string.setting_others_help_tab_display,
            R.string.setting_others_help_tab_route,
            R.string.setting_others_help_tab_search,
            R.string.setting_others_help_tab_broadcast,
            R.string.setting_others_help_tab_data
    };

    @Override
    public int onLayoutId() {
        return R.layout.fragment_setting_help;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        initView();
    }

    @Override
    public void onInitData() {
        initData();
    }

    @Override
    public void onInitObserver() {
        mBinding.helpTable.addOnTabSelectedListener(mOnTabSelectedListener);
    }

    private final TabLayout.OnTabSelectedListener mOnTabSelectedListener = new TabLayout.OnTabSelectedListener() {
        @Override
        public void onTabSelected(TabLayout.Tab tab) {
            View tabView = tab.getCustomView();
            if (tabView instanceof SkinLinearLayout) {
                tabView.setSelected(true);
                ((TextView) tabView.findViewById(R.id.tabText)).setTextColor(getResources().getColor(R.color.black));
            }
            mViewModel.setSelectPosition(tab.getPosition());
        }

        @Override
        public void onTabUnselected(TabLayout.Tab tab) {
            View tabView = tab.getCustomView();
            if (tabView instanceof SkinLinearLayout) {
                tabView.setSelected(false);
                ((TextView) tabView.findViewById(R.id.tabText)).setTextColor(getResources().getColor(R.color.setting_tab_gray));
            }
        }

        @Override
        public void onTabReselected(TabLayout.Tab tab) {

        }
    };

    private void initView(){
        for(int i = 0; i < 6; i++){
            TabLayout.Tab tab = mBinding.helpTable.newTab();
            View view = getLayoutInflater().inflate(R.layout.item_setting_help_tab, null);
            TextView tabText = view.findViewById(R.id.tabText);
            tabText.setText(mTabIds[i]);
            tab.setCustomView(view);
            mBinding.helpTable.addTab(tab, false);
        }
    }

    private void initData(){
        Bundle bundle = getArguments();
        if(bundle != null){
            Objects.requireNonNull(mBinding.helpTable.getTabAt(bundle.getInt("position"))).select();
        } else {
            Objects.requireNonNull(mBinding.helpTable.getTabAt(0)).select();
        }
    }
}
