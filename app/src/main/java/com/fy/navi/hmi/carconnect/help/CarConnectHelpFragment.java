package com.fy.navi.hmi.carconnect.help;

import android.os.Bundle;
import android.view.View;
import android.widget.TextView;

import com.android.utils.ResourceUtils;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentCarConnectHelpBinding;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.view.SkinLinearLayout;
import com.google.android.material.tabs.TabLayout;

import java.util.Objects;

public class CarConnectHelpFragment extends BaseFragment<FragmentCarConnectHelpBinding, CarConnectHelpViewModel> {

    private final int[] mTabIds = new int[]{
            R.string.setting_others_about_help_remote_navi_tab,
            R.string.setting_others_about_help_seamless_interconnection_tab,
            R.string.setting_others_about_help_find_car_tab,
            R.string.setting_others_about_help_data_sync_tab
    };

    @Override
    public int onLayoutId() {
        return R.layout.fragment_car_connect_help;
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
        public void onTabSelected(final TabLayout.Tab tab) {
            final View tabView = tab.getCustomView();
            if (tabView instanceof SkinLinearLayout) {
                tabView.setSelected(true);
                ((TextView) tabView.findViewById(R.id.tabText)).setTextColor(
                        ResourceUtils.Companion.getInstance().getColor(R.color.setting_bg_tab_text_select));
            }
            mViewModel.setSelectPosition(tab.getPosition());
        }

        @Override
        public void onTabUnselected(final TabLayout.Tab tab) {
            final View tabView = tab.getCustomView();
            if (tabView instanceof SkinLinearLayout) {
                tabView.setSelected(false);
                ((TextView) tabView.findViewById(R.id.tabText)).setTextColor(
                        ResourceUtils.Companion.getInstance().getColor(R.color.setting_bg_tab_text_unselect));
            }
        }

        @Override
        public void onTabReselected(final TabLayout.Tab tab) {

        }
    };

    /**
     * initView
     */
    private void initView(){
        for(int i = 0; i < 4; i++){
            final TabLayout.Tab tab = mBinding.helpTable.newTab();
            final View view = getLayoutInflater().inflate(R.layout.item_setting_help_tab, null);
            final TextView tabText = view.findViewById(R.id.tabText);
            tabText.setText(mTabIds[i]);
            tab.setCustomView(view);
            mBinding.helpTable.addTab(tab, false);
        }
    }

    /**
     * initData
     */
    private void initData(){
        final Bundle bundle = getArguments();
        if(bundle != null){
            Objects.requireNonNull(mBinding.helpTable.getTabAt(bundle.getInt("position"))).select();
        } else {
            Objects.requireNonNull(mBinding.helpTable.getTabAt(0)).select();
        }
    }
}
