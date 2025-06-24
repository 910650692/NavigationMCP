package com.sgm.navi.hmi.setting;

import android.os.Bundle;
import android.util.SparseArray;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentSettingBinding;
import com.sgm.navi.hmi.favorite.FavoriteFragment;
import com.sgm.navi.hmi.setting.broadcast.SettingBroadcastFragment;
import com.sgm.navi.hmi.setting.guide.SettingNaviFragment;
import com.sgm.navi.hmi.setting.others.SettingOthersFragment;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.ui.base.BaseFragment;
import com.sgm.navi.ui.view.SkinLinearLayout;
import com.google.android.material.tabs.TabLayout;

import java.lang.ref.WeakReference;

public class SettingFragment extends BaseFragment<FragmentSettingBinding, SettingViewModel> {

    private SparseArray<Fragment> mFragmentMap = new SparseArray<>();
    private Fragment mCurrentFragment;

    @Override
    public int onLayoutId() {
        return R.layout.fragment_setting;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    private WeakReference<SettingFragment> contextRef;
    @Override
    public void onInitView() {
        contextRef = new WeakReference<>(this);
        initListener();
    }

    @Override
    public void onInitData() {
        final Bundle bundle = getArguments();
        if (bundle != null) {
            final int id = bundle.getInt(AutoMapConstant.CommonBundleKey.BUNDLE_KEY_SETTING_TAB);
            showFragment(id - 1);
        }
        mBinding.tabLayout.addTab(mBinding.tabLayout.newTab().setCustomView(getCustomView(
                R.string.setting_tab_guide, R.drawable.bg_setting_tab_navi)));
        mBinding.tabLayout.addTab(mBinding.tabLayout.newTab().setCustomView(getCustomView(
                R.string.setting_tab_broadcast, R.drawable.bg_setting_tab_broadcast)));
        mBinding.tabLayout.addTab(mBinding.tabLayout.newTab().setCustomView(getCustomView(
                R.string.setting_tab_favorite, R.drawable.bg_setting_tab_favorite)));
        mBinding.tabLayout.addTab(mBinding.tabLayout.newTab().setCustomView(getCustomView(
                R.string.setting_tab_others, R.drawable.bg_setting_tab_others)));
    }

    private View getCustomView(int textRes, int imageRes) {
        final View view = getLayoutInflater().inflate(R.layout.item_setting_tab, null);
        final TextView tabText = view.findViewById(R.id.tabText);
        final ImageView tabIcon = view.findViewById(R.id.tabIcon);
        tabText.setText(textRes);
        tabIcon.setImageResource(imageRes);
        tabIcon.setVisibility(View.VISIBLE);
        tabText.setTextColor(getResources().getColor(R.color.setting_bg_tab_text_select));
        return view;
    }

    /**
     * 初始化监听
     */
    private void initListener() {
        mBinding.tabLayout.addOnTabSelectedListener(new TabLayout.OnTabSelectedListener() {
            @Override
            public void onTabSelected(TabLayout.Tab tab) {
                if (tab.getCustomView() instanceof SkinLinearLayout) {
                    tab.getCustomView().setSelected(true);
                }
                showFragment(tab.getPosition());
            }

            @Override
            public void onTabUnselected(TabLayout.Tab tab) {

            }

            @Override
            public void onTabReselected(TabLayout.Tab tab) {

            }
        });
    }

    private void showFragment(int position) {
        FragmentManager manager = getChildFragmentManager();
        FragmentTransaction transaction = manager.beginTransaction();
        if (mCurrentFragment != null) {
            transaction.hide(mCurrentFragment);
        }
        Fragment target = mFragmentMap.get(position);
        if (target == null) {
            switch (position) {
                case 0:
                    target = new SettingNaviFragment();
                    break;
                case 1:
                    target = new SettingBroadcastFragment();
                    break;
                case 2:
                    target = new FavoriteFragment();
                    break;
                case 3:
                    target = new SettingOthersFragment();
                    break;
                default:
                    break;
            }
            if (target == null) {
                return;
            }
            mFragmentMap.put(position, target);
            transaction.add(R.id.fragment_container, target);
        } else {
            transaction.show(target);
        }
        mCurrentFragment = target;
        transaction.commit();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        mFragmentMap.clear();
        mFragmentMap = null;
    }
}
