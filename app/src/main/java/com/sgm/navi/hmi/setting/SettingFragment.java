package com.sgm.navi.hmi.setting;

import android.annotation.SuppressLint;
import android.os.Bundle;
import android.util.SparseArray;
import android.view.KeyEvent;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.android.utils.ConvertUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.google.android.material.tabs.TabLayout;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentSettingBinding;
import com.sgm.navi.hmi.favorite.FavoriteFragment;
import com.sgm.navi.hmi.setting.broadcast.SettingBroadcastFragment;
import com.sgm.navi.hmi.setting.guide.SettingNaviFragment;
import com.sgm.navi.hmi.setting.others.SettingOthersFragment;
import com.sgm.navi.scene.ui.setting.SceneVerticalTabLayout;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.ui.BuildConfig;
import com.sgm.navi.ui.base.BaseFragment;
import com.sgm.navi.ui.view.SkinLinearLayout;

import java.lang.ref.WeakReference;

public class SettingFragment extends BaseFragment<FragmentSettingBinding, SettingViewModel> {

    private SparseArray<Fragment> mFragmentMap;
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

    @SuppressLint("UseCompatLoadingForDrawables")
    @Override
    public void onInitData() {
        SceneVerticalTabLayout tabLayout = mBinding.tabLayout;
        TabLayout.Tab tabNavi = tabLayout.newTab();
        mBinding.tabLayout.addTab(tabNavi.setCustomView(getCustomView(
                R.string.setting_tab_guide, R.drawable.bg_setting_tab_navi)));
        TabLayout.Tab tabBroadcast = tabLayout.newTab();
        mBinding.tabLayout.addTab(tabBroadcast.setCustomView(getCustomView(
                R.string.setting_tab_broadcast, R.drawable.bg_setting_tab_broadcast)));
        TabLayout.Tab tabFavorite = tabLayout.newTab();
        mBinding.tabLayout.addTab(tabFavorite.setCustomView(getCustomView(
                R.string.setting_tab_favorite, R.drawable.bg_setting_tab_favorite)));
        TabLayout.Tab tabOthers = tabLayout.newTab();
        mBinding.tabLayout.addTab(tabOthers.setCustomView(getCustomView(
                R.string.setting_tab_others, R.drawable.bg_setting_tab_others)));
        if (BuildConfig.FLAVOR.equals("cadi")) {
            tabNavi.view.setBackground(getResources().getDrawable(R.drawable.bg_base_mfc_cadillac_bg_selector));
            tabBroadcast.view.setBackground(getResources().getDrawable(R.drawable.bg_base_mfc_cadillac_bg_selector));
            tabFavorite.view.setBackground(getResources().getDrawable(R.drawable.bg_base_mfc_cadillac_bg_selector));
            tabOthers.view.setBackground(getResources().getDrawable(R.drawable.bg_base_mfc_cadillac_bg_selector));
        }
    }

    private View getCustomView(int textRes, int imageRes) {
        final View view = getLayoutInflater().inflate(R.layout.item_setting_tab, null);
        final TextView tabText = view.findViewById(R.id.tabText);
        final ImageView tabIcon = view.findViewById(R.id.tabIcon);
        tabText.setText(textRes);
        tabIcon.setImageResource(imageRes);
        tabIcon.setVisibility(View.VISIBLE);
        tabText.setTextColor(getResources().getColor(R.color.setting_bg_tab_text_unselect));
        return view;
    }

    /**
     * 初始化监听
     */
    private void initListener() {
        mBinding.tabLayout.addOnTabSelectedListener(new TabLayout.OnTabSelectedListener() {
            @Override
            public void onTabSelected(TabLayout.Tab tab) {
                final View tabView = tab.getCustomView();
                if (tabView instanceof SkinLinearLayout) {
                    tabView.setSelected(true);
                    ((TextView) tabView.findViewById(R.id.tabText)).setTextColor(getResources().getColor(R.color.black));
                }
                showFragment(tab.getPosition());
            }

            @Override
            public void onTabUnselected(TabLayout.Tab tab) {
                final View tabView = tab.getCustomView();
                if (tabView instanceof SkinLinearLayout) {
                    tabView.setSelected(false);
                    ((TextView) tabView.findViewById(R.id.tabText)).setTextColor(getResources().getColor(R.color.setting_bg_tab_text_unselect));
                }
            }

            @Override
            public void onTabReselected(TabLayout.Tab tab) {

            }
        });
    }

    @Override
    public void refreshFragment(Bundle bundle) {
        super.refreshFragment(bundle);
        int position = bundle.getInt(AutoMapConstant.CommonBundleKey.BUNDLE_KEY_SETTING_TAB);
        Logger.d("VrBridgeHandle", "voice open position: ", position);
        mBinding.tabLayout.selectTab(mBinding.tabLayout.getTabAt(position));
        showFragment(position);
    }

    private void showFragment(int position) {
        FragmentManager manager = getChildFragmentManager();
        FragmentTransaction transaction = manager.beginTransaction();
        if (mFragmentMap == null) {
            mFragmentMap = new SparseArray<>();
        }
        Fragment target = mFragmentMap.get(position);

        if (mCurrentFragment != null) {
            if (mCurrentFragment.equals(target)) {
                return;
            }
            transaction.hide(mCurrentFragment);
        }
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
        } else if (!target.isAdded()) {
            transaction.add(R.id.fragment_container, target);
        } else {
            transaction.show(target);
        }
        mCurrentFragment = target;
        transaction.commit();
    }

    @Override
    public void onStart() {
        super.onStart();
        final Bundle bundle = getArguments();
        if (!ConvertUtils.isEmpty(bundle) && bundle.containsKey(AutoMapConstant.CommonBundleKey.BUNDLE_KEY_SETTING_TAB)) {
            final int id = bundle.getInt(AutoMapConstant.CommonBundleKey.BUNDLE_KEY_SETTING_TAB);
            mBinding.tabLayout.selectTab(mBinding.tabLayout.getTabAt(id));
            showFragment(id);
        } else {
            int position = mBinding.tabLayout.getSelectedTabPosition();
            if (position == -1) {
                mBinding.tabLayout.selectTab(mBinding.tabLayout.getTabAt(0));
                showFragment(0);
            } else {
                showFragment(position);
            }
        }
    }

    @Override
    public void onStop() {
        super.onStop();
        FragmentManager manager = getChildFragmentManager();
        FragmentTransaction transaction = manager.beginTransaction();
        if (mFragmentMap != null) {
            for (int i = 0; i < mFragmentMap.size(); i++) {
                Fragment fragment = mFragmentMap.valueAt(i);
                if (fragment != null && fragment.isAdded()) {
                    transaction.remove(fragment);
                }
            }
        }
        transaction.commitAllowingStateLoss();
        mCurrentFragment = null;
        if (mFragmentMap != null) {
            mFragmentMap.clear();
            mFragmentMap = null;
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }
}
