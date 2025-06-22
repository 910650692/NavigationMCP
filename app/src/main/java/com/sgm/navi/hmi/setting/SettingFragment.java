package com.sgm.navi.hmi.setting;

import android.os.Bundle;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;
import androidx.viewpager2.adapter.FragmentStateAdapter;

import com.android.utils.log.Logger;
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
import com.google.android.material.tabs.TabLayoutMediator;

import java.lang.ref.WeakReference;

public class SettingFragment extends BaseFragment<FragmentSettingBinding, SettingViewModel> {

    private static SettingBroadcastFragment mSettingBroadcastFragment;
    private static FavoriteFragment mFavoriteFragment;
    private static SettingOthersFragment mSettingOthersFragment;
    private static SettingNaviFragment mSettingNaviFragment;

    @Override
    public int onLayoutId() {
        return R.layout.fragment_setting;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    private ViewPagerAdapter mViewPagerAdapter;
    private WeakReference<SettingFragment> contextRef;
    @Override
    public void onInitView() {

        // 禁用滑动
        mBinding.viewPager.setUserInputEnabled(false);
        contextRef = new WeakReference<>(this);
        mViewPagerAdapter = new ViewPagerAdapter(contextRef.get());
        mBinding.viewPager.setAdapter(mViewPagerAdapter);
        initView();
        initListener();
    }

    @Override
    public void onInitData() {
        final Bundle bundle = getArguments();
        if (bundle != null) {
            final int id  = bundle.getInt(AutoMapConstant.CommonBundleKey.BUNDLE_KEY_SETTING_TAB);
            mBinding.viewPager.setCurrentItem(id - 1, false);
        }

    }

    private static class ViewPagerAdapter extends FragmentStateAdapter {

        public ViewPagerAdapter(@NonNull final Fragment fragment) {
            super(fragment);
        }

        @NonNull
        @Override
        public Fragment createFragment(final int position) {
            return switch (position) {
                case 1 -> mSettingBroadcastFragment != null ? mSettingBroadcastFragment : new SettingBroadcastFragment();
                case 2 -> mFavoriteFragment != null ? mFavoriteFragment : new FavoriteFragment();
                case 3 -> mSettingOthersFragment != null ? mSettingOthersFragment : new SettingOthersFragment();
                default -> mSettingNaviFragment != null ? mSettingNaviFragment : new SettingNaviFragment();
            };
        }

        @Override
        public int getItemCount() {
            return 4;
        }
    }

    /**
     * 初始化TabLayout
     */
    private void initView() {

        new TabLayoutMediator(mBinding.tabLayout, mBinding.viewPager,true,false,
                (tab, position) -> {

                    final View view = getLayoutInflater().inflate(R.layout.item_setting_tab, null);
                    final TextView tabText = view.findViewById(R.id.tabText);
                    final ImageView tabIcon = view.findViewById(R.id.tabIcon);


                    switch (position) {
                        case 0:
                            tabText.setText(R.string.setting_tab_guide);
                            tabIcon.setImageResource(R.drawable.bg_setting_tab_navi);
                            tabIcon.setVisibility(View.VISIBLE);
                            tabText.setTextColor(getResources().getColor(R.color.setting_bg_tab_text_select));
                            break;
                        case 1:
                            tabText.setText(R.string.setting_tab_broadcast);
                            tabIcon.setImageResource(R.drawable.bg_setting_tab_broadcast);
                            tabIcon.setVisibility(View.VISIBLE);
                            break;
                        case 2:
                            tabText.setText(R.string.setting_tab_favorite);
                            tabIcon.setImageResource(R.drawable.bg_setting_tab_favorite);
                            tabIcon.setVisibility(View.VISIBLE);
                            break;
                        case 3:
                            tabText.setText(R.string.setting_tab_others);
                            tabIcon.setImageResource(R.drawable.bg_setting_tab_others);
                            tabIcon.setVisibility(View.VISIBLE);
                            break;
                        default:
                            break;
                    }
                    Logger.i("tab.getCustomView() = " + tabText.getCurrentTextColor());
                    tab.setCustomView(view);
                }).attach();
    }

    /**
     * 初始化监听
     */
    private void initListener() {

        mBinding.tabLayout.addOnTabSelectedListener(new TabLayout.OnTabSelectedListener() {
            @Override
            public void onTabSelected(final TabLayout.Tab tab) {
                // 设置选中Tab的背景和文字颜色
                final View tabView = tab.getCustomView();
                if (tabView instanceof SkinLinearLayout) {
                    tabView.setSelected(true);
                    ((TextView) tabView.findViewById(R.id.tabText)).setTextColor(getResources().getColor(R.color.setting_bg_tab_text_select));
                }
            }

            @Override
            public void onTabUnselected(final TabLayout.Tab tab) {
                // 设置非选中Tab的背景和文字颜色
                final View tabView = tab.getCustomView();
                if (tabView instanceof SkinLinearLayout) {
                    tabView.setSelected(false);
                    ((TextView) tabView.findViewById(R.id.tabText)).setTextColor(getResources().getColor(R.color.setting_bg_tab_text_unselect));
                }
            }

            @Override
            public void onTabReselected(final TabLayout.Tab tab) {
                // 处理Tab重新选中的情况（可选）
            }
        });
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        mBinding.viewPager.setAdapter(null);
        mViewPagerAdapter = null;
        mSettingBroadcastFragment = null;
        mFavoriteFragment = null;
        mSettingOthersFragment = null;
        mSettingNaviFragment = null;
    }
}
