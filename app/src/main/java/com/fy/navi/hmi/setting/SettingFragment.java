package com.fy.navi.hmi.setting;

import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;
import androidx.viewpager2.adapter.FragmentStateAdapter;

import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentSettingBinding;
import com.fy.navi.hmi.favorite.FavoriteFragment;
import com.fy.navi.hmi.setting.broadcast.SettingBroadcastFragment;
import com.fy.navi.hmi.setting.guide.SettingNaviFragment;
import com.fy.navi.hmi.setting.others.SettingOthersFragment;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.view.SkinLinearLayout;
import com.google.android.material.tabs.TabLayout;
import com.google.android.material.tabs.TabLayoutMediator;

/**
 *
 */
public class SettingFragment extends BaseFragment<FragmentSettingBinding, SettingViewModel> {

    @Override
    public int onLayoutId() {
        return R.layout.fragment_setting;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {

        // 禁用滑动
        mBinding.viewPager.setUserInputEnabled(false);
        mBinding.viewPager.setAdapter(new ViewPagerAdapter(this));
        initView();
        initListener();
    }

    @Override
    public void onInitData() {
    }

    private static class ViewPagerAdapter extends FragmentStateAdapter {
        public ViewPagerAdapter(@NonNull Fragment fragment) {
            super(fragment);
        }

        @NonNull
        @Override
        public Fragment createFragment(int position) {
            switch (position) {
                case 0:
                    return new SettingNaviFragment();
                case 1:
                    return new SettingBroadcastFragment();
                case 2:
                    return new FavoriteFragment();
                case 3:
                    return new SettingOthersFragment();
            }
            return new SettingNaviFragment();
        }

        @Override
        public int getItemCount() {
            return 4;
        }
    }

    private void initView() {

        new TabLayoutMediator(mBinding.tabLayout, mBinding.viewPager,true,false,
                (tab, position) -> {

                    View view = getLayoutInflater().inflate(R.layout.item_setting_tab, null);
                    TextView tabText = view.findViewById(R.id.tabText);
                    ImageView tabIcon = view.findViewById(R.id.tabIcon);


                    switch (position) {
                        case 0:
                            tabText.setText(R.string.setting_tab_guide);
                            tabIcon.setImageResource(R.drawable.bg_setting_tab_navi);
                            tabIcon.setVisibility(View.VISIBLE);
                            tabText.setTextColor(getResources().getColor(R.color.black));
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
                    }
                    Logger.i("tab.getCustomView() = " + tabText.getCurrentTextColor());
                    tab.setCustomView(view);
                }).attach();
    }

    private void initListener() {

        mBinding.tabLayout.addOnTabSelectedListener(new TabLayout.OnTabSelectedListener() {
            @Override
            public void onTabSelected(TabLayout.Tab tab) {
                // 设置选中Tab的背景和文字颜色
                View tabView = tab.getCustomView();
                if (tabView instanceof SkinLinearLayout) {
                    tabView.setSelected(true);
                    ((TextView) tabView.findViewById(R.id.tabText)).setTextColor(getResources().getColor(R.color.black));
                }
            }

            @Override
            public void onTabUnselected(TabLayout.Tab tab) {
                // 设置非选中Tab的背景和文字颜色
                View tabView = tab.getCustomView();
                if (tabView instanceof SkinLinearLayout) {
                    tabView.setSelected(false);
                    ((TextView) tabView.findViewById(R.id.tabText)).setTextColor(getResources().getColor(R.color.setting_tab_gray));
                }
            }

            @Override
            public void onTabReselected(TabLayout.Tab tab) {
                // 处理Tab重新选中的情况（可选）
            }
        });
    }
}
