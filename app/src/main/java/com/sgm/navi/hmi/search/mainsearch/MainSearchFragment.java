package com.sgm.navi.hmi.search.mainsearch;

import android.content.res.TypedArray;

import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.MainFragmentSearchBinding;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.ui.base.BaseFragment;


public class MainSearchFragment extends BaseFragment<MainFragmentSearchBinding, MainSearchViewModel> {

    @Override
    public int onLayoutId() {
        return R.layout.main_fragment_search;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        mBinding.searchView.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNestedScrollView.setScreenId(MapType.valueOf(mScreenId));

    }

    @Override
    public void onInitData() {
        final int powerType = mViewModel.powerType();
        final String[] categories;
        final TypedArray iconArray;
        mViewModel.getAppKey();
        // 油车
        if (powerType == 1) {
            categories = getResources().getStringArray(R.array.main_search_categories_name_charging);
            iconArray = getResources().obtainTypedArray(R.array.main_search_categories_charging_icons);
        } else if (powerType == 2) {
            categories = getResources().getStringArray(R.array.main_search_categories_name_phev);
            iconArray = getResources().obtainTypedArray(R.array.main_search_categories_phev_icons);
        } else {
            categories = getResources().getStringArray(R.array.main_search_categories_name_gas);
            iconArray = getResources().obtainTypedArray(R.array.main_search_categories_gas_icons);
        }
        mBinding.searchView.setSkinTextViews(categories, iconArray);
    }

    @Override
    public void onHiddenChanged(final boolean hidden) {
        super.onHiddenChanged(hidden);
        if (!hidden) {
            mBinding.sceneNestedScrollView.getSearchKeywordRecord();
            mBinding.sceneNestedScrollView.initRefreshCommonAddress();
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }
}
