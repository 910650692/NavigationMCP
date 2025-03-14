package com.fy.navi.hmi.search.mainsearch;

import android.content.res.TypedArray;

import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.MainFragmentSearchBinding;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.ui.base.BaseFragment;

/**
 * 搜索主页面
 */
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
        mBinding.searchView.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.sceneNestedScrollView.setScreenId(MapTypeId.valueOf(mScreenId));

    }

    @Override
    public void onInitData() {
        int powerType = mViewModel.powerType();
        String[] categories;
        TypedArray iconArray;
        // 油车
        if (powerType == 0){
            categories = getResources().getStringArray(R.array.main_search_categories_name_gas);
            iconArray = getResources().obtainTypedArray(R.array.main_search_categories_gas_icons);
        }else {
            categories = getResources().getStringArray(R.array.main_search_categories_name_charging);
            iconArray = getResources().obtainTypedArray(R.array.main_search_categories_charging_icons);
        }
        mBinding.searchView.setSkinTextViews(categories,iconArray);
    }

    @Override
    public void onHiddenChanged(boolean hidden) {
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
