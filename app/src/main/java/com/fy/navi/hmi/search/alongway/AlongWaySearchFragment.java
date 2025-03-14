package com.fy.navi.hmi.search.alongway;

import android.content.res.TypedArray;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentAlongWayBinding;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.ui.base.BaseFragment;

/**
 * 沿途搜索页面
 */
@Route(path = RoutePath.Search.ALONG_WAY_SEARCH_FRAGMENT)
public class AlongWaySearchFragment extends BaseFragment<FragmentAlongWayBinding, AlongWaySearchViewModel> {

    @Override
    public int onLayoutId() {
        return R.layout.fragment_along_way;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        mBinding.sceneQuickSearchView.setScreenId(MapTypeId.valueOf(mScreenId));
    }

    @Override
    public void onInitData() {
        defaultDataProcessing();
    }


    private void defaultDataProcessing() {
        int powerType = mViewModel.powerType();
        String[] categories;
        TypedArray iconArray;
        // 油车
        if (powerType == 0) {
            categories = getResources().getStringArray(R.array.along_way_categories_gas_name);
            iconArray = getResources().obtainTypedArray(R.array.along_way_category_gas_icons);
        }else {
            categories = getResources().getStringArray(R.array.along_way_categories_charging_name);
            iconArray = getResources().obtainTypedArray(R.array.along_way_category_charging_icons);
        }
        mBinding.sceneQuickSearchView.setQuickSearchListAdapterData(iconArray, categories);
        mBinding.sceneQuickSearchView.setSearchType(AutoMapConstant.SearchType.ALONG_WAY_SEARCH);
        mBinding.sceneQuickSearchView.setPoiInfoEntity(null);
        mBinding.sceneQuickSearchView.setTextView(getString(R.string.along_way_search_title));
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }
}
